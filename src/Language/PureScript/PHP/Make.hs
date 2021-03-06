{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.PHP.Make where

import           Debugger
import           Prelude

import           Control.Monad                             hiding (sequence)
import           Control.Monad.Error.Class                 (MonadError (..))
import           Control.Monad.Supply
import           Control.Monad.Trans.Class                 (MonadTrans (..))
import           Control.Monad.Writer.Class                (MonadWriter (..))
import           Data.Foldable                             (for_, minimum)
import qualified Data.List.NonEmpty                        as NEL
import qualified Data.Map                                  as M
import           Data.Maybe                                (catMaybes, isJust)
import qualified Data.Text                                 as T
import qualified Data.Text.Encoding                        as TE
import           Data.Time.Clock                           (UTCTime)
import           Data.Version                              (showVersion)
import qualified Language.PureScript                       as P
import qualified Language.PureScript.CoreFn                as CF
import           Language.PureScript.PHP.Make.Monad
import           Language.PureScript.PHP.Parser            (parseFile)
import           System.Directory                          (getCurrentDirectory)
import           System.FilePath                           ((</>))

import qualified Paths_purehp                              as Paths

import           Language.PureScript.PHP.CodeGen           (moduleToPHP)
import           Language.PureScript.PHP.CodeGen.Common    (moduleNameToPHP)
import           Language.PureScript.PHP.CodeGen.Optimizer (optimize)
import           Language.PureScript.PHP.Errors
import           Language.PureScript.PHP.Errors.Types
import           Language.PureScript.PHP.Pretty            (prettyPrintPHP)

import           Debug.Trace

data MakeActions m = MakeActions
  { codegen            :: CF.Module CF.Ann -> SupplyT m (FilePath, T.Text)
  -- ^ Run the code generator for the module and write any required output files.
  , ffiCodegen         :: CF.Module CF.Ann -> m ()
  , getOutputTimestamp :: P.ModuleName -> m (Maybe UTCTime)
  -- ^ Get the timestamp for the output files for a module. This should be the
  -- timestamp for the oldest modified file, or 'Nothing' if any of the required
  -- output files are missing.
  }

buildActions :: String -> P.Environment -> M.Map P.ModuleName FilePath -> Bool -> MakeActions Make
buildActions outputDir env foreigns usePrefix = do
  MakeActions codegen ffiCodegen getOutputTimestamp
  where

    getOutputTimestamp :: P.ModuleName -> Make (Maybe UTCTime)
    getOutputTimestamp mn = do
      let outputPaths = [ outFile mn ]
      timestamps <- traverse getTimestampMaybe outputPaths
      pure $ fmap minimum . NEL.nonEmpty =<< sequence timestamps

    codegen :: CF.Module CF.Ann -> SupplyT Make (FilePath, T.Text)
    codegen m = do
      let mn = CF.moduleName m
      foreignExports <- lift $ case mn `M.lookup` foreigns of
        Just path
          | not $ requiresForeign m ->
              return []
          | otherwise -> getForeigns path
        Nothing ->
          return []

      rawPHP <- moduleToPHP m Nothing --foreignExports
      -- lift $ cpprint rawPHP
      optimized <- traverse optimize rawPHP
      -- dir <- lift $ makeIO "get file info: ." getCurrentDirectory
      -- let makeAbsFile file = dir </> file
      let pretty = prettyPrintPHP optimized
      let
        prefix :: [T.Text]
        prefix = ["generated by purehp version " <> T.pack (showVersion Paths.version) | usePrefix]
        strict = "declare(strict_types=1);"
        (namespace, _) = moduleNameToPHP mn
        namespace' = if null namespace then Nothing else Just ("namespace " <> T.intercalate "\\" namespace <> ";")
        php = T.unlines $ ["<?php", "/**"] ++ map (" * " <>) prefix ++ catMaybes [pure " */"
                                                                                , pure strict
                                                                                , guard (isJust namespace') *> pure ""
                                                                                , namespace'
                                                                                , pure ""
                                                                                , pure pretty
                                                                                , pure ""
                                                                                , pure "?>"]
      pure (outFile mn, php)

    -- This whole function needs to be checked again
    ffiCodegen :: CF.Module CF.Ann -> Make ()
    ffiCodegen m = do
      let mn = CF.moduleName m
          (_, moduleName) = moduleNameToPHP mn
          foreignFile = moduleDir mn </> T.unpack (moduleName) ++ ".php"
      case mn `M.lookup` foreigns of
        Just path
          | not $ requiresForeign m ->
              tell $ errorMessage $ UnnecessaryFFIModule mn path
          | otherwise -> pure ()
        Nothing ->
          when (requiresForeign m) $ throwError . errorMessage $ MissingFFIModule mn
      for_ (mn `M.lookup` foreigns) $ \path ->
        copyFile path foreignFile

    outFile mn = moduleDir mn </> T.unpack (snd $ moduleNameToPHP mn) ++ ".php"
    moduleDir mn = outputDir </> T.unpack (T.intercalate "/" (fst $ moduleNameToPHP mn)) -- (P.runModuleName mn)

    requiresForeign :: CF.Module a -> Bool
    requiresForeign = not . null . CF.moduleForeign

    getForeigns :: String -> Make [(T.Text, Int)]
    getForeigns path = do
      text <- readTextFile path
      pure $ either (const []) id $ parseFile path text
