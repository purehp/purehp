{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module TestGolden where

-- | These tests check the codegen output agains the expected result.
--  The golden files are genearted automatically when missing, and can be
-- uupdated by passing `--accept` to `--test-arguments`.

import           Prelude                              ()
import           Prelude.Compat

import qualified Language.PureScript                  as P

import           Control.Exception                    (tryJust)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Supply
import           Data.Aeson                           as Aeson
import           Data.Aeson.Types                     (Value, parseMaybe)
import qualified Data.ByteString.Lazy                 as BS
import qualified Data.Map                             as M
import           Data.Maybe                           (catMaybes, mapMaybe)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import qualified Language.PureScript.CoreFn           as CoreFn
import qualified Language.PureScript.CoreFn.FromJSON  as CoreFn
import           Language.PureScript.PHP.Errors
import qualified Language.PureScript.PHP.Make         as Make
import qualified Language.PureScript.PHP.Make.Monad   as MM
import           System.Directory                     (doesFileExist)
import           System.FilePath
import           System.FilePath.Glob                 (glob)
import           System.IO
import           System.IO.Error                      (isDoesNotExistError)
import           Test.Tasty
import           Test.Tasty.Golden                    (goldenVsString)
import Debugger

main :: IO TestTree
main = do
  let buildOutputDir = "output"
      coreFnGlob = joinPath [ "tests", "golden", "output", "**", "corefn.json" ]
  traceShowIdPP coreFnGlob
  corefnFiles <- globWarningOnMisses warnFileTypeNotFound [coreFnGlob]
  res :: (Either MultipleErrors [[TestTree]], MultipleErrors) <- MM.runMake P.defaultOptions $ do
    modules <- forM corefnFiles $ \corefn -> do
      let extern = replaceFileName corefn "externs.cbor"
      res <- readJSONFile corefn
      resExterns <- MM.readExternsFile extern
      case res >>= parseMaybe CoreFn.moduleFromJSON of
          Just (_version, module') -> do
            foreignFile <- liftIO $ inferForeignModule $ CoreFn.modulePath module'
            case resExterns of
              Just f -> do
                when (not $ P.externsIsCurrentVersion f) $
                  liftIO $ hPutStrLn stderr $ "Found externs for wrong compiler version (continuing anyway): " <> extern
                sourceTime <- max <$> MM.getTimestamp corefn <*> MM.getTimestamp extern
                foreignTime <- case foreignFile of
                                Just ff -> max <$> MM.getTimestamp ff <*> pure sourceTime
                                Nothing -> pure sourceTime
                pure $ Just ( module', foreignFile, f, foreignTime)
              Nothing -> do
                liftIO $ hPutStrLn stderr $ "Error parsing externs: " <> extern
                pure Nothing
          Nothing -> do
            liftIO $ hPutStrLn stderr $ "Error parsing corefn: " <> corefn
            pure Nothing


    let modules' = catMaybes modules
        foreigns = M.fromList $ mapMaybe (\(m, fp, _, _) -> (CoreFn.moduleName m,) <$> fp) modules'
        env = foldr P.applyExternsFileToEnvironment P.initEnvironment $ map (\(_, _, e, _) -> e) modules'
        buildActions = Make.buildActions buildOutputDir env foreigns True

    forM modules' $ \(m, _, _, _) -> do
      ((file, php), _) <- runSupplyT 0 $ Make.codegen buildActions m
      return [ goldenVsString
               ("'" <> takeFileName file <> "' golden test")
               (joinPath ["tests", "golden", replaceExtension file ".php"])
               (return $ BS.fromStrict . T.encodeUtf8 $ php)
             ]
  case res of
    (Right tests, _) ->
       return $ testGroup "Golden tests" $ concat tests
    _ -> error "boh"


-- | Read a JSON file in the 'Make' monad, returning 'Nothing' if the file does.
-- not exist or could not be parsed. Errors are captured using the 'MonadError'
-- instance.
readJSONFile :: FilePath -> MM.Make (Maybe Value)
readJSONFile path =
  MM.makeIO ("read JSON file: " <> T.pack path) $ do
    r <- catchDoesNotExist $ Aeson.decodeFileStrict path
    return $ join r

inferForeignModule :: MonadIO m => FilePath -> m (Maybe FilePath)
inferForeignModule path = do
  let jsFile = replaceExtension path "php"
  exists <- liftIO $ doesFileExist jsFile
  if exists
    then return (Just jsFile)
    else return Nothing

-- | If the provided action threw an 'IsDoesNotExist' error, catch it and
-- return Nothing. Otherwise return Just the result of the inner action.
catchDoesNotExist :: IO a -> IO (Maybe a)
catchDoesNotExist inner = do
  r <- tryJust (guard . isDoesNotExistError) inner
  case r of
    Left () -> return Nothing
    Right x -> return (Just x)

warnFileTypeNotFound :: String -> IO ()
warnFileTypeNotFound = hPutStrLn stderr . ("purs compile: No files found using pattern: " ++)

globWarningOnMisses :: (String -> IO ()) -> [FilePath] -> IO [FilePath]
globWarningOnMisses warn = concatMapM globWithWarning
  where
    globWithWarning pattern' = do
      paths <- glob pattern'
      when (null paths) $ warn pattern'
      return paths
    concatMapM f = fmap concat . mapM f
