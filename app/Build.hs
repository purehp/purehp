{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections     #-}
module Build (parser) where

import           Control.Exception                   (tryJust)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Supply
import           Control.Monad.Trans.Class           (lift)
import           Data.Aeson                          as Aeson
import qualified Data.Aeson                          as A
import           Data.Aeson.Types                    (Value, parseMaybe)
import           Data.Bool                           (bool)
import qualified Data.ByteString.Lazy.UTF8           as LBU8
import qualified Data.Map                            as M
import           Data.Maybe                          (catMaybes, mapMaybe)
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as TE
import qualified Language.PureScript                 as P
import qualified Language.PureScript.CoreFn          as CoreFn
import qualified Language.PureScript.CoreFn.FromJSON as CoreFn
import           Language.PureScript.PHP.Errors      as E
import           Language.PureScript.PHP.Errors.JSON
import qualified Language.PureScript.PHP.Make        as Make
import qualified Language.PureScript.PHP.Make.Monad  as MM
import           Language.PureScript.PHP.Run         (runProgram)
import qualified Options.Applicative                 as Opts
import qualified System.Console.ANSI                 as ANSI
import           System.Directory                    (doesFileExist,
                                                      getCurrentDirectory)
import           System.Exit                         (exitFailure, exitSuccess)
import           System.FilePath                     (joinPath,
                                                      replaceExtension,
                                                      replaceFileName)
import           System.FilePath.Glob                (glob)
import           System.IO                           (hPutStr, hPutStrLn,
                                                      stderr)
import           System.IO.Error                     (isDoesNotExistError)

import           Debug.Trace


data BuildOptions = BuildOptions
  { buildOutputDir :: FilePath
  , buildRun       :: Maybe String
  }

compile :: BuildOptions -> IO ()
compile BuildOptions{..} = do
  let coreFnGlob = joinPath [ buildOutputDir, "*", "corefn.json" ]
  corefnFiles <- globWarningOnMisses warnFileTypeNotFound [coreFnGlob]
  when (null corefnFiles) $ do
    hPutStr stderr $ unlines [ "purerl: No input files."
                             , "Usage: For basic information, try the `--help' option."
                             ]
    exitFailure
  (makeErrors, makeWarnings) <- MM.runMake P.defaultOptions $ do
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

    forM_ modules' $ \(m, _, _, ts) -> do
      shouldBuild <- needsBuild buildActions ts $ CoreFn.moduleName m
      when shouldBuild $ do
        ((file, php), _) <- runSupplyT 0 $ Make.codegen buildActions m
        MM.writeTextFile file $ TE.encodeUtf8 php
        Make.ffiCodegen buildActions m

  printWarningsAndErrors False False makeWarnings makeErrors

  case buildRun of
    Nothing        -> pure ()
    Just runModule -> runProgram $ T.pack runModule

  exitSuccess
  where


  -- always true for now
  needsBuild buildActions ts m = do
    pure True
    -- outputTs <- Make.getOutputTimestamp buildActions m
    -- pure $ case outputTs of
    --   Nothing -> True
    --   Just outTs | outTs < ts -> True
    --   _ -> False

-- | Arguments: verbose, use JSON, warnings, errors
printWarningsAndErrors :: Bool -> Bool -> MultipleErrors -> Either MultipleErrors a -> IO ()
printWarningsAndErrors verbose False warnings errors = do
  pwd <- getCurrentDirectory
  cc <- bool Nothing (Just P.defaultCodeColor) <$> ANSI.hSupportsANSI stderr
  let ppeOpts = E.defaultPPEOptions { E.ppeCodeColor = cc
                                    , E.ppeFull = verbose
                                    , E.ppeRelativeDirectory = pwd
                                    }
  when (E.nonEmpty warnings) $
    hPutStrLn stderr (E.prettyPrintMultipleWarnings ppeOpts warnings)
  case errors of
    Left errs -> do
      hPutStrLn stderr (E.prettyPrintMultipleErrors ppeOpts errs)
      exitFailure
    Right _ -> return ()
printWarningsAndErrors verbose True warnings errors = do
  hPutStrLn stderr . LBU8.toString . A.encode $
    JSONResult (toJSONErrors verbose E.Warning warnings)
      (either (toJSONErrors verbose E.Error) (const []) errors)
  either (const exitFailure) (const (return ())) errors

inferForeignModule :: MonadIO m => FilePath -> m (Maybe FilePath)
inferForeignModule path = do
  let jsFile = replaceExtension path "php"
  exists <- liftIO $ doesFileExist jsFile
  if exists
    then return (Just jsFile)
    else return Nothing

-- | Read a JSON file in the 'Make' monad, returning 'Nothing' if the file does.
-- not exist or could not be parsed. Errors are captured using the 'MonadError'
-- instance.
readJSONFile :: FilePath -> MM.Make (Maybe Value)
readJSONFile path =
  MM.makeIO ("read JSON file: " <> T.pack path) $ do
    r <- catchDoesNotExist $ Aeson.decodeFileStrict path
    return $ join r

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

outputDirectory :: Opts.Parser FilePath
outputDirectory = Opts.strOption $
     Opts.short 'o'
  <> Opts.long "output"
  <> Opts.value "output"
  <> Opts.showDefault
  <> Opts.help "The output directory"

run :: Opts.Parser (Maybe String)
run = Opts.optional $ Opts.strOption $
  Opts.long "run"
  <> Opts.help "Run the given function"

buildOptions :: Opts.Parser BuildOptions
buildOptions = BuildOptions <$> outputDirectory <*> run

parser :: Opts.Parser (IO ())
parser = compile <$> buildOptions
