module TestGolden where

-- | These tests check the codegen output agains the expected result.
--  The golden files are genearted automatically when missing, and can be
-- uupdated by passing `--accept` to `--test-arguments`.

import Prelude ()
import Prelude.Compat

import qualified Language.PureScript as P

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import Control.Monad
import TestUtils
import Test.Tasty
import Test.Tasty (testGroup)
import Test.Tasty.Golden (goldenVsString)
import System.Exit
import System.Process
import System.FilePath
import System.IO
import System.IO.UTF8 (readUTF8File)

main :: IO TestTree
main = do
  -- (supportModules, supportExterns, supportForeigns) <- setupSupportModules
  -- testFiles <- getTestFiles "golden"
  -- tests <- forM testFiles $ \testPurs -> do
  --   let mainPath = getTestMain testPurs
  --   return [ goldenVsString
  --            ("'" <> takeFileName mainPath <> "' golden test")
  --            (replaceExtension mainPath ".php")
  --            (BS.fromStrict . T.encodeUtf8 . T.pack <$> printOutput supportModules supportExterns supportForeigns)
  --          ]
  let tests = []
  return $ testGroup "Golden tests" $ concat tests

-- printOutput
--   :: [P.Module]
--   -> [P.ExternsFile]
--   -> M.Map P.ModuleName FilePath
--   -> [FilePath]
--   -> IO String
-- printOutput supportModules supportExterns supportForeigns inputFiles = do
--   error "Implement printOutput"
