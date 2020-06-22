{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestUtils where

import           Prelude                    ()
import           Prelude.Compat

import qualified Language.PureScript        as P
import qualified Language.PureScript.CST    as CST

import           Control.Arrow              ((***), (>>>))
import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer.Class (tell)
import           Data.Char                  (isSpace)
import           Data.Function              (on)
import           Data.List                  (groupBy, sort, sortBy, stripPrefix)
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Data.Time.Clock            (UTCTime)
import           Data.Tuple                 (swap)
import           System.Directory
import           System.Exit                (exitFailure)
import           System.FilePath
import qualified System.FilePath.Glob       as Glob
import           System.Info
import           System.IO
import           System.IO.UTF8             (readUTF8FileT)
import           System.Process             hiding (cwd)
import           Test.Tasty.Hspec

-- |
-- Fetches code necesary to run the tests with. The resulting support cocde
-- should then be chcked in, so that npm/bower etc is not required to run the
-- tests.
--
-- Simply rerun this (via ghci is probably easiest) when the support code
-- needs updating.
--
updateSupportCode :: IO ()
updateSupportCode = do
  setCurrentDirectory "tests/golden"
  callProcess "npm" ["install"]
  callProcess "spago" ["install"]
  callProcess "purs" ["compile", "**/*.purs", "--codegen", "corefn"]
  setCurrentDirectory "../.."
