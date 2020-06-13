module Language.PureScript.PHP.Pretty (
  prettyPrintPHP
) where

import           Prelude                                ()
import           Prelude.Compat

import           Control.Arrow                          ((<+>))
import qualified Control.Arrow                          as A
import           Control.Monad.State                    hiding (sequence)
import           Control.PatternArrows

import           Language.PureScript.AST.SourcePos
import           Language.PureScript.Crash
import           Language.PureScript.PHP.CodeGen.AST
import           Language.PureScript.PHP.CodeGen.Common
import           Language.PureScript.PSString

import           Data.Maybe                             (fromMaybe)
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           Data.Word                              (Word16)

import           Language.PureScript.Pretty.Common      (Emit, emit,
                                                         intercalate, parensPos,
                                                         runPlainString)

prettyPrintPHP :: (String -> String) -> [PHP] -> Text
prettyPrintPHP transformFileName = error "Missing prettyPrintPHP"
