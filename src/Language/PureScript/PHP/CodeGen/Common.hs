-- |
-- Common code generation utility functions
--
module Language.PureScript.PHP.CodeGen.Common
( runAtom
-- , atomPS
, atom
, atomModuleName
, phpModuleName
, ModuleType(..)
-- , toAtomName
-- , toVarName
-- , identToAtomName
-- , identToVar
-- , nameIsErlReserved
-- , utf8Binary
-- , encodeChar
) where

import           Data.Char
import           Data.Monoid                         ((<>))
import           Data.Text                           (Text, all, cons,
                                                      intercalate, pack,
                                                      singleton, uncons)
import qualified Data.Text                           as T
import           Data.Word                           (Word16)
import           Language.PureScript.Names
import           Language.PureScript.PSString
import           Numeric
import           Prelude.Compat                      hiding (all)

import           Language.PureScript.PHP.CodeGen.AST


runAtom :: Atom -> Text
runAtom at = error "Missing"

atom :: Text -> Text
atom s = error "Missing"

data ModuleType = ForeignModule | PureScriptModule

atomModuleName :: ModuleName -> ModuleType -> Text
atomModuleName mn mt = phpModuleName mn mt

phpModuleName :: ModuleName -> ModuleType -> Text
phpModuleName (ModuleName pns) moduleType = error "Missing"
