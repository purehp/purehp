-- |
-- Common code generation utility functions
--
module Language.PureScript.PHP.CodeGen.Common
--( runAtom
-- , atomPS
-- , atom
-- , atomModuleName
( phpModuleName
, ModuleType(..)
-- , toAtomName
-- , toVarName
-- , identToAtomName
, identToVar
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


data ModuleType = ForeignModule | PureScriptModule

phpModuleName :: ModuleName -> ModuleType -> Text
phpModuleName (ModuleName pns) moduleType = pns  -- intercalate "_" ((toAtomName) `map` pns) <>
  -- case moduleType of
  --   ForeignModule    -> "@foreign"
  --   PureScriptModule -> "@ps"

toVarName :: Text -> Text
toVarName v = T.pack $ "$" ++ T.unpack v

identToVar :: Ident -> Text
identToVar = toVarName . runIdent
