{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Data types for the intermediate simplified-PHP AST
--
module Language.PureScript.PHP.CodeGen.AST where

import Prelude.Compat

import Data.Text (Text)

import Control.Monad.Identity
import Control.Arrow (second)

import Language.PureScript.PSString (PSString)
import Language.PureScript.AST.SourcePos

data PHP
  -- | A numeric literal
  = PNumericLiteral (Either Integer Double)
  -- | A string literal
  | PStringLiteral PSString
  -- | Top-level function definition
  | PFunctionDef (Maybe SourceSpan) [Text] PHP
  -- | Variable bind
  | PVarBind Text PHP
  -- | A variable
  | PVar Text
  -- | A function reference f/1
  | PFunRef PSString Int
  -- | A fun definition
  | PFunFull (Maybe Text) [(PFunBinder, PHP)]
  -- | Function application
  | PApp PHP [PHP]
  -- | Block
  | PBlock [PHP]
  -- | An array
  | PArrayLiteral [PHP]
  -- | An associative array
  | PAssociativeArrayLiteral [(PSString, PHP)]

  deriving (Show, Eq)

-- | Simple 0-arity version of PFun1
pattern PFun0 :: Maybe Text -> PHP -> PHP
pattern PFun0 name e = PFunFull name [(PFunBinder [], e)]

-- | Simple fun definition fun f(X) -> e end (arity 1 with single head with simple variable pattern, name optional)
pattern PFun1 :: Maybe Text -> Text -> PHP -> PHP
pattern PFun1 name var e = PFunFull name [(PFunBinder [PVar var], e)]

extractVars :: [PHP] -> Maybe [Text]
extractVars = traverse var
  where var (PVar x) = Just x
        var _ = Nothing

-- | Simple arity-N versions of PFun1
pattern PFunN :: Maybe Text -> [Text] -> PHP -> PHP
pattern PFunN name vars e <- PFunFull name [(PFunBinder (extractVars -> Just vars), e)] where
  PFunN name vars e = PFunFull name [(PFunBinder (map PVar vars), e)]

data PFunBinder
  = PFunBinder [PHP]
  deriving (Show, Eq)

data Guard = Guard PHP
  deriving (Show, Eq)
