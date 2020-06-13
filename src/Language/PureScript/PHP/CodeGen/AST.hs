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

data PHP = PHP

-- | Possibly qualified atom
-- TODO : This is not really an atom, each part is an atom.
data Atom
  = Atom (Maybe Text) Text
  | AtomPS (Maybe Text) PSString
  deriving (Show, Eq)
