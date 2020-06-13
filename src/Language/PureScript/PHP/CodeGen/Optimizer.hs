-- |
-- This module optimizes code in the simplified-Erlang intermediate representation.
--
-- The following optimizations are supported:
--
--  * Inlining of (>>=) and ret for the Eff monad
--
module Language.PureScript.PHP.CodeGen.Optimizer (optimize) where

import Prelude.Compat

import Control.Monad.Supply.Class (MonadSupply)

import Language.PureScript.PHP.CodeGen.AST
-- import Language.PureScript.PHP.CodeGen.Optimizer.MagicDo
-- import Language.PureScript.PHP.CodeGen.Optimizer.Blocks
-- import Language.PureScript.PHP.CodeGen.Optimizer.Common
-- import Language.PureScript.PHP.CodeGen.Optimizer.Inliner
-- import Language.PureScript.PHP.CodeGen.Optimizer.Guards

-- |
-- Apply a series of optimizer passes to simplified Javascript code
-- import qualified Language.PureScript.PHP.CodeGen.Constants as EC
-- |
-- Apply a series of optimizer passes to simplified Javascript code
--
optimize :: MonadSupply m => PHP -> m PHP
optimize php = pure php
