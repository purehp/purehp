{-# LANGUAGE GADTs #-}

-- |
-- This module generates code in the simplified Erlang intermediate representation from Purescript code
--
module Language.PureScript.PHP.CodeGen
  ( module AST
  , moduleToPHP
  ) where

import           Prelude.Compat

import           Language.PureScript.PHP.CodeGen.AST       as AST

import           Control.Monad                             (replicateM, unless)
import           Data.Foldable
import           Data.List                                 (nub)
import qualified Data.Text                                 as T
import           Data.Traversable

import           Data.Set                                  (Set)
import qualified Data.Set                                  as Set

import           Control.Applicative                       ((<|>))
import           Control.Arrow                             (first, second)
import           Control.Monad.Error.Class                 (MonadError (..))
import           Control.Monad.Reader                      (MonadReader (..))
import           Control.Monad.Writer                      (MonadWriter (..))
import qualified Data.Map                                  as M
import           Data.Maybe                                (catMaybes,
                                                            fromMaybe, mapMaybe)

import           Control.Monad.Supply.Class

import           Language.PureScript.AST                   (SourceSpan,
                                                            nullSourceSpan)
import qualified Language.PureScript.Constants             as C
import           Language.PureScript.CoreFn                hiding
                                                            (moduleExports)
import           Language.PureScript.Environment           as E
import           Language.PureScript.Errors                (ErrorMessageHint (..))
import           Language.PureScript.Names
import           Language.PureScript.Options
import           Language.PureScript.Traversals            (sndM)
import           Language.PureScript.Types

import           Language.PureScript.PHP.CodeGen.Common
import           Language.PureScript.PHP.CodeGen.Optimizer
import           Language.PureScript.PHP.Errors            (MultipleErrors,
                                                            addHint,
                                                            errorMessage,
                                                            rethrow,
                                                            rethrowWithPosition)
import           Language.PureScript.PHP.Errors.Types

import           Debug.Trace                               (traceM)

moduleToPHP :: forall m.
     (Monad m, MonadReader Options m, MonadSupply m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => E.Environment
  -> Module Ann
  -> [(T.Text, Int)]
  -> m ([T.Text], [PHP])
moduleToPHP env (Module _ _ mn _ _ declaredExports foreigns decls) foreignExports =
  error "Missing moduleToPHP"
