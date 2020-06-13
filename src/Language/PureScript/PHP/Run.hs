{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}


module Language.PureScript.PHP.Run  where

import           Prelude.Compat

import           Control.Monad                          (when)
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import qualified Language.PureScript                    as P
-- import           Language.PureScript.PHP.CodeGen.AST    (Atom (..))
-- import           Language.PureScript.PHP.CodeGen.Common (ModuleType (..),
--                                                          atomModuleName,
--                                                          runAtom)
import           System.Exit                            (ExitCode (..),
                                                         exitFailure)
import           System.FilePath.Glob                   (glob)
import           System.IO
import           System.Process                         (readProcessWithExitCode)

runProgram :: Text -> IO ()
runProgram runModule = error "Missing runProgram"
