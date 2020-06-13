module Language.PureScript.PHP.Make where

import           Prelude

import           Control.Monad hiding (sequence)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Writer.Class (MonadWriter(..))
import qualified Language.PureScript.CoreFn as CF
import           Language.PureScript.PHP.Make.Monad
import qualified Language.PureScript as P
import           Control.Monad.Supply
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NEL
-- import           Language.PureScript.Erl.Parser (parseFile)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           System.FilePath ((</>))
import           Data.Foldable (for_, minimum)
import           System.Directory (getCurrentDirectory)
import           Data.Version (showVersion)
import           Data.Time.Clock (UTCTime)

import qualified Paths_purehp as Paths

-- import           Language.PureScript.Erl.CodeGen.Common (erlModuleName, atomModuleName, atom, ModuleType(..))
-- import           Language.PureScript.Erl.CodeGen (moduleToErl)
-- import           Language.PureScript.Erl.CodeGen.Optimizer (optimize)
-- import           Language.PureScript.Erl.Pretty (prettyPrintErl)
import           Language.PureScript.PHP.Errors
import           Language.PureScript.PHP.Errors.Types

data MakeActions m = MakeActions
  { codegen :: CF.Module CF.Ann -> SupplyT m ()
  -- ^ Run the code generator for the module and write any required output files.
  , ffiCodegen :: CF.Module CF.Ann -> m ()
  , getOutputTimestamp :: P.ModuleName -> m (Maybe UTCTime)
  -- ^ Get the timestamp for the output files for a module. This should be the
  -- timestamp for the oldest modified file, or 'Nothing' if any of the required
  -- output files are missing.
  }

buildActions :: String -> P.Environment -> M.Map P.ModuleName FilePath -> Bool -> MakeActions Make
buildActions outputDir env foreigns usePrefix = error "Missing buildActions"
