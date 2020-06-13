module Debugger (cpprint) where

import Prelude
import Control.Monad.IO.Class
import Text.Show.Pretty hiding (Value)
import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise

cpprint :: (MonadIO m, Show a) => a -> m ()
cpprint = liftIO . putStrLn . hscolour TTY defaultColourPrefs False False "" False . ppShow
