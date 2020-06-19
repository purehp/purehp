module Debugger (cpprint, traceShowIdPP) where

import Prelude
import Control.Monad.IO.Class
import Text.Show.Pretty hiding (Value)
import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise
import Debug.Trace

cpprint :: (MonadIO m, Show a) => a -> m ()
cpprint = liftIO . putStrLn . hscolour TTY defaultColourPrefs False False "" False . ppShow

traceShowIdPP :: (Applicative f, Show a) => a -> f ()
traceShowIdPP = traceM . hscolour TTY defaultColourPrefs False False "" False . ppShow
