module Debugger (cpprint) where

import Prelude
import Text.Show.Pretty hiding (Value)
import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise

cpprint :: (Show a) => a -> IO ()
cpprint = putStrLn . hscolour TTY defaultColourPrefs False False "" False . ppShow
