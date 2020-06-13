module Build (parser) where

import qualified Options.Applicative as Opts

parser :: Opts.Parser (IO ())
parser = pure $ pure ()
