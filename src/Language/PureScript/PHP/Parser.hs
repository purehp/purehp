module Language.PureScript.PHP.Parser (parseFile) where

import           Prelude.Compat

import           Data.Text        (Text)
import qualified Data.Text        as T
import           Text.Parsec      ((<|>))
import qualified Text.Parsec      as P
import qualified Text.Parsec.Char as PC

parseFile :: P.SourceName -> Text -> Either P.ParseError [(Text, Int)]
parseFile = error "Missing"
