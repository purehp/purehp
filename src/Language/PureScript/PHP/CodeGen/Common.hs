-- |
-- Common code generation utility functions
--
module Language.PureScript.PHP.CodeGen.Common where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.Crash
import Language.PureScript.Names


-- | Creates a PHP module name
-- TODO: check that prefix thing
moduleNameToPHP :: ModuleName -> Text
moduleNameToPHP (ModuleName pns) =
  let name = T.intercalate "\\" (runProperName `map` pns)
  in if nameIsPHPBuiltIn name then "__" <> name else name

-- | Convert an 'Ident' into a valid PHP identifier:
--
-- * Alphanumeric characters are kept unmodified.
--
-- * Reserved php identifiers are prefixed with '__'
identToPHP :: Ident -> Text
identToPHP (Ident name) = anyNameToPHP name
identToPHP (GenIdent _ _) = internalError "GenIdent in identToPHP"
identToPHP UnusedIdent = "$__unused"

-- | Convert a 'ProperName' into a valid JavaScript identifier:
--
-- * Alphanumeric characters are kept unmodified.
--
-- * Reserved javascript identifiers are prefixed with '__'
properToPHP :: ProperName a -> Text
properToPHP = anyNameToPHP . runProperName

-- | Convert any name into a valid PHP identifier.
--
-- Note that this function assumes that the argument is a valid PureScript
-- identifier (either an 'Ident' or a 'ProperName') to begin with; as such it
-- will not produce valid JavaScript identifiers if the argument e.g. begins
-- with a digit. Prefer 'identToPHP' or 'properToPHP' where possible.
anyNameToPHP :: Text -> Text
anyNameToPHP name
  | nameIsPHPReseved name || nameIsPHPBuiltIn name = "__" <> name
  | otherwise = T.concatMap identCharToText name

-- | Test if a string is a valid PHP identifier as-is. Note that, while
-- a return value of 'True' guarantees that the string is a valid PHP
-- identifier, a return value of 'False' does not guarantee that the string is
-- not a valid PHP identifier. That is, this check is more conservative than
-- absolutely necessary.
isValidPHPIdentifier :: Text -> Bool
isvalidPHPIdentifier s =
  and
    [ not (T.null s)
    , isAlpha (T.head s)
    , s == anyNameToPHP s
    ]

-- | Attempts to find a human-readable name for a symbol, if none has been specified returns the
-- ordinal value.
-- TODO: See if and what's actually necessary here.
identCharToText :: Char -> Text
identCharToText c | isAlphaNum c = T.singleton c
identCharToText c = '_' `T.cons` T.pack(show (ord c))

-- | Checks whether an identifier name is reserved in PHP.
nameIsPHPReserved :: Text -> Bool
nameIsPHPReserved name =
  name `elem` phpAnyReserved

-- | Checks whether a name matches a built-in value in PHP.
-- TODO: create the list
nameIsPHPBuiltIn :: Text -> Bool
nameIsPHPBuiltIn name =
  name `elem`
  []

-- TODO: Create the list
phpAnyReserved :: [Text]
phpAnyReserved =
  concat
  []

-- old

-- toVarName :: Text -> Text
-- toVarName v = T.pack $ "$" ++ T.unpack v

-- identToVar :: Ident -> Text
-- identToVar = toVarName . runIdent
