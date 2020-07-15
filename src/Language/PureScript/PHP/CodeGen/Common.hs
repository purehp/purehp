{-# LANGUAGE OverloadedStrings #-}
-- |
-- Common code generation utility functions
--
module Language.PureScript.PHP.CodeGen.Common where

import           Prelude.Compat

import           Data.Char
import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           Language.PureScript.Crash
import           Language.PureScript.Names

-- | A prefix for invalid names.
phpPrefix :: Text
phpPrefix = "__"

-- | Creates a PHP module name, returning a tuple (Namespace, Classname)
-- TODO: find a more performant version?
moduleNameToPHP :: ModuleName -> ([Text], Text)
moduleNameToPHP (ModuleName mn) =
  case reverse $ T.splitOn "." mn of
    [] -> error "Shouldn't happen."
    -- TODO are we really allowed to prefix the module names here?
    (h:tl) -> (reverse tl, if nameIsPHPBuiltIn h then phpPrefix <> h else h)

-- | This one flattens the name
-- TODO: temp, can we find something better ?
moduleNameToFlatPHP :: ModuleName -> Text
moduleNameToFlatPHP mn =
  let (ns, n) = moduleNameToPHP mn
  in T.intercalate "\\" (ns ++ [n])

-- TODO: temp
moduleNameToVariablePHP :: ModuleName -> Text
moduleNameToVariablePHP mn =
  let (ns, n) = moduleNameToPHP mn
  in "__" <> (T.intercalate "_" (ns ++ [n]))

-- | Convert an 'Ident' into a valid PHP identifier:
--
-- * Alphanumeric characters are kept unmodified.
--
-- * Reserved php identifiers are prefixed with '__'
identToPHP :: Ident -> Text
identToPHP (Ident name)   = anyNameToPHP name
identToPHP (GenIdent _ _) = internalError "GenIdent in identToPHP"
identToPHP UnusedIdent    = "$__unused"

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
  | nameIsPHPReserved name || nameIsPHPBuiltIn name = phpPrefix <> name
  | otherwise = T.concatMap identCharToText name

-- | Test if a string is a valid PHP identifier as-is. Note that, while
-- a return value of 'True' guarantees that the string is a valid PHP
-- identifier, a return value of 'False' does not guarantee that the string is
-- not a valid PHP identifier. That is, this check is more conservative than
-- absolutely necessary.
isValidPHPIdentifier :: Text -> Bool
isValidPHPIdentifier s =
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
