{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.PHP.Pretty
  ( prettyPrintPHP
) where

import Prelude.Compat

import Debug.Trace

import Control.Arrow ((<+>))
import Control.Monad (forM, mzero)
import Control.Monad.State (StateT, evalStateT)
import Control.PatternArrows
import qualified Control.Arrow as A

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.AST (SourceSpan(..))
import Language.PureScript.PHP.CodeGen.Common
import Language.PureScript.PHP.CodeGen.AST
import Language.PureScript.Comments
import Language.PureScript.Crash
import Language.PureScript.Pretty.Common
import Language.PureScript.PSString (PSString, decodeString, prettyPrintStringJS)

literals :: (Emit gen) => Pattern PrinterState PHP gen
literals = mkPattern' match'
  where
    match' :: (Emit gen) => PHP -> StateT PrinterState Maybe gen
    match' php = (addMapping' (getSourceSpan php) <>) <$> match php

    match :: (Emit gen) => PHP -> StateT PrinterState Maybe gen
    match (PNumericLiteral _ n) = return $ emit $ T.pack $ either show show n
    match (PStringLiteral _ s) = return $ emit $ prettyPrintStringJS s
    match (PBooleanLiteral _ True) = return $ emit "true"
    match (PBooleanLiteral _ False) = return $ emit "false"
    match (PArrayLiteral _ xs) = mconcat <$> sequence
      [ return $ emit "array( "
      , intercalate (emit ", ") <$> forM xs prettyPrintPHP'
      , return $ emit " )"
      ]
    match (PBlock _ sts) = mconcat <$> sequence
      [ return $ emit "{\n"
      , withIndent $ prettyStatements sts
      , return $ emit "\n"
      , currentIndent
      , return $ emit "}"
      ]
    match (PVar _ ident) = return $ emit $ "$" <> ident
    match (PVariableIntroduction _ ident value) = mconcat <$> sequence
      [ return $ emit $ "$" <> ident
      , maybe (return mempty) (fmap (emit " = " <>) . prettyPrintPHP') value
      ]
    match (PClassVariableIntroduction _ ident value) = mconcat <$> sequence
      [ return $ emit $ "var $" <> ident
      , maybe (return mempty) (fmap (emit " = " <>) . prettyPrintPHP') value
      ]
    match (PAssignment _ target value) = mconcat <$> sequence
      [ prettyPrintPHP' target
      , return $ emit " = "
      , prettyPrintPHP' value
      ]
    match (PReturn _ value) = mconcat <$> sequence
      [ return $ emit "return "
      , prettyPrintPHP' value
      ]
    match (PReturnNoResult _) = return $ emit "return"
    match (PComment _ com php) = mconcat <$> sequence
      [ return $ emit "\n"
      , mconcat <$> forM com comment
      , prettyPrintPHP' php
      ]
    match _ = mzero

    comment :: (Emit gen) => Comment -> StateT PrinterState Maybe gen
    comment (LineComment com) = fmap mconcat $ sequence
      [ currentIndent
      , return $ emit "//" <> emit com <> emit "\n"
      ]
    comment (BlockComment com) = fmap mconcat $ sequence $
      [ currentIndent
      , return $ emit "/**\n"
      ] ++
      map asLine (T.lines com) ++
      [ currentIndent
      , return $ emit " *\n"
      , currentIndent
      ]
      where
        asLine :: (Emit gen) => Text -> StateT PrinterState Maybe gen
        asLine s = do
          i <- currentIndent
          return $ i <> emit " * " <> (emit . removeComments) s <> emit "\n"

        removeComments :: Text -> Text
        removeComments t =
          case T.stripPrefix "*/" t of
            Just rest -> removeComments rest
            Nothing -> case T.uncons t of
              Just (x, xs) -> x `T.cons` removeComments xs
              Nothing -> ""

prettyStatements :: (Emit gen) => [PHP] -> StateT PrinterState Maybe gen
prettyStatements sts = do
  php <- forM sts prettyPrintPHP'
  indentString <- currentIndent
  return $ intercalate (emit "\n") $ map ((<> emit ";") . (indentString <>)) php

prettyPrintPHP :: [PHP] -> Text
prettyPrintPHP = maybe (internalError "Incomplete pattern") runPlainString . flip evalStateT (PrinterState 0) . prettyStatements

recr :: Pattern PrinterState PHP (Maybe SourceSpan, PHP)
recr = mkPattern match
  where
    match (PObjectLiteral ss fields) = Just (ss, PBlock ss fields)
    match _ = Nothing

lam :: Pattern PrinterState PHP ((Maybe Text, [Text], Maybe SourceSpan), PHP)
lam = mkPattern match
  where
    match (PFunction ss name args ret) = Just ((name, args, ss), ret)
    match _ = Nothing

-- | Generate an indented, pretty-printed string representing a PHP expression
prettyPrintPHP' :: (Emit gen) => PHP -> StateT PrinterState Maybe gen
prettyPrintPHP' = A.runKleisli (runPattern matchValue)
  where
    matchValue :: (Emit gen) => Pattern PrinterState PHP gen
    matchValue = buildPrettyPrinter operators (literals <+> fmap parensPos matchValue)
    operators :: (Emit gen) => OperatorTable PrinterState PHP gen
    operators =
      OperatorTable [
        [ Wrap lam $ \(name, args, ss) ret -> addMapping' ss <>
          emit ("function "
            <> fromMaybe "" name
            <> "(" <> intercalate ", " (("$" <>) <$> args) <> ") ")
            <> ret
        ]
      , [ Wrap recr $ \ss fields -> addMapping' ss <>
          emit "new class "
            <> fields
         ]
      ]

{-

data PrinterState = PrinterState { indent :: Int, transformFilename :: String -> String }

withIndent :: StateT PrinterState Maybe gen -> StateT PrinterState Maybe gen
withIndent = withIndent' 2

withIndent' :: Int -> StateT PrinterState Maybe gen -> StateT PrinterState Maybe gen
withIndent' indentSize action = do
  modify $ \st -> st { indent = indent st + indentSize }
  result <- action
  modify $ \st -> st { indent = indent st - indentSize }
  return result

currentIndent :: (Emit gen) => StateT PrinterState Maybe gen
currentIndent = do
  current <- get
  return $ emit $ T.replicate (indent current) " "

literals :: (Emit gen) => Pattern PrinterState PHP gen
literals = mkPattern' match
  where

    match :: (Emit gen) => PHP -> StateT PrinterState Maybe gen
    match (PNumericLiteral _ n) = return $ emit $ T.pack $ either show show n
    match (PStringLiteral _ s)  = return $ emit $ prettyPrintString s
    -- match (PVarBind x e) = mconcat <$> sequence
    --   [ return $ emit $ "$" <> x <> " = "
    --   , prettyPrintPHP' e
    --   ]
    -- match (PVar x) = return $ emit x
    -- match (PArrayLiteral es) = mconcat <$> sequence
    --   [ return $ emit "array( "
    --   , intercalate (emit ", ") <$> mapM prettyPrintPHP' es
    --   , return $ emit " )"
    --   ]
    -- match (PAssociativeArrayLiteral es) = do
    --   els <- mapM (\(s, v) -> do
    --                  php <- prettyPrintPHP' v
    --                  return $ mconcat [ emit $ prettyPrintStringPHP s
    --                                   , emit " => "
    --                                   , php
    --                                   ]
    --              ) es
    --   return $ mconcat
    --     [ emit "array( "
    --     , intercalate (emit ", ") els
    --     , emit " )" ]
    -- match (PFunFull name binders) = mconcat <$> sequence
    --   [ return $ emit "function() {\n"
    --   , withIndent $ prettyPrintBinders binders
    --   , currentIndent
    --   , return $ emit "}"
    --   ]
    --   where
    --     prettyPrintBinders :: (Emit gen) => [(PFunBinder, PHP)] -> StateT PrinterState Maybe gen
    --     prettyPrintBinders bs = intercalate (emit ";\n") <$> mapM prettyPrintBinder bs

    --     matchBody (PBlock es) = prettyPrintBlockBody es
    --     matchBody e = mconcat <$> sequence
    --       [ currentIndent
    --       , prettyPrintPHP' e
    --       ]

    --     prettyPrintBinder :: (Emit gen) => (PFunBinder, PHP) -> StateT PrinterState Maybe gen
    --     prettyPrintBinder (PFunBinder binds, e') = do
    --       b' <- intercalate (emit ", ") <$> mapM prettyPrintPHP' binds
    --       v <- matchBody e'
    --       indentStr <- currentIndent
    --       return $ indentStr <> emit (fromMaybe "" name) <> parensPos b' <> emit " -> \n" <> v
    match _                     = mzero





    -- match (PFunctionDef ss xs e) = mconcat <$> sequence (
    --   (case ss of
    --     Just (SourceSpan { spanName = spanName, spanStart = spanStart }) ->
    --        [ do
    --            t <- transformFilename <$> get
    --            return $ emit $ "-file(\"" <> T.pack (t spanName) <> "\", " <> T.pack (show $ sourcePosLine spanStart)
    --        ]
    --      _ -> [])
    --   <>
    --   [ prettyPrintPHP' e
    --   , return $ emit ";"
    --   ])



-- fromChar :: Char -> Word16
-- fromChar = toEnum . fromEnum

-- prettyPrintBlockBody :: (Emit gen) => [PHP] -> StateT PrinterState Maybe gen
-- prettyPrintBlockBody es = do
--   es' <- mapM prettyPrintPHP' es
--   indentStr <- currentIndent
--   let lns = intercalate (emit ",\n" <> indentStr) es'
--   pure $ indentStr <> lns

-- -- | Pretty print a PSString.
-- prettyPrintStringPHP :: PSString -> Text
-- prettyPrintStringPHP = prettyPrintString

-- app :: (Emit gen) => Pattern PrinterState PHP (gen, PHP)
-- app = mkPattern' match
--   where
--     -- match (EApp val args) = do
--     --   jss <- traverse prettyPrintPHP' args
--     --   return (intercalate (emit ", ") jss, val)
--     match _ = mzero

prettyPrintPHP :: (String -> String) -> [PHP] -> Text
prettyPrintPHP transformFilename = error "PrettyPrint" -- maybe (internalError "Incomplete pattern") runPlainString . flip evalStateT (PrinterState 0 transformFilename) . prettyStatements

-- prettyStatements :: (Emit gen) => [PHP] -> StateT PrinterState Maybe gen
-- prettyStatements sts = do
--   jss <- forM sts prettyPrintPHP'
--   indentString <- currentIndent
--   return $ intercalate (emit "\n") $ map ((<> emit ";") . (indentString <>)) jss

-- -- | Generate an indented, pretty-printed string representing a Javascript expression
-- prettyPrintPHP' :: (Emit gen) => PHP -> StateT PrinterState Maybe gen
-- prettyPrintPHP' = A.runKleisli $ runPattern matchValue
--   where
--     matchValue :: (Emit gen) => Pattern PrinterState PHP gen
--     matchValue = buildPrettyPrinter operators (literals <+> fmap parensPos matchValue)
--     operators :: (Emit gen) => OperatorTable PrinterState PHP gen
--     operators =
--       OperatorTable []
-}
