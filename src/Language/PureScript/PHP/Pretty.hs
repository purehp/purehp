{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.PHP.Pretty
  ( prettyPrintPHP
) where

import Prelude.Compat

import Debugger
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
      [ return $ emit "["
      , intercalate (emit ", ") <$> forM xs prettyPrintPHP'
      , return $ emit "]"
      ]
    match (PAssociativeArrayField _ n v) = mconcat <$> sequence
      [ return $ emit $ "'" <> n <> "'"
      , return $ emit " => "
      , prettyPrintPHP' v
      ]
    match (PBlock _ True sts) = mconcat <$> sequence
      [ return $ emit "{\n"
      , withIndent $ prettyStatements sts
      , return $ emit "\n"
      , currentIndent
      , return $ emit "}"
      ]
    match (PBlock _ False sts) = mconcat <$> sequence
      [ return $ emit "\n"
      , withIndent $ prettyStatements sts
      ]
    match (PVar _ ident) = return $ emit $ "$" <> ident
    match (PVar' _ ident) = return $ emit ident
    match (PVariableIntroduction _ ident value) = mconcat <$> sequence
      [ return $ emit $ "$" <> ident
      , maybe (return $ emit ";") (fmap (\v -> emit " = " <> v <> emit ";") . prettyPrintPHP') value
      ]
    match (PClassVariableIntroduction _ ident value) = mconcat <$> sequence
      [ return $ emit $ "var $" <> ident
      , maybe (return $ emit ";") (fmap (\v -> emit " = " <> v <> emit ";") . prettyPrintPHP') value
      ]
    match (PAssignment _ target value) = mconcat <$> sequence
      [ prettyPrintPHP' target
      , return $ emit " = "
      , prettyPrintPHP' value
      , return $ emit ";"
      ]
    match (PWhile _ cond sts) = mconcat <$> sequence
      [ return $ emit "while ("
      , prettyPrintPHP' cond
      , return $ emit ") "
      , prettyPrintPHP' sts
      ]
    match (PFor _ ident start end sts) = mconcat <$> sequence
      [ return $ emit $ "for ($" <> ident <> " = "
      , prettyPrintPHP' start
      , return $ emit $ "; $" <> ident <> " < "
      , prettyPrintPHP' end
      , return $ emit $ "; $" <> "++) "
      , prettyPrintPHP' sts
      ]
    match (PForIn _ ident obj sts) = mconcat <$> sequence
      [ return $ emit $ "foreach ($"
      , prettyPrintPHP' obj
      , return $ emit $ " as $" <> ident <> ") "
      , prettyPrintPHP' sts
      ]
    match (PIfElse _ cond thens elses) = mconcat <$> sequence
      [ return $ emit "if ("
      , prettyPrintPHP' cond
      , return $ emit ") "
      , prettyPrintPHP' thens
      , maybe (return mempty) (fmap (emit " else " <>) . prettyPrintPHP') elses
      ]
    match (PReturn _ value) = mconcat <$> sequence
      [ return $ emit "return "
      , prettyPrintPHP' value
      , return $ emit ";"
      ]
    match (PReturnNoResult _) = return $ emit "return"
    match (PThrow _ value) = mconcat <$> sequence
      [ return $ emit "throw "
      , prettyPrintPHP' value
      , return $ emit ";"
      ]
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


recr :: Pattern PrinterState PHP (Maybe SourceSpan, PHP)
recr = mkPattern match
  where
    match (PObjectLiteral ss fields) = Just (ss, PArrayLiteral ss fields)
    match _ = Nothing

class_ :: Pattern PrinterState PHP ((Maybe Text, Maybe SourceSpan), PHP)
class_ = mkPattern match
  where
    match (PClass ss name ret) = Just ((name, ss), ret)
    match _ = Nothing

met :: Pattern PrinterState PHP (([Text], Text, [Text], Maybe SourceSpan), PHP)
met = mkPattern match
  where
    match (PMethod ss pre name args ret) = Just ((pre, name, args, ss), ret)
    match _ = mzero

arr :: Pattern PrinterState PHP (([Text], Maybe SourceSpan), PHP)
arr = mkPattern match
  where
    match (PArrowFunction ss args ret) = Just ((args, ss), ret)
    match _ = Nothing

accessor :: Pattern PrinterState PHP (Text, PHP)
accessor = mkPattern match
  where
    match (PIndexer _ (PStringLiteral _ prop) val) =
      case decodeString prop of
        Just s | isValidPHPIdentifier s -> Just (s, val)
        _ -> Nothing
    match _ = Nothing

saccessor :: Pattern PrinterState PHP (Text, PHP)
saccessor = mkPattern match
  where
    match (PStaticIndexer _ (PStringLiteral _ prop) val) =
      case decodeString prop of
        Just s | isValidPHPIdentifier s -> Just (s, val)
        _ -> Nothing
    match _ = Nothing

indexer :: (Emit gen) => Pattern PrinterState PHP (gen, PHP)
indexer = mkPattern' match
  where
    match (PIndexer _ index val) = (,) <$> prettyPrintPHP' index <*> pure val
    match _ = mzero

lam :: Pattern PrinterState PHP ((Maybe Text, [Text], Maybe SourceSpan), PHP)
lam = mkPattern match
  where
    match (PFunction ss name args ret) = Just ((name, args, ss), ret)
    match _ = Nothing

app :: (Emit gen) => Pattern PrinterState PHP (gen, PHP)
app = mkPattern' match
  where
    match (PApp _ val args) = do
      php <- traverse prettyPrintPHP' args
      return (intercalate (emit ", ") php, val)
    match _ = mzero

instanceOf :: Pattern PrinterState PHP (PHP, PHP)
instanceOf = mkPattern match
  where
    match (PInstanceOf _ val ty) = Just (val, ty)
    match _ = Nothing

unary' :: (Emit gen) => UnaryOperator -> (PHP -> Text) -> Operator PrinterState PHP gen
unary' op mkStr = Wrap match (<>)
  where
    match :: (Emit gen) => Pattern PrinterState PHP (gen, PHP)
    match = mkPattern match'
      where
        match' (PUnary _ op' val) | op' == op = Just (emit $ mkStr val, val)
        match' _ = Nothing

unary :: (Emit gen) => UnaryOperator -> Text -> Operator PrinterState PHP gen
unary op str = unary' op (const str)

negateOperator :: (Emit gen) => Operator PrinterState PHP gen
negateOperator = unary' PNegate (\v -> if isNegate v then "- " else "-")
  where
    isNegate (PUnary _ PNegate _) = True
    isNegate _ = False

binary :: (Emit gen) => BinaryOperator -> Text -> Operator PrinterState PHP gen
binary op str = AssocL match (\v1 v2 -> v1 <> emit (" " <> str <> " ") <> v2)
  where
    match :: Pattern PrinterState PHP (PHP, PHP)
    match = mkPattern match'
      where
        match' (PBinary _ op' v1 v2) | op' == op = Just (v1, v2)
        match' _ = Nothing

prettyStatements :: (Emit gen) => [PHP] -> StateT PrinterState Maybe gen
prettyStatements sts = do
  php <- forM sts prettyPrintPHP'
  indentString <- currentIndent
  return $ intercalate (emit "\n") $ map (indentString <>) php

prettyPrintPHP :: [PHP] -> Text
prettyPrintPHP = maybe (internalError "Incomplete pattern") runPlainString . flip evalStateT (PrinterState 0) . prettyStatements

-- | Generate an indented, pretty-printed string representing a PHP expression
prettyPrintPHP' :: (Emit gen) => PHP -> StateT PrinterState Maybe gen
prettyPrintPHP' = A.runKleisli (runPattern matchValue)
  where
    matchValue :: (Emit gen) => Pattern PrinterState PHP gen
    matchValue = buildPrettyPrinter operators (literals <+> fmap parensPos matchValue)
    operators :: (Emit gen) => OperatorTable PrinterState PHP gen
    operators =
      OperatorTable
        [ [ Wrap recr $ \ss fields -> addMapping' ss <>
            emit "(object) "
              <> fields ]
        , [ Wrap class_ $ \(name, ss) ret -> addMapping' ss <>
            emit ("class " <> (maybe "" (<> " ") name))
              <> ret ]
        , [ Wrap met $ \(pre, name, args, ss) ret -> addMapping' ss <>
            emit (T.unwords pre
              <> " function "
              <> name
              <> "(" <> intercalate ", " (("$" <>) <$> args) <> ") ")
              <> ret ]
        , [ Wrap arr $ \(args, ss) ret -> addMapping' ss <>
            emit ("fn("
              <> intercalate ", " (("$" <>) <$> args) <> ") => ")
              <> ret ]
        , [ Wrap indexer $ \index val -> val <> emit "[" <> index <> emit "]" ]
        , [ Wrap accessor $ \prop val -> val <> emit "->" <> emit prop ]
        , [ Wrap saccessor $ \prop val -> val <> emit "::" <> emit prop ]
        , [ Wrap app $ \args val -> val <> emit "(" <> args <> emit ")" ]
        , [ unary PNew "new " ]
        , [ Wrap lam $ \(name, args, ss) ret -> addMapping' ss <>
            emit ("function "
              <> fromMaybe "" name
              <> "(" <> intercalate ", " (("$" <>) <$> args) <> ") ")
              <> ret ]
        , [ unary PNot "!"
          , unary PBitwiseNot "~"
          , unary PPositive "+"
          , negateOperator ]
        , [ binary PMultiply "*"
          , binary PDivide "/"
          , binary PModulus "%" ]
        , [ binary PAdd "+"
          , binary PSubtract "-" ]
        , [ binary PShiftLeft "<<"
          , binary PShiftRight ">>"
          , binary PZeroFillShiftRight ">>>" ]
        , [ binary PLessThan "<"
          , binary PLessThanOrEqualTo "<="
          , binary PGreaterThan ">"
          , binary PGreaterThanOrEqualTo ">="
          , AssocR instanceOf $ \v1 v2 -> v1 <> emit " instanceof " <> v2 ]
        , [ binary PEqualsTo "==="
          , binary PNotEqualTo "!==" ]
        , [ binary PBitwiseAnd "&" ]
        , [ binary PBitwiseXor "^" ]
        , [ binary PBitwiseOr "|" ]
        , [ binary PAnd "&&" ]
        , [ binary POr "||" ]
      ]
