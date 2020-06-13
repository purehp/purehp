module Language.PureScript.PHP.Pretty (
  prettyPrintPHP
) where

import           Prelude                                ()
import           Prelude.Compat

import           Control.Arrow                          ((<+>))
import qualified Control.Arrow                          as A
import           Control.Monad.State                    hiding (sequence)
import           Control.PatternArrows

import           Language.PureScript.AST.SourcePos
import           Language.PureScript.Crash
import           Language.PureScript.PHP.CodeGen.AST
import           Language.PureScript.PHP.CodeGen.Common
import           Language.PureScript.PSString

import           Data.Maybe                             (fromMaybe)
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           Data.Word                              (Word16)

import           Language.PureScript.Pretty.Common      (Emit, emit,
                                                         intercalate, parensPos,
                                                         runPlainString)


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
    match (PNumericLiteral n) = return $ emit $ T.pack $ either show show n
    match (PStringLiteral s) = return $ emit $ prettyPrintStringPHP s
    match (PVarBind x e) = mconcat <$> sequence
      [ return $ emit $ "$" <> x <> " = "
      , prettyPrintPHP' e
      ]
    match (PVar x) = return $ emit x
    match (PArrayLiteral es) = mconcat <$> sequence
      [ return $ emit "array( "
      , intercalate (emit ", ") <$> mapM prettyPrintPHP' es
      , return $ emit " )"
      ]
    match (PAssociativeArrayLiteral es) = do
      els <- mapM (\(s, v) -> do
                     php <- prettyPrintPHP' v
                     return $ mconcat [ emit $ prettyPrintStringPHP s
                                      , emit " => "
                                      , php
                                      ]
                 ) es
      return $ mconcat
        [ emit "array( "
        , intercalate (emit ", ") els
        , emit " )" ]


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



fromChar :: Char -> Word16
fromChar = toEnum . fromEnum

prettyPrintBlockBody :: (Emit gen) => [PHP] -> StateT PrinterState Maybe gen
prettyPrintBlockBody es = do
  es' <- mapM prettyPrintPHP' es
  indentStr <- currentIndent
  let lns = intercalate (emit ",\n" <> indentStr) es'
  pure $ indentStr <> lns

-- | Pretty print a PSString.
prettyPrintStringPHP :: PSString -> Text
prettyPrintStringPHP = prettyPrintString

app :: (Emit gen) => Pattern PrinterState PHP (gen, PHP)
app = mkPattern' match
  where
    -- match (EApp val args) = do
    --   jss <- traverse prettyPrintPHP' args
    --   return (intercalate (emit ", ") jss, val)
    match _ = mzero

prettyPrintPHP :: (String -> String) -> [PHP] -> Text
prettyPrintPHP transformFilename = maybe (internalError "Incomplete pattern") runPlainString . flip evalStateT (PrinterState 0 transformFilename) . prettyStatements

prettyStatements :: (Emit gen) => [PHP] -> StateT PrinterState Maybe gen
prettyStatements sts = do
  jss <- forM sts prettyPrintPHP'
  indentString <- currentIndent
  return $ intercalate (emit "\n") $ map ((<> emit ";") . (indentString <>)) jss

-- | Generate an indented, pretty-printed string representing a Javascript expression
prettyPrintPHP' :: (Emit gen) => PHP -> StateT PrinterState Maybe gen
prettyPrintPHP' = A.runKleisli $ runPattern matchValue
  where
    matchValue :: (Emit gen) => Pattern PrinterState PHP gen
    matchValue = buildPrettyPrinter operators (literals <+> fmap parensPos matchValue)
    operators :: (Emit gen) => OperatorTable PrinterState PHP gen
    operators =
      OperatorTable []
