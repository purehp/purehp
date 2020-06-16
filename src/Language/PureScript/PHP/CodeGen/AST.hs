-- |
-- Data types for the intermediate simplified-PHP AST
--
module Language.PureScript.PHP.CodeGen.AST where

import Prelude.Compat

import Control.Monad ((>=>))
import Control.Monad.Identity (Identity(..), runIdentity)
import Data.Text (Text)

import Language.PureScript.AST (SourceSpan(..))
import Language.PureScript.Comments
import Language.PureScript.PSString (PSString)
import Language.PureScript.Traversals

-- | Built-in unary operators
-- Copying js for now
data UnaryOperator
  = PNegate
  | PNot
  | PBitwiseNot
  | PPositive
  | PNew
  deriving (Show, Eq)

-- | Built-in binary operators
-- Copying js for now
data BinaryOperator
  = PAdd
  | PSubtract
  | PMultiply
  | PModulus
  | PEqualsTo
  | PNotEqualTo
  | PLessThan
  | PLessThanOrEqualTo
  | PGreaterThan
  | PGreaterThanOrEqualTo
  | PAnd
  | POr
  | PBitwiseAnd
  | PBitwiseOr
  | PBitwiseXor
  | PShiftLeft
  | PShiftRight
  | PZeroFillShiftRight
  deriving (Show, Eq)

data PHP
  = PNumericLiteral (Maybe SourceSpan) (Either Integer Double)
  -- ^ A numeric literal
  | PStringLiteral (Maybe SourceSpan) PSString
  -- ^ A string literal
  | PBooleanLiteral (Maybe SourceSpan) Bool
  -- ^ A boolean literal
  | PUnary (Maybe SourceSpan) UnaryOperator PHP
  -- ^ A unary operator application
  | PBinary (Maybe SourceSpan) BinaryOperator PHP PHP
  -- ^ A binary operator application
  | PArrayLiteral (Maybe SourceSpan) [PHP]
  -- ^ An array literal
  | PAssociativeArrayField (Maybe SourceSpan) Text PHP
  -- ^ A field declaration for an associative array
  | PIndexer (Maybe SourceSpan) PHP PHP
  -- ^ An array indexer expression
  | PObjectLiteral (Maybe SourceSpan) [PHP] -- [(PSString, PHP)]
  -- ^ A class literal (for records?), the text is to store the name. tbd
  | PFunction (Maybe SourceSpan) (Maybe Text) [Text] PHP
  -- ^ A function introduction (optional name, arguments, body)
  | PApp (Maybe SourceSpan) PHP [PHP]
  -- ^ Function application
  | PVar (Maybe SourceSpan) Text
  -- ^ Variable
  | PBlock (Maybe SourceSpan) [PHP]
  -- ^ A block of expressions in braces
  | PVariableIntroduction (Maybe SourceSpan) Text (Maybe PHP)
  -- ^ A variable introduction and optional initialization
  | PClassVariableIntroduction (Maybe SourceSpan) Text (Maybe PHP)
  -- ^ A class variable introduction
  | PAssignment (Maybe SourceSpan) PHP PHP
  -- ^ A variable assignment
  -- while
  -- for
  -- forin
  -- ifelse
  | PReturn (Maybe SourceSpan) PHP
  -- ^ Return statement
  | PReturnNoResult (Maybe SourceSpan)
  -- ^ Return statement with no return value
  -- throw
  -- instanceof
  | PComment (Maybe SourceSpan) [Comment] PHP
  -- ^ Commented php
  deriving (Show, Eq)

withSourceSpan :: SourceSpan -> PHP -> PHP
withSourceSpan withSpan = go where
  ss :: Maybe SourceSpan
  ss = Just withSpan

  go :: PHP -> PHP
  go (PNumericLiteral _ n) = PNumericLiteral ss n
  go (PStringLiteral _ s) = PStringLiteral ss s
  go (PBooleanLiteral _ b) = PBooleanLiteral ss b
  go (PUnary _ op j) = PUnary ss op j
  go (PBinary _ op j1 j2) = PBinary ss op j1 j2
  go (PArrayLiteral _ js) = PArrayLiteral ss js
  go (PAssociativeArrayField _ n v) = PAssociativeArrayField ss n v
  go (PIndexer _ j1 j2) = PIndexer ss j1 j2
  go (PObjectLiteral _ js) = PObjectLiteral ss js
  go (PFunction _ name args j) = PFunction ss name args j
  go (PApp _ j js) = PApp ss j js
  go (PVar _ s) = PVar ss s
  go (PBlock _ js) = PBlock ss js
  go (PVariableIntroduction _ name j) = PVariableIntroduction ss name j
  go (PClassVariableIntroduction _ name j) = PClassVariableIntroduction ss name j
  go (PAssignment _ j1 j2) = PAssignment ss j1 j2
  -- go (While _ j1 j2) = While ss j1 j2
  -- go (For _ name j1 j2 j3) = For ss name j1 j2 j3
  -- go (ForIn _ name j1 j2) = ForIn ss name j1 j2
  -- go (IfElse _ j1 j2 j3) = IfElse ss j1 j2 j3
  go (PReturn _ js) = PReturn ss js
  go (PReturnNoResult _) = PReturnNoResult ss
  -- go (Throw _ js) = Throw ss js
  -- go (InstanceOf _ j1 j2) = InstanceOf ss j1 j2
  go (PComment _ com j) = PComment ss com j


getSourceSpan :: PHP -> Maybe SourceSpan
getSourceSpan = go where
  go :: PHP -> Maybe SourceSpan
  go (PNumericLiteral ss _) = ss
  go (PStringLiteral ss _) = ss
  go (PBooleanLiteral ss _) = ss
  go (PUnary ss _ _) = ss
  go (PBinary ss _ _ _) = ss
  go (PArrayLiteral ss _) = ss
  go (PAssociativeArrayField ss _ _) = ss
  go (PIndexer ss _ _) = ss
  go (PObjectLiteral ss  _) = ss
  go (PFunction ss _ _ _) = ss
  go (PApp ss _ _) = ss
  go (PVar ss _) = ss
  go (PBlock ss _) = ss
  go (PVariableIntroduction ss _ _) = ss
  go (PClassVariableIntroduction ss _ _) = ss
  go (PAssignment ss _ _) = ss
  -- go (While ss _ _) = ss
  -- go (For ss _ _ _ _) = ss
  -- go (ForIn ss _ _ _) = ss
  -- go (IfElse ss _ _ _) = ss
  go (PReturn ss _) = ss
  go (PReturnNoResult ss) = ss
  -- go (Throw ss _) = ss
  -- go (InstanceOf ss _ _) = ss
  go (PComment ss _ _) = ss


everywhere :: (PHP -> PHP) -> PHP -> PHP
everywhere f = go where
  go :: PHP -> PHP
  go (PUnary ss op j) = f (PUnary ss op (go j))
  go (PBinary ss op j1 j2) = f (PBinary ss op (go j1) (go j2))
  go (PArrayLiteral ss js) = f (PArrayLiteral ss (map go js))
  go (PIndexer ss j1 j2) = f (PIndexer ss (go j1) (go j2))
  go (PObjectLiteral ss js) = f (PObjectLiteral ss (map go js)) -- (map (fmap go) js))
  go (PFunction ss name args j) = f (PFunction ss name args (go j))
  go (PApp ss j js) = f (PApp ss (go j) (map go js))
  go (PBlock ss js) = f (PBlock ss (map go js))
  go (PVariableIntroduction ss name j) = f (PVariableIntroduction ss name (fmap go j))
  go (PClassVariableIntroduction ss name j) = f (PClassVariableIntroduction ss name (fmap go j))
  go (PAssignment ss j1 j2) = f (PAssignment ss (go j1) (go j2))
  -- go (While ss j1 j2) = f (While ss (go j1) (go j2))
  -- go (For ss name j1 j2 j3) = f (For ss name (go j1) (go j2) (go j3))
  -- go (ForIn ss name j1 j2) = f (ForIn ss name (go j1) (go j2))
  -- go (IfElse ss j1 j2 j3) = f (IfElse ss (go j1) (go j2) (fmap go j3))
  go (PReturn ss js) = f (PReturn ss (go js))
  -- go (Throw ss js) = f (Throw ss (go js))
  -- go (InstanceOf ss j1 j2) = f (InstanceOf ss (go j1) (go j2))
  go (PComment ss com j) = f (PComment ss com (go j))
  go other = f other

everywhereTopDown :: (PHP -> PHP) -> PHP -> PHP
everywhereTopDown f = runIdentity . everywhereTopDownM (Identity . f)

everywhereTopDownM :: (Monad m) => (PHP -> m PHP) -> PHP -> m PHP
everywhereTopDownM f = f >=> go where
  f' = f >=> go
  go (PUnary ss op j) = PUnary ss op <$> f' j
  go (PBinary ss op j1 j2) = PBinary ss op <$> f' j1 <*> f' j2
  go (PArrayLiteral ss js) = PArrayLiteral ss <$> traverse f' js
  go (PIndexer ss j1 j2) = PIndexer ss <$> f' j1 <*> f' j2
  go (PObjectLiteral ss js) = PObjectLiteral ss <$> traverse f' js -- traverse (sndM f') js
  go (PFunction ss name args j) = PFunction ss name args <$> f' j
  go (PApp ss j js) = PApp ss <$> f' j <*> traverse f' js
  go (PBlock ss js) = PBlock ss <$> traverse f' js
  go (PVariableIntroduction ss name j) = PVariableIntroduction ss name <$> traverse f' j
  go (PClassVariableIntroduction ss name j) = PClassVariableIntroduction ss name <$> traverse f' j
  go (PAssignment ss j1 j2) = PAssignment ss <$> f' j1 <*> f' j2
  -- go (While ss j1 j2) = While ss <$> f' j1 <*> f' j2
  -- go (For ss name j1 j2 j3) = For ss name <$> f' j1 <*> f' j2 <*> f' j3
  -- go (ForIn ss name j1 j2) = ForIn ss name <$> f' j1 <*> f' j2
  -- go (IfElse ss j1 j2 j3) = IfElse ss <$> f' j1 <*> f' j2 <*> traverse f' j3
  go (PReturn ss j) = PReturn ss <$> f' j
  -- go (Throw ss j) = Throw ss <$> f' j
  -- go (InstanceOf ss j1 j2) = InstanceOf ss <$> f' j1 <*> f' j2
  go (PComment ss com j) = PComment ss com <$> f' j
  go other = f other

everything :: (r -> r -> r) -> (PHP -> r) -> PHP -> r
everything (<>.) f = go where
  go j@(PUnary _ _ j1) = f j <>. go j1
  go j@(PBinary _ _ j1 j2) = f j <>. go j1 <>. go j2
  go j@(PArrayLiteral _ js) = foldl (<>.) (f j) (map go js)
  go j@(PIndexer _ j1 j2) = f j <>. go j1 <>. go j2
  go j@(PObjectLiteral _ js) = foldl (<>.) (f j) (map go js) -- (map (go . snd) js)
  go j@(PFunction _ _ _ j1) = f j <>. go j1
  go j@(PApp _ j1 js) = foldl (<>.) (f j <>. go j1) (map go js)
  go j@(PBlock _ js) = foldl (<>.) (f j) (map go js)
  go j@(PVariableIntroduction _ _ (Just j1)) = f j <>. go j1
  go j@(PClassVariableIntroduction _ _ (Just j1)) = f j <>. go j1
  go j@(PAssignment _ j1 j2) = f j <>. go j1 <>. go j2
  -- go j@(While _ j1 j2) = f j <>. go j1 <>. go j2
  -- go j@(For _ _ j1 j2 j3) = f j <>. go j1 <>. go j2 <>. go j3
  -- go j@(ForIn _ _ j1 j2) = f j <>. go j1 <>. go j2
  -- go j@(IfElse _ j1 j2 Nothing) = f j <>. go j1 <>. go j2
  -- go j@(IfElse _ j1 j2 (Just j3)) = f j <>. go j1 <>. go j2 <>. go j3
  go j@(PReturn _ j1) = f j <>. go j1
  -- go j@(Throw _ j1) = f j <>. go j1
  -- go j@(InstanceOf _ j1 j2) = f j <>. go j1 <>. go j2
  go j@(PComment _ _ j1) = f j <>. go j1
  go other = f other


-- old stuff

  -- -- | Top-level function definition
  -- | PFunctionDef (Maybe SourceSpan) [Text] PHP
  -- -- | Variable bind
  -- | PVarBind Text PHP
  -- -- | A variable
  -- | PVar Text
  -- -- | A function reference f/1
  -- | PFunRef PSString Int
  -- -- | A fun definition
  -- | PFunFull (Maybe Text) [(PFunBinder, PHP)]
  -- -- | Function application
  -- | PApp PHP [PHP]
  -- -- | Block
  -- | PBlock [PHP]
  -- -- | An array
  -- | PArrayLiteral [PHP]
  -- -- | An associative array
  -- | PAssociativeArrayLiteral [(PSString, PHP)]

  -- deriving (Show, Eq)

-- -- | Simple 0-arity version of PFun1
-- pattern PFun0 :: Maybe Text -> PHP -> PHP
-- pattern PFun0 name e = PFunFull name [(PFunBinder [], e)]

-- -- | Simple fun definition fun f(X) -> e end (arity 1 with single head with simple variable pattern, name optional)
-- pattern PFun1 :: Maybe Text -> Text -> PHP -> PHP
-- pattern PFun1 name var e = PFunFull name [(PFunBinder [PVar var], e)]

-- extractVars :: [PHP] -> Maybe [Text]
-- extractVars = traverse var
--   where var (PVar x) = Just x
--         var _ = Nothing

-- -- | Simple arity-N versions of PFun1
-- pattern PFunN :: Maybe Text -> [Text] -> PHP -> PHP
-- pattern PFunN name vars e <- PFunFull name [(PFunBinder (extractVars -> Just vars), e)] where
--   PFunN name vars e = PFunFull name [(PFunBinder (map PVar vars), e)]

-- data PFunBinder
--   = PFunBinder [PHP]
--   deriving (Show, Eq)

-- data Guard = Guard PHP
--   deriving (Show, Eq)
