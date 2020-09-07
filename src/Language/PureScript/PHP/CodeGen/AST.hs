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
-- import Language.PureScript.Traversals

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
  | PDivide
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
  | PArrayAccessor
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
  | PArrayIndexer (Maybe SourceSpan) PHP PHP
  -- ^ Array field accessor
  -- FIXME The array indexer shouldn't be neccessary. We already have the indexer, fix that.
  | PIndexer (Maybe SourceSpan) PHP PHP
  -- ^ Class method accessor
  | PStaticIndexer (Maybe SourceSpan) PHP PHP
  -- ^ Static version. TODO change names?
  | PObjectLiteral (Maybe SourceSpan) [PHP] -- [(PSString, PHP)]
  -- ^ Object literal for records.
  | PClass (Maybe SourceSpan) (Maybe Text) PHP
  -- ^ A class for data constructors.
  | PMethod (Maybe SourceSpan) [Text] Text [Text] PHP
  -- ^ A class method (prefixes e.g. visibility, name, args, body)
  -- TODO: use data type instead of text, NonEmptyList or fixed args?
  | PFunction (Maybe SourceSpan) (Maybe Text) [Text] [Text] PHP
  -- ^ A function introduction (optional name, arguments, outer scope, body)
  | PArrowFunction (Maybe SourceSpan) [Text] PHP
  -- ^ An arrow function
  | PApp (Maybe SourceSpan) PHP [PHP]
  -- ^ Function application
  | PVar (Maybe SourceSpan) Text
  -- ^ Variable
  | PVar' (Maybe SourceSpan) Text
  -- ^ A variable without the `$` prefix
  -- TODO can this be refactored a bit?
  | PBlock (Maybe SourceSpan) Bool [PHP]
  -- ^ A block of expressions with a bool indicating whether it requires braces
  | PVariableIntroduction (Maybe SourceSpan) Text (Maybe PHP)
  -- ^ A variable introduction and optional initialization
  | PClassVariableIntroduction (Maybe SourceSpan) Text (Maybe PHP)
  -- ^ A class variable introduction
  | PAssignment (Maybe SourceSpan) PHP PHP
  -- ^ A variable assignment
  | PWhile (Maybe SourceSpan) PHP PHP
  -- ^ While loop ?
  | PFor (Maybe SourceSpan) Text PHP PHP PHP
  -- ^ For loop
  | PForIn (Maybe SourceSpan) Text PHP PHP
  -- ^ ForIn loop
  | PIfElse (Maybe SourceSpan) PHP PHP (Maybe PHP)
  -- ^ If-then-else statement
  | PReturn (Maybe SourceSpan) PHP
  -- ^ Return statement
  | PReturnNoResult (Maybe SourceSpan)
  -- ^ Return statement with no return value
  | PThrow (Maybe SourceSpan) PHP
  -- ^ Throw statement
  | PInstanceOf (Maybe SourceSpan) PHP PHP
  -- ^ instance of check
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
  go (PArrayIndexer _ j1 j2) = PArrayIndexer ss j1 j2
  go (PIndexer _ j1 j2) = PIndexer ss j1 j2
  go (PStaticIndexer _ j1 j2) = PStaticIndexer ss j1 j2
  go (PObjectLiteral _ js) = PObjectLiteral ss js
  go (PClass _ mt p) = PClass ss mt p
  go (PMethod _ v name args p) = PMethod ss v name args p
  go (PFunction _ name args oscope j) = PFunction ss name args oscope j
  go (PArrowFunction _ t p) = PArrowFunction ss t p
  go (PApp _ j js) = PApp ss j js
  go (PVar _ s) = PVar ss s
  go (PVar' _ s) = PVar' ss s
  go (PBlock _ b js) = PBlock ss b js
  go (PVariableIntroduction _ name j) = PVariableIntroduction ss name j
  go (PClassVariableIntroduction _ name j) = PClassVariableIntroduction ss name j
  go (PAssignment _ j1 j2) = PAssignment ss j1 j2
  go (PWhile _ j1 j2) = PWhile ss j1 j2
  go (PFor _ name j1 j2 j3) = PFor ss name j1 j2 j3
  go (PForIn _ name j1 j2) = PForIn ss name j1 j2
  go (PIfElse _ j1 j2 j3) = PIfElse ss j1 j2 j3
  go (PReturn _ js) = PReturn ss js
  go (PReturnNoResult _) = PReturnNoResult ss
  go (PThrow _ js) = PThrow ss js
  go (PInstanceOf _ j1 j2) = PInstanceOf ss j1 j2
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
  go (PArrayIndexer ss _ _) = ss
  go (PIndexer ss _ _) = ss
  go (PStaticIndexer ss _ _) = ss
  go (PObjectLiteral ss  _) = ss
  go (PClass ss _ _) = ss
  go (PMethod ss _ _ _ _) = ss
  go (PFunction ss _ _ _ _) = ss
  go (PArrowFunction ss _ _) = ss
  go (PApp ss _ _) = ss
  go (PVar ss _) = ss
  go (PVar' ss _) = ss
  go (PBlock ss _ _) = ss
  go (PVariableIntroduction ss _ _) = ss
  go (PClassVariableIntroduction ss _ _) = ss
  go (PAssignment ss _ _) = ss
  go (PWhile ss _ _) = ss
  go (PFor ss _ _ _ _) = ss
  go (PForIn ss _ _ _) = ss
  go (PIfElse ss _ _ _) = ss
  go (PReturn ss _) = ss
  go (PReturnNoResult ss) = ss
  go (PThrow ss _) = ss
  go (PInstanceOf ss _ _) = ss
  go (PComment ss _ _) = ss


everywhere :: (PHP -> PHP) -> PHP -> PHP
everywhere f = go where
  go :: PHP -> PHP
  go (PUnary ss op j) = f (PUnary ss op (go j))
  go (PBinary ss op j1 j2) = f (PBinary ss op (go j1) (go j2))
  go (PArrayLiteral ss js) = f (PArrayLiteral ss (map go js))
  go (PArrayIndexer ss j1 j2) = f (PArrayIndexer ss j1 (go j2))
  go (PIndexer ss j1 j2) = f (PIndexer ss (go j1) (go j2))
  go (PStaticIndexer ss j1 j2) = f (PStaticIndexer ss (go j1) (go j2))
  go (PObjectLiteral ss js) = f (PObjectLiteral ss (map go js)) -- (map (fmap go) js))
  go (PClass ss t p) = f (PClass ss t (go p))
  go (PMethod ss vis name args p) = f (PMethod ss vis name args (go p))
  go (PFunction ss name args oscope j) = f (PFunction ss name args oscope (go j))
  go (PArrowFunction ss args j) = f (PArrowFunction ss args (go j))
  go (PApp ss j js) = f (PApp ss (go j) (map go js))
  go (PBlock ss b js) = f (PBlock ss b (map go js))
  go (PVariableIntroduction ss name j) = f (PVariableIntroduction ss name (fmap go j))
  go (PClassVariableIntroduction ss name j) = f (PClassVariableIntroduction ss name (fmap go j))
  go (PAssignment ss j1 j2) = f (PAssignment ss (go j1) (go j2))
  go (PWhile ss j1 j2) = f (PWhile ss (go j1) (go j2))
  go (PFor ss name j1 j2 j3) = f (PFor ss name (go j1) (go j2) (go j3))
  go (PForIn ss name j1 j2) = f (PForIn ss name (go j1) (go j2))
  go (PIfElse ss j1 j2 j3) = f (PIfElse ss (go j1) (go j2) (fmap go j3))
  go (PReturn ss js) = f (PReturn ss (go js))
  go (PThrow ss js) = f (PThrow ss (go js))
  go (PInstanceOf ss j1 j2) = f (PInstanceOf ss (go j1) (go j2))
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
  go (PArrayIndexer ss j1 j2) = PArrayIndexer ss j1 <$> f' j2
  go (PIndexer ss j1 j2) = PIndexer ss <$> f' j1 <*> f' j2
  go (PStaticIndexer ss j1 j2) = PStaticIndexer ss <$> f' j1 <*> f' j2
  go (PObjectLiteral ss js) = PObjectLiteral ss <$> traverse f' js -- traverse (sndM f') js
  go (PClass ss n j) = PClass ss n <$> f' j
  go (PMethod ss vis name args p) = PMethod ss vis name args <$> f' p
  go (PFunction ss name args oscope j) = PFunction ss name args oscope <$> f' j
  go (PArrowFunction ss args j) = PArrowFunction ss args <$> f' j
  go (PApp ss j js) = PApp ss <$> f' j <*> traverse f' js
  go (PBlock ss b js) = PBlock ss b <$> traverse f' js
  go (PVariableIntroduction ss name j) = PVariableIntroduction ss name <$> traverse f' j
  go (PClassVariableIntroduction ss name j) = PClassVariableIntroduction ss name <$> traverse f' j
  go (PAssignment ss j1 j2) = PAssignment ss <$> f' j1 <*> f' j2
  go (PWhile ss j1 j2) = PWhile ss <$> f' j1 <*> f' j2
  go (PFor ss name j1 j2 j3) = PFor ss name <$> f' j1 <*> f' j2 <*> f' j3
  go (PForIn ss name j1 j2) = PForIn ss name <$> f' j1 <*> f' j2
  go (PIfElse ss j1 j2 j3) = PIfElse ss <$> f' j1 <*> f' j2 <*> traverse f' j3
  go (PReturn ss j) = PReturn ss <$> f' j
  go (PThrow ss j) = PThrow ss <$> f' j
  go (PInstanceOf ss j1 j2) = PInstanceOf ss <$> f' j1 <*> f' j2
  go (PComment ss com j) = PComment ss com <$> f' j
  go other = f other

everything :: (r -> r -> r) -> (PHP -> r) -> PHP -> r
everything (<>.) f = go where
  go j@(PUnary _ _ j1) = f j <>. go j1
  go j@(PBinary _ _ j1 j2) = f j <>. go j1 <>. go j2
  go j@(PArrayLiteral _ js) = foldl (<>.) (f j) (map go js)
  go j@(PArrayIndexer _ _ j2) = f j <>. go j2
  go j@(PIndexer _ j1 j2) = f j <>. go j1 <>. go j2
  go j@(PStaticIndexer _ j1 j2) = f j <>. go j1 <>. go j2
  go j@(PObjectLiteral _ js) = foldl (<>.) (f j) (map go js) -- (map (go . snd) js)
  go j@(PClass _ _ js) = f j <>. go js
  go j@(PFunction _ _ _ _ j1) = f j <>. go j1
  go j@(PMethod _ _ _ _ p) = f j <>. go p
  go j@(PArrowFunction _ _ j1) = f j <>. go j1
  go j@(PApp _ j1 js) = foldl (<>.) (f j <>. go j1) (map go js)
  go j@(PBlock _ _ js) = foldl (<>.) (f j) (map go js)
  go j@(PVariableIntroduction _ _ (Just j1)) = f j <>. go j1
  go j@(PClassVariableIntroduction _ _ (Just j1)) = f j <>. go j1
  go j@(PAssignment _ j1 j2) = f j <>. go j1 <>. go j2
  go j@(PWhile _ j1 j2) = f j <>. go j1 <>. go j2
  go j@(PFor _ _ j1 j2 j3) = f j <>. go j1 <>. go j2 <>. go j3
  go j@(PForIn _ _ j1 j2) = f j <>. go j1 <>. go j2
  go j@(PIfElse _ j1 j2 Nothing) = f j <>. go j1 <>. go j2
  go j@(PIfElse _ j1 j2 (Just j3)) = f j <>. go j1 <>. go j2 <>. go j3
  go j@(PReturn _ j1) = f j <>. go j1
  go j@(PThrow _ j1) = f j <>. go j1
  go j@(PInstanceOf _ j1 j2) = f j <>. go j1 <>. go j2
  go j@(PComment _ _ j1) = f j <>. go j1
  go other = f other
