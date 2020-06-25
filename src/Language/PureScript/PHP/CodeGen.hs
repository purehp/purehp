{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}
-- |
-- This module generates code in the simplified Erlang intermediate representation from Purescript code
--
module Language.PureScript.PHP.CodeGen
  ( module AST
  , moduleToPHP
  ) where

import           Debugger
import           Prelude.Compat
import           Protolude                                 (ordNub)

import           Language.PureScript.PHP.CodeGen.AST       as AST

import           Control.Monad                             (replicateM, unless)
import qualified Data.Foldable                             as F
import           Data.List                                 (intersect, (\\))
import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
import           Data.Traversable

import qualified Data.Set                                  as S

import           Control.Applicative                       ((<|>))
import           Control.Arrow                             ((&&&))
import           Control.Monad                             (forM, replicateM,
                                                            void)
import           Control.Monad.Error.Class                 (MonadError (..),
                                                            throwError)
import           Control.Monad.Reader                      (MonadReader (..),
                                                            asks)
import           Control.Monad.Writer                      (MonadWriter (..))
import qualified Data.Map                                  as M
import           Data.Maybe                                (fromMaybe,
                                                            isNothing)
import           Data.String                               (fromString)

import           Control.Monad.Supply.Class

import           Language.PureScript.AST                   (SourceSpan,
                                                            nullSourceSpan)
import           Language.PureScript.AST.SourcePos
import qualified Language.PureScript.Constants             as C
import           Language.PureScript.CoreFn                hiding
                                                            (moduleExports)
import           Language.PureScript.Environment           as E
import           Language.PureScript.Errors                (ErrorMessageHint (..))
import           Language.PureScript.Names
import           Language.PureScript.Options
import           Language.PureScript.Traversals            (sndM)
import           Language.PureScript.Types
import Language.PureScript.CST.Utils (internalError)

import           Language.PureScript.PHP.CodeGen.AST       (PHP,
                                                            everywhereTopDownM,
                                                            withSourceSpan)
import qualified Language.PureScript.PHP.CodeGen.AST       as PHP
import           Language.PureScript.PHP.CodeGen.Common    as Common
import           Language.PureScript.PHP.CodeGen.Optimizer
import           Language.PureScript.PHP.Errors            (MultipleErrors,
                                                            addHint,
                                                            errorMessage,
                                                            rethrow,
                                                            rethrowWithPosition)
import           Language.PureScript.PHP.Errors.Types
import           Language.PureScript.PSString              (PSString, mkString, decodeString)

import           Debug.Trace                               (traceM)


moduleToPHP
  :: forall m
   . (Monad m, MonadReader Options m, MonadSupply m, MonadError MultipleErrors m)
  => Module Ann
  -> Maybe PHP
  -> m [PHP]
moduleToPHP (Module _ coms mn _ imps exps foreigns decls) foreign_ =
  rethrow (addHint (ErrorInModule mn)) $ do
    let usedNames = concatMap getNames decls
        mnLookup = renameImports usedNames imps
        decls' = renameModules mnLookup decls
    phpDecls <- mapM bindToPHP decls'
    -- pureerl does the reexports stuff here
    optimized <- traverse (traverse optimize) phpDecls
    let mnReverseLookup = M.fromList $ map (\(origName, (_, safeName)) -> (moduleNameToPHP safeName, origName)) $ M.toList mnLookup
        usedModuleNames = foldMap (foldMap (findModules mnReverseLookup)) optimized
    phpImports <- traverse (importToPHP mnLookup)
                  . filter (flip S.member usedModuleNames)
                  . (\\ (mn : C.primModules)) $ ordNub $ map snd imps
    F.traverse_ (F.traverse_ checkIntegers) optimized
    -- TODO: reimplement this
    comments <- not <$> asks optionsNoComments
    -- let phpTagOpen = PStringLiteral Nothing "<?php"
    --     phpTagClose = PStringLiteral Nothing "?>"
        --header = if comments && not (null coms) then PComment Nothing coms strict else strict
        --foreign'
    let moduleBody = phpImports ++ concat optimized
        -- foreignExps = exps `intersect` foreigns // module.exports ...
        -- standardExps = exps \\ foreignExps
    return moduleBody

    where

      -- | Extracts all declaration names from a binding group.
      getNames :: Bind Ann -> [Ident]
      getNames (NonRec _ ident _) = [ident]
      getNames (Rec vals)         = map (snd . fst) vals

      -- | Create alternative names for each module to ensure they don't collide
      -- with declaration names.
      renameImports :: [Ident] -> [(Ann, ModuleName)] -> M.Map ModuleName (Ann, ModuleName)
      renameImports = go M.empty
        where
          go :: M.Map ModuleName (Ann, ModuleName) -> [Ident] -> [(Ann, ModuleName)] -> M.Map ModuleName (Ann, ModuleName)
          go acc used ((ann, mn') : mns') =
            let mni = Ident $ runModuleName mn'
            in if mn' /= mn && mni `elem` used
               then let newName = freshModuleName 1 mn' used
                    in go (M.insert mn' (ann, newName) acc) (Ident (runModuleName newName) : used) mns'
               else go (M.insert mn' (ann, mn') acc) used mns'
          go acc _ [] = acc

      freshModuleName :: Integer -> ModuleName -> [Ident] -> ModuleName
      freshModuleName i mn'@(ModuleName name) used =
        let newName = ModuleName $ name <> "_" <> T.pack (show i)
        in if Ident (runModuleName newName) `elem` used
           then freshModuleName (i + 1) mn' used
           else newName

      -- | Generates PHP code for a module import, binding the required module
      -- to the alternative
      importToPHP :: M.Map ModuleName (Ann, ModuleName) -> ModuleName -> m PHP
      importToPHP _ _ = error "Implement importToPHP"

      -- | Replaces the `ModuleName`s in the AST so that the generated code referes to
      -- the collision-avoiding renamed module imports.
      renameModules :: M.Map ModuleName (Ann, ModuleName) -> [Bind Ann] -> [Bind Ann]
      renameModules mnLookup binds =
        let (f, _, _) = everywhereOnValues id goExpr goBinder
        in map f binds
        where
          goExpr :: Expr a -> Expr a
          goExpr (Var ann q) = Var ann (renameQual q)
          goExpr e = e
          goBinder :: Binder a -> Binder a
          goBinder (ConstructorBinder ann q1 q2 bs) = ConstructorBinder ann (renameQual q1) (renameQual q2) bs
          goBinder b = b
          renameQual :: Qualified a -> Qualified a
          renameQual (Qualified (Just mn') a) = 
            let (_,mnSafe) = fromMaybe (internalError "Missing value in mnLookup") $ M.lookup mn' mnLookup
            in Qualified (Just mnSafe) a
          renameQual q = q

      -- | Find the set of ModuleNames -> PHP -> S.Set ModuleName
      findModules :: M.Map Text ModuleName -> PHP -> S.Set ModuleName
      findModules mnReverseLookup = PHP.everything mappend go
        where
          go (PVar _ name) = foldMap S.singleton $ M.lookup name mnReverseLookup
          go _ = mempty

      -- | Generate code in the simplified PHP intermediate representation for a declaration
      bindToPHP (NonRec ann ident val) = return <$> nonRecToPHP ann ident val
      bindToPHP (Rec vals) = forM vals (uncurry . uncurry $ nonRecToPHP)

      -- | Generate code in the simplified PHP intermediate representation fora single non-recurtsive
      -- declaration.
      --
      -- The main purpose of this function is to handle code generation for comments.
      nonRecToPHP :: Ann -> Ident -> Expr Ann -> m PHP
      nonRecToPHP a i e@(extractAnn -> (_, com, _, _))
        | not (null com) = do
            withoutComment <- asks optionsNoComments
            if withoutComment
              then nonRecToPHP a i (modifyAnn removeComments e)
              else PComment Nothing com <$> nonRecToPHP a i (modifyAnn removeComments e)
      nonRecToPHP (ss, _, _, _) ident val@(Constructor _ _ _ fields)
        | fields /= [] = do
          php <- valueToPHP val
          withPos ss php
      nonRecToPHP (ss, _, _, _) ident val = do
        php <- valueToPHP val
        withPos ss $ PVariableIntroduction Nothing (identToPHP ident) (Just php)

      withPos :: SourceSpan -> PHP -> m PHP
      withPos ss php = do
        withSM <- asks (elem JSSourceMap . optionsCodegenTargets)
        return $ if withSM
          then withSourceSpan ss php
          else php

      -- | Generate code in the simplified PHP intermediate representation for a variable baded on a
      -- PureScript identifier.
      var :: Ident -> PHP
      var = PVar Nothing . identToPHP

      var' :: Ident -> PHP
      var' = PVar' Nothing . identToPHP

      -- | Generate code in the simplified PHP intermediate representation for an accessor based on
      -- a PureScript identifier. If the name is not valid in PHP (symbol based, reserved name) an
      -- indexer is returned.
      accessor :: Ident -> PHP -> PHP
      accessor (Ident prop) = accessorString $ mkString prop
      accessor (GenIdent _ _) = internalError "GenIdent in accessor"
      accessor UnusedIdent = internalError "UnusedIdent in accessor"

      accessorString :: PSString -> PHP -> PHP
      accessorString prop = PIndexer Nothing (PStringLiteral Nothing prop)

      staticAccessorString :: PSString -> PHP -> PHP
      staticAccessorString prop = PStaticIndexer Nothing (PStringLiteral Nothing prop)

      -- | Generate code in the simplified PHP intermediate representation fora value or expression.
      valueToPHP :: Expr Ann -> m PHP
      valueToPHP e =
        let (ss, _, _, _) = extractAnn e in
          withPos ss =<< valueToPHP' e

      valueToPHP' :: Expr Ann -> m PHP
      valueToPHP' (Literal (pos, _, _, _) l) =
        rethrowWithPosition pos $ literalToValuePHP pos l
      valueToPHP' (Var (_, _, _, Just (IsConstructor _ [])) name) =
        return $ qualifiedToPHP id name
      valueToPHP' (Var (_, _, _, Just (IsConstructor _ _)) name) =
        return $ staticAccessorString "create" $ qualifiedToPHP' id name
      valueToPHP' (Accessor _ prop val) = error "Accessor not implemented"
      valueToPHP' (ObjectUpdate _ o ps) = error "Object Update not implemented"
      valueToPHP' e@(Abs (_, _, _, Just IsTypeClassConstructor) _ _) =
        error "Abs TypeClassConstructor"  
      valueToPHP' (Abs _ arg val) = do
        ret <- valueToPHP val
        let phpArg = case arg of
                        UnusedIdent -> []
                        _           -> [identToPHP arg]
        return $ PFunction Nothing Nothing phpArg (PBlock Nothing True [PReturn Nothing ret])
      valueToPHP' e@App{} = do
        let (f, args) = unApp e []
        args' <- mapM valueToPHP args
        case f of
          Var (_, _, _, Just IsNewtype) _ -> error "newtype app"
          Var (_, _, _, Just (IsConstructor _ fields)) name | length args == length fields ->
            return $ PUnary Nothing PNew $ PApp Nothing (qualifiedToPHP' id name) args'
          Var (_, _, _, Just IsTypeClassConstructor) name ->
            error "typeclass constructor"
          _ -> flip (foldl (\fn a -> PApp Nothing fn [a])) args' <$> valueToPHP f
        where
          unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
          unApp (App _ val arg) args = unApp val (arg : args)
          unApp other args = (other, args)
      valueToPHP' (Var (_, _, _, Just IsForeign) ident) = error "VAr isForeign"
      -- valueToPHP' (Var (_, _, _, Just (IsConstructor _ fields)) ident)
      --   | fields /= [] = return $ varToPHP' ident
      valueToPHP' (Var _ ident) = return $ varToPHP ident
      valueToPHP' (Case (ss, _, _, _) values binders) = do
        vals <- mapM valueToPHP values
        bindersToPHP ss binders vals
      valueToPHP' (Let _ ds val) = error "Let not implemented"
      valueToPHP' (Constructor (_, _, _, Just IsNewtype) _ ctor _) =
        error "Constructor isnewtype"
      valueToPHP' (Constructor _ _ _ []) =
        return $ PUnary Nothing PNew (PClass Nothing Nothing (PBlock Nothing True []))
      valueToPHP' (Constructor _ _ ctor fields) = do
        let vars = map (\v -> PClassVariableIntroduction Nothing (identToPHP v) Nothing) fields
            constructor =
              let body = [ PAssignment Nothing ((accessorString $ mkString $ identToPHP f) (PVar Nothing "this")) (var f) | f <- fields ]
              in PMethod Nothing ["public"] "__construct" (identToPHP `map` fields) (PBlock Nothing True body)
            create = case fields of
              [] -> error "Unreachable"
              (f:fs) ->
                let body :: [Ident] -> PHP
                    body fs' = case fs' of
                      (h:hs) -> PArrowFunction Nothing [identToPHP h] (PBlock Nothing False [body hs])
                      [] -> PUnary Nothing PNew (PApp Nothing (PVar' Nothing "self") ((PVar Nothing . identToPHP) `map` fields))
                in PMethod Nothing ["public", "static"] "create" [identToPHP f] (PBlock Nothing True [PReturn Nothing (body fs)])
        return $ PClass Nothing (Just $ properToPHP ctor) (PBlock Nothing True $ vars <> [constructor, create])

      valueToPHP' e = error $ "valueToPHP' not implemented: " <> show e

      -- TODO: we probably don't need this
      iife :: Text -> [PHP] -> PHP
      iife v exprs = PApp Nothing (PFunction Nothing Nothing [] (PBlock Nothing True $ exprs ++ [PReturn Nothing $ PVar Nothing v])) []

      literalToValuePHP :: SourceSpan -> Literal (Expr Ann) -> m PHP
      literalToValuePHP ss (NumericLiteral n) = return $ PNumericLiteral (Just ss) n
      literalToValuePHP ss (StringLiteral s) = return $ PStringLiteral (Just ss) s
      literalToValuePHP ss (CharLiteral c) = return $ PStringLiteral (Just ss) (fromString [c])
      literalToValuePHP ss (BooleanLiteral b) = return $ PBooleanLiteral (Just ss) b
      literalToValuePHP ss (ArrayLiteral xs) = PArrayLiteral (Just ss) <$> mapM valueToPHP xs
      literalToValuePHP ss (ObjectLiteral ps) = do
        ret <- mapM (sndM valueToPHP) ps
        -- TODO this fromMaybe shouldn't be here. i think I'm doing something wrong
        let fields :: [PHP]
            fields = map (\(i', p) -> PAssociativeArrayField (Just ss) (fromMaybe "" $ decodeString i') p) ret
        return $ PObjectLiteral (Just ss) fields
        -- let fs = map (\(s, p) -> PClassVariableIntroduction Nothing (fromMaybe "" $ decodeString s) (Just p)) ps
        -- return $ PObjectLiteral (Just ss) fs
        -- PObjectLiteral (Just ss) <$> mapM (sndM (`valueToPHP` i)) ps

      -- | Shallow copy an objecExprt
      -- I don't think we'll need it?
      extendObj :: PHP -> [(PSString, PHP)] -> m PHP
      extendObj _ _ = error "Implement extend object"


      -- | Generate code in the simplified PHP intermediate representation for a reference to a
      -- variable.
      varToPHP :: Qualified Ident -> PHP
      varToPHP (Qualified Nothing ident) = var ident
      varToPHP qual = qualifiedToPHP id qual

      varToPHP' :: Qualified Ident -> PHP
      varToPHP' (Qualified Nothing ident) = var' ident
      varToPHP' qual = qualifiedToPHP' id qual

      -- | Generate code in the simplified PHP intermediate representation for a refrence to a
      -- variable that may have a qualified name.
      qualifiedToPHP :: (a -> Ident) -> Qualified a -> PHP
      qualifiedToPHP f (Qualified (Just C.Prim) a) = PVar Nothing . runIdent $ f a
      qualifiedToPHP f (Qualified (Just mn') a) 
        | mn /= mn' = accessor (f a) (PVar Nothing (moduleNameToPHP mn'))
      qualifiedToPHP f (Qualified _ a) = PVar Nothing $ identToPHP (f a)

      -- | Same as qualifiedToPHP, but generates PVar' (no $)
      -- TODO: This might need to be reworked in the future
      qualifiedToPHP' :: (a -> Ident) -> Qualified a -> PHP
      qualifiedToPHP' f (Qualified (Just C.Prim) a) = PVar' Nothing . runIdent $ f a
      qualifiedToPHP' f (Qualified (Just mn') a)
        | mn /= mn' = accessor (f a) (PVar' Nothing (moduleNameToPHP mn'))
      qualifiedToPHP' f (Qualified _ a) = PVar' Nothing $ identToPHP (f a)

      foreignIdent :: Ident -> PHP
      foreignIdent ident = accessorString (mkString $ runIdent ident) (PVar Nothing "__foreign")

      -- | Generate code in the simplified PHP intermediate representation for a pattern match binders
      -- and guards.
      bindersToPHP :: SourceSpan -> [CaseAlternative Ann] -> [PHP] -> m PHP
      bindersToPHP ss binders vals = do
        valNames <- replicateM (length vals) freshNamePHP
        traceShowIdPP valNames
        let assignments = zipWith (PVariableIntroduction Nothing) valNames (map Just vals)
        php <- forM binders $ \(CaseAlternative bs result) -> do
          ret <- guardsToPHP result
          go valNames ret bs
        return $ PApp Nothing (PFunction Nothing Nothing [] (PBlock Nothing True (assignments ++ concat php ++ [PThrow Nothing $ failedPatternError valNames])))
                   []
        where
          go :: [Text] -> [PHP] -> [Binder Ann] -> m [PHP]
          go _ done [] = return done
          go (v:vs) done' (b:bs) = do
            done'' <- go vs done' bs
            binderToPHP v done'' b
          go _ _ _ = internalError "Invalid argumentst to bindersToPHP"

          failedPatternError :: [Text] -> PHP
          failedPatternError names = PUnary Nothing PNew $ PApp Nothing (PVar Nothing "Error")
            [PBinary Nothing PAdd (PStringLiteral Nothing $ mkString failedPatternMessage) (PArrayLiteral Nothing $ zipWith valueError names vals)]

          failedPatternMessage :: Text
          failedPatternMessage = "Failed pattern match at " <> runModuleName mn <> " " <> displayStartEndPos ss <> ": "

          valueError :: Text -> PHP -> PHP
          valueError _ l@(PNumericLiteral _ _) = l
          valueError _ l@(PStringLiteral _ _) = l
          valueError _ l@(PBooleanLiteral _ _) = l
          valueError s _ = accessorString "name" . accessorString "constructor" $ PVar Nothing s

          guardsToPHP :: Either [(Guard Ann, Expr Ann)] (Expr Ann) -> m [PHP]
          guardsToPHP (Left gs) = traverse genGuard gs where
            genGuard (cond, val) = do
              cond' <- valueToPHP cond
              val' <- valueToPHP val
              return
                (PIfElse Nothing cond'
                  (PBlock Nothing True [PReturn Nothing val']) Nothing)
          guardsToPHP (Right v) = return . PReturn Nothing <$> valueToPHP v

      binderToPHP :: Text -> [PHP] -> Binder Ann -> m [PHP]
      binderToPHP s done binder =
        let (ss, _, _, _) = extractBinderAnn binder in
        traverse (withPos ss) =<< binderToPHP' s done binder

      -- | Generate code in the simplified PHP intermediate representation for a pattern match
      -- binder.
      binderToPHP' :: Text -> [PHP] -> Binder Ann -> m [PHP]
      binderToPHP' _ done NullBinder{} = return done
      binderToPHP' varName done (LiteralBinder _ l) =
        literalToBinderPHP varName done l
      binderToPHP' varName done (VarBinder _ ident) =
        return (PVariableIntroduction Nothing (identToPHP ident) (Just (PVar Nothing varName)) : done)
      binderToPHP' varName done (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) =
        binderToPHP varName done b
      binderToPHP' varName done (ConstructorBinder (_, _, _, Just (IsConstructor ctorType fields)) _ ctor bs) = do
        php <- go (zip fields bs) done
        return $ case ctorType of
          ProductType -> php
          SumType ->
            [PIfElse Nothing (PInstanceOf Nothing (PVar Nothing varName) (qualifiedToPHP (Ident . runProperName) ctor))
                  (PBlock Nothing True php)
                  Nothing]
        where
          go :: [(Ident, Binder Ann)] -> [PHP] -> m [PHP]
          go [] done' = return done'
          go ((field, binder) : remain) done' = do
            argVar <- freshNamePHP
            done'' <- go remain done'
            php <- binderToPHP argVar done'' binder
            return (PVariableIntroduction Nothing argVar (Just $ accessorString (mkString $ identToPHP field) $ PVar Nothing varName) : php)
      binderToPHP' _ _ ConstructorBinder{} =
        internalError "binderToJs: Invalid ConstructorBinder in binderToPHP"
      binderToPHP' varName done (NamedBinder _ ident binder) = do
        php <- binderToPHP varName done binder
        return (PVariableIntroduction Nothing (identToPHP ident) (Just (PVar Nothing varName)) : php)

      literalToBinderPHP :: Text -> [PHP] -> Literal (Binder Ann) -> m [PHP]
      literalToBinderPHP varName done (NumericLiteral num) =
        return [PIfElse Nothing (PBinary Nothing PEqualsTo (PVar Nothing varName) (PNumericLiteral Nothing num)) (PBlock Nothing True done) Nothing]
      literalToBinderPHP varName done (CharLiteral c) =
        return [PIfElse Nothing (PBinary Nothing PEqualsTo (PVar Nothing varName) (PStringLiteral Nothing (fromString [c]))) (PBlock Nothing True done) Nothing]
      literalToBinderPHP varName done (StringLiteral str) =
        return [PIfElse Nothing (PBinary Nothing PEqualsTo (PVar Nothing varName) (PStringLiteral Nothing str)) (PBlock Nothing True done) Nothing]
      literalToBinderPHP varName done (BooleanLiteral True) =
        return [PIfElse Nothing (PVar Nothing varName) (PBlock Nothing True done) Nothing]
      literalToBinderPHP varName done (BooleanLiteral False) =
        return [PIfElse Nothing (PUnary Nothing PNot (PVar Nothing varName)) (PBlock Nothing True done) Nothing]
      literalToBinderPHP varName done (ObjectLiteral bs) = go done bs
        where
          go :: [PHP] -> [(PSString, Binder Ann)] -> m [PHP]
          go done' [] = return done'
          go done' ((prop, binder):bs') = do
            propVar <- freshNamePHP
            done'' <- go done' bs'
            php <- binderToPHP propVar done'' binder
            return (PVariableIntroduction Nothing propVar (Just (accessorString prop (PVar Nothing varName))) : php)
      literalToBinderPHP varName done (ArrayLiteral bs) = do
        php <- go done 0 bs
        return [PIfElse Nothing (PBinary Nothing PEqualsTo (accessorString "length" (PVar Nothing varName)) (PNumericLiteral Nothing (Left (fromIntegral $ length bs)))) (PBlock Nothing True php) Nothing]
        where
          go :: [PHP] -> Integer -> [Binder Ann] -> m [PHP]
          go done' _ [] = return done'
          go done' index (binder:bs') = do
            elVar <- freshNamePHP
            done'' <- go done' (index + 1) bs'
            php <- binderToPHP elVar done'' binder
            return (PVariableIntroduction Nothing elVar (Just (PIndexer Nothing (PNumericLiteral Nothing (Left index)) (PVar Nothing varName))) : php)

      -- Check that all integers fall within the valid int range for PHP.
      -- TODO: check what's needed for PHP
      checkIntegers :: PHP -> m ()
      checkIntegers = void . everywhereTopDownM go
        where
          go :: PHP -> m PHP
          go p = return p
      --     go (PUnary _ PNegate (PNumericLiteral ss (Left i))) =
      --       -- Move the negation inside the literal; since this is a top-down
      --       -- traversal doing this replacement will stop the next case from raising
      --       -- the error when attempting to use -2147483648, as if left unrewritten
      --       -- the value is `Unary Negate (NumericLiteral (Left 2147483648))`, and
      --       -- 2147483648 is larger than the maximum allowed int.
      --       return $ PNumericLiteral ss (Left (-i))
      --     go php@(PNumericLiteral ss (Left i)) = 
      --       let minInt = -2147483648
      --           maxInt = 2147483647
      --       in if i < minInt || i > maxInt
      --         then throwError . maybe errorMessage errorMessage' ss $ InOutOfRange i "PHP" minInt maxInt
      --         else return php
      --     go other = return other

      


freshNamePHP :: (MonadSupply m) => m T.Text
freshNamePHP = fmap (("_@" <>) . T.pack . show) fresh

{-
isTopLevelBinding :: Qualified t -> Bool
isTopLevelBinding (Qualified (Just _) _) = True
isTopLevelBinding (Qualified Nothing _) = False

tyArity :: SourceType -> Int
tyArity (TypeApp _ (TypeApp _ fn _) ty) | fn == E.tyFunction = 1 + tyArity ty
tyArity (ForAll _ _ _ ty _)             = tyArity ty
tyArity (ConstrainedType _ _ ty)        = 1 + tyArity ty
tyArity _                               = 0

uncurriedFnArity :: ModuleName -> T.Text -> SourceType -> Maybe Int
uncurriedFnArity moduleName fnName = go (-1)
  where
    go :: Int -> SourceType -> Maybe Int
    go n (TypeConstructor _ (Qualified (Just mn) (ProperName fnN)))
      | n >= 1, n <= 10, fnN == (fnName <> T.pack (show n)), mn == moduleName = Just n
    go n (TypeApp _ t1 _) = go (n+1) t1
    go n (ForAll _ _ _ ty _) = go n ty
    go _ _ = Nothing

effectUncurried :: ModuleName
effectUncurried = ModuleName "Effect.Uncurried"

dataFunctionUncurried :: ModuleName
dataFunctionUncurried = ModuleName "Data.Function.Uncurried"

-- | Generate code in the simplified PHP intermediate representation for
-- all declarations in a module.
moduleToPHP :: forall m.
     (Monad m, MonadReader Options m, MonadSupply m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => E.Environment
  -> Module Ann
  -- -> [(T.Text, Int)]
  -- -> m ([T.Text], [PHP])
  -> m [PHP]
moduleToPHP env (Module _ _ mn _ _ declaredExports foreigns decls) = -- foreignExports =
  rethrow (addHint (ErrorInModule mn)) $ do
    res <- traverse topBindToPHP decls
    traceM $ show res
    reexports <- traverse reExportForeign foreigns
    -- let (exports, phpDecls) = concat $ res <> reexports
    let phpDecls = concat $ res <> reexports
    optimized <- traverse optimize phpDecls

    let usedFfi = Set.fromList $ map runIdent foreigns
        -- definedFfi = Set.fromList (map fst foreignExports)
        -- unusedFfi = definedFfi Set.\\ usedFfi
        unusedFfi = usedFfi
    unless (Set.null unusedFfi) $
      tell $ errorMessage $ UnusedFFIImplementations mn (Ident <$> Set.toAscList unusedFfi)

    return optimized

    where

      types :: M.Map (Qualified Ident) SourceType
      types = M.map (\(t, _, _) -> t) $ E.names env

      declaredExportsSet :: Set Ident
      declaredExportsSet = Set.fromList declaredExports

      biconcat :: [([a], [b])] -> ([a], [b])
      biconcat x = (concatMap fst x, concatMap snd x)

      explicitArities :: M.Map (Qualified Ident) Int
      explicitArities = tyArity <$> types

      arities :: M.Map (Qualified Ident) Int
      arities =
        -- max arities is max of actual impl and most saturated application
        -- {-# WARNING acutalAritis "not implemented" #-}
        let actualArities = M.empty
        -- let actualArities = M.fromList $ map (\(x, n) -> (Qualified (Just mn) (Ident x), n)) foreignExports
            inferredMaxArities = foldr findApps actualArities decls
        in explicitArities `M.union` inferredMaxArities

      -- | 're-export' foreign imports in the ps module - also used for internal calls for non-exported foreign imports
      -- {-# WARNING reExportForeign "not implemented" #-}
      reExportForeign :: Ident -> m [PHP]
      reExportForeign ident = do
        pure []

      -- {-# WARNING topBindToPHP "Missing Rec case" #-}
      topBindToPHP :: Bind Ann -> m [PHP]
      topBindToPHP (NonRec ann ident val) = topNonRecToPHP ann ident val
      topBindToPHP _                      = pure []
      -- topBindToPHP (Rec vals) = biconcat <$> traverse (uncurry . uncurry $ topNonRecToPHP) vals

      uncurriedFnArity' :: ModuleName -> T.Text -> Qualified Ident -> Maybe Int
      uncurriedFnArity' fnMod fn ident =
        case M.lookup ident types of
          Just t -> uncurriedFnArity fnMod fn t
          _      -> Nothing

      effFnArity = uncurriedFnArity' effectUncurried "EffectFn"
      fnArity = uncurriedFnArity' dataFunctionUncurried "Fn"

      topNonRecToPHP :: Ann -> Ident -> Expr Ann -> m [PHP]
      topNonRecToPHP _ _ _ = error "topNonRecToPHP"
      -- topNonRecToPHP _ ident val = do
      --   php <- valueToPHP val
      --   pure $ [PVarBind (runIdent ident) php]
      -- -- manual binding just ot make it work now
      -- topNonRecToPHP _ ident (Literal _ (NumericLiteral n)) = do
      --   pure $ [PVarBind (runIdent ident) (PNumericLiteral n)]
      -- topNonRecToPHP (ss, _, _, _) ident val = do
      --   let eann@(_, _, _, meta') = extractAnn val
      --       -- ident' = case meta of
      --       --   Just IsTypeClassConstructor -> identToTypeclassCtor ident
      --       --   -- _ -> ???
      --   generateFunctionOverloads (Just ss) eann ident val id

      generateFunctionOverloads :: Maybe SourceSpan -> Ann -> Ident -> Expr Ann -> (PHP -> PHP) -> m [PHP]
      generateFunctionOverloads ss eann ident val outerWrapper = do
        error "generateFunctionOverloads"
        -- -- Always generate the plain curried form, f x y = ... ~> f () -> fun (X) -> fun (Y)
        -- let qident = Qualified (Just mn) ident
        -- php <- valueToPHP val
        -- let curried = [PFunctionDef ss [] (outerWrapper php)]
        -- -- For effective > 0 (either plain urriedn funs, FnX or EffectFnX) generate an uncurried overload
        -- -- f x y = ... ~> f(X,Y) -. ((...)(X))(Y)
        -- -- Relying on inlining to clean up some junk here
        -- let mkRunApp modName prefix n = App eann (Var eann (Qualified (Just modName) (Ident $ prefix <> T.pack (show (n :: Int)))))
        --     (wrap, unwrap) = case effFnArity qident of
        --       Just n -> (mkRunApp effectUncurried C.runEffectFn n, \e -> PApp e [])
        --       _ | Just n <- fnArity qident -> (mkRunApp dataFunctionUncurried C.runFn n, id)
        --       _ -> (id, id)

        -- uncurried <- case effFnArity qident <|> fnArity qident <|> M.lookup qident arities of
        --   Just arity | arity > 0 -> do
        --     vars <- replicateM arity freshNamePHP
        --     -- Apply in CoreFn then translate to take advantage of translation of full/partial application
        --     php' <- valueToPHP $ foldl (\fn a -> App eann fn (Var eann (Qualified Nothing (Ident a)))) (wrap val) vars
        --     pure [PFunctionDef ss vars (outerWrapper (unwrap php'))]
        --   _ -> pure []

        -- let res = curried <> uncurried
        -- pure $ if ident `Set.member` declaredExportsSet
        --        then res
        --        else res

      findApps :: Bind Ann -> M.Map (Qualified Ident) Int -> M.Map (Qualified Ident) Int
      findApps (NonRec _ _ val) apps = findApps' val apps
      findApps (Rec vals) apps       = foldr findApps' apps $ map snd vals

      findApps' :: Expr Ann -> M.Map (Qualified Ident) Int -> M.Map (Qualified Ident) Int
      findApps' expr apps = case expr of
        e@App{} ->
          let (f, args) = unApp e []
              apps' = foldr findApps' apps args
          in
          case f of
            Var (_, _, _, Just IsNewtype) _ -> apps'
            Var (_, _, _, Just (IsConstructor _ fields)) (Qualified _ _)
              | length args == length fields -> apps'
            Var (_, _, _, Just IsTypeClassConstructor) _ ->
              apps'
            Var _ qi@(Qualified (Just mn') _)
              | mn' == mn -> M.alter (updateArity $ length args) qi apps'
            _ -> findApps' f apps'
        Accessor _ _ e -> findApps' e apps
        ObjectUpdate _ e es -> findApps' e $ foldr findApps' apps $ map snd es
        Abs _ _ e -> findApps' e apps
        Case _ e es -> foldr findApps' (foldr findAppCase apps es) e
        Let _ b e' ->
          findApps' e' $ foldr findApps'' apps b
        _ -> apps
        where
          updateArity newArity old@(Just oldArity) | oldArity > newArity = old
          updateArity newArity _                   = Just newArity

          unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
          unApp (App _ val arg) args = unApp val (arg : args)
          unApp other args           = (other, args)

      findApps'' (NonRec _ _ e) apps = findApps' e apps
      findApps'' (Rec binds) apps    = foldr findApps' apps $ map snd binds


      findAppCase (CaseAlternative _ (Right e)) apps = findApps' e apps
      findAppCase (CaseAlternative _ (Left ges)) apps = foldr findApps' apps $ map snd ges

      bindToPHP :: Bind Ann -> m [PHP]
      bindToPHP (NonRec _ ident val) = error "bindToPHP"
        -- pure . PVarBind (identToVar ident) <$> valueToPHP' (Just ident) val

      -- For recursive bindings F(X) = E1, G(X) = E2, ... we have a problem as the variables are not
      -- in scope until each expression is defined. To avoid lifting to the top level first generate
      -- funs which take a tuple of such funs F'({F', G'}) -> (X) -> E1 etc
      -- with occurrences of F, G replaced in E1, E2 with F'({F', G'})
      -- and then bind these F = F'({F', G'})
      -- TODO: Only do this if the are multiple muutally recursive bindings! Else a named fun works.
      -- bindToPHP (Rec vals) = do
      --   let vars = identToVar . snd . fst <$> vals
      --       varTup = PTupleLiteral $ PVar . (<> "@f") <$> vars
      --       replaceFun fvar = everywhereOnPHP go
      --         where
      --           go (PVar f) | f == fvar = PApp (PVar $ f <> "@f") [varTup]
      --           go e = e
      --   etc...


      qualifiedToPHP' ident = fromString $ T.unpack $ runIdent ident

      qualifiedToPHP :: Qualified Ident -> PSString
      qualifiedToPHP (Qualified (Just mn') ident)
        | mn == mn' && ident `Set.notMember` declaredExportsSet = qualifiedToPHP' ident
          -- PStringLiteral $ runIdent ident
      qualifiedToPHP (Qualified (Just mn') ident) = qualifiedToPHP' ident -- PStringLiteral $ runIdent ident
      qualifiedToPHP x = error $ "Invalid qualified identifier " <> T.unpack (showQualified showIdent x)


      qualifiedToVar (Qualified _ ident) = error "qualifiedToVar" -- identToVar ident

      valueToPHP :: Expr Ann -> m PHP
      valueToPHP = valueToPHP' Nothing

      valueToPHP' :: Maybe Ident -> Expr Ann -> m PHP
      valueToPHP' _ _ = error "valueToPHP'"

      -- valueToPHP' :: Maybe Ident -> Expr Ann -> m PHP
      -- valueToPHP' _ (Literal (pos, _, _, _) l) =
      --   rethrowWithPosition pos $ literalToValuePHP l
      -- valueToPHP' _ (Var _ (Qualified (Just (ModuleName prim)) (Ident undef)))
      --   | prim == C.prim, undef == C.undefined = error "Qualified Var"
      -- valueToPHP' _ (Var _ ident) | isTopLevelBinding ident = pure $
      --   case M.lookup ident arities of
      --     Just 1 -> PFunRef (qualifiedToPHP ident) 1
      --     _ | Just arity <- effFnArity ident <|> fnArity ident
      --       , arity > 0 -> PFunRef (qualifiedToPHP ident) arity
      --     _ -> PApp (PStringLiteral $ qualifiedToPHP ident) []
      -- valueToPHP' _ (Var _ ident) = return $ PVar $ qualifiedToVar ident

      -- valueToPHP' ident (Abs _ arg val) = do
      --   ret <- valueToPHP val

      --   let fixIdent (Ident "$__unused") = UnusedIdent
      --       fixIdent x = x
      --       arg' = case fixIdent arg of
      --                UnusedIdent -> "_"
      --                _           -> identToVar arg
      --   return $ PFun1 (fmap identToVar ident) arg' ret

      -- valueToPHP' _ e@App{} = do
      --   let (f, args) = unApp e []
      --   args' <- mapM valueToPHP args
      --   case f of
      --     Var (_, _, _, Just IsNewtype) _ -> return (head args')
      --     Var (_, _, _, Just (IsConstructor _ fields)) (Qualified _ ident)
      --       | length args == length fields ->
      --         return $ constructorLiteral (runIdent ident) args'
      --     Var (_, _, _, Just IsTypeClassConstructor) name ->
      --       error "Type class"
      --       -- return $ curriedApp args' $ PApp ()
      --     Var _ qi@(Qualified _ _)
      --       | arity <- fromMaybe 0 (M.lookup qi arities)
      --       , length args == arity
      --       -> return $ PApp (PStringLiteral $ qualifiedToPHP qi) args'
      --     Var _ _ -> error "Bog"

      --     where
      --       unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
      --       unApp (App _ val arg) args = unApp val (arg : args)
      --       unApp other args = (other, args)


      -- valueToPHP' _ (Let _ ds val) = do
      --   ds' <- concat <$> mapM bindToPHP ds
      --   ret <- valueToPHP val
      --   -- TODO: rename variables rather than creating temporary scope just for this
      --   -- TODO: This scope doesn't really work probably if we actually want to shadow parent scope
      --   return $ iife (ds' ++ [ret])

      -- valueToPHP' _ (Constructor (_, _, _, Just IsNewtype) _ _ _) = error "newtype ctor"

      -- valueToPHP' _ (Constructor _ _ ctor fields) =
      --   let createFn =
      --         let body = constructorLiteral ctor ((PVar . identToVar) `map` fields)
      --         in foldr (\f inner -> PFun1 Nothing (identToVar f) inner) body fields
      --   in pure createFn

      -- valueToPHP' _ x = error $ "Unknown " <> show x

      iife exprs = error "iife" -- PApp (PFun0 Nothing (PBlock exprs)) []

      constructorLiteral name args = error "Constructor"

      curriedApp :: [PHP] -> PHP -> PHP
      curriedApp _ _ = error "curried app"

      literalToValuePHP :: Literal (Expr Ann) -> m PHP
      literalToValuePHP = literalToValuePHP' valueToPHP

      literalToValuePHP' :: (a -> m PHP) -> Literal a -> m PHP
      literalToValuePHP' _ _ = error "LiteralToValuePHP'"
      -- literalToValuePHP' _ (NumericLiteral n) = return $ PNumericLiteral n
      -- literalToValuePHP' _ (StringLiteral s) = return $ PStringLiteral s
      -- literalToValuePHP' _ (CharLiteral c) = return $ PStringLiteral (fromString [c])
      -- literalToValuePHP' f (ArrayLiteral xs) = PArrayLiteral <$> mapM f xs
      -- literalToValuePHP' f (ObjectLiteral ps) = do
      --    pairs <- mapM (sndM f) ps
      --    pure $ PAssociativeArrayLiteral pairs
-}
