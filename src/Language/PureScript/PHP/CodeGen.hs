{-# LANGUAGE FlexibleContexts  #-}
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

import           Debug.Trace
import           Debugger
import           Prelude.Compat
import           Protolude                                 (ordNub)

import           Language.PureScript.PHP.CodeGen.AST       as AST

import           Control.Monad                             (replicateM, unless)
import qualified Data.Foldable                             as F
import           Data.List                                 (intersect, (\\), partition)
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
import           Language.PureScript.CST.Utils             (internalError)
import           Language.PureScript.Environment           as E
import           Language.PureScript.Errors                (ErrorMessageHint (..))
import           Language.PureScript.Names
import           Language.PureScript.Options
import           Language.PureScript.Traversals            (sndM)
import           Language.PureScript.Types

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
import           Language.PureScript.PSString              (PSString,
                                                            decodeString,
                                                            mkString)

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
        (_, moduleName) = moduleNameToPHP mn
    -- Convert to PHP
    phpDecls <- mapM bindToPHP decls'
    -- TODO should the optimize step run before or after wrapping in the module class?
    -- optimized <- traverse (traverse optimize) phpDecls
    let mnReverseLookup = M.fromList $ map (\(origName, (_, safeName)) -> (moduleNameToPHP safeName, origName)) $ M.toList mnLookup
        -- TODO what's this for?
        usedModuleNames = S.empty -- foldMap (foldMap (findModules mnReverseLookup)) optimized
    phpImports <- traverse (importToPHP mnLookup)
                  . filter (flip S.member usedModuleNames)
                  . (\\ (mn : C.primModules)) $ ordNub $ map snd imps
    -- This check was on optimized before
    F.traverse_ (F.traverse_ checkIntegers) phpDecls -- optimized
    -- TODO: reimplement this
    comments <- not <$> asks optionsNoComments
        --header = if comments && not (null coms) then PComment Nothing coms strict else strict
        --foreign'
    -- TODO grab all top level class assignments and wrap them in the constructor
    let
      -- Partition declaration between vars and functions
      (phpVars, phpFuncs) = partition isPhpAssign $ concat phpDecls
      -- Convert vars to class assignment
      phpConstr = PMethod Nothing ["public"] "__construct" [] (PBlock Nothing True phpVars)

    moduleClass <- optimize $ PClass Nothing (Just moduleName) (PBlock Nothing True $ phpConstr : phpFuncs)
    let moduleBody = phpImports ++ [moduleClass]
        -- foreignExps = exps `intersect` foreigns // module.exports ...
        -- standardExps = exps \\ foreignExps
    return moduleBody

    where

      -- | Checks whether the value is a PAssignment
      isPhpAssign :: PHP -> Bool
      isPhpAssign (PAssignment _ _ _) = True
      isPhpAssign _ = False

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
          goExpr e           = e
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
      -- TODO we don't have any Rec match so far, so it's probably broken.
      bindToPHP (Rec vals) = forM vals (uncurry . uncurry $ nonRecToPHP)



      -- | Generate code in the simplified PHP intermediate representation fora single non-recursive
      -- declaration.
      -- Top level bindings need to be handled specifically. All constant declarations (i.e. vars) have
      -- to live inside the constructor, while the rest has to be defined as a public static function.

      -- TODO: this has to be rewritten since top level bindings will have to be treated differently depending on what they are,
      -- considering that they are now wrapped in a class.
      -- literals (except records) -> PClassVariableIntroduction
      -- records public static functions returning the previous implementation
      -- constructors one public static function when args == 0 (or <= 1 ?), two otherwise
      -- typeclasses?
      -- anything which will be converted to a function
      nonRecToPHP :: (MonadError MultipleErrors m, MonadReader Options m) => Ann -> Ident -> Expr Ann -> m PHP
      -- handle comments?
      nonRecToPHP a i e@(extractAnn -> (_, com, _, _))
        | not (null com) = do
            withoutComment <- asks optionsNoComments
            if withoutComment
              then nonRecToPHP a i (modifyAnn removeComments e)
              else PComment Nothing com <$> nonRecToPHP a i (modifyAnn removeComments e)
      -- literals
      nonRecToPHP (ss, _, _, _) _ val@(Constructor _ _ _ fields)
        | fields /= [] = do
          php <- valueToPHP val []
          withPos ss php
      -- nonRecToPHP (ss, _, _, _) ident val@(Abs (_, _, _, Just IsTypeClassConstructor) _ _) = do
      --   php <- valueToPHP val []
      --   withPos ss (PClass Nothing (Just $ runIdent ident) php)
      nonRecToPHP (ss, _, _, _) ident (Abs _ arg val) = do
        let phpArg = case arg of
              UnusedIdent -> []
              _           -> [identToPHP arg]
            oscope = updateOScope [] phpArg
        ret <- valueToPHP val oscope
        withPos ss (PMethod Nothing ["public", "static"] (runIdent ident) phpArg (PBlock Nothing True [PReturn Nothing ret]))

      nonRecToPHP (ss, _, _, _) ident val = do
        php <- valueToPHP val []
        withPos ss $ PAssignment Nothing ((accessorString $ mkString $ identToPHP ident) (PVar Nothing "this")) php
        -- withPos ss $ PClassVariableIntroduction Nothing (identToPHP ident) (Just php)

      withPos :: MonadReader Options m => SourceSpan -> PHP -> m PHP
      withPos ss php = do
        withSM <- asks (elem JSSourceMap . optionsCodegenTargets)
        return $ if withSM
          then withSourceSpan ss php
          else php

      -- | Generate code in the simplified PHP intermediate representation for a variable baded on a
      -- PureScript identifier.
      var :: Ident -> PHP
      var = PVar Nothing . identToPHP

      -- | Generate a non $ variable.
      var' :: Ident -> PHP
      var' = PVar' Nothing . identToPHP

      -- | Generate code in the simplified PHP intermediate representation for an accessor based on
      -- a PureScript identifier. If the name is not valid in PHP (symbol based, reserved name) an
      -- indexer is returned.
      accessor :: Ident -> PHP -> PHP
      accessor (Ident prop)   = accessorString $ mkString prop
      accessor (GenIdent _ _) = internalError "GenIdent in accessor"
      accessor UnusedIdent    = internalError "UnusedIdent in accessor"

      accessorString :: PSString -> PHP -> PHP
      accessorString prop = PIndexer Nothing (PStringLiteral Nothing prop)

      staticAccessorString :: PSString -> PHP -> PHP
      staticAccessorString prop = PStaticIndexer Nothing (PStringLiteral Nothing prop)

      recBinding :: (Monad m) => Bind Ann -> m PHP
      recBinding (NonRec _ (Ident name) expr) = (return . PVariableIntroduction Nothing name) =<< (Just <$> valueToPHP' expr [])
      recBinding _                            = error "only nonrecursive alphanumeric identifiers are allowed in let bindings for the moment"

      -- | Generate code in the simplified PHP intermediate representation fora value or expression.
      -- It threads the outer scope down
      valueToPHP :: (MonadError MultipleErrors m, MonadReader Options m) => Expr Ann -> [Text] -> m PHP
      valueToPHP e oscope =
        let (ss, _, _, _) = extractAnn e in
          withPos ss =<< valueToPHP' e oscope

      -- | valueToPHP' thread the outerScope for functions
      valueToPHP' :: (MonadError MultipleErrors m, MonadReader Options m) => Expr Ann -> [Text] -> m PHP
      valueToPHP' (Literal (pos, _, _, _) l) _ =
        rethrowWithPosition pos $ literalToValuePHP pos l
      valueToPHP' (Var (_, _, _, Just (IsConstructor _ [])) name) _ =
        return $ qualifiedToPHP id name
      valueToPHP' (Var (_, _, _, Just (IsConstructor _ _)) name) _ =
        return $ staticAccessorString "create" $ qualifiedToPHP' id name
      valueToPHP' (Accessor _ prop val) oscope =
        accessorString prop <$> valueToPHP val oscope
      valueToPHP' (ObjectUpdate _ o ps) _ = error "Object Update not implemented"
      valueToPHP' e@(Abs (_, _, _, Just IsTypeClassConstructor) _ _) _ =
        let args = unAbs e
            vars = map assign args
            constructor =
              let body = [ PAssignment Nothing ((accessorString $ mkString $ identToPHP f) (PVar Nothing "this")) (var f) | f <- args ]
              in PMethod Nothing ["public"] "__construct" (identToPHP `map` args) (PBlock Nothing True body)
        in return $ PBlock Nothing True (vars <> [constructor])
        where
          unAbs :: Expr Ann -> [Ident]
          unAbs (Abs _ arg val) = arg : unAbs val
          unAbs _               = []
          assign :: Ident -> PHP
          assign name = PClassVariableIntroduction Nothing (runIdent name) Nothing
      valueToPHP' (Abs _ arg val@Case{}) oscope = do
        let phpArg = case arg of
                        UnusedIdent -> []
                        _           -> [identToPHP arg]
            oscope' = updateOScope oscope phpArg
        ret <- valueToPHP val oscope'
        return $ PFunction Nothing Nothing phpArg oscope ret -- (PBlock Nothing True [ret])
      valueToPHP' (Abs _ arg val) oscope = do
        let phpArg = case arg of
                        UnusedIdent -> []
                        _           -> [identToPHP arg]
            oscope' = updateOScope oscope phpArg
        ret <- valueToPHP val oscope'
        return $ PFunction Nothing Nothing phpArg oscope (PBlock Nothing True [PReturn Nothing ret])
      valueToPHP' e@App{} oscope = do
        traceShowIdPP "HERE"
        let (f, args) = unApp e []
        args' <- mapM (\v -> valueToPHP v oscope) args
        traceShowIdPP f
        traceShowIdPP args
        traceShowIdPP args'
        case f of
          Var (_, _, _, Just IsNewtype) _ -> error "newtype app"
          Var (_, _, _, Just (IsConstructor _ fields)) name | length args == length fields ->
            return $ PUnary Nothing PNew $ PApp Nothing (qualifiedToPHP' id name) args'
          Var (_, _, _, Just IsTypeClassConstructor) name ->
            return $ PUnary Nothing PNew $ PApp Nothing (qualifiedToPHP' id name) args'
          -- TODO this here is application. needs to become self::func (or class::func if foreign?)
          -- _ -> flip (foldl (\fn a -> PApp Nothing fn [a])) args' <$> (PAssignment Nothing ((accessorString $ "TEST") (PVar' Nothing "self")) <$> (valueToPHP f oscope))
          _ -> flip (foldl (\fn a -> PApp Nothing fn [a])) args' <$> valueToPHP f oscope
        where
          unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
          unApp (App _ val arg) args = unApp val (arg : args)
          unApp other args           = (other, args)
      valueToPHP' (Var (_, _, _, Just IsForeign) _) _ = error "VAr isForeign"
      -- valueToPHP' (Var (_, _, _, Just (IsConstructor _ fields)) ident)
      --   | fields /= [] = return $ varToPHP' ident
      valueToPHP' (Var _ ident) _ = return $ varToPHP ident
      valueToPHP' (Case (ss, _, _, _) values binders) oscope = do
        vals <- mapM (\v -> valueToPHP v oscope) values
        bindersToPHP ss binders vals
      valueToPHP' (Let _ ds val) oscope =
        (\x -> return $ PApp Nothing (PFunction Nothing Nothing [] [] x) []) =<<
          (\binds expr -> PBlock Nothing True $ binds ++ [PReturn Nothing expr]) <$> (sequence $ recBinding <$> ds) <*> (valueToPHP' val oscope)
      valueToPHP' (Constructor (_, _, _, Just IsNewtype) _ ctor _) _ =
        error "Constructor isnewtype"
      valueToPHP' (Constructor _ _ _ []) _ =
        return $ PUnary Nothing PNew (PClass Nothing Nothing (PBlock Nothing True []))
      -- TODO: this has to return the double constructors
      valueToPHP' (Constructor _ _ ctor fields) _ = do
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

      valueToPHP' e _ = error $ "valueToPHP' not implemented: " <> show e

      -- TODO: we probably don't need this
      iife :: Text -> [PHP] -> PHP
      iife v exprs = PApp Nothing (PFunction Nothing Nothing [] [] (PBlock Nothing True $ exprs ++ [PReturn Nothing $ PVar Nothing v])) []

      literalToValuePHP :: SourceSpan -> Literal (Expr Ann) -> m PHP
      literalToValuePHP ss (NumericLiteral n) = return $ PNumericLiteral (Just ss) n
      literalToValuePHP ss (StringLiteral s) = return $ PStringLiteral (Just ss) s
      literalToValuePHP ss (CharLiteral c) = return $ PStringLiteral (Just ss) (fromString [c])
      literalToValuePHP ss (BooleanLiteral b) = return $ PBooleanLiteral (Just ss) b
      literalToValuePHP ss (ArrayLiteral xs) = PArrayLiteral (Just ss) <$> mapM (\v -> valueToPHP v []) xs
      literalToValuePHP ss (ObjectLiteral ps) = do
        ret <- mapM (sndM (\v -> valueToPHP v [])) ps
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
      varToPHP qual                      = qualifiedToPHP id qual

      varToPHP' :: Qualified Ident -> PHP
      varToPHP' (Qualified Nothing ident) = var' ident
      varToPHP' qual                      = qualifiedToPHP' id qual

      -- | Generate code in the simplified PHP intermediate representation for a refrence to a
      -- variable that may have a qualified name.
      qualifiedToPHP :: (a -> Ident) -> Qualified a -> PHP
      qualifiedToPHP f (Qualified (Just C.Prim) a) = PVar Nothing . runIdent $ f a
      -- TODO changed moduleNameToPHP
      -- qualifiedToPHP f (Qualified (Just mn') a)
      --   | mn /= mn' = accessor (f a) (PVar Nothing (moduleNameToPHP mn'))
      qualifiedToPHP f (Qualified _ a) = PVar Nothing $ identToPHP (f a)

      -- | Same as qualifiedToPHP, but generates PVar' (no $)
      -- TODO: This might need to be reworked in the future
      qualifiedToPHP' :: (a -> Ident) -> Qualified a -> PHP
      qualifiedToPHP' f (Qualified (Just C.Prim) a) = PVar' Nothing . runIdent $ f a
      -- TODO changed moduleNametoPHP
      -- qualifiedToPHP' f (Qualified (Just mn') a)
      --   | mn /= mn' = accessor (f a) (PVar' Nothing (moduleNameToPHP mn'))
      qualifiedToPHP' f (Qualified _ a) = PVar' Nothing $ identToPHP (f a)

      foreignIdent :: Ident -> PHP
      foreignIdent ident = accessorString (mkString $ runIdent ident) (PVar Nothing "__foreign")

      -- | Generate code in the simplified PHP intermediate representation for a pattern match binders
      -- and guards.
      bindersToPHP :: SourceSpan -> [CaseAlternative Ann] -> [PHP] -> m PHP
      bindersToPHP ss binders vals = do
        valNames <- replicateM (length vals) freshNamePHP
        let assignments = zipWith (PVariableIntroduction Nothing) valNames (map Just vals)
        php <- forM binders $ \(CaseAlternative bs result) -> do
          ret <- guardsToPHP result
          go valNames ret bs
        return $ PBlock Nothing True (assignments ++ concat php ++ [PThrow Nothing $ failedPatternError valNames])
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
              cond' <- valueToPHP cond []
              val' <- valueToPHP val []
              return
                (PIfElse Nothing cond'
                  (PBlock Nothing True [PReturn Nothing val']) Nothing)
          guardsToPHP (Right v) = return . PReturn Nothing <$> valueToPHP v []

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


-- | Updates the outer scope when creating a function.
-- Right now it just appends them.
updateOScope :: [Text] -> [Text] -> [Text]
updateOScope = (<>)


freshNamePHP :: (MonadSupply m) => m T.Text
freshNamePHP = fmap (("__" <>) . T.pack . show) fresh
