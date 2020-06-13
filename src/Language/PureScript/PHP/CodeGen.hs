{-# LANGUAGE GADTs #-}

-- |
-- This module generates code in the simplified Erlang intermediate representation from Purescript code
--
module Language.PureScript.PHP.CodeGen
  ( module AST
  , moduleToPHP
  ) where

import           Debugger
import           Prelude.Compat

import           Language.PureScript.PHP.CodeGen.AST       as AST

import           Control.Monad                             (replicateM, unless)
import           Data.Foldable
import           Data.List                                 (nub)
import qualified Data.Text                                 as T
import           Data.Traversable

import           Data.Set                                  (Set)
import qualified Data.Set                                  as Set

import           Control.Applicative                       ((<|>))
import           Control.Arrow                             (first, second)
import           Control.Monad.Error.Class                 (MonadError (..))
import           Control.Monad.Reader                      (MonadReader (..))
import           Control.Monad.Writer                      (MonadWriter (..))
import qualified Data.Map                                  as M
import           Data.Maybe                                (catMaybes,
                                                            fromMaybe, mapMaybe)
import           Data.String                               (fromString)

import           Control.Monad.Supply.Class

import           Language.PureScript.AST                   (SourceSpan,
                                                            nullSourceSpan)
import qualified Language.PureScript.Constants             as C
import           Language.PureScript.CoreFn                hiding
                                                            (moduleExports)
import           Language.PureScript.Environment           as E
import           Language.PureScript.Errors                (ErrorMessageHint (..))
import           Language.PureScript.Names
import           Language.PureScript.Options
import           Language.PureScript.Traversals            (sndM)
import           Language.PureScript.Types

import           Language.PureScript.PHP.CodeGen.Common
import           Language.PureScript.PHP.CodeGen.Optimizer
import           Language.PureScript.PHP.Errors            (MultipleErrors,
                                                            addHint,
                                                            errorMessage,
                                                            rethrow,
                                                            rethrowWithPosition)
import           Language.PureScript.PHP.Errors.Types

import           Debug.Trace                               (traceM)

freshNamePHP :: (MonadSupply m) => m T.Text
freshNamePHP = fmap (("_@" <>) . T.pack . show) fresh

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
    reexports <- traverse reExportForeign foreigns
    -- let (exports, phpDecls) = concat $ res <> reexports
    let phpDecls = concat $ res <> reexports
    traceM $ show phpDecls
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
      topNonRecToPHP _ ident val = do
        php <- valueToPHP val
        pure $ [PVarBind (runIdent ident) php]
      -- manual binding just ot make it work now
      topNonRecToPHP _ ident (Literal _ (NumericLiteral n)) = do
        pure $ [PVarBind (runIdent ident) (PNumericLiteral n)]
      topNonRecToPHP (ss, _, _, _) ident val = do
        let eann@(_, _, _, meta') = extractAnn val
            -- ident' = case meta of
            --   Just IsTypeClassConstructor -> identToTypeclassCtor ident
            --   -- _ -> ???
        generateFunctionOverloads (Just ss) eann ident val id

      generateFunctionOverloads :: Maybe SourceSpan -> Ann -> Ident -> Expr Ann -> (PHP -> PHP) -> m [PHP]
      generateFunctionOverloads ss eann ident val outerWrapper = do
        -- Always generate the plain curried form, f x y = ... ~> f () -> fun (X) -> fun (Y)
        let qident = Qualified (Just mn) ident
        php <- valueToPHP val
        let curried = [PFunctionDef ss [] (outerWrapper php)]
        -- For effective > 0 (either plain urriedn funs, FnX or EffectFnX) generate an uncurried overload
        -- f x y = ... ~> f(X,Y) -. ((...)(X))(Y)
        -- Relying on inlining to clean up some junk here
        let mkRunApp modName prefix n = App eann (Var eann (Qualified (Just modName) (Ident $ prefix <> T.pack (show (n :: Int)))))
            (wrap, unwrap) = case effFnArity qident of
              Just n -> (mkRunApp effectUncurried C.runEffectFn n, \e -> PApp e [])
              _ | Just n <- fnArity qident -> (mkRunApp dataFunctionUncurried C.runFn n, id)
              _ -> (id, id)

        uncurried <- case effFnArity qident <|> fnArity qident <|> M.lookup qident arities of
          Just arity | arity > 0 -> do
            vars <- replicateM arity freshNamePHP
            -- Apply in CoreFn then translate to take advantage of translation of full/partial application
            php' <- valueToPHP $ foldl (\fn a -> App eann fn (Var eann (Qualified Nothing (Ident a)))) (wrap val) vars
            pure [PFunctionDef ss vars (outerWrapper (unwrap php'))]
          _ -> pure []

        let res = curried <> uncurried
        pure $ if ident `Set.member` declaredExportsSet
               then res
               else res

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
      bindToPHP (NonRec _ ident val) =
        pure . PVarBind (identToVar ident) <$> valueToPHP' (Just ident) val

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


      valueToPHP :: Expr Ann -> m PHP
      valueToPHP = valueToPHP' Nothing

      valueToPHP' :: Maybe Ident -> Expr Ann -> m PHP
      valueToPHP' _ (Literal (pos, _, _, _) l) =
        rethrowWithPosition pos $ literalToValuePHP l
      -- valueToPHP' _ (Var _ (Qualified (Just (ModuleName [ProperName prim])) (Ident undef)))
      --   | prim == C.prim, undef == C.undefined =
      -- ...
      valueToPHP' _ (Let _ ds val) = do
        ds' <- concat <$> mapM bindToPHP ds
        ret <- valueToPHP val
        -- TODO: rename variables rather than creating temporary scope just for this
        -- TODO: This scope doesn't really work probably if we actually want to shadow parent scope
        return $ iife (ds' ++ [ret])

      iife exprs = PApp (PFun0 Nothing (PBlock exprs)) []

      literalToValuePHP :: Literal (Expr Ann) -> m PHP
      literalToValuePHP = literalToValuePHP' valueToPHP

      literalToValuePHP' :: (a -> m PHP) -> Literal a -> m PHP
      literalToValuePHP' _ (NumericLiteral n) = return $ PNumericLiteral n
      literalToValuePHP' _ (StringLiteral s) = return $ PStringLiteral s
      literalToValuePHP' _ (CharLiteral c) = return $ PStringLiteral (fromString [c])
      literalToValuePHP' f (ArrayLiteral xs) = PArrayLiteral <$> mapM f xs
      literalToValuePHP' f (ObjectLiteral ps) = do
         pairs <- mapM (sndM f) ps
         pure $ PAssociativeArrayLiteral pairs
