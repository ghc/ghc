module Supercompile (supercompileProgram) where

import Supercompile.Utilities
import qualified Supercompile.Core.Syntax as S
import qualified Supercompile.Core.FreeVars as S
import qualified Supercompile.Evaluator.Syntax as S
import qualified Supercompile.Drive.Process as S

import CoreSyn
import CoreUtils  (exprType)
import DataCon    (dataConWorkId, dataConAllTyVars, dataConRepArgTys)
import VarSet
import Name       (localiseName)
import Var        (Var, isTyVar, varName, setVarName)
import Id         (mkSysLocal, mkSysLocalM, realIdUnfolding, isPrimOpId_maybe, isDataConWorkId_maybe, setIdNotExported)
import MkId       (mkPrimOpId)
import MkCore     (mkBigCoreVarTup, mkTupleSelector, mkWildValBinder)
import FastString (mkFastString, fsLit)
import PrimOp     (primOpSig)
import Type       (mkTyVarTy)
import DynFlags   (DynFlags)

import Control.Monad
import qualified Data.Map as M

topUnique :: Unique
anfUniqSupply' :: UniqSupply
(topUnique, anfUniqSupply') = takeUniqFromSupply anfUniqSupply


-- | Descriptions of terms: used for building readable names for ANF-introduced variables
data Description = Opaque String | ArgumentOf Description

descriptionString :: Description -> String
descriptionString = go (0 :: Int)
  where
    go n (Opaque s)     = s ++ (if n > 0 then show n else "")
    go n (ArgumentOf d) = go (n + 1) d

desc :: S.Term -> Description
desc = desc' . unI

desc' :: S.TermF Identity -> Description
desc' (S.Var x)            = Opaque (S.varString x)
desc' (S.Value _)          = Opaque "value"
desc' (S.App e1 _)         = argOf (desc e1)
desc' (S.TyApp e1 _)       = argOf (desc e1)
desc' (S.PrimOp pop as es) = foldr (\() d -> argOf d) (Opaque (show pop)) (map (const ()) as ++ map (const ()) es)
desc' (S.Case _ _ _ _)     = Opaque "case"
desc' (S.Cast _ _)         = Opaque "cast"
desc' (S.Let _ _ e)        = desc e
desc' (S.LetRec _ e)       = desc e

argOf :: Description -> Description
argOf = ArgumentOf


newtype ParseM a = ParseM { unParseM :: UniqSupply -> (UniqSupply, [(Var, S.Term)], a) }

instance Functor ParseM where
    fmap = liftM

instance Monad ParseM where
    return x = ParseM $ \s -> (s, [], x)
    mx >>= fxmy = ParseM $ \s -> case unParseM mx s of (s, floats1, x) -> case unParseM (fxmy x) s of (s, floats2, y) -> (s, floats1 ++ floats2, y)

instance MonadUnique ParseM where
    getUniqueSupplyM = ParseM $ \us -> case splitUniqSupply us of (us1, us2) -> (us1, [], us2)

runParseM :: ParseM a -> ([(Var, S.Term)], a)
runParseM act = (floats, x)
  where (_s, floats, x) = unParseM act anfUniqSupply'

freshFloatVar :: String -> S.Term -> ParseM (Maybe (Var, S.Term), Var)
freshFloatVar _ (I (S.Var x)) = return (Nothing, x)
freshFloatVar n e             = fmap (\x -> (Just (x, e), x)) $ mkSysLocalM (mkFastString n) (S.termType e)

floatIt :: [(Var, S.Term)] -> ParseM ()
floatIt floats = ParseM $ \s -> (s, floats, ())

nameIt :: Description -> S.Term -> ParseM Var
nameIt d e = freshFloatVar ("a" ++ descriptionString d) e >>= \(mb_float, x) -> floatIt (maybeToList mb_float) >> return x

bindFloats :: ParseM S.Term -> ParseM S.Term
bindFloats = bindFloatsWith . fmap ((,) [])

bindFloatsWith :: ParseM ([(Var, S.Term)], S.Term) -> ParseM S.Term
bindFloatsWith act = ParseM $ \s -> case unParseM act s of (s, floats, (xes, e)) -> (s, [], S.letRecSmart (xes ++ floats) e)

appE :: S.Term -> S.Term -> ParseM S.Term
appE e1 e2 = nameIt (argOf (desc e1)) e2 >>= \x2 -> return (e1 `S.app` x2)



coreExprToTerm :: CoreExpr -> S.Term
coreExprToTerm = uncurry S.letRecSmart . runParseM . term
  where
    -- PrimOp and Data are dealt with later on by generating appropriate unfoldings
    term (Var x)                   = return $ S.var x
    term (Lit l)                   = return $ S.value (S.Literal l)
    term (App e_fun (Type ty_arg)) = fmap (flip S.tyApp ty_arg) (term e_fun)
    term (App e_fun e_arg)         = join $ liftM2 appE (term e_fun) (term e_arg)
    term (Lam x e) | isTyVar x     = fmap (S.value . S.TyLambda x) (bindFloats (term e))
                   | otherwise     = fmap (S.value . S.Lambda x) (bindFloats (term e))
    term (Let (NonRec x e1) e2)    = liftM2 (S.let_ x) (term e1) (bindFloats (term e2))
    term (Let (Rec xes) e)         = bindFloatsWith (liftM2 (,) (mapM (secondM term) xes) (term e))
    term (Case e x ty alts)        = liftM2 (\e alts -> S.case_ e x ty alts) (term e) (mapM alt alts)
    term (Cast e co)               = fmap (flip S.cast co) (term e)
    term (Note _ e)                = term e -- FIXME: record notes
    term (Type ty)                 = pprPanic "termToCoreExpr" (ppr ty)
    term (Coercion co)             = return $ S.value (S.Coercion co)
    
    alt (DEFAULT,    [], e) = fmap ((,) S.DefaultAlt)         $ bindFloats (term e)
    alt (LitAlt l,   [], e) = fmap ((,) (S.LiteralAlt l))     $ bindFloats (term e)
    alt (DataAlt dc, xs, e) = fmap ((,) (S.DataAlt dc as ys)) $ bindFloats (term e)
      where (as, ys) = span isTyVar xs
    alt it                  = pprPanic "termToCoreExpr" (ppr it)

termToCoreExpr :: S.Term -> CoreExpr
termToCoreExpr = term
  where
    term e = case unI e of
        S.Var x             -> Var x
        S.Value v           -> value v
        S.App e x           -> term e `App` Var x
        S.TyApp e ty        -> term e `App` Type ty
        S.PrimOp pop tys es -> Var (mkPrimOpId pop) `mkTyApps` tys `mkApps` map term es
        S.Case e x ty alts  -> Case (term e) x ty (map alt alts)
        S.Let x e1 e2       -> Let (NonRec x (term e1)) (term e2)
        S.LetRec xes e      -> Let (Rec (map (second term) xes)) (term e)
        S.Cast e co         -> Cast (term e) co
    
    value (S.Indirect x)     = Var x
    value (S.Literal l)      = Lit l
    value (S.Coercion co)    = Coercion co
    value (S.TyLambda a e)   = Lam a (term e)
    value (S.Lambda x e)     = Lam x (term e)
    value (S.Data dc tys xs) = (Var (dataConWorkId dc) `mkTyApps` tys) `mkVarApps` xs
    
    alt (S.DataAlt dc as ys, e) = (DataAlt dc, as ++ ys, term e)
    alt (S.LiteralAlt l,     e) = (LitAlt l,   [],       term e)
    alt (S.DefaultAlt,       e) = (DEFAULT,    [],       term e)

coreBindsToCoreTerm :: [CoreBind] -> (CoreExpr, CoreExpr -> [CoreBind])
coreBindsToCoreTerm binds
  = (mkLets internal_binds (mkBigCoreVarTup internal_xs),
     \e -> let wild_id = mkWildValBinder (exprType e) in [NonRec x (mkTupleSelector internal_xs internal_x wild_id e) | (x, internal_x) <- xs `zip` internal_xs])
  where
    -- This is a sweet hack. Most of the top-level binders will be External names. It is a Bad Idea to locally-bind
    -- an External name, because several Externals with the same name but different uniques will generate clashing
    -- C labels at code-generation time (the unique is not included in the label).
    --
    -- To work around this, we deexternalise the variables at the *local binding sites* we are about to create.
    -- Note that we leave the *use sites* totally intact: we rely on the fact that a) variables are compared only by
    -- unique and b) the internality of these names will be carried down on the next simplifier run, so this works.
    -- The ice is thin, though!
    xs = bindersOfBinds binds
    internal_binds = [case bind of NonRec x e -> NonRec (localiseVar x) e
                                   Rec xes    -> Rec (map (first localiseVar) xes)
                     | bind <- binds]
    internal_xs = bindersOfBinds internal_binds
    localiseVar x = setIdNotExported (x `setVarName` localiseName (varName x))
     -- If we don't mark these Ids as not exported then we get lots of residual top-level bindings of the form x = y

termUnfoldings :: S.Term -> [(Var, S.Term)]
termUnfoldings e = go (S.termFreeVars e) emptyVarSet []
  where
    go new_fvs all_fvs all_xes
      | isEmptyVarSet added_fvs = all_xes
      | otherwise               = go (unionVarSets (map (S.termFreeVars . snd) added_xes)) (all_fvs `unionVarSet` added_fvs) (added_xes ++ all_xes)
      where added_fvs = new_fvs `minusVarSet` all_fvs
            added_xes = [ (x, e)
                        | x <- varSetElems added_fvs
                        , Just e <- [varUnfolding x]]

    varUnfolding x
      | Just pop <- isPrimOpId_maybe x     = Just $ primOpUnfolding pop
      | Just dc <- isDataConWorkId_maybe x = Just $ dataUnfolding dc
      | otherwise                          = fmap coreExprToTerm $ maybeUnfoldingTemplate (realIdUnfolding x)
       -- NB: it's OK if the unfolding is a non-value, as the evaluator won't inline LetBound non-values
    
    primOpUnfolding pop = S.tyLambdas as $ S.lambdas xs $ S.primOp pop (map mkTyVarTy as) (map S.var xs)
      where (as, arg_tys, _res_ty, _arity, _strictness) = primOpSig pop
            xs = zipWith (mkSysLocal (fsLit "x")) bv_uniques arg_tys
    
    dataUnfolding dc = S.tyLambdas as $ S.lambdas xs $ S.value (S.Data dc (map mkTyVarTy as) xs)
      where as = dataConAllTyVars dc
            arg_tys = dataConRepArgTys dc
            xs = zipWith (mkSysLocal (fsLit "x")) bv_uniques arg_tys
    
    -- It doesn't matter if we reuse Uniques here because by construction they can't shadow other uses of the anfUniqSupply'
    bv_uniques = uniqsFromSupply anfUniqSupply'

supercompile :: CoreExpr -> CoreExpr
supercompile e = termToCoreExpr (snd (S.supercompile (M.fromList unfs) e'))
  where unfs = termUnfoldings e'
        e' = coreExprToTerm e

supercompileProgram :: DynFlags -> [CoreBind] -> [CoreBind]
supercompileProgram _dflags binds = NonRec x (supercompile e) : rebuild (Var x)
  where x = mkSysLocal (fsLit "sc") topUnique (exprType e)
        (e, rebuild) = coreBindsToCoreTerm binds
