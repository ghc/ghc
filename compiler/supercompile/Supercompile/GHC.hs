module Supercompile.GHC where

-- FIXME: I need to document the basis on which I push down unlifted heap bindings (they are all values, IIRC)
-- TODO:
--  * Why does the supercompiler not match as much as it should? (e.g. Interpreter, UInterpreter)
--  * We should probably claimStep when supercompiling the RHS of an explicit lambda bound by a recursive "let".
--    Reason: it is tantamount to inlining the body one time. Note that we don't care about non-lambdas (we don't
--    pay for inlining them) or non-values (we don't put a copy of a non-value in the heap along with the RHS).
--
--    If there isn't enough left, what do we do?? Obvious answer: lambda abstract over the function name.
--    Better(?) answer: add it as a let-bound Nothing to the heap, so the resulting h-function is trapped by the residual letrec...

-- TODO: pre-transforming (case e1 of y { C z -> e2[z] }) to case (case e1 of y { C z -> z }) of z -> e2[z]
-- might help us replace CPR more because even if we generalise away the e2[z] we potentially keep the unboxing.
-- Probably can't/shouldn't do this if the wildcard binder y is used in the RHS.

import Supercompile.Utilities
import qualified Supercompile.Core.Syntax as S
import qualified Supercompile.Core.FreeVars as S
import qualified Supercompile.Evaluator.Syntax as S

import CoreSyn
import MkCore     (mkImpossibleExpr)
import CoreUtils  (exprType, bindNonRec)
import CoreUnfold
import Coercion   (Coercion, isCoVar, isCoVarType, mkCoVarCo, mkAxInstCo)
import DataCon    (DataCon, dataConWorkId, dataConTyCon, dataConName)
import Var        (isTyVar)
import PrimOp     (primOpSig)
import Id
import MkId       (mkPrimOpId)
import FastString (mkFastString)
import Type       (isUnLiftedType)
import TyCon      (newTyConCo_maybe)

import Control.Monad

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
desc' (S.TyApp e1 _)       = argOf (desc e1)
desc' (S.CoApp e1 _)       = argOf (desc e1)
desc' (S.App e1 _)         = argOf (desc e1)
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

runParseM' :: UniqSupply -> ParseM a -> ([(Var, S.Term)], a)
runParseM' us act = (floats, x)
  where (_s, floats, x) = unParseM act us

runParseM :: UniqSupply -> ParseM S.Term -> S.Term
runParseM us = uncurry (S.bindManyMixedLiftedness S.termFreeVars) . runParseM' us

freshFloatId :: String -> (CoreExpr, S.Term) -> ParseM (Maybe (Var, S.Term), Var)
freshFloatId _ (_, I (S.Var x)) = return (Nothing, x)
freshFloatId n (old_e, e)       = fmap (\x -> let x' = x `setIdUnfolding` mkUnfolding InlineRhs False (isBottomingId x) old_e in (Just (x', e), x')) $ mkSysLocalM (mkFastString n) (S.termType e)
 -- NB: we are careful to give fresh binders an unfolding so that the evaluator can use
 -- GHC's inlining heuristics to decide whether it is profitable to inline the RHS
 -- FIXME: this doesn't work at all because substituting into binders zaps their (unstable) unfoldings

freshFloatCoVar :: String -> S.Term -> ParseM (Maybe (Var, S.Term), Coercion)
freshFloatCoVar _ (I (S.Value (S.Coercion co))) = return (Nothing, co)
freshFloatCoVar n e                             = fmap (\x -> (Just (x, e), mkCoVarCo x)) $ mkSysLocalM (mkFastString n) (S.termType e)

floatIt :: [(Var, S.Term)] -> ParseM ()
floatIt floats = ParseM $ \s -> (s, floats, ())

nameIt :: Description -> (CoreExpr, S.Term) -> ParseM Var
nameIt d e = freshFloatId ("a" ++ descriptionString d) e >>= \(mb_float, x) -> floatIt (maybeToList mb_float) >> return x

nameCo :: Description -> S.Term -> ParseM Coercion
nameCo d e = freshFloatCoVar ("c" ++ descriptionString d) e >>= \(mb_float, co) -> floatIt (maybeToList mb_float) >> return co

bindFloats :: ParseM S.Term -> ParseM S.Term
bindFloats = bindFloatsWith . fmap ((,) [])

bindFloatsWith :: ParseM ([(Var, S.Term)], S.Term) -> ParseM S.Term
bindFloatsWith act = ParseM $ \s -> case unParseM act s of (s, floats, (xes, e)) -> (s, [], S.bindManyMixedLiftedness S.termFreeVars (xes ++ floats) e)

bindUnliftedFloats :: ParseM S.Term -> ParseM S.Term
bindUnliftedFloats act = ParseM $ \s -> case unParseM act s of (s, floats, e) -> if any (isUnLiftedType . idType . fst) floats
                                                                                 then (s, [], S.bindManyMixedLiftedness S.termFreeVars floats e)
                                                                                 else (s, floats, e)

appE :: S.Term -> (CoreExpr, S.Term) -> ParseM S.Term
appE e1 (old_e2, e2)
  | isCoVarType (S.termType e2) = fmap (e1 `S.coApp`) $ nameCo (argOf (desc e1)) e2
  | otherwise                   = fmap (e1 `S.app`)   $ nameIt (argOf (desc e1)) (old_e2, e2)



conAppToTerm :: DataCon -> [CoreExpr] -> ParseM S.Term
conAppToTerm dc es
  | Just co_axiom <- newTyConCo_maybe (dataConTyCon dc)
  , let [co_val_e] = co_val_es -- NB: newtypes may not have existential arguments
  = fmap (`S.cast` mkAxInstCo co_axiom tys') $ coreExprToTerm co_val_e
  | otherwise
  = do -- Put each argument into a form suitable for an explicit value
       -- NB: if any argument is non-trivial then the resulting binding will not be a simple value
       -- (some let-bindings will surround it) and inlining will be impeded.
       (d, cos') <- mapAccumLM (\d co_e -> fmap ((,) (argOf d)) $ coreExprToTerm co_e >>= nameCo (argOf d))
                               (Opaque (S.nameString (dataConName dc))) co_es
       (_, xs') <- mapAccumLM (\d val_e -> fmap ((,) (argOf d)) $ coreExprToTerm val_e >>= \val_e' -> nameIt (argOf d) (val_e, val_e'))
                              d val_es
       return $ S.value (S.Data dc tys' cos' xs')
  where
    -- Divide into type/coercion/value
    (tys', co_val_es) = takeWhileJust fromType_maybe es
    (co_es, val_es) = span (isCoVarType . exprType) co_val_es

    fromType_maybe (Type ty) = Just ty
    fromType_maybe _         = Nothing

coreExprToTerm :: CoreExpr -> ParseM S.Term
coreExprToTerm init_e = {-# SCC "coreExprToTerm" #-} term init_e
  where
    -- Partially-applied PrimOp and Data are dealt with later on by generating appropriate unfoldings
    -- We use exprIsConApp_maybe here to ensure we desugar explicit constructor use into something that looks cheap,
    -- and we do our own thing to spot saturated primop applications
    term e | Just (dc, univ_tys, es) <- exprIsConApp_maybe (const NoUnfolding) e
           = conAppToTerm dc (map Type univ_tys ++ es)
           | (Var x, es) <- collectArgs e
           , Just pop <- isPrimOpId_maybe x
           , (tys, es) <- takeWhileJust (\e -> case e of Type ty -> Just ty; _ -> Nothing) es
           , all isValArg es
           , (_,_,_,arity,_) <- primOpSig pop
           , length es == arity
           = fmap (S.primOp pop tys) (mapM term es)
    term (Var x)                   = return $ S.var x
    term (Lit l)                   = return $ S.value (S.Literal l)
    term (App e_fun (Type ty_arg)) = fmap (flip S.tyApp ty_arg) (term e_fun)
    term (App e_fun e_arg)         = join $ liftM2 appE (term e_fun) (fmap ((,) e_arg) $ maybeUnLiftedTerm (exprType e_arg) e_arg)
    term (Lam x e) | isTyVar x     = fmap (S.value . S.TyLambda x) (bindFloats (term e))
                   | otherwise     = fmap (S.value . S.Lambda x) (bindFloats (term e))
    term (Let (NonRec x e1) e2)    = liftM2 (S.let_ x) (maybeUnLiftedTerm (idType x) e1) (bindFloats (term e2))
    term (Let (Rec xes) e)         = bindFloatsWith (liftM2 (,) (mapM (secondM term) xes) (term e))
    term (Case e x ty alts)        = liftM2 (\e alts -> S.case_ e x ty alts) (term e) (mapM alt alts)
    term (Cast e co)               = fmap (flip S.cast co) (term e)
    term (Tick _ e)                = term e -- FIXME: record ticks
    term (Type ty)                 = pprPanic "termToCoreExpr" (ppr ty)
    term (Coercion co)             = return $ S.value (S.Coercion co)
    
    -- We can float unlifted bindings out of an unlifted argument/let
    -- because they were certain to be evaluated anyway. Otherwise we have
    -- to residualise all the floats if any of them were unlifted.
    maybeUnLiftedTerm ty e
      | isUnLiftedType ty = term e
      | otherwise         = bindUnliftedFloats (term e)

    alt (altcon, xs, e) = fmap ((,) (coreAltConToAltCon altcon xs)) $ bindFloats (term e)

coreAltConToAltCon :: AltCon -> [Var] -> S.AltCon
coreAltConToAltCon DEFAULT      [] = S.DefaultAlt
coreAltConToAltCon (LitAlt l)   [] = S.LiteralAlt l
coreAltConToAltCon (DataAlt dc) xs = S.DataAlt dc as qs zs
      where (as, ys) = span isTyVar xs
            (qs, zs) = span isCoVar ys
coreAltConToAltCon altcon       xs = pprPanic "coreAltConToAltCon" (ppr (altcon, xs))

termToCoreExpr :: S.Term -> CoreExpr
termToCoreExpr = term
  where
    term e = case unI e of
        S.Var x             -> Var x
        S.Value v           -> value v
        S.TyApp e ty        -> term e `App` Type ty
        S.CoApp e co        -> term e `App` Coercion co
        S.App e x           -> term e `App` Var x
        S.PrimOp pop tys es -> Var (mkPrimOpId pop) `mkTyApps` tys `mkApps` map term es
        S.Case e x ty alts  -> Case (term e) x ty (if null alts then [(DEFAULT, [], mkImpossibleExpr ty)] else map alt alts)
        S.Let x e1 e2       -> bindNonRec x (term e1) (term e2)
        S.LetRec xes e      -> Let (Rec (map (second term) xes)) (term e)
        S.Cast e co         -> Cast (term e) co
    
    value (S.Indirect x)         = Var x
    value (S.Literal l)          = Lit l
    value (S.Coercion co)        = Coercion co
    value (S.TyLambda a e)       = Lam a (term e)
    value (S.Lambda x e)         = Lam x (term e)
    value (S.Data dc tys cos xs) = ((Var (dataConWorkId dc) `mkTyApps` tys) `mkCoApps` cos) `mkVarApps` xs
    
    alt (altcon, e) = (altcon', xs, term e)
      where (altcon', xs) = altConToCoreAltCon altcon

altConToCoreAltCon :: S.AltCon -> (AltCon, [Var])
altConToCoreAltCon (S.DataAlt dc as qs ys) = (DataAlt dc, as ++ qs ++ ys)
altConToCoreAltCon (S.LiteralAlt l)        = (LitAlt l,   [])
altConToCoreAltCon (S.DefaultAlt)          = (DEFAULT,    [])
