module Supercompile.Drive.Match (match) where

#include "HsVersions.h"

import Supercompile.Core.Renaming
import Supercompile.Core.Syntax

import Supercompile.Evaluator.FreeVars
import Supercompile.Evaluator.Syntax

import Supercompile.Utilities hiding (guard)

import BasicTypes (RecFlag(..))
import Coercion
import Var        (varType, TyVar, isTyVar, tyVarKind)
import Id         (Id, idType)
import VarEnv
import TypeRep    (Kind, Type(..), PredType, Pred(..))
import MonadUtils (mapMaybeM)

import Control.Monad.Fix

import Data.Function (on)
import qualified Data.Map as M


--newtype Match a = Match { unMatch :: Either String a }
newtype Match a = Match { unMatch :: Maybe a }

instance Functor Match where
    fmap = liftM

instance Monad Match where
    return = Match . return
    mx >>= fxmy = Match $ unMatch mx >>= (unMatch . fxmy)
    --fail s = Match $ Left s
    fail s = Match $ fail s

instance MonadFix Match where
    mfix xmy = Match (mfix (unMatch . xmy))

guard :: String -> Bool -> Match ()
guard _   True  = return ()
guard msg False = fail msg

runMatch :: Match a -> Maybe a
-- runMatch (Match (Right x))  = Just x
-- runMatch (Match (Left msg)) = trace ("match " ++ msg) Nothing
runMatch = unMatch

matchRnEnv2 :: (a -> FreeVars) -> a -> a -> RnEnv2
matchRnEnv2 f x y = mkRnEnv2 (mkInScopeSet (f x `unionVarSet` f y))

-- instance MonadPlus Match where
--     mzero = fail "mzero"
--     mx1 `mplus` mx2 = Match $ unMatch mx1 `mplus` unMatch mx2


type MatchResult = M.Map Var Var

match :: State -- ^ Tieback semantics
      -> State -- ^ This semantics
      -> Maybe MatchResult -- ^ Renaming from left to right
match s_l@(_deeds_l, Heap h_l _, k_l, qa_l) s_r@(_deeds_r, Heap h_r _, k_r, qa_r) = -- (\res -> traceRender ("match", M.keysSet h_l, residualiseDriveState (Heap h_l prettyIdSupply, k_l, in_e_l), M.keysSet h_r, residualiseDriveState (Heap h_r prettyIdSupply, k_r, in_e_r), res) res) $
  runMatch $ do
    let init_rn2 = matchRnEnv2 stateFreeVars s_l s_r
    (rn2, mfree_eqs2) <- mfix $ \(~(rn2, _)) -> matchEC init_rn2 rn2 k_l k_r
    free_eqs1 <- pprTrace "match0" (rn2 `seq` empty) $ matchAnned (matchQA rn2) qa_l qa_r
    free_eqs2 <- pprTrace "match1" empty $ mfree_eqs2
    pprTrace "match2" (ppr free_eqs1) $ matchPureHeap rn2 (free_eqs1 ++ free_eqs2) h_l h_r >>= safeMkMatchResult

matchAnned :: (a -> a -> b)
           -> Anned a -> Anned a -> b
matchAnned f = f `on` annee

matchQA :: RnEnv2 -> QA -> QA -> Match [(Var, Var)]
matchQA rn2 (Question x_l') (Question x_r') = matchVar rn2 x_l' x_r'
matchQA rn2 (Answer in_v_l) (Answer in_v_r) = matchAnswer rn2 in_v_l in_v_r
matchQA _ _ _ = fail "matchQA"

matchAnswer :: RnEnv2 -> Answer -> Answer -> Match [(Var, Var)]
matchAnswer rn2 = matchCoerced (matchIn renameAnnedValue' matchValue) rn2

matchCoerced :: (RnEnv2 -> a -> a -> Match [(Var, Var)])
             -> RnEnv2 -> Coerced a -> Coerced a -> Match [(Var, Var)]
matchCoerced f rn2 (mb_co_l, x_l) (mb_co_r, x_r) = liftM2 (++) (matchMaybe (\(co_l, _tg_l) (co_r, _tg_r) -> matchCoercion rn2 co_l co_r) mb_co_l mb_co_r) (f rn2 x_l x_r) -- TODO: should match (Just Id) against Nothing

matchKind :: Kind -> Kind -> Match [(Var, Var)]
matchKind k_l k_r = guard "matchKind" (k_l `isSubKind` k_r && k_r `isSubKind` k_l) >> return []

matchType :: RnEnv2 -> Type -> Type -> Match [(Var, Var)]
matchType rn2 (TyVarTy x_l)         (TyVarTy x_r)         = matchVar rn2 x_l x_r
matchType rn2 (AppTy ty1_l ty2_l)   (AppTy ty1_r ty2_r)   = liftM2 (++) (matchType rn2 ty1_l ty1_r) (matchType rn2 ty2_l ty2_r)
matchType rn2 (TyConApp tc_l tys_l) (TyConApp tc_r tys_r) = guard "matchType: TyConApp" (tc_l == tc_r) >> matchList (matchType rn2) tys_l tys_r
matchType rn2 (FunTy ty1_l ty2_l)   (FunTy ty1_r ty2_r)   = liftM2 (++) (matchType rn2 ty1_l ty1_r) (matchType rn2 ty2_l ty2_r)
matchType rn2 (ForAllTy a_l ty_l)   (ForAllTy a_r ty_r)   = matchTyVarBndr rn2 a_l a_r $ \rn2 -> matchType rn2 ty_l ty_r
matchType rn2 (PredTy pred_l)       (PredTy pred_r)       = matchPredType rn2 pred_l pred_r
matchType _ _ _ = fail "matchType"

matchPredType :: RnEnv2 -> PredType -> PredType -> Match [(Var, Var)]
matchPredType rn2 (ClassP cls_l tys_l) (ClassP cls_r tys_r) = guard "matchPredType: ClassP" (cls_l == cls_r) >> matchList (matchType rn2) tys_l tys_r
matchPredType rn2 (IParam nm_l ty_l)   (IParam nm_r ty_r)   = guard "matchPredType: IParam" (nm_l == nm_r) >> matchType rn2 ty_l ty_r
matchPredType rn2 (EqPred ty1_l ty2_l) (EqPred ty1_r ty2_r) = liftM2 (++) (matchType rn2 (ty1_l) (ty1_r)) (matchType rn2 ty2_l ty2_r)
matchPredType _ _ _ = fail "matchType"

matchCoercion :: RnEnv2 -> Coercion -> Coercion -> Match [(Var, Var)]
matchCoercion rn2 (Refl ty_l)              (Refl ty_r)              = matchType rn2 (ty_l) (ty_r)
matchCoercion rn2 (TyConAppCo tc_l cos_l)  (TyConAppCo tc_r cos_r)  = guard "matchCoercion: TyConAppCo" (tc_l == tc_r) >> matchList (matchCoercion rn2) (cos_l) (cos_r)
matchCoercion rn2 (AppCo co1_l co2_l)      (AppCo co1_r co2_r)      = liftM2 (++) (matchCoercion rn2 (co1_l) (co1_r)) (matchCoercion rn2 (co2_l) (co2_r))
matchCoercion rn2 (ForAllCo a_l co_l)      (ForAllCo a_r co_r)      = matchTyVarBndr rn2 a_l a_r $ \rn2 -> matchCoercion rn2 co_l co_r
matchCoercion rn2 (CoVarCo a_l)            (CoVarCo a_r)            = matchVar rn2 a_l a_r
matchCoercion rn2 (AxiomInstCo ax_l cos_l) (AxiomInstCo ax_r cos_r) = guard "matchCoercion: AxiomInstCo" (ax_l == ax_r) >> matchList (matchCoercion rn2) (cos_l) (cos_r)
matchCoercion rn2 (UnsafeCo ty1_l ty2_l)   (UnsafeCo ty1_r ty2_r)   = liftM2 (++) (matchType rn2 (ty1_l) (ty1_r)) (matchType rn2 (ty2_l) (ty2_r))
matchCoercion rn2 (SymCo co_l)             (SymCo co_r)             = matchCoercion rn2 (co_l) (co_r)
matchCoercion rn2 (TransCo co1_l co2_l)    (TransCo co1_r co2_r)    = liftM2 (++) (matchCoercion rn2 (co1_l) (co1_r)) (matchCoercion rn2 (co2_l) (co2_r))
matchCoercion rn2 (NthCo i_l co_l)         (NthCo i_r co_r)         = guard "matchCoercion: NthCo" (i_l == i_r) >> matchCoercion rn2 (co_l) (co_r)
matchCoercion rn2 (InstCo co_l ty_l)       (InstCo co_r ty_r)       = liftM2 (++) (matchCoercion rn2 (co_l) (co_r)) (matchType rn2 (ty_l) (ty_r))
matchCoercion _ _ _ = fail "matchCoercion"

matchTerm :: RnEnv2 -> AnnedTerm -> AnnedTerm -> Match [(Var, Var)]
matchTerm rn2 = matchAnned go
  where
    go :: TermF Anned -> TermF Anned -> Match [(Var, Var)]
    go (Var x_l)                  (Var x_r)                  = matchVar rn2 x_l x_r
    go (Value v_l)                (Value v_r)                = matchValue rn2 v_l v_r
    go (TyApp e_l ty_l)           (TyApp e_r ty_r)           = liftM2 (++) (matchTerm rn2 e_l e_r) (matchType rn2 ty_l ty_r)
    go (App e_l x_l)              (App e_r x_r)              = liftM2 (++) (matchTerm rn2 e_l e_r) (matchVar   rn2 x_l x_r)
    go (PrimOp pop_l tys_l es_l)  (PrimOp pop_r tys_r es_r)  = guard "matchTerm: primop" (pop_l == pop_r) >> liftM2 (++) (matchList (matchType rn2) tys_l tys_r) (matchList (matchTerm rn2) es_l es_r)
    go (Case e_l x_l ty_l alts_l) (Case e_r x_r ty_r alts_r) = liftM3 app3 (matchTerm rn2 e_l e_r) (matchType rn2 ty_l ty_r) (matchIdCoVarBndr rn2 x_l x_r $ \rn2 -> matchAlts rn2 alts_l alts_r)
    go (Let x_l e1_l e2_l)        (Let x_r e1_r e2_r)        = matchTerm rn2' e2_l e2_r >>= \eqs -> matchLet NonRecursive rn2 eqs [(x_l, e1_l)] [(x_r, e1_r)]
      where rn2' = (rn2 `delBndrL` x_l) `delBndrR` x_r
    go (LetRec xes_l e_l)         (LetRec xes_r e_r)         = matchTerm rn2' e_l  e_r  >>= \eqs -> matchLet Recursive rn2' eqs xes_l xes_r
      where rn2' = (rn2 `delBndrsL` map fst xes_l) `delBndrsR` map fst xes_r
    go (Cast e_l co_l)            (Cast e_r co_r)            = liftM2 (++) (matchTerm rn2 (e_l) (e_r)) (matchCoercion rn2 (co_l) (co_r))
    go _ _ = fail "matchTerm"

matchValue :: RnEnv2 -> AnnedValue -> AnnedValue -> Match [(Var, Var)]
matchValue rn2 (Indirect x_l)               (Indirect x_r)               = matchVar rn2 x_l x_r
matchValue rn2 (TyLambda a_l e_l)           (TyLambda a_r e_r)           = matchTyVarBndr rn2 a_l a_r $ \rn2 -> matchTerm rn2 e_l e_r
matchValue rn2 (Lambda x_l e_l)             (Lambda x_r e_r)             = matchIdCoVarBndr rn2 x_l x_r $ \rn2 -> matchTerm rn2 e_l e_r
matchValue rn2 (Data dc_l tys_l cos_l xs_l) (Data dc_r tys_r cos_r xs_r) = guard "matchValue: datacon" (dc_l == dc_r) >> liftM3 app3 (matchList (matchType rn2) tys_l tys_r) (matchList (matchCoercion rn2) cos_l cos_r) (matchList (matchVar rn2) xs_l xs_r)
matchValue _   (Literal l_l)                (Literal l_r)                = guard "matchValue: literal" (l_l == l_r) >> return []
matchValue rn2 (Coercion co_l)              (Coercion co_r)              = matchCoercion rn2 (co_l) (co_r)
matchValue _ _ _ = fail "matchValue"

matchAlts :: RnEnv2 -> [AnnedAlt] -> [AnnedAlt] -> Match [(Var, Var)]
matchAlts rn2 = matchList (matchAlt rn2)

matchAlt :: RnEnv2 -> AnnedAlt -> AnnedAlt -> Match [(Var, Var)]
matchAlt rn2 (alt_con_l, alt_e_l) (alt_con_r, alt_e_r) = matchAltCon rn2 alt_con_l alt_con_r $ \rn2 -> matchTerm rn2 alt_e_l alt_e_r

matchAltCon :: RnEnv2 -> AltCon -> AltCon -> (RnEnv2 -> Match [(Var, Var)]) -> Match [(Var, Var)]
matchAltCon rn2 (DataAlt dc_l as_l qs_l xs_l) (DataAlt dc_r as_r qs_r xs_r) k = guard "matchAltCon: datacon" (dc_l == dc_r) >> (matchTyVarBndrs rn2 as_l as_r $ \rn2 -> matchIdCoVarBndrs rn2 qs_l qs_r $ \rn2 -> matchIdCoVarBndrs rn2 xs_l xs_r k)
matchAltCon rn2 (LiteralAlt l_l)              (LiteralAlt l_r)              k = guard "matchAltCon: literal" (l_l == l_r) >> k rn2
matchAltCon rn2 DefaultAlt                    DefaultAlt                    k = k rn2
matchAltCon _ _ _ _ = fail "matchAltCon"

matchTyVarBndr :: RnEnv2 -> TyVar -> TyVar -> (RnEnv2 -> Match [(Var, Var)]) -> Match [(Var, Var)]
matchTyVarBndr rn2 a_l a_r k = liftM2 (++) (matchKind (tyVarKind a_l) (tyVarKind a_r)) (k (rnBndr2 rn2 a_l a_r))

matchIdCoVarBndr :: RnEnv2 -> Id -> Id -> (RnEnv2 -> Match [(Var, Var)]) -> Match [(Var, Var)]
matchIdCoVarBndr rn2 x_l x_r k = liftM2 (++) (matchType rn2 (varType x_l) (varType x_r)) (k (rnBndr2 rn2 x_l x_r))

matchTyVarBndrs :: RnEnv2 -> [TyVar] -> [TyVar] -> (RnEnv2 -> Match [(Var, Var)]) -> Match [(Var, Var)]
matchTyVarBndrs = matchMany matchTyVarBndr

matchIdCoVarBndrs :: RnEnv2 -> [Id] -> [Id] -> (RnEnv2 -> Match [(Var, Var)]) -> Match [(Var, Var)]
matchIdCoVarBndrs = matchMany matchIdCoVarBndr

matchVarType :: RnEnv2 -> Var -> Var -> Match [(Var, Var)]
matchVarType rn2 x_l x_r
  | isTyVar x_l = ASSERT(isTyVar x_r)       matchKind     (tyVarKind x_l) (tyVarKind x_r)
  | otherwise   = ASSERT(not (isTyVar x_r)) matchType rn2 (varType x_l)   (varType x_r)

matchMany :: (RnEnv2 -> v -> v -> (RnEnv2 -> Match b) -> Match b)
          -> RnEnv2 -> [v] -> [v] -> (RnEnv2 -> Match b) -> Match b
matchMany _    rn2 []         []         k = k rn2
matchMany mtch rn2 (x_l:xs_l) (x_r:xs_r) k = mtch rn2 x_l x_r $ \rn2 -> matchMany mtch rn2 xs_l xs_r k
matchMany _ _ _ _ _ = fail "matchMany"

matchVar :: RnEnv2 -> Out Id -> Out Id -> Match [(Var, Var)]
matchVar rn2 x_l x_r = fmap maybeToList (matchVar_maybe rn2 x_l x_r)

matchVar_maybe :: RnEnv2 -> Out Id -> Out Id -> Match (Maybe (Var, Var))
matchVar_maybe rn2 x_l x_r = case (rnOccL_maybe rn2 x_l, rnOccR_maybe rn2 x_r) of
     -- Both rigidly bound: match iff they rename to the same thing
    (Just x_l', Just x_r') -> guard "matchVar: rigid" (x_l' == x_r') >> return Nothing
     -- Both bound by let: defer decision about matching
    (Nothing, Nothing)     -> return (Just (x_l, x_r))
     -- One bound by let and one bound rigidly: don't match
    _                      -> fail "matchVar: mismatch"

matchList :: (a -> a -> Match [(Var, Var)])
          -> [a] -> [a] -> Match [(Var, Var)]
matchList mtch xs_l xs_r = fmap concat (zipWithEqualM mtch xs_l xs_r)

matchMaybe :: (a -> a -> Match [(Var, Var)])
           -> Maybe a -> Maybe a -> Match [(Var, Var)]
matchMaybe _ Nothing    Nothing    = return []
matchMaybe f (Just x_l) (Just x_r) = f x_l x_r
matchMaybe _ _ _ = fail "matchMaybe"

matchIn :: (InScopeSet -> Renaming -> a -> a)
        -> (RnEnv2 -> a -> a -> Match b)
        -> RnEnv2 -> In a -> In a -> Match b
matchIn rnm mtch rn2 (rn_l, x_l) (rn_r, x_r) = mtch rn2 (rnm iss rn_l x_l) (rnm iss rn_r x_r)
  where iss = rnInScopeSet rn2 -- NB: this line is the only thing that relies on the RnEnv2 InScopeSet being correct

matchEC :: RnEnv2 -> RnEnv2 -> Stack -> Stack -> Match (RnEnv2, Match [(Var, Var)])
matchEC init_rn2 rn2 k_l k_r = foldZipEqualM (\(init_rn2', meqs) kf_l kf_r -> fmap (second (liftM2 (++) meqs)) $ matchECFrame init_rn2' rn2 kf_l kf_r) (init_rn2, return []) k_l k_r

matchECFrame :: RnEnv2 -> RnEnv2 -> Tagged StackFrame -> Tagged StackFrame -> Match (RnEnv2, Match [(Var, Var)])
matchECFrame init_rn2 rn2 kf_l kf_r = go (tagee kf_l) (tagee kf_r)
  where
    go :: StackFrame -> StackFrame -> Match (RnEnv2, Match [(Var, Var)])
    go (Apply x_l')                          (Apply x_r')                          = return (init_rn2, matchVar rn2 x_l' x_r')
    go (TyApply ty_l')                       (TyApply ty_r')                       = return (init_rn2, matchType rn2 ty_l' ty_r')
    go (Scrutinise x_l' ty_l' in_alts_l)     (Scrutinise x_r' ty_r' in_alts_r)     = return (init_rn2, liftM2 (++) (matchType rn2 ty_l' ty_r') (matchIdCoVarBndr rn2 x_l' x_r' $ \rn2 -> matchIn renameAnnedAlts matchAlts rn2 in_alts_l in_alts_r))
    go (PrimApply pop_l tys_l' as_l in_es_l) (PrimApply pop_r tys_r' as_r in_es_r) = return (init_rn2, guard "matchECFrame: primop" (pop_l == pop_r) >> liftM3 (\x y z -> x ++ y ++ z) (matchList (matchType rn2) tys_l' tys_r') (matchList (matchAnned (matchAnswer rn2)) as_l as_r) (matchList (matchIn renameAnnedTerm matchTerm rn2) in_es_l in_es_r))
    go (StrictLet x_l' in_e_l)               (StrictLet x_r' in_e_r)               = return (init_rn2, matchIdCoVarBndr rn2 x_l' x_r' $ \rn2 -> matchIn renameAnnedTerm matchTerm rn2 in_e_l in_e_r)
    go (CastIt co_l')                        (CastIt co_r')                        = return (init_rn2, matchCoercion rn2 co_l' co_r')
    go (Update x_l')                         (Update x_r')                         = return (rnBndr2 rn2 x_l' x_r', matchType rn2 (idType x_l') (idType x_r'))
    go _ _ = fail "matchECFrame"

--- Returns a renaming from the list only if the list maps a "left" variable to a unique "right" variable
-- If the left side var was free, we might have assumed two different corresponding rights for it. This is not necessarily a problem:
--      a |-> True; ()<(a, a)> `match` c |-> True; d |-> True; ()<(c, d)>
--      a |-> True; ()<(a, a)> `match` c |-> True; d |-> c; ()<(c, d)>
-- However, I'm going to reject this for now (simpler).
safeMkMatchResult :: [(Var, Var)] -> Match MatchResult
safeMkMatchResult eqs = guard "safeMkRenaming" (all (\(x_l, x_r) -> M.lookup x_l eqs_map == Just x_r) eqs) >> return eqs_map
  where eqs_map = M.fromList eqs


trimBounds :: BoundVars -> BoundVars -> [(Var, Var)] -> Match [(Var, Var)]
trimBounds internally_bound_l internally_bound_r eqs
  = flip mapMaybeM eqs $ \(x_l, x_r) -> case (x_l `elemVarSet` internally_bound_l, x_r `elemVarSet` internally_bound_r) of
            (True,  True)  -> return Nothing           -- Both local, don't want it in the output
            (False, False) -> return (Just (x_l, x_r)) -- Both free
            _              -> fail "trimBounds"        -- Mismatch

matchLet :: RecFlag -> RnEnv2 -> [(Var, Var)] -> [(Var, AnnedTerm)] -> [(Var, AnnedTerm)] -> Match [(Var, Var)]
matchLet is_rec rhs_rn2 init_free_eqs xes_l xes_r
  = matchLoop [] init_free_eqs emptyVarSet emptyVarSet >>=
    trimBounds (mkVarSet (M.keys h_l)) (mkVarSet (M.keys h_r))
  where
    markUsed x' e used = if termIsCheap e then used else used `extendVarSet` x'
    
    h_l = M.fromList xes_l
    h_r = M.fromList xes_r
    
    matchLoop known [] _ _ = return known
    matchLoop known ((x_l, x_r):free_eqs) used_l used_r
      | (x_l, x_r) `elem` known = matchLoop known free_eqs used_l used_r
      | otherwise = case (M.lookup x_l h_l, M.lookup x_r h_r) of
          (Nothing,  Nothing)  -> go [] [] used_l used_r
          (Just e_l, Just e_r) -> do
              extra_free_eqs <- matchTerm rhs_rn2 e_l e_r
              let used_l' = markUsed x_l e_l used_l
                  used_r' = markUsed x_r e_r used_r
              case is_rec of
                  Recursive    -> go []             extra_free_eqs used_l' used_r'
                  NonRecursive -> go extra_free_eqs []             used_l' used_r'
          _                    -> fail "matchLoop: presentness mismatch"
      where go extra_known extra_free_eqs used_l' used_r' = do
                -- Don't forget to match types of binders as well:
                ty_free_eqs <- matchVarType rhs_rn2 x_l x_r
                matchLoop (ty_free_eqs ++ extra_known ++ (x_l, x_r):known) (extra_free_eqs ++ free_eqs) used_l' used_r'

matchPureHeap :: RnEnv2 -> [(Var, Var)] -> PureHeap -> PureHeap -> Match [(Var, Var)]
matchPureHeap rn2 init_free_eqs h_l h_r
  = matchLoop [] init_free_eqs emptyVarSet emptyVarSet >>=
    trimBounds (fst (pureHeapVars h_l) InternallyBound) (fst (pureHeapVars h_r) InternallyBound)
    -- NB: if there are dead bindings in the left PureHeap then the output Renaming will not contain a renaming for their binders.
    --
    -- NB: The resulting equalities must only relate local vars to local vars (in which case we can discard them, because
    -- matchLoop would have ensured that they were fulfilled) or free vars to free vars (in which case we propagate them upward).
    --
    -- NB: We already know there are no eqs that relate update-frame bound (rigid) variables.
  where
    -- NB: must respect work-sharing for non-values
    --  x |-> e1, y |-> e1; (x, y) `match` x |-> e1; (x, x) == Nothing
    --  x |-> e1; (x, x) `match` x |-> e1; y |-> e1; (x, y) == Nothing (though this is more questionable, it seems a consistent choice)
    -- NB: treat equal values as equal regardless of duplication
    --  x |-> v, y |-> v; (x, y) `match` x |-> v; (x, x) /= Nothing
    -- TODO: look through variables on both sides
    --  x |-> e1; (x, x) `match` x |-> e1; y |-> x `match` (x, y) /= Nothing
    --  x |-> e1, y |-> x; (x, y) `match` x |-> e1 `match` (x, x) /= Nothing
    --
    -- It used to be important to allow instantiatation of a dynamic variable with a static *variable*.
    -- This was so because if we didn't tie back to a situation where all that had changed was that one more
    -- variable was static, we would immediately whistle because the tagbags would be the same.
    --
    -- In the new world, we record staticness as phantom heap bindings, so this just doesn't figure in at all.
    -- We can account for staticness using the standard generalisation mechanism, and there is no need for the
    -- matcher to have hacks like that (though we still have to be careful about how we match phantoms).
    
    markUsed x' (_, e) used = if termIsCheap e then used else used `extendVarSet` x'
    
    matchLoop known [] _ _ = return known
    matchLoop known ((x_l, x_r):free_eqs) used_l used_r
       -- Perhaps we have already assumed this equality is true?
      | (x_l, x_r) `elem` known = matchLoop known free_eqs used_l used_r
      | otherwise = case (M.lookup x_l h_l, M.lookup x_r h_r) of
           -- If matching an internal let, it is possible that variables occur free. Insist that free-ness matches:
          (Nothing, Nothing) -> go [] used_l used_r
          (Just _, Nothing) -> fail "matchLoop: matching binding on left not present in the right"
          (Nothing, Just _) -> fail "matchLoop: matching binding on right not present in the left"
          (Just hb_l, Just hb_r) -> case ((howBound &&& heapBindingTerm) hb_l, (howBound &&& heapBindingTerm) hb_r) of
               -- If the template provably doesn't use this heap binding, we can match it against anything at all
              ((InternallyBound, Nothing), _) -> go [] used_l used_r
               -- If the template internalises a binding of this form, check that the matchable semantics is the same.
               -- If the matchable doesn't have a corresponding binding tieback is impossible because we have less info this time.
              ((InternallyBound, Just in_e_l), (_how_r, mb_in_e_r)) -> case mb_in_e_r of
                  Just in_e_r | not (x_l `elemVarSet` used_l), not (x_r `elemVarSet` used_r) -> matchIn renameAnnedTerm matchTerm rn2 in_e_l in_e_r >>= \extra_free_eqs -> go extra_free_eqs (markUsed x_l in_e_l used_l) (markUsed x_r in_e_r used_r)
                  _ -> fail "matchLoop: can only match a termful InternallyBound on left against an actual term"
               -- If the template has no information but exposes a lambda, we can rename to tie back.
               -- If there is a corresponding binding in the matchable we can't tieback because we have more info this time.
               --
               -- NB: this may cause us to instantiate a lambda-bound var with one that is presently let-bound. The alternative
               -- (almost certainly an sc-stop) is worse, though... Doing this really matters; see for example the Bernouilli benchmark.
               --
               -- TODO: give let-bound nothings tags and generalise to get the same effect?
              ((LambdaBound, Nothing), (_how_r, mb_in_e_r)) -> case mb_in_e_r of
                  Nothing -> (if _how_r == LetBound then pprTrace "Downgrading" (ppr x_l <+> ppr x_r) else id) $
                             go [] used_l used_r
                  Just _ -> fail "matchLoop: cannot match termless LambdaBound on left against an actual term"
               -- If the template has an unfolding, we must do lookthrough
              ((LambdaBound, Just in_e_l), (_how_r, mb_in_e_r)) -> case mb_in_e_r of
                  Just in_e_r | not (x_l `elemVarSet` used_l), not (x_r `elemVarSet` used_r) -> matchIn renameAnnedTerm matchTerm rn2 in_e_l in_e_r >>= \extra_free_eqs -> go extra_free_eqs (markUsed x_l in_e_l used_l) (markUsed x_r in_e_r used_r)
                  _ -> fail "matchLoop: can only match a termful LambdaBound on left against an actual term"
               -- We assume the supercompiler gives us no-shadowing for let-bound names, so if two names are the same they must refer to the same thing
               -- NB: because I include this case, we may not include a renaming for some lambda-bound variables in the final knowns (if they are bound
               -- above the let-bound thing)
               --
               -- Interestingly, doing this matching here also improves matching in the case where a previous state had a more-or-less
               -- evaluated version of this heap binding in place. We "know" that we can match them since they originated from the same
               -- heap binding, even though evaluation may have changed their shape.
               --
               -- Of course, we still need to match the FVs on both sides. For example, the LHS could be {x |-> Just y} with the RHS
               -- {x |-> Just y, y |-> True} -- better not tie back in this situation, so we validate that the y bindings still match.
               -- This also ensures that the outgoing knowns can be used to build a renaming that includes the RHS of these bindings.
               --
               -- OK, I don't think this is safe in the case where either side is not LetBound. The reason is that we might have:
               --     D[(let x = e1 in x, let x = e2 in x)]
               -- ==> (D[let x = e1 in x], D[let x = e2 in x])
               --
               -- Which floats to:
               --     let h0 = D[let x = e1 in x]
               --     in in (h0, D[let x = e2 in x])
               --
               -- We better not tieback the second tuple component to h0 on the basis that the two x binders match!
               -- They are only guaranteed to match if the are **Let bound**, because in that case those binders must have been
               -- created by a common ancestor and hence we can just match the uniques to determine whether the binders are the "same".
               -- It is NOT safe to do this is both/either sides are LambdaBound, because we have no guarantee of a common ancestor in that case.
              ((LetBound, mb_in_e_l), (_how_r, mb_in_e_r))
                | x_l == x_r -> case (mb_in_e_l, mb_in_e_r) of
                  (Nothing,     Nothing)     -> go [] used_l used_r
                  (Just in_e_l, Just in_e_r) -> ASSERT2(inFreeVars annedTermFreeVars in_e_r `subVarSet` inFreeVars annedTermFreeVars in_e_l, text "match" <+> ppr (x_l, in_e_l, x_r, _how_r, in_e_r))
                                                go [(x, x) | x <- varSetElems (inFreeVars annedTermFreeVars in_e_l)] (markUsed x_l in_e_l used_l) (markUsed x_r in_e_r used_r)
                  _                          -> fail "matchLoop: insane LetBounds"
               -- If the template doesn't lambda abstract, we can't rename. Only tieback if we have an exact *name* match.
               --
               -- You might think that we could do better than this if both the LHS and RHS had unfoldings, by matching them.
               -- However, this is very dangerous because we don't want to match the template {x |-> let {Just y}, y |-> lam {}}
               -- against the matchable {x' |-> let {Just y'}, y' |-> lam {}}, since the template may still be able to reach y via the binding
               -- for x (we renamed the lambda-abstracted y to y' so there is nothing to fear from there).
               --
               -- NB: we can treat this *almost* exactly like the LambdaBound+unfolding case now since we have the invariant that LetBound things never
               -- refer to LambdaBound things. *However* we anticipate that doing so would almost always fail to tieback, so we elect to just stick with
               -- the "cheap-but-inaccurate" name-matching heuristic.
                | otherwise -> fail "matchLoop: LetBound"
      where go extra_free_eqs used_l' used_r' = do
                -- Don't forget to match types of binders as well:
                ty_free_eqs <- matchVarType rn2 x_l x_r
                matchLoop ((x_l, x_r) : ty_free_eqs ++ known) (extra_free_eqs ++ free_eqs) used_l' used_r'

app3 :: [a] -> [a] -> [a] -> [a]
app3 x y z = x ++ y ++ z
