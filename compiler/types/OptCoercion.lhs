%
% (c) The University of Glasgow 2006
%

\begin{code}
{-# OPTIONS_GHC -w #-}
module OptCoercion (
	optCoercion
   ) where 

#include "HsVersions.h"

import Unify	( tcMatchTy )
import Coercion
import Type
import TypeRep
import TyCon
import Var
import VarSet
import VarEnv
import PrelNames
import Util
import Outputable
\end{code}

%************************************************************************
%*                                                                      *
                 Optimising coercions									
%*                                                                      *
%************************************************************************

Note [Subtle shadowing in coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Supose we optimising a coercion
    optCoercion (forall (co_X5:t1~t2). ...co_B1...)
The co_X5 is a wild-card; the bound variable of a coercion for-all
should never appear in the body of the forall. Indeed we often
write it like this
    optCoercion ( (t1~t2) => ...co_B1... )

Just because it's a wild-card doesn't mean we are free to choose
whatever variable we like.  For example it'd be wrong for optCoercion
to return
   forall (co_B1:t1~t2). ...co_B1...
because now the co_B1 (which is really free) has been captured, and
subsequent substitutions will go wrong.  That's why we can't use
mkCoPredTy in the ForAll case, where this note appears.  

\begin{code}
optCoercion :: TvSubst -> Coercion -> NormalCo
-- ^ optCoercion applies a substitution to a coercion, 
--   *and* optimises it to reduce its size
optCoercion env co = opt_co env False co

type NormalCo = Coercion
  -- Invariants: 
  --  * The substitution has been fully applied
  --  * For trans coercions (co1 `trans` co2)
  --       co1 is not a trans, and neither co1 nor co2 is identity
  --  * If the coercion is the identity, it has no CoVars of CoTyCons in it (just types)

type NormalNonIdCo = NormalCo  -- Extra invariant: not the identity

opt_co, opt_co' :: TvSubst
       		-> Bool	       -- True <=> return (sym co)
       		-> Coercion
       		-> NormalCo	
opt_co = opt_co'

{-    Debuggery 
opt_co env sym co 
-- = pprTrace "opt_co {" (ppr sym <+> ppr co) $
--     	        co1 `seq` 
--               pprTrace "opt_co done }" (ppr co1) 
--               WARN( not same_co_kind, ppr co  <+> dcolon <+> pprEqPred (s1,t1) 
--                                   $$ ppr co1 <+> dcolon <+> pprEqPred (s2,t2) )
 =   WARN( not (coreEqType co1 simple_result), 
           (text "env=" <+> ppr env) $$
           (text "input=" <+> ppr co) $$
           (text "simple=" <+> ppr simple_result) $$
           (text "opt=" <+> ppr co1) )
     co1
 where
   co1 = opt_co' env sym co
   same_co_kind = s1 `coreEqType` s2 && t1 `coreEqType` t2
   (s,t) = coercionKind (substTy env co)
   (s1,t1) | sym = (t,s)
           | otherwise = (s,t)
   (s2,t2) = coercionKind co1

   simple_result | sym = mkSymCoercion (substTy env co)
                 | otherwise = substTy env co
-}

opt_co' env sym (AppTy ty1 ty2) 	  = mkAppTy (opt_co env sym ty1) (opt_co env sym ty2)
opt_co' env sym (FunTy ty1 ty2) 	  = FunTy (opt_co env sym ty1) (opt_co env sym ty2)
opt_co' env sym (PredTy (ClassP cls tys)) = PredTy (ClassP cls (map (opt_co env sym) tys))
opt_co' env sym (PredTy (IParam n ty))    = PredTy (IParam n (opt_co env sym ty))
opt_co' _   _   co@(PredTy (EqPred {}))   = pprPanic "optCoercion" (ppr co)

opt_co' env sym co@(TyVarTy tv)
  | Just ty <- lookupTyVar env tv = opt_co' (zapTvSubstEnv env) sym ty
  | not (isCoVar tv)     = co   -- Identity; does not mention a CoVar
  | ty1 `coreEqType` ty2 = ty1	-- Identity; ..ditto..
  | not sym              = co
  | otherwise            = mkSymCoercion co
  where
    (ty1,ty2) = coVarKind tv

opt_co' env sym (ForAllTy tv cor) 
  | isTyVar tv  = case substTyVarBndr env tv of
                   (env', tv') -> ForAllTy tv' (opt_co' env' sym cor)

opt_co' env sym co@(ForAllTy co_var cor) 
  | isCoVar co_var 
  = WARN( co_var `elemVarSet` tyVarsOfType cor, ppr co )
    ForAllTy co_var' cor'
  where
    (co1,co2) = coVarKind co_var
    co1' = opt_co' env sym co1
    co2' = opt_co' env sym co2
    cor' = opt_co' env sym cor
    co_var' = uniqAway (getTvInScope env) (mkWildCoVar (mkCoKind co1' co2'))
    -- See Note [Subtle shadowing in coercions]

opt_co' env sym (TyConApp tc cos)
  | Just (arity, desc) <- isCoercionTyCon_maybe tc
  = mkAppTys (opt_co_tc_app env sym tc desc (take arity cos))
             (map (opt_co env sym) (drop arity cos))
  | otherwise
  = TyConApp tc (map (opt_co env sym) cos)

--------
opt_co_tc_app :: TvSubst -> Bool -> TyCon -> CoTyConDesc -> [Coercion] -> NormalCo
-- Used for CoercionTyCons only
-- Arguments are *not* already simplified/substituted
opt_co_tc_app env sym tc desc cos
  = case desc of
      CoAxiom {} -- Do *not* push sym inside top-level axioms
    		 -- e.g. if g is a top-level axiom
    		 --   g a : F a ~ a
		 -- Then (sym (g ty)) /= g (sym ty) !!
        | sym       -> mkSymCoercion the_co  
        | otherwise -> the_co
        where
           the_co = TyConApp tc (map (opt_co env False) cos)
           -- Note that the_co does *not* have sym pushed into it
    
      CoTrans 
        | sym       -> opt_trans opt_co2 opt_co1   -- sym (g `o` h) = sym h `o` sym g
        | otherwise -> opt_trans opt_co1 opt_co2

      CoUnsafe
        | sym       -> mkUnsafeCoercion ty2' ty1'
        | otherwise -> mkUnsafeCoercion ty1' ty2'

      CoSym   -> opt_co env (not sym) co1
      CoLeft  -> opt_lr fst
      CoRight -> opt_lr snd
      CoCsel1 -> opt_csel fstOf3
      CoCsel2 -> opt_csel sndOf3
      CoCselR -> opt_csel thirdOf3

      CoInst        -- See if the first arg is already a forall
		    -- ...then we can just extend the current substitution
        | Just (tv, co1_body) <- splitForAllTy_maybe co1
        -> opt_co (extendTvSubst env tv ty2') sym co1_body

                    -- See if is *now* a forall
        | Just (tv, opt_co1_body) <- splitForAllTy_maybe opt_co1
        -> substTyWith [tv] [ty2'] opt_co1_body	-- An inefficient one-variable substitution

        | otherwise
        -> TyConApp tc [opt_co1, ty2']

  where
    (co1 : cos1) = cos
    (co2 : _)    = cos1

    ty1' = substTy env co1
    ty2' = substTy env co2

	-- These opt_cos have the sym pushed into them
    opt_co1 = opt_co env sym co1
    opt_co2 = opt_co env sym co2

    the_unary_opt_co = TyConApp tc [opt_co1]

    opt_lr   sel = case splitAppTy_maybe opt_co1 of
                     Nothing -> the_unary_opt_co 
                     Just lr -> sel lr
    opt_csel sel = case splitCoPredTy_maybe opt_co1 of
                     Nothing -> the_unary_opt_co 
                     Just lr -> sel lr

-------------
opt_transL :: [NormalCo] -> [NormalCo] -> [NormalCo]
opt_transL = zipWith opt_trans

opt_trans :: NormalCo -> NormalCo -> NormalCo
opt_trans co1 co2
  | isIdNormCo co1 = co2
  | otherwise      = opt_trans1 co1 co2

opt_trans1 :: NormalNonIdCo -> NormalCo -> NormalCo
-- First arg is not the identity
opt_trans1 co1 co2
  | isIdNormCo co2 = co1
  | otherwise      = opt_trans2 co1 co2

opt_trans2 :: NormalNonIdCo -> NormalNonIdCo -> NormalCo
-- Neither arg is the identity
opt_trans2 (TyConApp tc [co1a,co1b]) co2
  | tc `hasKey` transCoercionTyConKey
  = opt_trans1 co1a (opt_trans2 co1b co2)

opt_trans2 co1 co2 
  | Just co <- opt_trans_rule co1 co2
  = co

opt_trans2 co1 (TyConApp tc [co2a,co2b])
  | tc `hasKey` transCoercionTyConKey
  , Just co1_2a <- opt_trans_rule co1 co2a
  = if isIdNormCo co1_2a
    then co2b
    else opt_trans2 co1_2a co2b

opt_trans2 co1 co2
  = mkTransCoercion co1 co2

------
opt_trans_rule :: NormalNonIdCo -> NormalNonIdCo -> Maybe NormalCo
opt_trans_rule (TyConApp tc1 args1) (TyConApp tc2 args2)
  | tc1 == tc2
  = case isCoercionTyCon_maybe tc1 of
      Nothing 
        -> Just (TyConApp tc1 (opt_transL args1 args2))
      Just (arity, desc) 
        | arity == length args1
        -> opt_trans_rule_equal_tc desc args1 args2
        | otherwise
        -> case opt_trans_rule_equal_tc desc 
                         (take arity args1) 
                         (take arity args2) of
              Just co -> Just $ mkAppTys co $ 
                         opt_transL (drop arity args1) (drop arity args2)
	      Nothing -> Nothing 
 
-- Push transitivity inside apply
opt_trans_rule co1 co2
  | Just (co1a, co1b) <- splitAppTy_maybe co1
  , Just (co2a, co2b) <- etaApp_maybe co2
  = Just (mkAppTy (opt_trans co1a co2a) (opt_trans co1b co2b))

  | Just (co2a, co2b) <- splitAppTy_maybe co2
  , Just (co1a, co1b) <- etaApp_maybe co1
  = Just (mkAppTy (opt_trans co1a co2a) (opt_trans co1b co2b))

-- Push transitivity inside (s~t)=>r
-- We re-use the CoVar rather than using mkCoPredTy
-- See Note [Subtle shadowing in coercions]
opt_trans_rule co1 co2
  | Just (cv1,r1) <- splitForAllTy_maybe co1
  , isCoVar cv1
  , Just (s1,t1) <- coVarKind_maybe cv1
  , Just (s2,t2,r2) <- etaCoPred_maybe co2
  = Just (ForAllTy (mkCoVar (coVarName cv1) (mkCoKind (opt_trans s1 s2) (opt_trans t1 t2)))
                   (opt_trans r1 r2))

  | Just (cv2,r2) <- splitForAllTy_maybe co2
  , isCoVar cv2
  , Just (s2,t2) <- coVarKind_maybe cv2
  , Just (s1,t1,r1) <- etaCoPred_maybe co1
  = Just (ForAllTy (mkCoVar (coVarName cv2) (mkCoKind (opt_trans s1 s2) (opt_trans t1 t2)))
                   (opt_trans r1 r2))

-- Push transitivity inside forall
opt_trans_rule co1 co2
  | Just (tv1,r1) <- splitTypeForAll_maybe co1
  , Just (tv2,r2) <- etaForAll_maybe co2
  , let r2' = substTyWith [tv2] [TyVarTy tv1] r2
  = Just (ForAllTy tv1 (opt_trans2 r1 r2'))

  | Just (tv2,r2) <- splitTypeForAll_maybe co2
  , Just (tv1,r1) <- etaForAll_maybe co1
  , let r1' = substTyWith [tv1] [TyVarTy tv2] r1
  = Just (ForAllTy tv1 (opt_trans2 r1' r2))

opt_trans_rule co1 co2
{- 	Omitting for now, because unsound
  | Just (sym1, (ax_tc1, ax1_args, ax_tvs, ax_lhs, ax_rhs)) <- co1_is_axiom_maybe
  , Just (sym2, (ax_tc2, ax2_args, _, _, _)) <- co2_is_axiom_maybe
  , ax_tc1 == ax_tc2
  , sym1 /= sym2
  = Just $
    if sym1 
    then substTyWith ax_tvs (opt_transL (map mkSymCoercion ax1_args) ax2_args) ax_rhs
    else substTyWith ax_tvs (opt_transL ax1_args (map mkSymCoercion ax2_args)) ax_lhs
-}

  | Just (sym, (ax_tc, ax_args, ax_tvs, ax_lhs, _)) <- co1_is_axiom_maybe
  , Just cos <- matchesAxiomLhs ax_tvs ax_lhs co2
  = Just $ 
    if sym 
    then mkSymCoercion $ TyConApp ax_tc (opt_transL (map mkSymCoercion cos) ax_args)
    else                 TyConApp ax_tc (opt_transL ax_args cos)

  | Just (sym, (ax_tc, ax_args, ax_tvs, ax_lhs, _)) <- isAxiom_maybe co2
  , Just cos <- matchesAxiomLhs ax_tvs ax_lhs co1
  = Just $ 
    if sym 
    then mkSymCoercion $ TyConApp ax_tc (opt_transL ax_args (map mkSymCoercion cos))
    else                 TyConApp ax_tc (opt_transL cos ax_args)
  where
    co1_is_axiom_maybe = isAxiom_maybe co1
    co2_is_axiom_maybe = isAxiom_maybe co2

opt_trans_rule co1 co2	-- Identity rule
  | (ty1,_) <- coercionKind co1
  , (_,ty2) <- coercionKind co2
  , ty1 `coreEqType` ty2
  = Just ty2

opt_trans_rule _ _ = Nothing

-----------  
isAxiom_maybe :: Coercion -> Maybe (Bool, (TyCon, [Coercion], [TyVar], Type, Type))
isAxiom_maybe co
  | Just (tc, args) <- splitTyConApp_maybe co
  , Just (_, desc)  <- isCoercionTyCon_maybe tc
  = case desc of
      CoAxiom { co_ax_tvs = tvs, co_ax_lhs = lhs, co_ax_rhs = rhs } 
            -> Just (False, (tc, args, tvs, lhs, rhs))
      CoSym | (arg1:_) <- args  
            -> case isAxiom_maybe arg1 of
                 Nothing           -> Nothing
                 Just (sym, stuff) -> Just (not sym, stuff)
      _ -> Nothing
  | otherwise
  = Nothing

matchesAxiomLhs :: [TyVar] -> Type -> Type -> Maybe [Type]
matchesAxiomLhs tvs ty_tmpl ty 
  = case tcMatchTy (mkVarSet tvs) ty_tmpl ty of
      Nothing    -> Nothing
      Just subst -> Just (map (substTyVar subst) tvs)

-----------  
opt_trans_rule_equal_tc :: CoTyConDesc -> [Coercion] -> [Coercion] -> Maybe Coercion
-- Rules for Coercion TyCons only

-- Push transitivity inside instantiation
opt_trans_rule_equal_tc desc [co1,ty1] [co2,ty2]
  | CoInst <- desc
  , ty1 `coreEqType` ty2
  , co1 `compatible_co` co2
  = Just (mkInstCoercion (opt_trans2 co1 co2) ty1) 

opt_trans_rule_equal_tc desc [co1] [co2]
  | CoLeft  <- desc, is_compat = Just (mkLeftCoercion res_co)
  | CoRight <- desc, is_compat = Just (mkRightCoercion res_co)
  | CoCsel1 <- desc, is_compat = Just (mkCsel1Coercion res_co)
  | CoCsel2 <- desc, is_compat = Just (mkCsel2Coercion res_co)
  | CoCselR <- desc, is_compat = Just (mkCselRCoercion res_co)
  where
    is_compat = co1 `compatible_co` co2
    res_co    = opt_trans2 co1 co2

opt_trans_rule_equal_tc _ _ _ = Nothing

-------------
compatible_co :: Coercion -> Coercion -> Bool
-- Check whether (co1 . co2) will be well-kinded
compatible_co co1 co2
  = x1 `coreEqType` x2		
  where
    (_,x1) = coercionKind co1
    (x2,_) = coercionKind co2

-------------
etaForAll_maybe :: Coercion -> Maybe (TyVar, Coercion)
-- Try to make the coercion be of form (forall tv. co)
etaForAll_maybe co
  | Just (tv, r) <- splitForAllTy_maybe co
  , not (isCoVar tv)	-- Check it is a *type* forall, not a (t1~t2)=>co
  = Just (tv, r)

  | (ty1,ty2) <- coercionKind co
  , Just (tv1, _) <- splitTypeForAll_maybe ty1
  , Just (tv2, _) <- splitTypeForAll_maybe ty2
  , tyVarKind tv1 `eqKind` tyVarKind tv2
  = Just (tv1, mkInstCoercion co (mkTyVarTy tv1))

  | otherwise
  = Nothing

etaCoPred_maybe :: Coercion -> Maybe (Coercion, Coercion, Coercion)
etaCoPred_maybe co 
  | Just (s,t,r) <- splitCoPredTy_maybe co
  = Just (s,t,r)
  
  --  co :: (s1~t1)=>r1 ~ (s2~t2)=>r2
  | (ty1,ty2) <- coercionKind co	-- We know ty1,ty2 have same kind
  , Just (s1,_,_) <- splitCoPredTy_maybe ty1
  , Just (s2,_,_) <- splitCoPredTy_maybe ty2
  , typeKind s1 `eqKind` typeKind s2	-- t1,t2 have same kinds
  = Just (mkCsel1Coercion co, mkCsel2Coercion co, mkCselRCoercion co)
  
  | otherwise
  = Nothing

etaApp_maybe :: Coercion -> Maybe (Coercion, Coercion)
-- Split a coercion g :: t1a t1b ~ t2a t2b
-- into (left g, right g) if possible
etaApp_maybe co
  | Just (co1, co2) <- splitAppTy_maybe co
  = Just (co1, co2)

  | (ty1,ty2) <- coercionKind co
  , Just (ty1a, _) <- splitAppTy_maybe ty1
  , Just (ty2a, _) <- splitAppTy_maybe ty2
  , typeKind ty1a `eqKind` typeKind ty2a
  = Just (mkLeftCoercion co, mkRightCoercion co)

  | otherwise
  = Nothing

-------------
splitTypeForAll_maybe :: Type -> Maybe (TyVar, Type)
-- Returns Just only for a *type* forall, not a (t1~t2)=>co
splitTypeForAll_maybe ty
  | Just (tv, rty) <- splitForAllTy_maybe ty
  , not (isCoVar tv)
  = Just (tv, rty)

  | otherwise
  = Nothing

-------------
isIdNormCo :: NormalCo -> Bool
-- Cheap identity test: look for coercions with no coercion variables at all
-- So it'll return False for (sym g `trans` g)
isIdNormCo ty = go ty
  where
    go (TyVarTy tv)  	       = not (isCoVar tv)
    go (AppTy t1 t2) 	       = go t1 && go t2
    go (FunTy t1 t2) 	       = go t1 && go t2
    go (ForAllTy tv ty)        = go (tyVarKind tv) && go ty
    go (TyConApp tc tys)       = not (isCoercionTyCon tc) && all go tys
    go (PredTy (IParam _ ty))  = go ty
    go (PredTy (ClassP _ tys)) = all go tys
    go (PredTy (EqPred t1 t2)) = go t1 && go t2
\end{code}  
