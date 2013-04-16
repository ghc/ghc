\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module TcCanonical(
    canonicalize, flatten, flattenMany, occurCheckExpand,
    FlattenMode (..),
    StopOrContinue (..)
 ) where

#include "HsVersions.h"

import TcRnTypes
import TcType
import Type
import Kind
import TcEvidence
import Class
import TyCon
import TypeRep
import Var
import VarEnv
import Outputable
import Control.Monad    ( when )
import MonadUtils
import Control.Applicative ( (<|>) )

import TrieMap
import VarSet
import TcSMonad
import FastString

import Util


import TysWiredIn ( eqTyCon )

import Data.Maybe ( fromMaybe )
-- import Data.List  ( zip4 )
\end{code}


%************************************************************************
%*                                                                      *
%*                      The Canonicaliser                               *
%*                                                                      *
%************************************************************************

Note [Canonicalization]
~~~~~~~~~~~~~~~~~~~~~~~

Canonicalization converts a flat constraint to a canonical form. It is
unary (i.e. treats individual constraints one at a time), does not do
any zonking, but lives in TcS monad because it needs to create fresh
variables (for flattening) and consult the inerts (for efficiency).

The execution plan for canonicalization is the following:
 
  1) Decomposition of equalities happens as necessary until we reach a 
     variable or type family in one side. There is no decomposition step
     for other forms of constraints. 

  2) If, when we decompose, we discover a variable on the head then we 
     look at inert_eqs from the current inert for a substitution for this 
     variable and contine decomposing. Hence we lazily apply the inert 
     substitution if it is needed. 

  3) If no more decomposition is possible, we deeply apply the substitution
     from the inert_eqs and continue with flattening.

  4) During flattening, we examine whether we have already flattened some 
     function application by looking at all the CTyFunEqs with the same 
     function in the inert set. The reason for deeply applying the inert 
     substitution at step (3) is to maximise our chances of matching an 
     already flattened family application in the inert. 

The net result is that a constraint coming out of the canonicalization 
phase cannot be rewritten any further from the inerts (but maybe /it/ can 
rewrite an inert or still interact with an inert in a further phase in the
simplifier.

\begin{code}

-- Informative results of canonicalization
data StopOrContinue 
  = ContinueWith Ct   -- Either no canonicalization happened, or if some did 
                      -- happen, it is still safe to just keep going with this 
                      -- work item. 
  | Stop              -- Some canonicalization happened, extra work is now in 
                      -- the TcS WorkList. 

instance Outputable StopOrContinue where
  ppr Stop             = ptext (sLit "Stop")
  ppr (ContinueWith w) = ptext (sLit "ContinueWith") <+> ppr w


continueWith :: Ct -> TcS StopOrContinue
continueWith = return . ContinueWith

andWhenContinue :: TcS StopOrContinue 
                -> (Ct -> TcS StopOrContinue) 
                -> TcS StopOrContinue
andWhenContinue tcs1 tcs2
  = do { r <- tcs1
       ; case r of
           Stop            -> return Stop
           ContinueWith ct -> tcs2 ct }

\end{code}

Note [Caching for canonicals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
Our plan with pre-canonicalization is to be able to solve a constraint
really fast from existing bindings in TcEvBinds. So one may think that
the condition (isCNonCanonical) is not necessary.  However consider
the following setup:

InertSet = { [W] d1 : Num t } 
WorkList = { [W] d2 : Num t, [W] c : t ~ Int} 

Now, we prioritize equalities, but in our concrete example
(should_run/mc17.hs) the first (d2) constraint is dealt with first,
because (t ~ Int) is an equality that only later appears in the
worklist since it is pulled out from a nested implication
constraint. So, let's examine what happens:
 
   - We encounter work item (d2 : Num t)

   - Nothing is yet in EvBinds, so we reach the interaction with inerts 
     and set:
              d2 := d1 
    and we discard d2 from the worklist. The inert set remains unaffected.

   - Now the equation ([W] c : t ~ Int) is encountered and kicks-out
     (d1 : Num t) from the inerts.  Then that equation gets
     spontaneously solved, perhaps. We end up with:
        InertSet : { [G] c : t ~ Int }
        WorkList : { [W] d1 : Num t} 

   - Now we examine (d1), we observe that there is a binding for (Num
     t) in the evidence binds and we set:
             d1 := d2 
     and end up in a loop!

Now, the constraints that get kicked out from the inert set are always
Canonical, so by restricting the use of the pre-canonicalizer to
NonCanonical constraints we eliminate this danger. Moreover, for
canonical constraints we already have good caching mechanisms
(effectively the interaction solver) and we are interested in reducing
things like superclasses of the same non-canonical constraint being
generated hence I don't expect us to lose a lot by introducing the
(isCNonCanonical) restriction.

A similar situation can arise in TcSimplify, at the end of the
solve_wanteds function, where constraints from the inert set are
returned as new work -- our substCt ensures however that if they are
not rewritten by subst, they remain canonical and hence we will not
attempt to solve them from the EvBinds. If on the other hand they did
get rewritten and are now non-canonical they will still not match the
EvBinds, so we are again good.



\begin{code}

-- Top-level canonicalization
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

canonicalize :: Ct -> TcS StopOrContinue
canonicalize ct@(CNonCanonical { cc_ev = fl, cc_depth  = d })
  = do { traceTcS "canonicalize (non-canonical)" (ppr ct)
       ; {-# SCC "canEvVar" #-}
         canEvVar d fl (classifyPredType (ctPred ct)) }

canonicalize (CDictCan { cc_depth  = d
                       , cc_ev = fl
                       , cc_class  = cls
                       , cc_tyargs = xis })
  = {-# SCC "canClass" #-}
    canClass d fl cls xis -- Do not add any superclasses
canonicalize (CTyEqCan { cc_depth  = d
                       , cc_ev = fl
                       , cc_tyvar  = tv
                       , cc_rhs    = xi })
  = {-# SCC "canEqLeafTyVarLeftRec" #-}
    canEqLeafTyVarLeftRec d fl tv xi

canonicalize (CFunEqCan { cc_depth = d
                        , cc_ev = fl
                        , cc_fun    = fn
                        , cc_tyargs = xis1
                        , cc_rhs    = xi2 })
  = {-# SCC "canEqLeafFunEqLeftRec" #-}
    canEqLeafFunEqLeftRec d fl (fn,xis1) xi2

canonicalize (CIrredEvCan { cc_ev = fl
                          , cc_depth = d
                          , cc_ty = xi })
  = canIrred d fl xi


canEvVar :: SubGoalDepth 
         -> CtEvidence 
         -> PredTree 
         -> TcS StopOrContinue
-- Called only for non-canonical EvVars 
canEvVar d fl pred_classifier 
  = case pred_classifier of
      ClassPred cls tys -> canClassNC d fl cls tys 
      EqPred ty1 ty2    -> canEqNC    d fl ty1 ty2 
      IrredPred ev_ty   -> canIrred   d fl ev_ty
      TuplePred tys     -> canTuple   d fl tys
\end{code}


%************************************************************************
%*                                                                      *
%*                      Tuple Canonicalization
%*                                                                      *
%************************************************************************

\begin{code}
canTuple :: SubGoalDepth -- Depth 
         -> CtEvidence -> [PredType] -> TcS StopOrContinue
canTuple d fl tys
  = do { traceTcS "can_pred" (text "TuplePred!")
       ; let xcomp = EvTupleMk
             xdecomp x = zipWith (\_ i -> EvTupleSel x i) tys [0..]             
       ; ctevs <- xCtFlavor fl tys (XEvTerm xcomp xdecomp)
       ; mapM_ add_to_work ctevs
       ; return Stop }
  where
    add_to_work fl = addToWork $ canEvVar d fl (classifyPredType (ctEvPred fl))
\end{code}


%************************************************************************
%*                                                                      *
%*                      Class Canonicalization
%*                                                                      *
%************************************************************************

\begin{code}
canClass, canClassNC 
   :: SubGoalDepth -- Depth
   -> CtEvidence  
   -> Class -> [Type] -> TcS StopOrContinue
-- Precondition: EvVar is class evidence 

-- The canClassNC version is used on non-canonical constraints
-- and adds superclasses.  The plain canClass version is used
-- for already-canonical class constraints (but which might have
-- been subsituted or somthing), and hence do not need superclasses

canClassNC d fl cls tys 
  = canClass d fl cls tys 
    `andWhenContinue` emitSuperclasses

canClass d fl cls tys
  = do { -- sctx <- getTcSContext
       ; (xis, cos) <- flattenMany d FMFullFlatten fl tys
       ; let co = mkTcTyConAppCo (classTyCon cls) cos 
             xi = mkClassPred cls xis
             
       ; mb <- rewriteCtFlavor fl xi co

       ; case mb of
           Just new_fl -> 
             let (ClassPred cls xis_for_dict) = classifyPredType (ctEvPred new_fl)
             in continueWith $ 
                CDictCan { cc_ev = new_fl
                         , cc_tyargs = xis_for_dict, cc_class = cls, cc_depth = d }
           Nothing -> return Stop }

emitSuperclasses :: Ct -> TcS StopOrContinue
emitSuperclasses ct@(CDictCan { cc_depth = d, cc_ev = fl
                              , cc_tyargs = xis_new, cc_class = cls })
            -- Add superclasses of this one here, See Note [Adding superclasses]. 
            -- But only if we are not simplifying the LHS of a rule. 
 = do { newSCWorkFromFlavored d fl cls xis_new
      -- Arguably we should "seq" the coercions if they are derived, 
      -- as we do below for emit_kind_constraint, to allow errors in
      -- superclasses to be executed if deferred to runtime! 
      ; continueWith ct }
emitSuperclasses _ = panic "emit_superclasses of non-class!"
\end{code}

Note [Adding superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~ 
Since dictionaries are canonicalized only once in their lifetime, the
place to add their superclasses is canonicalisation (The alternative
would be to do it during constraint solving, but we'd have to be
extremely careful to not repeatedly introduced the same superclass in
our worklist). Here is what we do:

For Givens: 
       We add all their superclasses as Givens. 

For Wanteds: 
       Generally speaking we want to be able to add superclasses of 
       wanteds for two reasons:

       (1) Oportunities for improvement. Example: 
                  class (a ~ b) => C a b 
           Wanted constraint is: C alpha beta 
           We'd like to simply have C alpha alpha. Similar 
           situations arise in relation to functional dependencies. 
           
       (2) To have minimal constraints to quantify over: 
           For instance, if our wanted constraint is (Eq a, Ord a) 
           we'd only like to quantify over Ord a. 

       To deal with (1) above we only add the superclasses of wanteds
       which may lead to improvement, that is: equality superclasses or 
       superclasses with functional dependencies. 

       We deal with (2) completely independently in TcSimplify. See 
       Note [Minimize by SuperClasses] in TcSimplify. 


       Moreover, in all cases the extra improvement constraints are 
       Derived. Derived constraints have an identity (for now), but 
       we don't do anything with their evidence. For instance they 
       are never used to rewrite other constraints. 

       See also [New Wanted Superclass Work] in TcInteract. 


For Deriveds: 
       We do nothing.

Here's an example that demonstrates why we chose to NOT add
superclasses during simplification: [Comes from ticket #4497]
 
   class Num (RealOf t) => Normed t
   type family RealOf x

Assume the generated wanted constraint is: 
   RealOf e ~ e, Normed e 
If we were to be adding the superclasses during simplification we'd get: 
   Num uf, Normed e, RealOf e ~ e, RealOf e ~ uf 
==> 
   e ~ uf, Num uf, Normed e, RealOf e ~ e 
==> [Spontaneous solve] 
   Num uf, Normed uf, RealOf uf ~ uf 

While looks exactly like our original constraint. If we add the superclass again we'd loop. 
By adding superclasses definitely only once, during canonicalisation, this situation can't 
happen.

\begin{code}

newSCWorkFromFlavored :: SubGoalDepth -- Depth
                      -> CtEvidence -> Class -> [Xi] -> TcS ()
-- Returns superclasses, see Note [Adding superclasses]
newSCWorkFromFlavored d flavor cls xis 
  | isDerived flavor 
  = return ()  -- Deriveds don't yield more superclasses because we will
               -- add them transitively in the case of wanteds. 
    
  | isGiven flavor 
  = do { let sc_theta = immSuperClasses cls xis 
             xev_decomp x = zipWith (\_ i -> EvSuperClass x i) sc_theta [0..] 
             xev = XEvTerm { ev_comp   = panic "Can't compose for given!" 
                           , ev_decomp = xev_decomp }
       ; ctevs <- xCtFlavor flavor sc_theta xev

       ; traceTcS "newSCWork/Given" $ ppr "ctevs =" <+> ppr ctevs 
       ; mapM_ emit_non_can ctevs }

  | isEmptyVarSet (tyVarsOfTypes xis)
  = return () -- Wanteds with no variables yield no deriveds.
              -- See Note [Improvement from Ground Wanteds]

  | otherwise -- Wanted case, just add those SC that can lead to improvement. 
  = do { let sc_rec_theta = transSuperClasses cls xis 
             impr_theta   = filter is_improvement_pty sc_rec_theta
       ; traceTcS "newSCWork/Derived" $ text "impr_theta =" <+> ppr impr_theta
       ; mapM_ emit_der impr_theta }

  where emit_der pty = newDerived (ctev_wloc flavor) pty >>= mb_emit
        mb_emit Nothing     = return ()
        mb_emit (Just ctev) = emit_non_can ctev 
        emit_non_can ctev   = updWorkListTcS $ 
                              extendWorkListCt (CNonCanonical ctev d)

is_improvement_pty :: PredType -> Bool 
-- Either it's an equality, or has some functional dependency
is_improvement_pty ty = go (classifyPredType ty)
  where
    go (EqPred {})         = True 
    go (ClassPred cls _tys) = not $ null fundeps
      where (_,fundeps) = classTvsFds cls
    go (TuplePred ts)      = any is_improvement_pty ts
    go (IrredPred {})      = True -- Might have equalities after reduction?
\end{code}


%************************************************************************
%*                                                                      *
%*                      Irreducibles canonicalization
%*                                                                      *
%************************************************************************


\begin{code}
canIrred :: SubGoalDepth -- Depth
         -> CtEvidence -> TcType -> TcS StopOrContinue
-- Precondition: ty not a tuple and no other evidence form
canIrred d fl ty 
  = do { traceTcS "can_pred" (text "IrredPred = " <+> ppr ty) 
       ; (xi,co) <- flatten d FMFullFlatten fl ty -- co :: xi ~ ty
       ; let no_flattening = xi `eqType` ty 
                             -- In this particular case it is not safe to 
                             -- say 'isTcReflCo' because the new constraint may
                             -- be reducible!
       ; mb <- rewriteCtFlavor fl xi co 
       ; case mb of
             Just new_fl 
               | no_flattening
                 -> continueWith $
                    CIrredEvCan { cc_ev = new_fl, cc_ty = xi, cc_depth = d }
               | otherwise
                 -> canEvVar d new_fl (classifyPredType (ctEvPred new_fl))
             Nothing -> return Stop }

\end{code}

%************************************************************************
%*                                                                      *
%*        Flattening (eliminating all function symbols)                 *
%*                                                                      *
%************************************************************************

Note [Flattening]
~~~~~~~~~~~~~~~~~~~~
  flatten ty  ==>   (xi, cc)
    where
      xi has no type functions, unless they appear under ForAlls

      cc = Auxiliary given (equality) constraints constraining
           the fresh type variables in xi.  Evidence for these 
           is always the identity coercion, because internally the
           fresh flattening skolem variables are actually identified
           with the types they have been generated to stand in for.

Note that it is flatten's job to flatten *every type function it sees*.
flatten is only called on *arguments* to type functions, by canEqGiven.

Recall that in comments we use alpha[flat = ty] to represent a
flattening skolem variable alpha which has been generated to stand in
for ty.

----- Example of flattening a constraint: ------
  flatten (List (F (G Int)))  ==>  (xi, cc)
    where
      xi  = List alpha
      cc  = { G Int ~ beta[flat = G Int],
              F beta ~ alpha[flat = F beta] }
Here
  * alpha and beta are 'flattening skolem variables'.
  * All the constraints in cc are 'given', and all their coercion terms 
    are the identity.

NB: Flattening Skolems only occur in canonical constraints, which
are never zonked, so we don't need to worry about zonking doing
accidental unflattening.

Note that we prefer to leave type synonyms unexpanded when possible,
so when the flattener encounters one, it first asks whether its
transitive expansion contains any type function applications.  If so,
it expands the synonym and proceeds; if not, it simply returns the
unexpanded synonym.

\begin{code}

data FlattenMode = FMSubstOnly 
                 | FMFullFlatten

-- Flatten a bunch of types all at once.
flattenMany :: SubGoalDepth -- Depth
            -> FlattenMode
            -> CtEvidence -> [Type] -> TcS ([Xi], [TcCoercion])
-- Coercions :: Xi ~ Type 
-- Returns True iff (no flattening happened)
-- NB: The EvVar inside the flavor is unused, we merely want Given/Solved/Derived/Wanted info
flattenMany d f ctxt tys 
  = -- pprTrace "flattenMany" empty $
    go tys 
  where go []       = return ([],[])
        go (ty:tys) = do { (xi,co)    <- flatten d f ctxt ty
                         ; (xis,cos)  <- go tys
                         ; return (xi:xis,co:cos) }

-- Flatten a type to get rid of type function applications, returning
-- the new type-function-free type, and a collection of new equality
-- constraints.  See Note [Flattening] for more detail.
flatten :: SubGoalDepth -- Depth
        -> FlattenMode 
        -> CtEvidence -> TcType -> TcS (Xi, TcCoercion)
-- Postcondition: Coercion :: Xi ~ TcType
flatten d f ctxt ty 
  | Just ty' <- tcView ty
  = do { (xi, co) <- flatten d f ctxt ty'
       ; if eqType xi ty then return (ty,co) else return (xi,co) } 
       -- Small tweak for better error messages 

flatten _ _ _ xi@(LitTy {}) = return (xi, mkTcReflCo xi)

flatten d f ctxt (TyVarTy tv)
  = flattenTyVar d f ctxt tv

flatten d f ctxt (AppTy ty1 ty2)
  = do { (xi1,co1) <- flatten d f ctxt ty1
       ; (xi2,co2) <- flatten d f ctxt ty2
       ; return (mkAppTy xi1 xi2, mkTcAppCo co1 co2) }

flatten d f ctxt (FunTy ty1 ty2)
  = do { (xi1,co1) <- flatten d f ctxt ty1
       ; (xi2,co2) <- flatten d f ctxt ty2
       ; return (mkFunTy xi1 xi2, mkTcFunCo co1 co2) }

flatten d f fl (TyConApp tc tys)
  -- For a normal type constructor or data family application, we just
  -- recursively flatten the arguments.
  | not (isSynFamilyTyCon tc)
    = do { (xis,cos) <- flattenMany d f fl tys
         ; return (mkTyConApp tc xis, mkTcTyConAppCo tc cos) }

  -- Otherwise, it's a type function application, and we have to
  -- flatten it away as well, and generate a new given equality constraint
  -- between the application and a newly generated flattening skolem variable.
  | otherwise
  = ASSERT( tyConArity tc <= length tys )	-- Type functions are saturated
      do { (xis, cos) <- flattenMany d f fl tys
         ; let (xi_args, xi_rest)  = splitAt (tyConArity tc) xis
	       	 -- The type function might be *over* saturated
		 -- in which case the remaining arguments should
		 -- be dealt with by AppTys
               fam_ty = mkTyConApp tc xi_args
               
         ; (ret_co, rhs_xi, ct) <-
             case f of 
               FMSubstOnly -> 
                 return (mkTcReflCo fam_ty, fam_ty, [])
               FMFullFlatten -> 
                 do { flat_cache <- getFlatCache
                    ; case lookupTM fam_ty flat_cache of
                        Just ct 
                          | let ctev = cc_ev ct
                          , ctev `canRewrite` fl 
                          -> -- You may think that we can just return (cc_rhs ct) but not so. 
                             --            return (mkTcCoVarCo (ctId ct), cc_rhs ct, []) 
                             -- The cached constraint resides in the cache so we have to flatten 
                             -- the rhs to make sure we have applied any inert substitution to it.
                             -- Alternatively we could be applying the inert substitution to the 
                             -- cache as well when we interact an equality with the inert. 
                             -- The design choice is: do we keep the flat cache rewritten or not?
                             -- For now I say we don't keep it fully rewritten.
                            do { traceTcS "flatten/flat-cache hit" $ ppr ct
                               ; let rhs_xi = cc_rhs ct
                               ; (flat_rhs_xi,co) <- flatten (cc_depth ct) f ctev rhs_xi
                               ; let final_co = evTermCoercion (ctEvTerm ctev)
                                                `mkTcTransCo` mkTcSymCo co
                               ; return (final_co, flat_rhs_xi,[]) }
                          
                        _ | isGiven fl -- Given: make new flatten skolem
                          -> do { traceTcS "flatten/flat-cache miss" $ empty 
                                ; rhs_xi_var <- newFlattenSkolemTy fam_ty
                                ; let co = mkTcReflCo fam_ty
                                      new_fl = Given { ctev_gloc = ctev_gloc fl
                                                     , ctev_pred = mkTcEqPred fam_ty rhs_xi_var
                                                     , ctev_evtm = EvCoercion co }
                                      ct = CFunEqCan { cc_ev = new_fl
                                                     , cc_fun    = tc 
                                                     , cc_tyargs = xi_args 
                                                     , cc_rhs    = rhs_xi_var 
                                                     , cc_depth  = d }
                                      -- Update the flat cache
                                ; updFlatCache ct
                                ; return (co, rhs_xi_var, [ct]) }
                         | otherwise -- Wanted or Derived: make new unification variable
                         -> do { traceTcS "flatten/flat-cache miss" $ empty 
                               ; rhs_xi_var <- newFlexiTcSTy (typeKind fam_ty)
                               ; let pred = mkTcEqPred fam_ty rhs_xi_var
                                     wloc = ctev_wloc fl
                               ; mw <- newWantedEvVar wloc pred
                               ; case mw of
                                   Fresh ctev -> 
                                     do { let ct = CFunEqCan { cc_ev = ctev
                                                             , cc_fun = tc
                                                             , cc_tyargs = xi_args
                                                             , cc_rhs    = rhs_xi_var 
                                                             , cc_depth  = d }
                                          -- Update the flat cache: just an optimisation!
                                        ; updFlatCache ct
                                        ; return (evTermCoercion (ctEvTerm ctev), rhs_xi_var, [ct]) }
                                   Cached {} -> panic "flatten TyConApp, var must be fresh!" } 
                    }
                  -- Emit the flat constraints
         ; updWorkListTcS $ appendWorkListEqs ct
         ; let (cos_args, cos_rest) = splitAt (tyConArity tc) cos
         ; return ( mkAppTys rhs_xi xi_rest -- NB mkAppTys: rhs_xi might not be a type variable
                                            --    cf Trac #5655
                  , mkTcAppCos (mkTcSymCo ret_co `mkTcTransCo` mkTcTyConAppCo tc cos_args) $
                    cos_rest
                  ) 
         }

flatten d _f ctxt ty@(ForAllTy {})
-- We allow for-alls when, but only when, no type function
-- applications inside the forall involve the bound type variables.
  = do { let (tvs, rho) = splitForAllTys ty
       ; (rho', co) <- flatten d FMSubstOnly ctxt rho
       ; return (mkForAllTys tvs rho', foldr mkTcForAllCo co tvs) }

\end{code}

\begin{code}
flattenTyVar :: SubGoalDepth 
             -> FlattenMode 
             -> CtEvidence -> TcTyVar -> TcS (Xi, TcCoercion)
-- "Flattening" a type variable means to apply the substitution to it
flattenTyVar d f ctxt tv
  = do { ieqs <- getInertEqs
       ; let mco = tv_eq_subst (fst ieqs) tv  -- co : v ~ ty
       ; case mco of -- Done, but make sure the kind is zonked
           Nothing -> 
               do { let knd = tyVarKind tv
                  ; (new_knd,_kind_co) <- flatten d f ctxt knd
                  ; let ty = mkTyVarTy (setVarType tv new_knd)
                  ; return (ty, mkTcReflCo ty) }
           -- NB recursive call. 
           -- Why? Because inert subst. non-idempotent, Note [Detailed InertCans Invariants]
           -- In fact, because of flavors, it couldn't possibly be idempotent,
           -- this is explained in Note [Non-idempotent inert substitution]
           Just (co,ty) -> 
               do { (ty_final,co') <- flatten d f ctxt ty
                  ; return (ty_final, co' `mkTcTransCo` mkTcSymCo co) } }  
  where 
    tv_eq_subst subst tv
       | Just ct <- lookupVarEnv subst tv
       , let ctev = cc_ev ct
       , ctev `canRewrite` ctxt
       = Just (evTermCoercion (ctEvTerm ctev), cc_rhs ct)
              -- NB: even if ct is Derived we are not going to 
              -- touch the actual coercion so we are fine. 
       | otherwise = Nothing
\end{code}

Note [Non-idempotent inert substitution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The inert substitution is not idempotent in the broad sense. It is only idempotent in 
that it cannot rewrite the RHS of other inert equalities any further. An example of such 
an inert substitution is:

 [G] g1 : ta8 ~ ta4
 [W] g2 : ta4 ~ a5Fj

Observe that the wanted cannot rewrite the solved goal, despite the fact that ta4 appears on
an RHS of an equality. Now, imagine a constraint:

 [W] g3: ta8 ~ Int 

coming in. If we simply apply once the inert substitution we will get: 

 [W] g3_1: ta4 ~ Int 

and because potentially ta4 is untouchable we will try to insert g3_1 in the inert set, 
getting a panic since the inert only allows ONE equation per LHS type variable (as it 
should).

For this reason, when we reach to flatten a type variable, we flatten it recursively, 
so that we can make sure that the inert substitution /is/ fully applied.

Insufficient (non-recursive) rewriting was the reason for #5668.

\begin{code}

-----------------
addToWork :: TcS StopOrContinue -> TcS ()
addToWork tcs_action = tcs_action >>= stop_or_emit
  where stop_or_emit Stop              = return ()
        stop_or_emit (ContinueWith ct) = updWorkListTcS $ 
                                         extendWorkListCt ct
\end{code}


%************************************************************************
%*                                                                      *
%*        Equalities
%*                                                                      *
%************************************************************************

\begin{code}
canEqEvVarsCreated :: SubGoalDepth
                   -> [CtEvidence] -> TcS StopOrContinue
canEqEvVarsCreated _d [] = return Stop
canEqEvVarsCreated d (quad:quads) 
  = mapM_ (addToWork . do_quad) quads >> do_quad quad
           -- Add all but one to the work list
           -- and return the first (if any) for futher processing
  where do_quad fl = let EqPred ty1 ty2 = classifyPredType $ ctEvPred fl
                     in canEqNC d fl ty1 ty2
          -- Note the "NC": these are fresh equalities so we must be
          -- careful to add their kind constraints

-------------------------
canEqNC, canEq 
  :: SubGoalDepth 
  -> CtEvidence 
  -> Type -> Type -> TcS StopOrContinue

canEqNC d fl ty1 ty2
  = canEq d fl ty1 ty2
    `andWhenContinue` emitKindConstraint

canEq _d fl ty1 ty2
  | eqType ty1 ty2	-- Dealing with equality here avoids
    	     	 	-- later spurious occurs checks for a~a
  = if isWanted fl then
      setEvBind (ctev_evar fl) (EvCoercion (mkTcReflCo ty1)) >> return Stop
    else
      return Stop

-- If one side is a variable, orient and flatten,
-- WITHOUT expanding type synonyms, so that we tend to 
-- substitute a ~ Age rather than a ~ Int when @type Age = Int@
canEq d fl ty1@(TyVarTy {}) ty2 
  = canEqLeaf d fl ty1 ty2
canEq d fl ty1 ty2@(TyVarTy {})
  = canEqLeaf d fl ty1 ty2

-- See Note [Naked given applications]
canEq d fl ty1 ty2
  | Just ty1' <- tcView ty1 = canEq d fl ty1' ty2
  | Just ty2' <- tcView ty2 = canEq d fl ty1  ty2'

canEq d fl ty1@(TyConApp fn tys) ty2
  | isSynFamilyTyCon fn, length tys == tyConArity fn
  = canEqLeaf d fl ty1 ty2
canEq d fl ty1 ty2@(TyConApp fn tys)
  | isSynFamilyTyCon fn, length tys == tyConArity fn
  = canEqLeaf d fl ty1 ty2

canEq d fl ty1 ty2
  | Just (tc1,tys1) <- tcSplitTyConApp_maybe ty1
  , Just (tc2,tys2) <- tcSplitTyConApp_maybe ty2
  , isDecomposableTyCon tc1 && isDecomposableTyCon tc2
  = -- Generate equalities for each of the corresponding arguments
    if (tc1 /= tc2 || length tys1 /= length tys2)
    -- Fail straight away for better error messages
    then canEqFailure d fl
    else
    do { let xcomp xs  = EvCoercion (mkTcTyConAppCo tc1 (map evTermCoercion xs))
             xdecomp x = zipWith (\_ i -> EvCoercion $ mkTcNthCo i (evTermCoercion x)) tys1 [0..]
             xev = XEvTerm xcomp xdecomp
       ; ctevs <- xCtFlavor fl (zipWith mkTcEqPred tys1 tys2) xev
       ; canEqEvVarsCreated d ctevs }

-- See Note [Equality between type applications]
--     Note [Care with type applications] in TcUnify
canEq d fl ty1 ty2    -- e.g.  F a b ~ Maybe c
                          -- where F has arity 1
  | Just (s1,t1) <- tcSplitAppTy_maybe ty1
  , Just (s2,t2) <- tcSplitAppTy_maybe ty2
  = canEqAppTy d fl s1 t1 s2 t2

canEq d fl s1@(ForAllTy {}) s2@(ForAllTy {})
 | tcIsForAllTy s1, tcIsForAllTy s2
 , Wanted { ctev_wloc = loc, ctev_evar = orig_ev } <- fl 
 = do { let (tvs1,body1) = tcSplitForAllTys s1
            (tvs2,body2) = tcSplitForAllTys s2
      ; if not (equalLength tvs1 tvs2) then 
          canEqFailure d fl
        else
          do { traceTcS "Creating implication for polytype equality" $ ppr fl
             ; deferTcSForAllEq (loc,orig_ev) (tvs1,body1) (tvs2,body2) 
             ; return Stop } }
 | otherwise
 = do { traceTcS "Ommitting decomposition of given polytype equality" $ 
        pprEq s1 s2
      ; return Stop }
canEq d fl _ _  = canEqFailure d fl

------------------------
-- Type application
canEqAppTy :: SubGoalDepth 
           -> CtEvidence 
           -> Type -> Type -> Type -> Type
           -> TcS StopOrContinue
canEqAppTy d fl s1 t1 s2 t2
  = ASSERT( not (isKind t1) && not (isKind t2) )
    if isGiven fl then 
        do { traceTcS "canEq (app case)" $
                text "Ommitting decomposition of given equality between: " 
                    <+> ppr (AppTy s1 t1) <+> text "and" <+> ppr (AppTy s2 t2)
                   -- We cannot decompose given applications
                   -- because we no longer have 'left' and 'right'
           ; return Stop }
    else 
    do { let xevcomp [x,y] = EvCoercion (mkTcAppCo (evTermCoercion x) (evTermCoercion y))
             xevcomp _ = error "canEqAppTy: can't happen" -- Can't happen
             xev = XEvTerm { ev_comp = xevcomp
                           , ev_decomp = error "canEqAppTy: can't happen" }
       ; ctevs <- xCtFlavor fl [mkTcEqPred s1 s2, mkTcEqPred t1 t2] xev 
       ; canEqEvVarsCreated d ctevs }

canEqFailure :: SubGoalDepth -> CtEvidence -> TcS StopOrContinue
canEqFailure d fl = emitFrozenError fl d >> return Stop

------------------------
emitKindConstraint :: Ct -> TcS StopOrContinue
emitKindConstraint ct
  = case ct of 
      CTyEqCan { cc_depth = d
               , cc_ev = fl, cc_tyvar = tv
               , cc_rhs = ty }
          -> emit_kind_constraint d fl (mkTyVarTy tv) ty

      CFunEqCan { cc_depth = d
                , cc_ev = fl
                , cc_fun = fn, cc_tyargs = xis1
                , cc_rhs = xi2 }
          -> emit_kind_constraint d fl (mkTyConApp fn xis1) xi2

      _   -> continueWith ct
  where
    emit_kind_constraint d fl ty1 ty2 
       | compatKind k1 k2    -- True when ty1,ty2 are themselves kinds,
       = continueWith ct     -- because then k1, k2 are BOX
       
       | otherwise
       = ASSERT( isKind k1 && isKind k2 )
         do { kev <- 
                 do { mw <- newWantedEvVar kind_co_wloc (mkEqPred k1 k2) 
                    ; case mw of
                         Cached ev_tm -> return ev_tm
                         Fresh ctev   -> do { addToWork (canEq d ctev k1 k2) 
                                            ; return (ctEvTerm ctev) } }

            ; let xcomp [x] = mkEvKindCast x (evTermCoercion kev)
                  xcomp _   = panic "emit_kind_constraint:can't happen"
                  xdecomp x = [mkEvKindCast x (evTermCoercion kev)]
                  xev = XEvTerm xcomp xdecomp

            ; ctevs <- xCtFlavor fl [mkTcEqPred ty1 ty2] xev 
                     -- Important: Do not cache original as Solved since we are supposed to 
                     -- solve /exactly/ the same constraint later! Example:
                     -- (alpha :: kappa0) 
                     -- (T :: *)
                     -- Equality is: (alpha ~ T), so we will emitConstraint (kappa0 ~ *) but
                     -- we don't want to say that (alpha ~ T) is now Solved!

            ; case ctevs of
                []         -> return Stop
                [new_ctev] -> continueWith (ct { cc_ev = new_ctev }) 
                _          -> panic "emitKindConstraint" }
       where
         k1 = typeKind ty1
         k2 = typeKind ty2
         ctxt = mkKindErrorCtxtTcS ty1 k1 ty2 k2

         -- Always create a Wanted kind equality even if 
         -- you are decomposing a given constraint.
         -- NB: DV finds this reasonable for now. Maybe we have to revisit.
         kind_co_wloc = pushErrCtxtSameOrigin ctxt wanted_loc
         wanted_loc = case fl of
                         Wanted  { ctev_wloc = wloc } -> wloc
                         Derived { ctev_wloc = wloc } -> wloc
                         Given { ctev_gloc = gloc }   -> setCtLocOrigin gloc orig
         orig = TypeEqOrigin (UnifyOrigin ty1 ty2)
\end{code}

Note [Combining insoluble constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As this point we have an insoluble constraint, like Int~Bool.

 * If it is Wanted, delete it from the cache, so that subsequent
   Int~Bool constraints give rise to separate error messages

 * But if it is Derived, DO NOT delete from cache.  A class constraint
   may get kicked out of the inert set, and then have its functional
   dependency Derived constraints generated a second time. In that
   case we don't want to get two (or more) error messages by
   generating two (or more) insoluble fundep constraints from the same
   class constraint.
   

Note [Naked given applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider: 
   data A a 
   type T a = A a 
and the given equality:  
   [G] A a ~ T Int 
We will reach the case canEq where we do a tcSplitAppTy_maybe, but if
we dont have the guards (Nothing <- tcView ty1) (Nothing <- tcView
ty2) then the given equation is going to fall through and get
completely forgotten!

What we want instead is this clause to apply only when there is no
immediate top-level synonym; if there is one it will be later on
unfolded by the later stages of canEq.

Test-case is in typecheck/should_compile/GivenTypeSynonym.hs


Note [Equality between type applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we see an equality of the form s1 t1 ~ s2 t2 we can always split
it up into s1 ~ s2 /\ t1 ~ t2, since s1 and s2 can't be type
functions (type functions use the TyConApp constructor, which never
shows up as the LHS of an AppTy).  Other than type functions, types
in Haskell are always 

  (1) generative: a b ~ c d implies a ~ c, since different type
      constructors always generate distinct types

  (2) injective: a b ~ a d implies b ~ d; we never generate the
      same type from different type arguments.


Note [Canonical ordering for equality constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Implemented as (<+=) below:

  - Type function applications always come before anything else.  
  - Variables always come before non-variables (other than type
      function applications).

Note that we don't need to unfold type synonyms on the RHS to check
the ordering; that is, in the rules above it's OK to consider only
whether something is *syntactically* a type function application or
not.  To illustrate why this is OK, suppose we have an equality of the
form 'tv ~ S a b c', where S is a type synonym which expands to a
top-level application of the type function F, something like

  type S a b c = F d e

Then to canonicalize 'tv ~ S a b c' we flatten the RHS, and since S's
expansion contains type function applications the flattener will do
the expansion and then generate a skolem variable for the type
function application, so we end up with something like this:

  tv ~ x
  F d e ~ x

where x is the skolem variable.  This is one extra equation than
absolutely necessary (we could have gotten away with just 'F d e ~ tv'
if we had noticed that S expanded to a top-level type function
application and flipped it around in the first place) but this way
keeps the code simpler.

Unlike the OutsideIn(X) draft of May 7, 2010, we do not care about the
ordering of tv ~ tv constraints.  There are several reasons why we
might:

  (1) In order to be able to extract a substitution that doesn't
      mention untouchable variables after we are done solving, we might
      prefer to put touchable variables on the left. However, in and
      of itself this isn't necessary; we can always re-orient equality
      constraints at the end if necessary when extracting a substitution.

  (2) To ensure termination we might think it necessary to put
      variables in lexicographic order. However, this isn't actually 
      necessary as outlined below.

While building up an inert set of canonical constraints, we maintain
the invariant that the equality constraints in the inert set form an
acyclic rewrite system when viewed as L-R rewrite rules.  Moreover,
the given constraints form an idempotent substitution (i.e. none of
the variables on the LHS occur in any of the RHS's, and type functions
never show up in the RHS at all), the wanted constraints also form an
idempotent substitution, and finally the LHS of a given constraint
never shows up on the RHS of a wanted constraint.  There may, however,
be a wanted LHS that shows up in a given RHS, since we do not rewrite
given constraints with wanted constraints.

Suppose we have an inert constraint set


  tg_1 ~ xig_1         -- givens
  tg_2 ~ xig_2
  ...
  tw_1 ~ xiw_1         -- wanteds
  tw_2 ~ xiw_2
  ...

where each t_i can be either a type variable or a type function
application. Now suppose we take a new canonical equality constraint,
t' ~ xi' (note among other things this means t' does not occur in xi')
and try to react it with the existing inert set.  We show by induction
on the number of t_i which occur in t' ~ xi' that this process will
terminate.

There are several ways t' ~ xi' could react with an existing constraint:

TODO: finish this proof.  The below was for the case where the entire
inert set is an idempotent subustitution...

(b) We could have t' = t_j for some j.  Then we obtain the new
    equality xi_j ~ xi'; note that neither xi_j or xi' contain t_j.  We
    now canonicalize the new equality, which may involve decomposing it
    into several canonical equalities, and recurse on these.  However,
    none of the new equalities will contain t_j, so they have fewer
    occurrences of the t_i than the original equation.

(a) We could have t_j occurring in xi' for some j, with t' /=
    t_j. Then we substitute xi_j for t_j in xi' and continue.  However,
    since none of the t_i occur in xi_j, we have decreased the
    number of t_i that occur in xi', since we eliminated t_j and did not
    introduce any new ones.

\begin{code}
data TypeClassifier 
  = FskCls TcTyVar      -- ^ Flatten skolem 
  | VarCls TcTyVar      -- ^ Non-flatten-skolem variable 
  | FunCls TyCon [Type] -- ^ Type function, exactly saturated
  | OtherCls TcType     -- ^ Neither of the above


classify :: TcType -> TypeClassifier

classify (TyVarTy tv) 
  | isTcTyVar tv, 
    FlatSkol {} <- tcTyVarDetails tv = FskCls tv
  | otherwise                        = VarCls tv
classify (TyConApp tc tys) | isSynFamilyTyCon tc
                           , tyConArity tc == length tys
                           = FunCls tc tys
classify ty                | Just ty' <- tcView ty
	                   = case classify ty' of
                               OtherCls {} -> OtherCls ty
                               var_or_fn   -> var_or_fn
                           | otherwise 
                           = OtherCls ty

-- See note [Canonical ordering for equality constraints].
reOrient :: CtEvidence -> TypeClassifier -> TypeClassifier -> Bool	
-- (t1 `reOrient` t2) responds True 
--   iff we should flip to (t2~t1)
-- We try to say False if possible, to minimise evidence generation
--
-- Postcondition: After re-orienting, first arg is not OTherCls
reOrient _fl (OtherCls {}) (FunCls {})   = True
reOrient _fl (OtherCls {}) (FskCls {})   = True
reOrient _fl (OtherCls {}) (VarCls {})   = True
reOrient _fl (OtherCls {}) (OtherCls {}) = panic "reOrient"  -- One must be Var/Fun

reOrient _fl (FunCls {})   (VarCls _tv)  = False  
  -- But consider the following variation: isGiven fl && isMetaTyVar tv

  -- See Note [No touchables as FunEq RHS] in TcSMonad
reOrient _fl (FunCls {}) _                = False             -- Fun/Other on rhs

reOrient _fl (VarCls {}) (FunCls {})      = True 

reOrient _fl (VarCls {}) (FskCls {})      = False

reOrient _fl (VarCls {})  (OtherCls {})   = False
reOrient _fl (VarCls tv1)  (VarCls tv2)  
  | isMetaTyVar tv2 && not (isMetaTyVar tv1) = True 
  | otherwise                                = False 
  -- Just for efficiency, see CTyEqCan invariants 

reOrient _fl (FskCls {}) (VarCls tv2)     = isMetaTyVar tv2 
  -- Just for efficiency, see CTyEqCan invariants

reOrient _fl (FskCls {}) (FskCls {})     = False
reOrient _fl (FskCls {}) (FunCls {})     = True 
reOrient _fl (FskCls {}) (OtherCls {})   = False 

------------------

canEqLeaf :: SubGoalDepth -- Depth
          -> CtEvidence 
          -> Type -> Type 
          -> TcS StopOrContinue
-- Canonicalizing "leaf" equality constraints which cannot be
-- decomposed further (ie one of the types is a variable or
-- saturated type function application).  

-- Preconditions: 
--    * one of the two arguments is variable or family applications
--    * the two types are not equal (looking through synonyms)
canEqLeaf d fl s1 s2 
  | cls1 `re_orient` cls2
  = do { traceTcS "canEqLeaf (reorienting)" $ ppr fl <+> dcolon <+> pprEq s1 s2
       ; let xcomp [x] = EvCoercion (mkTcSymCo (evTermCoercion x))
             xcomp _ = panic "canEqLeaf: can't happen"
             xdecomp x = [EvCoercion (mkTcSymCo (evTermCoercion x))]
             xev = XEvTerm xcomp xdecomp
       ; ctevs <- xCtFlavor fl [mkTcEqPred s2 s1] xev 
       ; case ctevs of
           []     -> return Stop
           [ctev] -> canEqLeafOriented d ctev s2 s1
           _      -> panic "canEqLeaf" }

  | otherwise
  = do { traceTcS "canEqLeaf" $ ppr (mkTcEqPred s1 s2)
       ; canEqLeafOriented d fl s1 s2 }
  where
    re_orient = reOrient fl 
    cls1 = classify s1
    cls2 = classify s2

canEqLeafOriented :: SubGoalDepth -- Depth
                  -> CtEvidence
                  -> TcType -> TcType -> TcS StopOrContinue
-- By now s1 will either be a variable or a type family application
canEqLeafOriented d fl s1 s2
  = can_eq_split_lhs d fl s1 s2
  where can_eq_split_lhs d fl s1 s2
          | Just (fn,tys1) <- splitTyConApp_maybe s1
          = canEqLeafFunEqLeftRec d fl (fn,tys1) s2
          | Just tv <- getTyVar_maybe s1
          = canEqLeafTyVarLeftRec d fl tv s2
          | otherwise
          = pprPanic "canEqLeafOriented" $
            text "Non-variable or non-family equality LHS" <+> ppr (ctEvPred fl)

canEqLeafFunEqLeftRec :: SubGoalDepth
                      -> CtEvidence
                      -> (TyCon,[TcType]) -> TcType -> TcS StopOrContinue
canEqLeafFunEqLeftRec d fl (fn,tys1) ty2  -- fl :: F tys1 ~ ty2
  = do { traceTcS "canEqLeafFunEqLeftRec" $ pprEq (mkTyConApp fn tys1) ty2
       ; (xis1,cos1) <- 
           {-# SCC "flattenMany" #-}
           flattenMany d FMFullFlatten fl tys1 -- Flatten type function arguments
                                               -- cos1 :: xis1 ~ tys1
           
       ; let fam_head = mkTyConApp fn xis1
         -- Fancy higher-dimensional coercion between equalities!
       ; let co = mkTcTyConAppCo eqTyCon $ 
                  [mkTcReflCo (defaultKind $ typeKind ty2), mkTcTyConAppCo fn cos1, mkTcReflCo ty2]
             -- Why defaultKind? Same reason as the comment on TcType/mkTcEqPred. I trully hate this (DV)
             -- co :: (F xis1 ~ ty2) ~ (F tys1 ~ ty2)
             
       ; mb <- rewriteCtFlavor fl (mkTcEqPred fam_head ty2) co
       ; case mb of 
           Nothing -> return Stop
           Just new_fl -> canEqLeafFunEqLeft d new_fl (fn,xis1) ty2 }


canEqLeafFunEqLeft :: SubGoalDepth -- Depth
                   -> CtEvidence
                   -> (TyCon,[Xi])
                   -> TcType -> TcS StopOrContinue
-- Precondition: No more flattening is needed for the LHS
canEqLeafFunEqLeft d fl (fn,xis1) s2
 = {-# SCC "canEqLeafFunEqLeft" #-}
   do { traceTcS "canEqLeafFunEqLeft" $ pprEq (mkTyConApp fn xis1) s2
      ; (xi2,co2) <- 
          {-# SCC "flatten" #-} 
          flatten d FMFullFlatten fl s2 -- co2 :: xi2 ~ s2
          
      ; let fam_head = mkTyConApp fn xis1
      -- Fancy coercion between equalities! But it should just work! 
      ; let co = mkTcTyConAppCo eqTyCon $ [ mkTcReflCo (defaultKind $ typeKind s2)
                                          , mkTcReflCo fam_head, co2 ]
            -- Why defaultKind? Same reason as the comment at TcType/mkTcEqPred
            -- co :: (F xis1 ~ xi2) ~ (F xis1 ~ s2)
            --           new pred         old pred
      ; mb <- rewriteCtFlavor fl (mkTcEqPred fam_head xi2) co
      ; case mb of
          Nothing -> return Stop
          Just new_fl -> continueWith $ 
                         CFunEqCan { cc_ev = new_fl, cc_depth = d
                                   , cc_fun = fn, cc_tyargs = xis1, cc_rhs = xi2 } }   


canEqLeafTyVarLeftRec :: SubGoalDepth
                      -> CtEvidence
                      -> TcTyVar -> TcType -> TcS StopOrContinue
canEqLeafTyVarLeftRec d fl tv s2              -- fl :: tv ~ s2
  = do {  traceTcS "canEqLeafTyVarLeftRec" $ pprEq (mkTyVarTy tv) s2
       ; (xi1,co1) <- flattenTyVar d FMFullFlatten fl tv -- co1 :: xi1 ~ tv
       
       ; traceTcS "canEqLeafTyVarLeftRec2" $ empty 
         
       ; let co = mkTcTyConAppCo eqTyCon $ [ mkTcReflCo (defaultKind $ typeKind s2)
                                           , co1, mkTcReflCo s2]
             -- co :: (xi1 ~ s2) ~ (tv ~ s2)
       ; mb <- rewriteCtFlavor fl (mkTcEqPred xi1 s2) co
                -- NB that rewriteCtFlavor does not cache the result
                -- See Note [Caching loops]

       ; traceTcS "canEqLeafTyVarLeftRec3" $ empty 
               
       ; case mb of
           Nothing -> return Stop
           Just new_fl -> 
             case getTyVar_maybe xi1 of 
               Just tv' -> canEqLeafTyVarLeft d new_fl tv' s2
               Nothing  -> canEq d new_fl xi1 s2 }
    
canEqLeafTyVarLeft :: SubGoalDepth -- Depth
                   -> CtEvidence 
                   -> TcTyVar -> TcType -> TcS StopOrContinue
-- Precondition LHS is fully rewritten from inerts (but not RHS)
canEqLeafTyVarLeft d fl tv s2       -- eqv : tv ~ s2
  = do { let tv_ty = mkTyVarTy tv
       ; traceTcS "canEqLeafTyVarLeft" (pprEq tv_ty s2)
       ; (xi2, co2) <- flatten d FMFullFlatten fl s2 -- Flatten RHS co:xi2 ~ s2 
                       
       ; traceTcS "canEqLeafTyVarLeft" (nest 2 (vcat [ text "tv  =" <+> ppr tv
                                                     , text "s2  =" <+> ppr s2
                                                     , text "xi2 =" <+> ppr xi2]))

       -- Reflexivity exposed through flattening        
       ; if tv_ty `eqType` xi2 then
           when (isWanted fl) (setEvBind (ctev_evar fl) (EvCoercion co2)) >> 
           return Stop
         else do
       -- Not reflexivity but maybe an occurs error
       { let occ_check_result = occurCheckExpand tv xi2
             xi2' = fromMaybe xi2 occ_check_result
             co = mkTcTyConAppCo eqTyCon $
                  [mkTcReflCo (defaultKind $ typeKind s2), mkTcReflCo tv_ty, co2]
       ; mb <- rewriteCtFlavor fl (mkTcEqPred tv_ty xi2') co
                -- NB that rewriteCtFlavor does not cache the result (as it used to)
                -- which would be wrong if the constraint has an occurs error

       ; case mb of
           Just new_fl -> case occ_check_result of
                            Just {} -> continueWith $
                                       CTyEqCan { cc_ev = new_fl, cc_depth = d
                                                , cc_tyvar  = tv, cc_rhs = xi2' }
                            Nothing -> canEqFailure d new_fl
           Nothing -> return Stop
        } }
\end{code}

Note [Occurs check expansion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@occurCheckExpand tv xi@ expands synonyms in xi just enough to get rid
of occurrences of tv outside type function arguments, if that is
possible; otherwise, it returns Nothing.

For example, suppose we have
  type F a b = [a]
Then
  occurCheckExpand b (F Int b) = Just [Int]
but
  occurCheckExpand a (F a Int) = Nothing

We don't promise to do the absolute minimum amount of expanding
necessary, but we try not to do expansions we don't need to.  We
prefer doing inner expansions first.  For example,
  type F a b = (a, Int, a, [a])
  type G b   = Char
We have
  occurCheckExpand b (F (G b)) = F Char
even though we could also expand F to get rid of b.

See also Note [Type synonyms and canonicalization].

\begin{code}
occurCheckExpand :: TcTyVar -> Type -> Maybe Type
-- Check whether the given variable occurs in the given type.  We may
-- have needed to do some type synonym unfolding in order to get rid
-- of the variable, so we also return the unfolded version of the
-- type, which is guaranteed to be syntactically free of the given
-- type variable.  If the type is already syntactically free of the
-- variable, then the same type is returned.

occurCheckExpand tv ty
  | not (tv `elemVarSet` tyVarsOfType ty) = Just ty
  | otherwise                             = go ty
  where
    go t@(TyVarTy tv') | tv == tv' = Nothing
                       | otherwise = Just t
    go ty@(LitTy {}) = return ty
    go (AppTy ty1 ty2) = do { ty1' <- go ty1
           		    ; ty2' <- go ty2  
           		    ; return (mkAppTy ty1' ty2') }
    -- mkAppTy <$> go ty1 <*> go ty2
    go (FunTy ty1 ty2) = do { ty1' <- go ty1 
           		    ; ty2' <- go ty2 
           		    ; return (mkFunTy ty1' ty2') } 
    -- mkFunTy <$> go ty1 <*> go ty2
    go ty@(ForAllTy {})
       | tv `elemVarSet` tyVarsOfTypes tvs_knds = Nothing
           -- Can't expand away the kinds unless we create 
           -- fresh variables which we don't want to do at this point.
       | otherwise = do { rho' <- go rho
                        ; return (mkForAllTys tvs rho') }
       where
         (tvs,rho) = splitForAllTys ty
         tvs_knds  = map tyVarKind tvs 

    -- For a type constructor application, first try expanding away the
    -- offending variable from the arguments.  If that doesn't work, next
    -- see if the type constructor is a type synonym, and if so, expand
    -- it and try again.
    go ty@(TyConApp tc tys)
      | isSynFamilyTyCon tc    -- It's ok for tv to occur under a type family application
       = return ty             -- Eg.  (a ~ F a) is not an occur-check error
                               -- NB This case can't occur during canonicalisation,
                               --    because the arg is a Xi-type, but can occur in the
                               --    call from TcErrors
      | otherwise
      = (mkTyConApp tc <$> mapM go tys) <|> (tcView ty >>= go)
\end{code}

Note [Type synonyms and canonicalization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We treat type synonym applications as xi types, that is, they do not
count as type function applications.  However, we do need to be a bit
careful with type synonyms: like type functions they may not be
generative or injective.  However, unlike type functions, they are
parametric, so there is no problem in expanding them whenever we see
them, since we do not need to know anything about their arguments in
order to expand them; this is what justifies not having to treat them
as specially as type function applications.  The thing that causes
some subtleties is that we prefer to leave type synonym applications
*unexpanded* whenever possible, in order to generate better error
messages.

If we encounter an equality constraint with type synonym applications
on both sides, or a type synonym application on one side and some sort
of type application on the other, we simply must expand out the type
synonyms in order to continue decomposing the equality constraint into
primitive equality constraints.  For example, suppose we have

  type F a = [Int]

and we encounter the equality

  F a ~ [b]

In order to continue we must expand F a into [Int], giving us the
equality

  [Int] ~ [b]

which we can then decompose into the more primitive equality
constraint

  Int ~ b.

However, if we encounter an equality constraint with a type synonym
application on one side and a variable on the other side, we should
NOT (necessarily) expand the type synonym, since for the purpose of
good error messages we want to leave type synonyms unexpanded as much
as possible.

However, there is a subtle point with type synonyms and the occurs
check that takes place for equality constraints of the form tv ~ xi.
As an example, suppose we have

  type F a = Int

and we come across the equality constraint

  a ~ F a

This should not actually fail the occurs check, since expanding out
the type synonym results in the legitimate equality constraint a ~
Int.  We must actually do this expansion, because unifying a with F a
will lead the type checker into infinite loops later.  Put another
way, canonical equality constraints should never *syntactically*
contain the LHS variable in the RHS type.  However, we don't always
need to expand type synonyms when doing an occurs check; for example,
the constraint

  a ~ F b

is obviously fine no matter what F expands to. And in this case we
would rather unify a with F b (rather than F b's expansion) in order
to get better error messages later.

So, when doing an occurs check with a type synonym application on the
RHS, we use some heuristics to find an expansion of the RHS which does
not contain the variable from the LHS.  In particular, given

  a ~ F t1 ... tn

we first try expanding each of the ti to types which no longer contain
a.  If this turns out to be impossible, we next try expanding F
itself, and so on.

