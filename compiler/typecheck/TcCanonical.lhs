\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module TcCanonical(
    canonicalize,
    StopOrContinue (..)
 ) where

#include "HsVersions.h"

import BasicTypes ( IPName )
import TcErrors
import TcRnTypes
import TcType
import Type
import Kind
import TcEvidence
import Class
import TyCon
import TypeRep
import Name ( Name )
import Var
import VarEnv
import Util( equalLength )
import Outputable
import Control.Monad    ( when, unless, zipWithM )
import MonadUtils
import Control.Applicative ( (<|>) )

import TrieMap
import VarSet
import TcSMonad
import FastString

import Data.Maybe ( isNothing )
import Data.List  ( zip4 )
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
canonicalize ct@(CNonCanonical { cc_id = ev, cc_flavor = fl, cc_depth  = d })
  = do { traceTcS "canonicalize (non-canonical)" (ppr ct)
       ; {-# SCC "canEvVar" #-}
         canEvVar ev (classifyPredType (evVarPred ev)) d fl }

canonicalize (CDictCan { cc_id = ev, cc_depth = d
                       , cc_flavor = fl
                       , cc_class  = cls
                       , cc_tyargs = xis })
  = {-# SCC "canClass" #-}
    canClass d fl ev cls xis -- Do not add any superclasses
canonicalize (CTyEqCan { cc_id = ev, cc_depth = d
                       , cc_flavor = fl
                       , cc_tyvar  = tv
                       , cc_rhs    = xi })
  = {-# SCC "canEqLeafTyVarLeftRec" #-}
    canEqLeafTyVarLeftRec d fl ev tv xi

canonicalize (CFunEqCan { cc_id = ev, cc_depth = d
                        , cc_flavor = fl
                        , cc_fun    = fn
                        , cc_tyargs = xis1
                        , cc_rhs    = xi2 })
  = {-# SCC "canEqLeafFunEqLeftRec" #-}
    canEqLeafFunEqLeftRec d fl ev (fn,xis1) xi2

canonicalize (CIPCan { cc_id = ev, cc_depth = d
                     , cc_flavor = fl
                     , cc_ip_nm  = nm
                     , cc_ip_ty  = xi })
  = canIP d fl ev nm xi
canonicalize (CIrredEvCan { cc_id = ev, cc_flavor = fl
                          , cc_depth = d
                          , cc_ty = xi })
  = canIrred d fl ev xi


canEvVar :: EvVar -> PredTree 
         -> SubGoalDepth -> CtFlavor -> TcS StopOrContinue
-- Called only for non-canonical EvVars 
canEvVar ev pred_classifier d fl 
  = case pred_classifier of
      ClassPred cls tys -> canClassNC d fl ev cls tys 
      EqPred ty1 ty2    -> canEqNC    d fl ev ty1 ty2 
      IPPred nm ty      -> canIP      d fl ev nm ty
      IrredPred ev_ty   -> canIrred   d fl ev ev_ty
      TuplePred tys     -> canTuple   d fl ev tys
\end{code}


%************************************************************************
%*                                                                      *
%*                      Tuple Canonicalization
%*                                                                      *
%************************************************************************

\begin{code}
canTuple :: SubGoalDepth -- Depth 
         -> CtFlavor -> EvVar -> [PredType] -> TcS StopOrContinue
canTuple d fl ev tys
  = do { traceTcS "can_pred" (text "TuplePred!") 
       ; evs <- zipWithM can_pred_tup_one tys [0..]
       ; if (isWanted fl) then 
             do {_unused_fl <- setEvBind ev (EvTupleMk evs) fl
                ; return Stop }
         else return Stop }
  where 
     can_pred_tup_one ty n
          = do { evc <- newEvVar fl ty
               ; let ev' = evc_the_evvar evc
               ; fl' <- if isGivenOrSolved fl then 
                            setEvBind ev' (EvTupleSel ev n) fl
                        else return fl
               ; when (isNewEvVar evc) $
                      addToWork (canEvVar ev' (classifyPredType (evVarPred ev')) d fl')
               ; return ev' }
\end{code}


%************************************************************************
%*                                                                      *
%*                      Implicit Parameter Canonicalization
%*                                                                      *
%************************************************************************

\begin{code}
canIP :: SubGoalDepth -- Depth 
      -> CtFlavor -> EvVar 
      -> IPName Name -> Type -> TcS StopOrContinue
-- Precondition: EvVar is implicit parameter evidence
canIP d fl v nm ty
  =    -- Note [Canonical implicit parameter constraints] explains why it's 
       -- possible in principle to not flatten, but since flattening applies 
       -- the inert substitution we choose to flatten anyway.
    do { (xi,co) <- flatten d fl (mkIPPred nm ty)
       ; let no_flattening = isTcReflCo co 
       ; if no_flattening then
            let IPPred _ xi_in = classifyPredType xi 
            in continueWith $ CIPCan { cc_id = v, cc_flavor = fl
                                     , cc_ip_nm = nm, cc_ip_ty = xi_in
                                     , cc_depth = d }
         else do { evc <- newEvVar fl xi
                 ; let v_new          = evc_the_evvar evc
                       IPPred _ ip_xi = classifyPredType xi
                 ; fl_new <- case fl of 
                               Wanted {}  -> setEvBind v (EvCast v_new co) fl 
                               Given {}   -> setEvBind v_new (EvCast v (mkTcSymCo co)) fl
                               Derived {} -> return fl
                 ; if isNewEvVar evc then
                       continueWith $ CIPCan { cc_id     = v_new
                                             , cc_flavor = fl_new, cc_ip_nm = nm
                                             , cc_ip_ty  = ip_xi
                                             , cc_depth  = d }
                   else return Stop } }
\end{code}

Note [Canonical implicit parameter constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The type in a canonical implicit parameter constraint doesn't need to
be a xi (type-function-free type) since we can defer the flattening
until checking this type for equality with another type.  If we
encounter two IP constraints with the same name, they MUST have the
same type, and at that point we can generate a flattened equality
constraint between the types.  (On the other hand, the types in two
class constraints for the same class MAY be equal, so they need to be
flattened in the first place to facilitate comparing them.)

%************************************************************************
%*                                                                      *
%*                      Class Canonicalization
%*                                                                      *
%************************************************************************

\begin{code}
canClass, canClassNC 
   :: SubGoalDepth -- Depth
   -> CtFlavor -> EvVar  
   -> Class -> [Type] -> TcS StopOrContinue
-- Precondition: EvVar is class evidence 

-- The canClassNC version is used on non-canonical constraints
-- and adds superclasses.  The plain canClass version is used
-- for already-canonical class constraints (but which might have
-- been subsituted or somthing), and hence do not need superclasses

canClassNC d fl ev cls tys 
  = canClass d fl ev cls tys 
    `andWhenContinue` emitSuperclasses

canClass d fl v cls tys
  = do { -- sctx <- getTcSContext
       ; (xis, cos) <- flattenMany d fl tys
       ; let co = mkTcTyConAppCo (classTyCon cls) cos 
             xi = mkClassPred cls xis

       ; let no_flattening = all isTcReflCo cos
                  -- No flattening, continue with canonical
       ; if no_flattening then 
             continueWith $ CDictCan { cc_id = v, cc_flavor = fl
                                     , cc_tyargs = xis, cc_class = cls
                                     , cc_depth = d }
                   -- Flattening happened
         else do { evc <- newEvVar fl xi
                 ; let v_new = evc_the_evvar evc
                 ; fl_new <- case fl of
                     Wanted  {} -> setEvBind v (EvCast v_new co) fl
                     Given   {} -> setEvBind v_new (EvCast v (mkTcSymCo co)) fl
                     Derived {} -> return fl
                    -- Continue only if flat constraint is new
                 ; if isNewEvVar evc then
                        continueWith $ CDictCan { cc_id = v_new, cc_flavor = fl_new
                                                , cc_tyargs = xis, cc_class = cls
                                                , cc_depth  = d }
                   else return Stop } }

emitSuperclasses :: Ct -> TcS StopOrContinue
emitSuperclasses ct@(CDictCan { cc_id = v_new, cc_depth = d, cc_flavor = fl
                              , cc_tyargs = xis_new, cc_class = cls })
            -- Add superclasses of this one here, See Note [Adding superclasses]. 
            -- But only if we are not simplifying the LHS of a rule. 
 = do { sctxt <- getTcSContext
      ; unless (simplEqsOnly sctxt) $ 
               newSCWorkFromFlavored d v_new fl cls xis_new
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
                      -> EvVar -> CtFlavor -> Class -> [Xi] -> TcS ()
-- Returns superclasses, see Note [Adding superclasses]
newSCWorkFromFlavored d ev flavor cls xis 
  | isDerived flavor 
  = return ()  -- Deriveds don't yield more superclasses because we will
               -- add them transitively in the case of wanteds. 

  | Just gk <- isGiven_maybe flavor 
  = case gk of 
      GivenOrig -> do { let sc_theta = immSuperClasses cls xis 
                      ; sc_vars <- mapM (newEvVar flavor) sc_theta
                      ; sc_cts <- zipWithM (\scv ev_trm -> 
                                                do { let sc_evvar = evc_the_evvar scv
                                                   ; _unused_fl <- setEvBind sc_evvar ev_trm flavor
                                                      -- unused because it's the same
                                                   ; return $ 
                                                     CNonCanonical { cc_id = sc_evvar
                                                                   , cc_flavor = flavor
                                                                   , cc_depth = d }}) 
                                           sc_vars [EvSuperClass ev n | n <- [0..]]
                        -- Emit now, canonicalize later in a lazier fashion
                      ; traceTcS "newSCWorkFromFlavored" $
                                 text "Emitting superclass work:" <+> ppr sc_cts
                      ; updWorkListTcS $ appendWorkListCt sc_cts }
      GivenSolved {} -> return ()
      -- Seems very dangerous to add the superclasses for dictionaries that may be 
      -- partially solved because we may end up with evidence loops.

  | isEmptyVarSet (tyVarsOfTypes xis)
  = return () -- Wanteds with no variables yield no deriveds.
              -- See Note [Improvement from Ground Wanteds]

  | otherwise -- Wanted case, just add those SC that can lead to improvement. 
  = do { let sc_rec_theta = transSuperClasses cls xis 
             impr_theta   = filter is_improvement_pty sc_rec_theta 
             Wanted wloc  = flavor
       ; sc_cts <- mapM (\pty -> do { scv <- newEvVar (Derived wloc) pty
                                    ; if isNewEvVar scv then 
                                          return [ CNonCanonical { cc_id = evc_the_evvar scv
                                                                 , cc_flavor = Derived wloc
                                                                 , cc_depth = d } ]  
                                      else return [] }
                        ) impr_theta
       ; let sc_cts_flat = concat sc_cts
       ; traceTcS "newSCWorkFromFlavored" (text "Emitting superclass work:" <+> ppr sc_cts_flat)
       ; updWorkListTcS $ appendWorkListCt sc_cts_flat }

is_improvement_pty :: PredType -> Bool 
-- Either it's an equality, or has some functional dependency
is_improvement_pty ty = go (classifyPredType ty)
  where
    go (EqPred {})         = True 
    go (ClassPred cls _tys) = not $ null fundeps
      where (_,fundeps) = classTvsFds cls
    go (IPPred {})         = False
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
         -> CtFlavor -> EvVar -> TcType -> TcS StopOrContinue
-- Precondition: ty not a tuple and no other evidence form
canIrred d fl v ty 
  = do { traceTcS "can_pred" (text "IrredPred = " <+> ppr ty) 
       ; (xi,co) <- flatten d fl ty -- co :: xi ~ ty
       ; let no_flattening = xi `eqType` ty 
                             -- In this particular case it is not safe to 
                             -- say 'isTcReflCo' because the new constraint may
                             -- be reducible!
       ; if no_flattening then
            continueWith $ CIrredEvCan { cc_id = v, cc_flavor = fl
                                       , cc_ty = xi, cc_depth  = d }
         else do
      {   -- Flattening consults and applies family equations from the
          -- inerts, so 'xi' may become reducible. So just recursively
          -- canonicalise the resulting evidence variable
        evc <- newEvVar fl xi
      ; let v' = evc_the_evvar evc
      ; fl' <- case fl of 
          Wanted  {} -> setEvBind v (EvCast v' co) fl
          Given   {} -> setEvBind v' (EvCast v (mkTcSymCo co)) fl
          Derived {} -> return fl
      
      ; if isNewEvVar evc then 
            canEvVar v' (classifyPredType (evVarPred v')) d fl'
        else
            return Stop }
      }

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
      xi has no type functions
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

-- Flatten a bunch of types all at once.
flattenMany :: SubGoalDepth -- Depth
            -> CtFlavor -> [Type] -> TcS ([Xi], [TcCoercion])
-- Coercions :: Xi ~ Type 
-- Returns True iff (no flattening happened)
flattenMany d ctxt tys 
  = -- pprTrace "flattenMany" empty $
    go tys 
  where go []       = return ([],[])
        go (ty:tys) = do { (xi,co)    <- flatten d ctxt ty
                         ; (xis,cos)  <- go tys
                         ; return (xi:xis,co:cos) }

-- Flatten a type to get rid of type function applications, returning
-- the new type-function-free type, and a collection of new equality
-- constraints.  See Note [Flattening] for more detail.
flatten :: SubGoalDepth -- Depth
        -> CtFlavor -> TcType -> TcS (Xi, TcCoercion)
-- Postcondition: Coercion :: Xi ~ TcType
flatten d ctxt ty 
  | Just ty' <- tcView ty
  = do { (xi, co) <- flatten d ctxt ty'
       ; return (xi,co) } 

flatten _ _ xi@(LitTy {}) = return (xi, mkTcReflCo xi)

flatten d ctxt (TyVarTy tv)
  = flattenTyVar d ctxt tv

flatten d ctxt (AppTy ty1 ty2)
  = do { (xi1,co1) <- flatten d ctxt ty1
       ; (xi2,co2) <- flatten d ctxt ty2
       ; return (mkAppTy xi1 xi2, mkTcAppCo co1 co2) }

flatten d ctxt (FunTy ty1 ty2)
  = do { (xi1,co1) <- flatten d ctxt ty1
       ; (xi2,co2) <- flatten d ctxt ty2
       ; return (mkFunTy xi1 xi2, mkTcFunCo co1 co2) }

flatten d fl (TyConApp tc tys)
  -- For a normal type constructor or data family application, we just
  -- recursively flatten the arguments.
  | not (isSynFamilyTyCon tc)
    = do { (xis,cos) <- flattenMany d fl tys
         ; return (mkTyConApp tc xis, mkTcTyConAppCo tc cos) }

  -- Otherwise, it's a type function application, and we have to
  -- flatten it away as well, and generate a new given equality constraint
  -- between the application and a newly generated flattening skolem variable.
  | otherwise
  = ASSERT( tyConArity tc <= length tys )	-- Type functions are saturated
      do { (xis, cos) <- flattenMany d fl tys
         ; let (xi_args, xi_rest)  = splitAt (tyConArity tc) xis
	       	 -- The type function might be *over* saturated
		 -- in which case the remaining arguments should
		 -- be dealt with by AppTys
               fam_ty = mkTyConApp tc xi_args
         ; (ret_co, rhs_xi, ct) <-
             do { is_cached <- getCachedFlatEq tc xi_args fl Any
                ; case is_cached of
                    Just (rhs_xi,ret_eq) -> 
                        do { traceTcS "is_cached!" $ ppr ret_eq
                           ; return (ret_eq, rhs_xi, []) }
                    Nothing
                        | isGivenOrSolved fl ->
                            do { rhs_xi_var <- newFlattenSkolemTy fam_ty
                               ; (fl',eqv) 
                                   <- newGivenEqVar fl fam_ty rhs_xi_var (mkTcReflCo fam_ty)
                               ; let ct  = CFunEqCan { cc_id     = eqv
                                                     , cc_flavor = fl' -- Given
                                                     , cc_fun    = tc 
                                                     , cc_tyargs = xi_args 
                                                     , cc_rhs    = rhs_xi_var 
                                                     , cc_depth  = d }
                                           -- Update the flat cache: just an optimisation!
                               ; updateFlatCache eqv fl' tc xi_args rhs_xi_var WhileFlattening
                               ; return (mkTcCoVarCo eqv, rhs_xi_var, [ct]) }
                        | otherwise ->
                    -- Derived or Wanted: make a new /unification/ flatten variable
                            do { rhs_xi_var <- newFlexiTcSTy (typeKind fam_ty)
                               ; let wanted_flavor = mkWantedFlavor fl
                               ; evc <- newEqVar wanted_flavor fam_ty rhs_xi_var
                               ; let eqv = evc_the_evvar evc -- Not going to be cached
                                     ct = CFunEqCan { cc_id = eqv
                                                    , cc_flavor = wanted_flavor
                                                    -- Always Wanted, not Derived
                                                    , cc_fun = tc
                                                    , cc_tyargs = xi_args
                                                    , cc_rhs    = rhs_xi_var 
                                                    , cc_depth  = d }
                                          -- Update the flat cache: just an optimisation!
                               ; updateFlatCache eqv fl tc xi_args rhs_xi_var WhileFlattening
                               ; return (mkTcCoVarCo eqv, rhs_xi_var, [ct]) } }

           -- Emit the flat constraints
         ; updWorkListTcS $ appendWorkListEqs ct

         ; let (cos_args, cos_rest) = splitAt (tyConArity tc) cos
         ; return ( mkAppTys rhs_xi xi_rest    -- NB mkAppTys: rhs_xi might not be a type variable
	   	    	     	    	       --    cf Trac #5655
                  , mkTcAppCos (mkTcSymCo ret_co `mkTcTransCo` mkTcTyConAppCo tc cos_args)
                               cos_rest
                  ) }


flatten d ctxt ty@(ForAllTy {})
-- We allow for-alls when, but only when, no type function
-- applications inside the forall involve the bound type variables.
  = do { let (tvs, rho) = splitForAllTys ty
       ; when (under_families tvs rho) $ wrapErrTcS $ flattenForAllErrorTcS ctxt ty
       ; (rho', co) <- flatten d ctxt rho
       ; return (mkForAllTys tvs rho', foldr mkTcForAllCo co tvs) }

  where under_families tvs rho 
            = go (mkVarSet tvs) rho 
            where go _bound (TyVarTy _tv) = False
                  go _ (LitTy {}) = False
                  go bound (TyConApp tc tys)
                      | isSynFamilyTyCon tc
                      , (args,rest) <- splitAt (tyConArity tc) tys
                      = (tyVarsOfTypes args `intersectsVarSet` bound) || any (go bound) rest
                      | otherwise = any (go bound) tys
                  go bound (FunTy arg res)  = go bound arg || go bound res
                  go bound (AppTy fun arg)  = go bound fun || go bound arg
                  go bound (ForAllTy tv ty) = go (bound `extendVarSet` tv) ty
\end{code}

\begin{code}
flattenTyVar :: SubGoalDepth -> CtFlavor -> TcTyVar -> TcS (Xi, TcCoercion)
-- "Flattening" a type variable means to apply the substitution to it
flattenTyVar d ctxt tv
  = do { ieqs <- getInertEqs
       ; let mco = tv_eq_subst (fst ieqs) tv  -- co : v ~ ty
       ; case mco of -- Done, but make sure the kind is zonked
           Nothing -> 
               do { let knd = tyVarKind tv
                  ; (new_knd,_kind_co) <- flatten d ctxt knd
                  ; let ty = mkTyVarTy (setVarType tv new_knd)
                  ; return (ty, mkTcReflCo ty) }
           -- NB recursive call. 
           -- Why? See Note [Non-idempotent inert substitution]
           -- Actually, I think applying the substition just twice will suffice
           Just (co,ty) -> 
               do { (ty_final,co') <- flatten d ctxt ty
                  ; return (ty_final, co' `mkTcTransCo` mkTcSymCo co) } }  
  where tv_eq_subst subst tv
          | Just (ct,co) <- lookupVarEnv subst tv
          , cc_flavor ct `canRewrite` ctxt
          = Just (co,cc_rhs ct)
          | otherwise = Nothing
\end{code}

Note [Non-idempotent inert substitution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The inert substitution is not idempotent in the broad sense. It is only idempotent in 
that it cannot rewrite the RHS of other inert equalities any further. An example of such 
an inert substitution is:

 [Ś] g1 : ta8 ~ ta4
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

This insufficient rewriting was the reason for #5668.

\begin{code}
getCachedFlatEq :: TyCon -> [Xi] -> CtFlavor 
                -> FlatEqOrigin
                -> TcS (Maybe (Xi, TcCoercion))
-- Returns a coercion between (TyConApp tc xi_args ~ xi) if such an inert item exists
-- But also applies the substitution to the item via calling flatten recursively
getCachedFlatEq tc xi_args fl feq_origin
  = do { let pty = mkTyConApp tc xi_args
       ; traceTcS "getCachedFlatEq" $ ppr (mkTyConApp tc xi_args)
       ; flat_cache <- getTcSEvVarFlatCache
       ; inerts <- getTcSInerts
       ; case lookupFunEq pty fl (inert_funeqs inerts) of
           Nothing 
               -> lookup_in_flat_cache pty flat_cache
           res -> return res }
  where lookup_in_flat_cache pty flat_cache 
          = case lookupTM pty flat_cache of
              Just (co',(xi',fl',when_generated)) -- ev' :: (TyConApp tc xi_args) ~ xi'
               | fl' `canRewrite` fl
               , feq_origin `origin_matches` when_generated
               -> do { traceTcS "getCachedFlatEq" $ text "success!"
                     ; (xi'',co) <- flatten 0 fl' xi' -- co :: xi'' ~ xi'
                                    -- The only purpose of this flattening is to apply the
                                    -- inert substitution (since everything in the flat cache
                                    -- by construction will have a family-free RHS.
                     ; return $ Just (xi'', co' `mkTcTransCo` (mkTcSymCo co)) }
              _ -> do { traceTcS "getCachedFlatEq" $ text "failure!" <+> pprEvVarCache flat_cache
                      ; return Nothing }

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
                   -> [CtFlavor] -> [EvVarCreated] -> [Type] -> [Type]
                   -> TcS StopOrContinue
canEqEvVarsCreated d fls evcs tys1 tys2
  = ASSERT( equalLength fls evcs && equalLength fls tys1 && equalLength fls tys2 )
    case filter is_new (zip4 fls evcs tys1 tys2) of
      []             -> return Stop
      (quad : quads) -> do { mapM_ (addToWork . do_quad) quads
                           ; do_quad quad }
      -- For the new EvVars, add all but one to the work list
      -- and return the first (if any) for futher processing
 where
   is_new (_, evc, _, _) = isNewEvVar evc
   do_quad (fl, evc, ty1, ty2) = canEqNC d fl (evc_the_evvar evc) ty1 ty2
      -- Note the "NC": these are fresh equalities so we must be
      -- careful to add their kind constraints

-------------------------
canEqNC, canEq 
  :: SubGoalDepth 
  -> CtFlavor -> EqVar 
  -> Type -> Type -> TcS StopOrContinue

canEqNC d fl ev ty1 ty2
  = canEq d fl ev ty1 ty2
    `andWhenContinue` emitKindConstraint

canEq _d fl eqv ty1 ty2
  | eqType ty1 ty2	-- Dealing with equality here avoids
    	     	 	-- later spurious occurs checks for a~a
  = do { when (isWanted fl) $ 
              do { _ <- setEqBind eqv (mkTcReflCo ty1) fl; return () }
       ; return Stop }

-- If one side is a variable, orient and flatten,
-- WITHOUT expanding type synonyms, so that we tend to 
-- substitute a ~ Age rather than a ~ Int when @type Age = Int@
canEq d fl eqv ty1@(TyVarTy {}) ty2 
  = canEqLeaf d fl eqv ty1 ty2
canEq d fl eqv ty1 ty2@(TyVarTy {})
  = canEqLeaf d fl eqv ty1 ty2

-- See Note [Naked given applications]
canEq d fl eqv ty1 ty2
  | Just ty1' <- tcView ty1 = canEq d fl eqv ty1' ty2
  | Just ty2' <- tcView ty2 = canEq d fl eqv ty1  ty2'

canEq d fl eqv ty1@(TyConApp fn tys) ty2 
  | isSynFamilyTyCon fn, length tys == tyConArity fn
  = canEqLeaf d fl eqv ty1 ty2
canEq d fl eqv ty1 ty2@(TyConApp fn tys)
  | isSynFamilyTyCon fn, length tys == tyConArity fn
  = canEqLeaf d fl eqv ty1 ty2

canEq d fl eqv ty1 ty2
  | Just (tc1,tys1) <- tcSplitTyConApp_maybe ty1
  , Just (tc2,tys2) <- tcSplitTyConApp_maybe ty2
  , isDecomposableTyCon tc1 && isDecomposableTyCon tc2
  = -- Generate equalities for each of the corresponding arguments
    if (tc1 /= tc2 || length tys1 /= length tys2)
    -- Fail straight away for better error messages
    then canEqFailure d fl eqv
    else do
       { argeqvs <- zipWithM (newEqVar fl) tys1 tys2

       ; fls <- case fl of 
           Wanted {} -> 
             do { _ <- setEqBind eqv
                         (mkTcTyConAppCo tc1 (map (mkTcCoVarCo . evc_the_evvar) argeqvs)) fl
                ; return (map (\_ -> fl) argeqvs) }
           Given {} ->
             let do_one argeqv n = setEqBind (evc_the_evvar argeqv) 
                                             (mkTcNthCo n (mkTcCoVarCo eqv)) fl
             in zipWithM do_one argeqvs [0..]
           Derived {} -> return (map (\_ -> fl) argeqvs)

       ; canEqEvVarsCreated d fls argeqvs tys1 tys2 }

-- See Note [Equality between type applications]
--     Note [Care with type applications] in TcUnify
canEq d fl eqv ty1 ty2    -- e.g.  F a b ~ Maybe c
                          -- where F has arity 1
  | Just (s1,t1) <- tcSplitAppTy_maybe ty1
  , Just (s2,t2) <- tcSplitAppTy_maybe ty2
  = canEqAppTy d fl eqv s1 t1 s2 t2

canEq d fl eqv s1@(ForAllTy {}) s2@(ForAllTy {})
 | tcIsForAllTy s1, tcIsForAllTy s2, 
   Wanted {} <- fl 
 = canEqFailure d fl eqv
 | otherwise
 = do { traceTcS "Ommitting decomposition of given polytype equality" (pprEq s1 s2)
      ; return Stop }

canEq d fl eqv _ _                               = canEqFailure d fl eqv

------------------------
-- Type application
canEqAppTy :: SubGoalDepth 
           -> CtFlavor -> EqVar -> Type -> Type -> Type -> Type
           -> TcS StopOrContinue
canEqAppTy d fl eqv s1 t1 s2 t2
  = ASSERT( not (isKind t1) && not (isKind t2) )
    if isGivenOrSolved fl then 
        do { traceTcS "canEq (app case)" $
                text "Ommitting decomposition of given equality between: " 
                    <+> ppr (AppTy s1 t1) <+> text "and" <+> ppr (AppTy s2 t2)
                   -- We cannot decompose given applications
                   -- because we no longer have 'left' and 'right'
           ; return Stop }
    else
        do { evc1 <- newEqVar fl s1 s2
           ; evc2 <- newEqVar fl t1 t2
           ; let eqv1 = evc_the_evvar evc1
                 eqv2 = evc_the_evvar evc2
 
           ; when (isWanted fl) $
                  do { _ <- setEqBind eqv (mkTcAppCo (mkTcCoVarCo eqv1) (mkTcCoVarCo eqv2)) fl
                     ; return () }
           
           ; canEqEvVarsCreated d [fl,fl] [evc1,evc2] [s1,t1] [s2,t2] }

------------------------
canEqFailure :: SubGoalDepth 
             -> CtFlavor -> EvVar -> TcS StopOrContinue
canEqFailure d fl eqv 
  = do { when (isWanted fl) (delCachedEvVar eqv fl) 
          -- See Note [Combining insoluble constraints]
       ; emitFrozenError fl eqv d
       ; return Stop }

------------------------
emitKindConstraint :: Ct -> TcS StopOrContinue
emitKindConstraint ct
  = case ct of 
      CTyEqCan { cc_id = ev, cc_depth = d
               , cc_flavor = fl, cc_tyvar = tv
               , cc_rhs = ty }
          -> emit_kind_constraint ev d fl (mkTyVarTy tv) ty

      CFunEqCan { cc_id = ev, cc_depth = d
                , cc_flavor = fl
                , cc_fun = fn, cc_tyargs = xis1
                , cc_rhs = xi2 }
          -> emit_kind_constraint ev d fl (mkTyConApp fn xis1) xi2

      _   -> continueWith ct 
  where
    emit_kind_constraint eqv d fl ty1 ty2 
       | compatKind k1 k2    -- True when ty1,ty2 are themselves kinds,
       = continueWith ct     -- because then k1, k2 are BOX
       
       | otherwise
       = ASSERT( isKind k1 && isKind k2 )
         do { keqv <- forceNewEvVar kind_co_fl (mkNakedEqPred superKind k1 k2)
            ; eqv' <- forceNewEvVar fl (mkTcEqPred ty1 ty2)
            ; _fl <- case fl of
               Wanted {}-> setEvBind eqv
                            (mkEvKindCast eqv' (mkTcCoVarCo keqv)) fl
               Given {} -> setEvBind eqv'
                            (mkEvKindCast eqv (mkTcCoVarCo keqv)) fl
               Derived {} -> return fl

            ; traceTcS "Emitting kind constraint" $
                  vcat [ ppr keqv <+> dcolon <+> ppr (mkEqPred k1 k2)
                       , ppr eqv, ppr eqv' ] 
            ; addToWork (canEq d kind_co_fl keqv k1 k2) -- Emit kind equality
            ; continueWith (ct { cc_id = eqv' }) }
       where 
         k1 = typeKind ty1
         k2 = typeKind ty2
         ctxt = mkKindErrorCtxtTcS ty1 k1 ty2 k2
         -- Always create a Wanted kind equality even if 
         -- you are decomposing a given constraint.
         -- NB: DV finds this reasonable for now. Maybe we 
         --  have to revisit.
         kind_co_fl
           | Given (CtLoc _sk_info src_span err_ctxt) _ <- fl
           = let orig = TypeEqOrigin (UnifyOrigin ty1 ty2)
                 ctloc = pushErrCtxtSameOrigin ctxt $
                         CtLoc orig src_span err_ctxt
             in Wanted ctloc
           | Wanted ctloc <- fl
           = Wanted (pushErrCtxtSameOrigin ctxt ctloc)
           | Derived ctloc <- fl
           = Derived (pushErrCtxtSameOrigin ctxt ctloc)
           | otherwise 
           = panic "do_emit_kind_constraint: non-CtLoc inside!"
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

{- Useless these days! 
unClassify :: TypeClassifier -> TcType
unClassify (VarCls tv)      = TyVarTy tv
unClassify (FskCls tv) = TyVarTy tv 
unClassify (FunCls fn tys)  = TyConApp fn tys
unClassify (OtherCls ty)    = ty
-} 

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
reOrient :: CtFlavor -> TypeClassifier -> TypeClassifier -> Bool	
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
          -> CtFlavor -> EqVar 
          -> Type -> Type 
          -> TcS StopOrContinue
-- Canonicalizing "leaf" equality constraints which cannot be
-- decomposed further (ie one of the types is a variable or
-- saturated type function application).  

-- Preconditions: 
--    * one of the two arguments is variable or family applications
--    * the two types are not equal (looking through synonyms)
canEqLeaf d fl eqv s1 s2 
  | cls1 `re_orient` cls2
  = do { traceTcS "canEqLeaf (reorienting)" $ ppr eqv <+> dcolon <+> pprEq s1 s2
       ; delCachedEvVar eqv fl
       ; evc <- newEqVar fl s2 s1
       ; let eqv' = evc_the_evvar evc
       ; fl' <- case fl of 
           Wanted {}  -> setEqBind eqv (mkTcSymCo (mkTcCoVarCo eqv')) fl 
           Given {}   -> setEqBind eqv' (mkTcSymCo (mkTcCoVarCo eqv)) fl 
           Derived {} -> return fl 
       ; if isNewEvVar evc then 
             do { canEqLeafOriented d fl' eqv' s2 s1 }
         else return Stop 
       }
  | otherwise
  = do { traceTcS "canEqLeaf" $ ppr (mkEqPred s1 s2)
       ; canEqLeafOriented d fl eqv s1 s2 }
  where
    re_orient = reOrient fl 
    cls1 = classify s1
    cls2 = classify s2

canEqLeafOriented :: SubGoalDepth -- Depth
                  -> CtFlavor -> EqVar 
                  -> TcType -> TcType -> TcS StopOrContinue
-- By now s1 will either be a variable or a type family application
canEqLeafOriented d fl eqv s1 s2
  = can_eq_split_lhs d fl eqv s1 s2
  where can_eq_split_lhs d fl eqv s1 s2
          | Just (fn,tys1) <- splitTyConApp_maybe s1
          = canEqLeafFunEqLeftRec d fl eqv (fn,tys1) s2
          | Just tv <- getTyVar_maybe s1
          = canEqLeafTyVarLeftRec d fl eqv tv s2
          | otherwise
          = pprPanic "canEqLeafOriented" $
            text "Non-variable or non-family equality LHS" <+> 
                 ppr eqv <+> dcolon <+> ppr (evVarPred eqv)

canEqLeafFunEqLeftRec :: SubGoalDepth
                      -> CtFlavor 
                      -> EqVar 
                      -> (TyCon,[TcType]) -> TcType -> TcS StopOrContinue
canEqLeafFunEqLeftRec d fl eqv (fn,tys1) ty2  -- eqv :: F tys1 ~ ty2
  = do { traceTcS "canEqLeafFunEqLeftRec" $ pprEq (mkTyConApp fn tys1) ty2
       ; (xis1,cos1) <- 
           {-# SCC "flattenMany" #-}
           flattenMany d fl tys1 -- Flatten type function arguments
                                 -- cos1 :: xis1 ~ tys1

--       ; inerts <- getTcSInerts
--        ; let fam_eqs   = inert_funeqs inerts

       ; let flat_ty = mkTyConApp fn xis1

       ; is_cached <- getCachedFlatEq fn xis1 fl WhenSolved
                      -- Lookup if we have solved this goal already
{-
       ; let is_cached = {-# SCC "lookupFunEq" #-} 
                         lookupFunEq flat_ty fl fam_eqs
-}
       ; let no_flattening = all isTcReflCo cos1
                      
       ; if no_flattening && isNothing is_cached then 
             canEqLeafFunEqLeft d fl eqv (fn,xis1) ty2
         else do
       { let (final_co, final_ty)
                 | no_flattening        -- Just in inerts
                 , Just (rhs_ty, ret_eq) <- is_cached
                 = (mkTcSymCo ret_eq, rhs_ty)
                 | Nothing <- is_cached -- Just flattening
                 = (mkTcTyConAppCo fn cos1, flat_ty)
                 | Just (rhs_ty, ret_eq) <- is_cached  -- Both
                 = (mkTcSymCo ret_eq `mkTcTransCo` mkTcTyConAppCo fn cos1, rhs_ty)
                 | otherwise = panic "No flattening and not cached!"
       ; delCachedEvVar eqv fl
       ; evc <- newEqVar fl final_ty ty2
       ; let new_eqv = evc_the_evvar evc
       ; fl' <- case fl of
           Wanted {}  -> setEqBind eqv 
                           (mkTcSymCo final_co `mkTcTransCo` (mkTcCoVarCo new_eqv)) fl
           Given {}   -> setEqBind new_eqv (final_co `mkTcTransCo` (mkTcCoVarCo eqv)) fl
           Derived {} -> return fl
       ; if isNewEvVar evc then
             if isNothing is_cached then
                 {-# SCC "canEqLeafFunEqLeft" #-}
                 canEqLeafFunEqLeft d fl' new_eqv (fn,xis1) ty2
             else
                 canEq (d+1) fl' new_eqv final_ty ty2
         else return Stop
       }
       }

lookupFunEq :: PredType -> CtFlavor -> TypeMap Ct -> Maybe (TcType, TcCoercion)
lookupFunEq pty fl fam_eqs = lookup_funeq pty fam_eqs
  where lookup_funeq pty fam_eqs
          | Just ct <- lookupTM pty fam_eqs
          , cc_flavor ct `canRewrite` fl 
          = Just (cc_rhs ct, mkTcCoVarCo (cc_id ct))
          | otherwise 
          = Nothing

canEqLeafFunEqLeft :: SubGoalDepth -- Depth
                   -> CtFlavor -> EqVar -> (TyCon,[Xi]) 
                   -> TcType -> TcS StopOrContinue
-- Precondition: No more flattening is needed for the LHS
canEqLeafFunEqLeft d fl eqv (fn,xis1) s2
 = {-# SCC "canEqLeafFunEqLeft" #-}
   do { traceTcS "canEqLeafFunEqLeft" $ pprEq (mkTyConApp fn xis1) s2
      ; (xi2,co2) <- 
          {-# SCC "flatten" #-} 
          flatten d fl s2 -- co2 :: xi2 ~ s2
      ; let no_flattening_happened = isTcReflCo co2
      ; if no_flattening_happened then 
            continueWith $ CFunEqCan { cc_id     = eqv
                                     , cc_flavor = fl
                                     , cc_fun    = fn
                                     , cc_tyargs = xis1 
                                     , cc_rhs    = xi2 
                                     , cc_depth  = d }
        else do { delCachedEvVar eqv fl
                ; evc <- 
                    {-# SCC "newEqVar" #-}
                    newEqVar fl (mkTyConApp fn xis1) xi2
                ; let new_eqv = evc_the_evvar evc -- F xis1 ~ xi2 
                      new_cv  = mkTcCoVarCo new_eqv
                      cv      = mkTcCoVarCo eqv    -- F xis1 ~ s2
                ; fl' <- case fl of
                    Wanted {} -> setEqBind eqv (new_cv `mkTcTransCo` co2) fl 
                    Given {}  -> setEqBind new_eqv (cv `mkTcTransCo` mkTcSymCo co2) fl
                    Derived {} -> return fl
                ; if isNewEvVar evc then 
                      do { continueWith $
                           CFunEqCan { cc_id = new_eqv
                                     , cc_flavor = fl'
                                     , cc_fun    = fn
                                     , cc_tyargs = xis1 
                                     , cc_rhs    = xi2 
                                     , cc_depth  = d } }
                  else return Stop }  }


canEqLeafTyVarLeftRec :: SubGoalDepth
                      -> CtFlavor -> EqVar
                      -> TcTyVar -> TcType -> TcS StopOrContinue
canEqLeafTyVarLeftRec d fl eqv tv s2              -- eqv :: tv ~ s2
  = do {  traceTcS "canEqLeafTyVarLeftRec" $ pprEq (mkTyVarTy tv) s2
       ; (xi1,co1) <- flattenTyVar d fl tv -- co1 :: xi1 ~ tv
       ; case isTcReflCo co1 of 
             True -> case getTyVar_maybe xi1 of
                       Just tv' -> canEqLeafTyVarLeft d fl eqv tv' s2
                       Nothing  -> canEq d fl eqv xi1 s2

             False -> -- If not refl co, must rewrite and go on
               do { traceTcS "celtlr: rewrite" (ppr xi1 $$ ppr co1)
                  ; delCachedEvVar eqv fl
                  ; evc <- newEqVar fl xi1 s2  -- new_ev :: xi1 ~ s2
                  ; let new_ev = evc_the_evvar evc
                  ; fl' <- case fl of 
                    Wanted  {} -> setEqBind eqv 
                                    (mkTcSymCo co1 `mkTcTransCo` mkTcCoVarCo new_ev) fl
                    Given   {} -> setEqBind new_ev
                                    (co1 `mkTcTransCo` mkTcCoVarCo eqv) fl
                    Derived {} -> return fl
                  ; if isNewEvVar evc then
                      do { canEq d fl' new_ev xi1 s2 }
                    else return Stop
                  }
       }

canEqLeafTyVarLeft :: SubGoalDepth -- Depth
                   -> CtFlavor -> EqVar
                   -> TcTyVar -> TcType -> TcS StopOrContinue
-- Precondition LHS is fully rewritten from inerts (but not RHS)
canEqLeafTyVarLeft d fl eqv tv s2       -- eqv : tv ~ s2
  = do { traceTcS "canEqLeafTyVarLeft" (pprEq (mkTyVarTy tv) s2)
       ; (xi2, co) <- flatten d fl s2   -- Flatten RHS   co : xi2 ~ s2
                                               
       ; let no_flattening_happened = isTcReflCo co
             
       ; traceTcS "canEqLeafTyVarLeft" (nest 2 (vcat [ text "tv  =" <+> ppr tv
                                                     , text "s2  =" <+> ppr s2
                                                     , text "xi2 =" <+> ppr xi2]))

                      -- Flattening the RHS may reveal an identity coercion, which should
                      -- not be reported as occurs check error! 
       ; let is_same_tv
               | Just tv' <- getTyVar_maybe xi2, tv' == tv
               = True
               | otherwise = False
       ; if is_same_tv then
             do { delCachedEvVar eqv fl
                ; when (isWanted fl) $ 
                       do { _ <- setEqBind eqv co fl; return () }
                ; return Stop }
         else
    do { -- Do an occurs check, and return a possibly
         -- unfolded version of the RHS, if we had to 
         -- unfold any type synonyms to get rid of tv.
         occ_check_result <- canOccursCheck fl tv xi2

       ; let xi2'
              | Just xi2_unfolded <- occ_check_result
              = xi2_unfolded
              | otherwise = xi2


       ; if no_flattening_happened then
             if isNothing occ_check_result then 
                 canEqFailure d fl (setVarType eqv $ 
                                     mkTcEqPred (mkTyVarTy tv) xi2')
             else 
                 continueWith $ CTyEqCan { cc_id     = eqv
                                         , cc_flavor = fl
                                         , cc_tyvar  = tv
                                         , cc_rhs    = xi2'
                                         , cc_depth  = d }
         else -- Flattening happened, in any case we have to create new variable 
              -- even if we report an occurs check error
             do { delCachedEvVar eqv fl
                ; evc <- newEqVar fl (mkTyVarTy tv) xi2' 
                ; let eqv' = evc_the_evvar evc -- eqv' : tv ~ xi2'
                      cv   = mkTcCoVarCo eqv    -- cv : tv ~ s2
                      cv'  = mkTcCoVarCo eqv'   -- cv': tv ~ xi2'
                 ; fl' <- case fl of 
                     Wanted {}  -> setEqBind eqv (cv' `mkTcTransCo` co) fl         -- tv ~ xi2' ~ s2
                     Given {}   -> setEqBind eqv' (cv `mkTcTransCo` mkTcSymCo co) fl -- tv ~ s2 ~ xi2'
                     Derived {} -> return fl

                 ; if isNewEvVar evc then 
                       if isNothing occ_check_result then 
                           canEqFailure d fl eqv'
                       else continueWith CTyEqCan { cc_id     = eqv'
                                                  , cc_flavor = fl'
                                                  , cc_tyvar  = tv
                                                  , cc_rhs    = xi2' 
                                                  , cc_depth  = d }
                   else 
                       return Stop } } }


-- See Note [Type synonyms and canonicalization].
-- Check whether the given variable occurs in the given type.  We may
-- have needed to do some type synonym unfolding in order to get rid
-- of the variable, so we also return the unfolded version of the
-- type, which is guaranteed to be syntactically free of the given
-- type variable.  If the type is already syntactically free of the
-- variable, then the same type is returned.
--
-- Precondition: the two types are not equal (looking though synonyms)
canOccursCheck :: CtFlavor -> TcTyVar -> Xi -> TcS (Maybe Xi)
canOccursCheck _gw tv xi = return (expandAway tv xi)
\end{code}

@expandAway tv xi@ expands synonyms in xi just enough to get rid of
occurrences of tv, if that is possible; otherwise, it returns Nothing.
For example, suppose we have
  type F a b = [a]
Then
  expandAway b (F Int b) = Just [Int]
but
  expandAway a (F a Int) = Nothing

We don't promise to do the absolute minimum amount of expanding
necessary, but we try not to do expansions we don't need to.  We
prefer doing inner expansions first.  For example,
  type F a b = (a, Int, a, [a])
  type G b   = Char
We have
  expandAway b (F (G b)) = F Char
even though we could also expand F to get rid of b.

\begin{code}
expandAway :: TcTyVar -> Xi -> Maybe Xi
expandAway tv t@(TyVarTy tv')
  | tv == tv' = Nothing
  | otherwise = Just t
expandAway tv xi
  | not (tv `elemVarSet` tyVarsOfType xi) = Just xi
expandAway tv (AppTy ty1 ty2) 
  = do { ty1' <- expandAway tv ty1
       ; ty2' <- expandAway tv ty2 
       ; return (mkAppTy ty1' ty2') }
-- mkAppTy <$> expandAway tv ty1 <*> expandAway tv ty2
expandAway tv (FunTy ty1 ty2)
  = do { ty1' <- expandAway tv ty1 
       ; ty2' <- expandAway tv ty2 
       ; return (mkFunTy ty1' ty2') } 
-- mkFunTy <$> expandAway tv ty1 <*> expandAway tv ty2
expandAway tv ty@(ForAllTy {}) 
  = let (tvs,rho) = splitForAllTys ty
        tvs_knds  = map tyVarKind tvs 
    in if tv `elemVarSet` tyVarsOfTypes tvs_knds then
       -- Can't expand away the kinds unless we create 
       -- fresh variables which we don't want to do at this point.
           Nothing 
       else do { rho' <- expandAway tv rho
               ; return (mkForAllTys tvs rho') }
-- For a type constructor application, first try expanding away the
-- offending variable from the arguments.  If that doesn't work, next
-- see if the type constructor is a type synonym, and if so, expand
-- it and try again.
expandAway tv ty@(TyConApp tc tys)
  = (mkTyConApp tc <$> mapM (expandAway tv) tys) <|> (tcView ty >>= expandAway tv)

expandAway _ xi@(LitTy {}) = return xi

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

