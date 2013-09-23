\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module TcCanonical(
    canonicalize, emitWorkNC,
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
import OccName( OccName )
import Outputable
import Control.Monad    ( when )
import TysWiredIn ( eqTyCon )
import DynFlags( DynFlags )
import VarSet
import TcSMonad
import FastString

import Util
import Maybes( catMaybes )
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
canonicalize ct@(CNonCanonical { cc_ev = ev, cc_loc  = d })
  = do { traceTcS "canonicalize (non-canonical)" (ppr ct)
       ; {-# SCC "canEvVar" #-}
         canEvNC d ev }

canonicalize (CDictCan { cc_loc  = d
                       , cc_ev = ev
                       , cc_class  = cls
                       , cc_tyargs = xis })
  = {-# SCC "canClass" #-}
    canClass d ev cls xis -- Do not add any superclasses
canonicalize (CTyEqCan { cc_loc  = d
                       , cc_ev = ev
                       , cc_tyvar  = tv
                       , cc_rhs    = xi })
  = {-# SCC "canEqLeafTyVarEq" #-}
    canEqLeafTyVar d ev tv xi

canonicalize (CFunEqCan { cc_loc = d
                        , cc_ev = ev
                        , cc_fun    = fn
                        , cc_tyargs = xis1
                        , cc_rhs    = xi2 })
  = {-# SCC "canEqLeafFunEq" #-}
    canEqLeafFun d ev fn xis1 xi2

canonicalize (CIrredEvCan { cc_ev = ev
                          , cc_loc = d })
  = canIrred d ev
canonicalize (CHoleCan { cc_ev = ev, cc_loc = d, cc_occ = occ })
  = canHole d ev occ

canEvNC :: CtLoc -> CtEvidence -> TcS StopOrContinue
-- Called only for non-canonical EvVars 
canEvNC d ev 
  = case classifyPredType (ctEvPred ev) of
      ClassPred cls tys -> traceTcS "canEvNC:cls" (ppr cls <+> ppr tys) >> canClassNC d ev cls tys 
      EqPred ty1 ty2    -> traceTcS "canEvNC:eq" (ppr ty1 $$ ppr ty2)   >> canEqNC    d ev ty1 ty2 
      TuplePred tys     -> traceTcS "canEvNC:tup" (ppr tys)             >> canTuple   d ev tys
      IrredPred {}      -> traceTcS "canEvNC:irred" (ppr (ctEvPred ev)) >> canIrred   d ev 
\end{code}


%************************************************************************
%*                                                                      *
%*                      Tuple Canonicalization
%*                                                                      *
%************************************************************************

\begin{code}
canTuple :: CtLoc -> CtEvidence -> [PredType] -> TcS StopOrContinue
canTuple d ev tys
  = do { traceTcS "can_pred" (text "TuplePred!")
       ; let xcomp = EvTupleMk
             xdecomp x = zipWith (\_ i -> EvTupleSel x i) tys [0..]             
       ; ctevs <- xCtFlavor ev tys (XEvTerm xcomp xdecomp)
       ; canEvVarsCreated d ctevs }
\end{code}

%************************************************************************
%*                                                                      *
%*                      Class Canonicalization
%*                                                                      *
%************************************************************************

\begin{code}
canClass, canClassNC 
   :: CtLoc
   -> CtEvidence  
   -> Class -> [Type] -> TcS StopOrContinue
-- Precondition: EvVar is class evidence 

-- The canClassNC version is used on non-canonical constraints
-- and adds superclasses.  The plain canClass version is used
-- for already-canonical class constraints (but which might have
-- been subsituted or somthing), and hence do not need superclasses

canClassNC d ev cls tys 
  = canClass d ev cls tys 
    `andWhenContinue` emitSuperclasses

canClass d ev cls tys
  = do { (xis, cos) <- flattenMany d FMFullFlatten (ctEvFlavour ev) tys
       ; let co = mkTcTyConAppCo (classTyCon cls) cos 
             xi = mkClassPred cls xis
       ; mb <- rewriteCtFlavor ev xi co
       ; traceTcS "canClass" (vcat [ ppr ev <+> ppr cls <+> ppr tys 
                                   , ppr xi, ppr mb ])
       ; case mb of
           Nothing -> return Stop
           Just new_ev -> continueWith $ 
                          CDictCan { cc_ev = new_ev, cc_loc = d
                                   , cc_tyargs = xis, cc_class = cls } }

emitSuperclasses :: Ct -> TcS StopOrContinue
emitSuperclasses ct@(CDictCan { cc_loc = d, cc_ev = ev
                              , cc_tyargs = xis_new, cc_class = cls })
            -- Add superclasses of this one here, See Note [Adding superclasses]. 
            -- But only if we are not simplifying the LHS of a rule. 
 = do { newSCWorkFromFlavored d ev cls xis_new
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
newSCWorkFromFlavored :: CtLoc -- Depth
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
       ; emitWorkNC d ctevs }

  | isEmptyVarSet (tyVarsOfTypes xis)
  = return () -- Wanteds with no variables yield no deriveds.
              -- See Note [Improvement from Ground Wanteds]

  | otherwise -- Wanted case, just add those SC that can lead to improvement. 
  = do { let sc_rec_theta = transSuperClasses cls xis 
             impr_theta   = filter is_improvement_pty sc_rec_theta
       ; traceTcS "newSCWork/Derived" $ text "impr_theta =" <+> ppr impr_theta
       ; mb_der_evs <- mapM newDerived impr_theta
       ; emitWorkNC d (catMaybes mb_der_evs) }

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
canIrred :: CtLoc -> CtEvidence -> TcS StopOrContinue
-- Precondition: ty not a tuple and no other evidence form
canIrred d ev
  = do { let ty = ctEvPred ev
       ; traceTcS "can_pred" (text "IrredPred = " <+> ppr ty) 
       ; (xi,co) <- flatten d FMFullFlatten (ctEvFlavour ev) ty -- co :: xi ~ ty
       ; let no_flattening = xi `eqType` ty 
             -- We can't use isTcReflCo, because even if the coercion is
             -- Refl, the output type might have had a substitution 
             -- applied to it.  For example  'a' might now be 'C b'

       ; if no_flattening then
           continueWith $
           CIrredEvCan { cc_ev = ev, cc_loc = d }
         else do
       { mb <- rewriteCtFlavor ev xi co 
       ; case mb of
             Just new_ev -> canEvNC d new_ev  -- Re-classify and try again
             Nothing     -> return Stop } }   -- Found a cached copy

canHole :: CtLoc -> CtEvidence -> OccName -> TcS StopOrContinue
canHole d ev occ
  = do { let ty = ctEvPred ev
       ; (xi,co) <- flatten d FMFullFlatten (ctEvFlavour ev) ty -- co :: xi ~ ty
       ; mb <- rewriteCtFlavor ev xi co 
       ; case mb of
             Just new_ev -> emitInsoluble (CHoleCan { cc_ev = new_ev, cc_loc = d, cc_occ = occ })
             Nothing     -> return ()   -- Found a cached copy; won't happen
       ; return Stop } 
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

data FlattenMode = FMSubstOnly | FMFullFlatten

-- Flatten a bunch of types all at once.
flattenMany :: CtLoc -> FlattenMode
            -> CtFlavour -> [Type] -> TcS ([Xi], [TcCoercion])
-- Coercions :: Xi ~ Type 
-- Returns True iff (no flattening happened)
-- NB: The EvVar inside the 'ctxt :: CtEvidence' is unused, 
--     we merely want (a) Given/Solved/Derived/Wanted info
--                    (b) the GivenLoc/WantedLoc for when we create new evidence
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
flatten :: CtLoc -> FlattenMode 
        -> CtFlavour -> TcType -> TcS (Xi, TcCoercion)
-- Postcondition: Coercion :: Xi ~ TcType
flatten loc f ctxt ty 
  | Just ty' <- tcView ty
  = do { (xi, co) <- flatten loc f ctxt ty'
       ; if eqType xi ty then return (ty,co) else return (xi,co) } 
       -- Small tweak for better error messages 

flatten _ _ _ xi@(LitTy {}) = return (xi, mkTcReflCo xi)

flatten loc f ctxt (TyVarTy tv)
  = flattenTyVar loc f ctxt tv

flatten loc f ctxt (AppTy ty1 ty2)
  = do { (xi1,co1) <- flatten loc f ctxt ty1
       ; (xi2,co2) <- flatten loc f ctxt ty2
       ; return (mkAppTy xi1 xi2, mkTcAppCo co1 co2) }

flatten loc f ctxt (FunTy ty1 ty2)
  = do { (xi1,co1) <- flatten loc f ctxt ty1
       ; (xi2,co2) <- flatten loc f ctxt ty2
       ; return (mkFunTy xi1 xi2, mkTcFunCo co1 co2) }

flatten loc f ctxt (TyConApp tc tys)
  -- For a normal type constructor or data family application, we just
  -- recursively flatten the arguments.
  | not (isSynFamilyTyCon tc)
    = do { (xis,cos) <- flattenMany loc f ctxt tys
         ; return (mkTyConApp tc xis, mkTcTyConAppCo tc cos) }

  -- Otherwise, it's a type function application, and we have to
  -- flatten it away as well, and generate a new given equality constraint
  -- between the application and a newly generated flattening skolem variable.
  | otherwise
  = ASSERT( tyConArity tc <= length tys )	-- Type functions are saturated
      do { (xis, cos) <- flattenMany loc f ctxt tys
         ; let (xi_args,  xi_rest)  = splitAt (tyConArity tc) xis
               (cos_args, cos_rest) = splitAt (tyConArity tc) cos
	       	 -- The type function might be *over* saturated
		 -- in which case the remaining arguments should
		 -- be dealt with by AppTys
               fam_ty = mkTyConApp tc xi_args
               
         ; (ret_co, rhs_xi) <-
             case f of 
               FMSubstOnly -> 
                 return (mkTcReflCo fam_ty, fam_ty)
               FMFullFlatten -> 
                 do { mb_ct <- lookupFlatEqn fam_ty
                    ; case mb_ct of
                        Just (ctev, rhs_ty)
                          | let flav = ctEvFlavour ctev
                          , flav `canRewrite` ctxt 
                          -> -- You may think that we can just return (cc_rhs ct) but not so. 
                             --            return (mkTcCoVarCo (ctId ct), cc_rhs ct, []) 
                             -- The cached constraint resides in the cache so we have to flatten 
                             -- the rhs to make sure we have applied any inert substitution to it.
                             -- Alternatively we could be applying the inert substitution to the 
                             -- cache as well when we interact an equality with the inert. 
                             -- The design choice is: do we keep the flat cache rewritten or not?
                             -- For now I say we don't keep it fully rewritten.
                            do { (rhs_xi,co) <- flatten loc f flav rhs_ty
                               ; let final_co = evTermCoercion (ctEvTerm ctev)
                                                `mkTcTransCo` mkTcSymCo co
                               ; traceTcS "flatten/flat-cache hit" $ (ppr ctev $$ ppr rhs_xi $$ ppr final_co)
                               ; return (final_co, rhs_xi) }
                          
                        _ -> do { (ctev, rhs_xi) <- newFlattenSkolem ctxt fam_ty
                                ; let ct = CFunEqCan { cc_ev     = ctev
                                                     , cc_fun    = tc
                                                     , cc_tyargs = xi_args
                                                     , cc_rhs    = rhs_xi
                                                     , cc_loc    = loc }
                                ; updWorkListTcS $ extendWorkListFunEq ct
                                ; traceTcS "flatten/flat-cache miss" $ (ppr fam_ty $$ ppr rhs_xi $$ ppr ctev)
                                ; return (evTermCoercion (ctEvTerm ctev), rhs_xi) }
                    }
                  -- Emit the flat constraints
         ; return ( mkAppTys rhs_xi xi_rest -- NB mkAppTys: rhs_xi might not be a type variable
                                            --    cf Trac #5655
                  , mkTcAppCos (mkTcSymCo ret_co `mkTcTransCo` mkTcTyConAppCo tc cos_args) $
                    cos_rest
                  ) 
         }

flatten loc _f ctxt ty@(ForAllTy {})
-- We allow for-alls when, but only when, no type function
-- applications inside the forall involve the bound type variables.
  = do { let (tvs, rho) = splitForAllTys ty
       ; (rho', co) <- flatten loc FMSubstOnly ctxt rho   
                         -- Substitute only under a forall
                         -- See Note [Flattening under a forall]
       ; return (mkForAllTys tvs rho', foldr mkTcForAllCo co tvs) }
\end{code}

Note [Flattening under a forall]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Under a forall, we
  (a) MUST apply the inert subsitution
  (b) MUST NOT flatten type family applications
Hence FMSubstOnly.

For (a) consider   c ~ a, a ~ T (forall b. (b, [c])
If we don't apply the c~a substitution to the second constraint
we won't see the occurs-check error.

For (b) consider  (a ~ forall b. F a b), we don't want to flatten
to     (a ~ forall b.fsk, F a b ~ fsk)
because now the 'b' has escaped its scope.  We'd have to flatten to
       (a ~ forall b. fsk b, forall b. F a b ~ fsk b)
and we have not begun to think about how to make that work!

\begin{code}
flattenTyVar, flattenFinalTyVar
        :: CtLoc -> FlattenMode 
        -> CtFlavour -> TcTyVar -> TcS (Xi, TcCoercion)
-- "Flattening" a type variable means to apply the substitution to it
-- The substitution is actually the union of the substitution in the TyBinds
-- for the unification variables that have been unified already with the inert
-- equalities, see Note [Spontaneously solved in TyBinds] in TcInteract.
flattenTyVar loc f ctxt tv
  | not (isTcTyVar tv)	              -- Happens when flatten under a (forall a. ty)
  = flattenFinalTyVar loc f ctxt tv   -- So ty contains referneces to the non-TcTyVar a
  | otherwise
  = do { mb_ty <- isFilledMetaTyVar_maybe tv
       ; case mb_ty of {
           Just ty -> do { traceTcS "Following filled tyvar" (ppr tv <+> equals <+> ppr ty)
                         ; flatten loc f ctxt ty } ;
           Nothing -> 

    -- Try in ty_binds
    do { ty_binds <- getTcSTyBindsMap
       ; case lookupVarEnv ty_binds tv of {
           Just (_tv,ty) -> do { traceTcS "Following bound tyvar" (ppr tv <+> equals <+> ppr ty)
                               ; flatten loc f ctxt ty } ;
                 -- NB: ty_binds coercions are all ReflCo,
                 -- so no need to transitively compose co' with another coercion,
                 -- unlike in 'flatten_from_inerts'
           Nothing -> 

    -- Try in the inert equalities
    do { ieqs <- getInertEqs
       ; let mco = tv_eq_subst ieqs tv  -- co : v ~ ty
       ; case mco of { 
           Just (co,ty) -> 
             do { traceTcS "Following inert tyvar" (ppr tv <+> equals <+> ppr ty)
                ; (ty_final,co') <- flatten loc f ctxt ty
                ; return (ty_final, co' `mkTcTransCo` mkTcSymCo co) } ;
       -- NB recursive call. 
       -- Why? Because inert subst. non-idempotent, Note [Detailed InertCans Invariants]
       -- In fact, because of flavors, it couldn't possibly be idempotent,
       -- this is explained in Note [Non-idempotent inert substitution]

           Nothing -> flattenFinalTyVar loc f ctxt tv
    } } } } } } 
  where
    tv_eq_subst subst tv
       | Just ct <- lookupVarEnv subst tv
       , let ctev = cc_ev ct
             rhs  = cc_rhs ct
       , ctEvFlavour ctev `canRewrite` ctxt
       = Just (evTermCoercion (ctEvTerm ctev), rhs)
              -- NB: even if ct is Derived we are not going to 
              -- touch the actual coercion so we are fine. 
       | otherwise = Nothing

flattenFinalTyVar loc f ctxt tv
  = -- Done, but make sure the kind is zonked
    do { let knd = tyVarKind tv
       ; (new_knd, _kind_co) <- flatten loc f ctxt knd
       ; let ty = mkTyVarTy (setVarType tv new_knd)
       ; return (ty, mkTcReflCo ty) }
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


%************************************************************************
%*                                                                      *
%*        Equalities
%*                                                                      *
%************************************************************************

\begin{code}
canEvVarsCreated :: CtLoc -> [CtEvidence] -> TcS StopOrContinue
canEvVarsCreated _loc [] = return Stop
    -- Add all but one to the work list
    -- and return the first (if any) for futher processing
canEvVarsCreated loc (ev : evs) 
  = do { emitWorkNC loc evs; canEvNC loc ev }
          -- Note the "NC": these are fresh goals, not necessarily canonical

emitWorkNC :: CtLoc -> [CtEvidence] -> TcS ()
emitWorkNC loc evs 
  | null evs  = return ()
  | otherwise = updWorkListTcS (extendWorkListCts (map mk_nc evs))
  where
    mk_nc ev = CNonCanonical { cc_ev = ev, cc_loc = loc }

-------------------------
canEqNC :: CtLoc -> CtEvidence -> Type -> Type -> TcS StopOrContinue

canEqNC _loc ev ty1 ty2
  | eqType ty1 ty2	-- Dealing with equality here avoids
    	     	 	-- later spurious occurs checks for a~a
  = if isWanted ev then
      setEvBind (ctev_evar ev) (EvCoercion (mkTcReflCo ty1)) >> return Stop
    else
      return Stop

-- If one side is a variable, orient and flatten,
-- WITHOUT expanding type synonyms, so that we tend to 
-- substitute a ~ Age rather than a ~ Int when @type Age = Int@
canEqNC loc ev ty1@(TyVarTy {}) ty2 
  = canEqLeaf loc ev ty1 ty2
canEqNC loc ev ty1 ty2@(TyVarTy {})
  = canEqLeaf loc ev ty1 ty2

-- See Note [Naked given applications]
canEqNC loc ev ty1 ty2
  | Just ty1' <- tcView ty1 = canEqNC loc ev ty1' ty2
  | Just ty2' <- tcView ty2 = canEqNC loc ev ty1  ty2'

canEqNC loc ev ty1@(TyConApp fn tys) ty2
  | isSynFamilyTyCon fn, length tys == tyConArity fn
  = canEqLeaf loc ev ty1 ty2
canEqNC loc ev ty1 ty2@(TyConApp fn tys)
  | isSynFamilyTyCon fn, length tys == tyConArity fn
  = canEqLeaf loc ev ty1 ty2

canEqNC loc ev ty1 ty2
  | Just (tc1,tys1) <- tcSplitTyConApp_maybe ty1
  , Just (tc2,tys2) <- tcSplitTyConApp_maybe ty2
  , isDecomposableTyCon tc1 && isDecomposableTyCon tc2
  = canDecomposableTyConApp loc ev tc1 tys1 tc2 tys2 

canEqNC loc ev s1@(ForAllTy {}) s2@(ForAllTy {})
 | tcIsForAllTy s1, tcIsForAllTy s2
 , CtWanted { ctev_evar = orig_ev } <- ev 
 = do { let (tvs1,body1) = tcSplitForAllTys s1
            (tvs2,body2) = tcSplitForAllTys s2
      ; if not (equalLength tvs1 tvs2) then 
          canEqFailure loc ev s1 s2
        else
          do { traceTcS "Creating implication for polytype equality" $ ppr ev
             ; deferTcSForAllEq (loc,orig_ev) (tvs1,body1) (tvs2,body2) 
             ; return Stop } }
 | otherwise
 = do { traceTcS "Ommitting decomposition of given polytype equality" $ 
        pprEq s1 s2    -- See Note [Do not decompose given polytype equalities]
      ; return Stop }

-- The last remaining source of success is an application
-- e.g.  F a b ~ Maybe c   where F has arity 1
-- See Note [Equality between type applications]
--     Note [Care with type applications] in TcUnify
canEqNC loc ev ty1 ty2 
 =  do { let flav = ctEvFlavour ev
       ; (s1, co1) <- flatten loc FMSubstOnly flav ty1
       ; (s2, co2) <- flatten loc FMSubstOnly flav ty2
       ; mb_ct <- rewriteCtFlavor ev (mkTcEqPred s1 s2) (mkHdEqPred s2 co1 co2)
       ; case mb_ct of
           Nothing     -> return Stop
           Just new_ev -> last_chance new_ev s1 s2 }
  where
    last_chance ev ty1 ty2
      | Just (tc1,tys1) <- tcSplitTyConApp_maybe ty1
      , Just (tc2,tys2) <- tcSplitTyConApp_maybe ty2
      , isDecomposableTyCon tc1 && isDecomposableTyCon tc2
      = canDecomposableTyConApp loc ev tc1 tys1 tc2 tys2
    
      | Just (s1,t1) <- tcSplitAppTy_maybe ty1
      , Just (s2,t2) <- tcSplitAppTy_maybe ty2
      = do { let xevcomp [x,y] = EvCoercion (mkTcAppCo (evTermCoercion x) (evTermCoercion y))
             	 xevcomp _ = error "canEqAppTy: can't happen" -- Can't happen
             	 xevdecomp x = let xco = evTermCoercion x 
       	                       in [EvCoercion (mkTcLRCo CLeft xco), EvCoercion (mkTcLRCo CRight xco)]
       	   ; ctevs <- xCtFlavor ev [mkTcEqPred s1 s2, mkTcEqPred t1 t2] (XEvTerm xevcomp xevdecomp)
       	   ; canEvVarsCreated loc ctevs }

      | otherwise
      = do { emitInsoluble (CNonCanonical { cc_ev = ev, cc_loc = loc })
           ; return Stop }

------------------------
canDecomposableTyConApp :: CtLoc -> CtEvidence 
                        -> TyCon -> [TcType] 
                        -> TyCon -> [TcType] 
                        -> TcS StopOrContinue
canDecomposableTyConApp loc ev tc1 tys1 tc2 tys2
  | tc1 /= tc2 || length tys1 /= length tys2
    -- Fail straight away for better error messages
  = canEqFailure loc ev (mkTyConApp tc1 tys1) (mkTyConApp tc2 tys2)
  | otherwise
  = do { let xcomp xs  = EvCoercion (mkTcTyConAppCo tc1 (map evTermCoercion xs))
             xdecomp x = zipWith (\_ i -> EvCoercion $ mkTcNthCo i (evTermCoercion x)) tys1 [0..]
             xev = XEvTerm xcomp xdecomp
       ; ctevs <- xCtFlavor ev (zipWith mkTcEqPred tys1 tys2) xev
       ; canEvVarsCreated loc ctevs }

canEqFailure :: CtLoc -> CtEvidence -> TcType -> TcType -> TcS StopOrContinue
-- See Note [Make sure that insolubles are fully rewritten]
canEqFailure loc ev ty1 ty2
  = do { let flav = ctEvFlavour ev
       ; (s1, co1) <- flatten loc FMSubstOnly flav ty1
       ; (s2, co2) <- flatten loc FMSubstOnly flav ty2
       ; mb_ct <- rewriteCtFlavor ev (mkTcEqPred s1 s2)
                                     (mkHdEqPred s2 co1 co2)
       ; case mb_ct of
           Just new_ev -> emitInsoluble (CNonCanonical { cc_ev = new_ev, cc_loc = loc }) 
           Nothing -> pprPanic "canEqFailure" (ppr ev $$ ppr ty1 $$ ppr ty2)
       ; return Stop }
\end{code}

Note [Make sure that insolubles are fully rewritten]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When an equality fails, we still want to rewrite the equality 
all the way down, so that it accurately reflects 
 (a) the mutable reference substitution in force at start of solving
 (b) any ty-binds in force at this point in solving
See Note [Kick out insolubles] in TcInteract.
And if we don't do this there is a bad danger that 
TcSimplify.applyTyVarDefaulting will find a variable
that has in fact been substituted.

Note [Do not decompose given polytype equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider [G] (forall a. t1 ~ forall a. t2).  Can we decompose this?
No -- what would the evidence look like.  So instead we simply discard
this given evidence.   


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
We will reach the case canEqNC where we do a tcSplitAppTy_maybe, but if
we dont have the guards (Nothing <- tcView ty1) (Nothing <- tcView
ty2) then the given equation is going to fall through and get
completely forgotten!

What we want instead is this clause to apply only when there is no
immediate top-level synonym; if there is one it will be later on
unfolded by the later stages of canEqNC.

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
  = VarCls TcTyVar      -- ^ Type variable 
  | FunCls TyCon [Type] -- ^ Type function, exactly saturated
  | OtherCls TcType     -- ^ Neither of the above


classify :: TcType -> TypeClassifier

classify (TyVarTy tv) = ASSERT2( isTcTyVar tv, ppr tv ) VarCls tv
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
reOrient :: TypeClassifier -> TypeClassifier -> Bool	
-- (t1 `reOrient` t2) responds True 
--   iff we should flip to (t2~t1)
-- We try to say False if possible, to minimise evidence generation
--
-- Postcondition: After re-orienting, first arg is not OTherCls
reOrient (OtherCls {}) cls2 = ASSERT( case cls2 of { OtherCls {} -> False; _ -> True } )
                              True  -- One must be Var/Fun

reOrient (FunCls {}) _      = False             -- Fun/Other on rhs
  -- But consider the following variation: isGiven ev && isMetaTyVar tv
  -- See Note [No touchables as FunEq RHS] in TcSMonad

reOrient (VarCls {})   (FunCls {})           = True 
reOrient (VarCls {})   (OtherCls {})         = False
reOrient (VarCls tv1)  (VarCls tv2)  
  | not (k2 `isSubKind` k1),   k1 `isSubKind` k2   = True  -- Note [Kind orientation for CTyEqCan]
                                                           -- in TcRnTypes
  | not (isMetaTyVar tv1),     isMetaTyVar     tv2 = True 
  | not (isFlatSkolTyVar tv1), isFlatSkolTyVar tv2 = True  -- Note [Eliminate flat-skols]
  | otherwise                                      = False 
  where
    k1 = tyVarKind tv1
    k2 = tyVarKind tv2
  -- Just for efficiency, see CTyEqCan invariants 

------------------

canEqLeaf :: CtLoc -> CtEvidence 
          -> Type -> Type 
          -> TcS StopOrContinue
-- Canonicalizing "leaf" equality constraints which cannot be
-- decomposed further (ie one of the types is a variable or
-- saturated type function application).  

-- Preconditions: 
--    * one of the two arguments is variable 
--      or an exactly-saturated family application
--    * the two types are not equal (looking through synonyms)

-- NB: at this point we do NOT know that the kinds of s1 and s2 are
--     compatible.  See Note [Equalities with incompatible kinds]

canEqLeaf loc ev s1 s2 
  | cls1 `reOrient` cls2
  = do { traceTcS "canEqLeaf (reorienting)" $ ppr ev <+> dcolon <+> pprEq s1 s2
       ; let xcomp [x] = EvCoercion (mkTcSymCo (evTermCoercion x))
             xcomp _ = panic "canEqLeaf: can't happen"
             xdecomp x = [EvCoercion (mkTcSymCo (evTermCoercion x))]
             xev = XEvTerm xcomp xdecomp
       ; ctevs <- xCtFlavor ev [mkTcEqPred s2 s1] xev 
       ; case ctevs of
           []     -> return Stop
           [ctev] -> canEqLeafOriented loc ctev cls2 s1
           _      -> panic "canEqLeaf" }

  | otherwise
  = do { traceTcS "canEqLeaf" $ ppr (mkTcEqPred s1 s2)
       ; canEqLeafOriented loc ev cls1 s2 }
  where
    cls1 = classify s1
    cls2 = classify s2

canEqLeafOriented :: CtLoc -> CtEvidence
                  -> TypeClassifier -> TcType -> TcS StopOrContinue
-- By now s1 will either be a variable or a type family application
canEqLeafOriented loc ev (FunCls fn tys1) s2 = canEqLeafFun loc ev fn tys1 s2
canEqLeafOriented loc ev (VarCls tv)      s2 = canEqLeafTyVar loc ev tv s2
canEqLeafOriented _   ev (OtherCls {})    _  = pprPanic "canEqLeafOriented" (ppr (ctEvPred ev))

canEqLeafFun :: CtLoc -> CtEvidence
             -> TyCon -> [TcType] -> TcType -> TcS StopOrContinue
canEqLeafFun loc ev fn tys1 ty2  -- ev :: F tys1 ~ ty2
  = do { traceTcS "canEqLeafFun" $ pprEq (mkTyConApp fn tys1) ty2
       ; let flav = ctEvFlavour ev

            -- Flatten type function arguments
            -- cos1 :: xis1 ~ tys1
            -- co2  :: xi2 ~ ty2
      ; (xis1,cos1) <- flattenMany loc FMFullFlatten flav tys1 
      ; (xi2, co2)  <- flatten     loc FMFullFlatten flav ty2
           
          -- Fancy higher-dimensional coercion between equalities!
          -- SPJ asks why?  Why not just co : F xis1 ~ F tys1?
       ; let fam_head = mkTyConApp fn xis1
             xco = mkHdEqPred ty2 (mkTcTyConAppCo fn cos1) co2
             -- xco :: (F xis1 ~ xi2) ~ (F tys1 ~ ty2)

       ; mb <- rewriteCtFlavor ev (mkTcEqPred fam_head xi2) xco
       ; case mb of 
            Nothing     -> return Stop 
            Just new_ev | typeKind fam_head `isSubKind` typeKind xi2
                        -- Establish CFunEqCan kind invariant
                        -> continueWith (CFunEqCan { cc_ev = new_ev, cc_loc = loc
                                                   , cc_fun = fn, cc_tyargs = xis1, cc_rhs = xi2 })
                        | otherwise
                        -> checkKind loc new_ev fam_head xi2 }

canEqLeafTyVar :: CtLoc -> CtEvidence
               -> TcTyVar -> TcType -> TcS StopOrContinue
canEqLeafTyVar loc ev tv s2              -- ev :: tv ~ s2
  = do { traceTcS "canEqLeafTyVar 1" $ pprEq (mkTyVarTy tv) s2
       ; let flav = ctEvFlavour ev
       ; (xi1,co1) <- flattenTyVar loc FMFullFlatten flav tv -- co1 :: xi1 ~ tv
       ; (xi2,co2) <- flatten      loc FMFullFlatten flav s2 -- co2 :: xi2 ~ s2 
       ; let co = mkHdEqPred s2 co1 co2
             -- co :: (xi1 ~ xi2) ~ (tv ~ s2)
       
       ; traceTcS "canEqLeafTyVar 2" $ vcat [ppr xi1, ppr xi2]
       ; case (getTyVar_maybe xi1, getTyVar_maybe xi2) of
           (Nothing,  _) -> -- Rewriting the LHS did not yield a type variable
                            -- so go around again to canEq
                            do { mb <- rewriteCtFlavor ev (mkTcEqPred xi1 xi2) co
                               ; case mb of
                                   Nothing     -> return Stop
                                   Just new_ev -> canEqNC loc new_ev xi1 xi2 }

           (Just tv1, Just tv2) | tv1 == tv2
              -> do { when (isWanted ev) $
                      setEvBind (ctev_evar ev) (mkEvCast (EvCoercion (mkTcReflCo xi1)) co)
                    ; return Stop } 

           (Just tv1, _) -> do { dflags <- getDynFlags
                               ; canEqLeafTyVar2 dflags loc ev tv1 xi2 co } }

canEqLeafTyVar2 :: DynFlags -> CtLoc -> CtEvidence
                -> TyVar -> Type -> TcCoercion 
                -> TcS StopOrContinue
-- LHS rewrote to a type variable, 
-- RHS to something else (possibly a tyvar, but not the *same* tyvar)
canEqLeafTyVar2 dflags loc ev tv1 xi2 co
  | OC_OK xi2' <- occurCheckExpand dflags tv1 xi2  -- No occurs check
  = do { mb <- rewriteCtFlavor ev (mkTcEqPred xi1 xi2') co
                -- Ensure that the new goal has enough type synonyms 
                -- expanded by the occurCheckExpand; hence using xi2' here

       ; case mb of 
            Nothing     -> return Stop 
            Just new_ev | typeKind xi2' `isSubKind` tyVarKind tv1
                        -- Establish CTyEqCan kind invariant
                        -- Reorientation has done its best, but the kinds might
                        -- simply be incompatible
                        -> continueWith (CTyEqCan { cc_ev = new_ev, cc_loc = loc
                                                  , cc_tyvar  = tv1, cc_rhs = xi2' })
                        | otherwise
                        -> checkKind loc new_ev xi1 xi2' }

  | otherwise  -- Occurs check error
  = do { mb <- rewriteCtFlavor ev (mkTcEqPred xi1 xi2) co
       ; case mb of
           Nothing     -> return Stop
           Just new_ev -> canEqFailure loc new_ev xi1 xi2 }
  where
    xi1 = mkTyVarTy tv1

checkKind :: CtLoc 
          -> CtEvidence          -- t1~t2
          -> TcType -> TcType    -- s1~s2, flattened and zonked
          -> TcS StopOrContinue
-- LHS and RHS have incompatible kinds, so emit an "irreducible" constraint 
--       CIrredEvCan (NOT CTyEqCan or CFunEqCan)
-- for the type equality; and continue with the kind equality constraint.
-- When the latter is solved, it'll kick out the irreducible equality for 
-- a second attempt at solving
-- See Note [Equalities with incompatible kinds]

checkKind loc new_ev s1 s2
  = ASSERT( isKind k1 && isKind k2 )
    do {  -- See Note [Equalities with incompatible kinds]
         traceTcS "canEqLeaf: incompatible kinds" (vcat [ppr k1, ppr k2])
       ; updWorkListTcS $ extendWorkListNonEq $ 
         CIrredEvCan { cc_ev = new_ev, cc_loc = loc }
       ; mw <- newDerived (mkEqPred k1 k2) 
       ; case mw of
           Nothing  -> return Stop
           Just kev -> canEqNC kind_co_loc kev k1 k2 }

         -- Always create a Wanted kind equality even if 
         -- you are decomposing a given constraint.
         -- NB: DV finds this reasonable for now. Maybe we have to revisit.
  where
    k1 = typeKind s1
    k2 = typeKind s2
    kind_co_loc = setCtLocOrigin loc (KindEqOrigin s1 s2 (ctLocOrigin loc))


mkHdEqPred :: Type -> TcCoercion -> TcCoercion -> TcCoercion
-- Make a higher-dimensional equality
--    co1 :: s1~t1,  co2 :: s2~t2
-- Then (mkHdEqPred t2 co1 co2) :: (s1~s2) ~ (t1~t2)
mkHdEqPred t2 co1 co2 = mkTcTyConAppCo eqTyCon [mkTcReflCo (defaultKind (typeKind t2)), co1, co2]
   -- Why defaultKind? Same reason as the comment on TcType/mkTcEqPred. I truly hate this (DV)
\end{code}

Note [Eliminate flat-skols]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have  [G] Num (F [a])
then we flatten to
     [G] Num fsk
     [G] F [a] ~ fsk
where fsk is a flatten-skolem (FlatSkol). Suppose we have
      type instance F [a] = a
then we'll reduce the second constraint to
     [G] a ~ fsk
and then replace all uses of 'a' with fsk.  That's bad because
in error messages intead of saying 'a' we'll say (F [a]).  In all
places, including those where the programmer wrote 'a' in the first
place.  Very confusing!  See Trac #7862.

Solution: re-orient a~fsk to fsk~a, so that we preferentially eliminate
the fsk.

Note [Equalities with incompatible kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
canEqLeaf is about to make a CTyEqCan or CFunEqCan; but both have the
invariant that LHS and RHS satisfy the kind invariants for CTyEqCan,
CFunEqCan.  What if we try to unify two things with incompatible
kinds?

eg    a ~ b  where a::*, b::*->*
or    a ~ b  where a::*, b::k, k is a kind variable

The CTyEqCan compatKind invariant is important.  If we make a CTyEqCan
for a~b, then we might well *substitute* 'b' for 'a', and that might make
a well-kinded type ill-kinded; and that is bad (eg typeKind can crash, see
Trac #7696).

So instead for these ill-kinded equalities we generate a CIrredCan, 
which keeps it out of the way until a subsequent substitution (on kind 
variables, say) re-activates it.

NB: it is important that the types s1,s2 are flattened and zonked
    so that their kinds k1, k2 are inert wrt the substitution.  That
    means that they can only become the same if we change the inert
    set, which in turn will kick out the irreducible equality
    E.g. it is WRONG to make an irred (a:k1)~(b:k2)
         if we already have a substitution k1:=k2

See also Note [Kind orientation for CTyEqCan] and
         Note [Kind orientation for CFunEqCan] in TcRnTypes

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

