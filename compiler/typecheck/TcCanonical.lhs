\begin{code}
{-# LANGUAGE CPP #-}

module TcCanonical(
    canonicalize, 
    flattenMany, FlattenEnv(..), FlattenMode(..)
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
import Name( isSystemName )
import OccName( OccName )
import Outputable
import Control.Monad    ( when )
import DynFlags( DynFlags )
import VarSet
import TcSMonad

import Util
import BasicTypes
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

canonicalize :: Ct -> TcS (StopOrContinue Ct)
canonicalize ct@(CNonCanonical { cc_ev = ev })
  = do { traceTcS "canonicalize (non-canonical)" (ppr ct)
       ; {-# SCC "canEvVar" #-}
         canEvNC ev }

canonicalize (CDictCan { cc_ev = ev
                       , cc_class  = cls
                       , cc_tyargs = xis })
  = {-# SCC "canClass" #-}
    canClass ev cls xis -- Do not add any superclasses
canonicalize (CTyEqCan { cc_ev = ev
                       , cc_tyvar  = tv
                       , cc_rhs    = xi })
  = {-# SCC "canEqLeafTyVarEq" #-}
    canEqTyVar ev NotSwapped tv xi xi

canonicalize (CFunEqCan { cc_ev = ev
                        , cc_fun    = fn
                        , cc_tyargs = xis1
                        , cc_fsk    = fsk })
  = {-# SCC "canEqLeafFunEq" #-}
    canCFunEqCan ev fn xis1 fsk

canonicalize (CIrredEvCan { cc_ev = ev })
  = canIrred ev
canonicalize (CHoleCan { cc_ev = ev, cc_occ = occ })
  = canHole ev occ

canEvNC :: CtEvidence -> TcS (StopOrContinue Ct)
-- Called only for non-canonical EvVars
canEvNC ev
  = case classifyPredType (ctEvPred ev) of
      ClassPred cls tys -> traceTcS "canEvNC:cls" (ppr cls <+> ppr tys) >> canClassNC ev cls tys
      EqPred ty1 ty2    -> traceTcS "canEvNC:eq" (ppr ty1 $$ ppr ty2)   >> canEqNC    ev ty1 ty2
      TuplePred tys     -> traceTcS "canEvNC:tup" (ppr tys)             >> canTuple   ev tys
      IrredPred {}      -> traceTcS "canEvNC:irred" (ppr (ctEvPred ev)) >> canIrred   ev
\end{code}


%************************************************************************
%*                                                                      *
%*                      Tuple Canonicalization
%*                                                                      *
%************************************************************************

\begin{code}
canTuple :: CtEvidence -> [PredType] -> TcS (StopOrContinue Ct)
canTuple ev tys
  = do { traceTcS "can_pred" (text "TuplePred!")
       ; let xcomp = EvTupleMk
             xdecomp x = zipWith (\_ i -> EvTupleSel x i) tys [0..]
       ; xCtEvidence ev (XEvTerm tys xcomp xdecomp)
       ; stopWith ev "Decomposed tuple constraint" }
\end{code}

%************************************************************************
%*                                                                      *
%*                      Class Canonicalization
%*                                                                      *
%************************************************************************

\begin{code}
canClass, canClassNC
   :: CtEvidence
   -> Class -> [Type] -> TcS (StopOrContinue Ct)
-- Precondition: EvVar is class evidence

-- The canClassNC version is used on non-canonical constraints
-- and adds superclasses.  The plain canClass version is used
-- for already-canonical class constraints (but which might have
-- been subsituted or somthing), and hence do not need superclasses

canClassNC ev cls tys
  = canClass ev cls tys
    `andWhenContinue` emitSuperclasses

canClass ev cls tys
  = do { let fmode = FE { fe_ev = ev, fe_mode = FM_FlattenAll }
       ; (xis, cos) <- flattenMany fmode tys
       ; let co = mkTcTyConAppCo Nominal (classTyCon cls) cos
             xi = mkClassPred cls xis
             mk_ct new_ev = CDictCan { cc_ev = new_ev
                                     , cc_tyargs = xis, cc_class = cls }
       ; mb <- rewriteEvidence ev xi co
       ; traceTcS "canClass" (vcat [ ppr ev <+> ppr cls <+> ppr tys
                                   , ppr xi, ppr mb ])
       ; return (fmap mk_ct mb) }

emitSuperclasses :: Ct -> TcS (StopOrContinue Ct)
emitSuperclasses ct@(CDictCan { cc_ev = ev , cc_tyargs = xis_new, cc_class = cls })
            -- Add superclasses of this one here, See Note [Adding superclasses].
            -- But only if we are not simplifying the LHS of a rule.
 = do { newSCWorkFromFlavored ev cls xis_new
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
newSCWorkFromFlavored :: CtEvidence -> Class -> [Xi] -> TcS ()
-- Returns superclasses, see Note [Adding superclasses]
newSCWorkFromFlavored flavor cls xis
  | isDerived flavor
  = return ()  -- Deriveds don't yield more superclasses because we will
               -- add them transitively in the case of wanteds.

  | isGiven flavor
  = do { let sc_theta = immSuperClasses cls xis
             xev_decomp x = zipWith (\_ i -> EvSuperClass x i) sc_theta [0..]
             xev = XEvTerm { ev_preds  =  sc_theta
                           , ev_comp   = panic "Can't compose for given!"
                           , ev_decomp = xev_decomp }
       ; xCtEvidence flavor xev }

  | isEmptyVarSet (tyVarsOfTypes xis)
  = return () -- Wanteds with no variables yield no deriveds.
              -- See Note [Improvement from Ground Wanteds]

  | otherwise -- Wanted case, just add those SC that can lead to improvement.
  = do { let sc_rec_theta = transSuperClasses cls xis
             impr_theta   = filter is_improvement_pty sc_rec_theta
             loc          = ctev_loc flavor
       ; traceTcS "newSCWork/Derived" $ text "impr_theta =" <+> ppr impr_theta
       ; mb_der_evs <- mapM (newDerived loc) impr_theta
       ; emitWorkNC (catMaybes mb_der_evs) }

is_improvement_pty :: PredType -> Bool
-- Either it's an equality, or has some functional dependency
is_improvement_pty ty = go (classifyPredType ty)
  where
    go (EqPred t1 t2)       = not (t1 `tcEqType` t2)
    go (ClassPred cls _tys) = not $ null fundeps
                            where (_,fundeps) = classTvsFds cls
    go (TuplePred ts)       = any is_improvement_pty ts
    go (IrredPred {})       = True -- Might have equalities after reduction?
\end{code}


%************************************************************************
%*                                                                      *
%*                      Irreducibles canonicalization
%*                                                                      *
%************************************************************************


\begin{code}
canIrred :: CtEvidence -> TcS (StopOrContinue Ct)
-- Precondition: ty not a tuple and no other evidence form
canIrred old_ev
  = do { let old_ty = ctEvPred old_ev
             fmode  = FE { fe_ev = old_ev, fe_mode = FM_FlattenAll }
                      -- Flatten (F [a]), say, so that it can reduce to Eq a
       ; traceTcS "can_pred" (text "IrredPred = " <+> ppr old_ty)
       ; (xi,co) <- flatten fmode old_ty -- co :: xi ~ old_ty
       ; mb <- rewriteEvidence old_ev xi co
       ; case mb of {
             Stop ev s           -> return (Stop ev s) ;
             ContinueWith new_ev ->

    do { -- Re-classify, in case flattening has improved its shape
       ; case classifyPredType (ctEvPred new_ev) of
           ClassPred cls tys -> canClassNC new_ev cls tys
           TuplePred tys     -> canTuple   new_ev tys
           EqPred ty1 ty2    -> canEqNC new_ev ty1 ty2
           _                 -> continueWith $
                                CIrredEvCan { cc_ev = new_ev } } } }

canHole :: CtEvidence -> OccName -> TcS (StopOrContinue Ct)
canHole ev occ
  = do { let ty    = ctEvPred ev
             fmode = FE { fe_ev = ev, fe_mode = FM_SubstOnly }
       ; (xi,co) <- flatten fmode ty -- co :: xi ~ ty
       ; mb <- rewriteEvidence ev xi co
       ; case mb of
           ContinueWith new_ev -> do { emitInsoluble (CHoleCan { cc_ev = new_ev, cc_occ = occ })
                                     ; stopWith new_ev "Emit insoluble hole" }
           Stop ev s -> return (Stop ev s) } -- Found a cached copy; won't happen
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
data FlattenEnv
  = FE { fe_mode :: FlattenMode
       , fe_ev   :: CtEvidence }

data FlattenMode  -- Postcondition for all three: inert wrt the type substitution
  = FM_FlattenAll          -- Postcondition: function-free

  | FM_Avoid TcTyVar Bool  -- Postcondition:
                           --  * tyvar is only mentioned in result under a rigid path
                           --    e.g.   [a] is ok, but F a won't happen
                           --  * If flat_top is True, top level is not a function application
                           --   (but under type constructors is ok e.g. [F a])

  | FM_SubstOnly           -- See Note [Flattening under a forall]

-- Flatten a bunch of types all at once.
flattenMany :: FlattenEnv -> [Type] -> TcS ([Xi], [TcCoercion])
-- Coercions :: Xi ~ Type
-- Returns True iff (no flattening happened)
-- NB: The EvVar inside the 'fe_ev :: CtEvidence' is unused,
--     we merely want (a) Given/Solved/Derived/Wanted info
--                    (b) the GivenLoc/WantedLoc for when we create new evidence
flattenMany fmode tys
  = -- pprTrace "flattenMany" empty $
    go tys
  where go []       = return ([],[])
        go (ty:tys) = do { (xi,co)    <- flatten fmode ty
                         ; (xis,cos)  <- go tys
                         ; return (xi:xis,co:cos) }

flatten :: FlattenEnv -> TcType -> TcS (Xi, TcCoercion)
-- Flatten a type to get rid of type function applications, returning
-- the new type-function-free type, and a collection of new equality
-- constraints.  See Note [Flattening] for more detail.
--
-- Postcondition: Coercion :: Xi ~ TcType

flatten _ xi@(LitTy {}) = return (xi, mkTcNomReflCo xi)

flatten fmode (TyVarTy tv)
  = flattenTyVar fmode tv

flatten fmode (AppTy ty1 ty2)
  = do { (xi1,co1) <- flatten fmode ty1
       ; (xi2,co2) <- flatten fmode ty2
       ; traceTcS "flatten/appty" (ppr ty1 $$ ppr ty2 $$ ppr xi1 $$ ppr co1 $$ ppr xi2 $$ ppr co2)
       ; return (mkAppTy xi1 xi2, mkTcAppCo co1 co2) }

flatten fmode (FunTy ty1 ty2)
  = do { (xi1,co1) <- flatten fmode ty1
       ; (xi2,co2) <- flatten fmode ty2
       ; return (mkFunTy xi1 xi2, mkTcFunCo Nominal co1 co2) }

flatten fmode (TyConApp tc tys)

  -- Expand type synonyms that mention type families 
  -- on the RHS; see Note [Flattening synonyms]
  | Just (tenv, rhs, tys') <- tcExpandTyCon_maybe tc tys
  , let expanded_ty = mkAppTys (substTy (mkTopTvSubst tenv) rhs) tys'
  = case fe_mode fmode of
      FM_FlattenAll | any isSynFamilyTyCon (tyConsOfType rhs)
                   -> flatten fmode expanded_ty
                    | otherwise
                   -> flattenTyConApp fmode tc tys
      _ -> flattenTyConApp fmode tc tys

  -- Otherwise, it's a type function application, and we have to
  -- flatten it away as well, and generate a new given equality constraint
  -- between the application and a newly generated flattening skolem variable.
  | isSynFamilyTyCon tc
  = flattenFamApp fmode tc tys

  -- For * a normal data type application
  --     * data family application
  -- we just recursively flatten the arguments.
  | otherwise  -- Switch off the flat_top bit in FM_Avoid
  , let fmode' = case fmode of
                   FE { fe_mode = FM_Avoid tv _ }
                     -> fmode { fe_mode = FM_Avoid tv False }
                   _ -> fmode
  = flattenTyConApp fmode' tc tys

flatten fmode ty@(ForAllTy {})
-- We allow for-alls when, but only when, no type function
-- applications inside the forall involve the bound type variables.
  = do { let (tvs, rho) = splitForAllTys ty
       ; (rho', co) <- flatten (fmode { fe_mode = FM_SubstOnly }) rho
                         -- Substitute only under a forall
                         -- See Note [Flattening under a forall]
       ; return (mkForAllTys tvs rho', foldr mkTcForAllCo co tvs) }

flattenTyConApp :: FlattenEnv -> TyCon -> [TcType] -> TcS (Xi, TcCoercion)
flattenTyConApp fmode tc tys
  = do { (xis, cos) <- flattenMany fmode tys
       ; return (mkTyConApp tc xis, mkTcTyConAppCo Nominal tc cos) }
\end{code}

Note [Flattening synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Not expanding synonyms aggressively improves error messages, and
keeps types smaller. But we need to take care.

Suppose
   type T a = a -> a
and we want to flatten the type (T (F a)).  Then we can safely flatten
the (F a) to a skolem, and return (T fsk).  We don't need to expand the
synonym.  This works because TcTyConAppCo can deal with synonyms
(unlike TyConAppCo), see Note [TcCoercions] in TcEvidence.

But (Trac #8979) for
   type T a = (F a, a)    where F is a type function
we must expand the synonym in (say) T Int, to expose the type function
to the flattener.


Note [Flattening under a forall]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Under a forall, we
  (a) MUST apply the inert substitution
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
flattenFamApp, flattenExactFamApp, flattenExactFamApp_fully
  :: FlattenEnv -> TyCon -> [TcType] -> TcS (Xi, TcCoercion)
  -- flattenFamApp      can be over-saturated
  -- flattenExactFamApp is exactly saturated
  -- Postcondition: Coercion :: Xi ~ F tys
flattenFamApp fmode tc tys  -- Can be over-saturated
    = ASSERT( tyConArity tc <= length tys )  -- Type functions are saturated
                 -- The type function might be *over* saturated
                 -- in which case the remaining arguments should
                 -- be dealt with by AppTys
      do { let (tys1, tys_rest) = splitAt (tyConArity tc) tys
         ; (xi1, co1) <- flattenExactFamApp fmode tc tys1
               -- co1 :: xi1 ~ F tys1
         ; (xis_rest, cos_rest) <- flattenMany fmode tys_rest
               -- cos_res :: xis_rest ~ tys_rest
         ; return ( mkAppTys xi1 xis_rest   -- NB mkAppTys: rhs_xi might not be a type variable
                                            --    cf Trac #5655
                  , mkTcAppCos co1 cos_rest -- (rhs_xi :: F xis) ; (F cos :: F xis ~ F tys)
                  ) }

flattenExactFamApp fmode tc tys
  = case fe_mode fmode of
       FM_SubstOnly -> do { (xis, cos) <- flattenMany fmode tys
                          ; return ( mkTyConApp tc xis
                                   , mkTcTyConAppCo Nominal tc cos ) }

       FM_Avoid tv flat_top -> do { (xis, cos) <- flattenMany fmode tys
                                  ; if flat_top || tv `elemVarSet` tyVarsOfTypes xis
                                    then flattenExactFamApp_fully fmode tc tys
                                    else return ( mkTyConApp tc xis
                                                , mkTcTyConAppCo Nominal tc cos ) }
       FM_FlattenAll -> flattenExactFamApp_fully fmode tc tys

flattenExactFamApp_fully fmode tc tys
  = do { (xis, cos) <- flattenMany (fmode { fe_mode = FM_FlattenAll })tys
       ; let ret_co = mkTcTyConAppCo Nominal tc cos
              -- ret_co :: F xis ~ F tys
             ctxt_ev = fe_ev fmode

       ; mb_ct <- lookupFlatCache tc xis
       ; case mb_ct of
           Just (co, fsk)  -- co :: F xis ~ fsk
             | isFskTyVar fsk || not (isGiven ctxt_ev)
             ->  -- Usable hit in the flat-cache
                 -- isFskTyVar checks for a "given" in the cache
                do { traceTcS "flatten/flat-cache hit" $ (ppr tc <+> ppr xis $$ ppr fsk $$ ppr co)
                   ; (fsk_xi, fsk_co) <- flattenTyVar fmode fsk
                          -- The fsk may already have been unified, so flatten it
                          -- fsk_co :: fsk_xi ~ fsk
                   ; return (fsk_xi, fsk_co `mkTcTransCo` mkTcSymCo co `mkTcTransCo` ret_co) }
                                    -- :: fsk_xi ~ F xis

           _ -> do { let fam_ty = mkTyConApp tc xis
                   ; (ev, fsk) <- newFlattenSkolem ctxt_ev fam_ty
                   ; extendFlatCache tc xis (ctEvCoercion ev, fsk)

                   -- The new constraint (F xis ~ fsk) is not necessarily inert
                   -- (e.g. the LHS may be a redex) so we must put it in the work list
                   ; let ct = CFunEqCan { cc_ev     = ev
                                        , cc_fun    = tc
                                        , cc_tyargs = xis
                                        , cc_fsk    = fsk }
                   ; updWorkListTcS (extendWorkListFunEq ct)

                   ; traceTcS "flatten/flat-cache miss" $ (ppr fam_ty $$ ppr fsk $$ ppr ev)
                   ; return (mkTyVarTy fsk, mkTcSymCo (ctEvCoercion ev) `mkTcTransCo` ret_co) } }
\end{code}

\begin{code}
flattenTyVar :: FlattenEnv -> TcTyVar -> TcS (Xi, TcCoercion)
-- "Flattening" a type variable means to apply the substitution to it
-- The substitution is actually the union of the substitution in the TyBinds
-- for the unification variables that have been unified already with the inert
-- equalities, see Note [Spontaneously solved in TyBinds] in TcInteract.
--
-- Postcondition: co : xi ~ tv
flattenTyVar fmode tv
  = do { mb_yes <- flattenTyVarOuter (fe_ev fmode) tv
       ; case mb_yes of
           Left tv'         -> -- Done 
                               do { traceTcS "flattenTyVar1" (ppr tv $$ ppr (tyVarKind tv'))
                                  ; return (ty', mkTcNomReflCo ty') }
                            where
                               ty' = mkTyVarTy tv'

           Right (ty1, co1) -> -- Recurse
                               do { (ty2, co2) <- flatten fmode ty1
                                  ; traceTcS "flattenTyVar2" (ppr tv $$ ppr ty2)
                                  ; return (ty2, co2 `mkTcTransCo` co1) }
       }

flattenTyVarOuter, flattenTyVarFinal 
   :: CtEvidence -> TcTyVar -> TcS (Either TyVar (TcType, TcCoercion))
-- Look up the tyvar in 
--   a) the internal MetaTyVar box
--   b) the tyvar binds 
--   c) the inerts
-- Return (Left tv')       if it is not found, tv' has a properly zonked kind
--        (Right (ty, co)) if found, with co :: ty ~ tv
--                         NB: in the latter case ty is not necessarily flattened

flattenTyVarOuter ctxt_ev tv
  | not (isTcTyVar tv)             -- Happens when flatten under a (forall a. ty)
  = flattenTyVarFinal ctxt_ev tv   -- So ty contains refernces to the non-TcTyVar a
  | otherwise
  = do { mb_ty <- isFilledMetaTyVar_maybe tv
       ; case mb_ty of {
           Just ty -> do { traceTcS "Following filled tyvar" (ppr tv <+> equals <+> ppr ty)
                         ; return (Right (ty, mkTcNomReflCo ty)) } ;
           Nothing ->

    -- Try in ty_binds
    do { ty_binds <- getTcSTyBindsMap
       ; case lookupVarEnv ty_binds tv of {
           Just (_tv,ty) -> do { traceTcS "Following bound tyvar" (ppr tv <+> equals <+> ppr ty)
                               ; return (Right (ty, mkTcNomReflCo ty)) } ;
                                 -- NB: ty_binds coercions are all ReflCo,
           Nothing ->

    -- Try in the inert equalities
    do { ieqs <- getInertEqs
       ; case lookupVarEnv ieqs tv of
           Just (ct:_)   -- If the first doesn't work,
                         -- the subsequent ones won't either
             | CTyEqCan { cc_ev = ctev, cc_tyvar = tv, cc_rhs = rhs_ty } <- ct
             , eqCanRewrite tv ctev ctxt_ev
             ->  do { traceTcS "Following inert tyvar" (ppr tv <+> equals <+> ppr rhs_ty $$ ppr ctev)
                    ; return (Right (rhs_ty, mkTcSymCo (ctEvCoercion ctev))) }
                    -- NB: ct is Derived then (fe_ev fmode) must be also, hence
                    -- we are not going to touch the returned coercion
                    -- so ctEvCoercion is fine.

           _other -> flattenTyVarFinal ctxt_ev tv
    } } } } }

flattenTyVarFinal ctxt_ev tv
  = -- Done, but make sure the kind is zonked
    do { let kind       = tyVarKind tv
             kind_fmode = FE { fe_ev = ctxt_ev, fe_mode = FM_SubstOnly }
       ; (new_knd, _kind_co) <- flatten kind_fmode kind
       ; return (Left (setVarType tv new_knd)) }
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
canEqNC :: CtEvidence -> Type -> Type -> TcS (StopOrContinue Ct)
canEqNC ev ty1 ty2 = can_eq_nc ev ty1 ty1 ty2 ty2

can_eq_nc, can_eq_nc' 
   :: CtEvidence 
   -> Type -> Type    -- LHS, after and before type-synonym expansion, resp 
   -> Type -> Type    -- RHS, after and before type-synonym expansion, resp 
   -> TcS (StopOrContinue Ct)

can_eq_nc ev ty1 ps_ty1 ty2 ps_ty2
  = do { traceTcS "can_eq_nc" $ 
         vcat [ ppr ev, ppr ty1, ppr ps_ty1, ppr ty2, ppr ps_ty2 ]
       ; can_eq_nc' ev ty1 ps_ty1 ty2 ps_ty2 }

-- Expand synonyms first; see Note [Type synonyms and canonicalization]
can_eq_nc' ev ty1 ps_ty1 ty2 ps_ty2
  | Just ty1' <- tcView ty1 = can_eq_nc ev ty1' ps_ty1 ty2  ps_ty2
  | Just ty2' <- tcView ty2 = can_eq_nc ev ty1  ps_ty1 ty2' ps_ty2

-- Type family on LHS or RHS take priority over tyvars,
-- so that  tv ~ F ty gets flattened
-- Otherwise  F a ~ F a  might not get solved!
can_eq_nc' ev (TyConApp fn1 tys1) _ ty2 ps_ty2
  | isSynFamilyTyCon fn1 = can_eq_fam_nc ev NotSwapped fn1 tys1 ty2 ps_ty2
can_eq_nc' ev ty1 ps_ty1 (TyConApp fn2 tys2) _
  | isSynFamilyTyCon fn2 = can_eq_fam_nc ev IsSwapped fn2 tys2 ty1 ps_ty1

-- Type variable on LHS or RHS are next
can_eq_nc' ev (TyVarTy tv1) _ ty2 ps_ty2
  = canEqTyVar ev NotSwapped tv1 ty2 ps_ty2
can_eq_nc' ev ty1 ps_ty1 (TyVarTy tv2) _
  = canEqTyVar ev IsSwapped tv2 ty1 ps_ty1

----------------------
-- Otherwise try to decompose
----------------------

-- Literals
can_eq_nc' ev ty1@(LitTy l1) _ (LitTy l2) _
 | l1 == l2
  = do { when (isWanted ev) $
         setEvBind (ctev_evar ev) (EvCoercion (mkTcNomReflCo ty1))
       ; stopWith ev "Equal LitTy" }

-- Decomposable type constructor applications 
-- Synonyms and type functions (which are not decomposable)
-- have already been dealt with 
can_eq_nc' ev (TyConApp tc1 tys1) _ (TyConApp tc2 tys2) _
  | isDecomposableTyCon tc1
  , isDecomposableTyCon tc2
  = canDecomposableTyConApp ev tc1 tys1 tc2 tys2

can_eq_nc' ev (TyConApp tc1 _) ps_ty1 (FunTy {}) ps_ty2
  | isDecomposableTyCon tc1 
      -- The guard is important
      -- e.g.  (x -> y) ~ (F x y) where F has arity 1
      --       should not fail, but get the app/app case
  = canEqFailure ev ps_ty1 ps_ty2

can_eq_nc' ev (FunTy s1 t1) _ (FunTy s2 t2) _
  = canDecomposableTyConAppOK ev funTyCon [s1,t1] [s2,t2]

can_eq_nc' ev (FunTy {}) ps_ty1 (TyConApp tc2 _) ps_ty2
  | isDecomposableTyCon tc2 
  = canEqFailure ev ps_ty1 ps_ty2

can_eq_nc' ev s1@(ForAllTy {}) _ s2@(ForAllTy {}) _
 | CtWanted { ctev_loc = loc, ctev_evar = orig_ev } <- ev
 = do { let (tvs1,body1) = tcSplitForAllTys s1
            (tvs2,body2) = tcSplitForAllTys s2
      ; if not (equalLength tvs1 tvs2) then
          canEqFailure ev s1 s2
        else
          do { traceTcS "Creating implication for polytype equality" $ ppr ev
             ; ev_term <- deferTcSForAllEq Nominal loc (tvs1,body1) (tvs2,body2)
             ; setEvBind orig_ev ev_term
             ; stopWith ev "Deferred polytype equality" } }
 | otherwise
 = do { traceTcS "Ommitting decomposition of given polytype equality" $
        pprEq s1 s2    -- See Note [Do not decompose given polytype equalities]
      ; stopWith ev "Discard given polytype equality" }

can_eq_nc' ev (AppTy s1 t1) ps_ty1 ty2 ps_ty2
  = can_eq_app ev NotSwapped s1 t1 ps_ty1 ty2 ps_ty2
can_eq_nc' ev ty1 ps_ty1 (AppTy s2 t2) ps_ty2
  = can_eq_app ev IsSwapped s2 t2 ps_ty2 ty1 ps_ty1

-- Everything else is a definite type error, eg LitTy ~ TyConApp
can_eq_nc' ev _ ps_ty1 _ ps_ty2
  = canEqFailure ev ps_ty1 ps_ty2

------------
can_eq_fam_nc :: CtEvidence -> SwapFlag
              -> TyCon -> [TcType]
              -> TcType -> TcType
              -> TcS (StopOrContinue Ct)
-- Canonicalise a non-canonical equality of form (F tys ~ ty)
--   or the swapped version thereof
-- Flatten both sides and go round again
can_eq_fam_nc ev swapped fn tys rhs ps_rhs
  = do { let fmode = FE { fe_ev = ev, fe_mode = FM_FlattenAll }
       ; (xi_lhs, co_lhs) <- flattenFamApp fmode fn tys
       ; mb_ct <- rewriteEqEvidence ev swapped xi_lhs rhs co_lhs (mkTcNomReflCo rhs)
       ; case mb_ct of
           Stop ev s           -> return (Stop ev s)
           ContinueWith new_ev -> can_eq_nc new_ev xi_lhs xi_lhs rhs ps_rhs }

------------
can_eq_app, can_eq_flat_app
    :: CtEvidence -> SwapFlag
    -> Type -> Type -> Type  -- LHS (s1 t2), after and before type-synonym expansion, resp
    -> Type -> Type          -- RHS (ty2),   after and before type-synonym expansion, resp
    -> TcS (StopOrContinue Ct)
-- See Note [Canonicalising type applications]
can_eq_app ev swapped s1 t1 ps_ty1 ty2 ps_ty2
  =  do { traceTcS "can_eq_app 1" $
          vcat [ ppr ev, ppr swapped, ppr s1, ppr t1, ppr ty2 ]
        ; let fmode = FE { fe_ev = ev, fe_mode = FM_FlattenAll }
        ; (xi_s1, co_s1) <- flatten fmode s1
        ; traceTcS "can_eq_app 2" $ vcat [ ppr ev, ppr xi_s1 ]
        ; if s1 `tcEqType` xi_s1
          then can_eq_flat_app ev swapped s1 t1 ps_ty1 ty2 ps_ty2
          else
     do { (xi_t1, co_t1) <- flatten fmode t1
             -- We flatten t1 as well so that (xi_s1 xi_t1) is well-kinded
             -- If we form (xi_s1 t1) that might (appear) ill-kinded, 
             -- and then crash in a call to typeKind
        ; let xi1 = mkAppTy xi_s1 xi_t1
              co1 = mkTcAppCo co_s1 co_t1
        ; traceTcS "can_eq_app 3" $ vcat [ ppr ev, ppr xi1, ppr co1 ]
        ; mb_ct <- rewriteEqEvidence ev swapped xi1 ps_ty2 
                                     co1 (mkTcNomReflCo ps_ty2)
        ; traceTcS "can_eq_app 4" $ vcat [ ppr ev, ppr xi1, ppr co1 ]
        ; case mb_ct of
            Stop ev s           -> return (Stop ev s)
            ContinueWith new_ev -> can_eq_nc new_ev xi1 xi1 ty2 ps_ty2 }}

-- Preconditions: s1  is already flattened
--                ty2 is not a type variable, so flattening
--                    can't turn it into an application if it
--                    doesn't look like one already
-- See Note [Canonicalising type applications]
can_eq_flat_app ev swapped s1 t1 ps_ty1 ty2 ps_ty2
  | Just (s2,t2) <- tcSplitAppTy_maybe ty2
  = unSwap swapped decompose_it (s1,t1) (s2,t2)
  | otherwise
  = unSwap swapped (canEqFailure ev) ps_ty1 ps_ty2
  where
    decompose_it (s1,t1) (s2,t2) 
      = do { let xevcomp [x,y] = EvCoercion (mkTcAppCo (evTermCoercion x) (evTermCoercion y))
                 xevcomp _ = error "canEqAppTy: can't happen" -- Can't happen
                 xevdecomp x = let xco = evTermCoercion x
                               in [ EvCoercion (mkTcLRCo CLeft xco)
                                  , EvCoercion (mkTcLRCo CRight xco)]
           ; xCtEvidence ev (XEvTerm [mkTcEqPred s1 s2, mkTcEqPred t1 t2] xevcomp xevdecomp)
           ; stopWith ev "Decomposed AppTy" }


------------------------
canDecomposableTyConApp :: CtEvidence
                        -> TyCon -> [TcType]
                        -> TyCon -> [TcType]
                        -> TcS (StopOrContinue Ct)
canDecomposableTyConApp ev tc1 tys1 tc2 tys2
  | tc1 /= tc2 || length tys1 /= length tys2
    -- Fail straight away for better error messages
  = canEqFailure ev (mkTyConApp tc1 tys1) (mkTyConApp tc2 tys2)
  | otherwise
  = do { traceTcS "canDecomposableTyConApp" (ppr ev $$ ppr tc1 $$ ppr tys1 $$ ppr tys2)
       ; canDecomposableTyConAppOK ev tc1 tys1 tys2 }

canDecomposableTyConAppOK :: CtEvidence
                          -> TyCon -> [TcType] -> [TcType]
                          -> TcS (StopOrContinue Ct)

canDecomposableTyConAppOK ev tc1 tys1 tys2
  = do { let xcomp xs  = EvCoercion (mkTcTyConAppCo Nominal tc1 (map evTermCoercion xs))
             xdecomp x = zipWith (\_ i -> EvCoercion $ mkTcNthCo i (evTermCoercion x)) tys1 [0..]
             xev = XEvTerm (zipWith mkTcEqPred tys1 tys2) xcomp xdecomp
       ; xCtEvidence ev xev
       ; stopWith ev "Decomposed TyConApp" }

canEqFailure :: CtEvidence -> TcType -> TcType -> TcS (StopOrContinue Ct)
-- See Note [Make sure that insolubles are fully rewritten]
canEqFailure ev ty1 ty2
  = do { let fmode = FE { fe_ev = ev, fe_mode = FM_SubstOnly }
       ; (s1, co1) <- flatten fmode ty1
       ; (s2, co2) <- flatten fmode ty2
       ; mb_ct <- rewriteEqEvidence ev NotSwapped s1 s2 co1 co2
       ; case mb_ct of
           ContinueWith new_ev -> do { emitInsoluble (mkNonCanonical new_ev)
                                     ; stopWith new_ev "Definitely not equal" }
           Stop ev s -> pprPanic "canEqFailure" (s $$ ppr ev $$ ppr ty1 $$ ppr ty2) }
\end{code}

Note [Canonicalising type applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given (s1 t1) ~ ty2, how should we proceed?
The simple things is to see if ty2 is of form (s2 t2), and 
decompose.  By this time s1 and s2 can't be saturated type
function applications, because those have been dealt with 
by an earlier equation in can_eq_nc, so it is always sound to 
decompose.

However, over-eager decomposition gives bad error messages 
for things like
   a b ~ Maybe c
   e f ~ p -> q
Suppose (in the first example) we already know a~Array.  Then if we
decompose the application eagerly, yielding
   a ~ Maybe
   b ~ c
we get an error        "Can't match Array ~ Maybe", 
but we'd prefer to get "Can't match Array b ~ Maybe c".

So instead can_eq_app flattens s1.  If flattening does something, it
rewrites, and goes round can_eq_nc again.  If flattening 
does nothing, then (at least with our present state of knowledge)
we can only decompose, and that is what can_eq_flat_app attempts
to do. 

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

Note [Do not decompose Given polytype equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider [G] (forall a. t1 ~ forall a. t2).  Can we decompose this?
No -- what would the evidence look like?  So instead we simply discard
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
canCFunEqCan :: CtEvidence 
             -> TyCon -> [TcType]   -- LHS
             -> TcTyVar             -- RHS
             -> TcS (StopOrContinue Ct)
-- ^ Canonicalise a CFunEqCan.  We know that 
--     the arg types are already flat, 
-- and the RHS is a fsk, which we must *not* substitute.
-- So just substitute in the LHS
canCFunEqCan ev fn tys fsk
  = do { let fmode = FE { fe_ev = ev, fe_mode = FM_FlattenAll }
       ; (tys', cos) <- flattenMany fmode tys
                        -- cos :: tys' ~ tys
       ; let lhs_co  = mkTcTyConAppCo Nominal fn cos
                        -- :: F tys' ~ F tys
             new_lhs = mkTyConApp fn tys'
             fsk_ty  = mkTyVarTy fsk
       ; mb_ev <- rewriteEqEvidence ev NotSwapped new_lhs fsk_ty 
                                    lhs_co (mkTcNomReflCo fsk_ty)
       ; case mb_ev of {
           Stop ev s        -> return (Stop ev s) ;
           ContinueWith ev' -> 

    do { extendFlatCache fn tys' (ctEvCoercion ev', fsk)
       ; continueWith (CFunEqCan { cc_ev = ev', cc_fun = fn
                                 , cc_tyargs = tys', cc_fsk = fsk }) } } }

---------------------
canEqTyVar :: CtEvidence -> SwapFlag
           -> TcTyVar 
           -> TcType -> TcType
           -> TcS (StopOrContinue Ct)
-- A TyVar on LHS, but so far un-zonked
canEqTyVar ev swapped tv1 ty2 ps_ty2              -- ev :: tv ~ s2
  = do { traceTcS "canEqTyVar" (ppr tv1 $$ ppr ty2 $$ ppr swapped)
       ; mb_yes <- flattenTyVarOuter ev tv1
       ; case mb_yes of
           Right (ty1, co1) -> -- co1 :: ty1 ~ tv1
                               do { mb <- rewriteEqEvidence ev swapped  ty1 ps_ty2
                                                            co1 (mkTcNomReflCo ps_ty2)
                                  ; traceTcS "canEqTyVar2" (vcat [ppr tv1, ppr ty2, ppr swapped, ppr ty1,
                                                                  ppUnless (isDerived ev) (ppr co1)])
                                  ; case mb of
                                      Stop ev s           -> return (Stop ev s)
                                      ContinueWith new_ev -> can_eq_nc new_ev ty1 ty1 ty2 ps_ty2 }

           Left tv1' -> do { let fmode = FE { fe_ev = ev, fe_mode = FM_Avoid tv1' True }
                                 -- Flatten the RHS less vigorously, to avoid gratuitous
                           ; (xi2, co2) <- flatten fmode ps_ty2 -- co2 :: xi2 ~ ps_ty2
                                           -- Use ps_ty2 to preserve type synonyms if poss
                           ; dflags <- getDynFlags
                           ; canEqTyVar2 dflags ev swapped tv1' xi2 co2 } }

canEqTyVar2 :: DynFlags
            -> CtEvidence   -- olhs ~ orhs (or, if swapped, orhs ~ olhs)
            -> SwapFlag
            -> TcTyVar      -- olhs
            -> TcType       -- nrhs
            -> TcCoercion   -- nrhs ~ orhs
            -> TcS (StopOrContinue Ct)
-- LHS is an inert type variable, 
-- and RHS is fully rewritten, but with type synonyms
-- preserved as much as possible

canEqTyVar2 dflags ev swapped tv1 xi2 co2
  | Just tv2 <- getTyVar_maybe xi2
  = canEqTyVarTyVar ev swapped tv1 tv2 co2

  | OC_OK xi2' <- occurCheckExpand dflags tv1 xi2  -- No occurs check
  = do { mb <- rewriteEqEvidence ev swapped xi1 xi2' co1 co2
                -- Ensure that the new goal has enough type synonyms
                -- expanded by the occurCheckExpand; hence using xi2' here
                -- See Note [occurCheckExpand]

       ; let k1 = tyVarKind tv1
             k2 = typeKind xi2'
       ; case mb of
            Stop ev s -> return (Stop ev s)
            ContinueWith new_ev 
                | k2 `isSubKind` k1
                -- Establish CTyEqCan kind invariant
                -- Reorientation has done its best, but the kinds might
                -- simply be incompatible
                -> continueWith (CTyEqCan { cc_ev = new_ev
                                          , cc_tyvar  = tv1, cc_rhs = xi2' })
                | otherwise
                -> incompatibleKind new_ev xi1 k1 xi2' k2 }

  | otherwise  -- Occurs check error
  = do { mb <- rewriteEqEvidence ev swapped xi1 xi2 co1 co2
       ; case mb of
           Stop ev s           -> return (Stop ev s)
           ContinueWith new_ev -> do { emitInsoluble (mkNonCanonical new_ev)
              -- If we have a ~ [a], it is not canonical, and in particular
              -- we don't want to rewrite existing inerts with it, otherwise
              -- we'd risk divergence in the constraint solver
                                     ; stopWith new_ev "Occurs check" } }
  where
    xi1 = mkTyVarTy tv1
    co1 = mkTcNomReflCo xi1



canEqTyVarTyVar :: CtEvidence       -- tv1 ~ orhs (or orhs ~ tv1, if swapped)
                -> SwapFlag
                -> TcTyVar -> TcTyVar   -- tv2, tv2
                -> TcCoercion           -- tv2 ~ orhs
                -> TcS (StopOrContinue Ct)
-- Both LHS and RHS rewrote to a type variable,
-- If swapped = NotSwapped, then
--     rw_orhs = tv1, rw_olhs = orhs
--     rw_nlhs = tv2, rw_nrhs = xi1
-- See note [Canonical ordering for equality constraints].
canEqTyVarTyVar ev swapped tv1 tv2 co2
  | tv1 == tv2
  = do { when (isWanted ev) $
         ASSERT( tcCoercionRole co2 == Nominal )
         setEvBind (ctev_evar ev) (EvCoercion (maybeSym swapped co2))
       ; stopWith ev "Equal tyvars" }

  | incompat_kind   = incompat
  | isFmvTyVar tv1  = do_fmv swapped            tv1 xi1 xi2 co1 co2
  | isFmvTyVar tv2  = do_fmv (flipSwap swapped) tv2 xi2 xi1 co2 co1
  | same_kind       = if swap_over then do_swap else no_swap
  | k1_sub_k2       = do_swap   -- Note [Kind orientation for CTyEqCan]
  | otherwise       = no_swap   -- k2_sub_k1
  where
    no_swap = canon_eq swapped            tv1 xi1 xi2 co1 co2
    do_swap = canon_eq (flipSwap swapped) tv2 xi2 xi1 co2 co1

    do_fmv swapped tv1 xi1 xi2 co1 co2
      = canon_eq swapped tv1 xi1 xi2 co1 co2
--      | same_kind = canon_eq swapped tv1 xi1 xi2 co1 co2
--      | otherwise  -- Presumably tv1 `subKind` tv2, which is the wrong way round
--      = do { tv_ty <- newFlexiTcsTy (tyVarKind tv1)

    canon_eq swapped tv1 xi1 xi2 co1 co2
      = do { mb <- rewriteEqEvidence ev swapped xi1 xi2 co1 co2
           ; let mk_ct ev' = CTyEqCan { cc_ev = ev', cc_tyvar = tv1, cc_rhs = xi2 }
           ; return (fmap mk_ct mb) }

    incompat
      = do { mb <- rewriteEqEvidence ev swapped xi1 xi2 (mkTcNomReflCo xi1) co2
           ; case mb of
               Stop ev s        -> return (Stop ev s)
               ContinueWith ev' -> incompatibleKind ev' xi1 k1 xi2 k2 }

    xi1 = mkTyVarTy tv1
    xi2 = mkTyVarTy tv2
    k1  = tyVarKind tv1
    k2  = tyVarKind tv2
    co1 = mkTcNomReflCo xi1
    k1_sub_k2     = k1 `isSubKind` k2
    k2_sub_k1     = k2 `isSubKind` k1
    same_kind     = k1_sub_k2 && k2_sub_k1
    incompat_kind = not (k1_sub_k2 || k2_sub_k1)

    swap_over
      -- If tv1 is touchable, swap only if tv2 is also
      -- touchable and it's strictly better to update the latter
      -- But see Note [Avoid unnecessary swaps]
      | Just lvl1 <- metaTyVarUntouchables_maybe tv1
      = case metaTyVarUntouchables_maybe tv2 of
          Nothing   -> False
          Just lvl2 | lvl2 `strictlyDeeperThan` lvl1 -> True
                    | lvl1 `strictlyDeeperThan` lvl2 -> False
                    | otherwise                      -> nicer_to_update_tv2

      -- So tv1 is not a meta tyvar
      -- If only one is a meta tyvar, put it on the left
      -- This is not because it'll be solved; but becuase
      -- the floating step looks for meta tyvars on the left
      | isMetaTyVar tv2 = True

      -- So neither is a meta tyvar

      -- If only one is a flatten tyvar, put it on the left
      -- See Note [Eliminate flat-skols]
      | not (isFlattenTyVar tv1), isFlattenTyVar tv2 = True

      | otherwise = False

    nicer_to_update_tv2
      =  (isSigTyVar tv1                 && not (isSigTyVar tv2))
      || (isSystemName (Var.varName tv2) && not (isSystemName (Var.varName tv1)))

incompatibleKind :: CtEvidence         -- t1~t2
                 -> TcType -> TcKind
                 -> TcType -> TcKind   -- s1~s2, flattened and zonked
                 -> TcS (StopOrContinue Ct)
-- LHS and RHS have incompatible kinds, so emit an "irreducible" constraint
--       CIrredEvCan (NOT CTyEqCan or CFunEqCan)
-- for the type equality; and continue with the kind equality constraint.
-- When the latter is solved, it'll kick out the irreducible equality for
-- a second attempt at solving
--
-- See Note [Equalities with incompatible kinds]

incompatibleKind new_ev s1 k1 s2 k2   -- See Note [Equalities with incompatible kinds]
  = ASSERT( isKind k1 && isKind k2 )
    do { traceTcS "canEqLeaf: incompatible kinds" (vcat [ppr k1, ppr k2])

         -- Create a derived kind-equality, and solve it
       ; mw <- newDerived kind_co_loc (mkTcEqPred k1 k2)
       ; case mw of
           Nothing  -> return ()
           Just kev -> emitWorkNC [kev]

         -- Put the not-currently-soluble thing into the inert set
       ; continueWith (CIrredEvCan { cc_ev = new_ev }) }
  where
    loc = ctev_loc new_ev
    kind_co_loc = setCtLocOrigin loc (KindEqOrigin s1 s2 (ctLocOrigin loc))
\end{code}

Note [Avoid unnecessary swaps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we swap without actually improving matters, we can get an infnite loop.
Consider
    work item:  a ~ b
   inert item:  b ~ c
We canonicalise the work-time to (a ~ c).  If we then swap it before
aeding to the inert set, we'll add (c ~ a), and therefore kick out the
inert guy, so we get
   new work item:  b ~ c
   inert item:     c ~ a
And now the cycle just repeats

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
and put it in the inert set, which keeps it out of the way until a
subsequent substitution (on kind variables, say) re-activates it.

NB: it is important that the types s1,s2 are flattened and zonked
    so that their kinds k1, k2 are inert wrt the substitution.  That
    means that they can only become the same if we change the inert
    set, which in turn will kick out the irreducible equality
    E.g. it is WRONG to make an irred (a:k1)~(b:k2)
         if we already have a substitution k1:=k2

NB: it's important that the new CIrredCan goes in the inert set rather
than back into the work list. We used to do the latter, but that led
to an infinite loop when we encountered it again, and put it back in
the work list again.

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
as possible.  Hence the ps_ty1, ps_ty2 argument passed to canEqTyVar.


Note [occurCheckExpand]
~~~~~~~~~~~~~~~~~~~~~~~
There is a subtle point with type synonyms and the occurs check that
takes place for equality constraints of the form tv ~ xi.  As an
example, suppose we have

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
itself, and so on.  See Note [Occurs check expansion] in TcType

