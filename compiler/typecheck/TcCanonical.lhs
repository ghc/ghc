\begin{code}
{-# LANGUAGE CPP #-}

module TcCanonical( canonicalize ) where

#include "HsVersions.h"

import TcRnTypes
import TcType
import Type
import Kind
import TcFlatten
import TcSMonad
import TcEvidence
import Class
import TyCon
import TypeRep
import Var
import Name( isSystemName )
import OccName( OccName )
import Outputable
import Control.Monad    ( when )
import DynFlags( DynFlags )
import VarSet

import Util
import BasicTypes
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
             loc          = ctEvLoc flavor
       ; traceTcS "newSCWork/Derived" $ text "impr_theta =" <+> ppr impr_theta
       ; mapM_ (emitNewDerived loc) impr_theta }

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
  | isTypeFamilyTyCon fn1 = can_eq_fam_nc ev NotSwapped fn1 tys1 ty2 ps_ty2
can_eq_nc' ev ty1 ps_ty1 (TyConApp fn2 tys2) _
  | isTypeFamilyTyCon fn2 = can_eq_fam_nc ev IsSwapped fn2 tys2 ty1 ps_ty1

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
           Right (ty1, co1, _) -- co1 :: ty1 ~ tv1
                     -> do { mb <- rewriteEqEvidence ev swapped  ty1 ps_ty2
                                                     co1 (mkTcNomReflCo ps_ty2)
                           ; traceTcS "canEqTyVar2" (vcat [ppr tv1, ppr ty2, ppr swapped, ppr ty1,
                                                           ppUnless (isDerived ev) (ppr co1)])
                           ; case mb of
                               Stop ev s           -> return (Stop ev s)
                               ContinueWith new_ev -> can_eq_nc new_ev ty1 ty1 ty2 ps_ty2 }

           Left tv1' -> do { -- FM_Avoid commented out: see Note [Lazy flattening] in TcFlatten
                             -- let fmode = FE { fe_ev = ev, fe_mode = FM_Avoid tv1' True }
                                 -- Flatten the RHS less vigorously, to avoid gratuitous flattening
                                 -- True <=> xi2 should not itself be a type-function application
                             let fmode = FE { fe_ev = ev, fe_mode = FM_FlattenAll }
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



canEqTyVarTyVar :: CtEvidence           -- tv1 ~ orhs (or orhs ~ tv1, if swapped)
                -> SwapFlag
                -> TcTyVar -> TcTyVar   -- tv2, tv2
                -> TcCoercion           -- tv2 ~ orhs
                -> TcS (StopOrContinue Ct)
-- Both LHS and RHS rewrote to a type variable,
-- If swapped = NotSwapped, then
--     rw_orhs = tv1, rw_olhs = orhs
--     rw_nlhs = tv2, rw_nrhs = xi1
-- See Note [Canonical orientation for tyvar/tyvar equality constraints]
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
    xi1 = mkTyVarTy tv1
    xi2 = mkTyVarTy tv2
    k1  = tyVarKind tv1
    k2  = tyVarKind tv2
    co1 = mkTcNomReflCo xi1
    k1_sub_k2     = k1 `isSubKind` k2
    k2_sub_k1     = k2 `isSubKind` k1
    same_kind     = k1_sub_k2 && k2_sub_k1
    incompat_kind = not (k1_sub_k2 || k2_sub_k1)

    no_swap = canon_eq swapped            tv1 xi1 xi2 co1 co2
    do_swap = canon_eq (flipSwap swapped) tv2 xi2 xi1 co2 co1

    canon_eq swapped tv1 xi1 xi2 co1 co2
        -- ev  : tv1 ~ orhs  (not swapped) or   orhs ~ tv1   (swapped)
        -- co1 : xi1 ~ tv1
        -- co2 : xi2 ~ tv2
      = do { mb <- rewriteEqEvidence ev swapped xi1 xi2 co1 co2
           ; let mk_ct ev' = CTyEqCan { cc_ev = ev', cc_tyvar = tv1, cc_rhs = xi2 }
           ; return (fmap mk_ct mb) }

    -- See Note [Orient equalities with flatten-meta-vars on the left] in TcFlatten
    do_fmv swapped tv1 xi1 xi2 co1 co2
      | same_kind
      = canon_eq swapped tv1 xi1 xi2 co1 co2
      | otherwise  -- Presumably tv1 `subKind` tv2, which is the wrong way round
      = ASSERT2( k1_sub_k2, ppr tv1 $$ ppr tv2 )
        ASSERT2( isWanted ev, ppr ev )  -- Only wanteds have flatten meta-vars
        do { tv_ty <- newFlexiTcSTy (tyVarKind tv1)
           ; new_ev <- newWantedEvVarNC (ctEvLoc ev) (mkTcEqPred tv_ty xi2)
           ; emitWorkNC [new_ev]
           ; canon_eq swapped tv1 xi1 tv_ty co1 (ctEvCoercion new_ev `mkTcTransCo` co2) }

    incompat
      = do { mb <- rewriteEqEvidence ev swapped xi1 xi2 (mkTcNomReflCo xi1) co2
           ; case mb of
               Stop ev s        -> return (Stop ev s)
               ContinueWith ev' -> incompatibleKind ev' xi1 k1 xi2 k2 }

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
       ; emitNewDerived kind_co_loc (mkTcEqPred k1 k2)

         -- Put the not-currently-soluble thing into the inert set
       ; continueWith (CIrredEvCan { cc_ev = new_ev }) }
  where
    loc = ctEvLoc new_ev
    kind_co_loc = setCtLocOrigin loc (KindEqOrigin s1 s2 (ctLocOrigin loc))
\end{code}

Note [Canonical orientation for tyvar/tyvar equality constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we have a ~ b where both 'a' and 'b' are TcTyVars, which way
round should be oriented in the CTyEqCan?  The rules, implemented by
canEqTyVarTyVar, are these

 * If either is a flatten-meta-variables, it goes on the left.

 * If one is a strict sub-kind of the other e.g.
       (alpha::?) ~ (beta::*)
   orient them so RHS is a subkind of LHS.  That way we will replace
   'a' with 'b', correctly narrowing the kind.
   This establishes the subkind invariant of CTyEqCan.

 * Put a meta-tyvar on the left if possible
       alpha[3] ~ r

 * If both are meta-tyvars, put the more touchable one (deepest level
   number) on the left, so there is the best chance of unifying it
        alpha[3] ~ beta[2]

 * If both are meta-tyvars and both at the same level, put a SigTv
   on the right if possible
        alpha[2] ~ beta[2](sig-tv)
   That way, when we unify alpha := beta, we don't lose the SigTv flag.

 * Put a meta-tv with a System Name on the left if possible so it
   gets eliminated (improves error messages)

 * If one is a flatten-skolem, put it on the left so that it is
   substituted out  Note [Elminate flat-skols]
        fsk ~ a

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

