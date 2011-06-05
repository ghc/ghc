\begin{code}
module TcCanonical(
    mkCanonical, mkCanonicals, mkCanonicalFEV, mkCanonicalFEVs, canWanteds, canGivens,
    canOccursCheck, canEqToWorkList,
    rewriteWithFunDeps
 ) where

#include "HsVersions.h"

import BasicTypes
import Id	( evVarPred )
import TcErrors
import TcRnTypes
import FunDeps
import qualified TcMType as TcM
import TcType
import Type
import Coercion
import Class
import TyCon
import TypeRep
import Name
import Var
import VarEnv		( TidyEnv )
import Outputable
import Control.Monad    ( unless, when, zipWithM, zipWithM_ )
import MonadUtils
import Control.Applicative ( (<|>) )

import VarSet
import Bag

import HsBinds
import TcSMonad
import FastString
\end{code}

Note [Canonicalisation]
~~~~~~~~~~~~~~~~~~~~~~~
* Converts (Constraint f) _which_does_not_contain_proper_implications_ to CanonicalCts
* Unary: treats individual constraints one at a time
* Does not do any zonking
* Lives in TcS monad so that it can create new skolem variables


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

TODO: caching the information about whether transitive synonym
expansions contain any type function applications would speed things
up a bit; right now we waste a lot of energy traversing the same types
multiple times.


\begin{code}

-- Flatten a bunch of types all at once.
flattenMany :: CtFlavor -> [Type] -> TcS ([Xi], [Coercion], CanonicalCts)
-- Coercions :: Xi ~ Type 
flattenMany ctxt tys 
  = do { (xis, cos, cts_s) <- mapAndUnzip3M (flatten ctxt) tys
       ; return (xis, cos, andCCans cts_s) }

-- Flatten a type to get rid of type function applications, returning
-- the new type-function-free type, and a collection of new equality
-- constraints.  See Note [Flattening] for more detail.
flatten :: CtFlavor -> TcType -> TcS (Xi, Coercion, CanonicalCts)
-- Postcondition: Coercion :: Xi ~ TcType 
flatten ctxt ty 
  | Just ty' <- tcView ty
  = do { (xi, co, ccs) <- flatten ctxt ty'
	-- Preserve type synonyms if possible
	-- We can tell if ty' is function-free by
	-- whether there are any floated constraints
        ; if isReflCo co then
             return (ty, mkReflCo ty, emptyCCan)
         else
             return (xi, co, ccs) }

flatten _ v@(TyVarTy _)
  = return (v, mkReflCo v, emptyCCan)

flatten ctxt (AppTy ty1 ty2)
  = do { (xi1,co1,c1) <- flatten ctxt ty1
       ; (xi2,co2,c2) <- flatten ctxt ty2
       ; return (mkAppTy xi1 xi2, mkAppCo co1 co2, c1 `andCCan` c2) }

flatten ctxt (FunTy ty1 ty2)
  = do { (xi1,co1,c1) <- flatten ctxt ty1
       ; (xi2,co2,c2) <- flatten ctxt ty2
       ; return (mkFunTy xi1 xi2, mkFunCo co1 co2, c1 `andCCan` c2) }

flatten fl (TyConApp tc tys)
  -- For a normal type constructor or data family application, we just
  -- recursively flatten the arguments.
  | not (isSynFamilyTyCon tc)
    = do { (xis,cos,ccs) <- flattenMany fl tys
         ; return (mkTyConApp tc xis, mkTyConAppCo tc cos, ccs) }

  -- Otherwise, it's a type function application, and we have to
  -- flatten it away as well, and generate a new given equality constraint
  -- between the application and a newly generated flattening skolem variable.
  | otherwise
  = ASSERT( tyConArity tc <= length tys )	-- Type functions are saturated
      do { (xis, cos, ccs) <- flattenMany fl tys
         ; let (xi_args, xi_rest)  = splitAt (tyConArity tc) xis
               (cos_args, cos_rest) = splitAt (tyConArity tc) cos 
	       	 -- The type function might be *over* saturated
		 -- in which case the remaining arguments should
		 -- be dealt with by AppTys
               fam_ty = mkTyConApp tc xi_args
         ; (ret_co, rhs_var, ct) <-
             do { is_cached <- lookupFlatCacheMap tc xi_args fl 
                ; case is_cached of 
                    Just (rhs_var,ret_co,_fl) -> return (ret_co, rhs_var, emptyCCan)
                    Nothing
                        | isGivenOrSolved fl ->
                            do { rhs_var <- newFlattenSkolemTy fam_ty
                               ; cv <- newGivenCoVar fam_ty rhs_var (mkReflCo fam_ty)
                               ; let ct = CFunEqCan { cc_id     = cv
                                                    , cc_flavor = fl -- Given
                                                    , cc_fun    = tc 
                                                    , cc_tyargs = xi_args 
                                                    , cc_rhs    = rhs_var }
                               ; let ret_co = mkCoVarCo cv 
                               ; updateFlatCacheMap tc xi_args rhs_var fl ret_co 
                               ; return $ (ret_co, rhs_var, singleCCan ct) }
                        | otherwise ->
                    -- Derived or Wanted: make a new *unification* flatten variable
                            do { rhs_var <- newFlexiTcSTy (typeKind fam_ty)
                               ; cv <- newCoVar fam_ty rhs_var
                               ; let ct = CFunEqCan { cc_id = cv
                                                    , cc_flavor = mkWantedFlavor fl
                                                    -- Always Wanted, not Derived
                                                    , cc_fun = tc
                                                    , cc_tyargs = xi_args
                                                    , cc_rhs    = rhs_var }
                               ; let ret_co = mkCoVarCo cv
                               ; updateFlatCacheMap tc xi_args rhs_var fl ret_co
                               ; return $ (ret_co, rhs_var, singleCCan ct) } }
         ; return ( foldl AppTy rhs_var xi_rest
                  , foldl AppCo (mkSymCo ret_co 
                                   `mkTransCo` mkTyConAppCo tc cos_args) 
                                cos_rest
                  , ccs `andCCan` ct) }

flatten ctxt (PredTy pred) 
  = do { (pred', co, ccs) <- flattenPred ctxt pred
       ; return (PredTy pred', co, ccs) }

flatten ctxt ty@(ForAllTy {})
-- We allow for-alls when, but only when, no type function
-- applications inside the forall involve the bound type variables
-- TODO: What if it is a (t1 ~ t2) => t3
--       Must revisit when the New Coercion API is here! 
  = do { let (tvs, rho) = splitForAllTys ty
       ; (rho', co, ccs) <- flatten ctxt rho
       ; let bad_eqs  = filterBag is_bad ccs
             is_bad c = tyVarsOfCanonical c `intersectsVarSet` tv_set
             tv_set   = mkVarSet tvs
       ; unless (isEmptyBag bad_eqs)
                (flattenForAllErrorTcS ctxt ty bad_eqs)
       ; return (mkForAllTys tvs rho', foldr mkForAllCo co tvs, ccs)  }

---------------
flattenPred :: CtFlavor -> TcPredType -> TcS (TcPredType, Coercion, CanonicalCts)
flattenPred ctxt (ClassP cls tys)
  = do { (tys', cos, ccs) <- flattenMany ctxt tys
       ; return (ClassP cls tys', mkPredCo $ ClassP cls cos, ccs) }
flattenPred ctxt (IParam nm ty)
  = do { (ty', co, ccs) <- flatten ctxt ty
       ; return (IParam nm ty', mkPredCo $ IParam nm co, ccs) }
flattenPred ctxt (EqPred ty1 ty2)
  = do { (ty1', co1, ccs1) <- flatten ctxt ty1
       ; (ty2', co2, ccs2) <- flatten ctxt ty2
       ; return (EqPred ty1' ty2', mkPredCo $ EqPred co1 co2, ccs1 `andCCan` ccs2) }
\end{code}

%************************************************************************
%*                                                                      *
%*                Canonicalising given constraints                      *
%*                                                                      *
%************************************************************************

\begin{code}
canWanteds :: [WantedEvVar] -> TcS WorkList
canWanteds = fmap unionWorkLists . mapM (\(EvVarX ev loc) -> mkCanonical (Wanted loc) ev)

canGivens :: GivenLoc -> [EvVar] -> TcS WorkList
canGivens loc givens = do { ccs <- mapM (mkCanonical (Given loc GivenOrig)) givens
                          ; return (unionWorkLists ccs) }

mkCanonicals :: CtFlavor -> [EvVar] -> TcS WorkList
mkCanonicals fl vs = fmap unionWorkLists (mapM (mkCanonical fl) vs)

mkCanonicalFEV :: FlavoredEvVar -> TcS WorkList
mkCanonicalFEV (EvVarX ev fl) = mkCanonical fl ev

mkCanonicalFEVs :: Bag FlavoredEvVar -> TcS WorkList
mkCanonicalFEVs = foldrBagM canon_one emptyWorkList
  where 	-- Preserves order (shouldn't be important, but curently
  		--                  is important for the vectoriser)
    canon_one fev wl = do { wl' <- mkCanonicalFEV fev
                          ; return (unionWorkList wl' wl) }


mkCanonical :: CtFlavor -> EvVar -> TcS WorkList
mkCanonical fl ev = case evVarPred ev of 
                        ClassP clas tys -> canClassToWorkList fl ev clas tys 
                        IParam ip ty    -> canIPToWorkList    fl ev ip ty 
                        EqPred ty1 ty2  -> canEqToWorkList    fl ev ty1 ty2 
                         

canClassToWorkList :: CtFlavor -> EvVar -> Class -> [TcType] -> TcS WorkList
canClassToWorkList fl v cn tys 
  = do { (xis,cos,ccs) <- flattenMany fl tys  -- cos :: xis ~ tys
       ; let no_flattening_happened = all isReflCo cos
             dict_co = mkTyConAppCo (classTyCon cn) cos
       ; v_new <- if no_flattening_happened  then return v
                  else if isGivenOrSolved fl then return v
                         -- The cos are all identities if fl=Given,
                         -- hence nothing to do
                  else do { v' <- newDictVar cn xis  -- D xis
                          ; when (isWanted fl) $ setDictBind v  (EvCast v' dict_co)
                          ; when (isGivenOrSolved fl) $ setDictBind v' (EvCast v (mkSymCo dict_co))
                                 -- NB: No more setting evidence for derived now 
                          ; return v' }

       -- Add the superclasses of this one here, See Note [Adding superclasses]. 
       -- But only if we are not simplifying the LHS of a rule. 
       ; sctx <- getTcSContext
       ; sc_cts <- if simplEqsOnly sctx then return emptyWorkList
                   else newSCWorkFromFlavored v_new fl cn xis

       ; return (sc_cts `unionWorkList` 
                 workListFromEqs ccs `unionWorkList` 
                 workListFromNonEq CDictCan { cc_id     = v_new
                                           , cc_flavor = fl
                                           , cc_class  = cn 
                                           , cc_tyargs = xis }) }
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

newSCWorkFromFlavored :: EvVar -> CtFlavor -> Class -> [Xi] -> TcS WorkList
-- Returns superclasses, see Note [Adding superclasses]
newSCWorkFromFlavored ev orig_flavor cls xis 
  | isDerived orig_flavor 
  = return emptyWorkList  -- Deriveds don't yield more superclasses because we will
                          -- add them transitively in the case of wanteds. 

  | Just gk <- isGiven_maybe orig_flavor 
  = case gk of 
      GivenOrig -> do { let sc_theta = immSuperClasses cls xis 
                            flavor   = orig_flavor
                      ; sc_vars <- mapM newEvVar sc_theta
                      ; _ <- zipWithM_ setEvBind sc_vars [EvSuperClass ev n | n <- [0..]]
                      ; mkCanonicals flavor sc_vars }
      GivenSolved -> return emptyWorkList 
      -- Seems very dangerous to add the superclasses for dictionaries that may be 
      -- partially solved because we may end up with evidence loops.

  | isEmptyVarSet (tyVarsOfTypes xis)
  = return emptyWorkList -- Wanteds with no variables yield no deriveds.
                         -- See Note [Improvement from Ground Wanteds]

  | otherwise -- Wanted case, just add those SC that can lead to improvement. 
  = do { let sc_rec_theta = transSuperClasses cls xis 
             impr_theta   = filter is_improvement_pty sc_rec_theta 
             Wanted wloc  = orig_flavor
       ; der_ids <- mapM newDerivedId impr_theta
       ; mkCanonicals (Derived wloc) der_ids }


is_improvement_pty :: PredType -> Bool 
-- Either it's an equality, or has some functional dependency
is_improvement_pty (EqPred {})      = True 
is_improvement_pty (ClassP cls _ty) = not $ null fundeps
 where (_,fundeps,_,_,_,_) = classExtraBigSig cls
is_improvement_pty _ = False




canIPToWorkList :: CtFlavor -> EvVar -> IPName Name -> TcType -> TcS WorkList
-- See Note [Canonical implicit parameter constraints] to see why we don't 
-- immediately canonicalize (flatten) IP constraints. 
canIPToWorkList fl v nm ty 
  = return $ workListFromNonEq (CIPCan { cc_id = v
                                      , cc_flavor = fl
                                      , cc_ip_nm = nm
                                      , cc_ip_ty = ty })

-----------------
canEqToWorkList :: CtFlavor -> EvVar -> Type -> Type -> TcS WorkList
canEqToWorkList fl cv ty1 ty2 = do { cts <- canEq fl cv ty1 ty2 
                         ; return $ workListFromEqs cts }

canEq :: CtFlavor -> EvVar -> Type -> Type -> TcS CanonicalCts 
canEq fl cv ty1 ty2 
  | eqType ty1 ty2	-- Dealing with equality here avoids
    	     	 	-- later spurious occurs checks for a~a
  = do { when (isWanted fl) (setCoBind cv (mkReflCo ty1))
       ; return emptyCCan }

-- If one side is a variable, orient and flatten, 
-- WITHOUT expanding type synonyms, so that we tend to 
-- substitute a ~ Age rather than a ~ Int when @type Age = Int@
canEq fl cv ty1@(TyVarTy {}) ty2 
  = do { untch <- getUntouchables 
       ; canEqLeaf untch fl cv (classify ty1) (classify ty2) }
canEq fl cv ty1 ty2@(TyVarTy {}) 
  = do { untch <- getUntouchables 
       ; canEqLeaf untch fl cv (classify ty1) (classify ty2) }
      -- NB: don't use VarCls directly because tv1 or tv2 may be scolems!

-- Split up an equality between function types into two equalities.
canEq fl cv (FunTy s1 t1) (FunTy s2 t2)
  = do { (argv, resv) <- 
             if isWanted fl then 
                 do { argv <- newCoVar s1 s2 
                    ; resv <- newCoVar t1 t2 
                    ; setCoBind cv $ 
                      mkFunCo (mkCoVarCo argv) (mkCoVarCo resv) 
                    ; return (argv,resv) } 
             else if isGivenOrSolved fl then 
                      let [arg,res] = decomposeCo 2 (mkCoVarCo cv) 
                      in do { argv <- newGivenCoVar s1 s2 arg 
                            ; resv <- newGivenCoVar t1 t2 res
                            ; return (argv,resv) } 

             else -- Derived 
                 do { argv <- newDerivedId (EqPred s1 s2)
                    ; resv <- newDerivedId (EqPred t1 t2)
                    ; return (argv,resv) }

       ; cc1 <- canEq fl argv s1 s2 -- inherit original kinds and locations
       ; cc2 <- canEq fl resv t1 t2
       ; return (cc1 `andCCan` cc2) }

canEq fl cv (TyConApp fn tys) ty2 
  | isSynFamilyTyCon fn, length tys == tyConArity fn
  = do { untch <- getUntouchables 
       ; canEqLeaf untch fl cv (FunCls fn tys) (classify ty2) }
canEq fl cv ty1 (TyConApp fn tys)
  | isSynFamilyTyCon fn, length tys == tyConArity fn
  = do { untch <- getUntouchables 
       ; canEqLeaf untch fl cv (classify ty1) (FunCls fn tys) }

canEq fl cv (TyConApp tc1 tys1) (TyConApp tc2 tys2)
  | isDecomposableTyCon tc1 && isDecomposableTyCon tc2
  , tc1 == tc2
  , length tys1 == length tys2
  = -- Generate equalities for each of the corresponding arguments
    do { argsv 
             <- if isWanted fl then
                    do { argsv <- zipWithM newCoVar tys1 tys2
                       ; setCoBind cv $ 
                         mkTyConAppCo tc1 (map mkCoVarCo argsv)
                       ; return argsv }
                else if isGivenOrSolved fl then
                    let cos = decomposeCo (length tys1) (mkCoVarCo cv)
                    in zipWith3M newGivenCoVar tys1 tys2 cos

                else -- Derived 
                    zipWithM (\t1 t2 -> newDerivedId (EqPred t1 t2)) tys1 tys2

       ; andCCans <$> zipWith3M (canEq fl) argsv tys1 tys2 }

-- See Note [Equality between type applications]
--     Note [Care with type applications] in TcUnify
canEq fl cv ty1 ty2
  | Just (s1,t1) <- tcSplitAppTy_maybe ty1
  , Just (s2,t2) <- tcSplitAppTy_maybe ty2
    = if isWanted fl 
      then do { cv1 <- newCoVar s1 s2 
              ; cv2 <- newCoVar t1 t2 
              ; setCoBind cv $ 
                mkAppCo (mkCoVarCo cv1) (mkCoVarCo cv2) 
              ; cc1 <- canEq fl cv1 s1 s2 
              ; cc2 <- canEq fl cv2 t1 t2 
              ; return (cc1 `andCCan` cc2) } 

      else if isDerived fl 
      then do { cv1 <- newDerivedId (EqPred s1 s2)
              ; cv2 <- newDerivedId (EqPred t1 t2)
              ; cc1 <- canEq fl cv1 s1 s2 
              ; cc2 <- canEq fl cv2 t1 t2 
              ; return (cc1 `andCCan` cc2) } 
      
      else return emptyCCan    -- We cannot decompose given applications
      	   	  	       -- because we no longer have 'left' and 'right'

canEq fl cv s1@(ForAllTy {}) s2@(ForAllTy {})
 | tcIsForAllTy s1, tcIsForAllTy s2, 
   Wanted {} <- fl 
 = canEqFailure fl cv
 | otherwise
 = do { traceTcS "Ommitting decomposition of given polytype equality" (pprEq s1 s2)
      ; return emptyCCan }

-- Finally expand any type synonym applications.
canEq fl cv ty1 ty2 | Just ty1' <- tcView ty1 = canEq fl cv ty1' ty2
canEq fl cv ty1 ty2 | Just ty2' <- tcView ty2 = canEq fl cv ty1 ty2'
canEq fl cv _ _                               = canEqFailure fl cv

canEqFailure :: CtFlavor -> EvVar -> TcS CanonicalCts
canEqFailure fl cv = return (singleCCan (mkFrozenError fl cv))
\end{code}

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

unClassify :: TypeClassifier -> TcType
unClassify (VarCls tv)      = TyVarTy tv
unClassify (FskCls tv) = TyVarTy tv 
unClassify (FunCls fn tys)  = TyConApp fn tys
unClassify (OtherCls ty)    = ty

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
canEqLeaf :: TcsUntouchables 
          -> CtFlavor -> CoVar 
          -> TypeClassifier -> TypeClassifier -> TcS CanonicalCts 
-- Canonicalizing "leaf" equality constraints which cannot be
-- decomposed further (ie one of the types is a variable or
-- saturated type function application).  

  -- Preconditions: 
  --    * one of the two arguments is not OtherCls
  --    * the two types are not equal (looking through synonyms)
canEqLeaf _untch fl cv cls1 cls2 
  | cls1 `re_orient` cls2
  = do { cv' <- if isWanted fl 
                then do { cv' <- newCoVar s2 s1 
                        ; setCoBind cv $ mkSymCo (mkCoVarCo cv') 
                        ; return cv' } 
                else if isGivenOrSolved fl then
                         newGivenCoVar s2 s1 (mkSymCo (mkCoVarCo cv))
                else -- Derived
                    newDerivedId (EqPred s2 s1)
       ; canEqLeafOriented fl cv' cls2 s1 }

  | otherwise
  = do { traceTcS "canEqLeaf" (ppr (unClassify cls1) $$ ppr (unClassify cls2))
       ; canEqLeafOriented fl cv cls1 s2 }
  where
    re_orient = reOrient fl 
    s1 = unClassify cls1  
    s2 = unClassify cls2  

------------------
canEqLeafOriented :: CtFlavor -> CoVar 
                  -> TypeClassifier -> TcType -> TcS CanonicalCts 
-- First argument is not OtherCls
canEqLeafOriented fl cv cls1@(FunCls fn tys1) s2         -- cv : F tys1
  | let k1 = kindAppResult (tyConKind fn) tys1,
    let k2 = typeKind s2, 
    not (k1 `compatKind` k2) -- Establish the kind invariant for CFunEqCan
  = canEqFailure fl cv
    -- Eagerly fails, see Note [Kind errors] in TcInteract

  | otherwise 
  = ASSERT2( isSynFamilyTyCon fn, ppr (unClassify cls1) )
    do { (xis1,cos1,ccs1) <- flattenMany fl tys1 -- Flatten type function arguments
                                                 -- cos1 :: xis1 ~ tys1
       ; (xi2, co2, ccs2) <- flatten fl s2       -- Flatten entire RHS
                                                 -- co2  :: xi2 ~ s2
       ; let ccs = ccs1 `andCCan` ccs2
             no_flattening_happened = all isReflCo (co2:cos1)
       ; cv_new <- if no_flattening_happened  then return cv
                   else if isGivenOrSolved fl then return cv
                   else if isWanted fl then 
                         do { cv' <- newCoVar (unClassify (FunCls fn xis1)) xi2
                                 -- cv' : F xis ~ xi2
                            ; let -- fun_co :: F xis1 ~ F tys1
                                 fun_co = mkTyConAppCo fn cos1
                                 -- want_co :: F tys1 ~ s2
                                 want_co = mkSymCo fun_co
                                           `mkTransCo` mkCoVarCo cv'
                                           `mkTransCo` co2
                            ; setCoBind cv  want_co
                            ; return cv' }
                   else -- Derived 
                       newDerivedId (EqPred (unClassify (FunCls fn xis1)) xi2)

       ; let final_cc = CFunEqCan { cc_id     = cv_new
                                  , cc_flavor = fl
                                  , cc_fun    = fn
                                  , cc_tyargs = xis1 
                                  , cc_rhs    = xi2 }
       ; return $ ccs `extendCCans` final_cc }

-- Otherwise, we have a variable on the left, so call canEqLeafTyVarLeft
canEqLeafOriented fl cv (FskCls tv) s2 
  = canEqLeafTyVarLeft fl cv tv s2 
canEqLeafOriented fl cv (VarCls tv) s2 
  = canEqLeafTyVarLeft fl cv tv s2 
canEqLeafOriented _ cv (OtherCls ty1) ty2 
  = pprPanic "canEqLeaf" (ppr cv $$ ppr ty1 $$ ppr ty2)

canEqLeafTyVarLeft :: CtFlavor -> CoVar -> TcTyVar -> TcType -> TcS CanonicalCts
-- Establish invariants of CTyEqCans 
canEqLeafTyVarLeft fl cv tv s2       -- cv : tv ~ s2
  | not (k1 `compatKind` k2) -- Establish the kind invariant for CTyEqCan
  = canEqFailure fl cv
       -- Eagerly fails, see Note [Kind errors] in TcInteract
  | otherwise
  = do { (xi2, co, ccs2) <- flatten fl s2  -- Flatten RHS   co : xi2 ~ s2
       ; mxi2' <- canOccursCheck fl tv xi2 -- Do an occurs check, and return a possibly
                                           -- unfolded version of the RHS, if we had to 
                                           -- unfold any type synonyms to get rid of tv.
       ; case mxi2' of {
           Nothing   -> canEqFailure fl cv ;
           Just xi2' ->
    do { let no_flattening_happened = isReflCo co
       ; cv_new <- if no_flattening_happened  then return cv
                   else if isGivenOrSolved fl then return cv
                   else if isWanted fl then 
                         do { cv' <- newCoVar (mkTyVarTy tv) xi2'  -- cv' : tv ~ xi2
                            ; setCoBind cv  (mkCoVarCo cv' `mkTransCo` co)
                            ; return cv' }
                   else -- Derived
                       newDerivedId (EqPred (mkTyVarTy tv) xi2')

       ; return $ ccs2 `extendCCans` CTyEqCan { cc_id     = cv_new
                                              , cc_flavor = fl
                                              , cc_tyvar  = tv
                                              , cc_rhs    = xi2' } } } }
  where
    k1 = tyVarKind tv
    k2 = typeKind s2

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
expandAway tv (PredTy pred) 
  = do { pred' <- expandAwayPred tv pred  
       ; return (PredTy pred') }
-- For a type constructor application, first try expanding away the
-- offending variable from the arguments.  If that doesn't work, next
-- see if the type constructor is a type synonym, and if so, expand
-- it and try again.
expandAway tv ty@(TyConApp tc tys)
  = (mkTyConApp tc <$> mapM (expandAway tv) tys) <|> (tcView ty >>= expandAway tv)

expandAwayPred :: TcTyVar -> TcPredType -> Maybe TcPredType 
expandAwayPred tv (ClassP cls tys) 
  = do { tys' <- mapM (expandAway tv) tys; return (ClassP cls tys') } 
expandAwayPred tv (EqPred ty1 ty2)
  = do { ty1' <- expandAway tv ty1
       ; ty2' <- expandAway tv ty2 
       ; return (EqPred ty1' ty2') }
expandAwayPred tv (IParam nm ty) 
  = do { ty' <- expandAway tv ty
       ; return (IParam nm ty') }

                

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


%************************************************************************
%*                                                                      *
%*          Functional dependencies, instantiation of equations
%*                                                                      *
%************************************************************************

When we spot an equality arising from a functional dependency,
we now use that equality (a "wanted") to rewrite the work-item
constraint right away.  This avoids two dangers

 Danger 1: If we send the original constraint on down the pipeline
           it may react with an instance declaration, and in delicate
	   situations (when a Given overlaps with an instance) that
	   may produce new insoluble goals: see Trac #4952

 Danger 2: If we don't rewrite the constraint, it may re-react
           with the same thing later, and produce the same equality
           again --> termination worries.

To achieve this required some refactoring of FunDeps.lhs (nicer
now!).  

\begin{code}
rewriteWithFunDeps :: [Equation]
                   -> [Xi] -> CtFlavor
                   -> TcS (Maybe ([Xi], [Coercion], WorkList))
rewriteWithFunDeps eqn_pred_locs xis fl
 = do { fd_ev_poss <- mapM (instFunDepEqn fl) eqn_pred_locs
      ; let fd_ev_pos :: [(Int,FlavoredEvVar)]
            fd_ev_pos = concat fd_ev_poss
            (rewritten_xis, cos) = unzip (rewriteDictParams fd_ev_pos xis)
      ; fds <- mapM (\(_,fev) -> mkCanonicalFEV fev) fd_ev_pos
      ; let fd_work = unionWorkLists fds
      ; if isEmptyWorkList fd_work 
        then return Nothing
        else return (Just (rewritten_xis, cos, fd_work)) }

instFunDepEqn :: CtFlavor -- Precondition: Only Wanted or Derived
              -> Equation
              -> TcS [(Int, FlavoredEvVar)]
-- Post: Returns the position index as well as the corresponding FunDep equality
instFunDepEqn fl (FDEqn { fd_qtvs = qtvs, fd_eqs = eqs
                        , fd_pred1 = d1, fd_pred2 = d2 })
  = do { let tvs = varSetElems qtvs
       ; tvs' <- mapM instFlexiTcS tvs
       ; let subst = zipTopTvSubst tvs (mkTyVarTys tvs')
       ; mapM (do_one subst) eqs }
  where 
    fl' = case fl of 
             Given {}    -> panic "mkFunDepEqns"
             Wanted  loc -> Wanted  (push_ctx loc)
             Derived loc -> Derived (push_ctx loc)

    push_ctx loc = pushErrCtxt FunDepOrigin (False, mkEqnMsg d1 d2) loc

    do_one subst (FDEq { fd_pos = i, fd_ty_left = ty1, fd_ty_right = ty2 })
       = do { let sty1 = Type.substTy subst ty1
                  sty2 = Type.substTy subst ty2
            ; ev <- newCoVar sty1 sty2
            ; return (i, mkEvVarX ev fl') }

rewriteDictParams :: [(Int,FlavoredEvVar)] -- A set of coercions : (pos, ty' ~ ty)
                  -> [Type]                -- A sequence of types: tys
                  -> [(Type,Coercion)]     -- Returns            : [(ty', co : ty' ~ ty)]
rewriteDictParams param_eqs tys
  = zipWith do_one tys [0..]
  where
    do_one :: Type -> Int -> (Type,Coercion)
    do_one ty n = case lookup n param_eqs of
                    Just wev -> (get_fst_ty wev, mkCoVarCo (evVarOf wev))
                    Nothing  -> (ty,             mkReflCo ty)	-- Identity

    get_fst_ty wev = case evVarOfPred wev of
                          EqPred ty1 _ -> ty1
                          _ -> panic "rewriteDictParams: non equality fundep"

mkEqnMsg :: (TcPredType, SDoc) -> (TcPredType, SDoc) -> TidyEnv
         -> TcM (TidyEnv, SDoc)
mkEqnMsg (pred1,from1) (pred2,from2) tidy_env
  = do  { zpred1 <- TcM.zonkTcPredType pred1
        ; zpred2 <- TcM.zonkTcPredType pred2
	; let { tpred1 = tidyPred tidy_env zpred1
              ; tpred2 = tidyPred tidy_env zpred2 }
	; let msg = vcat [ptext (sLit "When using functional dependencies to combine"),
			  nest 2 (sep [ppr tpred1 <> comma, nest 2 from1]), 
			  nest 2 (sep [ppr tpred2 <> comma, nest 2 from2])]
	; return (tidy_env, msg) }
\end{code}