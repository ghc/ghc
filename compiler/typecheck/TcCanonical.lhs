\begin{code}
module TcCanonical(
    mkCanonical, mkCanonicals, canWanteds, canGivens, canOccursCheck, 
    canEq, canEqLeafTyVarLeft 
 ) where

#include "HsVersions.h"

import BasicTypes 
import Type
import TcRnTypes

import TcType
import TcErrors
import Coercion
import Class
import TyCon
import TypeRep
import Name
import Var
import Outputable
import Control.Monad    ( when, zipWithM )
import MonadUtils
import Control.Applicative ( (<|>) )

import VarSet
import Bag

import Control.Monad  ( unless )
import TcSMonad  -- The TcS Monad 
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

NB: Note that (unlike the OutsideIn(X) draft of 7 May 2010) we are
actually doing the SAME thing here no matter whether we are flattening
a wanted or a given constraint.  In both cases we simply generate some
flattening skolem variables and some extra given constraints; we never
generate actual unification variables or non-identity coercions.
Hopefully this will work, although SPJ had some vague worries about
unification variables from wanted constraints finding their way into
the generated given constraints...?

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
flattenMany :: CtFlavor -> [Type] -> TcS ([Xi], CanonicalCts)
flattenMany ctxt tys 
  = do { (xis, cts_s) <- mapAndUnzipM (flatten ctxt) tys
       ; return (xis, andCCans cts_s) }

-- Flatten a type to get rid of type function applications, returning
-- the new type-function-free type, and a collection of new equality
-- constraints.  See Note [Flattening] for more detail.  This needs to
-- be in the TcS monad so we can generate new flattening skolem
-- variables.
flatten :: CtFlavor -> TcType -> TcS (Xi, CanonicalCts)

flatten ctxt ty 
  | Just ty' <- tcView ty
  = do { (xi, ccs) <- flatten ctxt ty'
	-- Preserve type synonyms if possible
	-- We can tell if t' is function-free by
	-- whether there are any floated constraints
       ; if isEmptyCCan ccs then
             return (ty, emptyCCan)  
         else
             return (xi, ccs) }

flatten _ v@(TyVarTy _)
  = return (v, emptyCCan)

flatten ctxt (AppTy ty1 ty2)
  = do { (xi1,c1) <- flatten ctxt ty1
       ; (xi2,c2) <- flatten ctxt ty2
       ; return (mkAppTy xi1 xi2, c1 `andCCan` c2) }

flatten ctxt (FunTy ty1 ty2)
  = do { (xi1,c1) <- flatten ctxt ty1
       ; (xi2,c2) <- flatten ctxt ty2
       ; return (mkFunTy xi1 xi2, c1 `andCCan` c2) }

flatten fl (TyConApp tc tys)
  -- For a normal type constructor or data family application, we just
  -- recursively flatten the arguments.
  | not (isSynFamilyTyCon tc)
    = do { (xis,ccs) <- flattenMany fl tys
         ; return (mkTyConApp tc xis, ccs) }

  -- Otherwise, it's a type function application, and we have to
  -- flatten it away as well, and generate a new given equality constraint
  -- between the application and a newly generated flattening skolem variable.
  | otherwise
    = ASSERT( tyConArity tc <= length tys )	-- Type functions are saturated
      do { (xis, ccs) <- flattenMany fl tys
         ; let (xi_args, xi_rest) = splitAt (tyConArity tc) xis
	       	 -- The type function might be *over* saturated
		 -- in which case the remaining arguments should
		 -- be dealt with by AppTys
               fam_ty = mkTyConApp tc xi_args 
               fam_co = fam_ty -- identity 

         ; xi_skol <- newFlattenSkolemTy fam_ty
         ; cv <- newGivOrDerCoVar fam_ty xi_skol fam_co 

         ; let ceq_given = CFunEqCan { cc_id     = cv 
                                     , cc_flavor = mkGivenFlavor fl UnkSkol
                                     , cc_fun    = tc 
                                     , cc_tyargs = xi_args 
                                     , cc_rhs    = xi_skol
                                     }
                 -- ceq_given : F xi_args ~ xi_skol

         ; return ( foldl AppTy xi_skol xi_rest
                  , ccs `extendCCans` ceq_given) }

flatten ctxt (PredTy pred) 
  = do { (pred',ccs) <- flattenPred ctxt pred
       ; return (PredTy pred', ccs) }

flatten ctxt ty@(ForAllTy {})
-- We allow for-alls when, but only when, no type function
-- applications inside the forall involve the bound type variables
  = do { let (tvs, rho) = splitForAllTys ty
       ; (rho', ccs) <- flatten ctxt rho
       ; let bad_eqs  = filterBag is_bad ccs
             is_bad c = tyVarsOfCanonical c `intersectsVarSet` tv_set
             tv_set   = mkVarSet tvs
       ; unless (isEmptyBag bad_eqs)
                (flattenForAllErrorTcS ctxt ty bad_eqs)
       ; return (mkForAllTys tvs rho', ccs)  }

---------------
flattenPred :: CtFlavor -> TcPredType -> TcS (TcPredType, CanonicalCts)
flattenPred ctxt (ClassP cls tys)
  = do { (tys', ccs) <- flattenMany ctxt tys
       ; return (ClassP cls tys', ccs) }
flattenPred ctxt (IParam nm ty)
  = do { (ty', ccs) <- flatten ctxt ty
       ; return (IParam nm ty', ccs) }
flattenPred ctxt (EqPred ty1 ty2)
  = do { (ty1', ccs1) <- flatten ctxt ty1
       ; (ty2', ccs2) <- flatten ctxt ty2
       ; return (EqPred ty1' ty2', ccs1 `andCCan` ccs2) }
\end{code}

%************************************************************************
%*                                                                      *
%*                Canonicalising given constraints                      *
%*                                                                      *
%************************************************************************

\begin{code}
canWanteds :: [WantedEvVar] -> TcS CanonicalCts 
canWanteds = fmap andCCans . mapM (\(WantedEvVar ev loc) -> mkCanonical (Wanted loc) ev)

canGivens :: GivenLoc -> [EvVar] -> TcS CanonicalCts
canGivens loc givens = do { ccs <- mapM (mkCanonical (Given loc)) givens
                          ; return (andCCans ccs) }

mkCanonicals :: CtFlavor -> [EvVar] -> TcS CanonicalCts 
mkCanonicals fl vs = fmap andCCans (mapM (mkCanonical fl) vs)

mkCanonical :: CtFlavor -> EvVar -> TcS CanonicalCts 
mkCanonical fl ev = case evVarPred ev of 
                        ClassP clas tys -> canClass fl ev clas tys 
                        IParam ip ty    -> canIP    fl ev ip ty
                        EqPred ty1 ty2  -> canEq    fl ev ty1 ty2 
                         

canClass :: CtFlavor -> EvVar -> Class -> [TcType] -> TcS CanonicalCts 
canClass fl v cn tys 
  = do { (xis,ccs) <- flattenMany fl tys 
       ; return $ ccs `extendCCans` CDictCan { cc_id = v 
                                             , cc_flavor = fl 
                                             , cc_class = cn 
                                             , cc_tyargs = xis } }
canIP :: CtFlavor -> EvVar -> IPName Name -> TcType -> TcS CanonicalCts 
canIP fl v nm ty 
  = return $ singleCCan $ CIPCan { cc_id = v
                                 , cc_flavor = fl
                                 , cc_ip_nm = nm
                                 , cc_ip_ty = ty } 


-----------------
canEq :: CtFlavor -> EvVar -> Type -> Type -> TcS CanonicalCts 
canEq fl cv ty1 ty2 
  | tcEqType ty1 ty2	-- Dealing with equality here avoids
    	     	 	-- later spurious occurs checks for a~a
  = do { when (isWanted fl) (setWantedCoBind cv ty1)
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

canEq fl cv (TyConApp fn tys) ty2 
  | isSynFamilyTyCon fn, length tys == tyConArity fn
  = do { untch <- getUntouchables 
       ; canEqLeaf untch fl cv (FunCls fn tys) (classify ty2) }
canEq fl cv ty1 (TyConApp fn tys)
  | isSynFamilyTyCon fn, length tys == tyConArity fn
  = do { untch <- getUntouchables 
       ; canEqLeaf untch fl cv (classify ty1) (FunCls fn tys) }

canEq fl cv s1 s2
  | Just (t1a,t1b,t1c) <- splitCoPredTy_maybe s1, 
    Just (t2a,t2b,t2c) <- splitCoPredTy_maybe s2
  = do { (v1,v2,v3) <- if isWanted fl then 
                         do { v1 <- newWantedCoVar t1a t2a
                            ; v2 <- newWantedCoVar t1b t2b 
                            ; v3 <- newWantedCoVar t1c t2c 
                            ; let res_co = mkCoPredCo (mkCoVarCoercion v1) 
                                                      (mkCoVarCoercion v2) (mkCoVarCoercion v3)
                            ; setWantedCoBind cv res_co
                            ; return (v1,v2,v3) }
                       else let co_orig = mkCoVarCoercion cv 
                                coa = mkCsel1Coercion co_orig
                                cob = mkCsel2Coercion co_orig
                                coc = mkCselRCoercion co_orig
                            in do { v1 <- newGivOrDerCoVar t1a t2a coa
                                  ; v2 <- newGivOrDerCoVar t1b t2b cob
                                  ; v3 <- newGivOrDerCoVar t1c t2c coc 
                                  ; return (v1,v2,v3) }
       ; cc1 <- canEq fl v1 t1a t2a 
       ; cc2 <- canEq fl v2 t1b t2b 
       ; cc3 <- canEq fl v3 t1c t2c 
       ; return (cc1 `andCCan` cc2 `andCCan` cc3) }


-- Split up an equality between function types into two equalities.
canEq fl cv (FunTy s1 t1) (FunTy s2 t2)
  = do { (argv, resv) <- 
             if isWanted fl then 
                 do { argv <- newWantedCoVar s1 s2 
                    ; resv <- newWantedCoVar t1 t2 
                    ; setWantedCoBind cv $ 
                      mkFunCoercion (mkCoVarCoercion argv) (mkCoVarCoercion resv) 
                    ; return (argv,resv) } 
             else let [arg,res] = decomposeCo 2 (mkCoVarCoercion cv) 
                  in do { argv <- newGivOrDerCoVar s1 s2 arg 
                        ; resv <- newGivOrDerCoVar t1 t2 res
                        ; return (argv,resv) } 
       ; cc1 <- canEq fl argv s1 s2 -- inherit original kinds and locations
       ; cc2 <- canEq fl resv t1 t2
       ; return (cc1 `andCCan` cc2) }

canEq fl cv (PredTy p1) (PredTy p2) = canEqPred p1 p2 
  where canEqPred (IParam n1 t1) (IParam n2 t2) 
          | n1 == n2 
          = if isWanted fl then 
                do { v <- newWantedCoVar t1 t2 
                   ; setWantedCoBind cv $ mkIParamPredCo n1 (mkCoVarCoercion cv)
                   ; canEq fl v t1 t2 } 
            else return emptyCCan -- DV: How to decompose given IP coercions? 

        canEqPred (ClassP c1 tys1) (ClassP c2 tys2) 
          | c1 == c2 
          = if isWanted fl then 
               do { vs <- zipWithM newWantedCoVar tys1 tys2 
                  ; setWantedCoBind cv $ mkClassPPredCo c1 (map mkCoVarCoercion vs) 
                  ; andCCans <$> zipWith3M (canEq fl) vs tys1 tys2
                  }
            else return emptyCCan 
          -- How to decompose given dictionary (and implicit parameter) coercions? 
          -- You may think that the following is right: 
          --    let cos = decomposeCo (length tys1) (mkCoVarCoercion cv) 
          --    in  zipWith3M newGivOrDerCoVar tys1 tys2 cos
          -- But this assumes that the coercion is a type constructor-based 
          -- coercion, and not a PredTy (ClassP cn cos) coercion. So we chose
          -- to not decompose these coercions. We have to get back to this 
          -- when we clean up the Coercion API.

        canEqPred p1 p2 = misMatchErrorTcS fl (mkPredTy p1) (mkPredTy p2) 


canEq fl cv (TyConApp tc1 tys1) (TyConApp tc2 tys2) 
  | isAlgTyCon tc1 && isAlgTyCon tc2
  , tc1 == tc2
  , length tys1 == length tys2
  = -- Generate equalities for each of the corresponding arguments
    do { argsv <- if isWanted fl then
                    do { argsv <- zipWithM newWantedCoVar tys1 tys2
                            ; setWantedCoBind cv $ mkTyConCoercion tc1 (map mkCoVarCoercion argsv)
                            ; return argsv } 
                  else 
                    let cos = decomposeCo (length tys1) (mkCoVarCoercion cv) 
                    in zipWith3M newGivOrDerCoVar tys1 tys2 cos
       ; andCCans <$> zipWith3M (canEq fl) argsv tys1 tys2 }

-- See Note [Equality between type applications]
--     Note [Care with type applications] in TcUnify
canEq fl cv ty1 ty2
  | Just (s1,t1) <- tcSplitAppTy_maybe ty1
  , Just (s2,t2) <- tcSplitAppTy_maybe ty2
    = do { (cv1,cv2) <- 
             if isWanted fl 
             then do { cv1 <- newWantedCoVar s1 s2 
                     ; cv2 <- newWantedCoVar t1 t2 
                     ; setWantedCoBind cv $ 
                       mkAppCoercion (mkCoVarCoercion cv1) (mkCoVarCoercion cv2) 
                     ; return (cv1,cv2) } 
             else let co1 = mkLeftCoercion  $ mkCoVarCoercion cv 
                      co2 = mkRightCoercion $ mkCoVarCoercion cv
                  in do { cv1 <- newGivOrDerCoVar s1 s2 co1 
                        ; cv2 <- newGivOrDerCoVar t1 t2 co2 
                        ; return (cv1,cv2) } 
         ; cc1 <- canEq fl cv1 s1 s2 
         ; cc2 <- canEq fl cv2 t1 t2 
         ; return (cc1 `andCCan` cc2) } 

canEq fl _ s1@(ForAllTy {}) s2@(ForAllTy {})  
 | tcIsForAllTy s1, tcIsForAllTy s2, 
   Wanted {} <- fl 
 = misMatchErrorTcS fl s1 s2 
 | otherwise 
 = do { traceTcS "Ommitting decomposition of given polytype equality" (pprEq s1 s2)
      ; return emptyCCan }

-- Finally expand any type synonym applications.
canEq fl cv ty1 ty2 | Just ty1' <- tcView ty1 = canEq fl cv ty1' ty2
canEq fl cv ty1 ty2 | Just ty2' <- tcView ty2 = canEq fl cv ty1 ty2'
canEq fl _ ty1 ty2 
  = misMatchErrorTcS fl ty1 ty2


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
  | VarCls TcTyVar      -- ^ *Non-flatten-skolem* variable 
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
reOrient :: Untouchables -> TypeClassifier -> TypeClassifier -> Bool	
-- (t1 `reOrient` t2) responds True 
--   iff we should flip to (t2~t1)
-- We try to say False if possible, to minimise evidence generation
--
-- Postcondition: After re-orienting, first arg is not OTherCls
reOrient _untch (OtherCls {}) (FunCls {})   = True
reOrient _untch (OtherCls {}) (FskCls {})   = True
reOrient _untch (OtherCls {}) (VarCls {})   = True
reOrient _untch (OtherCls {}) (OtherCls {}) = panic "reOrient"  -- One must be Var/Fun

reOrient _untch (FunCls {})   (VarCls tv2)  = isMetaTyVar tv2
  -- See Note [No touchables as FunEq RHS] in TcSMonad
reOrient _untch (FunCls {}) _               = False             -- Fun/Other on rhs

reOrient _untch (VarCls tv1) (FunCls {})    = not $ isMetaTyVar tv1
	 -- Put function on the left, *except* if the RHS becomes
	 -- a meta-tyvar; see invariant on CFunEqCan 
	 -- and Note [No touchables as FunEq RHS]

reOrient _untch (VarCls tv1) (FskCls {})    = not $ isMetaTyVar tv1
   -- Put flatten-skolems on the left if possible:
   --   see Note [Loopy Spontaneous Solving, Example 4] in TcInteract

reOrient _untch (VarCls {})  (OtherCls {})  = False
reOrient _untch (VarCls {})  (VarCls {})    = False

reOrient _untch (FskCls {}) (VarCls tv2)    = isMetaTyVar tv2 
      -- See Note [Loopy Spontaneous Solving, Example 4] in TcInteract

reOrient _untch (FskCls {}) (FskCls {})     = False
reOrient _untch (FskCls {}) (FunCls {})     = True 
reOrient _untch (FskCls {}) (OtherCls {})   = False 

------------------
canEqLeaf :: Untouchables 
          -> CtFlavor -> CoVar 
          -> TypeClassifier -> TypeClassifier -> TcS CanonicalCts 
-- Canonicalizing "leaf" equality constraints which cannot be
-- decomposed further (ie one of the types is a variable or
-- saturated type function application).  

  -- Preconditions: 
  --    * one of the two arguments is not OtherCls
  --    * the two types are not equal (looking through synonyms)
canEqLeaf untch fl cv cls1 cls2 
  | cls1 `re_orient` cls2
  = do { cv' <- if isWanted fl 
                then do { cv' <- newWantedCoVar s2 s1 
                        ; setWantedCoBind cv $ mkSymCoercion (mkCoVarCoercion cv') 
                        ; return cv' } 
                else newGivOrDerCoVar s2 s1 (mkSymCoercion (mkCoVarCoercion cv)) 
       ; canEqLeafOriented fl cv' cls2 s1 }

  | otherwise
  = canEqLeafOriented fl cv cls1 s2
  where
    re_orient = reOrient untch 
    s1 = unClassify cls1  
    s2 = unClassify cls2  

------------------
canEqLeafOriented :: CtFlavor -> CoVar 
                  -> TypeClassifier -> TcType -> TcS CanonicalCts 
-- First argument is not OtherCls
canEqLeafOriented fl cv cls1@(FunCls fn tys) s2 
  | let k1 = kindAppResult (tyConKind fn) tys, 
    let k2 = typeKind s2, 
    isGiven fl && not (k1 `compatKind` k2) -- Establish the kind invariant for CFunEqCan
  = kindErrorTcS fl (unClassify cls1) s2   -- Eagerly fails, see Note [Kind errors] in TcInteract
  | otherwise 
  = ASSERT2( isSynFamilyTyCon fn, ppr (unClassify cls1) )
    do { (xis1,ccs1) <- flattenMany fl tys -- flatten type function arguments
       ; (xi2,ccs2)  <- flatten fl s2      -- flatten entire RHS
       ; let final_cc = CFunEqCan { cc_id     = cv 
                                  , cc_flavor = fl 
                                  , cc_fun    = fn
                                  , cc_tyargs = xis1 
                                  , cc_rhs    = xi2 }
       ; return $ ccs1 `andCCan` ccs2 `extendCCans` final_cc }

-- Otherwise, we have a variable on the left, so call canEqLeafTyVarLeft
canEqLeafOriented fl cv (FskCls tv) s2 
  = do { (cc,ccs) <- canEqLeafTyVarLeft fl cv tv s2 
       ; return $ ccs `extendCCans` cc } 
canEqLeafOriented fl cv (VarCls tv) s2 
  = do { (cc,ccs) <- canEqLeafTyVarLeft fl cv tv s2 
       ; return $ ccs `extendCCans` cc } 
canEqLeafOriented _ cv (OtherCls ty1) ty2 
  = pprPanic "canEqLeaf" (ppr cv $$ ppr ty1 $$ ppr ty2)

canEqLeafTyVarLeft :: CtFlavor -> CoVar -> TcTyVar -> TcType -> TcS (CanonicalCt, CanonicalCts)
-- Establish invariants of CTyEqCans 
canEqLeafTyVarLeft fl cv tv s2 
  | isGiven fl && not (k1 `compatKind` k2) -- Establish the kind invariant for CTyEqCan
  = kindErrorTcS fl (mkTyVarTy tv) s2      -- Eagerly fails, see Note [Kind errors] in TcInteract

  | otherwise
  = do { (xi2,ccs2) <- flatten fl s2      -- flatten RHS
       ; xi2' <- canOccursCheck fl tv xi2 -- do an occurs check, and return a possibly 
                                          -- unfolded version of the RHS, if we had to 
                                          -- unfold any type synonyms to get rid of tv.
       ; let final_cc = CTyEqCan { cc_id     = cv 
                                 , cc_flavor = fl
                                 , cc_tyvar  = tv
                                 , cc_rhs    = xi2'
                                 } 
       ; return $ (final_cc, ccs2) }
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
canOccursCheck :: CtFlavor -> TcTyVar -> Xi -> TcS Xi
canOccursCheck gw tv xi 
  | Just xi' <- expandAway tv xi = return xi'
  | otherwise = occursCheckErrorTcS gw tv xi
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
  = mkAppTy <$> expandAway tv ty1 <*> expandAway tv ty2
expandAway tv (FunTy ty1 ty2)
  = mkFunTy <$> expandAway tv ty1 <*> expandAway tv ty2
expandAway _ (ForAllTy {}) = error "blorg"  -- TODO
expandAway _ (PredTy {})   = error "flerg"  -- TODO

-- For a type constructor application, first try expanding away the
-- offending variable from the arguments.  If that doesn't work, next
-- see if the type constructor is a type synonym, and if so, expand
-- it and try again.
expandAway tv ty@(TyConApp tc tys)
    = (mkTyConApp tc <$> mapM (expandAway tv) tys) <|> (tcView ty >>= expandAway tv)
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



