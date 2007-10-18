Normalisation of type terms relative to type instances as well as
normalisation and entailment checking of equality constraints.

\begin{code}
module TcTyFuns (
	tcNormaliseFamInst,

	normaliseGivenEqs, normaliseGivenDicts, 
	normaliseWantedEqs, normaliseWantedDicts,
	solveWantedEqs,
	substEqInDictInsts,
	
        -- errors
        eqInstMisMatch, misMatchMsg,
  ) where


#include "HsVersions.h"

--friends
import TcRnMonad
import TcEnv
import Inst
import TcType
import TcMType

-- GHC
import Coercion
import Type
import TypeRep 	( Type(..) )
import TyCon
import HsSyn
import VarEnv
import Var
import Name
import Bag
import Outputable
import SrcLoc	( Located(..) )
import Maybes

-- standard
import Data.List
import Control.Monad (liftM)
\end{code}


%************************************************************************
%*									*
		Normalisation of types
%*									*
%************************************************************************

Unfold a single synonym family instance and yield the witnessing coercion.
Return 'Nothing' if the given type is either not synonym family instance
or is a synonym family instance that has no matching instance declaration.
(Applies only if the type family application is outermost.)

For example, if we have

  :Co:R42T a :: T [a] ~ :R42T a

then 'T [Int]' unfolds to (:R42T Int, :Co:R42T Int).

\begin{code}
tcUnfoldSynFamInst :: Type -> TcM (Maybe (Type, Coercion))
tcUnfoldSynFamInst (TyConApp tycon tys)
  | not (isOpenSynTyCon tycon)     -- unfold *only* _synonym_ family instances
  = return Nothing
  | otherwise
  = do { -- we only use the indexing arguments for matching, 
         -- not the additional ones
       ; maybeFamInst <- tcLookupFamInst tycon idxTys
       ; case maybeFamInst of
           Nothing                -> return Nothing
           Just (rep_tc, rep_tys) -> return $ Just (mkTyConApp rep_tc tys',
		                                    mkTyConApp coe_tc tys')
             where
               tys'   = rep_tys ++ restTys
               coe_tc = expectJust "TcTyFun.tcUnfoldSynFamInst" 
                                   (tyConFamilyCoercion_maybe rep_tc)
       }
    where
        n                = tyConArity tycon
        (idxTys, restTys) = splitAt n tys
tcUnfoldSynFamInst _other = return Nothing
\end{code}

Normalise 'Type's and 'PredType's by unfolding type family applications where
possible (ie, we treat family instances as a TRS).  Also zonk meta variables.

	tcNormaliseFamInst ty = (co, ty')
	then   co : ty ~ ty'

\begin{code}
tcNormaliseFamInst :: TcType -> TcM (CoercionI, TcType)
tcNormaliseFamInst = tcGenericNormaliseFamInst tcUnfoldSynFamInst

tcNormaliseFamInstPred :: TcPredType -> TcM (CoercionI, TcPredType)
tcNormaliseFamInstPred = tcGenericNormaliseFamInstPred tcUnfoldSynFamInst
\end{code}

An elementary rewrite is a properly oriented equality with associated coercion
that has one of the following two forms:

(1) co :: F t1..tn ~ t
(2) co :: a ~ t         , where t /= F t1..tn and a is a skolem tyvar

NB: We do *not* use equalities of the form a ~ t where a is a meta tyvar as a
reqrite rule.  Instead, such equalities are solved by unification.  This is
essential; cf Note [skolemOccurs loop].

The following functions takes an equality instance and turns it into an
elementary rewrite if possible.

\begin{code}
data Rewrite = Rewrite TcType           -- lhs of rewrite rule
                       TcType           -- rhs of rewrite rule
                       TcType           -- coercion witnessing the rewrite rule

eqInstToRewrite :: Inst -> Maybe (Rewrite, Bool)
                                           -- True iff rewrite swapped equality
eqInstToRewrite inst
  = ASSERT( isEqInst inst )
    go (eqInstLeftTy inst) (eqInstRightTy inst) (eqInstType inst)
  where
    -- look through synonyms
    go ty1 ty2 co | Just ty1' <- tcView ty1 = go ty1' ty2 co
    go ty1 ty2 co | Just ty2' <- tcView ty2 = go ty1 ty2' co

    -- left-to-right rule with type family head
    go ty1@(TyConApp con _) ty2 co 
      | isOpenSynTyCon con
      = Just (Rewrite ty1 ty2 co, False)                     -- not swapped

    -- left-to-right rule with type variable head
    go ty1@(TyVarTy tv) ty2 co 
      | isSkolemTyVar tv
      = Just (Rewrite ty1 ty2 co, False)                     -- not swapped

    -- right-to-left rule with type family head, only after
    -- having checked whether we can work left-to-right
    go ty1 ty2@(TyConApp con _) co 
      | isOpenSynTyCon con
      = Just (Rewrite ty2 ty1 (mkSymCoercion co), True)      -- swapped

    -- right-to-left rule with type variable head, only after 
    -- having checked whether we can work left-to-right 
    go ty1 ty2@(TyVarTy tv) co 
      | isSkolemTyVar tv
      = Just (Rewrite ty2 ty1 (mkSymCoercion co), True)      -- swapped

    -- this equality is not a rewrite rule => ignore
    go _ _ _ = Nothing
\end{code}

Normalise a type relative to an elementary rewrite implied by an EqInst or an
explicitly given elementary rewrite.

\begin{code}
-- Rewrite by EqInst
--   Precondition: the EqInst passes the occurs check
tcEqInstNormaliseFamInst :: Inst -> TcType -> TcM (CoercionI, TcType)
tcEqInstNormaliseFamInst inst ty
  = case eqInstToRewrite inst of
      Just (rewrite, _) -> tcEqRuleNormaliseFamInst rewrite ty
      Nothing           -> return (IdCo, ty)

-- Rewrite by equality rewrite rule
tcEqRuleNormaliseFamInst :: Rewrite                     -- elementary rewrite
                         -> TcType                      -- type to rewrite
                         -> TcM (CoercionI,             -- witnessing coercion
                                 TcType)                -- rewritten type
tcEqRuleNormaliseFamInst (Rewrite pat rhs co) ty
  = tcGenericNormaliseFamInst matchEqRule ty
  where
    matchEqRule sty | pat `tcEqType` sty = return $ Just (rhs, co)
                    | otherwise          = return $ Nothing
\end{code}

Generic normalisation of 'Type's and 'PredType's; ie, walk the type term and
apply the normalisation function gives as the first argument to every TyConApp
and every TyVarTy subterm.

	tcGenericNormaliseFamInst fun ty = (co, ty')
	then   co : ty ~ ty'

This function is (by way of using smart constructors) careful to ensure that
the returned coercion is exactly IdCo (and not some semantically equivalent,
but syntactically different coercion) whenever (ty' `tcEqType` ty).  This
makes it easy for the caller to determine whether the type changed.  BUT
even if we return IdCo, ty' may be *syntactically* different from ty due to
unfolded closed type synonyms (by way of tcCoreView).  In the interest of
good error messages, callers should discard ty' in favour of ty in this case.

\begin{code}
tcGenericNormaliseFamInst :: (TcType -> TcM (Maybe (TcType, Coercion))) 	
                             -- what to do with type functions and tyvars
	                   -> TcType  			-- old type
	                   -> TcM (CoercionI, TcType)	-- (coercion, new type)
tcGenericNormaliseFamInst fun ty
  | Just ty' <- tcView ty = tcGenericNormaliseFamInst fun ty' 
tcGenericNormaliseFamInst fun (TyConApp tyCon tys)
  = do	{ (cois, ntys) <- mapAndUnzipM (tcGenericNormaliseFamInst fun) tys
	; let tycon_coi = mkTyConAppCoI tyCon ntys cois
	; maybe_ty_co <- fun (mkTyConApp tyCon ntys)     -- use normalised args!
	; case maybe_ty_co of
	    -- a matching family instance exists
	    Just (ty', co) ->
	      do { let first_coi = mkTransCoI tycon_coi (ACo co)
		 ; (rest_coi, nty) <- tcGenericNormaliseFamInst fun ty'
		 ; let fix_coi = mkTransCoI first_coi rest_coi
	   	 ; return (fix_coi, nty)
		 }
	    -- no matching family instance exists
	    -- we do not do anything
	    Nothing -> return (tycon_coi, mkTyConApp tyCon ntys)
	}
tcGenericNormaliseFamInst fun (AppTy ty1 ty2)
  = do	{ (coi1,nty1) <- tcGenericNormaliseFamInst fun ty1
	; (coi2,nty2) <- tcGenericNormaliseFamInst fun ty2
	; return (mkAppTyCoI nty1 coi1 nty2 coi2, mkAppTy nty1 nty2)
	}
tcGenericNormaliseFamInst fun (FunTy ty1 ty2)
  = do	{ (coi1,nty1) <- tcGenericNormaliseFamInst fun ty1
	; (coi2,nty2) <- tcGenericNormaliseFamInst fun ty2
	; return (mkFunTyCoI nty1 coi1 nty2 coi2, mkFunTy nty1 nty2)
	}
tcGenericNormaliseFamInst fun (ForAllTy tyvar ty1)
  = do 	{ (coi,nty1) <- tcGenericNormaliseFamInst fun ty1
	; return (mkForAllTyCoI tyvar coi, mkForAllTy tyvar nty1)
	}
tcGenericNormaliseFamInst fun (NoteTy note ty1)
  = do	{ (coi,nty1) <- tcGenericNormaliseFamInst fun ty1
	; return (mkNoteTyCoI note coi, NoteTy note nty1)
	}
tcGenericNormaliseFamInst fun ty@(TyVarTy tv)
  | isTcTyVar tv
  = do	{ traceTc (text "tcGenericNormaliseFamInst" <+> ppr ty)
	; res <- lookupTcTyVar tv
	; case res of
	    DoneTv _ -> 
	      do { maybe_ty' <- fun ty
		 ; case maybe_ty' of
		     Nothing	     -> return (IdCo, ty)
		     Just (ty', co1) -> 
                       do { (coi2, ty'') <- tcGenericNormaliseFamInst fun ty'
			  ; return (ACo co1 `mkTransCoI` coi2, ty'') 
			  }
		 }
	    IndirectTv ty' -> tcGenericNormaliseFamInst fun ty' 
	}
  | otherwise
  = return (IdCo, ty)
tcGenericNormaliseFamInst fun (PredTy predty)
  = do 	{ (coi, pred') <- tcGenericNormaliseFamInstPred fun predty
	; return (coi, PredTy pred') }

---------------------------------
tcGenericNormaliseFamInstPred :: (TcType -> TcM (Maybe (TcType,Coercion)))
	                      -> TcPredType
	                      -> TcM (CoercionI, TcPredType)

tcGenericNormaliseFamInstPred fun (ClassP cls tys) 
  = do { (cois, tys')<- mapAndUnzipM (tcGenericNormaliseFamInst fun) tys
       ; return (mkClassPPredCoI cls tys' cois, ClassP cls tys')
       }
tcGenericNormaliseFamInstPred fun (IParam ipn ty) 
  = do { (coi, ty') <- tcGenericNormaliseFamInst fun ty
       ; return $ (mkIParamPredCoI ipn coi, IParam ipn ty')
       }
tcGenericNormaliseFamInstPred fun (EqPred ty1 ty2) 
  = do { (coi1, ty1') <- tcGenericNormaliseFamInst fun ty1
       ; (coi2, ty2') <- tcGenericNormaliseFamInst fun ty2
       ; return (mkEqPredCoI ty1' coi1 ty2' coi2, EqPred ty1' ty2') }
\end{code}


%************************************************************************
%*									*
\section{Normalisation of equality constraints}
%*									*
%************************************************************************

Note [Inconsistencies in equality constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We guarantee that we raise an error if we discover any inconsistencies (i.e.,
equalities that if presented to the unifer in TcUnify would result in an
error) during normalisation of wanted constraints.  This is especially so that
we don't solve wanted constraints under an inconsistent given set.  In
particular, we don't want to permit signatures, such as

  bad :: (Int ~ Bool => Int) -> a -> a

\begin{code}
normaliseGivenEqs :: [Inst] -> TcM ([Inst], TcM ())
normaliseGivenEqs givens
 = do { traceTc (text "normaliseGivenEqs <-" <+> ppr givens)
      ; (result, deSkolem) <- 
          rewriteToFixedPoint (Just ("(SkolemOccurs)", skolemOccurs))
	    [ ("(ZONK)",    dontRerun $ zonkInsts)
	    , ("(TRIVIAL)", dontRerun $ trivialRule)
	    , ("(DECOMP)",  decompRule)
	    , ("(TOP)",	    topRule)
	    , ("(SUBST)",   substRule)                   -- incl. occurs check
            ] givens
      ; traceTc (text "normaliseGivenEqs ->" <+> ppr result)
      ; return (result, deSkolem)
      }
\end{code}

\begin{code}
normaliseWantedEqs :: [Inst] -> TcM [Inst]
normaliseWantedEqs insts 
  = do { traceTc (text "normaliseWantedEqs <-" <+> ppr insts)
       ; result <- liftM fst $ rewriteToFixedPoint Nothing
		     [ ("(ZONK)",    dontRerun $ zonkInsts)
		     , ("(TRIVIAL)", dontRerun $ trivialRule)
		     , ("(DECOMP)",  decompRule)
		     , ("(TOP)",     topRule)
		     , ("(UNIFY)",   unifyMetaRule)      -- incl. occurs check
		     , ("(SUBST)",   substRule)          -- incl. occurs check
                     ] insts
       ; traceTc (text "normaliseWantedEqs ->" <+> ppr result)
       ; return result
       }
\end{code}


%************************************************************************
%*									*
\section{Solving of wanted constraints with respect to a given set}
%*									*
%************************************************************************

The set of given equalities must have been normalised already.

\begin{code}
solveWantedEqs :: [Inst]        -- givens
	     -> [Inst] 	        -- wanteds
	     -> TcM [Inst]	-- irreducible wanteds
solveWantedEqs givens wanteds 
  = do { traceTc $ text "solveWantedEqs <-" <+> ppr wanteds <+> text "with" <+> 
                   ppr givens
       ; result <- liftM fst $ rewriteToFixedPoint Nothing
                     [ ("(ZONK)",    dontRerun $ zonkInsts)
                     , ("(TRIVIAL)", dontRerun $ trivialRule)
                     , ("(DECOMP)",  decompRule)
                     , ("(TOP)",     topRule)
                     , ("(GIVEN)",   substGivens givens) -- incl. occurs check
                     , ("(UNIFY)",   unifyMetaRule)      -- incl. occurs check
                     ] wanteds
       ; traceTc (text "solveWantedEqs ->" <+> ppr result)
       ; return result
       }
  where
    -- Use `substInst' with every given on all the wanteds.
    substGivens :: [Inst] -> [Inst] -> TcM ([Inst], Bool)		 
    substGivens []     wanteds = return (wanteds, False)
    substGivens (g:gs) wanteds
      = do { (wanteds1, changed1) <- substGivens gs wanteds
	   ; (wanteds2, changed2) <- substInst g wanteds1
	   ; return (wanteds2, changed1 || changed2)
	   }
\end{code}


%************************************************************************
%*									*
\section{Normalisation of non-equality dictionaries}
%*									*
%************************************************************************

\begin{code}
normaliseGivenDicts, normaliseWantedDicts
	:: [Inst] 		-- given equations
	-> [Inst]		-- dictionaries
	-> TcM ([Inst],TcDictBinds)

normaliseGivenDicts  eqs dicts = normalise_dicts eqs dicts False
normaliseWantedDicts eqs dicts = normalise_dicts eqs dicts True

normalise_dicts
	:: [Inst]	-- given equations
	-> [Inst]	-- dictionaries
	-> Bool		-- True <=> the dicts are wanted 
			-- Fals <=> they are given
	-> TcM ([Inst],TcDictBinds)
normalise_dicts given_eqs dicts is_wanted
  = do	{ traceTc $ text "normalise???Dicts <-" <+> ppr dicts <+> 
                    text "with" <+> ppr given_eqs
	; (dicts0, binds0)  <- normaliseInsts is_wanted dicts
	; (dicts1, binds1)  <- substEqInDictInsts given_eqs dicts0
	; let binds01 = binds0 `unionBags` binds1
	; if isEmptyBag binds1
	  then return (dicts1, binds01)
	  else do { (dicts2, binds2) <- normaliseGivenDicts given_eqs dicts1
		  ; return (dicts2, binds01 `unionBags` binds2) } }
\end{code}


%************************************************************************
%*									*
\section{Normalisation rules and iterative rule application}
%*									*
%************************************************************************

We have three kinds of normalising rewrite rules:

(1) Normalisation rules that rewrite a set of insts and return a flag indicating
    whether any changes occurred during rewriting that necessitate re-running
    the current rule set.

(2) Precondition rules that rewrite a set of insts and return a monadic action
    that reverts the effect of preconditioning.

(3) Idempotent normalisation rules that never require re-running the rule set. 

\begin{code}
type RewriteRule     = [Inst] -> TcM ([Inst], Bool)   -- rewrite, maybe re-run
type PrecondRule     = [Inst] -> TcM ([Inst], TcM ()) -- rewrite, revertable
type IdemRewriteRule = [Inst] -> TcM [Inst]           -- rewrite, don't re-run

type NamedRule       = (String, RewriteRule)          -- rule with description
type NamedPreRule    = (String, PrecondRule)          -- precond with desc
\end{code}

Template lifting idempotent rules to full rules (which can be put into a rule
set).

\begin{code}
dontRerun :: IdemRewriteRule -> RewriteRule
dontRerun rule insts = liftM addFalse $ rule insts
  where
    addFalse x = (x, False)
\end{code}

The following function applies a set of rewrite rules until a fixed point is
reached; i.e., none of the `RewriteRule's require re-running the rule set.
Optionally, there may be a pre-conditing rule that is applied before any other
rules are applied and before the rule set is re-run.

The result is the set of rewritten (i.e., normalised) insts and, in case of a
pre-conditing rule, a monadic action that reverts the effects of
pre-conditioning - specifically, this is removing introduced skolems.

\begin{code}
rewriteToFixedPoint :: Maybe NamedPreRule   -- optional preconditioning rule
                    -> [NamedRule]          -- rule set
                    -> [Inst]               -- insts to rewrite
                    -> TcM ([Inst], TcM ())
rewriteToFixedPoint precondRule rules insts
  = completeRewrite (return ()) precondRule insts
  where
    completeRewrite :: TcM () -> Maybe NamedPreRule -> [Inst] 
                    -> TcM ([Inst], TcM ())
    completeRewrite dePrecond (Just (precondName, precond)) insts
      = do { traceTc $ text precondName <+> text " <- " <+> ppr insts
           ; (insts', dePrecond') <- precond insts
           ; traceTc $ text precondName <+> text " -> " <+> ppr insts'
           ; tryRules (dePrecond >> dePrecond') rules insts'
           }
    completeRewrite dePrecond Nothing insts
      = tryRules dePrecond rules insts

    tryRules dePrecond _                    []    = return ([]   , dePrecond)
    tryRules dePrecond []                   insts = return (insts, dePrecond)
    tryRules dePrecond ((name, rule):rules) insts 
      = do { traceTc $ text name <+> text " <- " <+> ppr insts
           ; (insts', rerun) <- rule insts
           ; traceTc $ text name <+> text " -> " <+> ppr insts'
	   ; if rerun then completeRewrite dePrecond precondRule insts'
		      else tryRules dePrecond rules insts'
           }
\end{code}


%************************************************************************
%*									*
\section{Different forms of Inst rewrite rules}
%*									*
%************************************************************************

Splitting of non-terminating given constraints: skolemOccurs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This is a preconditioning rule exclusively applied to given constraints.
Moreover, its rewriting is only temporary, as it is undone by way of
side-effecting mutable type variables after simplification and constraint
entailment has been completed.

This version is an (attempt at, yet unproven, an) *unflattened* version of
the SubstL-Ev completion rule.

The above rule is essential to catch non-terminating rules that cannot be
oriented properly, like 

     F a ~ [G (F a)]
 or even
     a ~ [G a]          , where a is a skolem tyvar

The left-to-right orientiation is not suitable because it does not
terminate. The right-to-left orientation is not suitable because it 
does not have a type-function on the left. This is undesirable because
it would hide information. E.g. assume 

	instance C [x]

then rewriting C [G (F a)] to C (F a) is bad because we cannot now
see that the C [x] instance applies.

The rule also caters for badly-oriented rules of the form:

     F a ~ G (F a)

for which other solutions are possible, but this one will do too.

It's behavior is:

  co : ty1 ~ ty2{F ty1}
     >-->
  co         : ty1 ~ ty2{b}
  sym (F co) : F ty2{b} ~ b
 	where b is a fresh skolem variable

We also cater for the symmetric situation *if* the rule cannot be used as a
left-to-right rewrite rule.

We also return an action (b := ty1) which is used to eliminate b 
after the dust of normalisation with the completed rewrite system
has settled.

A subtle point of this transformation is that both coercions in the results
are strictly speaking incorrect.  However, they are correct again after the
action {B := ty1} has removed the skolem again.  This happens immediately
after constraint entailment has been checked; ie, code outside of the
simplification and entailment checking framework will never see these
temporarily incorrect coercions.

NB: We perform this transformation for multiple occurences of ty1 under one
    or multiple family applications on the left-hand side at once (ie, the
    rule doesn't need to be applied multiple times at a single inst).  As a 
    result we can get two or more insts back.

Note [skolemOccurs loop]
~~~~~~~~~~~~~~~~~~~~~~~~
You might think that under

  type family F a 
  type instance F [a] = [F a]

a signature such as

  foo :: (F [a] ~ a) => a

will get us into a loop.  However, this is *not* the case.  Here is why:

    F [a<sk>] ~ a<sk>

    -->(TOP)

    [F a<sk>] ~ a<sk>

    -->(SkolemOccurs)

    [b<tau>] ~ a<sk>
    F [b<tau>] ~ b<tau>   , with b := F a

    -->(TOP)

    [b<tau>] ~ a<sk>
    [F b<tau>] ~ b<tau>   , with b := F a

At this point (SkolemOccurs) does *not* apply anymore, as 

  [F b<tau>] ~ b<tau>

is not used as a rewrite rule.  The variable b<tau> is not a skolem (cf
eqInstToRewrite). 

(The regression test indexed-types/should_compile/Simple20 checks that the
described property of the system does not change.)

\begin{code}
skolemOccurs :: PrecondRule
skolemOccurs insts
  = do { (instss, undoSkolems) <- mapAndUnzipM oneSkolemOccurs insts
       ; return (concat instss, sequence_ undoSkolems)
       }
  where
    oneSkolemOccurs inst
      = ASSERT( isEqInst inst )
        case eqInstToRewrite inst of
          Just (rewrite, swapped) -> breakRecursion rewrite swapped
          Nothing                 -> return ([inst], return ())
      where
        -- inst is an elementary rewrite rule, check whether we need to break
        -- it up
        breakRecursion (Rewrite pat body _) swapped

          -- skolemOccurs does not apply, leave as is
          | null tysToLiftOut 
          = do { traceTc $ text "oneSkolemOccurs: no tys to lift out"
               ; return ([inst], return ())
               }

          -- recursive occurence of pat in body under a type family application
          | otherwise
          = do { traceTc $ text "oneSkolemOccurs[TLO]:" <+> ppr tysToLiftOut
               ; skTvs <- mapM (newMetaTyVar TauTv . typeKind) tysToLiftOut
               ; let skTvs_tysTLO   = zip skTvs tysToLiftOut
                     insertSkolems = return . replace skTvs_tysTLO
               ; (_, body') <- tcGenericNormaliseFamInst insertSkolems body
               ; inst' <- if swapped then mkEqInst (EqPred body' pat) co
                                     else mkEqInst (EqPred pat body') co
                                     -- ensure to reconstruct the inst in the
                                     -- original orientation
               ; traceTc $ text "oneSkolemOccurs[inst']:" <+> ppr inst'
               ; (insts, undoSk) <- mapAndUnzipM (mkSkolemInst inst') 
                                                 skTvs_tysTLO
               ; return (inst':insts, sequence_ undoSk)
               }
          where
            co  = eqInstCoercion inst

            -- all subtypes that are (1) type family instances and (2) contain
            -- the lhs type as part of the type arguments of the type family
            -- constructor 
            tysToLiftOut = [mkTyConApp tc tys | (tc, tys) <- tyFamInsts body
                                              , any (pat `tcPartOfType`) tys]

            replace :: [(TcTyVar, Type)] -> Type -> Maybe (Type, Coercion)
            replace []                   _  = Nothing
            replace ((skTv, tyTLO):rest) ty 
              | tyTLO `tcEqType` ty         = Just (mkTyVarTy skTv, undefined)
              | otherwise                   = replace rest ty

            -- create the EqInst for the equality determining the skolem and a
            -- TcM action undoing the skolem introduction
            mkSkolemInst inst' (skTv, tyTLO)
              = do { (co, tyLiftedOut) <- tcEqInstNormaliseFamInst inst' tyTLO
                   ; inst <- mkEqInst (EqPred tyLiftedOut (mkTyVarTy skTv)) 
                                      (mkGivenCo $ mkSymCoercion (fromACo co))
                                      -- co /= IdCo due to construction of inst'
                   ; return (inst, writeMetaTyVar skTv tyTLO) 
                   }
\end{code}


Removal of trivial equalities: trivialRule
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The following rules exploits the reflexivity of equality:

  (Trivial)
    g1 : t ~ t
      >-->
    g1 := t

\begin{code}
trivialRule :: IdemRewriteRule
trivialRule insts 
  = liftM catMaybes $ mappM trivial insts
  where
    trivial inst
      | ASSERT( isEqInst inst )
        ty1 `tcEqType` ty2
      = do { eitherEqInst inst
	       (\cotv -> writeMetaTyVar cotv ty1) 
	       (\_    -> return ())
	   ; return Nothing
	   }
      | otherwise
      = return $ Just inst
      where
	 ty1 = eqInstLeftTy  inst
	 ty2 = eqInstRightTy inst
\end{code}


Decomposition of data type constructors: decompRule
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Whenever, the same *data* constructors occurs on both sides of an equality, we
can decompose as in standard unification.

  (Decomp)
    g1 : T cs ~ T ds
      >-->
    g21 : c1 ~ d1, ..., g2n : cn ~ dn
    g1 := T g2s

Works also for the case where T is actually an application of a type family
constructor to a set of types, provided the applications on both sides of the
~ are identical; see also Note [OpenSynTyCon app] in TcUnify.

We guarantee to raise an error for any inconsistent equalities; 
cf Note [Inconsistencies in equality constraints].

\begin{code}
decompRule :: RewriteRule
decompRule insts 
  = do { (insts, changed) <- mapAndUnzipM decomp insts
       ; return (concat insts, or changed)
       }
  where
    decomp inst
      = ASSERT( isEqInst inst )
        go (eqInstLeftTy inst) (eqInstRightTy inst)
      where
        go ty1 ty2		
          | Just ty1' <- tcView ty1 = go ty1' ty2 
          | Just ty2' <- tcView ty2 = go ty1 ty2' 

        go (TyConApp con1 tys1) (TyConApp con2 tys2)
          | con1 == con2 && identicalHead
          = mkArgInsts (mkTyConApp con1) tys1 tys2

          | con1 /= con2 && not (isOpenSynTyCon con1 || isOpenSynTyCon con2)
            -- not matching data constructors (of any flavour) are bad news
          = eqInstMisMatch inst
          where
            n             = tyConArity con1
            (idxTys1, _)  = splitAt n tys1
            (idxTys2, _)  = splitAt n tys2
            identicalHead = not (isOpenSynTyCon con1) ||
                            idxTys1 `tcEqTypes` idxTys2

        go (FunTy fun1 arg1) (FunTy fun2 arg2)
          = mkArgInsts (\[funCo, argCo] -> mkFunTy funCo argCo) [fun1, arg1]
                                                                [fun2, arg2]

	-- Applications need a bit of care!
	-- They can match FunTy and TyConApp, so use splitAppTy_maybe
        go (AppTy s1 t1) ty2
          | Just (s2, t2) <- tcSplitAppTy_maybe ty2
          = mkArgInsts (\[s, t] -> mkAppTy s t) [s1, t1] [s2, t2]

        -- Symmetric case
        go ty1 (AppTy s2 t2)
          | Just (s1, t1) <- tcSplitAppTy_maybe ty1
          = mkArgInsts (\[s, t] -> mkAppTy s t) [s1, t1] [s2, t2]

        -- We already covered all the consistent cases of rigid types on both
        -- sides; so, if we see two rigid types here, we discovered an
        -- inconsistency. 
        go ty1 ty2 
          | isRigid ty1 && isRigid ty2
          = eqInstMisMatch inst

        -- We can neither assert consistency nor inconsistency => defer
        go _ _ = return ([inst], False)

        isRigid (TyConApp con _) = not (isOpenSynTyCon con)
        isRigid (FunTy _ _)      = True
        isRigid (AppTy _ _)      = True
        isRigid _                = False

        -- Create insts for matching argument positions (ie, the bit after
        -- '>-->' in the rule description above)
        mkArgInsts con tys1 tys2
          = do { cos <- eitherEqInst inst
                          -- old_co := Con1 cos
                          (\old_covar ->
                            do { cotvs <- zipWithM newMetaCoVar tys1 tys2
                               ; let cos = map mkTyVarTy cotvs
                               ; writeMetaTyVar old_covar (con cos)
                               ; return $ map mkWantedCo cotvs
                               })
                          -- co_i := Con_i old_co
                          (\old_co -> 
                            return $ map mkGivenCo $
                                         mkRightCoercions (length tys1) old_co)
               ; insts <- zipWithM mkEqInst (zipWith EqPred tys1 tys2) cos
               ; traceTc (text "decomp identicalHead" <+> ppr insts) 
               ; return (insts, not $ null insts) 
               }
\end{code}


Rewriting with type instances: topRule
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We use (toplevel) type instances to normalise both sides of equalities.

  (Top)
    g1 : t ~ s
      >--> co1 :: t ~ t' / co2 :: s ~ s'
    g2 : t' ~ s'
    g1 := co1 * g2 * sym co2

\begin{code}
topRule :: RewriteRule
topRule insts 
  =  do { (insts, changed) <- mapAndUnzipM top insts
	; return (insts, or changed)
	}
  where
    top inst
      = ASSERT( isEqInst inst )
        do { (coi1, ty1') <- tcNormaliseFamInst ty1
	   ; (coi2, ty2') <- tcNormaliseFamInst ty2
	   ; case (coi1, coi2) of
	       (IdCo, IdCo) -> return (inst, False)
	       _            -> 
                 do { wg_co <- 
                       eitherEqInst inst
                         -- old_co = co1 * new_co * sym co2
                         (\old_covar ->
                           do { new_cotv <- newMetaCoVar ty1' ty2'
                              ; let new_co  = mkTyVarTy new_cotv
                                    old_coi = coi1 `mkTransCoI` 
                                              ACo new_co `mkTransCoI` 
                                              (mkSymCoI coi2)
                              ; writeMetaTyVar old_covar (fromACo old_coi)
                              ; return $ mkWantedCo new_cotv
                              })
                         -- new_co = sym co1 * old_co * co2
                         (\old_co -> 
                           return $ 
                             mkGivenCo $ 
                               fromACo $ 
                                 mkSymCoI coi1 `mkTransCoI` 
                                 ACo old_co `mkTransCoI` coi2)	
                   ; new_inst <- mkEqInst (EqPred ty1' ty2') wg_co 
                   ; return (new_inst, True)
                   }
	     }
      where
        ty1 = eqInstLeftTy  inst
	ty2 = eqInstRightTy inst
\end{code}


Rewriting with equalities: substRule
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
From a set of insts, use all insts that can be read as rewrite rules to
rewrite the types in all other insts.

  (Subst)
    g : F c ~ t,
    forall g1 : s1{F c} ~ s2{F c}
      >-->
    g2 : s1{t} ~ s2{t}
    g1 := s1{g} * g2  * sym s2{g}  <=>  g2 := sym s1{g} * g1 * s2{g}

Alternatively, the rewrite rule may have the form (g : a ~ t).

To avoid having to swap rules of the form (g : t ~ F c) and (g : t ~ a),
where t is neither a variable nor a type family application, we use them for
rewriting from right-to-left.  However, it is crucial to only apply rules
from right-to-left if they cannot be used left-to-right.

The workhorse is substInst, which performs an occurs check before actually
using an equality for rewriting.  If the type pattern occurs in the type we
substitute for the pattern, normalisation would diverge.

\begin{code}
substRule :: RewriteRule
substRule insts = tryAllInsts insts []
  where
    -- for every inst check whether it can be used to rewrite the others
    -- (we make an effort to keep the insts in order; it makes debugging
    -- easier)
    tryAllInsts []           triedInsts = return (reverse triedInsts, False)
    tryAllInsts (inst:insts) triedInsts
      = do { (insts', changed) <- substInst inst (reverse triedInsts ++ insts)
           ; if changed then return (insertAt (length triedInsts) inst insts',
                                     True)
                        else tryAllInsts insts (inst:triedInsts)
           }
      where
        insertAt n x xs = let (xs1, xs2) = splitAt n xs
                          in xs1 ++ [x] ++ xs2

-- Use the given inst as a rewrite rule to normalise the insts in the second
-- argument.  Don't do anything if the inst cannot be used as a rewrite rule,
-- but do apply it right-to-left, if possible, and if it cannot be used
-- left-to-right. 
--
substInst :: Inst -> [Inst] -> TcM ([Inst], Bool)
substInst inst insts
  = case eqInstToRewrite inst of
      Just (rewrite, _) -> substEquality rewrite insts
      Nothing           -> return (insts, False)
  where
    substEquality :: Rewrite            -- elementary rewrite
                  -> [Inst]             -- insts to rewrite
                  -> TcM ([Inst], Bool)
    substEquality eqRule@(Rewrite pat rhs _) insts
      | pat `tcPartOfType` rhs      -- occurs check!
      = occurCheckErr pat rhs
      | otherwise
      = do { (insts', changed) <- mapAndUnzipM substOne insts
           ; return (insts', or changed)
           }
      where
        substOne inst
          = ASSERT( isEqInst inst )
            do { (coi1, ty1') <- tcEqRuleNormaliseFamInst eqRule ty1
	       ; (coi2, ty2') <- tcEqRuleNormaliseFamInst eqRule ty2
	       ; case (coi1, coi2) of
		(IdCo, IdCo) -> return (inst, False)
		_            -> 
		  do { gw_co <- 
                         eitherEqInst inst
			   -- old_co := co1 * new_co * sym co2
			   (\old_covar ->
			     do { new_cotv <- newMetaCoVar ty1' ty2'
			        ; let new_co  = mkTyVarTy new_cotv
				      old_coi = coi1 `mkTransCoI` 
                                                ACo new_co `mkTransCoI` 
                                                (mkSymCoI coi2)
				; writeMetaTyVar old_covar (fromACo old_coi)
			        ; return $ mkWantedCo new_cotv
				})
			   -- new_co := sym co1 * old_co * co2
			   (\old_co -> 
                             return $ 
                               mkGivenCo $ 
                                 fromACo $ 
                                   mkSymCoI coi1 `mkTransCoI` 
                                   ACo old_co `mkTransCoI` coi2)
	             ; new_inst <- mkEqInst (EqPred ty1' ty2') gw_co
		     ; return (new_inst, True)
		     }
	       }
	  where 
            ty1 = eqInstLeftTy  inst
            ty2 = eqInstRightTy inst
\end{code}


Instantiate meta variables: unifyMetaRule
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If an equality equates a meta type variable with a type, we simply instantiate
the meta variable.

  (UnifyMeta)
    g : alpha ~ t
      >-->
    alpha := t
    g     := t

Meta variables can only appear in wanted constraints, and this rule should
only be applied to wanted constraints.  We also know that t definitely is
distinct from alpha (as the trivialRule) has been run on the insts beforehand.

NB: We cannot assume that meta tyvars are empty.  They may have been updated
by another inst in the currently processed wanted list.  We need to be very
careful when updateing type variables (see TcUnify.uUnfilledVar), but at least
we know that we have no boxes.  It's unclear that it would be an advantage to
common up the code in TcUnify and the code below.  Firstly, we don't want
calls to TcUnify.defer_unification here, and secondly, TcUnify import the
current module, so we would have to move everything here (Yuk!) or to
TcMType.  Besides, the code here is much simpler due to the lack of boxes.

\begin{code}
unifyMetaRule :: RewriteRule
unifyMetaRule insts 
  = do { (insts', changed) <- mapAndUnzipM unifyMeta insts
       ; return (concat insts', or changed)
       }
  where
    unifyMeta inst
      = ASSERT( isEqInst inst )
        go (eqInstLeftTy inst) (eqInstRightTy inst)
           (fromWantedCo "unifyMetaRule" $ eqInstCoercion inst)
      where
        go ty1 ty2 cotv
          | Just ty1' <- tcView ty1 = go ty1' ty2 cotv
          | Just ty2' <- tcView ty2 = go ty1 ty2' cotv

          | TyVarTy tv1 <- ty1
          , isMetaTyVar tv1         = do { lookupTV <- lookupTcTyVar tv1
                                         ; uMeta False tv1 lookupTV ty2 cotv
                                         }
          | TyVarTy tv2 <- ty2
          , isMetaTyVar tv2         = do { lookupTV <- lookupTcTyVar tv2
                                         ; uMeta True tv2 lookupTV ty1 cotv
                                         }
          | otherwise               = return ([inst], False) 

        -- meta variable has been filled already
        -- => ignore this inst (we'll come around again, after zonking)
        uMeta _swapped _tv (IndirectTv _) _ty _cotv
          = return ([inst], False)

        -- signature skolem meets non-variable type
        -- => cannot update!
        uMeta _swapped _tv (DoneTv (MetaTv (SigTv _) _)) ty _cotv
          | not $ isTyVarTy ty
          = return ([inst], False)

        -- type variable meets type variable
        -- => check that tv2 hasn't been updated yet and choose which to update
	uMeta swapped tv1 (DoneTv details1) (TyVarTy tv2) cotv
	  = do { lookupTV2 <- lookupTcTyVar tv2
               ; case lookupTV2 of
                   IndirectTv ty  -> uMeta swapped tv1 (DoneTv details1) ty cotv
                   DoneTv details2 -> 
                     uMetaVar swapped tv1 details1 tv2 details2 cotv
	       }

        -- updatable meta variable meets non-variable type
        -- => occurs check, monotype check, and kinds match check, then update
	uMeta swapped tv (DoneTv (MetaTv _ ref)) ty cotv
	  = do { mb_ty' <- checkTauTvUpdate tv ty    -- occurs + monotype check
               ; case mb_ty' of
                   Nothing  -> return ([inst], False)  -- tv occurs in faminst
                   Just ty' ->
                     do { checkUpdateMeta swapped tv ref ty'  -- update meta var
	                ; writeMetaTyVar cotv ty'             -- update co var
	                ; return ([], True)
                        }
	       }

        uMeta _ _ _ _ _ = panic "uMeta"

        -- meta variable meets skolem 
        -- => just update
        uMetaVar swapped tv1 (MetaTv _ ref) tv2 (SkolemTv _) cotv
          = do { checkUpdateMeta swapped tv1 ref (mkTyVarTy tv2)
	       ; writeMetaTyVar cotv (mkTyVarTy tv2)
	       ; return ([], True)
	       }

        -- meta variable meets meta variable 
        -- => be clever about which of the two to update 
        --   (from TcUnify.uUnfilledVars minus boxy stuff)
        uMetaVar swapped tv1 (MetaTv info1 ref1) tv2 (MetaTv info2 ref2) cotv
          = do { case (info1, info2) of
                   -- Avoid SigTvs if poss
                   (SigTv _, _      ) | k1_sub_k2 -> update_tv2
                   (_,       SigTv _) | k2_sub_k1 -> update_tv1

                   (_,   _) | k1_sub_k2 -> if k2_sub_k1 && nicer_to_update_tv1
                                           then update_tv1 	-- Same kinds
                                           else update_tv2
                            | k2_sub_k1 -> update_tv1
                            | otherwise -> kind_err
              -- Update the variable with least kind info
              -- See notes on type inference in Kind.lhs
              -- The "nicer to" part only applies if the two kinds are the same,
              -- so we can choose which to do.

	       ; writeMetaTyVar cotv (mkTyVarTy tv2)
	       ; return ([], True)
               }
          where
                -- Kinds should be guaranteed ok at this point
            update_tv1 = updateMeta tv1 ref1 (mkTyVarTy tv2)
            update_tv2 = updateMeta tv2 ref2 (mkTyVarTy tv1)

            kind_err = addErrCtxtM (unifyKindCtxt swapped tv1 (mkTyVarTy tv2)) $
                       unifyKindMisMatch k1 k2

            k1 = tyVarKind tv1
            k2 = tyVarKind tv2
            k1_sub_k2 = k1 `isSubKind` k2
            k2_sub_k1 = k2 `isSubKind` k1

            nicer_to_update_tv1 = isSystemName (Var.varName tv1)
                -- Try to update sys-y type variables in preference to ones
                -- gotten (say) by instantiating a polymorphic function with
                -- a user-written type sig 

        uMetaVar _ _ _ _ _ _ = panic "uMetaVar"
\end{code}


%************************************************************************
%*									*
\section{Normalisation of Insts}
%*									*
%************************************************************************

Normalises a set of dictionaries relative to a set of given equalities (which
are interpreted as rewrite rules).  We only consider given equalities of the
form

  F ts ~ t    or    a ~ t

where F is a type family.

\begin{code}
substEqInDictInsts :: [Inst]    -- given equalities (used as rewrite rules)
                   -> [Inst]    -- dictinaries to be normalised
                   -> TcM ([Inst], TcDictBinds)
substEqInDictInsts eqInsts dictInsts 
  = do { traceTc (text "substEqInDictInst <-" <+> ppr dictInsts)
       ; dictInsts' <- 
           foldlM rewriteWithOneEquality (dictInsts, emptyBag) eqInsts
       ; traceTc (text "substEqInDictInst ->" <+> ppr dictInsts')
       ; return dictInsts'
       }
  where
      -- (1) Given equality of form 'F ts ~ t' or 'a ~ t': use for rewriting
    rewriteWithOneEquality (dictInsts, dictBinds)
                           eqInst@(EqInst {tci_left  = pattern, 
                                           tci_right = target})
      | isOpenSynTyConApp pattern || isTyVarTy pattern
      = do { (dictInsts', moreDictBinds) <- 
               genericNormaliseInsts True {- wanted -} applyThisEq dictInsts
           ; return (dictInsts', dictBinds `unionBags` moreDictBinds)
           }
      where
        applyThisEq = tcGenericNormaliseFamInstPred (return . matchResult)

        -- rewrite in case of an exact match
        matchResult ty | tcEqType pattern ty = Just (target, eqInstType eqInst)
                       | otherwise           = Nothing

      -- (2) Given equality has the wrong form: ignore
    rewriteWithOneEquality (dictInsts, dictBinds) _not_a_rewrite_rule
      = return (dictInsts, dictBinds)
\end{code}


Take a bunch of Insts (not EqInsts), and normalise them wrt the top-level
type-function equations, where

	(norm_insts, binds) = normaliseInsts is_wanted insts

If 'is_wanted'
  = True,  (binds + norm_insts) defines insts       (wanteds)
  = False, (binds + insts)      defines norm_insts  (givens)

Ie, in the case of normalising wanted dictionaries, we use the normalised
dictionaries to define the originally wanted ones.  However, in the case of
given dictionaries, we use the originally given ones to define the normalised
ones. 

\begin{code}
normaliseInsts :: Bool   	  		-- True <=> wanted insts
	       -> [Inst] 			-- wanted or given insts 
	       -> TcM ([Inst], TcDictBinds) 	-- normalised insts and bindings
normaliseInsts isWanted insts 
  = genericNormaliseInsts isWanted tcNormaliseFamInstPred insts

genericNormaliseInsts  :: Bool          	    -- True <=> wanted insts
	               -> (TcPredType -> TcM (CoercionI, TcPredType))  
                                                    -- how to normalise
	               -> [Inst]      		    -- wanted or given insts 
	               -> TcM ([Inst], TcDictBinds) -- normalised insts & binds
genericNormaliseInsts isWanted fun insts
  = do { (insts', binds) <- mapAndUnzipM (normaliseOneInst isWanted fun) insts
       ; return (insts', unionManyBags binds)
       }
  where
    normaliseOneInst isWanted fun
	             dict@(Dict {tci_pred = pred,
                                 tci_loc  = loc})
      = do { traceTc $ text "genericNormaliseInst <-" <+> ppr dict
	   ; (coi, pred') <- fun pred

	   ; case coi of
	       IdCo   -> 
                 do { traceTc $ text "genericNormaliseInst ->" <+> ppr dict
                    ; return (dict, emptyBag)
                    }
                         -- don't use pred' in this case; otherwise, we get
                         -- more unfolded closed type synonyms in error messages
	       ACo co -> 
                 do { -- an inst for the new pred
		    ; dict' <- newDictBndr loc pred'
		      -- relate the old inst to the new one
		      -- target_dict = source_dict `cast` st_co
		    ; let (target_dict, source_dict, st_co) 
			    | isWanted  = (dict,  dict', mkSymCoercion co)
			    | otherwise = (dict', dict,  co)
			      -- we have
                              --   co :: dict ~ dict'
			      -- hence, if isWanted
			      -- 	  dict  = dict' `cast` sym co
			      --        else
			      -- 	  dict' = dict  `cast` co
		          expr      = HsVar $ instToId source_dict
		          cast_expr = HsWrap (WpCo st_co) expr
			  rhs       = L (instLocSpan loc) cast_expr
			  binds     = instToDictBind target_dict rhs
		      -- return the new inst
		    ; traceTc $ text "genericNormaliseInst ->" <+> ppr dict'
                    ; return (dict', binds)
		    }
	   }
	
	-- TOMDO: What do we have to do about ImplicInst, Method, and LitInst??
    normaliseOneInst _isWanted _fun inst
      = do { inst' <- zonkInst inst
	   ; return (inst', emptyBag)
	   }
\end{code}


%************************************************************************
%*									*
\section{Errors}
%*									*
%************************************************************************

The infamous couldn't match expected type soandso against inferred type
somethingdifferent message.

\begin{code}
eqInstMisMatch :: Inst -> TcM a
eqInstMisMatch inst
  = ASSERT( isEqInst inst )
    do { (env, msg) <- misMatchMsg ty_act ty_exp
       ; setErrCtxt ctxt $
           failWithTcM (env, msg)
       }
  where
    ty_act           = eqInstLeftTy  inst
    ty_exp           = eqInstRightTy inst
    InstLoc _ _ ctxt = instLoc       inst

-----------------------
misMatchMsg :: TcType -> TcType -> TcM (TidyEnv, SDoc)
-- Generate the message when two types fail to match,
-- going to some trouble to make it helpful.
-- The argument order is: actual type, expected type
misMatchMsg ty_act ty_exp
  = do	{ env0 <- tcInitTidyEnv
        ; ty_exp <- zonkTcType ty_exp
        ; ty_act <- zonkTcType ty_act
	; (env1, pp_exp, extra_exp) <- ppr_ty env0 ty_exp
	; (env2, pp_act, extra_act) <- ppr_ty env1 ty_act
	; return (env2, 
                  sep [sep [ptext SLIT("Couldn't match expected type") <+> pp_exp, 
			    nest 7 $
                              ptext SLIT("against inferred type") <+> pp_act],
		       nest 2 (extra_exp $$ extra_act)]) }

ppr_ty :: TidyEnv -> TcType -> TcM (TidyEnv, SDoc, SDoc)
ppr_ty env ty
  = do	{ let (env1, tidy_ty) = tidyOpenType env ty
	; (env2, extra) <- ppr_extra env1 tidy_ty
	; return (env2, quotes (ppr tidy_ty), extra) }

-- (ppr_extra env ty) shows extra info about 'ty'
ppr_extra :: TidyEnv -> Type -> TcM (TidyEnv, SDoc)
ppr_extra env (TyVarTy tv)
  | isTcTyVar tv && (isSkolemTyVar tv || isSigTyVar tv)
  = return (env1, pprSkolTvBinding tv1)
  where
    (env1, tv1) = tidySkolemTyVar env tv

ppr_extra env _ty = return (env, empty)		-- Normal case
\end{code}
