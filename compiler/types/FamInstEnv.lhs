%
% (c) The University of Glasgow 2006
%

FamInstEnv: Type checked family instance declarations

\begin{code}
module FamInstEnv (
        FamInst(..), FamFlavor(..), famInstAxiom,
        famInstsRepTyCons, famInstRepTyCon_maybe, dataFamInstRepTyCon,
        famInstLHS,
        pprFamInst, pprFamInstHdr, pprFamInsts,
        mkSynFamInst, mkDataFamInst, mkImportedFamInst,

        FamInstEnvs, FamInstEnv, emptyFamInstEnv, emptyFamInstEnvs,
        extendFamInstEnv, deleteFromFamInstEnv, extendFamInstEnvList,
        identicalFamInst, famInstEnvElts, familyInstances,

        lookupFamInstEnv, lookupFamInstEnvConflicts, lookupFamInstEnvConflicts',

        -- Normalisation
        topNormaliseType, normaliseType, normaliseTcApp
    ) where

#include "HsVersions.h"

import InstEnv
import Unify
import Type
import TypeRep
import TyCon
import Coercion
import VarSet
import VarEnv
import Name
import UniqFM
import Outputable
import Maybes
import Util
import FastString
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Type checked family instance heads}
%*                                                                      *
%************************************************************************

Note [FamInsts and CoAxioms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* CoAxioms and FamInsts are just like
  DFunIds  and ClsInsts

* A CoAxiom is a System-FC thing: it can relate any two types

* A FamInst is a Haskell source-language thing, corresponding
  to a type/data family instance declaration.
    - The FamInst contains a CoAxiom, which is the evidence
      for the instance

    - The LHS of the CoAxiom is always of form F ty1 .. tyn
      where F is a type family


\begin{code}
data FamInst  -- See Note [FamInsts and CoAxioms]
  = FamInst { fi_axiom  :: CoAxiom      -- The new coercion axiom introduced
                                        -- by this family instance
            , fi_flavor :: FamFlavor

            -- Everything below here is a redundant,
            -- cached version of the two things above
            , fi_fam   :: Name          -- Family name
                -- INVARIANT: fi_fam = name of fi_fam_tc

                -- Used for "rough matching"; same idea as for class instances
                -- See Note [Rough-match field] in InstEnv
            , fi_tcs   :: [Maybe Name]  -- Top of type args
                -- INVARIANT: fi_tcs = roughMatchTcs fi_tys

                -- Used for "proper matching"; ditto
            , fi_tvs    :: TyVarSet     -- Template tyvars for full match
            , fi_fam_tc :: TyCon        -- Family tycon
            , fi_tys    :: [Type]       --   and its arg types
                -- INVARIANT: fi_tvs = coAxiomTyVars fi_axiom
                --            (fi_fam_tc, fi_tys) = coAxiomSplitLHS fi_axiom
            }

data FamFlavor
  = SynFamilyInst         -- A synonym family
  | DataFamilyInst TyCon  -- A data family, with its representation TyCon
\end{code}


\begin{code}
-- Obtain the axiom of a family instance
famInstAxiom :: FamInst -> CoAxiom
famInstAxiom = fi_axiom

famInstLHS :: FamInst -> (TyCon, [Type])
famInstLHS (FamInst { fi_fam_tc = tc, fi_tys = tys }) = (tc, tys)

-- Return the representation TyCons introduced by data family instances, if any
famInstsRepTyCons :: [FamInst] -> [TyCon]
famInstsRepTyCons fis = [tc | FamInst { fi_flavor = DataFamilyInst tc } <- fis]

-- Extracts the TyCon for this *data* (or newtype) instance
famInstRepTyCon_maybe :: FamInst -> Maybe TyCon
famInstRepTyCon_maybe fi
  = case fi_flavor fi of
       DataFamilyInst tycon -> Just tycon
       SynFamilyInst        -> Nothing

dataFamInstRepTyCon :: FamInst -> TyCon
dataFamInstRepTyCon fi
  = case fi_flavor fi of
       DataFamilyInst tycon -> tycon
       SynFamilyInst        -> pprPanic "dataFamInstRepTyCon" (ppr fi)
\end{code}

\begin{code}
instance NamedThing FamInst where
   getName = coAxiomName . fi_axiom

instance Outputable FamInst where
   ppr = pprFamInst

-- Prints the FamInst as a family instance declaration
pprFamInst :: FamInst -> SDoc
pprFamInst famInst
  = hang (pprFamInstHdr famInst)
       2 (vcat [ ifPprDebug (ptext (sLit "Coercion axiom:") <+> ppr ax)
               , ifPprDebug (ptext (sLit "RHS:") <+> ppr (coAxiomRHS ax))
               , ptext (sLit "--") <+> pprDefinedAt (getName famInst)])
  where
    ax = fi_axiom famInst

pprFamInstHdr :: FamInst -> SDoc
pprFamInstHdr (FamInst {fi_axiom = axiom, fi_flavor = flavor})
  = pprTyConSort <+> pp_instance <+> pprHead
  where
    (fam_tc, tys) = coAxiomSplitLHS axiom

    -- For *associated* types, say "type T Int = blah"
    -- For *top level* type instances, say "type instance T Int = blah"
    pp_instance
      | isTyConAssoc fam_tc = empty
      | otherwise           = ptext (sLit "instance")

    pprHead = sep [ ifPprDebug (ptext (sLit "forall")
                       <+> pprTvBndrs (coAxiomTyVars axiom))
                  , pprTypeApp fam_tc tys ]
    pprTyConSort = case flavor of
                     SynFamilyInst        -> ptext (sLit "type")
                     DataFamilyInst tycon
                       | isDataTyCon     tycon -> ptext (sLit "data")
                       | isNewTyCon      tycon -> ptext (sLit "newtype")
                       | isAbstractTyCon tycon -> ptext (sLit "data")
                       | otherwise             -> ptext (sLit "WEIRD") <+> ppr tycon

pprFamInsts :: [FamInst] -> SDoc
pprFamInsts finsts = vcat (map pprFamInst finsts)

-- | Create a coercion identifying a @type@ family instance.
-- It has the form @Co tvs :: F ts ~ R@, where @Co@ is
-- the coercion constructor built here, @F@ the family tycon and @R@ the
-- right-hand side of the type family instance.
mkSynFamInst :: Name       -- ^ Unique name for the coercion tycon
             -> [TyVar]    -- ^ Type parameters of the coercion (@tvs@)
             -> TyCon      -- ^ Family tycon (@F@)
             -> [Type]     -- ^ Type instance (@ts@)
             -> Type       -- ^ Representation tycon (@R@)
             -> FamInst
mkSynFamInst name tvs fam_tc inst_tys rep_ty
  = FamInst { fi_fam    = tyConName fam_tc,
              fi_fam_tc = fam_tc,
              fi_tcs    = roughMatchTcs inst_tys,
              fi_tvs    = mkVarSet tvs,
              fi_tys    = inst_tys,
              fi_flavor = SynFamilyInst,
              fi_axiom  = axiom }
  where
    axiom = CoAxiom { co_ax_unique   = nameUnique name
                    , co_ax_name     = name
                    , co_ax_implicit = False
                    , co_ax_tvs      = tvs
                    , co_ax_lhs      = mkTyConApp fam_tc inst_tys
                    , co_ax_rhs      = rep_ty }

-- | Create a coercion identifying a @data@ or @newtype@ representation type
-- and its family instance.  It has the form @Co tvs :: F ts ~ R tvs@,
-- where @Co@ is the coercion constructor built here, @F@ the family tycon
-- and @R@ the (derived) representation tycon.
mkDataFamInst :: Name         -- ^ Unique name for the coercion tycon
              -> [TyVar]      -- ^ Type parameters of the coercion (@tvs@)
              -> TyCon        -- ^ Family tycon (@F@)
              -> [Type]       -- ^ Type instance (@ts@)
              -> TyCon        -- ^ Representation tycon (@R@)
              -> FamInst
mkDataFamInst name tvs fam_tc inst_tys rep_tc
  = FamInst { fi_fam    = tyConName fam_tc,
              fi_fam_tc = fam_tc,
              fi_tcs    = roughMatchTcs inst_tys,
              fi_tvs    = mkVarSet tvs,
              fi_tys    = inst_tys,
              fi_flavor = DataFamilyInst rep_tc,
              fi_axiom  = axiom }
  where
    axiom = CoAxiom { co_ax_unique   = nameUnique name
                    , co_ax_name     = name
                    , co_ax_implicit = False
                    , co_ax_tvs      = tvs
                    , co_ax_lhs      = mkTyConApp fam_tc inst_tys
                    , co_ax_rhs      = mkTyConApp rep_tc (mkTyVarTys tvs) }

-- Make a family instance representation from the information found in an
-- interface file.  In particular, we get the rough match info from the iface
-- (instead of computing it here).
mkImportedFamInst :: Name               -- Name of the family
                  -> [Maybe Name]       -- Rough match info
                  -> CoAxiom            -- Axiom introduced
                  -> FamInst            -- Resulting family instance
mkImportedFamInst fam mb_tcs axiom
  = FamInst {
      fi_fam    = fam,
      fi_fam_tc = fam_tc,
      fi_tcs    = mb_tcs,
      fi_tvs    = mkVarSet . coAxiomTyVars $ axiom,
      fi_tys    = tys,
      fi_axiom  = axiom,
      fi_flavor = flavor }
  where
     (fam_tc, tys) = coAxiomSplitLHS axiom

         -- Derive the flavor for an imported FamInst rather disgustingly
         -- Maybe we should store it in the IfaceFamInst?
     flavor = case splitTyConApp_maybe (coAxiomRHS axiom) of
                Just (tc, _)
                  | Just ax' <- tyConFamilyCoercion_maybe tc
                  , ax' == axiom
                  -> DataFamilyInst tc
                _ -> SynFamilyInst
\end{code}



%************************************************************************
%*                                                                      *
                FamInstEnv
%*                                                                      *
%************************************************************************

Note [FamInstEnv]
~~~~~~~~~~~~~~~~~~~~~
A FamInstEnv maps a family name to the list of known instances for that family.

The same FamInstEnv includes both 'data family' and 'type family' instances.
Type families are reduced during type inference, but not data families;
the user explains when to use a data family instance by using contructors
and pattern matching.

Neverthless it is still useful to have data families in the FamInstEnv:

 - For finding overlaps and conflicts

 - For finding the representation type...see FamInstEnv.topNormaliseType
   and its call site in Simplify

 - In standalone deriving instance Eq (T [Int]) we need to find the
   representation type for T [Int]

\begin{code}
type FamInstEnv = UniqFM FamilyInstEnv  -- Maps a family to its instances
     -- See Note [FamInstEnv]

type FamInstEnvs = (FamInstEnv, FamInstEnv)
     -- External package inst-env, Home-package inst-env

data FamilyInstEnv
  = FamIE [FamInst]     -- The instances for a particular family, in any order
          Bool          -- True <=> there is an instance of form T a b c
                        --      If *not* then the common case of looking up
                        --      (T a b c) can fail immediately

instance Outputable FamilyInstEnv where
  ppr (FamIE fs b) = ptext (sLit "FamIE") <+> ppr b <+> vcat (map ppr fs)

-- INVARIANTS:
--  * The fs_tvs are distinct in each FamInst
--      of a range value of the map (so we can safely unify them)

emptyFamInstEnvs :: (FamInstEnv, FamInstEnv)
emptyFamInstEnvs = (emptyFamInstEnv, emptyFamInstEnv)

emptyFamInstEnv :: FamInstEnv
emptyFamInstEnv = emptyUFM

famInstEnvElts :: FamInstEnv -> [FamInst]
famInstEnvElts fi = [elt | FamIE elts _ <- eltsUFM fi, elt <- elts]

familyInstances :: (FamInstEnv, FamInstEnv) -> TyCon -> [FamInst]
familyInstances (pkg_fie, home_fie) fam
  = get home_fie ++ get pkg_fie
  where
    get env = case lookupUFM env fam of
                Just (FamIE insts _) -> insts
                Nothing              -> []

extendFamInstEnvList :: FamInstEnv -> [FamInst] -> FamInstEnv
extendFamInstEnvList inst_env fis = foldl extendFamInstEnv inst_env fis

extendFamInstEnv :: FamInstEnv -> FamInst -> FamInstEnv
extendFamInstEnv inst_env ins_item@(FamInst {fi_fam = cls_nm, fi_tcs = mb_tcs})
  = addToUFM_C add inst_env cls_nm (FamIE [ins_item] ins_tyvar)
  where
    add (FamIE items tyvar) _ = FamIE (ins_item:items)
                                      (ins_tyvar || tyvar)
    ins_tyvar = not (any isJust mb_tcs)

deleteFromFamInstEnv :: FamInstEnv -> FamInst -> FamInstEnv
deleteFromFamInstEnv inst_env fam_inst@(FamInst {fi_fam = fam_nm})
 = adjustUFM adjust inst_env fam_nm
 where
   adjust :: FamilyInstEnv -> FamilyInstEnv
   adjust (FamIE items tyvars)
     = FamIE (filterOut (identicalFamInst fam_inst) items) tyvars

identicalFamInst :: FamInst -> FamInst -> Bool
-- Same LHS, *and* the instance is defined in the same module
-- Used for overriding in GHCi
identicalFamInst (FamInst { fi_axiom = ax1 }) (FamInst { fi_axiom = ax2 })
  =  nameModule (coAxiomName ax1) == nameModule (coAxiomName ax2)
  && eqTypeX rn_env (coAxiomLHS ax1) (coAxiomLHS ax2)
  where
     tvs1 = coAxiomTyVars ax1
     tvs2 = coAxiomTyVars ax2
     rn_env = ASSERT( equalLength tvs1 tvs2 )
              rnBndrs2 (mkRnEnv2 emptyInScopeSet) tvs1 tvs2

\end{code}

%************************************************************************
%*                                                                      *
                Looking up a family instance
%*                                                                      *
%************************************************************************

@lookupFamInstEnv@ looks up in a @FamInstEnv@, using a one-way match.
Multiple matches are only possible in case of type families (not data
families), and then, it doesn't matter which match we choose (as the
instances are guaranteed confluent).

We return the matching family instances and the type instance at which it
matches.  For example, if we lookup 'T [Int]' and have a family instance

  data instance T [a] = ..

desugared to

  data :R42T a = ..
  coe :Co:R42T a :: T [a] ~ :R42T a

we return the matching instance '(FamInst{.., fi_tycon = :R42T}, Int)'.

\begin{code}
type FamInstMatch = (FamInst, [Type])           -- Matching type instance
  -- See Note [Over-saturated matches]

lookupFamInstEnv
    :: FamInstEnvs
    -> TyCon -> [Type]          -- What we are looking for
    -> [FamInstMatch]           -- Successful matches
-- Precondition: the tycon is saturated (or over-saturated)

lookupFamInstEnv
   = lookup_fam_inst_env match True
   where
     match _ tpl_tvs tpl_tys tys = tcMatchTys tpl_tvs tpl_tys tys

lookupFamInstEnvConflicts
    :: FamInstEnvs
    -> FamInst          -- Putative new instance
    -> [TyVar]          -- Unique tyvars, matching arity of FamInst
    -> [FamInstMatch]   -- Conflicting matches
-- E.g. when we are about to add
--    f : type instance F [a] = a->a
-- we do (lookupFamInstConflicts f [b])
-- to find conflicting matches
-- The skolem tyvars are needed because we don't have a
-- unique supply to hand
--
-- Precondition: the tycon is saturated (or over-saturated)

lookupFamInstEnvConflicts envs fam_inst skol_tvs
  = lookup_fam_inst_env my_unify False envs fam tys1
  where
    inst_axiom = famInstAxiom fam_inst
    (fam, tys) = famInstLHS fam_inst
    skol_tys   = mkTyVarTys skol_tvs
    tys1       = substTys (zipTopTvSubst (coAxiomTyVars inst_axiom) skol_tys) tys
        -- In example above,   fam tys' = F [b]

    my_unify old_fam_inst tpl_tvs tpl_tys match_tys
       = ASSERT2( tyVarsOfTypes tys1 `disjointVarSet` tpl_tvs,
                  (ppr fam <+> ppr tys1) $$
                  (ppr tpl_tvs <+> ppr tpl_tys) )
                -- Unification will break badly if the variables overlap
                -- They shouldn't because we allocate separate uniques for them
         case tcUnifyTys instanceBindFun tpl_tys match_tys of
              Just subst | conflicting old_fam_inst subst -> Just subst
              _other                                      -> Nothing

      -- Note [Family instance overlap conflicts]
    conflicting old_fam_inst subst
      | isAlgTyCon fam = True
      | otherwise      = not (old_rhs `eqType` new_rhs)
      where
        old_axiom = famInstAxiom old_fam_inst
        old_tvs   = coAxiomTyVars old_axiom
        old_rhs   = mkAxInstRHS old_axiom  (substTyVars subst old_tvs)
        new_rhs   = mkAxInstRHS inst_axiom (substTyVars subst skol_tvs)

-- This variant is called when we want to check if the conflict is only in the
-- home environment (see FamInst.addLocalFamInst)
lookupFamInstEnvConflicts' :: FamInstEnv -> FamInst -> [TyVar] -> [FamInstMatch]
lookupFamInstEnvConflicts' env fam_inst skol_tvs
  = lookupFamInstEnvConflicts (emptyFamInstEnv, env) fam_inst skol_tvs
\end{code}

Note [Family instance overlap conflicts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
- In the case of data family instances, any overlap is fundamentally a
  conflict (as these instances imply injective type mappings).

- In the case of type family instances, overlap is admitted as long as
  the right-hand sides of the overlapping rules coincide under the
  overlap substitution.  eg
       type instance F a Int = a
       type instance F Int b = b
  These two overlap on (F Int Int) but then both RHSs are Int,
  so all is well. We require that they are syntactically equal;
  anything else would be difficult to test for at this stage.


While @lookupFamInstEnv@ uses a one-way match, the next function
@lookupFamInstEnvConflicts@ uses two-way matching (ie, unification).  This is
needed to check for overlapping instances.

For class instances, these two variants of lookup are combined into one
function (cf, @InstEnv@).  We don't do that for family instances as the
results of matching and unification are used in two different contexts.
Moreover, matching is the wildly more frequently used operation in the case of
indexed synonyms and we don't want to slow that down by needless unification.

\begin{code}
------------------------------------------------------------
-- Might be a one-way match or a unifier
type MatchFun =  FamInst                -- The FamInst template
              -> TyVarSet -> [Type]     --   fi_tvs, fi_tys of that FamInst
              -> [Type]                 -- Target to match against
              -> Maybe TvSubst

type OneSidedMatch = Bool     -- Are optimisations that are only valid for
                              -- one sided matches allowed?

lookup_fam_inst_env'          -- The worker, local to this module
    :: MatchFun
    -> OneSidedMatch
    -> FamInstEnv
    -> TyCon -> [Type]          -- What we are looking for
    -> [FamInstMatch]           -- Successful matches
lookup_fam_inst_env' match_fun one_sided ie fam tys
  | not (isFamilyTyCon fam)
  = []
  | otherwise
  = ASSERT2( n_tys >= arity, ppr fam <+> ppr tys )      -- Family type applications must be saturated
    lookup ie
  where
    -- See Note [Over-saturated matches]
    arity = tyConArity fam
    n_tys = length tys
    extra_tys = drop arity tys
    (match_tys, add_extra_tys)
       | arity < n_tys = (take arity tys, \res_tys -> res_tys ++ extra_tys)
       | otherwise     = (tys,            \res_tys -> res_tys)
         -- The second case is the common one, hence functional representation

    --------------
    rough_tcs = roughMatchTcs match_tys
    all_tvs   = all isNothing rough_tcs && one_sided

    --------------
    lookup env = case lookupUFM env fam of
                   Nothing -> []        -- No instances for this class
                   Just (FamIE insts has_tv_insts)
                       -- Short cut for common case:
                       --   The thing we are looking up is of form (C a
                       --   b c), and the FamIE has no instances of
                       --   that form, so don't bother to search
                     | all_tvs && not has_tv_insts -> []
                     | otherwise                   -> find insts

    --------------
    find [] = []
    find (item@(FamInst { fi_tcs = mb_tcs, fi_tvs = tpl_tvs,
                          fi_tys = tpl_tys, fi_axiom = axiom }) : rest)
        -- Fast check for no match, uses the "rough match" fields
      | instanceCantMatch rough_tcs mb_tcs
      = find rest

        -- Proper check
      | Just subst <- match_fun item tpl_tvs tpl_tys match_tys
      = (item, add_extra_tys $ substTyVars subst (coAxiomTyVars axiom)) : find rest

        -- No match => try next
      | otherwise
      = find rest
-- Precondition: the tycon is saturated (or over-saturated)

lookup_fam_inst_env           -- The worker, local to this module
    :: MatchFun
    -> OneSidedMatch
    -> FamInstEnvs
    -> TyCon -> [Type]          -- What we are looking for
    -> [FamInstMatch]           -- Successful matches

-- Precondition: the tycon is saturated (or over-saturated)

lookup_fam_inst_env match_fun one_sided (pkg_ie, home_ie) fam tys =
    lookup_fam_inst_env' match_fun one_sided home_ie fam tys ++
    lookup_fam_inst_env' match_fun one_sided pkg_ie  fam tys

\end{code}

Note [Over-saturated matches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's ok to look up an over-saturated type constructor.  E.g.
     type family F a :: * -> *
     type instance F (a,b) = Either (a->b)

The type instance gives rise to a newtype TyCon (at a higher kind
which you can't do in Haskell!):
     newtype FPair a b = FP (Either (a->b))

Then looking up (F (Int,Bool) Char) will return a FamInstMatch
     (FPair, [Int,Bool,Char])

The "extra" type argument [Char] just stays on the end.




%************************************************************************
%*                                                                      *
                Looking up a family instance
%*                                                                      *
%************************************************************************

\begin{code}
topNormaliseType :: FamInstEnvs
                 -> Type
                 -> Maybe (Coercion, Type)

-- Get rid of *outermost* (or toplevel)
--      * type functions
--      * newtypes
-- using appropriate coercions.
-- By "outer" we mean that toplevelNormaliseType guarantees to return
-- a type that does not have a reducible redex (F ty1 .. tyn) as its
-- outermost form.  It *can* return something like (Maybe (F ty)), where
-- (F ty) is a redex.

-- Its a bit like Type.repType, but handles type families too

topNormaliseType env ty
  = go [] ty
  where
    go :: [TyCon] -> Type -> Maybe (Coercion, Type)
    go rec_nts ty | Just ty' <- coreView ty     -- Expand synonyms
        = go rec_nts ty'

    go rec_nts (TyConApp tc tys)
        | isNewTyCon tc         -- Expand newtypes
        = if tc `elem` rec_nts  -- See Note [Expanding newtypes] in Type.lhs
          then Nothing
          else let nt_co = mkAxInstCo (newTyConCo tc) tys
               in add_co nt_co rec_nts' nt_rhs

        | isFamilyTyCon tc              -- Expand open tycons
        , (co, ty) <- normaliseTcApp env tc tys
                -- Note that normaliseType fully normalises 'tys',
                -- It has do to so to be sure that nested calls like
                --    F (G Int)
                -- are correctly top-normalised
        , not (isReflCo co)
        = add_co co rec_nts ty
        where
          nt_rhs = newTyConInstRhs tc tys
          rec_nts' | isRecursiveTyCon tc = tc:rec_nts
                   | otherwise           = rec_nts

    go _ _ = Nothing

    add_co co rec_nts ty
        = case go rec_nts ty of
                Nothing         -> Just (co, ty)
                Just (co', ty') -> Just (mkTransCo co co', ty')


---------------
normaliseTcApp :: FamInstEnvs -> TyCon -> [Type] -> (Coercion, Type)
normaliseTcApp env tc tys
  | isFamilyTyCon tc
  , tyConArity tc <= length tys    -- Unsaturated data families are possible
  , [(fam_inst, inst_tys)] <- lookupFamInstEnv env tc ntys
  = let    -- A matching family instance exists
        ax              = famInstAxiom fam_inst
        co              = mkAxInstCo  ax inst_tys
        rhs             = mkAxInstRHS ax inst_tys
        first_coi       = mkTransCo tycon_coi co
        (rest_coi,nty)  = normaliseType env rhs
        fix_coi         = mkTransCo first_coi rest_coi
    in
    (fix_coi, nty)

  | otherwise   -- No unique matching family instance exists;
                -- we do not do anything
  = (tycon_coi, TyConApp tc ntys)

  where
        -- Normalise the arg types so that they'll match
        -- when we lookup in in the instance envt
    (cois, ntys) = mapAndUnzip (normaliseType env) tys
    tycon_coi    = mkTyConAppCo tc cois

---------------
normaliseType :: FamInstEnvs            -- environment with family instances
              -> Type                   -- old type
              -> (Coercion, Type)       -- (coercion,new type), where
                                        -- co :: old-type ~ new_type
-- Normalise the input type, by eliminating *all* type-function redexes
-- Returns with Refl if nothing happens

normaliseType env ty
  | Just ty' <- coreView ty = normaliseType env ty'
normaliseType env (TyConApp tc tys)
  = normaliseTcApp env tc tys
normaliseType _env ty@(LitTy {}) = (Refl ty, ty)
normaliseType env (AppTy ty1 ty2)
  = let (coi1,nty1) = normaliseType env ty1
        (coi2,nty2) = normaliseType env ty2
    in  (mkAppCo coi1 coi2, mkAppTy nty1 nty2)
normaliseType env (FunTy ty1 ty2)
  = let (coi1,nty1) = normaliseType env ty1
        (coi2,nty2) = normaliseType env ty2
    in  (mkFunCo coi1 coi2, mkFunTy nty1 nty2)
normaliseType env (ForAllTy tyvar ty1)
  = let (coi,nty1) = normaliseType env ty1
    in  (mkForAllCo tyvar coi, ForAllTy tyvar nty1)
normaliseType _   ty@(TyVarTy _)
  = (Refl ty,ty)
\end{code}
