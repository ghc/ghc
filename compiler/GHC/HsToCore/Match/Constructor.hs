
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-warning-flags -Wno-x-data-list-nonempty-unzip #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Pattern-matching constructors
-}

module GHC.HsToCore.Match.Constructor ( matchConFamily, matchPatSyn ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.HsToCore.Match ( match )

import GHC.Hs
import GHC.HsToCore.Binds
import GHC.Core.ConLike
import GHC.Core.Coercion ( MCoercion(..), coercionLKind, mkUnbranchedAxInstCo )
import GHC.Core.DataCon ( dataConTyCon, dataConRepArgBinders )
import GHC.Core.Type ( isNativeIndexType, isManyTy )
import GHC.Core.TyCon ( isNewTyCon, newTyConCo )
import GHC.Tc.Types.Evidence ( isEmptyTcEvBinds, isIdHsWrapper )
import GHC.Tc.Utils.TcType
import GHC.Core.Multiplicity
import GHC.HsToCore.Monad
import GHC.HsToCore.Errors.Types (DsMessage(DsRetainedPackagePatterns))
import GHC.HsToCore.Utils
import GHC.Core ( CoreExpr )
import GHC.Core.Make ( mkCoreLets )
import GHC.Utils.Misc
import GHC.Types.Id
import GHC.Types.Var ( isCoVar )
import GHC.Types.Name.Env
import GHC.Types.FieldLabel ( flSelector )
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import Control.Monad(liftM, unless, join)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

{-
We are confronted with the first column of patterns in a set of
equations, all beginning with constructors from one ``family'' (e.g.,
@[]@ and @:@ make up the @List@ ``family'').  We want to generate the
alternatives for a @Case@ expression.  There are several choices:
\begin{enumerate}
\item
Generate an alternative for every constructor in the family, whether
they are used in this set of equations or not; this is what the Wadler
chapter does.
\begin{description}
\item[Advantages:]
(a)~Simple.  (b)~It may also be that large sparsely-used constructor
families are mainly handled by the code for literals.
\item[Disadvantages:]
(a)~Not practical for large sparsely-used constructor families, e.g.,
the ASCII character set.  (b)~Have to look up a list of what
constructors make up the whole family.
\end{description}

\item
Generate an alternative for each constructor used, then add a default
alternative in case some constructors in the family weren't used.
\begin{description}
\item[Advantages:]
(a)~Alternatives aren't generated for unused constructors.  (b)~The
STG is quite happy with defaults.  (c)~No lookup in an environment needed.
\item[Disadvantages:]
(a)~A spurious default alternative may be generated.
\end{description}

\item
``Do it right:'' generate an alternative for each constructor used,
and add a default alternative if all constructors in the family
weren't used.
\begin{description}
\item[Advantages:]
(a)~You will get cases with only one alternative (and no default),
which should be amenable to optimisation.  Tuples are a common example.
\item[Disadvantages:]
(b)~Have to look up constructor families in TDE (as above).
\end{description}
\end{enumerate}

We are implementing the ``do-it-right'' option for now.  The arguments
to @matchConFamily@ are the same as to @match@; the extra @Int@
returned is the number of constructors in the family.

The function @matchConFamily@ is concerned with this
have-we-used-all-the-constructors? question; the local function
@match_cons_used@ does all the real work.
-}

matchConFamily :: NonEmpty MatchId
               -> Type
               -> NonEmpty (NonEmpty EquationInfoNE)
               -> DsM (MatchResult CoreExpr)
-- Each group of eqns is for a single constructor
matchConFamily (var :| vars) ty groups@(group1 :| other_groups)

  -- Desugar a newtype constructor match using a cast.
  | RealDataCon dc <- con1
  , isNewTyCon (dataConTyCon dc)
  = assertPpr (null other_groups) (ppr con1) $
    matchNewtypeCon var vars ty group1

  -- See Note [Retained package equation scope].
  | RealDataCon dc <- con1
  , not (null (dataConRepArgBinders dc))
  = do { unless (null other_groups) (failWithDs DsRetainedPackagePatterns)
       ; results <- mapM match_retained_row group1
       ; return (foldr1 combineMatchResults results) }

  -- See Note [Retained constructor equation scope].
  | any (any has_native_proof) groups
  = do { results <- mapM match_retained_row (join groups)
       ; return (foldr1 combineMatchResults results) }

  | otherwise
  = do let mult = matchIdMult var
           -- Each variable in the argument list correspond to one column in the
           -- pattern matching equations. Its multiplicity is the context
           -- multiplicity of the pattern. We extract that multiplicity, so that
           -- 'matchOneconLike' knows the context multiplicity, in case it needs
           -- to come up with new variables.
       alts <- mapM (fmap toRealAlt . matchOneConLike vars ty mult) groups
       return (mkDataConCase var ty alts)
  where
    ConPat { pat_con = L _ con1 } = firstPat (NE.head group1)

    -- Compile one equation with its own runtime index binders.
    match_retained_row :: EquationInfoNE -> DsM (MatchResult CoreExpr)
    match_retained_row eqn
      = do { alt <- matchOneConLike vars ty (matchIdMult var) (eqn :| [])
           ; return (mkDataConCase var ty (toRealAlt alt :| [])) }

    -- Identify a checked native constructor pattern with its virtual proof.
    has_native_proof :: EquationInfoNE -> Bool
    has_native_proof eqn
      | ConPat { pat_con = L _ (RealDataCon con)
               , pat_con_ext = ConPatTc { cpt_tvs = [], cpt_dicts = [cv] } }
          <- firstPat eqn
      = isCoVar cv && isNativeIndexType (mkTyConTy (dataConTyCon con))
      | otherwise = False

    toRealAlt alt = case alt_pat alt of
        RealDataCon dcon -> alt{ alt_pat = dcon }
        _ -> panic "matchConFamily: not RealDataCon"

matchNewtypeCon
  :: HasDebugCallStack
  => MatchId -> [MatchId] -> Type
  -> NonEmpty EquationInfoNE
  -> DsM (MatchResult CoreExpr)
matchNewtypeCon var vars ty eqns
  = assertPpr (matchIdType var `eqType` coercionLKind nt_co)
              (ppr var $$ ppr nt_co) $

    -- Push the newtype coercion into the 'MatchId' and continue with
    -- the inner pattern.
    match (var `castMatchId` MCo nt_co : vars) ty
          (NE.toList (field_pat <$> eqns))
  where
    ConPat { pat_con = L _ con1
           , pat_con_ext = ConPatTc { cpt_arg_tys = arg_tys } }
      = firstPat (NE.head eqns)
    dc = case con1 of
      RealDataCon dc' -> dc'
      PatSynCon {}    -> pprPanic "matchNewtypeCon" (ppr con1)

    fld_tys = conLikeInstOrigArgTys con1 arg_tys -- Newtypes have no existentials

    nt_co = mkUnbranchedAxInstCo Representational
              (newTyConCo (dataConTyCon dc)) arg_tys []
      -- nt_co :: N arg_tys ~R# fld_ty

    -- Replace the constructor pattern with its inner field pattern.
    field_pat :: EquationInfoNE -> EquationInfoNE
    field_pat ( EqnMatch { eqn_pat = L _ p, eqn_rest = rest })
      | ConPat
        { pat_con_ext = _con_pat_tc
        , pat_args    = args
        } <- p
      = assertPpr (isTrivialConPatTc _con_pat_tc) (ppr con1) $
        prependPats (conArgPats fld_tys args) rest
    field_pat eqn = pprPanic "matchNewtypeCon" (ppr eqn)

-- | Is this 'ConPatTc' trivial, in that it contains no existentials, no
-- dictionaries or evidence bindings, only identity 'HsWrapper's, etc?
isTrivialConPatTc :: ConPatTc -> Bool
isTrivialConPatTc (ConPatTc _arg_tys tvs dicts binds wrap) =
  -- NB: Use a positional pattern match in order to guarantee that all fields
  --     will continue to be handled if new fields are added.
  and
    [ null tvs
    , null dicts
    , isEmptyTcEvBinds binds
    , isIdHsWrapper wrap
    ]

matchPatSyn :: NonEmpty MatchId
            -> Type
            -> NonEmpty EquationInfoNE
            -> DsM (MatchResult CoreExpr)
matchPatSyn (var :| vars) ty eqns
  = do let mult = matchIdMult var
       alt <- fmap toSynAlt $ matchOneConLike vars ty mult eqns
       return (mkCoSynCaseMatchResult var ty alt)
  where
    toSynAlt alt = case alt_pat alt of
        PatSynCon psyn -> alt{ alt_pat = psyn }
        _ -> panic "matchPatSyn: not PatSynCon"

type ConArgPats = HsConPatDetails GhcTc


{- Note [Retained constructor field scope]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A native index constructor has no existential variables or ordinary evidence.
Its ConPatTc can carry one virtual branch proof in cpt_dicts. That proof can
mention the actual runtime fields, as in n ~# 'S k. Put the fields before this
proof in the Core alternative. Core Lint checks the field identities and the
exact proof type. CoreToStg erases only the trailing virtual proof.

Bare field patterns retain their renamed Unique values through tcPatBndr and
selectConMatchVars. The proof therefore names the actual alternative fields.
Keep the usual existential/evidence/field order for ordinary GADT constructors.
See Note [Retained constructor matches] in GHC.Tc.Gen.Match and the Agda
constructor-context provenance in docs/dependent-haskell/REFERENCES.md.
-}

{- Note [Retained package equation scope]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Each package equation binds its own runtime index Id. Later field types and
the typed RHS refer to that Id. Sharing the first equation's fields with a
later equation through ordinary let aliases does not substitute those types.

Compile each equation with its own constructor case on the original shared
scrutinee. Combine the MatchResults in source order. A failed field pattern
or guard tries the next case. An earlier infallible match discards later rows.
Each case keeps its actual retained source field Ids. selectMatchVar chooses
ordinary fields from their checked outer pattern types. Their nested patterns
use the usual match compiler. No typed-AST or runtime-index alias is required.

Re-casing the shared scrutinee does not repeat its evaluation. The fields stay
lazy until their patterns or RHS demand them. This can generate more cases
than field sharing. Ordinary constructor families retain their current path.
Agda supplies the constructor-telescope and clause-discrimination precedent.
Separate cases preserve Haskell's field demand and source clause order.
See docs/dependent-haskell/REFERENCES.md#source-package-patterns.
-}

{- Note [Retained constructor equation scope]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Several rows can match the same native index constructor. Each row's virtual
proof names that row's actual fields. For example, two S patterns bind distinct
fields k1 and k2 and proofs n ~# 'S k1 and n ~# 'S k2. Ordinary constructor
grouping shares the first row's fields and introduces evidence aliases. Those
aliases do not substitute the runtime Ids in later proof types or RHS types.

Use the separate-row strategy from Note [Retained package equation scope] when
a checked native constructor pattern carries a virtual proof. Compile each row
with its own fields and proof. Keep source order within each constructor group.
Different constructors do not overlap. Each case uses the existing DEFAULT
failure path for other constructors. A failed later column or guard tries the
next row. An infallible row discards later rows for the same constructor.

All cases inspect the same shared scrutinee. Re-casing does not evaluate its
fields. Later pattern columns keep their ordinary demand and failure order.
Ordinary GADT constructors keep the existing shared-field path: the native
index-domain predicate excludes their existentials and equality constraints.
-}

matchOneConLike :: [MatchId]
                -> Type
                -> Mult
                -> NonEmpty EquationInfoNE
                -> DsM (CaseAlt ConLike)
matchOneConLike vars ty mult (eqn1 :| eqns)   -- All eqns for a single constructor
  = do  { retained_vars <- case con1 of
            RealDataCon con | not (null (dataConRepArgBinders con)) ->
              putSrcSpanDsA (getLoc (eqn_pat eqn1)) $
                do { unless (null eqns && isManyTy mult) package_error
                   ; case args1 of
                       PrefixCon _ pats ->
                         Just <$> mapM (selectMatchVar mult . unLoc) pats
                       _ -> package_error }
            _ -> return Nothing
        ; let inst_tys = assert (all tcIsTcTyVar ex_tvs) $
                           -- ex_tvs can only be tyvars as data types in source
                           -- Haskell cannot mention covar yet (Aug 2018).
                         assert (tvs1 `equalLength` ex_tvs) $
                         arg_tys ++ mkTyVarTys tvs1

              val_arg_tys = case retained_vars of
                Just fields -> map (\v -> Scaled (idMult v) (idType v)) fields
                Nothing -> conLikeInstOrigArgTys con1 inst_tys
        -- dataConInstOrigArgTys takes the univ and existential tyvars
        -- and returns the types of the *value* args, which is what we want

              match_group :: [Id]
                          -> NonEmpty (ConArgPats, EquationInfoNE)
                          -> DsM (MatchResult CoreExpr)
              -- All members of the group have compatible ConArgPats
              match_group arg_vars arg_eqn_prs
                = do { (wraps, eqns') <- liftM NE.unzip (mapM shift arg_eqn_prs)
                     ; let group_arg_vars = select_arg_vars arg_vars arg_eqn_prs
                     ; match_result <- match (map mkMatchId group_arg_vars ++ vars) ty (NE.toList eqns')
                     ; return $ foldr1 (.) wraps <$> match_result
                     }

              shift (_, EqnMatch {
                      eqn_pat = L _ (ConPat
                                    { pat_args = args
                                    , pat_con_ext = ConPatTc
                                      { cpt_tvs = tvs
                                      , cpt_dicts = ds
                                      , cpt_binds = bind }})
                    , eqn_rest = rest })
                = do dsTcEvBinds bind $ \ds_bind ->
                       return ( wrapBinds (tvs `zip` tvs1)
                              . wrapBinds (ds  `zip` dicts1)
                              . mkCoreLets ds_bind
                              , prependPats (conArgPats val_arg_tys args) rest
                              )
              shift (_, eqn) = pprPanic "matchOneCon/shift" (ppr eqn)
        ; let scaled_arg_tys = map (scaleScaled mult) val_arg_tys
            -- The 'val_arg_tys' are taken from the data type definition, they
            -- do not take into account the context multiplicity, therefore we
            -- need to scale them back to get the correct context multiplicity
            -- to desugar the sub-pattern in each field. We need to know these
            -- multiplicity because of the invariant that, in Core, binders in a
            -- constructor pattern must be scaled by the multiplicity of the
            -- case. See Note [Case expression invariants].
        ; arg_vars <- case retained_vars of
            Just fields -> return fields
            Nothing -> selectConMatchVars scaled_arg_tys args1
                -- Use the first equation as a source of
                -- suggestions for the new variables

        -- Divide into sub-groups; see Note [Record patterns]
        ; let groups :: NonEmpty (NonEmpty (ConArgPats, EquationInfoNE))
              groups = NE.groupBy1 compatible_pats
                     $ fmap (\eqn -> (con_pat_args (firstPat eqn), eqn)) (eqn1 :| eqns)

        ; match_results <- mapM (match_group arg_vars) groups

        ; return $ MkCaseAlt{ alt_pat = con1,
                              alt_bndrs = case con1 of
                                RealDataCon con
                                  | null tvs1, [_] <- dicts1
                                  , isNativeIndexType (mkTyConTy (dataConTyCon con))
                                  -- See Note [Retained constructor field scope].
                                  -> arg_vars ++ dicts1
                                _ -> tvs1 ++ dicts1 ++ arg_vars,
                              alt_wrapper = wrapper1,
                              alt_result = foldr1 combineMatchResults match_results } }
  where
    package_error :: DsM a
    package_error = failWithDs DsRetainedPackagePatterns

    con_pat_args :: Pat GhcTc -> HsConPatDetails GhcTc
    con_pat_args (ConPat { pat_args = args }) = args
    con_pat_args p = pprPanic "matchOneConLike" (ppr p)  -- All patterns are ConPats

    ConPat { pat_con = L _ con1
           , pat_args = args1
           , pat_con_ext = ConPatTc
             { cpt_arg_tys = arg_tys
             , cpt_wrap = wrapper1
             , cpt_tvs = tvs1
             , cpt_dicts = dicts1
             }
           } = firstPat eqn1
    fields1 = map flSelector (conLikeFieldLabels con1)

    ex_tvs = conLikeExTyCoVars con1

    -- Choose the right arg_vars in the right order for this group
    -- Note [Record patterns]
    select_arg_vars :: [Id] -> NonEmpty (ConArgPats, EquationInfo) -> [Id]
    select_arg_vars arg_vars ((arg_pats, _) :| _)
      | RecCon _ flds <- arg_pats
      , let rpats = rec_flds flds
      , not (null rpats)     -- Treated specially; cf conArgPats
      = assertPpr (fields1 `equalLength` arg_vars)
                  (ppr con1 $$ ppr fields1 $$ ppr arg_vars) $
        map lookup_fld rpats
      | otherwise
      = arg_vars
      where
        fld_var_env = mkNameEnv $ zipEqual fields1 arg_vars
        lookup_fld (L _ rpat) = lookupNameEnv_NF fld_var_env
                                            (idName (hsRecFieldId rpat))

-----------------
compatible_pats :: (ConArgPats,a) -> (ConArgPats,a) -> Bool
-- Two constructors have compatible argument patterns if the number
-- and order of sub-matches is the same in both cases
compatible_pats (RecCon _ flds1, _) (RecCon _ flds2, _) = same_fields flds1 flds2
compatible_pats (RecCon _ flds1, _) _                   = null (rec_flds flds1)
compatible_pats _                   (RecCon _ flds2, _) = null (rec_flds flds2)
compatible_pats _                   _                   = True -- Prefix or infix con

same_fields :: HsRecFields GhcTc (LPat GhcTc) -> HsRecFields GhcTc (LPat GhcTc)
            -> Bool
same_fields flds1 flds2
  = all2 (\(L _ f1) (L _ f2)
                          -> hsRecFieldId f1 == hsRecFieldId f2)
         (rec_flds flds1) (rec_flds flds2)


-----------------
selectConMatchVars :: [Scaled Type] -> ConArgPats -> DsM [Id]
selectConMatchVars arg_tys con
  = case con of
      RecCon {}        -> newSysLocalsDs arg_tys
      PrefixCon _ ps   -> selectMatchVars (zipMults arg_tys ps)
      InfixCon _ p1 p2 -> selectMatchVars (zipMults arg_tys [p1, p2])
  where
    zipMults = zipWithEqual (\a b -> (scaledMult a, unLoc b))

conArgPats :: [Scaled Type]-- Instantiated argument types
                          -- Used only to fill in the types of WildPats, which
                          -- are probably never looked at anyway
           -> ConArgPats
           -> [LPat GhcTc]
conArgPats _arg_tys (PrefixCon _ ps) = ps
conArgPats _arg_tys (InfixCon _ p1 p2) = [p1, p2]
conArgPats  arg_tys (RecCon _ (HsRecFields { rec_flds = rpats }))
  | null rpats = map (noLocA . WildPat . scaledThing) arg_tys
        -- Important special case for C {}, which can be used for a
        -- datacon that isn't declared to have fields at all
  | otherwise  = map (hfbRHS . unLoc) rpats

{-
Note [Record patterns]
~~~~~~~~~~~~~~~~~~~~~~
Consider
         data T = T { x,y,z :: Bool }

         f (T { y=True, x=False }) = ...

We must match the patterns IN THE ORDER GIVEN, thus for the first
one we match y=True before x=False.  See #246; or imagine
matching against (T { y=False, x=undefined }): should fail without
touching the undefined.

Now consider:

         f (T { y=True, x=False }) = ...
         f (T { x=True, y= False}) = ...

In the first we must test y first; in the second we must test x
first.  So we must divide even the equations for a single constructor
T into sub-groups, based on whether they match the same field in the
same order.  That's what the (groupBy compatible_pats) grouping.

All non-record patterns are "compatible" in this sense, because the
positional patterns (T a b) and (a `T` b) all match the arguments
in order.  Also T {} is special because it's equivalent to (T _ _).
Hence the (null rpats) checks here and there.

-}
