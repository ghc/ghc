
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
import GHC.Tc.Utils.TcType
import GHC.Core.Multiplicity
import GHC.HsToCore.Monad
import GHC.HsToCore.Utils
import GHC.Core ( CoreExpr )
import GHC.Core.Make ( mkCoreLets )
import GHC.Utils.Misc
import GHC.Types.Id
import GHC.Types.Name.Env
import GHC.Types.FieldLabel ( flSelector )
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import Control.Monad(liftM)
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

matchConFamily :: NonEmpty Id
               -> Type
               -> NonEmpty (NonEmpty EquationInfoNE)
               -> DsM (MatchResult CoreExpr)
-- Each group of eqns is for a single constructor
matchConFamily (var :| vars) ty groups
  = do let mult = idMult var
           -- Each variable in the argument list correspond to one column in the
           -- pattern matching equations. Its multiplicity is the context
           -- multiplicity of the pattern. We extract that multiplicity, so that
           -- 'matchOneconLike' knows the context multiplicity, in case it needs
           -- to come up with new variables.
       alts <- mapM (fmap toRealAlt . matchOneConLike vars ty mult) groups
       return (mkCoAlgCaseMatchResult var ty alts)
  where
    toRealAlt alt = case alt_pat alt of
        RealDataCon dcon -> alt{ alt_pat = dcon }
        _ -> panic "matchConFamily: not RealDataCon"

matchPatSyn :: NonEmpty Id
            -> Type
            -> NonEmpty EquationInfoNE
            -> DsM (MatchResult CoreExpr)
matchPatSyn (var :| vars) ty eqns
  = do let mult = idMult var
       alt <- fmap toSynAlt $ matchOneConLike vars ty mult eqns
       return (mkCoSynCaseMatchResult var ty alt)
  where
    toSynAlt alt = case alt_pat alt of
        PatSynCon psyn -> alt{ alt_pat = psyn }
        _ -> panic "matchPatSyn: not PatSynCon"

type ConArgPats = HsConPatDetails GhcTc

matchOneConLike :: [Id]
                -> Type
                -> Mult
                -> NonEmpty EquationInfoNE
                -> DsM (CaseAlt ConLike)
matchOneConLike vars ty mult (eqn1 :| eqns)   -- All eqns for a single constructor
  = do  { let inst_tys = assert (all tcIsTcTyVar ex_tvs) $
                           -- ex_tvs can only be tyvars as data types in source
                           -- Haskell cannot mention covar yet (Aug 2018).
                         assert (tvs1 `equalLength` ex_tvs) $
                         arg_tys ++ mkTyVarTys tvs1

              val_arg_tys = conLikeInstOrigArgTys con1 inst_tys
        -- dataConInstOrigArgTys takes the univ and existential tyvars
        -- and returns the types of the *value* args, which is what we want

              match_group :: [Id]
                          -> NonEmpty (ConArgPats, EquationInfoNE)
                          -> DsM (MatchResult CoreExpr)
              -- All members of the group have compatible ConArgPats
              match_group arg_vars arg_eqn_prs
                = do { (wraps, eqns') <- liftM NE.unzip (mapM shift arg_eqn_prs)
                     ; let group_arg_vars = select_arg_vars arg_vars arg_eqn_prs
                     ; match_result <- match (group_arg_vars ++ vars) ty (NE.toList eqns')
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
        ; arg_vars <- selectConMatchVars scaled_arg_tys args1
                -- Use the first equation as a source of
                -- suggestions for the new variables

        -- Divide into sub-groups; see Note [Record patterns]
        ; let groups :: NonEmpty (NonEmpty (ConArgPats, EquationInfoNE))
              groups = NE.groupBy1 compatible_pats
                     $ fmap (\eqn -> (con_pat_args (firstPat eqn), eqn)) (eqn1 :| eqns)

        ; match_results <- mapM (match_group arg_vars) groups

        ; return $ MkCaseAlt{ alt_pat = con1,
                              alt_bndrs = tvs1 ++ dicts1 ++ arg_vars,
                              alt_wrapper = wrapper1,
                              alt_result = foldr1 combineMatchResults match_results } }
  where
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
      | RecCon flds <- arg_pats
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
compatible_pats (RecCon flds1, _) (RecCon flds2, _) = same_fields flds1 flds2
compatible_pats (RecCon flds1, _) _                 = null (rec_flds flds1)
compatible_pats _                 (RecCon flds2, _) = null (rec_flds flds2)
compatible_pats _                 _                 = True -- Prefix or infix con

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
      RecCon {}      -> newSysLocalsDs arg_tys
      PrefixCon ps   -> selectMatchVars (zipMults arg_tys ps)
      InfixCon p1 p2 -> selectMatchVars (zipMults arg_tys [p1, p2])
  where
    zipMults = zipWithEqual (\a b -> (scaledMult a, unLoc b))

conArgPats :: [Scaled Type]-- Instantiated argument types
                          -- Used only to fill in the types of WildPats, which
                          -- are probably never looked at anyway
           -> ConArgPats
           -> [LPat GhcTc]
conArgPats _arg_tys (PrefixCon ps) = ps
conArgPats _arg_tys (InfixCon p1 p2) = [p1, p2]
conArgPats  arg_tys (RecCon (HsRecFields { rec_flds = rpats }))
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
