{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}

{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[RnBinds]{Renaming and dependency analysis of bindings}

This module does renaming and dependency analysis on value bindings in
the abstract syntax.  It does {\em not} do cycle-checks on class or
type-synonym declarations; those cannot be done at this stage because
they may be affected by renaming (which isn't fully worked out yet).
-}

module RnBinds (
   -- Renaming top-level bindings
   rnTopBindsLHS, rnTopBindsBoot, rnValBindsRHS,

   -- Renaming local bindings
   rnLocalBindsAndThen, rnLocalValBindsLHS, rnLocalValBindsRHS,

   -- Other bindings
   rnMethodBinds, renameSigs,
   rnMatchGroup, rnGRHSs, rnGRHS,
   makeMiniFixityEnv, MiniFixityEnv,
   HsSigCtxt(..)
   ) where

import {-# SOURCE #-} RnExpr( rnLExpr, rnStmts )

import HsSyn
import TcRnMonad
import TcEvidence     ( emptyTcEvBinds )
import RnTypes
import RnPat
import RnNames
import RnEnv
import DynFlags
import Module
import Name
import NameEnv
import NameSet
import RdrName          ( RdrName, rdrNameOcc )
import SrcLoc
import ListSetOps       ( findDupsEq )
import BasicTypes       ( RecFlag(..), LexicalFixity(..) )
import Digraph          ( SCC(..) )
import Bag
import Util
import Outputable
import FastString
import UniqFM
import Maybes           ( orElse )
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Data.List        ( partition, sort )

{-
-- ToDo: Put the annotations into the monad, so that they arrive in the proper
-- place and can be used when complaining.

The code tree received by the function @rnBinds@ contains definitions
in where-clauses which are all apparently mutually recursive, but which may
not really depend upon each other. For example, in the top level program
\begin{verbatim}
f x = y where a = x
              y = x
\end{verbatim}
the definitions of @a@ and @y@ do not depend on each other at all.
Unfortunately, the typechecker cannot always check such definitions.
\footnote{Mycroft, A. 1984. Polymorphic type schemes and recursive
definitions. In Proceedings of the International Symposium on Programming,
Toulouse, pp. 217-39. LNCS 167. Springer Verlag.}
However, the typechecker usually can check definitions in which only the
strongly connected components have been collected into recursive bindings.
This is precisely what the function @rnBinds@ does.

ToDo: deal with case where a single monobinds binds the same variable
twice.

The vertag tag is a unique @Int@; the tags only need to be unique
within one @MonoBinds@, so that unique-Int plumbing is done explicitly
(heavy monad machinery not needed).


************************************************************************
*                                                                      *
* naming conventions                                                   *
*                                                                      *
************************************************************************

\subsection[name-conventions]{Name conventions}

The basic algorithm involves walking over the tree and returning a tuple
containing the new tree plus its free variables. Some functions, such
as those walking polymorphic bindings (HsBinds) and qualifier lists in
list comprehensions (@Quals@), return the variables bound in local
environments. These are then used to calculate the free variables of the
expression evaluated in these environments.

Conventions for variable names are as follows:
\begin{itemize}
\item
new code is given a prime to distinguish it from the old.

\item
a set of variables defined in @Exp@ is written @dvExp@

\item
a set of variables free in @Exp@ is written @fvExp@
\end{itemize}

************************************************************************
*                                                                      *
* analysing polymorphic bindings (HsBindGroup, HsBind)
*                                                                      *
************************************************************************

\subsubsection[dep-HsBinds]{Polymorphic bindings}

Non-recursive expressions are reconstructed without any changes at top
level, although their component expressions may have to be altered.
However, non-recursive expressions are currently not expected as
\Haskell{} programs, and this code should not be executed.

Monomorphic bindings contain information that is returned in a tuple
(a @FlatMonoBinds@) containing:

\begin{enumerate}
\item
a unique @Int@ that serves as the ``vertex tag'' for this binding.

\item
the name of a function or the names in a pattern. These are a set
referred to as @dvLhs@, the defined variables of the left hand side.

\item
the free variables of the body. These are referred to as @fvBody@.

\item
the definition's actual code. This is referred to as just @code@.
\end{enumerate}

The function @nonRecDvFv@ returns two sets of variables. The first is
the set of variables defined in the set of monomorphic bindings, while the
second is the set of free variables in those bindings.

The set of variables defined in a non-recursive binding is just the
union of all of them, as @union@ removes duplicates. However, the
free variables in each successive set of cumulative bindings is the
union of those in the previous set plus those of the newest binding after
the defined variables of the previous set have been removed.

@rnMethodBinds@ deals only with the declarations in class and
instance declarations.  It expects only to see @FunMonoBind@s, and
it expects the global environment to contain bindings for the binders
(which are all class operations).

************************************************************************
*                                                                      *
\subsubsection{ Top-level bindings}
*                                                                      *
************************************************************************
-}

-- for top-level bindings, we need to make top-level names,
-- so we have a different entry point than for local bindings
rnTopBindsLHS :: MiniFixityEnv
              -> HsValBinds RdrName
              -> RnM (HsValBindsLR Name RdrName)
rnTopBindsLHS fix_env binds
  = rnValBindsLHS (topRecNameMaker fix_env) binds

rnTopBindsBoot :: NameSet -> HsValBindsLR Name RdrName -> RnM (HsValBinds Name, DefUses)
-- A hs-boot file has no bindings.
-- Return a single HsBindGroup with empty binds and renamed signatures
rnTopBindsBoot bound_names (ValBindsIn mbinds sigs)
  = do  { checkErr (isEmptyLHsBinds mbinds) (bindsInHsBootFile mbinds)
        ; (sigs', fvs) <- renameSigs (HsBootCtxt bound_names) sigs
        ; return (ValBindsOut [] sigs', usesOnly fvs) }
rnTopBindsBoot _ b = pprPanic "rnTopBindsBoot" (ppr b)

{-
*********************************************************
*                                                      *
                HsLocalBinds
*                                                      *
*********************************************************
-}

rnLocalBindsAndThen :: HsLocalBinds RdrName
                    -> (HsLocalBinds Name -> FreeVars -> RnM (result, FreeVars))
                    -> RnM (result, FreeVars)
-- This version (a) assumes that the binding vars are *not* already in scope
--               (b) removes the binders from the free vars of the thing inside
-- The parser doesn't produce ThenBinds
rnLocalBindsAndThen EmptyLocalBinds thing_inside =
  thing_inside EmptyLocalBinds emptyNameSet

rnLocalBindsAndThen (HsValBinds val_binds) thing_inside
  = rnLocalValBindsAndThen val_binds $ \ val_binds' ->
      thing_inside (HsValBinds val_binds')

rnLocalBindsAndThen (HsIPBinds binds) thing_inside = do
    (binds',fv_binds) <- rnIPBinds binds
    (thing, fvs_thing) <- thing_inside (HsIPBinds binds') fv_binds
    return (thing, fvs_thing `plusFV` fv_binds)

rnIPBinds :: HsIPBinds RdrName -> RnM (HsIPBinds Name, FreeVars)
rnIPBinds (IPBinds ip_binds _no_dict_binds) = do
    (ip_binds', fvs_s) <- mapAndUnzipM (wrapLocFstM rnIPBind) ip_binds
    return (IPBinds ip_binds' emptyTcEvBinds, plusFVs fvs_s)

rnIPBind :: IPBind RdrName -> RnM (IPBind Name, FreeVars)
rnIPBind (IPBind ~(Left n) expr) = do
    (expr',fvExpr) <- rnLExpr expr
    return (IPBind (Left n) expr', fvExpr)

{-
************************************************************************
*                                                                      *
                ValBinds
*                                                                      *
************************************************************************
-}

-- Renaming local binding groups
-- Does duplicate/shadow check
rnLocalValBindsLHS :: MiniFixityEnv
                   -> HsValBinds RdrName
                   -> RnM ([Name], HsValBindsLR Name RdrName)
rnLocalValBindsLHS fix_env binds
  = do { binds' <- rnValBindsLHS (localRecNameMaker fix_env) binds

         -- Check for duplicates and shadowing
         -- Must do this *after* renaming the patterns
         -- See Note [Collect binders only after renaming] in HsUtils

         -- We need to check for dups here because we
         -- don't don't bind all of the variables from the ValBinds at once
         -- with bindLocatedLocals any more.
         --
         -- Note that we don't want to do this at the top level, since
         -- sorting out duplicates and shadowing there happens elsewhere.
         -- The behavior is even different. For example,
         --   import A(f)
         --   f = ...
         -- should not produce a shadowing warning (but it will produce
         -- an ambiguity warning if you use f), but
         --   import A(f)
         --   g = let f = ... in f
         -- should.
       ; let bound_names = collectHsValBinders binds'
             -- There should be only Ids, but if there are any bogus
             -- pattern synonyms, we'll collect them anyway, so that
             -- we don't generate subsequent out-of-scope messages
       ; envs <- getRdrEnvs
       ; checkDupAndShadowedNames envs bound_names

       ; return (bound_names, binds') }

-- renames the left-hand sides
-- generic version used both at the top level and for local binds
-- does some error checking, but not what gets done elsewhere at the top level
rnValBindsLHS :: NameMaker
              -> HsValBinds RdrName
              -> RnM (HsValBindsLR Name RdrName)
rnValBindsLHS topP (ValBindsIn mbinds sigs)
  = do { mbinds' <- mapBagM (wrapLocM (rnBindLHS topP doc)) mbinds
       ; return $ ValBindsIn mbinds' sigs }
  where
    bndrs = collectHsBindsBinders mbinds
    doc   = text "In the binding group for:" <+> pprWithCommas ppr bndrs

rnValBindsLHS _ b = pprPanic "rnValBindsLHSFromDoc" (ppr b)

-- General version used both from the top-level and for local things
-- Assumes the LHS vars are in scope
--
-- Does not bind the local fixity declarations
rnValBindsRHS :: HsSigCtxt
              -> HsValBindsLR Name RdrName
              -> RnM (HsValBinds Name, DefUses)

rnValBindsRHS ctxt (ValBindsIn mbinds sigs)
  = do { (sigs', sig_fvs) <- renameSigs ctxt sigs
       ; binds_w_dus <- mapBagM (rnLBind (mkSigTvFn sigs')) mbinds
       ; let !(anal_binds, anal_dus) = depAnalBinds binds_w_dus

       ; let patsyn_fvs = foldr (unionNameSet . psb_fvs) emptyNameSet $
                          getPatSynBinds anal_binds
                -- The uses in binds_w_dus for PatSynBinds do not include
                -- variables used in the patsyn builders; see
                -- Note [Pattern synonym builders don't yield dependencies]
                -- But psb_fvs /does/ include those builder fvs.  So we
                -- add them back in here to avoid bogus warnings about
                -- unused variables (Trac #12548)

             valbind'_dus = anal_dus `plusDU` usesOnly sig_fvs
                                     `plusDU` usesOnly patsyn_fvs
                            -- Put the sig uses *after* the bindings
                            -- so that the binders are removed from
                            -- the uses in the sigs

        ; return (ValBindsOut anal_binds sigs', valbind'_dus) }

rnValBindsRHS _ b = pprPanic "rnValBindsRHS" (ppr b)

-- Wrapper for local binds
--
-- The *client* of this function is responsible for checking for unused binders;
-- it doesn't (and can't: we don't have the thing inside the binds) happen here
--
-- The client is also responsible for bringing the fixities into scope
rnLocalValBindsRHS :: NameSet  -- names bound by the LHSes
                   -> HsValBindsLR Name RdrName
                   -> RnM (HsValBinds Name, DefUses)
rnLocalValBindsRHS bound_names binds
  = rnValBindsRHS (LocalBindCtxt bound_names) binds

-- for local binds
-- wrapper that does both the left- and right-hand sides
--
-- here there are no local fixity decls passed in;
-- the local fixity decls come from the ValBinds sigs
rnLocalValBindsAndThen
  :: HsValBinds RdrName
  -> (HsValBinds Name -> FreeVars -> RnM (result, FreeVars))
  -> RnM (result, FreeVars)
rnLocalValBindsAndThen binds@(ValBindsIn _ sigs) thing_inside
 = do   {     -- (A) Create the local fixity environment
          new_fixities <- makeMiniFixityEnv [L loc sig
                                                  | L loc (FixSig sig) <- sigs]

              -- (B) Rename the LHSes
        ; (bound_names, new_lhs) <- rnLocalValBindsLHS new_fixities binds

              --     ...and bring them (and their fixities) into scope
        ; bindLocalNamesFV bound_names              $
          addLocalFixities new_fixities bound_names $ do

        {      -- (C) Do the RHS and thing inside
          (binds', dus) <- rnLocalValBindsRHS (mkNameSet bound_names) new_lhs
        ; (result, result_fvs) <- thing_inside binds' (allUses dus)

                -- Report unused bindings based on the (accurate)
                -- findUses.  E.g.
                --      let x = x in 3
                -- should report 'x' unused
        ; let real_uses = findUses dus result_fvs
              -- Insert fake uses for variables introduced implicitly by
              -- wildcards (#4404)
              implicit_uses = hsValBindsImplicits binds'
        ; warnUnusedLocalBinds bound_names
                                      (real_uses `unionNameSet` implicit_uses)

        ; let
            -- The variables "used" in the val binds are:
            --   (1) the uses of the binds (allUses)
            --   (2) the FVs of the thing-inside
            all_uses = allUses dus `plusFV` result_fvs
                -- Note [Unused binding hack]
                -- ~~~~~~~~~~~~~~~~~~~~~~~~~~
                -- Note that *in contrast* to the above reporting of
                -- unused bindings, (1) above uses duUses to return *all*
                -- the uses, even if the binding is unused.  Otherwise consider:
                --      x = 3
                --      y = let p = x in 'x'    -- NB: p not used
                -- If we don't "see" the dependency of 'y' on 'x', we may put the
                -- bindings in the wrong order, and the type checker will complain
                -- that x isn't in scope
                --
                -- But note that this means we won't report 'x' as unused,
                -- whereas we would if we had { x = 3; p = x; y = 'x' }

        ; return (result, all_uses) }}
                -- The bound names are pruned out of all_uses
                -- by the bindLocalNamesFV call above

rnLocalValBindsAndThen bs _ = pprPanic "rnLocalValBindsAndThen" (ppr bs)


---------------------

-- renaming a single bind

rnBindLHS :: NameMaker
          -> SDoc
          -> HsBind RdrName
          -- returns the renamed left-hand side,
          -- and the FreeVars *of the LHS*
          -- (i.e., any free variables of the pattern)
          -> RnM (HsBindLR Name RdrName)

rnBindLHS name_maker _ bind@(PatBind { pat_lhs = pat })
  = do
      -- we don't actually use the FV processing of rnPatsAndThen here
      (pat',pat'_fvs) <- rnBindPat name_maker pat
      return (bind { pat_lhs = pat', bind_fvs = pat'_fvs })
                -- We temporarily store the pat's FVs in bind_fvs;
                -- gets updated to the FVs of the whole bind
                -- when doing the RHS below

rnBindLHS name_maker _ bind@(FunBind { fun_id = rdr_name })
  = do { name <- applyNameMaker name_maker rdr_name
       ; return (bind { fun_id   = name
                      , bind_fvs = placeHolderNamesTc }) }

rnBindLHS name_maker _ (PatSynBind psb@PSB{ psb_id = rdrname })
  | isTopRecNameMaker name_maker
  = do { addLocM checkConName rdrname
       ; L _ name <- lookupLocatedTopBndrRn $ unLEmb rdrname
                    -- Should be in scope already
       ; return (PatSynBind psb{ psb_id = reLEmb rdrname name }) }

  | otherwise  -- Pattern synonym, not at top level
  = do { addErr localPatternSynonymErr  -- Complain, but make up a fake
                                        -- name so that we can carry on
       ; L _ name <- applyNameMaker name_maker $ unLEmb rdrname
       ; return (PatSynBind psb{ psb_id = reLEmb rdrname name }) }
  where
    localPatternSynonymErr :: SDoc
    localPatternSynonymErr
      = hang (text "Illegal pattern synonym declaration for" <+> quotes (ppr rdrname))
           2 (text "Pattern synonym declarations are only valid at top level")

rnBindLHS _ _ b = pprPanic "rnBindHS" (ppr b)

rnLBind :: (Name -> [Name])             -- Signature tyvar function
        -> LHsBindLR Name RdrName
        -> RnM (LHsBind Name, [Name], Uses)
rnLBind sig_fn (L loc bind)
  = setSrcSpan loc $
    do { (bind', bndrs, dus) <- rnBind sig_fn bind
       ; return (L loc bind', bndrs, dus) }

-- assumes the left-hands-side vars are in scope
rnBind :: (Name -> [Name])              -- Signature tyvar function
       -> HsBindLR Name RdrName
       -> RnM (HsBind Name, [Name], Uses)
rnBind _ bind@(PatBind { pat_lhs = pat
                       , pat_rhs = grhss
                                   -- pat fvs were stored in bind_fvs
                                   -- after processing the LHS
                       , bind_fvs = pat_fvs })
  = do  { mod <- getModule
        ; (grhss', rhs_fvs) <- rnGRHSs PatBindRhs rnLExpr grhss

                -- No scoped type variables for pattern bindings
        ; let all_fvs = pat_fvs `plusFV` rhs_fvs
              fvs'    = filterNameSet (nameIsLocalOrFrom mod) all_fvs
                -- Keep locally-defined Names
                -- As well as dependency analysis, we need these for the
                -- MonoLocalBinds test in TcBinds.decideGeneralisationPlan
              bndrs = collectPatBinders pat
              bind' = bind { pat_rhs  = grhss',
                             pat_rhs_ty = placeHolderType, bind_fvs = fvs' }
              is_wild_pat = case pat of
                              L _ (WildPat {})                 -> True
                              L _ (BangPat (L _ (WildPat {}))) -> True -- #9127
                              _                                -> False

        -- Warn if the pattern binds no variables, except for the
        -- entirely-explicit idiom    _ = rhs
        -- which (a) is not that different from  _v = rhs
        --       (b) is sometimes used to give a type sig for,
        --           or an occurrence of, a variable on the RHS
        ; whenWOptM Opt_WarnUnusedPatternBinds $
          when (null bndrs && not is_wild_pat) $
          addWarn (Reason Opt_WarnUnusedPatternBinds) $ unusedPatBindWarn bind'

        ; fvs' `seq` -- See Note [Free-variable space leak]
          return (bind', bndrs, all_fvs) }

rnBind sig_fn bind@(FunBind { fun_id = name
                            , fun_matches = matches })
       -- invariant: no free vars here when it's a FunBind
  = do  { let plain_name = unLoc name

        ; (matches', rhs_fvs) <- bindSigTyVarsFV (sig_fn plain_name) $
                                -- bindSigTyVars tests for LangExt.ScopedTyVars
                                 rnMatchGroup (FunRhs name Prefix)
                                              rnLExpr matches
        ; let is_infix = isInfixFunBind bind
        ; when is_infix $ checkPrecMatch plain_name matches'

        ; mod <- getModule
        ; let fvs' = filterNameSet (nameIsLocalOrFrom mod) rhs_fvs
                -- Keep locally-defined Names
                -- As well as dependency analysis, we need these for the
                -- MonoLocalBinds test in TcBinds.decideGeneralisationPlan

        ; fvs' `seq` -- See Note [Free-variable space leak]
          return (bind { fun_matches = matches'
                       , bind_fvs   = fvs' },
                  [plain_name], rhs_fvs)
      }

rnBind sig_fn (PatSynBind bind)
  = do  { (bind', name, fvs) <- rnPatSynBind sig_fn bind
        ; return (PatSynBind bind', name, fvs) }

rnBind _ b = pprPanic "rnBind" (ppr b)

{-
Note [Free-variable space leak]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have
    fvs' = trim fvs
and we seq fvs' before turning it as part of a record.

The reason is that trim is sometimes something like
    \xs -> intersectNameSet (mkNameSet bound_names) xs
and we don't want to retain the list bound_names. This showed up in
trac ticket #1136.
-}

{- *********************************************************************
*                                                                      *
          Dependency analysis and other support functions
*                                                                      *
********************************************************************* -}

depAnalBinds :: Bag (LHsBind Name, [Name], Uses)
             -> ([(RecFlag, LHsBinds Name)], DefUses)
-- Dependency analysis; this is important so that
-- unused-binding reporting is accurate
depAnalBinds binds_w_dus
  = (map get_binds sccs, map get_du sccs)
  where
    sccs = depAnal (\(_, defs, _) -> defs)
                   (\(_, _, uses) -> nonDetEltsUFM uses)
                   -- It's OK to use nonDetEltsUFM here as explained in
                   -- Note [depAnal determinism] in NameEnv.
                   (bagToList binds_w_dus)

    get_binds (AcyclicSCC (bind, _, _)) = (NonRecursive, unitBag bind)
    get_binds (CyclicSCC  binds_w_dus)  = (Recursive, listToBag [b | (b,_,_) <- binds_w_dus])

    get_du (AcyclicSCC (_, bndrs, uses)) = (Just (mkNameSet bndrs), uses)
    get_du (CyclicSCC  binds_w_dus)      = (Just defs, uses)
        where
          defs = mkNameSet [b | (_,bs,_) <- binds_w_dus, b <- bs]
          uses = unionNameSets [u | (_,_,u) <- binds_w_dus]

---------------------
-- Bind the top-level forall'd type variables in the sigs.
-- E.g  f :: a -> a
--      f = rhs
--      The 'a' scopes over the rhs
--
-- NB: there'll usually be just one (for a function binding)
--     but if there are many, one may shadow the rest; too bad!
--      e.g  x :: [a] -> [a]
--           y :: [(a,a)] -> a
--           (x,y) = e
--      In e, 'a' will be in scope, and it'll be the one from 'y'!

mkSigTvFn :: [LSig Name] -> (Name -> [Name])
-- Return a lookup function that maps an Id Name to the names
-- of the type variables that should scope over its body.
mkSigTvFn sigs = \n -> lookupNameEnv env n `orElse` []
  where
    env = mkHsSigEnv get_scoped_tvs sigs

    get_scoped_tvs :: LSig Name -> Maybe ([Located Name], [Name])
    -- Returns (binders, scoped tvs for those binders)
    get_scoped_tvs (L _ (ClassOpSig _ names sig_ty))
      = Just (map unLEmb names, hsScopedTvs sig_ty)
    get_scoped_tvs (L _ (TypeSig names sig_ty))
      = Just (map unLEmb names, hsWcScopedTvs sig_ty)
    get_scoped_tvs (L _ (PatSynSig names sig_ty))
      = Just (map unLEmb names, hsScopedTvs sig_ty)
    get_scoped_tvs _ = Nothing

-- Process the fixity declarations, making a FastString -> (Located Fixity) map
-- (We keep the location around for reporting duplicate fixity declarations.)
--
-- Checks for duplicates, but not that only locally defined things are fixed.
-- Note: for local fixity declarations, duplicates would also be checked in
--       check_sigs below.  But we also use this function at the top level.

makeMiniFixityEnv :: [LFixitySig RdrName] -> RnM MiniFixityEnv

makeMiniFixityEnv decls = foldlM add_one_sig emptyFsEnv decls
 where
   add_one_sig env (L loc (FixitySig names fixity)) =
     foldlM add_one env [ (loc,name_loc,name,fixity)
                        | L name_loc name <- names ]

   add_one env (loc, name_loc, name, fixity) = do
     { -- this fixity decl is a duplicate iff
       -- the ReaderName's OccName's FastString is already in the env
       -- (we only need to check the local fix_env because
       --  definitions of non-local will be caught elsewhere)
       let { fs = occNameFS (rdrNameOcc $ unEmb name)
           ; fix_item = L loc fixity };

       case lookupFsEnv env fs of
         Nothing -> return $ extendFsEnv env fs fix_item
         Just (L loc' _) -> do
           { setSrcSpan loc $
             addErrAt name_loc (dupFixityDecl loc' (unEmb name))
           ; return env}
     }

dupFixityDecl :: SrcSpan -> RdrName -> SDoc
dupFixityDecl loc rdr_name
  = vcat [text "Multiple fixity declarations for" <+> quotes (ppr rdr_name),
          text "also at " <+> ppr loc]


{- *********************************************************************
*                                                                      *
                Pattern synonym bindings
*                                                                      *
********************************************************************* -}

rnPatSynBind :: (Name -> [Name])                -- Signature tyvar function
             -> PatSynBind Name RdrName
             -> RnM (PatSynBind Name Name, [Name], Uses)
rnPatSynBind sig_fn bind@(PSB { psb_id = L l name
                              , psb_args = details
                              , psb_def = pat
                              , psb_dir = dir })
       -- invariant: no free vars here when it's a FunBind
  = do  { pattern_synonym_ok <- xoptM LangExt.PatternSynonyms
        ; unless pattern_synonym_ok (addErr patternSynonymErr)
        ; let sig_tvs = sig_fn $ unEmb name

        ; ((pat', details'), fvs1) <- bindSigTyVarsFV sig_tvs $
                                      rnPat PatSyn pat $ \pat' ->
         -- We check the 'RdrName's instead of the 'Name's
         -- so that the binding locations are reported
         -- from the left-hand side
            case details of
               PrefixPatSyn vars ->
                   do { checkDupRdrNames vars
                      ; names <- mapM lookupVar vars
                      ; return ( (pat', PrefixPatSyn names)
                               , mkFVs (map unLoc names)) }
               InfixPatSyn var1 var2 ->
                   do { checkDupRdrNames [var1, var2]
                      ; name1 <- lookupVar var1
                      ; name2 <- lookupVar var2
                      -- ; checkPrecMatch -- TODO
                      ; return ( (pat', InfixPatSyn name1 name2)
                               , mkFVs (map unLoc [name1, name2])) }
               RecordPatSyn vars ->
                   do { checkDupRdrNames (map recordPatSynSelectorId vars)
                      ; let rnRecordPatSynField
                              (RecordPatSynField { recordPatSynSelectorId = visible
                                                 , recordPatSynPatVar = hidden })
                              = do { visible' <- lookupLocatedTopBndrRn visible
                                   ; hidden'  <- lookupVar hidden
                                   ; return $ RecordPatSynField { recordPatSynSelectorId = visible'
                                                                , recordPatSynPatVar = hidden' } }
                      ; names <- mapM rnRecordPatSynField  vars
                      ; return ( (pat', RecordPatSyn names)
                               , mkFVs (map (unLoc . recordPatSynPatVar) names)) }

        ; (dir', fvs2) <- case dir of
            Unidirectional -> return (Unidirectional, emptyFVs)
            ImplicitBidirectional -> return (ImplicitBidirectional, emptyFVs)
            ExplicitBidirectional mg ->
              do { (mg', fvs) <- bindSigTyVarsFV sig_tvs $
                                 rnMatchGroup (FunRhs (L l $ unEmb name) Prefix)
                                              rnLExpr mg
                 ; return (ExplicitBidirectional mg', fvs) }

        ; mod <- getModule
        ; let fvs = fvs1 `plusFV` fvs2
              fvs' = filterNameSet (nameIsLocalOrFrom mod) fvs
                -- Keep locally-defined Names
                -- As well as dependency analysis, we need these for the
                -- MonoLocalBinds test in TcBinds.decideGeneralisationPlan

              bind' = bind{ psb_args = details'
                          , psb_def = pat'
                          , psb_dir = dir'
                          , psb_fvs = fvs' }
              selector_names = case details' of
                                 RecordPatSyn names ->
                                  map (unLoc . recordPatSynSelectorId) names
                                 _ -> []

        ; fvs' `seq` -- See Note [Free-variable space leak]
          return (bind', unEmb name : selector_names , fvs1)
          -- Why fvs1?  See Note [Pattern synonym builders don't yield dependencies]
      }
  where
    lookupVar = wrapLocM lookupOccRn

    patternSynonymErr :: SDoc
    patternSynonymErr
      = hang (text "Illegal pattern synonym declaration")
           2 (text "Use -XPatternSynonyms to enable this extension")

{-
Note [Pattern synonym builders don't yield dependencies]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When renaming a pattern synonym that has an explicit builder,
references in the builder definition should not be used when
calculating dependencies. For example, consider the following pattern
synonym definition:

pattern P x <- C1 x where
  P x = f (C1 x)

f (P x) = C2 x

In this case, 'P' needs to be typechecked in two passes:

1. Typecheck the pattern definition of 'P', which fully determines the
   type of 'P'. This step doesn't require knowing anything about 'f',
   since the builder definition is not looked at.

2. Typecheck the builder definition, which needs the typechecked
   definition of 'f' to be in scope; done by calls oo tcPatSynBuilderBind
   in TcBinds.tcValBinds.

This behaviour is implemented in 'tcValBinds', but it crucially
depends on 'P' not being put in a recursive group with 'f' (which
would make it look like a recursive pattern synonym a la 'pattern P =
P' which is unsound and rejected).

So:
 * We do not include builder fvs in the Uses returned by rnPatSynBind
   (which is then used for dependency analysis)
 * But we /do/ include them in the psb_fvs for the PatSynBind
 * In rnValBinds we record these builder uses, to avoid bogus
   unused-variable warnings (Trac #12548)
-}

{- *********************************************************************
*                                                                      *
                Class/instance method bindings
*                                                                      *
********************************************************************* -}

{- @rnMethodBinds@ is used for the method bindings of a class and an instance
declaration.   Like @rnBinds@ but without dependency analysis.

NOTA BENE: we record each {\em binder} of a method-bind group as a free variable.
That's crucial when dealing with an instance decl:
\begin{verbatim}
        instance Foo (T a) where
           op x = ...
\end{verbatim}
This might be the {\em sole} occurrence of @op@ for an imported class @Foo@,
and unless @op@ occurs we won't treat the type signature of @op@ in the class
decl for @Foo@ as a source of instance-decl gates.  But we should!  Indeed,
in many ways the @op@ in an instance decl is just like an occurrence, not
a binder.
-}

rnMethodBinds :: Bool                   -- True <=> is a class declaration
              -> Name                   -- Class name
              -> [Name]                 -- Type variables from the class/instance header
              -> LHsBinds RdrName       -- Binds
              -> [LSig RdrName]         -- and signatures/pragmas
              -> RnM (LHsBinds Name, [LSig Name], FreeVars)
-- Used for
--   * the default method bindings in a class decl
--   * the method bindings in an instance decl
rnMethodBinds is_cls_decl cls ktv_names binds sigs
  = do { checkDupRdrNames (collectMethodBinders binds)
             -- Check that the same method is not given twice in the
             -- same instance decl      instance C T where
             --                       f x = ...
             --                       g y = ...
             --                       f x = ...
             -- We must use checkDupRdrNames because the Name of the
             -- method is the Name of the class selector, whose SrcSpan
             -- points to the class declaration; and we use rnMethodBinds
             -- for instance decls too

       -- Rename the bindings LHSs
       ; binds' <- foldrBagM (rnMethodBindLHS is_cls_decl cls) emptyBag binds

       -- Rename the pragmas and signatures
       -- Annoyingly the type variables /are/ in scope for signatures, but
       -- /are not/ in scope in the SPECIALISE instance pramas; e.g.
       --    instance Eq a => Eq (T a) where
       --       (==) :: a -> a -> a
       --       {-# SPECIALISE instance Eq a => Eq (T [a]) #-}
       ; let (spec_inst_prags, other_sigs) = partition isSpecInstLSig sigs
             bound_nms = mkNameSet (collectHsBindsBinders binds')
             sig_ctxt | is_cls_decl = ClsDeclCtxt cls
                      | otherwise   = InstDeclCtxt bound_nms
       ; (spec_inst_prags', sip_fvs) <- renameSigs sig_ctxt spec_inst_prags
       ; (other_sigs',      sig_fvs) <- extendTyVarEnvFVRn ktv_names $
                                        renameSigs sig_ctxt other_sigs

       -- Rename the bindings RHSs.  Again there's an issue about whether the
       -- type variables from the class/instance head are in scope.
       -- Answer no in Haskell 2010, but yes if you have -XScopedTypeVariables
       ; scoped_tvs  <- xoptM LangExt.ScopedTypeVariables
       ; (binds'', bind_fvs) <- maybe_extend_tyvar_env scoped_tvs $
              do { binds_w_dus <- mapBagM (rnLBind (mkSigTvFn other_sigs')) binds'
                 ; let bind_fvs = foldrBag (\(_,_,fv1) fv2 -> fv1 `plusFV` fv2)
                                           emptyFVs binds_w_dus
                 ; return (mapBag fstOf3 binds_w_dus, bind_fvs) }

       ; return ( binds'', spec_inst_prags' ++ other_sigs'
                , sig_fvs `plusFV` sip_fvs `plusFV` bind_fvs) }
  where
    -- For the method bindings in class and instance decls, we extend
    -- the type variable environment iff -XScopedTypeVariables
    maybe_extend_tyvar_env scoped_tvs thing_inside
       | scoped_tvs = extendTyVarEnvFVRn ktv_names thing_inside
       | otherwise  = thing_inside

rnMethodBindLHS :: Bool -> Name
                -> LHsBindLR RdrName RdrName
                -> LHsBindsLR Name RdrName
                -> RnM (LHsBindsLR Name RdrName)
rnMethodBindLHS _ cls (L loc bind@(FunBind { fun_id = name })) rest
  = setSrcSpan loc $ do
    do { sel_name <- wrapLocM (lookupInstDeclBndr cls (text "method")) name
                     -- We use the selector name as the binder
       ; let bind' = bind { fun_id = sel_name
                          , bind_fvs = placeHolderNamesTc }

       ; return (L loc bind' `consBag` rest ) }

-- Report error for all other forms of bindings
-- This is why we use a fold rather than map
rnMethodBindLHS is_cls_decl _ (L loc bind) rest
  = do { addErrAt loc $
         vcat [ what <+> text "not allowed in" <+> decl_sort
              , nest 2 (ppr bind) ]
       ; return rest }
  where
    decl_sort | is_cls_decl = text "class declaration:"
              | otherwise   = text "instance declaration:"
    what = case bind of
              PatBind {}    -> text "Pattern bindings (except simple variables)"
              PatSynBind {} -> text "Pattern synonyms"
                               -- Associated pattern synonyms are not implemented yet
              _ -> pprPanic "rnMethodBind" (ppr bind)

{-
************************************************************************
*                                                                      *
\subsubsection[dep-Sigs]{Signatures (and user-pragmas for values)}
*                                                                      *
************************************************************************

@renameSigs@ checks for:
\begin{enumerate}
\item more than one sig for one thing;
\item signatures given for things not bound here;
\end{enumerate}

At the moment we don't gather free-var info from the types in
signatures.  We'd only need this if we wanted to report unused tyvars.
-}

renameSigs :: HsSigCtxt
           -> [LSig RdrName]
           -> RnM ([LSig Name], FreeVars)
-- Renames the signatures and performs error checks
renameSigs ctxt sigs
  = do  { mapM_ dupSigDeclErr (findDupSigs sigs)

        ; checkDupMinimalSigs sigs

        ; (sigs', sig_fvs) <- mapFvRn (wrapLocFstM (renameSig ctxt)) sigs

        ; let (good_sigs, bad_sigs) = partition (okHsSig ctxt) sigs'
        ; mapM_ misplacedSigErr bad_sigs                 -- Misplaced

        ; return (good_sigs, sig_fvs) }

----------------------
-- We use lookupSigOccRn in the signatures, which is a little bit unsatisfactory
-- because this won't work for:
--      instance Foo T where
--        {-# INLINE op #-}
--        Baz.op = ...
-- We'll just rename the INLINE prag to refer to whatever other 'op'
-- is in scope.  (I'm assuming that Baz.op isn't in scope unqualified.)
-- Doesn't seem worth much trouble to sort this.

renameSig :: HsSigCtxt -> Sig RdrName -> RnM (Sig Name, FreeVars)
-- FixitySig is renamed elsewhere.
renameSig _ (IdSig x)
  = return (IdSig x, emptyFVs)    -- Actually this never occurs

renameSig ctxt sig@(TypeSig vs ty)
  = do  { new_vs <- mapM (lookupLESigOccRn ctxt sig) vs
        ; let doc = TypeSigCtx (ppr_sig_bndrs vs)
        ; (new_ty, fvs) <- rnHsSigWcType doc ty
        ; return (TypeSig new_vs new_ty, fvs) }

renameSig ctxt sig@(ClassOpSig is_deflt vs ty)
  = do  { defaultSigs_on <- xoptM LangExt.DefaultSignatures
        ; when (is_deflt && not defaultSigs_on) $
          addErr (defaultSigErr sig)
        ; new_v <- mapM (lookupLESigOccRn ctxt sig) vs
        ; (new_ty, fvs) <- rnHsSigType ty_ctxt ty
        ; return (ClassOpSig is_deflt new_v new_ty, fvs) }
  where
    (v1:_) = vs
    ty_ctxt = GenericCtx (text "a class method signature for"
                          <+> quotes (ppr v1))

renameSig _ (SpecInstSig src ty)
  = do  { (new_ty, fvs) <- rnHsSigType SpecInstSigCtx ty
        ; return (SpecInstSig src new_ty,fvs) }

-- {-# SPECIALISE #-} pragmas can refer to imported Ids
-- so, in the top-level case (when mb_names is Nothing)
-- we use lookupOccRn.  If there's both an imported and a local 'f'
-- then the SPECIALISE pragma is ambiguous, unlike all other signatures
renameSig ctxt sig@(SpecSig v tys inl)
  = do  { new_v <- case ctxt of
                     TopSigCtxt {} -> lookupLEmbellishedOccRn v
                     _             -> lookupLESigOccRn ctxt sig v
        ; (new_ty, fvs) <- foldM do_one ([],emptyFVs) tys
        ; return (SpecSig new_v new_ty inl, fvs) }
  where
    ty_ctxt = GenericCtx (text "a SPECIALISE signature for"
                          <+> quotes (ppr v))
    do_one (tys,fvs) ty
      = do { (new_ty, fvs_ty) <- rnHsSigType ty_ctxt ty
           ; return ( new_ty:tys, fvs_ty `plusFV` fvs) }

renameSig ctxt sig@(InlineSig v s)
  = do  { new_v <- lookupLESigOccRn ctxt sig v
        ; return (InlineSig new_v s, emptyFVs) }

renameSig ctxt sig@(FixSig (FixitySig vs f))
  = do  { new_vs <- mapM (lookupLESigOccRn ctxt sig) vs
        ; return (FixSig (FixitySig new_vs f), emptyFVs) }

renameSig ctxt sig@(MinimalSig s (L l bf))
  = do new_bf <- traverse (lookupLESigOccRn ctxt sig) bf
       return (MinimalSig s (L l new_bf), emptyFVs)

renameSig ctxt sig@(PatSynSig vs ty)
  = do  { new_vs <- mapM (lookupLESigOccRn ctxt sig) vs
        ; (ty', fvs) <- rnHsSigType ty_ctxt ty
        ; return (PatSynSig new_vs ty', fvs) }
  where
    ty_ctxt = GenericCtx (text "a pattern synonym signature for"
                          <+> ppr_sig_bndrs vs)

renameSig ctxt sig@(SCCFunSig st v s)
  = do  { new_v <- lookupLESigOccRn ctxt sig v
        ; return (SCCFunSig st new_v s, emptyFVs) }

-- COMPLETE Sigs can refer to imported IDs which is why we use
-- lookupLocatedOccRn rather than lookupSigOccRn
renameSig _ctxt (CompleteMatchSig s (L l bf) mty)
  = do new_bf  <- traverse lookupLEmbellishedOccRn bf
       new_mty <- traverse lookupLEmbellishedOccRn mty
       return (CompleteMatchSig s (L l new_bf) new_mty, emptyFVs)

ppr_sig_bndrs :: [LEmbellished RdrName] -> SDoc
ppr_sig_bndrs bs = quotes (pprWithCommas ppr bs)

okHsSig :: HsSigCtxt -> LSig a -> Bool
okHsSig ctxt (L _ sig)
  = case (sig, ctxt) of
     (ClassOpSig {}, ClsDeclCtxt {})  -> True
     (ClassOpSig {}, InstDeclCtxt {}) -> True
     (ClassOpSig {}, _)               -> False

     (TypeSig {}, ClsDeclCtxt {})  -> False
     (TypeSig {}, InstDeclCtxt {}) -> False
     (TypeSig {}, _)               -> True

     (PatSynSig {}, TopSigCtxt{}) -> True
     (PatSynSig {}, _)            -> False

     (FixSig {}, InstDeclCtxt {}) -> False
     (FixSig {}, _)               -> True

     (IdSig {}, TopSigCtxt {})   -> True
     (IdSig {}, InstDeclCtxt {}) -> True
     (IdSig {}, _)               -> False

     (InlineSig {}, HsBootCtxt {}) -> False
     (InlineSig {}, _)             -> True

     (SpecSig {}, TopSigCtxt {})    -> True
     (SpecSig {}, LocalBindCtxt {}) -> True
     (SpecSig {}, InstDeclCtxt {})  -> True
     (SpecSig {}, _)                -> False

     (SpecInstSig {}, InstDeclCtxt {}) -> True
     (SpecInstSig {}, _)               -> False

     (MinimalSig {}, ClsDeclCtxt {}) -> True
     (MinimalSig {}, _)              -> False

     (SCCFunSig {}, HsBootCtxt {}) -> False
     (SCCFunSig {}, _)             -> True

     (CompleteMatchSig {}, TopSigCtxt {} ) -> True
     (CompleteMatchSig {}, _)              -> False

-------------------
findDupSigs :: [LSig RdrName] -> [[(Located RdrName, Sig RdrName)]]
-- Check for duplicates on RdrName version,
-- because renamed version has unboundName for
-- not-in-scope binders, which gives bogus dup-sig errors
-- NB: in a class decl, a 'generic' sig is not considered
--     equal to an ordinary sig, so we allow, say
--           class C a where
--             op :: a -> a
--             default op :: Eq a => a -> a
findDupSigs sigs
  = findDupsEq matching_sig (concatMap (expand_sig . unLoc) sigs)
  where
    expand_sig sig@(FixSig (FixitySig ns _)) = zip (map unLEmb ns) (repeat sig)
    expand_sig sig@(InlineSig n _)           = [(unLEmb n,sig)]
    expand_sig sig@(TypeSig ns _)            = [(unLEmb n,sig) | n <- ns]
    expand_sig sig@(ClassOpSig _ ns _)       = [(unLEmb n,sig) | n <- ns]
    expand_sig sig@(PatSynSig ns  _ )        = [(unLEmb n,sig) | n <- ns]
    expand_sig sig@(SCCFunSig _ n _)         = [(unLEmb n,sig)]
    expand_sig _ = []

    matching_sig (L _ n1,sig1) (L _ n2,sig2)       = n1 == n2 && mtch sig1 sig2
    mtch (FixSig {})           (FixSig {})         = True
    mtch (InlineSig {})        (InlineSig {})      = True
    mtch (TypeSig {})          (TypeSig {})        = True
    mtch (ClassOpSig d1 _ _)   (ClassOpSig d2 _ _) = d1 == d2
    mtch (PatSynSig _ _)       (PatSynSig _ _)     = True
    mtch (SCCFunSig{})         (SCCFunSig{})       = True
    mtch _ _ = False

-- Warn about multiple MINIMAL signatures
checkDupMinimalSigs :: [LSig RdrName] -> RnM ()
checkDupMinimalSigs sigs
  = case filter isMinimalLSig sigs of
      minSigs@(_:_:_) -> dupMinimalSigErr minSigs
      _ -> return ()

{-
************************************************************************
*                                                                      *
\subsection{Match}
*                                                                      *
************************************************************************
-}

rnMatchGroup :: Outputable (body RdrName) => HsMatchContext Name
             -> (Located (body RdrName) -> RnM (Located (body Name), FreeVars))
             -> MatchGroup RdrName (Located (body RdrName))
             -> RnM (MatchGroup Name (Located (body Name)), FreeVars)
rnMatchGroup ctxt rnBody (MG { mg_alts = L _ ms, mg_origin = origin })
  = do { empty_case_ok <- xoptM LangExt.EmptyCase
       ; when (null ms && not empty_case_ok) (addErr (emptyCaseErr ctxt))
       ; (new_ms, ms_fvs) <- mapFvRn (rnMatch ctxt rnBody) ms
       ; return (mkMatchGroup origin new_ms, ms_fvs) }

rnMatch :: Outputable (body RdrName) => HsMatchContext Name
        -> (Located (body RdrName) -> RnM (Located (body Name), FreeVars))
        -> LMatch RdrName (Located (body RdrName))
        -> RnM (LMatch Name (Located (body Name)), FreeVars)
rnMatch ctxt rnBody = wrapLocFstM (rnMatch' ctxt rnBody)

rnMatch' :: Outputable (body RdrName) => HsMatchContext Name
         -> (Located (body RdrName) -> RnM (Located (body Name), FreeVars))
         -> Match RdrName (Located (body RdrName))
         -> RnM (Match Name (Located (body Name)), FreeVars)
rnMatch' ctxt rnBody match@(Match { m_ctxt = mf, m_pats = pats
                                  , m_type = maybe_rhs_sig, m_grhss = grhss })
  = do  {       -- Result type signatures are no longer supported
          case maybe_rhs_sig of
                Nothing -> return ()
                Just (L loc ty) -> addErrAt loc (resSigErr match ty)

        ; let fixity = if isInfixMatch match then Infix else Prefix
               -- Now the main event
               -- Note that there are no local fixity decls for matches
        ; rnPats ctxt pats      $ \ pats' -> do
        { (grhss', grhss_fvs) <- rnGRHSs ctxt rnBody grhss
        ; let mf' = case (ctxt,mf) of
                      (FunRhs (L _ funid) _,FunRhs (L lf _) _)
                                            -> FunRhs (L lf funid) fixity
                      _                     -> ctxt
        ; return (Match { m_ctxt = mf', m_pats = pats'
                        , m_type = Nothing, m_grhss = grhss'}, grhss_fvs ) }}

emptyCaseErr :: HsMatchContext Name -> SDoc
emptyCaseErr ctxt = hang (text "Empty list of alternatives in" <+> pp_ctxt)
                       2 (text "Use EmptyCase to allow this")
  where
    pp_ctxt = case ctxt of
                CaseAlt    -> text "case expression"
                LambdaExpr -> text "\\case expression"
                _ -> text "(unexpected)" <+> pprMatchContextNoun ctxt


resSigErr :: Outputable body
          => Match RdrName body -> HsType RdrName -> SDoc
resSigErr match ty
   = vcat [ text "Illegal result type signature" <+> quotes (ppr ty)
          , nest 2 $ ptext (sLit
                 "Result signatures are no longer supported in pattern matches")
          , pprMatchInCtxt match ]

{-
************************************************************************
*                                                                      *
\subsubsection{Guarded right-hand sides (GRHSs)}
*                                                                      *
************************************************************************
-}

rnGRHSs :: HsMatchContext Name
        -> (Located (body RdrName) -> RnM (Located (body Name), FreeVars))
        -> GRHSs RdrName (Located (body RdrName))
        -> RnM (GRHSs Name (Located (body Name)), FreeVars)
rnGRHSs ctxt rnBody (GRHSs grhss (L l binds))
  = rnLocalBindsAndThen binds   $ \ binds' _ -> do
    (grhss', fvGRHSs) <- mapFvRn (rnGRHS ctxt rnBody) grhss
    return (GRHSs grhss' (L l binds'), fvGRHSs)

rnGRHS :: HsMatchContext Name
       -> (Located (body RdrName) -> RnM (Located (body Name), FreeVars))
       -> LGRHS RdrName (Located (body RdrName))
       -> RnM (LGRHS Name (Located (body Name)), FreeVars)
rnGRHS ctxt rnBody = wrapLocFstM (rnGRHS' ctxt rnBody)

rnGRHS' :: HsMatchContext Name
        -> (Located (body RdrName) -> RnM (Located (body Name), FreeVars))
        -> GRHS RdrName (Located (body RdrName))
        -> RnM (GRHS Name (Located (body Name)), FreeVars)
rnGRHS' ctxt rnBody (GRHS guards rhs)
  = do  { pattern_guards_allowed <- xoptM LangExt.PatternGuards
        ; ((guards', rhs'), fvs) <- rnStmts (PatGuard ctxt) rnLExpr guards $ \ _ ->
                                    rnBody rhs

        ; unless (pattern_guards_allowed || is_standard_guard guards')
                 (addWarn NoReason (nonStdGuardErr guards'))

        ; return (GRHS guards' rhs', fvs) }
  where
        -- Standard Haskell 1.4 guards are just a single boolean
        -- expression, rather than a list of qualifiers as in the
        -- Glasgow extension
    is_standard_guard []                       = True
    is_standard_guard [L _ (BodyStmt _ _ _ _)] = True
    is_standard_guard _                        = False

{-
************************************************************************
*                                                                      *
\subsection{Error messages}
*                                                                      *
************************************************************************
-}

dupSigDeclErr :: [(Located RdrName, Sig RdrName)] -> RnM ()
dupSigDeclErr pairs@((L loc name, sig) : _)
  = addErrAt loc $
    vcat [ text "Duplicate" <+> what_it_is
           <> text "s for" <+> quotes (ppr name)
         , text "at" <+> vcat (map ppr $ sort $ map (getLoc . fst) pairs) ]
  where
    what_it_is = hsSigDoc sig

dupSigDeclErr [] = panic "dupSigDeclErr"

misplacedSigErr :: LSig Name -> RnM ()
misplacedSigErr (L loc sig)
  = addErrAt loc $
    sep [text "Misplaced" <+> hsSigDoc sig <> colon, ppr sig]

defaultSigErr :: Sig RdrName -> SDoc
defaultSigErr sig = vcat [ hang (text "Unexpected default signature:")
                              2 (ppr sig)
                         , text "Use DefaultSignatures to enable default signatures" ]

bindsInHsBootFile :: LHsBindsLR Name RdrName -> SDoc
bindsInHsBootFile mbinds
  = hang (text "Bindings in hs-boot files are not allowed")
       2 (ppr mbinds)

nonStdGuardErr :: Outputable body => [LStmtLR Name Name body] -> SDoc
nonStdGuardErr guards
  = hang (text "accepting non-standard pattern guards (use PatternGuards to suppress this message)")
       4 (interpp'SP guards)

unusedPatBindWarn :: HsBind Name -> SDoc
unusedPatBindWarn bind
  = hang (text "This pattern-binding binds no variables:")
       2 (ppr bind)

dupMinimalSigErr :: [LSig RdrName] -> RnM ()
dupMinimalSigErr sigs@(L loc _ : _)
  = addErrAt loc $
    vcat [ text "Multiple minimal complete definitions"
         , text "at" <+> vcat (map ppr $ sort $ map getLoc sigs)
         , text "Combine alternative minimal complete definitions with `|'" ]
dupMinimalSigErr [] = panic "dupMinimalSigErr"
