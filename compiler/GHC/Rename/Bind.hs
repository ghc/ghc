{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

Renaming and dependency analysis of bindings

This module does renaming and dependency analysis on value bindings in
the abstract syntax.  It does {\em not} do cycle-checks on class or
type-synonym declarations; those cannot be done at this stage because
they may be affected by renaming (which isn't fully worked out yet).
-}

module GHC.Rename.Bind (
   -- Renaming top-level bindings
   rnTopBindsLHS, rnTopBindsLHSBoot, rnTopBindsBoot, rnValBindsRHS,

   -- Renaming local bindings
   rnLocalBindsAndThen, rnLocalValBindsLHS, rnLocalValBindsRHS,

   -- Other bindings
   rnMethodBinds, renameSigs,
   rnMatchGroup, rnGRHSs, rnGRHS, rnSrcFixityDecl,
   makeMiniFixityEnv, MiniFixityEnv,
   HsSigCtxt(..)
   ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Rename.Expr( rnExpr, rnLExpr, rnStmts )

import GHC.Hs
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Rename.HsType
import GHC.Rename.Pat
import GHC.Rename.Names
import GHC.Rename.Env
import GHC.Rename.Fixity
import GHC.Rename.Utils ( HsDocContext(..), mapFvRn
                        , checkDupRdrNames, checkDupRdrNamesN
                        , warnUnusedLocalBinds
                        , warnForallIdentifier
                        , checkUnusedRecordWildcard
                        , checkDupAndShadowedNames, bindLocalNamesFV
                        , addNoNestedForallsContextsErr, checkInferredVars )
import GHC.Driver.Session
import GHC.Unit.Module
import GHC.Types.Error
import GHC.Types.FieldLabel
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.Name.Reader ( RdrName, rdrNameOcc )
import GHC.Types.SrcLoc as SrcLoc
import GHC.Data.List.SetOps    ( findDupsEq )
import GHC.Types.Basic         ( RecFlag(..), TypeOrKind(..) )
import GHC.Data.Graph.Directed ( SCC(..) )
import GHC.Data.Bag
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Types.Unique.Set
import GHC.Data.Maybe          ( orElse )
import GHC.Data.OrdList
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Data.Foldable      ( toList )
import Data.List          ( partition, sortBy )
import Data.List.NonEmpty ( NonEmpty(..) )

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
              -> HsValBinds GhcPs
              -> RnM (HsValBindsLR GhcRn GhcPs)
rnTopBindsLHS fix_env binds
  = rnValBindsLHS (topRecNameMaker fix_env) binds

-- Ensure that a hs-boot file has no top-level bindings.
rnTopBindsLHSBoot :: MiniFixityEnv
                  -> HsValBinds GhcPs
                  -> RnM (HsValBindsLR GhcRn GhcPs)
rnTopBindsLHSBoot fix_env binds
  = do  { topBinds <- rnTopBindsLHS fix_env binds
        ; case topBinds of
            ValBinds x mbinds sigs ->
              do  { mapM_ bindInHsBootFileErr mbinds
                  ; pure (ValBinds x emptyBag sigs) }
            _ -> pprPanic "rnTopBindsLHSBoot" (ppr topBinds) }

rnTopBindsBoot :: NameSet -> HsValBindsLR GhcRn GhcPs
               -> RnM (HsValBinds GhcRn, DefUses)
-- A hs-boot file has no bindings.
-- Return a single HsBindGroup with empty binds and renamed signatures
rnTopBindsBoot bound_names (ValBinds _ _ sigs)
  = do  { (sigs', fvs) <- renameSigs (HsBootCtxt bound_names) sigs
        ; return (XValBindsLR (NValBinds [] sigs'), usesOnly fvs) }
rnTopBindsBoot _ b = pprPanic "rnTopBindsBoot" (ppr b)

{-
*********************************************************
*                                                      *
                HsLocalBinds
*                                                      *
*********************************************************
-}

rnLocalBindsAndThen :: HsLocalBinds GhcPs
                   -> (HsLocalBinds GhcRn -> FreeVars -> RnM (result, FreeVars))
                   -> RnM (result, FreeVars)
-- This version (a) assumes that the binding vars are *not* already in scope
--               (b) removes the binders from the free vars of the thing inside
-- The parser doesn't produce ThenBinds
rnLocalBindsAndThen (EmptyLocalBinds x) thing_inside =
  thing_inside (EmptyLocalBinds x) emptyNameSet

rnLocalBindsAndThen (HsValBinds x val_binds) thing_inside
  = rnLocalValBindsAndThen val_binds $ \ val_binds' ->
      thing_inside (HsValBinds x val_binds')

rnLocalBindsAndThen (HsIPBinds x binds) thing_inside = do
    (binds',fv_binds) <- rnIPBinds binds
    (thing, fvs_thing) <- thing_inside (HsIPBinds x binds') fv_binds
    return (thing, fvs_thing `plusFV` fv_binds)

rnIPBinds :: HsIPBinds GhcPs -> RnM (HsIPBinds GhcRn, FreeVars)
rnIPBinds (IPBinds _ ip_binds ) = do
    (ip_binds', fvs_s) <- mapAndUnzipM (wrapLocFstMA rnIPBind) ip_binds
    return (IPBinds noExtField ip_binds', plusFVs fvs_s)

rnIPBind :: IPBind GhcPs -> RnM (IPBind GhcRn, FreeVars)
rnIPBind (IPBind _ ~(Left n) expr) = do
    (expr',fvExpr) <- rnLExpr expr
    return (IPBind noAnn (Left n) expr', fvExpr)

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
                   -> HsValBinds GhcPs
                   -> RnM ([Name], HsValBindsLR GhcRn GhcPs)
rnLocalValBindsLHS fix_env binds
  = do { binds' <- rnValBindsLHS (localRecNameMaker fix_env) binds

         -- Check for duplicates and shadowing
         -- Must do this *after* renaming the patterns
         -- See Note [Collect binders only after renaming] in GHC.Hs.Utils

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
       ; let bound_names = collectHsValBinders CollNoDictBinders binds'
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
              -> HsValBinds GhcPs
              -> RnM (HsValBindsLR GhcRn GhcPs)
rnValBindsLHS topP (ValBinds x mbinds sigs)
  = do { mbinds' <- mapBagM (wrapLocMA (rnBindLHS topP doc)) mbinds
       ; return $ ValBinds x mbinds' sigs }
  where
    bndrs = collectHsBindsBinders CollNoDictBinders mbinds
    doc   = text "In the binding group for:" <+> pprWithCommas ppr bndrs

rnValBindsLHS _ b = pprPanic "rnValBindsLHSFromDoc" (ppr b)

-- General version used both from the top-level and for local things
-- Assumes the LHS vars are in scope
--
-- Does not bind the local fixity declarations
rnValBindsRHS :: HsSigCtxt
              -> HsValBindsLR GhcRn GhcPs
              -> RnM (HsValBinds GhcRn, DefUses)

rnValBindsRHS ctxt (ValBinds _ mbinds sigs)
  = do { (sigs', sig_fvs) <- renameSigs ctxt sigs
       ; binds_w_dus <- mapBagM (rnLBind (mkScopedTvFn sigs')) mbinds
       ; let !(anal_binds, anal_dus) = depAnalBinds binds_w_dus

       ; let patsyn_fvs = foldr (unionNameSet . psb_ext) emptyNameSet $
                          getPatSynBinds anal_binds
                -- The uses in binds_w_dus for PatSynBinds do not include
                -- variables used in the patsyn builders; see
                -- Note [Pattern synonym builders don't yield dependencies]
                -- But psb_fvs /does/ include those builder fvs.  So we
                -- add them back in here to avoid bogus warnings about
                -- unused variables (#12548)

             valbind'_dus = anal_dus `plusDU` usesOnly sig_fvs
                                     `plusDU` usesOnly patsyn_fvs
                            -- Put the sig uses *after* the bindings
                            -- so that the binders are removed from
                            -- the uses in the sigs

        ; return (XValBindsLR (NValBinds anal_binds sigs'), valbind'_dus) }

rnValBindsRHS _ b = pprPanic "rnValBindsRHS" (ppr b)

-- Wrapper for local binds
--
-- The *client* of this function is responsible for checking for unused binders;
-- it doesn't (and can't: we don't have the thing inside the binds) happen here
--
-- The client is also responsible for bringing the fixities into scope
rnLocalValBindsRHS :: NameSet  -- names bound by the LHSes
                   -> HsValBindsLR GhcRn GhcPs
                   -> RnM (HsValBinds GhcRn, DefUses)
rnLocalValBindsRHS bound_names binds
  = rnValBindsRHS (LocalBindCtxt bound_names) binds

-- for local binds
-- wrapper that does both the left- and right-hand sides
--
-- here there are no local fixity decls passed in;
-- the local fixity decls come from the ValBinds sigs
rnLocalValBindsAndThen
  :: HsValBinds GhcPs
  -> (HsValBinds GhcRn -> FreeVars -> RnM (result, FreeVars))
  -> RnM (result, FreeVars)
rnLocalValBindsAndThen binds@(ValBinds _ _ sigs) thing_inside
 = do   {     -- (A) Create the local fixity environment
          new_fixities <- makeMiniFixityEnv [ L loc sig
                                            | L loc (FixSig _ sig) <- sigs]

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
              rec_uses = hsValBindsImplicits binds'
              implicit_uses = mkNameSet $ concatMap snd
                                        $ rec_uses
        ; mapM_ (\(loc, ns) ->
                    checkUnusedRecordWildcard loc real_uses (Just ns))
                rec_uses
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
          -> HsBind GhcPs
          -- returns the renamed left-hand side,
          -- and the FreeVars *of the LHS*
          -- (i.e., any free variables of the pattern)
          -> RnM (HsBindLR GhcRn GhcPs)

rnBindLHS name_maker _ bind@(PatBind { pat_lhs = pat })
  = do
      -- we don't actually use the FV processing of rnPatsAndThen here
      (pat',pat'_fvs) <- rnBindPat name_maker pat
      return (bind { pat_lhs = pat', pat_ext = pat'_fvs })
                -- We temporarily store the pat's FVs in bind_fvs;
                -- gets updated to the FVs of the whole bind
                -- when doing the RHS below

rnBindLHS name_maker _ bind@(FunBind { fun_id = rdr_name })
  = do { name <- applyNameMaker name_maker rdr_name
       ; return (bind { fun_id = name
                      , fun_ext = noExtField }) }

rnBindLHS name_maker _ (PatSynBind x psb@PSB{ psb_id = rdrname })
  | isTopRecNameMaker name_maker
  = do { addLocMA checkConName rdrname
       ; name <-
           lookupLocatedTopConstructorRnN rdrname -- Should be in scope already
       ; return (PatSynBind x psb{ psb_ext = noAnn, psb_id = name }) }

  | otherwise  -- Pattern synonym, not at top level
  = do { addErr localPatternSynonymErr  -- Complain, but make up a fake
                                        -- name so that we can carry on
       ; name <- applyNameMaker name_maker rdrname
       ; return (PatSynBind x psb{ psb_ext = noAnn, psb_id = name }) }
  where
    localPatternSynonymErr :: TcRnMessage
    localPatternSynonymErr = TcRnIllegalPatSynDecl rdrname

rnBindLHS _ _ b = pprPanic "rnBindHS" (ppr b)

rnLBind :: (Name -> [Name])      -- Signature tyvar function
        -> LHsBindLR GhcRn GhcPs
        -> RnM (LHsBind GhcRn, [Name], Uses)
rnLBind sig_fn (L loc bind)
  = setSrcSpanA loc $
    do { (bind', bndrs, dus) <- rnBind sig_fn bind
       ; return (L loc bind', bndrs, dus) }

-- assumes the left-hands-side vars are in scope
rnBind :: (Name -> [Name])        -- Signature tyvar function
       -> HsBindLR GhcRn GhcPs
       -> RnM (HsBind GhcRn, [Name], Uses)
rnBind _ bind@(PatBind { pat_lhs = pat
                       , pat_rhs = grhss
                                   -- pat fvs were stored in bind_fvs
                                   -- after processing the LHS
                       , pat_ext = pat_fvs })
  = do  { mod <- getModule
        ; (grhss', rhs_fvs) <- rnGRHSs PatBindRhs rnLExpr grhss

                -- No scoped type variables for pattern bindings
        ; let all_fvs = pat_fvs `plusFV` rhs_fvs
              fvs'    = filterNameSet (nameIsLocalOrFrom mod) all_fvs
                -- Keep locally-defined Names
                -- As well as dependency analysis, we need these for the
                -- MonoLocalBinds test in GHC.Tc.Gen.Bind.decideGeneralisationPlan
              bndrs = collectPatBinders CollNoDictBinders pat
              bind' = bind { pat_rhs  = grhss'
                           , pat_ext = fvs' }

              ok_nobind_pat
                  = -- See Note [Pattern bindings that bind no variables]
                    case unLoc pat of
                       WildPat {}   -> True
                       BangPat {}   -> True -- #9127, #13646
                       SplicePat {} -> True
                       _            -> False

        -- Warn if the pattern binds no variables
        -- See Note [Pattern bindings that bind no variables]
        ; whenWOptM Opt_WarnUnusedPatternBinds $
          when (null bndrs && not ok_nobind_pat) $
          addTcRnDiagnostic (TcRnUnusedPatternBinds bind')

        ; fvs' `seq` -- See Note [Free-variable space leak]
          return (bind', bndrs, all_fvs) }

rnBind sig_fn bind@(FunBind { fun_id = name
                            , fun_matches = matches })
       -- invariant: no free vars here when it's a FunBind
  = do  { let plain_name = unLoc name

        ; (matches', rhs_fvs) <- bindSigTyVarsFV (sig_fn plain_name) $
                                -- bindSigTyVars tests for LangExt.ScopedTyVars
                                 rnMatchGroup (mkPrefixFunRhs name)
                                              rnLExpr matches
        ; let is_infix = isInfixFunBind bind
        ; when is_infix $ checkPrecMatch plain_name matches'

        ; mod <- getModule
        ; let fvs' = filterNameSet (nameIsLocalOrFrom mod) rhs_fvs
                -- Keep locally-defined Names
                -- As well as dependency analysis, we need these for the
                -- MonoLocalBinds test in GHC.Tc.Gen.Bind.decideGeneralisationPlan

        ; fvs' `seq` -- See Note [Free-variable space leak]
          return (bind { fun_matches = matches'
                       , fun_ext     = fvs' },
                  [plain_name], rhs_fvs)
      }

rnBind sig_fn (PatSynBind x bind)
  = do  { (bind', name, fvs) <- rnPatSynBind sig_fn bind
        ; return (PatSynBind x bind', name, fvs) }

rnBind _ b = pprPanic "rnBind" (ppr b)

{- Note [Pattern bindings that bind no variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally, we want to warn about pattern bindings like
  Just _ = e
because they don't do anything!  But we have three exceptions:

* A wildcard pattern
       _ = rhs
  which (a) is not that different from  _v = rhs
        (b) is sometimes used to give a type sig for,
            or an occurrence of, a variable on the RHS

* A strict pattern binding; that is, one with an outermost bang
     !Just _ = e
  This can fail, so unlike the lazy variant, it is not a no-op.
  Moreover, #13646 argues that even for single constructor
  types, you might want to write the constructor.  See also #9127.

* A splice pattern
      $(th-lhs) = rhs
   It is impossible to determine whether or not th-lhs really
   binds any variable. We should disable the warning for any pattern
   which contain splices, but that is a more expensive check.

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

depAnalBinds :: Bag (LHsBind GhcRn, [Name], Uses)
             -> ([(RecFlag, LHsBinds GhcRn)], DefUses)
-- Dependency analysis; this is important so that
-- unused-binding reporting is accurate
depAnalBinds binds_w_dus
  = (map get_binds sccs, toOL $ map get_du sccs)
  where
    sccs = depAnal (\(_, defs, _) -> defs)
                   (\(_, _, uses) -> nonDetEltsUniqSet uses)
                   -- It's OK to use nonDetEltsUniqSet here as explained in
                   -- Note [depAnal determinism] in GHC.Types.Name.Env.
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
-- E.g  f :: forall a. a -> a
--      f = rhs
--      The 'a' scopes over the rhs
--
-- NB: there'll usually be just one (for a function binding)
--     but if there are many, one may shadow the rest; too bad!
--      e.g  x :: forall a. [a] -> [a]
--           y :: forall a. [(a,a)] -> a
--           (x,y) = e
--      In e, 'a' will be in scope, and it'll be the one from 'y'!

mkScopedTvFn :: [LSig GhcRn] -> (Name -> [Name])
-- Return a lookup function that maps an Id Name to the names
-- of the type variables that should scope over its body.
mkScopedTvFn sigs = \n -> lookupNameEnv env n `orElse` []
  where
    env = mkHsSigEnv get_scoped_tvs sigs

    get_scoped_tvs :: LSig GhcRn -> Maybe ([LocatedN Name], [Name])
    -- Returns (binders, scoped tvs for those binders)
    get_scoped_tvs (L _ (ClassOpSig _ _ names sig_ty))
      = Just (names, hsScopedTvs sig_ty)
    get_scoped_tvs (L _ (TypeSig _ names sig_ty))
      = Just (names, hsWcScopedTvs sig_ty)
    get_scoped_tvs (L _ (PatSynSig _ names sig_ty))
      = Just (names, hsScopedTvs sig_ty)
    get_scoped_tvs _ = Nothing

-- Process the fixity declarations, making a FastString -> (Located Fixity) map
-- (We keep the location around for reporting duplicate fixity declarations.)
--
-- Checks for duplicates, but not that only locally defined things are fixed.
-- Note: for local fixity declarations, duplicates would also be checked in
--       check_sigs below.  But we also use this function at the top level.

makeMiniFixityEnv :: [LFixitySig GhcPs] -> RnM MiniFixityEnv

makeMiniFixityEnv decls = foldlM add_one_sig emptyFsEnv decls
 where
   add_one_sig :: MiniFixityEnv -> LFixitySig GhcPs -> RnM MiniFixityEnv
   add_one_sig env (L loc (FixitySig _ names fixity)) =
     foldlM add_one env [ (locA loc,locA name_loc,name,fixity)
                        | L name_loc name <- names ]

   add_one env (loc, name_loc, name,fixity) = do
     { -- this fixity decl is a duplicate iff
       -- the ReaderName's OccName's FastString is already in the env
       -- (we only need to check the local fix_env because
       --  definitions of non-local will be caught elsewhere)
       let { fs = occNameFS (rdrNameOcc name)
           ; fix_item = L loc fixity };

       case lookupFsEnv env fs of
         Nothing -> return $ extendFsEnv env fs fix_item
         Just (L loc' _) -> do
           { setSrcSpan loc $
             addErrAt name_loc (dupFixityDecl loc' name)
           ; return env}
     }

dupFixityDecl :: SrcSpan -> RdrName -> TcRnMessage
dupFixityDecl loc rdr_name
  = TcRnUnknownMessage $ mkPlainError noHints $
    vcat [text "Multiple fixity declarations for" <+> quotes (ppr rdr_name),
          text "also at " <+> ppr loc]


{- *********************************************************************
*                                                                      *
                Pattern synonym bindings
*                                                                      *
********************************************************************* -}

rnPatSynBind :: (Name -> [Name])           -- Signature tyvar function
             -> PatSynBind GhcRn GhcPs
             -> RnM (PatSynBind GhcRn GhcRn, [Name], Uses)
rnPatSynBind sig_fn bind@(PSB { psb_id = L l name
                              , psb_args = details
                              , psb_def = pat
                              , psb_dir = dir })
       -- invariant: no free vars here when it's a FunBind
  = do  { pattern_synonym_ok <- xoptM LangExt.PatternSynonyms
        ; unless pattern_synonym_ok (addErr patternSynonymErr)
        ; let scoped_tvs = sig_fn name

        ; ((pat', details'), fvs1) <- bindSigTyVarsFV scoped_tvs $
                                      rnPat PatSyn pat $ \pat' ->
         -- We check the 'RdrName's instead of the 'Name's
         -- so that the binding locations are reported
         -- from the left-hand side
            case details of
               PrefixCon _ vars ->
                   do { checkDupRdrNamesN vars
                      ; names <- mapM lookupPatSynBndr vars
                      ; return ( (pat', PrefixCon noTypeArgs names)
                               , mkFVs (map unLoc names)) }
               InfixCon var1 var2 ->
                   do { checkDupRdrNames [var1, var2]
                      ; name1 <- lookupPatSynBndr var1
                      ; name2 <- lookupPatSynBndr var2
                      -- ; checkPrecMatch -- TODO
                      ; return ( (pat', InfixCon name1 name2)
                               , mkFVs (map unLoc [name1, name2])) }
               RecCon vars ->
                   do { checkDupRdrNames (map (foLabel . recordPatSynField) vars)
                      ; fls <- lookupConstructorFields name
                      ; let fld_env = mkFsEnv [ (flLabel fl, fl) | fl <- fls ]
                      ; let rnRecordPatSynField
                              (RecordPatSynField { recordPatSynField  = visible
                                                 , recordPatSynPatVar = hidden })
                              = do { let visible' = lookupField fld_env visible
                                   ; hidden'  <- lookupPatSynBndr hidden
                                   ; return $ RecordPatSynField { recordPatSynField  = visible'
                                                                , recordPatSynPatVar = hidden' } }
                      ; names <- mapM rnRecordPatSynField  vars
                      ; return ( (pat', RecCon names)
                               , mkFVs (map (unLoc . recordPatSynPatVar) names)) }

        ; (dir', fvs2) <- case dir of
            Unidirectional -> return (Unidirectional, emptyFVs)
            ImplicitBidirectional -> return (ImplicitBidirectional, emptyFVs)
            ExplicitBidirectional mg ->
                do { (mg', fvs) <- bindSigTyVarsFV scoped_tvs $
                                   rnMatchGroup (mkPrefixFunRhs (L l name))
                                                rnLExpr mg
                   ; return (ExplicitBidirectional mg', fvs) }

        ; mod <- getModule
        ; let fvs = fvs1 `plusFV` fvs2
              fvs' = filterNameSet (nameIsLocalOrFrom mod) fvs
                -- Keep locally-defined Names
                -- As well as dependency analysis, we need these for the
                -- MonoLocalBinds test in GHC.Tc.Gen.Bind.decideGeneralisationPlan

              bind' = bind{ psb_args = details'
                          , psb_def = pat'
                          , psb_dir = dir'
                          , psb_ext = fvs' }
              selector_names = case details' of
                                 RecCon names ->
                                  map (foExt . recordPatSynField) names
                                 _ -> []

        ; fvs' `seq` -- See Note [Free-variable space leak]
          return (bind', name : selector_names , fvs1)
          -- Why fvs1?  See Note [Pattern synonym builders don't yield dependencies]
      }
  where
    -- See Note [Renaming pattern synonym variables]
    lookupPatSynBndr = wrapLocMA lookupLocalOccRn

    patternSynonymErr :: TcRnMessage
    patternSynonymErr
      = TcRnUnknownMessage $ mkPlainError noHints $
        hang (text "Illegal pattern synonym declaration")
           2 (text "Use -XPatternSynonyms to enable this extension")

{-
Note [Renaming pattern synonym variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We rename pattern synonym declaractions backwards to normal to reuse
the logic already implemented for renaming patterns.

We first rename the RHS of a declaration which brings into
scope the variables bound by the pattern (as they would be
in normal function definitions). We then lookup the variables
which we want to bind in this local environment.

It is crucial that we then only lookup in the *local* environment which
only contains the variables brought into scope by the pattern and nothing
else. Amazingly no-one encountered this bug for 3 GHC versions but
it was possible to define a pattern synonym which referenced global
identifiers and worked correctly.

```
x = 5

pattern P :: Int -> ()
pattern P x <- _

f (P x) = x

> f () = 5
```

See #13470 for the original report.

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
   in GHC.Tc.Gen.Bind.tcValBinds.

This behaviour is implemented in 'tcValBinds', but it crucially
depends on 'P' not being put in a recursive group with 'f' (which
would make it look like a recursive pattern synonym a la 'pattern P =
P' which is unsound and rejected).

So:
 * We do not include builder fvs in the Uses returned by rnPatSynBind
   (which is then used for dependency analysis)
 * But we /do/ include them in the psb_fvs for the PatSynBind
 * In rnValBinds we record these builder uses, to avoid bogus
   unused-variable warnings (#12548)
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
              -> LHsBinds GhcPs         -- Binds
              -> [LSig GhcPs]           -- and signatures/pragmas
              -> RnM (LHsBinds GhcRn, [LSig GhcRn], FreeVars)
-- Used for
--   * the default method bindings in a class decl
--   * the method bindings in an instance decl
rnMethodBinds is_cls_decl cls ktv_names binds sigs
  = do { checkDupRdrNamesN (collectMethodBinders binds)
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
       ; binds' <- foldrM (rnMethodBindLHS is_cls_decl cls) emptyBag binds

       -- Rename the pragmas and signatures
       -- Annoyingly the type variables /are/ in scope for signatures, but
       -- /are not/ in scope in the SPECIALISE instance pramas; e.g.
       --    instance Eq a => Eq (T a) where
       --       (==) :: a -> a -> a
       --       {-# SPECIALISE instance Eq a => Eq (T [a]) #-}
       ; let (spec_inst_prags, other_sigs) = partition isSpecInstLSig sigs
             bound_nms = mkNameSet (collectHsBindsBinders CollNoDictBinders binds')
             sig_ctxt | is_cls_decl = ClsDeclCtxt cls
                      | otherwise   = InstDeclCtxt bound_nms
       ; (spec_inst_prags', sip_fvs) <- renameSigs sig_ctxt spec_inst_prags
       ; (other_sigs',      sig_fvs) <- bindLocalNamesFV ktv_names $
                                        renameSigs sig_ctxt other_sigs

       -- Rename the bindings RHSs.  Again there's an issue about whether the
       -- type variables from the class/instance head are in scope.
       -- Answer no in Haskell 2010, but yes if you have -XScopedTypeVariables
       ; (binds'', bind_fvs) <- bindSigTyVarsFV ktv_names $
              do { binds_w_dus <- mapBagM (rnLBind (mkScopedTvFn other_sigs')) binds'
                 ; let bind_fvs = foldr (\(_,_,fv1) fv2 -> fv1 `plusFV` fv2)
                                           emptyFVs binds_w_dus
                 ; return (mapBag fstOf3 binds_w_dus, bind_fvs) }

       ; return ( binds'', spec_inst_prags' ++ other_sigs'
                , sig_fvs `plusFV` sip_fvs `plusFV` bind_fvs) }

rnMethodBindLHS :: Bool -> Name
                -> LHsBindLR GhcPs GhcPs
                -> LHsBindsLR GhcRn GhcPs
                -> RnM (LHsBindsLR GhcRn GhcPs)
rnMethodBindLHS _ cls (L loc bind@(FunBind { fun_id = name })) rest
  = setSrcSpanA loc $ do
    do { sel_name <- wrapLocMA (lookupInstDeclBndr cls (text "method")) name
                     -- We use the selector name as the binder
       ; let bind' = bind { fun_id = sel_name, fun_ext = noExtField }
       ; return (L loc bind' `consBag` rest ) }

-- Report error for all other forms of bindings
-- This is why we use a fold rather than map
rnMethodBindLHS is_cls_decl _ (L loc bind) rest
  = do { addErrAt (locA loc) $ TcRnUnknownMessage $ mkPlainError noHints $
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
           -> [LSig GhcPs]
           -> RnM ([LSig GhcRn], FreeVars)
-- Renames the signatures and performs error checks
renameSigs ctxt sigs
  = do  { mapM_ dupSigDeclErr (findDupSigs sigs)

        ; checkDupMinimalSigs sigs

        ; (sigs', sig_fvs) <- mapFvRn (wrapLocFstMA (renameSig ctxt)) sigs

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

renameSig :: HsSigCtxt -> Sig GhcPs -> RnM (Sig GhcRn, FreeVars)
renameSig _ (IdSig _ x)
  = return (IdSig noExtField x, emptyFVs)    -- Actually this never occurs

renameSig ctxt sig@(TypeSig _ vs ty)
  = do  { new_vs <- mapM (lookupSigOccRnN ctxt sig) vs
        ; let doc = TypeSigCtx (ppr_sig_bndrs vs)
        ; (new_ty, fvs) <- rnHsSigWcType doc ty
        ; return (TypeSig noAnn new_vs new_ty, fvs) }

renameSig ctxt sig@(ClassOpSig _ is_deflt vs ty)
  = do  { defaultSigs_on <- xoptM LangExt.DefaultSignatures
        ; when (is_deflt && not defaultSigs_on) $
          addErr (defaultSigErr sig)
        ; mapM_ warnForallIdentifier vs
        ; new_v <- mapM (lookupSigOccRnN ctxt sig) vs
        ; (new_ty, fvs) <- rnHsSigType ty_ctxt TypeLevel ty
        ; return (ClassOpSig noAnn is_deflt new_v new_ty, fvs) }
  where
    (v1:_) = vs
    ty_ctxt = GenericCtx (text "a class method signature for"
                          <+> quotes (ppr v1))

renameSig _ (SpecInstSig _ src ty)
  = do  { checkInferredVars doc inf_msg ty
        ; (new_ty, fvs) <- rnHsSigType doc TypeLevel ty
          -- Check if there are any nested `forall`s or contexts, which are
          -- illegal in the type of an instance declaration (see
          -- Note [No nested foralls or contexts in instance types] in
          -- GHC.Hs.Type).
        ; addNoNestedForallsContextsErr doc (text "SPECIALISE instance type")
            (getLHsInstDeclHead new_ty)
        ; return (SpecInstSig noAnn src new_ty,fvs) }
  where
    doc = SpecInstSigCtx
    inf_msg = Just (text "Inferred type variables are not allowed")

-- {-# SPECIALISE #-} pragmas can refer to imported Ids
-- so, in the top-level case (when mb_names is Nothing)
-- we use lookupOccRn.  If there's both an imported and a local 'f'
-- then the SPECIALISE pragma is ambiguous, unlike all other signatures
renameSig ctxt sig@(SpecSig _ v tys inl)
  = do  { new_v <- case ctxt of
                     TopSigCtxt {} -> lookupLocatedOccRn v
                     _             -> lookupSigOccRnN ctxt sig v
        ; (new_ty, fvs) <- foldM do_one ([],emptyFVs) tys
        ; return (SpecSig noAnn new_v new_ty inl, fvs) }
  where
    ty_ctxt = GenericCtx (text "a SPECIALISE signature for"
                          <+> quotes (ppr v))
    do_one (tys,fvs) ty
      = do { (new_ty, fvs_ty) <- rnHsSigType ty_ctxt TypeLevel ty
           ; return ( new_ty:tys, fvs_ty `plusFV` fvs) }

renameSig ctxt sig@(InlineSig _ v s)
  = do  { new_v <- lookupSigOccRnN ctxt sig v
        ; return (InlineSig noAnn new_v s, emptyFVs) }

renameSig ctxt (FixSig _ fsig)
  = do  { new_fsig <- rnSrcFixityDecl ctxt fsig
        ; return (FixSig noAnn new_fsig, emptyFVs) }

renameSig ctxt sig@(MinimalSig _ s (L l bf))
  = do new_bf <- traverse (lookupSigOccRnN ctxt sig) bf
       return (MinimalSig noAnn s (L l new_bf), emptyFVs)

renameSig ctxt sig@(PatSynSig _ vs ty)
  = do  { new_vs <- mapM (lookupSigOccRnN ctxt sig) vs
        ; (ty', fvs) <- rnHsSigType ty_ctxt TypeLevel ty
        ; return (PatSynSig noAnn new_vs ty', fvs) }
  where
    ty_ctxt = GenericCtx (text "a pattern synonym signature for"
                          <+> ppr_sig_bndrs vs)

renameSig ctxt sig@(SCCFunSig _ st v s)
  = do  { new_v <- lookupSigOccRnN ctxt sig v
        ; return (SCCFunSig noAnn st new_v s, emptyFVs) }

-- COMPLETE Sigs can refer to imported IDs which is why we use
-- lookupLocatedOccRn rather than lookupSigOccRn
renameSig _ctxt sig@(CompleteMatchSig _ s (L l bf) mty)
  = do new_bf <- traverse lookupLocatedOccRn bf
       new_mty  <- traverse lookupLocatedOccRn mty

       this_mod <- fmap tcg_mod getGblEnv
       unless (any (nameIsLocalOrFrom this_mod . unLoc) new_bf) $
         -- Why 'any'? See Note [Orphan COMPLETE pragmas]
         addErrCtxt (text "In" <+> ppr sig) $ failWithTc orphanError

       return (CompleteMatchSig noAnn s (L l new_bf) new_mty, emptyFVs)
  where
    orphanError :: TcRnMessage
    orphanError = TcRnUnknownMessage $ mkPlainError noHints $
      text "Orphan COMPLETE pragmas not supported" $$
      text "A COMPLETE pragma must mention at least one data constructor" $$
      text "or pattern synonym defined in the same module."

{-
Note [Orphan COMPLETE pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We define a COMPLETE pragma to be a non-orphan if it includes at least
one conlike defined in the current module. Why is this sufficient?
Well if you have a pattern match

  case expr of
    P1 -> ...
    P2 -> ...
    P3 -> ...

any COMPLETE pragma which mentions a conlike other than P1, P2 or P3
will not be of any use in verifying that the pattern match is
exhaustive. So as we have certainly read the interface files that
define P1, P2 and P3, we will have loaded all non-orphan COMPLETE
pragmas that could be relevant to this pattern match.

For now we simply disallow orphan COMPLETE pragmas, as the added
complexity of supporting them properly doesn't seem worthwhile.
-}

ppr_sig_bndrs :: [LocatedN RdrName] -> SDoc
ppr_sig_bndrs bs = quotes (pprWithCommas ppr bs)

okHsSig :: HsSigCtxt -> LSig (GhcPass a) -> Bool
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
findDupSigs :: [LSig GhcPs] -> [NonEmpty (LocatedN RdrName, Sig GhcPs)]
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
    expand_sig :: Sig GhcPs -> [(LocatedN RdrName, Sig GhcPs)] -- AZ
    expand_sig sig@(FixSig _ (FixitySig _ ns _)) = zip ns (repeat sig)
    expand_sig sig@(InlineSig _ n _)             = [(n,sig)]
    expand_sig sig@(TypeSig _ ns _)              = [(n,sig) | n <- ns]
    expand_sig sig@(ClassOpSig _ _ ns _)         = [(n,sig) | n <- ns]
    expand_sig sig@(PatSynSig _ ns  _ )          = [(n,sig) | n <- ns]
    expand_sig sig@(SCCFunSig _ _ n _)           = [(n,sig)]
    expand_sig _ = []

    matching_sig :: (LocatedN RdrName, Sig GhcPs) -> (LocatedN RdrName, Sig GhcPs) -> Bool --AZ
    matching_sig (L _ n1,sig1) (L _ n2,sig2)       = n1 == n2 && mtch sig1 sig2
    mtch (FixSig {})           (FixSig {})         = True
    mtch (InlineSig {})        (InlineSig {})      = True
    mtch (TypeSig {})          (TypeSig {})        = True
    mtch (ClassOpSig _ d1 _ _) (ClassOpSig _ d2 _ _) = d1 == d2
    mtch (PatSynSig _ _ _)     (PatSynSig _ _ _)   = True
    mtch (SCCFunSig{})         (SCCFunSig{})       = True
    mtch _ _ = False

-- Warn about multiple MINIMAL signatures
checkDupMinimalSigs :: [LSig GhcPs] -> RnM ()
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

type AnnoBody body
  = ( Anno [LocatedA (Match GhcRn (LocatedA (body GhcRn)))] ~ SrcSpanAnnL
    , Anno [LocatedA (Match GhcPs (LocatedA (body GhcPs)))] ~ SrcSpanAnnL
    , Anno (Match GhcRn (LocatedA (body GhcRn))) ~ SrcSpanAnnA
    , Anno (Match GhcPs (LocatedA (body GhcPs))) ~ SrcSpanAnnA
    , Anno (GRHS GhcRn (LocatedA (body GhcRn))) ~ SrcAnn NoEpAnns
    , Anno (GRHS GhcPs (LocatedA (body GhcPs))) ~ SrcAnn NoEpAnns
    , Outputable (body GhcPs)
    )

rnMatchGroup :: (Outputable (body GhcPs), AnnoBody body) => HsMatchContext GhcRn
             -> (LocatedA (body GhcPs) -> RnM (LocatedA (body GhcRn), FreeVars))
             -> MatchGroup GhcPs (LocatedA (body GhcPs))
             -> RnM (MatchGroup GhcRn (LocatedA (body GhcRn)), FreeVars)
rnMatchGroup ctxt rnBody (MG { mg_alts = L lm ms, mg_origin = origin })
  = do { empty_case_ok <- xoptM LangExt.EmptyCase
       ; when (null ms && not empty_case_ok) (addErr (emptyCaseErr ctxt))
       ; (new_ms, ms_fvs) <- mapFvRn (rnMatch ctxt rnBody) ms
       ; return (mkMatchGroup origin (L lm new_ms), ms_fvs) }

rnMatch :: AnnoBody body
        => HsMatchContext GhcRn
        -> (LocatedA (body GhcPs) -> RnM (LocatedA (body GhcRn), FreeVars))
        -> LMatch GhcPs (LocatedA (body GhcPs))
        -> RnM (LMatch GhcRn (LocatedA (body GhcRn)), FreeVars)
rnMatch ctxt rnBody = wrapLocFstMA (rnMatch' ctxt rnBody)

rnMatch' :: (AnnoBody body)
         => HsMatchContext GhcRn
         -> (LocatedA (body GhcPs) -> RnM (LocatedA (body GhcRn), FreeVars))
         -> Match GhcPs (LocatedA (body GhcPs))
         -> RnM (Match GhcRn (LocatedA (body GhcRn)), FreeVars)
rnMatch' ctxt rnBody (Match { m_ctxt = mf, m_pats = pats, m_grhss = grhss })
  = rnLMatchPats ctxt pats $ \ pats' -> do
        { (grhss', grhss_fvs) <- rnGRHSs ctxt rnBody grhss
        ; let mf' = case (ctxt, mf) of
                      (FunRhs { mc_fun = L _ funid }, FunRhs { mc_fun = L lf _ })
                                            -> mf { mc_fun = L lf funid }
                      _                     -> ctxt
        ; return (Match { m_ext = noAnn, m_ctxt = mf', m_pats = pats'
                        , m_grhss = grhss'}, grhss_fvs ) }

emptyCaseErr :: HsMatchContext GhcRn -> TcRnMessage
emptyCaseErr ctxt = TcRnUnknownMessage $ mkPlainError noHints $
  hang (text "Empty list of alternatives in" <+> pp_ctxt ctxt)
        2 (text "Use EmptyCase to allow this")
  where
    pp_ctxt :: HsMatchContext GhcRn -> SDoc
    pp_ctxt c = case c of
      CaseAlt       -> text "case expression"
      LambdaExpr    -> text "\\case expression"
      ArrowMatchCtxt ArrowCaseAlt -> text "case expression"
      ArrowMatchCtxt KappaExpr    -> text "kappa abstraction"
      _             -> text "(unexpected)" <+> pprMatchContextNoun c

{-
************************************************************************
*                                                                      *
\subsubsection{Guarded right-hand sides (GRHSs)}
*                                                                      *
************************************************************************
-}

rnGRHSs :: AnnoBody body
        => HsMatchContext GhcRn
        -> (LocatedA (body GhcPs) -> RnM (LocatedA (body GhcRn), FreeVars))
        -> GRHSs GhcPs (LocatedA (body GhcPs))
        -> RnM (GRHSs GhcRn (LocatedA (body GhcRn)), FreeVars)
rnGRHSs ctxt rnBody (GRHSs _ grhss binds)
  = rnLocalBindsAndThen binds   $ \ binds' _ -> do
    (grhss', fvGRHSs) <- mapFvRn (rnGRHS ctxt rnBody) grhss
    return (GRHSs emptyComments grhss' binds', fvGRHSs)

rnGRHS :: AnnoBody body
       => HsMatchContext GhcRn
       -> (LocatedA (body GhcPs) -> RnM (LocatedA (body GhcRn), FreeVars))
       -> LGRHS GhcPs (LocatedA (body GhcPs))
       -> RnM (LGRHS GhcRn (LocatedA (body GhcRn)), FreeVars)
rnGRHS ctxt rnBody = wrapLocFstMA (rnGRHS' ctxt rnBody)

rnGRHS' :: HsMatchContext GhcRn
        -> (LocatedA (body GhcPs) -> RnM (LocatedA (body GhcRn), FreeVars))
        -> GRHS GhcPs (LocatedA (body GhcPs))
        -> RnM (GRHS GhcRn (LocatedA (body GhcRn)), FreeVars)
rnGRHS' ctxt rnBody (GRHS _ guards rhs)
  = do  { pattern_guards_allowed <- xoptM LangExt.PatternGuards
        ; ((guards', rhs'), fvs) <- rnStmts (PatGuard ctxt) rnExpr guards $ \ _ ->
                                    rnBody rhs

        ; unless (pattern_guards_allowed || is_standard_guard guards') $
            let diag = TcRnUnknownMessage $
                  mkPlainDiagnostic WarningWithoutFlag noHints (nonStdGuardErr guards')
            in addDiagnostic diag

        ; return (GRHS noAnn guards' rhs', fvs) }
  where
        -- Standard Haskell 1.4 guards are just a single boolean
        -- expression, rather than a list of qualifiers as in the
        -- Glasgow extension
    is_standard_guard []                  = True
    is_standard_guard [L _ (BodyStmt {})] = True
    is_standard_guard _                   = False

{-
*********************************************************
*                                                       *
        Source-code fixity declarations
*                                                       *
*********************************************************
-}

rnSrcFixityDecl :: HsSigCtxt -> FixitySig GhcPs -> RnM (FixitySig GhcRn)
-- Rename a fixity decl, so we can put
-- the renamed decl in the renamed syntax tree
-- Errors if the thing being fixed is not defined locally.
rnSrcFixityDecl sig_ctxt = rn_decl
  where
    rn_decl :: FixitySig GhcPs -> RnM (FixitySig GhcRn)
        -- GHC extension: look up both the tycon and data con
        -- for con-like things; hence returning a list
        -- If neither are in scope, report an error; otherwise
        -- return a fixity sig for each (slightly odd)
    rn_decl (FixitySig _ fnames fixity)
      = do names <- concatMapM lookup_one fnames
           return (FixitySig noExtField names fixity)

    lookup_one :: LocatedN RdrName -> RnM [LocatedN Name]
    lookup_one (L name_loc rdr_name)
      = setSrcSpanA name_loc $
                    -- This lookup will fail if the name is not defined in the
                    -- same binding group as this fixity declaration.
        do names <- lookupLocalTcNames sig_ctxt what rdr_name
           return [ L name_loc name | (_, name) <- names ]
    what = text "fixity signature"

{-
************************************************************************
*                                                                      *
\subsection{Error messages}
*                                                                      *
************************************************************************
-}

dupSigDeclErr :: NonEmpty (LocatedN RdrName, Sig GhcPs) -> RnM ()
dupSigDeclErr pairs@((L loc name, sig) :| _)
  = addErrAt (locA loc) $ TcRnUnknownMessage $ mkPlainError noHints $
    vcat [ text "Duplicate" <+> what_it_is
           <> text "s for" <+> quotes (ppr name)
         , text "at" <+> vcat (map ppr $ sortBy SrcLoc.leftmost_smallest
                                       $ map (getLocA . fst)
                                       $ toList pairs)
         ]
  where
    what_it_is = hsSigDoc sig

misplacedSigErr :: LSig GhcRn -> RnM ()
misplacedSigErr (L loc sig)
  = addErrAt (locA loc) $ TcRnUnknownMessage $ mkPlainError noHints $
    sep [text "Misplaced" <+> hsSigDoc sig <> colon, ppr sig]

defaultSigErr :: Sig GhcPs -> TcRnMessage
defaultSigErr sig = TcRnUnknownMessage $ mkPlainError noHints $
  vcat [ hang (text "Unexpected default signature:")
         2 (ppr sig)
       , text "Use DefaultSignatures to enable default signatures" ]

bindInHsBootFileErr :: LHsBindLR GhcRn GhcPs -> RnM ()
bindInHsBootFileErr (L loc _)
  = addErrAt (locA loc) $ TcRnUnknownMessage $ mkPlainError noHints $
      vcat [ text "Bindings in hs-boot files are not allowed" ]

nonStdGuardErr :: (Outputable body,
                   Anno (Stmt GhcRn body) ~ SrcSpanAnnA)
               => [LStmtLR GhcRn GhcRn body] -> SDoc
nonStdGuardErr guards
  = hang (text "accepting non-standard pattern guards (use PatternGuards to suppress this message)")
       4 (interpp'SP guards)

dupMinimalSigErr :: [LSig GhcPs] -> RnM ()
dupMinimalSigErr sigs@(L loc _ : _)
  = addErrAt (locA loc) $ TcRnUnknownMessage $ mkPlainError noHints $
    vcat [ text "Multiple minimal complete definitions"
         , text "at" <+> vcat (map ppr $ sortBy SrcLoc.leftmost_smallest $ map getLocA sigs)
         , text "Combine alternative minimal complete definitions with `|'" ]
dupMinimalSigErr [] = panic "dupMinimalSigErr"
