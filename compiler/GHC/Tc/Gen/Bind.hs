
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

module GHC.Tc.Gen.Bind
   ( tcLocalBinds
   , tcTopBinds
   , tcValBinds
   , tcHsBootSigs
   , tcPolyCheck
   , chooseInferredQuantifiers
   )
where

import GHC.Prelude

import {-# SOURCE #-} GHC.Tc.Gen.Match ( tcGRHSsPat, tcFunBindMatches )
import {-# SOURCE #-} GHC.Tc.Gen.Expr  ( tcCheckMonoExpr )
import {-# SOURCE #-} GHC.Tc.TyCl.PatSyn ( tcPatSynDecl, tcPatSynBuilderBind )

import GHC.Types.Tickish (CoreTickish, GenTickish (..))
import GHC.Types.CostCentre (mkUserCC, mkDeclCCFlavour)
import GHC.Driver.DynFlags
import GHC.Data.FastString
import GHC.Hs

import GHC.Rename.Bind ( rejectBootDecls )

import GHC.Tc.Errors.Types
import GHC.Tc.Gen.Sig
import GHC.Tc.Utils.Concrete ( hasFixedRuntimeRep_syntactic )
import GHC.Tc.Utils.Monad
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Unify
import GHC.Tc.Solver
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Constraint
import GHC.Core.Predicate
import GHC.Core.UsageEnv ( bottomUE )
import GHC.Tc.Gen.HsType
import GHC.Tc.Gen.Pat
import GHC.Tc.Utils.TcMType
import GHC.Tc.Instance.Family( tcGetFamInstEnvs )
import GHC.Tc.Utils.TcType
import GHC.Tc.Validity (checkValidType, checkEscapingKind)
import GHC.Tc.Zonk.TcType
import GHC.Core.Reduction ( Reduction(..) )
import GHC.Core.Multiplicity
import GHC.Core.FamInstEnv( normaliseType )
import GHC.Core.Class   ( Class )
import GHC.Core.Coercion( mkSymCo )
import GHC.Core.Type (mkStrLitTy, tidyOpenTypeX, mkCastTy)
import GHC.Core.TyCo.Ppr( pprTyVars )

import GHC.Builtin.Types ( mkConstraintTupleTy, multiplicityTy, oneDataConTy  )
import GHC.Builtin.Types.Prim
import GHC.Unit.Module

import GHC.Types.SourceText
import GHC.Types.Id
import GHC.Types.Var as Var
import GHC.Types.Var.Set
import GHC.Types.Var.Env( TidyEnv, TyVarEnv, mkVarEnv, lookupVarEnv )
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Name.Env
import GHC.Types.SourceFile
import GHC.Types.SrcLoc

import GHC.Utils.Misc
import GHC.Types.Basic
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import GHC.Builtin.Names( ipClassName )
import GHC.Types.Unique.FM
import GHC.Types.Unique.Set
import qualified GHC.LanguageExtensions as LangExt

import GHC.Data.Bag
import GHC.Data.Graph.Directed
import GHC.Data.Maybe

import Control.Monad
import Data.Foldable (find, traverse_)
import qualified Data.List.NonEmpty as NE

{-
************************************************************************
*                                                                      *
\subsection{Type-checking bindings}
*                                                                      *
************************************************************************

@tcBindsAndThen@ typechecks a @HsBinds@.  The "and then" part is because
it needs to know something about the {\em usage} of the things bound,
so that it can create specialisations of them.  So @tcBindsAndThen@
takes a function which, given an extended environment, E, typechecks
the scope of the bindings returning a typechecked thing and (most
important) an LIE.  It is this LIE which is then used as the basis for
specialising the things bound.

@tcBindsAndThen@ also takes a "combiner" which glues together the
bindings and the "thing" to make a new "thing".

The real work is done by @tcBindWithSigsAndThen@.

Recursive and non-recursive binds are handled in essentially the same
way: because of uniques there are no scoping issues left.  The only
difference is that non-recursive bindings can bind primitive values.

Even for non-recursive binding groups we add typings for each binder
to the LVE for the following reason.  When each individual binding is
checked the type of its LHS is unified with that of its RHS; and
type-checking the LHS of course requires that the binder is in scope.

At the top-level the LIE is sure to contain nothing but constant
dictionaries, which we resolve at the module level.

Note [Polymorphic recursion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The game plan for polymorphic recursion in the code above is

        * Bind any variable for which we have a type signature
          to an Id with a polymorphic type.  Then when type-checking
          the RHSs we'll make a full polymorphic call.

This fine, but if you aren't a bit careful you end up with a horrendous
amount of partial application and (worse) a huge space leak. For example:

        f :: Eq a => [a] -> [a]
        f xs = ...f...

If we don't take care, after typechecking we get

        f = /\a -> \d::Eq a -> let f' = f a d
                               in
                               \ys:[a] -> ...f'...

Notice the stupid construction of (f a d), which is of course
identical to the function we're executing.  In this case, the
polymorphic recursion isn't being used (but that's a very common case).
This can lead to a massive space leak, from the following top-level defn
(post-typechecking)

        ff :: [Int] -> [Int]
        ff = f Int dEqInt

Now (f dEqInt) evaluates to a lambda that has f' as a free variable; but
f' is another thunk which evaluates to the same thing... and you end
up with a chain of identical values all hung onto by the CAF ff.

        ff = f Int dEqInt

           = let f' = f Int dEqInt in \ys. ...f'...

           = let f' = let f' = f Int dEqInt in \ys. ...f'...
                      in \ys. ...f'...

Etc.

NOTE: a bit of arity analysis would push the (f a d) inside the (\ys...),
which would make the space leak go away in this case

Solution: when typechecking the RHSs we always have in hand the
*monomorphic* Ids for each binding.  So we just need to make sure that
if (Method f a d) shows up in the constraints emerging from (...f...)
we just use the monomorphic Id.  We achieve this by adding monomorphic Ids
to the "givens" when simplifying constraints.  That's what the "lies_avail"
is doing.

Then we get

        f = /\a -> \d::Eq a -> letrec
                                 fm = \ys:[a] -> ...fm...
                               in
                               fm
-}

tcTopBinds :: [(RecFlag, LHsBinds GhcRn)] -> [LSig GhcRn]
           -> TcM (TcGblEnv, TcLclEnv)
-- The TcGblEnv contains the new tcg_binds and tcg_spects
-- The TcLclEnv has an extended type envt for the new bindings
tcTopBinds binds sigs
  = do  { -- Pattern synonym bindings populate the global environment
          (binds', (tcg_env, tcl_env)) <- tcValBinds TopLevel binds sigs getEnvs
        ; specs <- tcImpPrags sigs   -- SPECIALISE prags for imported Ids


        ; let { tcg_env' = tcg_env { tcg_imp_specs
                                      = specs ++ tcg_imp_specs tcg_env }
                           `addTypecheckedBinds` map snd binds' }

        ; return (tcg_env', tcl_env) }
        -- The top level bindings are flattened into a giant
        -- implicitly-mutually-recursive LHsBinds

tcHsBootSigs :: [(RecFlag, LHsBinds GhcRn)] -> [LSig GhcRn] -> TcM [Id]
-- A hs-boot file has only one BindGroup, and it only has type
-- signatures in it.  The renamer checked all this.
tcHsBootSigs binds sigs
  = do  { unless (null binds) $
            rejectBootDecls HsBoot BootBindsRn (concatMap snd binds)
        ; concatMapM (addLocM tc_boot_sig) (filter isTypeLSig sigs) }
  where
    tc_boot_sig (TypeSig _ lnames hs_ty) = mapM f lnames
      where
        f (L _ name)
          = do { sigma_ty <- tcHsSigWcType (FunSigCtxt name NoRRC) hs_ty
               ; return (mkVanillaGlobal name sigma_ty) }
        -- Notice that we make GlobalIds, not LocalIds
    tc_boot_sig s = pprPanic "tcHsBootSigs/tc_boot_sig" (ppr s)

------------------------

tcLocalBinds :: HsLocalBinds GhcRn -> TcM thing
             -> TcM (HsLocalBinds GhcTc, thing)

tcLocalBinds (EmptyLocalBinds x) thing_inside
  = do  { thing <- thing_inside
        ; return (EmptyLocalBinds x, thing) }

tcLocalBinds (HsValBinds x (XValBindsLR (NValBinds binds sigs))) thing_inside
  = do  { (binds', thing) <- tcValBinds NotTopLevel binds sigs thing_inside
        ; return (HsValBinds x (XValBindsLR (NValBinds binds' sigs)), thing) }
tcLocalBinds (HsValBinds _ (ValBinds {})) _ = panic "tcLocalBinds"

tcLocalBinds (HsIPBinds x (IPBinds _ ip_binds)) thing_inside
  = do  { ipClass <- tcLookupClass ipClassName
        ; (given_ips, ip_binds') <-
            mapAndUnzipM (wrapLocSndMA (tc_ip_bind ipClass)) ip_binds

        -- Add all the IP bindings as givens for the body of the 'let'
        ; (ev_binds, result) <- checkConstraints (IPSkol ips)
                                  [] given_ips thing_inside

        ; return (HsIPBinds x (IPBinds ev_binds ip_binds') , result) }
  where
    ips = [ip | (L _ (IPBind _ (L _ ip) _)) <- ip_binds]

        -- I wonder if we should do these one at a time
        -- Consider     ?x = 4
        --              ?y = ?x + 1
    tc_ip_bind :: Class -> IPBind GhcRn -> TcM (DictId, IPBind GhcTc)
    tc_ip_bind ipClass (IPBind _ l_name@(L _ ip) expr)
       = do { ty <- newFlexiTyVarTy liftedTypeKind  -- see #24298
            ; let p = mkStrLitTy $ hsIPNameFS ip
            ; ip_id <- newDict ipClass [ p, ty ]
            ; expr' <- tcCheckMonoExpr expr ty
            ; let d = fmap (toDict ipClass p ty) expr'
            ; return (ip_id, (IPBind ip_id l_name d)) }

    -- Coerces a `t` into a dictionary for `IP "x" t`.
    -- co : t -> IP "x" t
    toDict :: Class  -- IP class
           -> Type   -- type-level string for name of IP
           -> Type   -- type of IP
           -> HsExpr GhcTc   -- def'n of IP variable
           -> HsExpr GhcTc   -- dictionary for IP
    toDict ipClass x ty = mkHsWrap $ mkWpCastR $
                          wrapIP $ mkClassPred ipClass [x,ty]

tcValBinds :: TopLevelFlag
           -> [(RecFlag, LHsBinds GhcRn)] -> [LSig GhcRn]
           -> TcM thing
           -> TcM ([(RecFlag, LHsBinds GhcTc)], thing)

tcValBinds top_lvl binds sigs thing_inside
  = do  {   -- Typecheck the signatures
            -- It's easier to do so now, once for all the SCCs together
            -- because a single signature  f,g :: <type>
            -- might relate to more than one SCC
          (poly_ids, sig_fn) <- tcAddPatSynPlaceholders patsyns $
                                tcTySigs sigs

        -- Extend the envt right away with all the Ids
        --   declared with complete type signatures
        -- Do not extend the TcBinderStack; instead
        --   we extend it on a per-rhs basis in tcExtendForRhs
        --   See Note [Relevant bindings and the binder stack]
        --
        -- For the moment, let bindings and top-level bindings introduce
        -- only unrestricted variables.
        ; tcExtendSigIds top_lvl poly_ids $
     do { (binds', (extra_binds', thing))
              <- tcBindGroups top_lvl sig_fn prag_fn binds $
                 do { thing <- thing_inside
                       -- See Note [Pattern synonym builders don't yield dependencies]
                       --     in GHC.Rename.Bind
                    ; patsyn_builders <- mapM (tcPatSynBuilderBind prag_fn) patsyns
                    ; let extra_binds = [ (NonRecursive, builder)
                                        | builder <- patsyn_builders ]
                    ; return (extra_binds, thing) }
        ; return (binds' ++ extra_binds', thing) }}
  where
    patsyns = getPatSynBinds binds
    prag_fn = mkPragEnv sigs (concatMap snd binds)

------------------------

tcBindGroups :: TopLevelFlag -> TcSigFun -> TcPragEnv
             -> [(RecFlag, LHsBinds GhcRn)] -> TcM thing
             -> TcM ([(RecFlag, LHsBinds GhcTc)], thing)
-- Typecheck a whole lot of value bindings,
-- one strongly-connected component at a time
-- Here a "strongly connected component" has the straightforward
-- meaning of a group of bindings that mention each other,
-- ignoring type signatures (that part comes later)

tcBindGroups _ _ _ [] thing_inside
  = do  { thing <- thing_inside
        ; return ([], thing) }

tcBindGroups top_lvl sig_fn prag_fn (group : groups) thing_inside
  = do  { -- See Note [Closed binder groups]
          type_env <- getLclTypeEnv
        ; let closed = isClosedBndrGroup type_env (snd group)
        ; (group', (groups', thing))
                <- tc_group top_lvl sig_fn prag_fn group closed $
                   tcBindGroups top_lvl sig_fn prag_fn groups thing_inside
        ; return (group' ++ groups', thing) }

-- Note [Closed binder groups]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
--  A mutually recursive group is "closed" if all of the free variables of
--  the bindings are closed. For example
--
-- >  h = \x -> let f = ...g...
-- >                g = ....f...x...
-- >             in ...
--
-- Here @g@ is not closed because it mentions @x@; and hence neither is @f@
-- closed.
--
-- So we need to compute closed-ness on each strongly connected components,
-- before we sub-divide it based on what type signatures it has.
--

------------------------
tc_group :: forall thing.
            TopLevelFlag -> TcSigFun -> TcPragEnv
         -> (RecFlag, LHsBinds GhcRn) -> IsGroupClosed -> TcM thing
         -> TcM ([(RecFlag, LHsBinds GhcTc)], thing)

-- Typecheck one strongly-connected component of the original program.
-- We get a list of groups back, because there may
-- be specialisations etc as well

tc_group top_lvl sig_fn prag_fn (NonRecursive, binds) closed thing_inside
        -- A single non-recursive binding
        -- We want to keep non-recursive things non-recursive
        -- so that we desugar unlifted bindings correctly
  = do { let bind = case binds of
                 [bind] -> bind
                 []     -> panic "tc_group: empty list of binds"
                 _      -> panic "tc_group: NonRecursive binds is not a singleton bag"
       ; (bind', thing) <- tc_single top_lvl sig_fn prag_fn bind closed
                             thing_inside
       ; return ( [(NonRecursive, bind')], thing) }

tc_group top_lvl sig_fn prag_fn (Recursive, binds) closed thing_inside
  =     -- To maximise polymorphism, we do a new
        -- strongly-connected-component analysis, this time omitting
        -- any references to variables with type signatures.
        -- (This used to be optional, but isn't now.)
        -- See Note [Polymorphic recursion] in "GHC.Hs.Binds".
    do  { traceTc "tc_group rec" (pprLHsBinds binds)
        ; whenIsJust mbFirstPatSyn $ \lpat_syn ->
            recursivePatSynErr (locA $ getLoc lpat_syn) binds
        ; (binds1, thing) <- go sccs
        ; return ([(Recursive, binds1)], thing) }
                -- Rec them all together
  where
    mbFirstPatSyn = find (isPatSyn . unLoc) binds
    isPatSyn PatSynBind{} = True
    isPatSyn _ = False

    sccs :: [SCC (LHsBind GhcRn)]
    sccs = stronglyConnCompFromEdgedVerticesUniq (mkEdges sig_fn binds)

    go :: [SCC (LHsBind GhcRn)] -> TcM (LHsBinds GhcTc, thing)
    go (scc:sccs) = do  { (binds1, ids1) <- tc_scc scc
                         -- recursive bindings must be unrestricted
                         -- (the ids added to the environment here are the name of the recursive definitions).
                        ; (binds2, thing) <-
                              tcExtendLetEnv top_lvl sig_fn closed ids1
                              (go sccs)
                        ; return (binds1 ++ binds2, thing) }
    go []         = do  { thing <- thing_inside; return ([], thing) }

    tc_scc (AcyclicSCC bind) = tc_sub_group NonRecursive [bind]
    tc_scc (NECyclicSCC binds) = tc_sub_group Recursive  (NE.toList binds)

    tc_sub_group rec_tc binds = tcPolyBinds top_lvl sig_fn prag_fn
                                            Recursive rec_tc closed binds

recursivePatSynErr
  :: SrcSpan -- ^ The location of the first pattern synonym binding
             --   (for error reporting)
  -> LHsBinds GhcRn
  -> TcM a
recursivePatSynErr loc binds
  = failAt loc $ TcRnRecursivePatternSynonym binds

tc_single :: forall thing.
            TopLevelFlag -> TcSigFun -> TcPragEnv
          -> LHsBind GhcRn -> IsGroupClosed -> TcM thing
          -> TcM (LHsBinds GhcTc, thing)
tc_single _top_lvl sig_fn prag_fn
          (L loc (PatSynBind _ psb))
          _ thing_inside
  = do { (aux_binds, tcg_env) <- tcPatSynDecl (L loc psb) sig_fn prag_fn
       ; thing <- setGblEnv tcg_env thing_inside
       ; return (aux_binds, thing)
       }

tc_single top_lvl sig_fn prag_fn lbind closed thing_inside
  = do { (binds1, ids) <- tcPolyBinds top_lvl sig_fn prag_fn
                                      NonRecursive NonRecursive
                                      closed
                                      [lbind]
       ; thing <- tcExtendLetEnv top_lvl sig_fn closed ids thing_inside
       ; return (binds1, thing) }

------------------------
type BKey = Int -- Just number off the bindings

mkEdges :: TcSigFun -> LHsBinds GhcRn -> [Node BKey (LHsBind GhcRn)]
-- See Note [Polymorphic recursion] in "GHC.Hs.Binds".
mkEdges sig_fn binds
  = [ DigraphNode bind key [key | n <- nonDetEltsUniqSet (bind_fvs (unLoc bind)),
                         Just key <- [lookupNameEnv key_map n], no_sig n ]
    | (bind, key) <- keyd_binds
    ]
    -- It's OK to use nonDetEltsUFM here as stronglyConnCompFromEdgedVertices
    -- is still deterministic even if the edges are in nondeterministic order
    -- as explained in Note [Deterministic SCC] in GHC.Data.Graph.Directed.
  where
    bind_fvs (FunBind { fun_ext = fvs }) = fvs
    bind_fvs (PatBind { pat_ext = fvs }) = fvs
    bind_fvs _                           = emptyNameSet

    no_sig :: Name -> Bool
    no_sig n = not (hasCompleteSig sig_fn n)

    keyd_binds = binds `zip` [0::BKey ..]

    key_map :: NameEnv BKey     -- Which binding it comes from
    key_map = mkNameEnv [(bndr, key) | (L _ bind, key) <- keyd_binds
                                     , bndr <- collectHsBindBinders CollNoDictBinders bind ]

------------------------
tcPolyBinds :: TopLevelFlag -> TcSigFun -> TcPragEnv
            -> RecFlag         -- Whether the group is really recursive
            -> RecFlag         -- Whether it's recursive after breaking
                               -- dependencies based on type signatures
            -> IsGroupClosed   -- Whether the group is closed
            -> [LHsBind GhcRn]  -- None are PatSynBind
            -> TcM (LHsBinds GhcTc, [Scaled TcId])

-- Typechecks a single bunch of values bindings all together,
-- and generalises them.  The bunch may be only part of a recursive
-- group, because we use type signatures to maximise polymorphism
--
-- Returns a list because the input may be a single non-recursive binding,
-- in which case the dependency order of the resulting bindings is
-- important.
--
-- Knows nothing about the scope of the bindings
-- None of the bindings are pattern synonyms

tcPolyBinds top_lvl sig_fn prag_fn rec_group rec_tc closed bind_list
  = setSrcSpan loc                              $
    recoverM (recoveryCode binder_names sig_fn) $ do
        -- Set up main recover; take advantage of any type sigs

    { traceTc "------------------------------------------------" Outputable.empty
    ; traceTc "Bindings for {" (ppr binder_names)
    ; dflags   <- getDynFlags
    ; let plan = decideGeneralisationPlan dflags top_lvl closed sig_fn bind_list
    ; traceTc "Generalisation plan" (ppr plan)
    ; result@(_, scaled_poly_ids) <- case plan of
         NoGen              -> tcPolyNoGen         rec_tc prag_fn sig_fn bind_list
         InferGen           -> tcPolyInfer top_lvl rec_tc prag_fn sig_fn bind_list
         CheckGen lbind sig -> tcPolyCheck prag_fn sig lbind

    ; let poly_ids = map scaledThing scaled_poly_ids

    ; mapM_ (\ poly_id ->
        hasFixedRuntimeRep_syntactic (FRRBinder $ idName poly_id) (idType poly_id))
        poly_ids

    ; traceTc "} End of bindings for" (vcat [ ppr binder_names, ppr rec_group
                                            , vcat [ppr id <+> ppr (idType id) | id <- poly_ids]
                                          ])

    ; return result }
  where
    binder_names = collectHsBindListBinders CollNoDictBinders bind_list
    loc = foldr1 combineSrcSpans (map (locA . getLoc) bind_list)
         -- The mbinds have been dependency analysed and
         -- may no longer be adjacent; so find the narrowest
         -- span that includes them all

--------------
-- If typechecking the binds fails, then return with each
-- signature-less binder given type (forall a.a), to minimise
-- subsequent error messages
recoveryCode :: [Name] -> TcSigFun -> TcM (LHsBinds GhcTc, [Scaled Id])
recoveryCode binder_names sig_fn
  = do  { traceTc "tcBindsWithSigs: error recovery" (ppr binder_names)
        ; let poly_ids = map (Scaled ManyTy) $ map mk_dummy binder_names
        ; return ([], poly_ids) }
  where
    mk_dummy name
      | Just sig <- sig_fn name
      , Just poly_id <- completeSigPolyId_maybe sig
      = poly_id
      | otherwise
      = mkLocalId name ManyTy forall_a_a

forall_a_a :: TcType
-- At one point I had (forall r (a :: TYPE r). a), but of course
-- that type is ill-formed: its mentions 'r' which escapes r's scope.
-- Another alternative would be (forall (a :: TYPE kappa). a), where
-- kappa is a unification variable. But I don't think we need that
-- complication here. I'm going to just use (forall (a::*). a).
-- See #15276
forall_a_a = mkSpecForAllTys [alphaTyVar] alphaTy

{- *********************************************************************
*                                                                      *
                         tcPolyNoGen
*                                                                      *
********************************************************************* -}

tcPolyNoGen     -- No generalisation whatsoever
  :: RecFlag       -- Whether it's recursive after breaking
                   -- dependencies based on type signatures
  -> TcPragEnv -> TcSigFun
  -> [LHsBind GhcRn]
  -> TcM (LHsBinds GhcTc, [Scaled TcId])

tcPolyNoGen rec_tc prag_fn tc_sig_fn bind_list
  = do { (binds', mono_infos) <- tcMonoBinds rec_tc tc_sig_fn
                                             (LetGblBndr prag_fn)
                                             bind_list
       ; mono_ids' <- mapM tc_mono_info mono_infos
       ; return (binds', mono_ids') }
  where
    tc_mono_info (MBI { mbi_poly_name = name, mbi_mono_id = mono_id, mbi_mono_mult = mult })
      = do { _specs <- tcSpecPrags mono_id (lookupPragEnv prag_fn name)
           ; return $ Scaled mult mono_id }
           -- NB: tcPrags generates error messages for
           --     specialisation pragmas for non-overloaded sigs
           -- Indeed that is why we call it here!
           -- So we can safely ignore _specs


{- *********************************************************************
*                                                                      *
                         tcPolyCheck
*                                                                      *
********************************************************************* -}

tcPolyCheck :: TcPragEnv
            -> TcCompleteSig
            -> LHsBind GhcRn   -- Must be a FunBind
            -> TcM (LHsBinds GhcTc, [Scaled TcId])
-- There is just one binding,
--   it is a FunBind
--   it has a complete type signature,
tcPolyCheck prag_fn
            sig@(CSig { sig_bndr = poly_id, sig_ctxt = ctxt })
            (L bind_loc (FunBind { fun_id = L nm_loc name
                                 , fun_matches = matches }))
  = do { traceTc "tcPolyCheck" (ppr sig)

       -- Make a new Name, whose SrcSpan is nm_loc.  For a ClassOp
       -- The original Name, in the FunBind{fun_id}, is bound in the
       -- class declaration, whereas we want a Name bound right here.
       -- We pass mono_name to tcFunBindMatches which in turn puts it in
       -- the BinderStack, whence it shows up in "Relevant bindings.."
       ; mono_name <- newNameAt (nameOccName name) (locA nm_loc)

       ; mult <- tcMultAnn (HsNoMultAnn noExtField)
       ; (wrap_gen, (wrap_res, matches'))
             <- tcSkolemiseCompleteSig sig $ \invis_pat_tys rho_ty ->

                let mono_id = mkLocalId mono_name (idMult poly_id) rho_ty in
                tcExtendBinderStack [TcIdBndr mono_id NotTopLevel] $
                -- Why mono_id in the BinderStack?
                -- See Note [Relevant bindings and the binder stack]

                setSrcSpanA bind_loc  $
                tcFunBindMatches ctxt mono_name mult matches invis_pat_tys (mkCheckExpType rho_ty)

       -- We make a funny AbsBinds, abstracting over nothing,
       -- just so we have somewhere to put the SpecPrags.
       -- Otherwise we could just use the FunBind
       -- Hence poly_id2 is just a clone of poly_id;
       -- We re-use mono-name, but we could equally well use a fresh one

       ; let prag_sigs = lookupPragEnv prag_fn name
             poly_id2  = mkLocalId mono_name (idMult poly_id) (idType poly_id)
       ; spec_prags <- tcSpecPrags    poly_id prag_sigs
       ; poly_id    <- addInlinePrags poly_id prag_sigs

       ; mod <- getModule
       ; tick <- funBindTicks (locA nm_loc) poly_id mod prag_sigs

       ; let bind' = FunBind { fun_id      = L nm_loc poly_id2
                             , fun_matches = matches'
                             , fun_ext     = (wrap_gen <.> wrap_res, tick) }

             export = ABE { abe_wrap  = idHsWrapper
                          , abe_poly  = poly_id
                          , abe_mono  = poly_id2
                          , abe_prags = SpecPrags spec_prags }

             abs_bind = L bind_loc $ XHsBindsLR $
                        AbsBinds { abs_tvs      = []
                                 , abs_ev_vars  = []
                                 , abs_ev_binds = []
                                 , abs_exports  = [export]
                                 , abs_binds    = [L bind_loc bind']
                                 , abs_sig      = True }

       ; return ([abs_bind], [Scaled mult poly_id]) }

tcPolyCheck _prag_fn sig bind
  = pprPanic "tcPolyCheck" (ppr sig $$ ppr bind)

funBindTicks :: SrcSpan -> TcId -> Module -> [LSig GhcRn]
             -> TcM [CoreTickish]
funBindTicks loc fun_id mod sigs
  | (mb_cc_str : _) <- [ cc_name | L _ (SCCFunSig _ _ cc_name) <- sigs ]
      -- this can only be a singleton list, as duplicate pragmas are rejected
      -- by the renamer
  , let cc_str
          | Just cc_str <- mb_cc_str
          = sl_fs $ unLoc cc_str
          | otherwise
          = getOccFS (Var.varName fun_id)
        cc_name = concatFS [moduleNameFS (moduleName mod), fsLit ".", cc_str]
  = do
      flavour <- mkDeclCCFlavour <$> getCCIndexTcM cc_name
      let cc = mkUserCC cc_name mod loc flavour
      return [ProfNote cc True True]
  | otherwise
  = return []

{- Note [Instantiate sig with fresh variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's vital to instantiate a type signature with fresh variables.
For example:
      type T = forall a. [a] -> [a]
      f :: T;
      f = g where { g :: T; g = <rhs> }

 We must not use the same 'a' from the defn of T at both places!!
(Instantiation is only necessary because of type synonyms.  Otherwise,
it's all cool; each signature has distinct type variables from the renamer.)
-}


{- *********************************************************************
*                                                                      *
                         tcPolyInfer
*                                                                      *
********************************************************************* -}

{- Note [Non-variable pattern bindings aren't linear]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A fundamental limitation of the typechecking algorithm is that we cannot have a
binding which, at the same time,
- is linear in its rhs
- is a non-variable pattern
- binds variables to polymorphic or qualified types

A detailed explanation can be found at:
https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst#let-bindings-and-polymorphism

To address this we to do a few things

- (NVP1) When a pattern is annotated with a multiplicity annotation `let %q pat = rhs
  in body` (note: multiplicity-annotated bindings are always parsed as a
  PatBind, see Note [Multiplicity annotations] in Language.Haskell.Syntax.Binds),
  then the let is never generalised (we use the NoGen plan). We do this with a
  dedicated test in decideGeneralisationPlan.
- (NVP2) Whenever the typechecker infers an AbsBind *and* the inner binding is a
  non-variable PatBind, then the multiplicity of the binding is inferred to be
  Many. We do this by calling manyIfPats in tcPolyInfer. This is a little
  infelicitous: sometimes the typechecker infers an AbsBind where it didn't need
  to. This may cause some programs to be spuriously rejected, when
  NoMonoLocalBinds is on.
- (NVP3) LinearLet implies MonoLocalBinds to avoid the AbsBind case altogether.
- (NVP4) Wrinkle: even when other conditions (including MonoLocalBinds), GHC
  will generalise some binders, namely so-called closed binding groups. We need
  to make sure that the test for (NVP1) has priority over the test for closed
  binders.
- (NVP5) Wrinkle: Closed binding groups (NVP4) are usually fine to type with
  multiplicity Many. But there's one exception: when there's no binder at all,
  the binding group is considered closed. Even if the rhs contains arbitrary
  variables.

     f :: () %1 -> Bool
     f x = let !() = x in True

  If we consider `!() = x` as a generalisable group (which does nothing anyway),
  then (NVP2) will infer the pattern as multiplicity Many, and reject the
  function. We don't want that, see also #25428. So we take care not to
  generalise in this case, by excluding the no-binder case from automatic
  generalisation in decideGeneralisationPlan.
-}

tcPolyInfer
  :: TopLevelFlag
  -> RecFlag       -- Whether it's recursive after breaking
                   -- dependencies based on type signatures
  -> TcPragEnv -> TcSigFun
  -> [LHsBind GhcRn]
  -> TcM (LHsBinds GhcTc, [Scaled TcId])
tcPolyInfer top_lvl rec_tc prag_fn tc_sig_fn bind_list
  = do { (tclvl, wanted, (binds', mono_infos))
             <- pushLevelAndCaptureConstraints  $
                tcMonoBinds rec_tc tc_sig_fn LetLclBndr bind_list

       ; apply_mr <- checkMonomorphismRestriction mono_infos bind_list

       -- AbsBinds which are PatBinds can't be linear.
       -- See (NVP2) in Note [Non-variable pattern bindings aren't linear]
       ; manyIfPats binds'

       ; traceTc "tcPolyInfer" (ppr apply_mr $$ ppr (map mbi_sig mono_infos))

       ; let name_taus  = [ (mbi_poly_name info, idType (mbi_mono_id info))
                          | info <- mono_infos ]
             sigs       = [ sig | MBI { mbi_sig = Just sig } <- mono_infos ]
             infer_mode = if apply_mr then ApplyMR else NoRestrictions

       ; traceTc "simplifyInfer call" (ppr tclvl $$ ppr name_taus $$ ppr wanted)
       ; ((qtvs, givens, ev_binds, insoluble), residual)
            <- captureConstraints $ simplifyInfer top_lvl tclvl infer_mode sigs name_taus wanted

       ; let inferred_theta = map evVarPred givens
       ; scaled_exports <- checkNoErrs $
                    mapM (mkExport prag_fn residual insoluble qtvs inferred_theta) mono_infos
       ; let exports = map scaledThing scaled_exports

         -- NB: *after* the checkNoErrs call above. This ensures that we don't get an error
         -- cascade in case mkExport runs into trouble. In particular, this avoids duplicate
         -- errors when a partial type signature cannot be quantified in chooseInferredQuantifiers.
         -- See Note [Quantification and partial signatures] in GHC.Tc.Solver, Wrinkle 4.
         -- Tested in partial-sigs/should_fail/NamedWilcardExplicitForall.
       ; emitConstraints residual

       ; loc <- getSrcSpanM
       ; let scaled_poly_ids = [ Scaled p (abe_poly export) | Scaled p export <- scaled_exports]
             poly_ids = map scaledThing scaled_poly_ids
             abs_bind = L (noAnnSrcSpan loc) $ XHsBindsLR $
                        AbsBinds { abs_tvs = qtvs
                                 , abs_ev_vars = givens, abs_ev_binds = [ev_binds]
                                 , abs_exports = exports, abs_binds = binds'
                                 , abs_sig = False }

       ; traceTc "Binding:" (ppr (poly_ids `zip` map idType poly_ids))
       ; return ([abs_bind], scaled_poly_ids) }
         -- poly_ids are guaranteed zonked by mkExport
  where
    manyIfPat (L _ (PatBind{pat_lhs=(L _ (VarPat{}))}))
      = return ()
    manyIfPat (L _ (PatBind {pat_mult=mult_ann}))
      = tcSubMult (NonLinearPatternOrigin GeneralisedPatternReason nlWildPatName) ManyTy (getTcMultAnn mult_ann)
    manyIfPat _ = return ()
    manyIfPats binds' = traverse_ manyIfPat binds'

checkMonomorphismRestriction :: [MonoBindInfo] -> [LHsBind GhcRn] -> TcM Bool
-- True <=> apply the MR
-- See Note [When the MR applies]
checkMonomorphismRestriction mbis lbinds
  = do { mr_on <- xoptM LangExt.MonomorphismRestriction
       ; let mr_applies = mr_on && any (restricted . unLoc) lbinds
       ; when mr_applies $ mapM_ checkOverloadedSig mbis
       ; return mr_applies }
  where
    no_mr_bndrs :: NameSet
    no_mr_bndrs = mkNameSet (mapMaybe no_mr_name mbis)

    no_mr_name :: MonoBindInfo -> Maybe Name
    -- Just n for binders that have a signature that says "no MR needed for me"
    no_mr_name (MBI { mbi_sig = Just sig })
       | TISI { sig_inst_sig = info, sig_inst_theta = theta, sig_inst_wcx = wcx } <- sig
       = case info of
           TcCompleteSig (CSig { sig_bndr = bndr }) -> Just (idName bndr)
           TcPartialSig (PSig { psig_name = nm })
             | null theta, isNothing wcx   -> Nothing  -- f :: _ -> _
             | otherwise                   -> Just nm  -- f :: Num a => a -> _
             -- For the latter case, we don't want the MR:
             -- the user has explicitly specified a type-class context
    no_mr_name _ = Nothing

    -- The Haskell 98 monomorphism restriction
    restricted :: HsBindLR GhcRn GhcRn -> Bool
    restricted (PatBind {})                              = True
    restricted (FunBind { fun_id = v, fun_matches = m }) = restricted_match m
                                                           && mr_needed_for (unLoc v)
    restricted (VarBind { var_ext = x })                 = dataConCantHappen x
    restricted b@(PatSynBind {}) = pprPanic "isRestrictedGroup/unrestricted" (ppr b)

    restricted_match mg = matchGroupArity mg == 0
        -- No args => like a pattern binding
        -- Some args => a function binding

    mr_needed_for nm = not (nm `elemNameSet` no_mr_bndrs)

checkOverloadedSig :: MonoBindInfo -> TcM ()
-- Example:
--   f :: Eq a => a -> a
--   K f = e
-- The MR applies, but the signature is overloaded, and it's
-- best to complain about this directly
-- c.f #11339
checkOverloadedSig (MBI { mbi_sig = Just sig })
  | TISI { sig_inst_sig = orig_sig, sig_inst_theta = theta, sig_inst_wcx = wcx } <- sig
  , not (null theta && isNothing wcx)
  = setSrcSpan (tcIdSigLoc orig_sig) $
    failWith $ TcRnOverloadedSig orig_sig
checkOverloadedSig _ = return ()

{- Note [When the MR applies]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Monomorphism Restriction (MR) applies as specifies in the Haskell Report:

* If -XMonomorphismRestriction is on, and
* Any binding is restricted

A binding is restricted if:
* It is a pattern binding e.g. (x,y) = e
* Or it is a FunBind with no arguments e.g. f = rhs
     and the binder `f` lacks a No-MR signature

A binder f has a No-MR signature if

* It has a complete type signature
    e.g. f :: Num a => a -> a

* Or it has a /partial/ type signature with a /context/
    e.g.  f :: (_) => a -> _
          g :: Num a => a -> _
          h :: (Num a, _) => a -> _
   All of f,g,h have a No-MR signature.  They say that the function is overloaded
   so it's silly to try to apply the MR. This means that #19106 works out
   fine.  Ditto #11016, which looked like
      f4 :: (?loc :: Int) => _
      f4 = ?loc

   This partial-signature stuff is a bit ad-hoc but seems to match our
   use-cases.  See also Note [Constraints in partial type signatures]
   in GHC.Tc.Solver.

Example: the MR does apply to
   k :: _ -> _
   k = rhs
because k's binding has no arguments, and `k` does not have
a No-MR signature.

All of this checking takes place after synonym expansion.  For example:
   type Wombat a = forall b. Eq [b] => ...b...a...
   f5 :: Wombat _
This (and does) behave just like
   f5 :: forall b. Eq [b] => ...b..._...

-}

--------------
mkExport :: TcPragEnv
         -> WantedConstraints  -- residual constraints, already emitted (for errors only)
         -> Bool                        -- True <=> there was an insoluble type error
                                        --          when typechecking the bindings
         -> [TyVar] -> TcThetaType      -- Both already zonked
         -> MonoBindInfo
         -> TcM (Scaled ABExport)
-- Only called for generalisation plan InferGen, not by CheckGen or NoGen
--
-- mkExport generates exports with
--      zonked type variables,
--      zonked poly_ids
-- The former is just because no further unifications will change
-- the quantified type variables, so we can fix their final form
-- right now.
-- The latter is needed because the poly_ids are used to extend the
-- type environment; see the invariant on GHC.Tc.Utils.Env.tcExtendIdEnv

-- Pre-condition: the qtvs and theta are already zonked

mkExport prag_fn residual insoluble qtvs theta
         (MBI { mbi_poly_name = poly_name
              , mbi_sig       = mb_sig
              , mbi_mono_id   = mono_id
              , mbi_mono_mult = mono_mult })
  = do  { mono_ty <- liftZonkM $ zonkTcType (idType mono_id)
        ; poly_id <- mkInferredPolyId residual insoluble qtvs theta poly_name mb_sig mono_ty

        -- NB: poly_id has a zonked type
        ; poly_id <- addInlinePrags poly_id prag_sigs
        ; spec_prags <- tcSpecPrags poly_id prag_sigs
                -- tcPrags requires a zonked poly_id

        -- See Note [Impedance matching]
        -- NB: we have already done checkValidType, including an ambiguity check,
        --     on the type; either when we checked the sig or in mkInferredPolyId
        ; let poly_ty     = idType poly_id
              sel_poly_ty = mkInfSigmaTy qtvs theta mono_ty
                -- This type is just going into tcSubType,
                -- so Inferred vs. Specified doesn't matter

        ; traceTc "mkExport" (vcat [ ppr poly_id <+> dcolon <+> ppr poly_ty
                                   , ppr sel_poly_ty ])

        ; wrap <- if sel_poly_ty `eqType` poly_ty  -- NB: eqType ignores visibility
                  then return idHsWrapper  -- Fast path; also avoids complaint when we infer
                                           -- an ambiguous type and have AllowAmbiguousType
                                           -- e..g infer  x :: forall a. F a -> Int
                  else tcSubTypeSigma (ImpedanceMatching poly_id)
                                      sig_ctxt sel_poly_ty poly_ty
                       -- See Note [Impedance matching]

        ; localSigWarn poly_id mb_sig

        ; return (Scaled mono_mult $
                  ABE { abe_wrap = wrap
                        -- abe_wrap :: (forall qtvs. theta => mono_ty) ~ idType poly_id
                      , abe_poly  = poly_id
                      , abe_mono  = mono_id
                      , abe_prags = SpecPrags spec_prags }) }
  where
    prag_sigs = lookupPragEnv prag_fn poly_name
    sig_ctxt  = InfSigCtxt poly_name

mkInferredPolyId :: WantedConstraints   -- the residual constraints, already emitted
                 -> Bool  -- True <=> there was an insoluble error when
                          --          checking the binding group for this Id
                 -> [TyVar] -> TcThetaType
                 -> Name -> Maybe TcIdSigInst -> TcType
                 -> TcM TcId
mkInferredPolyId residual insoluble qtvs inferred_theta poly_name mb_sig_inst mono_ty
  | Just (TISI { sig_inst_sig = sig })  <- mb_sig_inst
  , TcCompleteSig (CSig { sig_bndr = poly_id }) <- sig
  = return poly_id

  | otherwise  -- Either no type sig or partial type sig
  = checkNoErrs $  -- The checkNoErrs ensures that if the type is ambiguous
                   -- we don't carry on to the impedance matching, and generate
                   -- a duplicate ambiguity error.  There is a similar
                   -- checkNoErrs for complete type signatures too.
    do { fam_envs <- tcGetFamInstEnvs
       ; let mono_ty' = reductionReducedType $ normaliseType fam_envs Nominal mono_ty
               -- Unification may not have normalised the type,
               -- so do it here to make it as uncomplicated as possible.
               -- Example: f :: [F Int] -> Bool
               -- should be rewritten to f :: [Char] -> Bool, if possible
               --
               -- We can discard the coercion _co, because we'll reconstruct
               -- it in the call to tcSubType below

       ; (binders, theta') <- chooseInferredQuantifiers residual inferred_theta
                                (tyCoVarsOfType mono_ty') qtvs mb_sig_inst

       ; let inferred_poly_ty = mkInvisForAllTys binders (mkPhiTy theta' mono_ty')

       ; traceTc "mkInferredPolyId" (vcat [ppr poly_name, ppr qtvs, ppr theta'
                                          , ppr inferred_poly_ty
                                          , text "insoluble" <+> ppr insoluble ])

       ; unless insoluble $
         addErrCtxtM (mk_inf_msg poly_name inferred_poly_ty) $
         do { checkEscapingKind inferred_poly_ty
                 -- See Note [Inferred type with escaping kind]
            ; checkValidType (InfSigCtxt poly_name) inferred_poly_ty }
                 -- See Note [Validity of inferred types]
         -- unless insoluble: if we found an insoluble error in the
         -- function definition, don't do this check; otherwise
         -- (#14000) we may report an ambiguity error for a rather
         -- bogus type.

       ; return (mkLocalId poly_name ManyTy inferred_poly_ty) }


chooseInferredQuantifiers :: WantedConstraints  -- residual constraints
                          -> TcThetaType   -- inferred
                          -> TcTyVarSet    -- tvs free in tau type
                          -> [TcTyVar]     -- inferred quantified tvs
                          -> Maybe TcIdSigInst
                          -> TcM ([InvisTVBinder], TcThetaType)
chooseInferredQuantifiers _residual inferred_theta tau_tvs qtvs Nothing
  = -- No type signature (partial or complete) for this binder,
    do { let free_tvs = closeOverKinds (growThetaTyVars inferred_theta tau_tvs)
                        -- See Note [growThetaTyVars vs closeWrtFunDeps] in GHC.Tc.Solver
                        -- Include kind variables!  #7916
             my_theta = pickCapturedPreds free_tvs inferred_theta
             binders  = [ mkTyVarBinder InferredSpec tv
                        | tv <- qtvs
                        , tv `elemVarSet` free_tvs ]
       ; return (binders, my_theta) }

chooseInferredQuantifiers residual inferred_theta tau_tvs qtvs
    (Just (TISI { sig_inst_sig = sig, sig_inst_wcx = wcx
                , sig_inst_theta = annotated_theta, sig_inst_skols = annotated_tvs }))
  | TcPartialSig (PSig { psig_name = fn_name, psig_hs_ty = hs_ty }) <- sig
  = -- Choose quantifiers for a partial type signature
    do { let (psig_qtv_nms, psig_qtv_bndrs) = unzip annotated_tvs
       ; psig_qtv_bndrs <- liftZonkM $ mapM zonkInvisTVBinder psig_qtv_bndrs
       ; let psig_qtvs    = map binderVar psig_qtv_bndrs
             psig_qtv_set = mkVarSet psig_qtvs
             psig_qtv_prs = psig_qtv_nms `zip` psig_qtvs
             psig_bndr_map :: TyVarEnv InvisTVBinder
             psig_bndr_map = mkVarEnv [ (binderVar tvb, tvb) | tvb <- psig_qtv_bndrs ]

            -- Check whether the quantified variables of the
            -- partial signature have been unified together
            -- See Note [Quantified variables in partial type signatures]
       ; mapM_ (report_dup_tyvar_tv_err fn_name hs_ty) $
         findDupTyVarTvs psig_qtv_prs

            -- Check whether a quantified variable of the partial type
            -- signature is not actually quantified.  How can that happen?
            -- See Note [Quantification and partial signatures] Wrinkle 4
            --     in GHC.Tc.Solver
       ; mapM_ (report_mono_sig_tv_err fn_name hs_ty)
         [ pr | pr@(_,tv) <- psig_qtv_prs, not (tv `elem` qtvs) ]

       ; annotated_theta      <- liftZonkM $ zonkTcTypes annotated_theta
       ; (free_tvs, my_theta) <- choose_psig_context psig_qtv_set annotated_theta wcx
                                 -- NB: free_tvs includes tau_tvs

       ; let (_,final_qtvs) = foldr (choose_qtv psig_bndr_map) (free_tvs, []) qtvs
                              -- Pulling from qtvs maintains original order
                              -- NB: qtvs is already in dependency order

       ; traceTc "chooseInferredQuantifiers" $
         vcat [ text "qtvs" <+> pprTyVars qtvs
              , text "psig_qtv_bndrs" <+> ppr psig_qtv_bndrs
              , text "free_tvs" <+> ppr free_tvs
              , text "final_tvs" <+> ppr final_qtvs ]

       ; return (final_qtvs, my_theta) }
  where
    choose_qtv :: TyVarEnv InvisTVBinder -> TcTyVar
             -> (TcTyVarSet, [InvisTVBinder]) -> (TcTyVarSet, [InvisTVBinder])
    -- Pick which of the original qtvs should be retained
    -- Keep it if (a) it is mentioned in the body of the type (free_tvs)
    --            (b) it is a forall'd variable of the partial signature (psig_qtv_bndrs)
    --            (c) it is mentioned in the kind of a retained qtv (#22065)
    choose_qtv psig_bndr_map tv (free_tvs, qtvs)
       | Just psig_bndr <- lookupVarEnv psig_bndr_map tv
       = (free_tvs', psig_bndr : qtvs)
       | tv `elemVarSet` free_tvs
       = (free_tvs', mkTyVarBinder InferredSpec tv : qtvs)
       | otherwise  -- Do not pick it
       = (free_tvs, qtvs)
       where
         free_tvs' = free_tvs `unionVarSet` tyCoVarsOfType (tyVarKind tv)

    choose_psig_context :: VarSet -> TcThetaType -> Maybe TcType
                        -> TcM (VarSet, TcThetaType)
    choose_psig_context _ annotated_theta Nothing
      = do { let free_tvs = closeOverKinds (tyCoVarsOfTypes annotated_theta
                                            `unionVarSet` tau_tvs)
           ; return (free_tvs, annotated_theta) }

    choose_psig_context psig_qtvs annotated_theta (Just wc_var_ty)
      = do { let free_tvs = closeOverKinds (growThetaTyVars inferred_theta seed_tvs)
                            -- growThetaTyVars just like the no-type-sig case
                            -- See Note [growThetaTyVars vs closeWrtFunDeps] in GHC.Tc.Solver
                            -- Omitting this caused #12844
                 seed_tvs = tyCoVarsOfTypes annotated_theta  -- These are put there
                            `unionVarSet` tau_tvs            --       by the user

           ; let keep_me  = psig_qtvs `unionVarSet` free_tvs
                 my_theta = pickCapturedPreds keep_me inferred_theta

           -- Fill in the extra-constraints wildcard hole with inferred_theta,
           -- so that the Hole constraint we have already emitted
           -- (in tcHsPartialSigType) can report what filled it in.
           -- NB: my_theta already includes all the annotated constraints
           ; diff_theta <- findInferredDiff annotated_theta my_theta

           ; case getCastedTyVar_maybe wc_var_ty of
               -- We know that wc_co must have type kind(wc_var) ~ Constraint, as it
               -- comes from the checkExpectedKind in GHC.Tc.Gen.HsType.tcAnonWildCardOcc.
               -- So, to make the kinds work out, we reverse the cast here.
               Just (wc_var, wc_co) -> liftZonkM $
                                       writeMetaTyVar wc_var (mkConstraintTupleTy diff_theta
                                                              `mkCastTy` mkSymCo wc_co)
               Nothing              -> pprPanic "chooseInferredQuantifiers 1" (ppr wc_var_ty)

           ; traceTc "completeTheta" $
                vcat [ ppr sig
                     , text "annotated_theta:" <+> ppr annotated_theta
                     , text "inferred_theta:" <+> ppr inferred_theta
                     , text "my_theta:" <+> ppr my_theta
                     , text "diff_theta:" <+> ppr diff_theta ]
           ; return (free_tvs, annotated_theta ++ diff_theta) }
             -- Return (annotated_theta ++ diff_theta)
             -- See Note [Extra-constraints wildcards]

    report_dup_tyvar_tv_err fn_name hs_ty (n1,n2)
      = addErrTc (TcRnPartialTypeSigTyVarMismatch n1 n2 fn_name hs_ty)

    report_mono_sig_tv_err fn_name hs_ty (n,tv)
      = addErrTc (TcRnPartialTypeSigBadQuantifier n fn_name m_unif_ty hs_ty)
      where
        m_unif_ty = listToMaybe
                      [ rhs
                      -- recall that residuals are always implications
                      | residual_implic <- bagToList $ wc_impl residual
                      , residual_ct <- bagToList $ wc_simple (ic_wanted residual_implic)
                      , let residual_pred = ctPred residual_ct
                      , Just (Nominal, lhs, rhs) <- [ getEqPredTys_maybe residual_pred ]
                      , Just lhs_tv <- [ getTyVar_maybe lhs ]
                      , lhs_tv == tv ]

chooseInferredQuantifiers _ _ _ _ (Just sig)
  = pprPanic "chooseInferredQuantifiers" (ppr sig)

mk_inf_msg :: Name -> TcType -> TidyEnv -> ZonkM (TidyEnv, ErrCtxtMsg)
mk_inf_msg poly_name poly_ty tidy_env
 = do { (tidy_env1, poly_ty) <- zonkTidyTcType tidy_env poly_ty
      ; return (tidy_env1, InferredTypeCtxt poly_name poly_ty) }

-- | Warn the user about polymorphic local binders that lack type signatures.
localSigWarn :: Id -> Maybe TcIdSigInst -> TcM ()
localSigWarn id mb_sig
  | Just _ <- mb_sig               = return ()
  | not (isSigmaTy (idType id))    = return ()
  | otherwise                      = warnMissingSignatures id

warnMissingSignatures :: Id -> TcM ()
warnMissingSignatures id
  = do  { env0 <- liftZonkM $ tcInitTidyEnv
        ; let (env1, tidy_ty) = tidyOpenTypeX env0 (idType id)
        ; let dia = TcRnPolymorphicBinderMissingSig (idName id) tidy_ty
        ; addDiagnosticTcM (env1, dia) }

{- Note [Partial type signatures and generalisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If /any/ of the signatures in the group is a partial type signature
   f :: _ -> Int
then we *always* use the InferGen plan, and hence tcPolyInfer.
We do this even for a local binding with -XMonoLocalBinds, when
we normally use NoGen.

Reasons:
  * The TcSigInfo for 'f' has a unification variable for the '_',
    whose TcLevel is one level deeper than the current level.
    (See pushTcLevelM in tcTySig.)  But NoGen doesn't increase
    the TcLevel like InferGen, so we lose the level invariant.

  * The signature might be   f :: forall a. _ -> a
    so it really is polymorphic.  It's not clear what it would
    mean to use NoGen on this, and indeed the ASSERT in tcLhs,
    in the (Just sig) case, checks that if there is a signature
    then we are using LetLclBndr, and hence a nested AbsBinds with
    increased TcLevel

It might be possible to fix these difficulties somehow, but there
doesn't seem much point.  Indeed, adding a partial type signature is a
way to get per-binding inferred generalisation.

Note [Quantified variables in partial type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  f :: forall a. a -> a -> _
  f x y = g x y
  g :: forall b. b -> b -> _
  g x y = [x, y]

Here, 'f' and 'g' are mutually recursive, and we end up unifying 'a' and 'b'
together, which is fine.  So we bind 'a' and 'b' to TyVarTvs, which can then
unify with each other.

But now consider:
  f :: forall a b. a -> b -> _
  f x y = [x, y]

We want to get an error from this, because 'a' and 'b' get unified.
So we make a test, one per partial signature, to check that the
explicitly-quantified type variables have not been unified together.
#14449 showed this up.

Note [Extra-constraints wildcards]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this from #18646
    class Foo x where
      foo :: x

    bar :: (Foo (), _) => f ()
    bar = pure foo

We get [W] Foo (), [W] Applicative f.   When we do pickCapturedPreds in
choose_psig_context, we'll discard Foo ()!  Usually would not quantify over
such (closed) predicates.  So my_theta will be (Applicative f). But we really
do want to quantify over (Foo ()) -- it was specified by the programmer.
Solution: always return annotated_theta (user-specified) plus the extra piece
diff_theta.

Note [Validity of inferred types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to check inferred type for validity, in case it uses language
extensions that are not turned on.  The principle is that if the user
simply adds the inferred type to the program source, it'll compile fine.
See #8883.

Examples that might fail:
 - the type might be ambiguous

 - an inferred theta that requires type equalities e.g. (F a ~ G b)
                                or multi-parameter type classes
 - an inferred type that includes unboxed tuples

Note [Inferred type with escaping kind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Check for an inferred type with an escaping kind; e.g. #23051
   forall {k} {f :: k -> RuntimeRep} {g :: k} {a :: TYPE (f g)}. a
where the kind of the body of the forall mentions `f` and `g` which
are bound by the forall.  No no no.

This check, mkInferredPolyId, is really in the wrong place:
`inferred_poly_ty` doesn't obey the PKTI and it would be better not to
generalise it in the first place; see #20686.  But for now it works.

I considered adjusting the generalisation in GHC.Tc.Solver to directly check for
escaping kind variables; instead, promoting or defaulting them. But that
gets into the defaulting swamp and is a non-trivial and unforced
change, so I have left it alone for now.

Note [Impedance matching]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f 0 x = x
   f n x = g [] (not x)

   g [] y = f 10 y
   g _  y = f 9  y

After typechecking we'll get
  f_mono_ty :: a -> Bool -> Bool
  g_mono_ty :: [b] -> Bool -> Bool
with constraints
  (Eq a, Num a)

Note that f is polymorphic in 'a' and g in 'b'; and these are not linked.
The types we really want for f and g are
   f :: forall a. (Eq a, Num a) => a -> Bool -> Bool
   g :: forall b. [b] -> Bool -> Bool

We can get these by "impedance matching":
   tuple :: forall a b. (Eq a, Num a) => (a -> Bool -> Bool, [b] -> Bool -> Bool)
   tuple a b d1 d1 = let ...bind f_mono, g_mono in (f_mono, g_mono)

   f a d1 d2 = case tuple a Any d1 d2 of (f_mono, g_mono) -> f_mono
   g b = case tuple Integer b dEqInteger dNumInteger of (f_mono,g_mono) -> g_mono

Suppose the shared quantified tyvars are qtvs and constraints theta.
Then we want to check that
     forall qtvs. theta => f_mono_ty   is more polymorphic than   f's polytype
and the proof is the impedance matcher.

The impedance matcher can do defaulting: in the above example, we default
to Integer because of Num. See #7173. If we're dealing with a nondefaultable
class, impedance matching can fail. See #23427.

Note [SPECIALISE pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~
There is no point in a SPECIALISE pragma for a non-overloaded function:
   reverse :: [a] -> [a]
   {-# SPECIALISE reverse :: [Int] -> [Int] #-}

But SPECIALISE INLINE *can* make sense for GADTS:
   data Arr e where
     ArrInt :: !Int -> ByteArray# -> Arr Int
     ArrPair :: !Int -> Arr e1 -> Arr e2 -> Arr (e1, e2)

   (!:) :: Arr e -> Int -> e
   {-# SPECIALISE INLINE (!:) :: Arr Int -> Int -> Int #-}
   {-# SPECIALISE INLINE (!:) :: Arr (a, b) -> Int -> (a, b) #-}
   (ArrInt _ ba)     !: (I# i) = I# (indexIntArray# ba i)
   (ArrPair _ a1 a2) !: i      = (a1 !: i, a2 !: i)

When (!:) is specialised it becomes non-recursive, and can usefully
be inlined.  Scary!  So we only warn for SPECIALISE *without* INLINE
for a non-overloaded function.

************************************************************************
*                                                                      *
                         tcMonoBinds
*                                                                      *
************************************************************************

@tcMonoBinds@ deals with a perhaps-recursive group of HsBinds.
The signatures have been dealt with already.
-}

data MonoBindInfo = MBI { mbi_poly_name :: Name
                        , mbi_sig       :: Maybe TcIdSigInst
                        , mbi_mono_id   :: TcId
                        , mbi_mono_mult :: Mult }

tcMonoBinds :: RecFlag  -- Whether the binding is recursive for typechecking purposes
                        -- i.e. the binders are mentioned in their RHSs, and
                        --      we are not rescued by a type signature
            -> TcSigFun -> LetBndrSpec
            -> [LHsBind GhcRn]
            -> TcM (LHsBinds GhcTc, [MonoBindInfo])

-- SPECIAL CASE 1: see Note [Special case for non-recursive function bindings]
tcMonoBinds is_rec sig_fn no_gen
           [ L b_loc (FunBind { fun_id = L nm_loc name
                              , fun_matches = matches })]
                             -- Single function binding,
  | NonRecursive <- is_rec   -- ...binder isn't mentioned in RHS
  , Nothing <- sig_fn name   -- ...with no type signature
  = setSrcSpanA b_loc    $
    do  { mult <- tcMultAnn (HsNoMultAnn noExtField)

        ; ((co_fn, matches'), rhs_ty')
            <- tcInferFRR (FRRBinder name) $ \ exp_ty ->
                 -- tcInferFRR: the type of a let-binder must have
                 -- a fixed runtime rep. See #23176
               tcExtendBinderStack [TcIdBndr_ExpType name exp_ty NotTopLevel] $
                 -- We extend the error context even for a non-recursive
                 -- function so that in type error messages we show the
                 -- type of the thing whose rhs we are type checking.
                 -- See Note [Relevant bindings and the binder stack]
               tcFunBindMatches (InfSigCtxt name) name mult matches [] exp_ty
       ; mono_id <- newLetBndr no_gen name mult rhs_ty'

        ; return (singleton $ L b_loc $
                     FunBind { fun_id      = L nm_loc mono_id,
                               fun_matches = matches',
                               fun_ext     = (co_fn, []) },
                  [MBI { mbi_poly_name = name
                       , mbi_sig       = Nothing
                       , mbi_mono_id   = mono_id
                       , mbi_mono_mult = mult }]) }

-- SPECIAL CASE 2: see Note [Special case for non-recursive pattern bindings]
tcMonoBinds is_rec sig_fn no_gen
           [L b_loc (PatBind { pat_lhs = pat, pat_rhs = grhss, pat_mult = mult_ann })]
  | NonRecursive <- is_rec   -- ...binder isn't mentioned in RHS
  , all (isNothing . sig_fn) bndrs
  = addErrCtxt (PatMonoBindsCtxt pat grhss) $
    do { mult <- tcMultAnn mult_ann

       ; (grhss', pat_ty) <- tcInferFRR FRRPatBind $ \ exp_ty ->
                          -- tcInferFRR: the type of each let-binder must have
                          -- a fixed runtime rep. See #23176
                             tcGRHSsPat mult grhss exp_ty

       ; let exp_pat_ty :: Scaled ExpSigmaTypeFRR
             exp_pat_ty = Scaled mult (mkCheckExpType pat_ty)
       ; (_, (pat', mbis)) <- tcCollectingUsage $
                         tcLetPat (const Nothing) no_gen pat exp_pat_ty $ do
                           tcEmitBindingUsage bottomUE
                           mapM lookupMBI bndrs
            -- What's happening here? Typing pattern-matching (either from case
            -- expression or equation) and typing bindings (let or where) have a
            -- different control flow: for pattern-matching, the rhs is typed
            -- within the `thing_inside` argument. The type-checker walks down
            -- the pattern, and when finally it is done, all variables have been
            -- added to the environment, thing_inside is called. So, when
            -- type-checking patterns, the check for the correctness of
            -- multiplicity is generated in the VarPat case. This is quite
            -- natural.
            --
            -- Bindings, however, have a more complex control flow for our
            -- purpose: we collect all the variables as we go down, then return
            -- them (here as `mapM lookupMBI bndrs`), and in a subsequent
            -- computation (rather than an inner computation), the rhs is
            -- type-checked. This poses a problem here: we're calling
            -- `tcLetPat`, which will verify the proper usage of the introduced
            -- variable when reaching the `VarPat` case. But there is no actual
            -- usage of variable in the `thing_inside`. This would always
            -- fail. So we emit a `bottomUE`, which is compatible with every
            -- usage. So that we can bypass the check in VarPat. Then we use
            -- `tcCollectingUsage` to throw the `bottomUE` away, since it would
            -- let us bypass many linearity checks.

       ; return ( singleton $ L b_loc $
                     PatBind { pat_lhs = pat', pat_rhs = grhss'
                             , pat_ext = (pat_ty, ([],[]))
                             , pat_mult = setTcMultAnn mult mult_ann }

                , mbis ) }
  where
    bndrs = collectPatBinders CollNoDictBinders pat

-- GENERAL CASE
tcMonoBinds _ sig_fn no_gen binds
  = do  { tc_binds <- mapM (wrapLocMA (tcLhs sig_fn no_gen)) binds

        -- Bring the monomorphic Ids, into scope for the RHSs
        ; let mono_infos = getMonoBindInfo tc_binds
              rhs_id_env = [ (name, mono_id)
                           | MBI { mbi_poly_name = name
                                 , mbi_sig       = mb_sig
                                 , mbi_mono_id   = mono_id } <- mono_infos
                           , case mb_sig of
                               Just sig -> isPartialSig sig
                               Nothing  -> True ]
                -- A monomorphic binding for each term variable that lacks
                -- a complete type sig.  (Ones with a sig are already in scope.)

        ; traceTc "tcMonoBinds" $ vcat [ ppr n <+> ppr id <+> ppr (idType id)
                                       | (n,id) <- rhs_id_env]
        ; binds' <- tcExtendRecIds rhs_id_env $
                    mapM (wrapLocMA tcRhs) tc_binds

        ; return (binds', mono_infos) }

{- Note [Special case for non-recursive function bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the special case of
* A non-recursive FunBind
* With no type signature
we infer the type of the right hand side first (it may have a
higher-rank type) and *then* make the monomorphic Id for the LHS e.g.
   f = \(x::forall a. a->a) -> <body>

We want to infer a higher-rank type for f

Note [Special case for non-recursive pattern bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the special case of
* A pattern binding
* With no type signature for any of the binders
we can /infer/ the type of the RHS, and /check/ the pattern
against that type.  For example (#18323)

  ids :: [forall a. a -> a]
  combine :: (forall a . [a] -> a) -> [forall a. a -> a]
          -> ((forall a . [a] -> a), [forall a. a -> a])

  (x,y) = combine head ids

with -XImpredicativeTypes we can infer a good type for
(combine head ids), and use that to tell us the polymorphic
types of x and y.

We don't need to check -XImpredicativeTypes because without it
these types like [forall a. a->a] are illegal anyway, so this
special case code only really has an effect if -XImpredicativeTypes
is on.  Small exception:
  (x) = e
is currently treated as a pattern binding so, even absent
-XImpredicativeTypes, we will get a small improvement in behaviour.
But I don't think it's worth an extension flag.

Why do we require no type signatures on /any/ of the binders?
Consider
   x :: forall a. a->a
   y :: forall a. a->a
   (x,y) = (id,id)

Here we should /check/ the RHS with expected type
  (forall a. a->a, forall a. a->a).

If we have no signatures, we can the approach of this Note
to /infer/ the type of the RHS.

But what if we have some signatures, but not all? Say this:
  p :: forall a. a->a
  (p,q) = (id,  (\(x::forall b. b->b). x True))

Here we want to push p's signature inwards, i.e. /checking/, to
correctly elaborate 'id'. But we want to /infer/ q's higher rank
type.  There seems to be no way to do this.  So currently we only
switch to inference when we have no signature for any of the binders.

-}


------------------------
-- tcLhs typechecks the LHS of the bindings, to construct the environment in which
-- we typecheck the RHSs.  Basically what we are doing is this: for each binder:
--      if there's a signature for it, use the instantiated signature type
--      otherwise invent a type variable
-- You see that quite directly in the FunBind case.
--
-- But there's a complication for pattern bindings:
--      data T = MkT (forall a. a->a)
--      MkT f = e
-- Here we can guess a type variable for the entire LHS (which will be refined to T)
-- but we want to get (f::forall a. a->a) as the RHS environment.
-- The simplest way to do this is to typecheck the pattern, and then look up the
-- bound mono-ids.  Then we want to retain the typechecked pattern to avoid re-doing
-- it; hence the TcMonoBind data type in which the LHS is done but the RHS isn't

data TcMonoBind         -- Half completed; LHS done, RHS not done
  = TcFunBind  MonoBindInfo  SrcSpan Mult (MatchGroup GhcRn (LHsExpr GhcRn))
  | TcPatBind [MonoBindInfo] (LPat GhcTc) Mult (HsMultAnn GhcRn) (GRHSs GhcRn (LHsExpr GhcRn))
              TcSigmaTypeFRR

tcLhs :: TcSigFun -> LetBndrSpec -> HsBind GhcRn -> TcM TcMonoBind
-- Only called with plan InferGen (LetBndrSpec = LetLclBndr)
--                    or NoGen    (LetBndrSpec = LetGblBndr)
-- CheckGen is used only for functions with a complete type signature,
--          and tcPolyCheck doesn't use tcMonoBinds at all

tcLhs sig_fn no_gen (FunBind { fun_id = L nm_loc name
                             , fun_matches = matches })
  | Just (TcIdSig sig) <- sig_fn name
  = -- There is a type signature.
    -- It must be partial; if complete we'd be in tcPolyCheck!
    --    e.g.   f :: _ -> _
    --           f x = ...g...
    --           Just g = ...f...
    -- Hence always typechecked with InferGen
    do { mono_info <- tcLhsSigId no_gen (name, sig)
       ; mult <- tcMultAnn (HsNoMultAnn noExtField)
       ; return (TcFunBind mono_info (locA nm_loc) mult matches) }

  | otherwise  -- No type signature
  = do { mono_ty <- newOpenFlexiTyVarTy
       ; mult <- tcMultAnn (HsNoMultAnn noExtField)
       ; mono_id <- newLetBndr no_gen name mult mono_ty
       ; let mono_info = MBI { mbi_poly_name = name
                             , mbi_sig       = Nothing
                             , mbi_mono_id   = mono_id
                             , mbi_mono_mult = mult}
       ; return (TcFunBind mono_info (locA nm_loc) mult matches) }

tcLhs sig_fn no_gen (PatBind { pat_lhs = pat, pat_rhs = grhss, pat_mult = mult_ann })
  = -- See Note [Typechecking pattern bindings]
    do  { sig_mbis <- mapM (tcLhsSigId no_gen) sig_names

        ; let inst_sig_fun = lookupNameEnv $ mkNameEnv $
                             [ (mbi_poly_name mbi, mbi_mono_id mbi)
                             | mbi <- sig_mbis ]
        ; mult <- tcMultAnn mult_ann
            -- See Note [Typechecking pattern bindings]
        ; ((pat', nosig_mbis), pat_ty)
            <- addErrCtxt (PatMonoBindsCtxt pat grhss) $
               tcInferFRR FRRPatBind $ \ exp_ty ->
               tcLetPat inst_sig_fun no_gen pat (Scaled mult exp_ty) $
                 -- The above inferred type get an unrestricted multiplicity. It may be
                 -- worth it to try and find a finer-grained multiplicity here
                 -- if examples warrant it.
               mapM lookupMBI nosig_names

        ; let mbis = sig_mbis ++ nosig_mbis

        ; traceTc "tcLhs" (vcat [ ppr id <+> dcolon <+> ppr (idType id)
                                | mbi <- mbis, let id = mbi_mono_id mbi ]
                           $$ ppr no_gen)

        ; return (TcPatBind mbis pat' mult mult_ann grhss pat_ty) }
  where
    bndr_names = collectPatBinders CollNoDictBinders pat
    (nosig_names, sig_names) = partitionWith find_sig bndr_names

    find_sig :: Name -> Either Name (Name, TcIdSig)
    find_sig name = case sig_fn name of
                      Just (TcIdSig sig) -> Right (name, sig)
                      _                  -> Left name

tcLhs _ _ b@(PatSynBind {}) = pprPanic "tcLhs: PatSynBind" (ppr b)
  -- pattern synonyms are handled separately; see tc_single

tcLhs _ _ (VarBind { var_ext = x }) = dataConCantHappen x

lookupMBI :: Name -> TcM MonoBindInfo
-- After typechecking the pattern, look up the binder
-- names that lack a signature, which the pattern has brought
-- into scope.
lookupMBI name
  = do { mono_id <- tcLookupId name
       ; return (MBI { mbi_poly_name = name
                     , mbi_sig       = Nothing
                     , mbi_mono_id   = mono_id
                     , mbi_mono_mult = idMult mono_id }) }

-------------------
tcLhsSigId :: LetBndrSpec -> (Name, TcIdSig) -> TcM MonoBindInfo
tcLhsSigId no_gen (name, sig)
  = do { inst_sig <- tcInstSig sig
       ; mono_id <- newSigLetBndr no_gen name inst_sig
       ; return (MBI { mbi_poly_name = name
                     , mbi_sig       = Just inst_sig
                     , mbi_mono_id   = mono_id
                     , mbi_mono_mult = idMult mono_id }) }

------------
newSigLetBndr :: LetBndrSpec -> Name -> TcIdSigInst -> TcM TcId
newSigLetBndr (LetGblBndr prags) name (TISI { sig_inst_sig = id_sig })
  | TcCompleteSig (CSig { sig_bndr = poly_id }) <- id_sig
  = addInlinePrags poly_id (lookupPragEnv prags name)
newSigLetBndr no_gen name (TISI { sig_inst_tau = tau })
  = newLetBndr no_gen name ManyTy tau
    -- Binders with a signature are currently always of multiplicity
    -- Many. Because they come either from toplevel, let, or where
    -- declarations. Which are all unrestricted currently.

-------------------
tcRhs :: TcMonoBind -> TcM (HsBind GhcTc)
tcRhs (TcFunBind info@(MBI { mbi_sig = mb_sig, mbi_mono_id = mono_id })
                 loc mult matches)
  = tcExtendIdBinderStackForRhs [info]  $
    tcExtendTyVarEnvForRhs mb_sig       $
    do  { let mono_ty = idType mono_id
              mono_name = idName mono_id
        ; traceTc "tcRhs: fun bind" (ppr mono_id $$ ppr mono_ty)
        ; (co_fn, matches') <- tcFunBindMatches (InfSigCtxt mono_name) mono_name mult
                                                matches [] (mkCheckExpType mono_ty)
        ; return ( FunBind { fun_id      = L (noAnnSrcSpan loc) mono_id
                           , fun_matches = matches'
                           , fun_ext     = (co_fn, [])
                           } ) }

tcRhs (TcPatBind infos pat' mult mult_ann grhss pat_ty)
  = -- When we are doing pattern bindings we *don't* bring any scoped
    -- type variables into scope unlike function bindings
    -- Wny not?  They are not completely rigid.
    -- That's why we have the special case for a single FunBind in tcMonoBinds
    tcExtendIdBinderStackForRhs infos        $
    do  { traceTc "tcRhs: pat bind" (ppr pat' $$ ppr pat_ty)
        ; grhss' <- addErrCtxt (PatMonoBindsCtxt pat' grhss) $
                    tcGRHSsPat mult grhss (mkCheckExpType pat_ty)

        ; return ( PatBind { pat_lhs = pat', pat_rhs = grhss'
                           , pat_ext = (pat_ty, ([],[]))
                           , pat_mult = setTcMultAnn mult mult_ann } )}


-- | @'tcMultAnn' ann@ takes an optional multiplicity annotation. If
-- present the multiplicity is returned, otherwise a fresh unification variable
-- is generated so that multiplicity can be inferred.
tcMultAnn :: HsMultAnn GhcRn -> TcM Mult
tcMultAnn (HsPct1Ann _) = return oneDataConTy
tcMultAnn (HsMultAnn _ p) = tcCheckLHsTypeInContext p (TheKind multiplicityTy)
tcMultAnn (HsNoMultAnn _) = newFlexiTyVarTy multiplicityTy

tcExtendTyVarEnvForRhs :: Maybe TcIdSigInst -> TcM a -> TcM a
tcExtendTyVarEnvForRhs Nothing thing_inside
  = thing_inside
tcExtendTyVarEnvForRhs (Just sig) thing_inside
  = tcExtendTyVarEnvFromSig sig thing_inside

tcExtendTyVarEnvFromSig :: TcIdSigInst -> TcM a -> TcM a
tcExtendTyVarEnvFromSig sig_inst thing_inside
  | TISI { sig_inst_skols = skol_prs, sig_inst_wcs = wcs } <- sig_inst
  = tcExtendNameTyVarEnv wcs $
    tcExtendNameTyVarEnv (mapSnd binderVar skol_prs) $
    thing_inside

tcExtendIdBinderStackForRhs :: [MonoBindInfo] -> TcM a -> TcM a
-- See Note [Relevant bindings and the binder stack]
tcExtendIdBinderStackForRhs infos thing_inside
  = tcExtendBinderStack [ TcIdBndr mono_id NotTopLevel
                        | MBI { mbi_mono_id = mono_id } <- infos ]
                        thing_inside
    -- NotTopLevel: it's a monomorphic binding

---------------------
getMonoBindInfo :: [LocatedA TcMonoBind] -> [MonoBindInfo]
getMonoBindInfo tc_binds
  = foldr (get_info . unLoc) [] tc_binds
  where
    get_info (TcFunBind info _ _ _)    rest = info : rest
    get_info (TcPatBind infos _ _ _ _ _) rest = infos ++ rest


{- Note [Relevant bindings and the binder stack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When typecking a binding we extend the TcBinderStack for the RHS of
the binding, with the /monomorphic/ Id.  That way, if we have, say
    f = \x -> blah
and something goes wrong in 'blah', we get a "relevant binding"
looking like  f :: alpha -> beta
This applies if 'f' has a type signature too:
   f :: forall a. [a] -> [a]
   f x = True
We can't unify True with [a], and a relevant binding is f :: [a] -> [a]
If we had the *polymorphic* version of f in the TcBinderStack, it
would not be reported as relevant, because its type is closed.
(See TcErrors.relevantBindings.)

Note [Typechecking pattern bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Look at:
   - typecheck/should_compile/ExPat
   - #12427, typecheck/should_compile/T12427{a,b}

  data T where
    MkT :: Integral a => a -> Int -> T

and suppose t :: T.  Which of these pattern bindings are ok?

  E1. let { MkT p _ = t } in <body>

  E2. let { MkT _ q = t } in <body>

  E3. let { MkT (toInteger -> r) _ = t } in <body>

* (E1) is clearly wrong because the existential 'a' escapes.
  What type could 'p' possibly have?

* (E2) is fine, despite the existential pattern, because
  q::Int, and nothing escapes.

* Even (E3) is fine.  The existential pattern bindings a dictionary
  for (Integral a) which the view pattern can use to convert the
  a-valued field to an Integer, so r :: Integer.

An easy way to see all three is to imagine the desugaring.
For (E2) it would look like
    let q = case t of MkT _ q' -> q'
    in <body>


We typecheck pattern bindings as follows.  First tcLhs does this:

  1. Take each type signature q :: ty, partial or complete, and
     instantiate it (with tcLhsSigId) to get a MonoBindInfo.  This
     gives us a fresh "mono_id" qm :: instantiate(ty), where qm has
     a fresh name.

     Any fresh unification variables in instantiate(ty) born here, not
     deep under implications as would happen if we allocated them when
     we encountered q during tcPat.

  2. Build a little environment mapping "q" -> "qm" for those Ids
     with signatures (inst_sig_fun)

  3. Invoke tcLetPat to typecheck the pattern.

     - We pass in the current TcLevel.  This is captured by
       GHC.Tc.Gen.Pat.tcLetPat, and put into the pc_lvl field of PatCtxt, in
       PatEnv.

     - When tcPat finds an existential constructor, it binds fresh
       type variables and dictionaries as usual, increments the TcLevel,
       and emits an implication constraint.

     - When we come to a binder (GHC.Tc.Gen.Pat.tcPatBndr), it looks it up
       in the little environment (the pc_sig_fn field of PatCtxt).

         Success => There was a type signature, so just use it,
                    checking compatibility with the expected type.

         Failure => No type signature.
             Infer case: (happens only outside any constructor pattern)
                         use a unification variable
                         at the outer level pc_lvl

             Check case: use promoteTcType to promote the type
                         to the outer level pc_lvl.  This is the
                         place where we emit a constraint that'll blow
                         up if existential capture takes place

       Result: the type of the binder is always at pc_lvl. This is
       crucial.

  4. Throughout, when we are making up an Id for the pattern-bound variables
     (newLetBndr), we have two cases:

     - If we are generalising (generalisation plan is InferGen or
       CheckGen), then the let_bndr_spec will be LetLclBndr.  In that case
       we want to bind a cloned, local version of the variable, with the
       type given by the pattern context, *not* by the signature (even if
       there is one; see #7268). The mkExport part of the
       generalisation step will do the checking and impedance matching
       against the signature.

     - If for some reason we are not generalising (plan = NoGen), the
       LetBndrSpec will be LetGblBndr.  In that case we must bind the
       global version of the Id, and do so with precisely the type given
       in the signature.  (Then we unify with the type from the pattern
       context type.)


And that's it!  The implication constraints check for the skolem
escape.  It's quite simple and neat, and more expressive than before
e.g. GHC 8.0 rejects (E2) and (E3).

Example for (E1), starting at level 1.  We generate
     p :: beta:1, with constraints (forall:3 a. Integral a => a ~ beta)
The (a~beta) can't float (because of the 'a'), nor be solved (because
beta is untouchable.)

Example for (E2), we generate
     q :: beta:1, with constraint (forall:3 a. Integral a => Int ~ beta)
The beta is untouchable, but floats out of the constraint and can
be solved absolutely fine.


************************************************************************
*                                                                      *
                Generalisation
*                                                                      *
********************************************************************* -}

data GeneralisationPlan
  = NoGen               -- No generalisation, no AbsBinds

  | InferGen            -- Implicit generalisation; there is an AbsBinds

  | CheckGen            -- One FunBind with a complete signature:
       (LHsBind GhcRn)  --   do explicit generalisation
       TcCompleteSig

-- A consequence of the no-AbsBinds choice (NoGen) is that there is
-- no "polymorphic Id" and "monmomorphic Id"; there is just the one

instance Outputable GeneralisationPlan where
  ppr NoGen          = text "NoGen"
  ppr InferGen       = text "InferGen"
  ppr (CheckGen _ s) = text "CheckGen" <+> ppr s

decideGeneralisationPlan
   :: DynFlags -> TopLevelFlag -> IsGroupClosed -> TcSigFun
   -> [LHsBind GhcRn] -> GeneralisationPlan
decideGeneralisationPlan dflags top_lvl closed sig_fn lbinds
  | Just (bind, sig) <- one_funbind_with_sig = CheckGen bind sig
  | generalise_binds                         = InferGen
  | otherwise                                = NoGen
  where
    generalise_binds
      | isTopLevel top_lvl             = True
        -- See Note [Always generalise top-level bindings]

      | has_mult_anns_and_pats = False
        -- See (NVP1) and (NVP4) in Note [Non-variable pattern bindings aren't linear]

      | IsGroupClosed _ True <- closed
      , not (null binders) = True
        -- The 'True' means that all of the group's
        -- free vars have ClosedTypeId=True; so we can ignore
        -- -XMonoLocalBinds, and generalise anyway.
        -- Except if 'fv' is empty: there is no binder to generalise, so
        -- generalising does nothing. And trying to generalise hurts linear
        -- types (see #25428). So we don't force it.
        -- See (NVP5) in Note [Non-variable pattern bindings aren't linear] in GHC.Tc.Gen.Bind.

      | has_partial_sigs = True
        -- See Note [Partial type signatures and generalisation]

      | otherwise = not (xopt LangExt.MonoLocalBinds dflags)

    -- With OutsideIn, all nested bindings are monomorphic
    -- except a single function binding with a complete signature
    one_funbind_with_sig
      | [lbind@(L _ (FunBind { fun_id = v }))] <- lbinds
      , Just (TcIdSig (TcCompleteSig sig)) <- sig_fn (unLoc v)
      = Just (lbind, sig)
      | otherwise
      = Nothing

    binders          = collectHsBindListBinders CollNoDictBinders lbinds
    has_partial_sigs = any has_partial_sig binders
    has_partial_sig nm = case sig_fn nm of
      Just (TcIdSig (TcPartialSig {})) -> True
      _                                -> False
    has_mult_anns_and_pats = any has_mult_ann_and_pat lbinds
    has_mult_ann_and_pat (L _ (PatBind{pat_mult=HsNoMultAnn{}})) = False
    has_mult_ann_and_pat (L _ (PatBind{pat_lhs=(L _ (VarPat{}))})) = False
    has_mult_ann_and_pat (L _ (PatBind{})) = True
    has_mult_ann_and_pat _ = False

isClosedBndrGroup :: TcTypeEnv -> [(LHsBind GhcRn)] -> IsGroupClosed
isClosedBndrGroup type_env binds
  = IsGroupClosed fv_env type_closed
  where
    type_closed = allUFM (nameSetAll is_closed_type_id) fv_env

    fv_env :: NameEnv NameSet
    fv_env = mkNameEnv $ concatMap (bindFvs . unLoc) binds

    bindFvs :: HsBindLR GhcRn GhcRn -> [(Name, NameSet)]
    bindFvs (FunBind { fun_id = L _ f
                     , fun_ext = fvs })
       = let open_fvs = get_open_fvs fvs
         in [(f, open_fvs)]
    bindFvs (PatBind { pat_lhs = pat, pat_ext = fvs })
       = let open_fvs = get_open_fvs fvs
         in [(b, open_fvs) | b <- collectPatBinders CollNoDictBinders pat]
    bindFvs _
       = []

    get_open_fvs fvs = filterNameSet (not . is_closed) fvs

    is_closed :: Name -> ClosedTypeId
    is_closed name
      | Just thing <- lookupNameEnv type_env name
      = case thing of
          AGlobal {}                     -> True
          ATcId { tct_info = ClosedLet } -> True
          _                              -> False

      | otherwise
      = True  -- The free-var set for a top level binding mentions


    is_closed_type_id :: Name -> Bool
    -- We're already removed Global and ClosedLet Ids
    is_closed_type_id name
      | Just thing <- lookupNameEnv type_env name
      = case thing of
          ATcId { tct_info = NonClosedLet _ cl } -> cl
          ATcId { tct_info = NotLetBound }       -> False
          ATyVar {}                              -> False
               -- In-scope type variables are not closed!
          _ -> pprPanic "is_closed_id" (ppr name)

      | otherwise
      = True   -- The free-var set for a top level binding mentions
               -- imported things too, so that we can report unused imports
               -- These won't be in the local type env.
               -- Ditto class method etc from the current module

{- Note [Always generalise top-level bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is very confusing to apply NoGen to a top level binding. Consider (#20123):
   module M where
     x = 5
     f y = (x, y)

The MR means that x=5 is not generalise, so f's binding is no Closed.  So we'd
be tempted to use NoGen. But that leads to f :: Any -> (Integer, Any), which
is plain stupid.

NoGen is good when we have call sites, but not at top level, where the
function may be exported.  And it's easier to grok "MonoLocalBinds" as
applying to, well, local bindings.
-}

