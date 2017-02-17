{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[TcBinds]{TcBinds}
-}

{-# LANGUAGE CPP, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module TcBinds ( tcLocalBinds, tcTopBinds, tcRecSelBinds,
                 tcHsBootSigs, tcPolyCheck,
                 tcVectDecls, addTypecheckedBinds,
                 chooseInferredQuantifiers,
                 badBootDeclErr ) where

import {-# SOURCE #-} TcMatches ( tcGRHSsPat, tcMatchesFun )
import {-# SOURCE #-} TcExpr  ( tcMonoExpr )
import {-# SOURCE #-} TcPatSyn ( tcInferPatSynDecl, tcCheckPatSynDecl
                               , tcPatSynBuilderBind )
import CoreSyn (Tickish (..))
import CostCentre (mkUserCC)
import DynFlags
import FastString
import HsSyn
import HscTypes( isHsBootOrSig )
import TcSigs
import TcRnMonad
import TcEnv
import TcUnify
import TcSimplify
import TcEvidence
import TcHsType
import TcPat
import TcMType
import FamInstEnv( normaliseType )
import FamInst( tcGetFamInstEnvs )
import TyCon
import TcType
import Type( mkStrLitTy, tidyOpenType, mkTyVarBinder, splitTyConApp_maybe)
import TysPrim
import TysWiredIn( cTupleTyConName )
import Id
import Var
import VarSet
import VarEnv( TidyEnv )
import Module
import Name
import NameSet
import NameEnv
import SrcLoc
import Bag
import ListSetOps
import ErrUtils
import Digraph
import Maybes
import Util
import BasicTypes
import Outputable
import PrelNames( ipClassName )
import TcValidity (checkValidType)
import Unique (getUnique)
import UniqFM
import qualified GHC.LanguageExtensions as LangExt
import ConLike

import Control.Monad

#include "HsVersions.h"

{- *********************************************************************
*                                                                      *
               A useful helper function
*                                                                      *
********************************************************************* -}

addTypecheckedBinds :: TcGblEnv -> [LHsBinds Id] -> TcGblEnv
addTypecheckedBinds tcg_env binds
  | isHsBootOrSig (tcg_src tcg_env) = tcg_env
    -- Do not add the code for record-selector bindings
    -- when compiling hs-boot files
  | otherwise = tcg_env { tcg_binds = foldr unionBags
                                            (tcg_binds tcg_env)
                                            binds }

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

Notice the the stupid construction of (f a d), which is of course
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

NOTE: a bit of arity anaysis would push the (f a d) inside the (\ys...),
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

tcTopBinds :: [(RecFlag, LHsBinds Name)] -> [LSig Name] -> TcM (TcGblEnv, TcLclEnv)
-- The TcGblEnv contains the new tcg_binds and tcg_spects
-- The TcLclEnv has an extended type envt for the new bindings
tcTopBinds binds sigs
  = do  { -- Pattern synonym bindings populate the global environment
          (binds', (tcg_env, tcl_env)) <- tcValBinds TopLevel binds sigs $
            do { gbl <- getGblEnv
               ; lcl <- getLclEnv
               ; return (gbl, lcl) }
        ; specs <- tcImpPrags sigs   -- SPECIALISE prags for imported Ids

        ; complete_matches <- setEnvs (tcg_env, tcl_env) $ tcCompleteSigs sigs
        ; traceTc "complete_matches" (ppr binds $$ ppr sigs)
        ; traceTc "complete_matches" (ppr complete_matches)

        ; let { tcg_env' = tcg_env { tcg_imp_specs
                                      = specs ++ tcg_imp_specs tcg_env
                                   , tcg_complete_matches
                                      = complete_matches
                                          ++ tcg_complete_matches tcg_env }
                           `addTypecheckedBinds` map snd binds' }

        ; return (tcg_env', tcl_env) }
        -- The top level bindings are flattened into a giant
        -- implicitly-mutually-recursive LHsBinds


-- Note [Typechecking Complete Matches]
-- Much like when a user bundled a pattern synonym, the result types of
-- all the constructors in the match pragma must be consistent.
--
-- If we allowed pragmas with inconsistent types then it would be
-- impossible to ever match every constructor in the list and so
-- the pragma would be useless.





-- This is only used in `tcCompleteSig`. We fold over all the conlikes,
-- this accumulator keeps track of the first `ConLike` with a concrete
-- return type. After fixing the return type, all other constructors with
-- a fixed return type must agree with this.
--
-- The fields of `Fixed` cache the first conlike and its return type so
-- that that we can compare all the other conlikes to it. The conlike is
-- stored for error messages.
--
-- `Nothing` in the case that the type is fixed by a type signature
data CompleteSigType = AcceptAny | Fixed (Maybe ConLike) TyCon

tcCompleteSigs  :: [LSig Name] -> TcM [CompleteMatch]
tcCompleteSigs sigs =
  let
      doOne :: Sig Name -> TcM (Maybe CompleteMatch)
      doOne c@(CompleteMatchSig _ lns mtc)
        = fmap Just $ do
           addErrCtxt (text "In" <+> ppr c) $
            case mtc of
              Nothing -> infer_complete_match
              Just tc -> check_complete_match $ unLEmb tc
        where

          checkCLTypes acc = foldM checkCLType (acc, []) (unLoc lns)

          infer_complete_match = do
            (res, cls) <- checkCLTypes AcceptAny
            case res of
              AcceptAny -> failWithTc ambiguousError
              Fixed _ tc  -> return $ CompleteMatch cls tc

          check_complete_match tc_name = do
            ty_con <- tcLookupLocatedTyCon tc_name
            (_, cls) <- checkCLTypes (Fixed Nothing ty_con)
            return $ CompleteMatch cls ty_con
      doOne _ = return Nothing

      ambiguousError :: SDoc
      ambiguousError =
        text "A type signature must be provided for a set of polymorphic"
          <+> text "pattern synonyms."


      -- See note [Typechecking Complete Matches]
      checkCLType :: (CompleteSigType, [ConLike]) -> LEmbellished Name
                  -> TcM (CompleteSigType, [ConLike])
      checkCLType (cst, cs) n = do
        cl <- addLocM tcLookupConLike $ unLEmb n
        let   (_,_,_,_,_,_, res_ty) = conLikeFullSig cl
              res_ty_con = fst <$> splitTyConApp_maybe res_ty
        case (cst, res_ty_con) of
          (AcceptAny, Nothing) -> return (AcceptAny, cl:cs)
          (AcceptAny, Just tc) -> return (Fixed (Just cl) tc, cl:cs)
          (Fixed mfcl tc, Nothing)  -> return (Fixed mfcl tc, cl:cs)
          (Fixed mfcl tc, Just tc') ->
            if tc == tc'
              then return (Fixed mfcl tc, cl:cs)
              else case mfcl of
                     Nothing ->
                      addErrCtxt (text "In" <+> ppr cl) $
                        failWithTc typeSigErrMsg
                     Just cl -> failWithTc (errMsg cl)
             where
              typeSigErrMsg :: SDoc
              typeSigErrMsg =
                text "Couldn't match expected type"
                      <+> quotes (ppr tc)
                      <+> text "with"
                      <+> quotes (ppr tc')

              errMsg :: ConLike -> SDoc
              errMsg fcl =
                text "Cannot form a group of complete patterns from patterns"
                  <+> quotes (ppr fcl) <+> text "and" <+> quotes (ppr cl)
                  <+> text "as they match different type constructors"
                  <+> parens (quotes (ppr tc)
                               <+> text "resp."
                               <+> quotes (ppr tc'))
  in  mapMaybeM (addLocM doOne) sigs

tcRecSelBinds :: HsValBinds Name -> TcM TcGblEnv
tcRecSelBinds (ValBindsOut binds sigs)
  = tcExtendGlobalValEnv [sel_id | L _ (IdSig sel_id) <- sigs] $
    do { (rec_sel_binds, tcg_env) <- discardWarnings $
                                     tcValBinds TopLevel binds sigs getGblEnv
       ; let tcg_env' = tcg_env `addTypecheckedBinds` map snd rec_sel_binds
       ; return tcg_env' }
tcRecSelBinds (ValBindsIn {}) = panic "tcRecSelBinds"

tcHsBootSigs :: [(RecFlag, LHsBinds Name)] -> [LSig Name] -> TcM [Id]
-- A hs-boot file has only one BindGroup, and it only has type
-- signatures in it.  The renamer checked all this
tcHsBootSigs binds sigs
  = do  { checkTc (null binds) badBootDeclErr
        ; concat <$> mapM (addLocM tc_boot_sig) (filter isTypeLSig sigs) }
  where
    tc_boot_sig (TypeSig lnames hs_ty) = mapM f lnames
      where
        f (L _ name)
          = do { sigma_ty <- solveEqualities $
                             tcHsSigWcType (FunSigCtxt (unEmb name) False) hs_ty
               ; return (mkVanillaGlobal (unEmb name) sigma_ty) }
        -- Notice that we make GlobalIds, not LocalIds
    tc_boot_sig s = pprPanic "tcHsBootSigs/tc_boot_sig" (ppr s)

badBootDeclErr :: MsgDoc
badBootDeclErr = text "Illegal declarations in an hs-boot file"

------------------------
tcLocalBinds :: HsLocalBinds Name -> TcM thing
             -> TcM (HsLocalBinds TcId, thing)

tcLocalBinds EmptyLocalBinds thing_inside
  = do  { thing <- thing_inside
        ; return (EmptyLocalBinds, thing) }

tcLocalBinds (HsValBinds (ValBindsOut binds sigs)) thing_inside
  = do  { (binds', thing) <- tcValBinds NotTopLevel binds sigs thing_inside
        ; return (HsValBinds (ValBindsOut binds' sigs), thing) }
tcLocalBinds (HsValBinds (ValBindsIn {})) _ = panic "tcLocalBinds"

tcLocalBinds (HsIPBinds (IPBinds ip_binds _)) thing_inside
  = do  { ipClass <- tcLookupClass ipClassName
        ; (given_ips, ip_binds') <-
            mapAndUnzipM (wrapLocSndM (tc_ip_bind ipClass)) ip_binds

        -- If the binding binds ?x = E, we  must now
        -- discharge any ?x constraints in expr_lie
        -- See Note [Implicit parameter untouchables]
        ; (ev_binds, result) <- checkConstraints (IPSkol ips)
                                  [] given_ips thing_inside

        ; return (HsIPBinds (IPBinds ip_binds' ev_binds), result) }
  where
    ips = [ip | L _ (IPBind (Left (L _ ip)) _) <- ip_binds]

        -- I wonder if we should do these one at at time
        -- Consider     ?x = 4
        --              ?y = ?x + 1
    tc_ip_bind ipClass (IPBind (Left (L _ ip)) expr)
       = do { ty <- newOpenFlexiTyVarTy
            ; let p = mkStrLitTy $ hsIPNameFS ip
            ; ip_id <- newDict ipClass [ p, ty ]
            ; expr' <- tcMonoExpr expr (mkCheckExpType ty)
            ; let d = toDict ipClass p ty `fmap` expr'
            ; return (ip_id, (IPBind (Right ip_id) d)) }
    tc_ip_bind _ (IPBind (Right {}) _) = panic "tc_ip_bind"

    -- Coerces a `t` into a dictionry for `IP "x" t`.
    -- co : t -> IP "x" t
    toDict ipClass x ty = HsWrap $ mkWpCastR $
                          wrapIP $ mkClassPred ipClass [x,ty]

{- Note [Implicit parameter untouchables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We add the type variables in the types of the implicit parameters
as untouchables, not so much because we really must not unify them,
but rather because we otherwise end up with constraints like this
    Num alpha, Implic { wanted = alpha ~ Int }
The constraint solver solves alpha~Int by unification, but then
doesn't float that solved constraint out (it's not an unsolved
wanted).  Result disaster: the (Num alpha) is again solved, this
time by defaulting.  No no no.

However [Oct 10] this is all handled automatically by the
untouchable-range idea.
-}

tcValBinds :: TopLevelFlag
           -> [(RecFlag, LHsBinds Name)] -> [LSig Name]
           -> TcM thing
           -> TcM ([(RecFlag, LHsBinds TcId)], thing)

tcValBinds top_lvl binds sigs thing_inside
  = do  { let patsyns = getPatSynBinds binds

            -- Typecheck the signature
        ; (poly_ids, sig_fn) <- tcAddPatSynPlaceholders patsyns $
                                tcTySigs sigs

        ; let prag_fn = mkPragEnv sigs (foldr (unionBags . snd) emptyBag binds)

                -- Extend the envt right away with all the Ids
                -- declared with complete type signatures
                -- Do not extend the TcIdBinderStack; instead
                -- we extend it on a per-rhs basis in tcExtendForRhs
        ; tcExtendLetEnvIds top_lvl [(idName id, id) | id <- poly_ids] $ do
            { (binds', (extra_binds', thing)) <- tcBindGroups top_lvl sig_fn prag_fn binds $ do
                   { thing <- thing_inside
                     -- See Note [Pattern synonym builders don't yield dependencies]
                     --     in RnBinds
                   ; patsyn_builders <- mapM tcPatSynBuilderBind patsyns
                   ; let extra_binds = [ (NonRecursive, builder) | builder <- patsyn_builders ]
                   ; return (extra_binds, thing) }
            ; return (binds' ++ extra_binds', thing) }}

------------------------
tcBindGroups :: TopLevelFlag -> TcSigFun -> TcPragEnv
             -> [(RecFlag, LHsBinds Name)] -> TcM thing
             -> TcM ([(RecFlag, LHsBinds TcId)], thing)
-- Typecheck a whole lot of value bindings,
-- one strongly-connected component at a time
-- Here a "strongly connected component" has the strightforward
-- meaning of a group of bindings that mention each other,
-- ignoring type signatures (that part comes later)

tcBindGroups _ _ _ [] thing_inside
  = do  { thing <- thing_inside
        ; return ([], thing) }

tcBindGroups top_lvl sig_fn prag_fn (group : groups) thing_inside
  = do  { -- See Note [Closed binder groups]
          closed <- isClosedBndrGroup $ snd group
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
         -> (RecFlag, LHsBinds Name) -> IsGroupClosed -> TcM thing
         -> TcM ([(RecFlag, LHsBinds TcId)], thing)

-- Typecheck one strongly-connected component of the original program.
-- We get a list of groups back, because there may
-- be specialisations etc as well

tc_group top_lvl sig_fn prag_fn (NonRecursive, binds) closed thing_inside
        -- A single non-recursive binding
        -- We want to keep non-recursive things non-recursive
        -- so that we desugar unlifted bindings correctly
  = do { let bind = case bagToList binds of
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
        -- See Note [Polymorphic recursion] in HsBinds.
    do  { traceTc "tc_group rec" (pprLHsBinds binds)
        ; when hasPatSyn $ recursivePatSynErr binds
        ; (binds1, thing) <- go sccs
        ; return ([(Recursive, binds1)], thing) }
                -- Rec them all together
  where
    hasPatSyn = anyBag (isPatSyn . unLoc) binds
    isPatSyn PatSynBind{} = True
    isPatSyn _ = False

    sccs :: [SCC (LHsBind Name)]
    sccs = stronglyConnCompFromEdgedVerticesUniq (mkEdges sig_fn binds)

    go :: [SCC (LHsBind Name)] -> TcM (LHsBinds TcId, thing)
    go (scc:sccs) = do  { (binds1, ids1) <- tc_scc scc
                        ; (binds2, thing) <- tcExtendLetEnv top_lvl closed ids1
                                                            (go sccs)
                        ; return (binds1 `unionBags` binds2, thing) }
    go []         = do  { thing <- thing_inside; return (emptyBag, thing) }

    tc_scc (AcyclicSCC bind) = tc_sub_group NonRecursive [bind]
    tc_scc (CyclicSCC binds) = tc_sub_group Recursive    binds

    tc_sub_group rec_tc binds =
      tcPolyBinds sig_fn prag_fn Recursive rec_tc closed binds

recursivePatSynErr :: OutputableBndr name => LHsBinds name -> TcM a
recursivePatSynErr binds
  = failWithTc $
    hang (text "Recursive pattern synonym definition with following bindings:")
       2 (vcat $ map pprLBind . bagToList $ binds)
  where
    pprLoc loc  = parens (text "defined at" <+> ppr loc)
    pprLBind (L loc bind) = pprWithCommas ppr (collectHsBindBinders bind) <+>
                            pprLoc loc

tc_single :: forall thing.
            TopLevelFlag -> TcSigFun -> TcPragEnv
          -> LHsBind Name -> IsGroupClosed -> TcM thing
          -> TcM (LHsBinds TcId, thing)
tc_single _top_lvl sig_fn _prag_fn
          (L _ (PatSynBind psb@PSB{ psb_id = L _ name }))
          _ thing_inside
  = do { (aux_binds, tcg_env) <- tc_pat_syn_decl
       ; thing <- setGblEnv tcg_env thing_inside
       ; return (aux_binds, thing)
       }
  where
    tc_pat_syn_decl :: TcM (LHsBinds TcId, TcGblEnv)
    tc_pat_syn_decl = case sig_fn $ unEmb name of
        Nothing                 -> tcInferPatSynDecl psb
        Just (TcPatSynSig tpsi) -> tcCheckPatSynDecl psb tpsi
        Just                 _  -> panic "tc_single"

tc_single top_lvl sig_fn prag_fn lbind closed thing_inside
  = do { (binds1, ids) <- tcPolyBinds sig_fn prag_fn
                                      NonRecursive NonRecursive
                                      closed
                                      [lbind]
       ; thing <- tcExtendLetEnv top_lvl closed ids thing_inside
       ; return (binds1, thing) }

------------------------
type BKey = Int -- Just number off the bindings

mkEdges :: TcSigFun -> LHsBinds Name -> [Node BKey (LHsBind Name)]
-- See Note [Polymorphic recursion] in HsBinds.
mkEdges sig_fn binds
  = [ (bind, key, [key | n <- nonDetEltsUFM (bind_fvs (unLoc bind)),
                         Just key <- [lookupNameEnv key_map n], no_sig n ])
    | (bind, key) <- keyd_binds
    ]
    -- It's OK to use nonDetEltsUFM here as stronglyConnCompFromEdgedVertices
    -- is still deterministic even if the edges are in nondeterministic order
    -- as explained in Note [Deterministic SCC] in Digraph.
  where
    no_sig :: Name -> Bool
    no_sig n = noCompleteSig (sig_fn n)

    keyd_binds = bagToList binds `zip` [0::BKey ..]

    key_map :: NameEnv BKey     -- Which binding it comes from
    key_map = mkNameEnv [(bndr, key) | (L _ bind, key) <- keyd_binds
                                     , bndr <- collectHsBindBinders bind ]

------------------------
tcPolyBinds :: TcSigFun -> TcPragEnv
            -> RecFlag         -- Whether the group is really recursive
            -> RecFlag         -- Whether it's recursive after breaking
                               -- dependencies based on type signatures
            -> IsGroupClosed   -- Whether the group is closed
            -> [LHsBind Name]  -- None are PatSynBind
            -> TcM (LHsBinds TcId, [TcId])

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

tcPolyBinds sig_fn prag_fn rec_group rec_tc closed bind_list
  = setSrcSpan loc                              $
    recoverM (recoveryCode binder_names sig_fn) $ do
        -- Set up main recover; take advantage of any type sigs

    { traceTc "------------------------------------------------" Outputable.empty
    ; traceTc "Bindings for {" (ppr binder_names)
    ; dflags   <- getDynFlags
    ; let plan = decideGeneralisationPlan dflags bind_list closed sig_fn
    ; traceTc "Generalisation plan" (ppr plan)
    ; result@(_, poly_ids) <- case plan of
         NoGen              -> tcPolyNoGen rec_tc prag_fn sig_fn bind_list
         InferGen mn        -> tcPolyInfer rec_tc prag_fn sig_fn mn bind_list
         CheckGen lbind sig -> tcPolyCheck prag_fn sig lbind

    ; traceTc "} End of bindings for" (vcat [ ppr binder_names, ppr rec_group
                                            , vcat [ppr id <+> ppr (idType id) | id <- poly_ids]
                                          ])

    ; return result }
  where
    binder_names = collectHsBindListBinders bind_list
    loc = foldr1 combineSrcSpans (map getLoc bind_list)
         -- The mbinds have been dependency analysed and
         -- may no longer be adjacent; so find the narrowest
         -- span that includes them all

--------------
-- If typechecking the binds fails, then return with each
-- signature-less binder given type (forall a.a), to minimise
-- subsequent error messages
recoveryCode :: [Name] -> TcSigFun -> TcM (LHsBinds TcId, [Id])
recoveryCode binder_names sig_fn
  = do  { traceTc "tcBindsWithSigs: error recovery" (ppr binder_names)
        ; let poly_ids = map mk_dummy binder_names
        ; return (emptyBag, poly_ids) }
  where
    mk_dummy name
      | Just sig <- sig_fn name
      , Just poly_id <- completeSigPolyId_maybe sig
      = poly_id
      | otherwise
      = mkLocalId name forall_a_a

forall_a_a :: TcType
forall_a_a = mkSpecForAllTys [runtimeRep1TyVar, openAlphaTyVar] openAlphaTy

{- *********************************************************************
*                                                                      *
                         tcPolyNoGen
*                                                                      *
********************************************************************* -}

tcPolyNoGen     -- No generalisation whatsoever
  :: RecFlag       -- Whether it's recursive after breaking
                   -- dependencies based on type signatures
  -> TcPragEnv -> TcSigFun
  -> [LHsBind Name]
  -> TcM (LHsBinds TcId, [TcId])

tcPolyNoGen rec_tc prag_fn tc_sig_fn bind_list
  = do { (binds', mono_infos) <- tcMonoBinds rec_tc tc_sig_fn
                                             (LetGblBndr prag_fn)
                                             bind_list
       ; mono_ids' <- mapM tc_mono_info mono_infos
       ; return (binds', mono_ids') }
  where
    tc_mono_info (MBI { mbi_poly_name = name, mbi_mono_id = mono_id })
      = do { _specs <- tcSpecPrags mono_id (lookupPragEnv prag_fn name)
           ; return mono_id }
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
            -> TcIdSigInfo     -- Must be a complete signature
            -> LHsBind Name    -- Must be a FunBind
            -> TcM (LHsBinds TcId, [TcId])
-- There is just one binding,
--   it is a Funbind
--   it has a complete type signature,
tcPolyCheck prag_fn
            (CompleteSig { sig_bndr  = poly_id
                         , sig_ctxt  = ctxt
                         , sig_loc   = sig_loc })
            (L loc (FunBind { fun_id = L nm_loc name
                            , fun_matches = matches }))
  = setSrcSpan sig_loc $
    do { traceTc "tcPolyCheck" (ppr poly_id $$ ppr sig_loc)
       ; (tv_prs, theta, tau) <- tcInstType (tcInstSigTyVars sig_loc) poly_id
                -- See Note [Instantiate sig with fresh variables]

       ; mono_name <- newNameAt (nameOccName name) nm_loc
       ; ev_vars   <- newEvVars theta
       ; let mono_id   = mkLocalId mono_name tau
             skol_info = SigSkol ctxt (mkPhiTy theta tau)
             skol_tvs  = map snd tv_prs

       ; (ev_binds, (co_fn, matches'))
            <- checkConstraints skol_info skol_tvs ev_vars $
               tcExtendIdBndrs [TcIdBndr mono_id NotTopLevel]  $
               tcExtendTyVarEnv2 tv_prs $
               setSrcSpan loc           $
               tcMatchesFun (L nm_loc mono_name) matches (mkCheckExpType tau)

       ; let prag_sigs = lookupPragEnv prag_fn name
       ; spec_prags <- tcSpecPrags poly_id prag_sigs
       ; poly_id    <- addInlinePrags poly_id prag_sigs

       ; mod <- getModule
       ; let bind' = FunBind { fun_id      = L nm_loc mono_id
                             , fun_matches = matches'
                             , fun_co_fn   = co_fn
                             , bind_fvs    = placeHolderNamesTc
                             , fun_tick    = funBindTicks nm_loc mono_id mod prag_sigs }

             abs_bind = L loc $ AbsBindsSig
                        { abs_sig_export  = poly_id
                        , abs_tvs         = skol_tvs
                        , abs_ev_vars     = ev_vars
                        , abs_sig_prags   = SpecPrags spec_prags
                        , abs_sig_ev_bind = ev_binds
                        , abs_sig_bind    = L loc bind' }

       ; return (unitBag abs_bind, [poly_id]) }

tcPolyCheck _prag_fn sig bind
  = pprPanic "tcPolyCheck" (ppr sig $$ ppr bind)

funBindTicks :: SrcSpan -> TcId -> Module -> [LSig Name] -> [Tickish TcId]
funBindTicks loc fun_id mod sigs
  | (mb_cc_str : _) <- [ cc_name | L _ (SCCFunSig _ _ cc_name) <- sigs ]
      -- this can only be a singleton list, as duplicate pragmas are rejected
      -- by the renamer
  , let cc_str
          | Just cc_str <- mb_cc_str
          = sl_fs cc_str
          | otherwise
          = getOccFS (Var.varName fun_id)
        cc_name = moduleNameFS (moduleName mod) `appendFS` consFS '.' cc_str
        cc = mkUserCC cc_name mod loc (getUnique fun_id)
  = [ProfNote cc True True]
  | otherwise
  = []

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

tcPolyInfer
  :: RecFlag       -- Whether it's recursive after breaking
                   -- dependencies based on type signatures
  -> TcPragEnv -> TcSigFun
  -> Bool         -- True <=> apply the monomorphism restriction
  -> [LHsBind Name]
  -> TcM (LHsBinds TcId, [TcId])
tcPolyInfer rec_tc prag_fn tc_sig_fn mono bind_list
  = do { (tclvl, wanted, (binds', mono_infos))
             <- pushLevelAndCaptureConstraints  $
                tcMonoBinds rec_tc tc_sig_fn LetLclBndr bind_list

       ; let name_taus  = [ (mbi_poly_name info, idType (mbi_mono_id info))
                          | info <- mono_infos ]
             sigs       = [ sig | MBI { mbi_sig = Just sig } <- mono_infos ]
             infer_mode = if mono then ApplyMR else NoRestrictions

       ; mapM_ (checkOverloadedSig mono) sigs

       ; traceTc "simplifyInfer call" (ppr tclvl $$ ppr name_taus $$ ppr wanted)
       ; (qtvs, givens, ev_binds)
                 <- simplifyInfer tclvl infer_mode sigs name_taus wanted

       ; let inferred_theta = map evVarPred givens
       ; exports <- checkNoErrs $
                    mapM (mkExport prag_fn qtvs inferred_theta) mono_infos

       ; loc <- getSrcSpanM
       ; let poly_ids = map abe_poly exports
             abs_bind = L loc $
                        AbsBinds { abs_tvs = qtvs
                                 , abs_ev_vars = givens, abs_ev_binds = [ev_binds]
                                 , abs_exports = exports, abs_binds = binds' }

       ; traceTc "Binding:" (ppr (poly_ids `zip` map idType poly_ids))
       ; return (unitBag abs_bind, poly_ids) }
         -- poly_ids are guaranteed zonked by mkExport

--------------
mkExport :: TcPragEnv
         -> [TyVar] -> TcThetaType      -- Both already zonked
         -> MonoBindInfo
         -> TcM (ABExport Id)
-- Only called for generalisation plan InferGen, not by CheckGen or NoGen
--
-- mkExport generates exports with
--      zonked type variables,
--      zonked poly_ids
-- The former is just because no further unifications will change
-- the quantified type variables, so we can fix their final form
-- right now.
-- The latter is needed because the poly_ids are used to extend the
-- type environment; see the invariant on TcEnv.tcExtendIdEnv

-- Pre-condition: the qtvs and theta are already zonked

mkExport prag_fn qtvs theta
         mono_info@(MBI { mbi_poly_name = poly_name
                        , mbi_sig       = mb_sig
                        , mbi_mono_id   = mono_id })
  = do  { mono_ty <- zonkTcType (idType mono_id)
        ; poly_id <- mkInferredPolyId qtvs theta poly_name mb_sig mono_ty

        -- NB: poly_id has a zonked type
        ; poly_id <- addInlinePrags poly_id prag_sigs
        ; spec_prags <- tcSpecPrags poly_id prag_sigs
                -- tcPrags requires a zonked poly_id

        -- See Note [Impedence matching]
        -- NB: we have already done checkValidType, including an ambiguity check,
        --     on the type; either when we checked the sig or in mkInferredPolyId
        ; let poly_ty     = idType poly_id
              sel_poly_ty = mkInfSigmaTy qtvs theta mono_ty
                -- This type is just going into tcSubType,
                -- so Inferred vs. Specified doesn't matter

        ; wrap <- if sel_poly_ty `eqType` poly_ty  -- NB: eqType ignores visibility
                  then return idHsWrapper  -- Fast path; also avoids complaint when we infer
                                           -- an ambiguouse type and have AllowAmbiguousType
                                           -- e..g infer  x :: forall a. F a -> Int
                  else addErrCtxtM (mk_impedence_match_msg mono_info sel_poly_ty poly_ty) $
                       tcSubType_NC sig_ctxt sel_poly_ty poly_ty

        ; warn_missing_sigs <- woptM Opt_WarnMissingLocalSignatures
        ; when warn_missing_sigs $
              localSigWarn Opt_WarnMissingLocalSignatures poly_id mb_sig

        ; return (ABE { abe_wrap = wrap
                        -- abe_wrap :: idType poly_id ~ (forall qtvs. theta => mono_ty)
                      , abe_poly = poly_id
                      , abe_mono = mono_id
                      , abe_prags = SpecPrags spec_prags}) }
  where
    prag_sigs = lookupPragEnv prag_fn poly_name
    sig_ctxt  = InfSigCtxt poly_name

mkInferredPolyId :: [TyVar] -> TcThetaType
                 -> Name -> Maybe TcIdSigInst -> TcType
                 -> TcM TcId
mkInferredPolyId qtvs inferred_theta poly_name mb_sig_inst mono_ty
  | Just (TISI { sig_inst_sig = sig })  <- mb_sig_inst
  , CompleteSig { sig_bndr = poly_id } <- sig
  = return poly_id

  | otherwise  -- Either no type sig or partial type sig
  = checkNoErrs $  -- The checkNoErrs ensures that if the type is ambiguous
                   -- we don't carry on to the impedence matching, and generate
                   -- a duplicate ambiguity error.  There is a similar
                   -- checkNoErrs for complete type signatures too.
    do { fam_envs <- tcGetFamInstEnvs
       ; let (_co, mono_ty') = normaliseType fam_envs Nominal mono_ty
               -- Unification may not have normalised the type,
               -- (see Note [Lazy flattening] in TcFlatten) so do it
               -- here to make it as uncomplicated as possible.
               -- Example: f :: [F Int] -> Bool
               -- should be rewritten to f :: [Char] -> Bool, if possible
               --
               -- We can discard the coercion _co, because we'll reconstruct
               -- it in the call to tcSubType below

       ; (binders, theta') <- chooseInferredQuantifiers inferred_theta
                                (tyCoVarsOfType mono_ty') qtvs mb_sig_inst

       ; let inferred_poly_ty = mkForAllTys binders (mkPhiTy theta' mono_ty')

       ; traceTc "mkInferredPolyId" (vcat [ppr poly_name, ppr qtvs, ppr theta'
                                          , ppr inferred_poly_ty])
       ; addErrCtxtM (mk_inf_msg poly_name inferred_poly_ty) $
         checkValidType (InfSigCtxt poly_name) inferred_poly_ty
         -- See Note [Validity of inferred types]

       ; return (mkLocalIdOrCoVar poly_name inferred_poly_ty) }


chooseInferredQuantifiers :: TcThetaType   -- inferred
                          -> TcTyVarSet    -- tvs free in tau type
                          -> [TcTyVar]     -- inferred quantified tvs
                          -> Maybe TcIdSigInst
                          -> TcM ([TyVarBinder], TcThetaType)
chooseInferredQuantifiers inferred_theta tau_tvs qtvs Nothing
  = -- No type signature (partial or complete) for this binder,
    do { let free_tvs = closeOverKinds (growThetaTyVars inferred_theta tau_tvs)
                        -- Include kind variables!  Trac #7916
             my_theta = pickCapturedPreds free_tvs inferred_theta
             binders  = [ mkTyVarBinder Inferred tv
                        | tv <- qtvs
                        , tv `elemVarSet` free_tvs ]
       ; return (binders, my_theta) }

chooseInferredQuantifiers inferred_theta tau_tvs qtvs
                          (Just (TISI { sig_inst_sig   = sig  -- Always PartialSig
                                      , sig_inst_wcx   = wcx
                                      , sig_inst_theta = annotated_theta
                                      , sig_inst_skols = annotated_tvs }))
  | Nothing <- wcx
  = do { annotated_theta <- zonkTcTypes annotated_theta
       ; let free_tvs = closeOverKinds (tyCoVarsOfTypes annotated_theta
                                        `unionVarSet` tau_tvs)
       ; traceTc "ciq" (vcat [ ppr sig, ppr annotated_theta, ppr free_tvs])
       ; return (mk_binders free_tvs, annotated_theta) }

  | Just wc_var <- wcx
  = do { annotated_theta <- zonkTcTypes annotated_theta
       ; let free_tvs = closeOverKinds (growThetaTyVars inferred_theta seed_tvs)
                          -- growThetaVars just like the no-type-sig case
                          -- Omitting this caused #12844
             seed_tvs = tyCoVarsOfTypes annotated_theta  -- These are put there
                        `unionVarSet` tau_tvs            --       by the user
             my_theta = pickCapturedPreds free_tvs inferred_theta

       -- Report the inferred constraints for an extra-constraints wildcard/hole as
       -- an error message, unless the PartialTypeSignatures flag is enabled. In this
       -- case, the extra inferred constraints are accepted without complaining.
       -- NB: inferred_theta already includes all the annotated constraints
             inferred_diff = [ pred
                             | pred <- my_theta
                             , all (not . (`eqType` pred)) annotated_theta ]
       ; ctuple <- mk_ctuple inferred_diff
       ; writeMetaTyVar wc_var ctuple
       ; traceTc "completeTheta" $
            vcat [ ppr sig
                 , ppr annotated_theta, ppr inferred_theta
                 , ppr inferred_diff ]

       ; return (mk_binders free_tvs, my_theta) }

  | otherwise  -- A complete type signature is dealt with in mkInferredPolyId
  = pprPanic "chooseInferredQuantifiers" (ppr sig)

  where
    spec_tv_set = mkVarSet $ map snd annotated_tvs
    mk_binders free_tvs
      = [ mkTyVarBinder vis tv
        | tv <- qtvs
        , tv `elemVarSet` free_tvs
        , let vis | tv `elemVarSet` spec_tv_set = Specified
                  | otherwise                   = Inferred ]
                          -- Pulling from qtvs maintains original order

    mk_ctuple [pred] = return pred
    mk_ctuple preds  = do { tc <- tcLookupTyCon (cTupleTyConName (length preds))
                          ; return (mkTyConApp tc preds) }

mk_impedence_match_msg :: MonoBindInfo
                       -> TcType -> TcType
                       -> TidyEnv -> TcM (TidyEnv, SDoc)
-- This is a rare but rather awkward error messages
mk_impedence_match_msg (MBI { mbi_poly_name = name, mbi_sig = mb_sig })
                       inf_ty sig_ty tidy_env
 = do { (tidy_env1, inf_ty) <- zonkTidyTcType tidy_env  inf_ty
      ; (tidy_env2, sig_ty) <- zonkTidyTcType tidy_env1 sig_ty
      ; let msg = vcat [ text "When checking that the inferred type"
                       , nest 2 $ ppr name <+> dcolon <+> ppr inf_ty
                       , text "is as general as its" <+> what <+> text "signature"
                       , nest 2 $ ppr name <+> dcolon <+> ppr sig_ty ]
      ; return (tidy_env2, msg) }
  where
    what = case mb_sig of
             Nothing                     -> text "inferred"
             Just sig | isPartialSig sig -> text "(partial)"
                      | otherwise        -> empty


mk_inf_msg :: Name -> TcType -> TidyEnv -> TcM (TidyEnv, SDoc)
mk_inf_msg poly_name poly_ty tidy_env
 = do { (tidy_env1, poly_ty) <- zonkTidyTcType tidy_env poly_ty
      ; let msg = vcat [ text "When checking the inferred type"
                       , nest 2 $ ppr poly_name <+> dcolon <+> ppr poly_ty ]
      ; return (tidy_env1, msg) }


-- | Warn the user about polymorphic local binders that lack type signatures.
localSigWarn :: WarningFlag -> Id -> Maybe TcIdSigInst -> TcM ()
localSigWarn flag id mb_sig
  | Just _ <- mb_sig               = return ()
  | not (isSigmaTy (idType id))    = return ()
  | otherwise                      = warnMissingSignatures flag msg id
  where
    msg = text "Polymorphic local binding with no type signature:"

warnMissingSignatures :: WarningFlag -> SDoc -> Id -> TcM ()
warnMissingSignatures flag msg id
  = do  { env0 <- tcInitTidyEnv
        ; let (env1, tidy_ty) = tidyOpenType env0 (idType id)
        ; addWarnTcM (Reason flag) (env1, mk_msg tidy_ty) }
  where
    mk_msg ty = sep [ msg, nest 2 $ pprPrefixName (idName id) <+> dcolon <+> ppr ty ]

checkOverloadedSig :: Bool -> TcIdSigInst -> TcM ()
-- Example:
--   f :: Eq a => a -> a
--   K f = e
-- The MR applies, but the signature is overloaded, and it's
-- best to complain about this directly
-- c.f Trac #11339
checkOverloadedSig monomorphism_restriction_applies sig
  | not (null (sig_inst_theta sig))
  , monomorphism_restriction_applies
  , let orig_sig = sig_inst_sig sig
  = setSrcSpan (sig_loc orig_sig) $
    failWith $
    hang (text "Overloaded signature conflicts with monomorphism restriction")
       2 (ppr orig_sig)
  | otherwise
  = return ()

{- Note [Partial type signatures and generalisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If /any/ of the signatures in the gropu is a partial type signature
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

We apply the MR if /all/ of the partial signatures lack a context.
In particular (Trac #11016):
   f2 :: (?loc :: Int) => _
   f2 = ?loc
It's stupid to apply the MR here.  This test includes an extra-constraints
wildcard; that is, we don't apply the MR if you write
   f3 :: _ => blah

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


Note [Impedence matching]
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

   f a d1 d2 = case tuple a Any d1 d2 of (f, g) -> f
   g b = case tuple Integer b dEqInteger dNumInteger of (f,g) -> g

Suppose the shared quantified tyvars are qtvs and constraints theta.
Then we want to check that
     forall qtvs. theta => f_mono_ty   is more polymorphic than   f's polytype
and the proof is the impedance matcher.

Notice that the impedance matcher may do defaulting.  See Trac #7173.

It also cleverly does an ambiguity check; for example, rejecting
   f :: F a -> F a
where F is a non-injective type function.
-}

{- *********************************************************************
*                                                                      *
                         Vectorisation
*                                                                      *
********************************************************************* -}

tcVectDecls :: [LVectDecl Name] -> TcM ([LVectDecl TcId])
tcVectDecls decls
  = do { decls' <- mapM (wrapLocM tcVect) decls
       ; let ids  = [lvectDeclName decl | decl <- decls', not $ lvectInstDecl decl]
             dups = findDupsEq (==) ids
       ; mapM_ reportVectDups dups
       ; traceTcConstraints "End of tcVectDecls"
       ; return decls'
       }
  where
    reportVectDups (first:_second:_more)
      = addErrAt (getSrcSpan first) $
          text "Duplicate vectorisation declarations for" <+> ppr first
    reportVectDups _ = return ()

--------------
tcVect :: VectDecl Name -> TcM (VectDecl TcId)
-- FIXME: We can't typecheck the expression of a vectorisation declaration against the vectorised
--   type of the original definition as this requires internals of the vectoriser not available
--   during type checking.  Instead, constrain the rhs of a vectorisation declaration to be a single
--   identifier (this is checked in 'rnHsVectDecl').  Fix this by enabling the use of 'vectType'
--   from the vectoriser here.
tcVect (HsVect s name rhs)
  = addErrCtxt (vectCtxt name) $
    do { var <- wrapLocM tcLookupId $ unLEmb name
       ; let L rhs_loc (HsVar (L lv rhs_var_name)) = rhs
       ; rhs_id <- tcLookupId $ unEmb rhs_var_name
       ; return $ HsVect s (reLEmb name (unLoc var))
                          (L rhs_loc (HsVar (L lv (reEmb rhs_var_name rhs_id))))
       }

tcVect (HsNoVect s name)
  = addErrCtxt (vectCtxt $ unLEmb name) $
    do { var <- wrapLocM tcLookupId $ unLEmb name
       ; return $ HsNoVect s (reLEmb name (unLoc var))
       }
tcVect (HsVectTypeIn _ isScalar lname rhs_name)
  = addErrCtxt (vectCtxt lname) $
    do { tycon <- tcLookupLocatedTyCon $ unLEmb lname
       ; checkTc (   not isScalar             -- either    we have a non-SCALAR declaration
                 || isJust rhs_name           -- or        we explicitly provide a vectorised type
                 || tyConArity tycon == 0     -- otherwise the type constructor must be nullary
                 )
                 scalarTyConMustBeNullary

       ; rhs_tycon <- fmapMaybeM (tcLookupTyCon . unLocEmb) rhs_name
       ; return $ HsVectTypeOut isScalar tycon rhs_tycon
       }
tcVect (HsVectTypeOut _ _ _)
  = panic "TcBinds.tcVect: Unexpected 'HsVectTypeOut'"
tcVect (HsVectClassIn _ lname)
  = addErrCtxt (vectCtxt lname) $
    do { cls <- tcLookupLocatedClass $ unLEmb lname
       ; return $ HsVectClassOut cls
       }
tcVect (HsVectClassOut _)
  = panic "TcBinds.tcVect: Unexpected 'HsVectClassOut'"
tcVect (HsVectInstIn linstTy)
  = addErrCtxt (vectCtxt linstTy) $
    do { (cls, tys) <- tcHsVectInst linstTy
       ; inst       <- tcLookupInstance cls tys
       ; return $ HsVectInstOut inst
       }
tcVect (HsVectInstOut _)
  = panic "TcBinds.tcVect: Unexpected 'HsVectInstOut'"

vectCtxt :: Outputable thing => thing -> SDoc
vectCtxt thing = text "When checking the vectorisation declaration for" <+> ppr thing

scalarTyConMustBeNullary :: MsgDoc
scalarTyConMustBeNullary = text "VECTORISE SCALAR type constructor must be nullary"

{-
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
                        , mbi_mono_id   :: TcId }

tcMonoBinds :: RecFlag  -- Whether the binding is recursive for typechecking purposes
                        -- i.e. the binders are mentioned in their RHSs, and
                        --      we are not rescued by a type signature
            -> TcSigFun -> LetBndrSpec
            -> [LHsBind Name]
            -> TcM (LHsBinds TcId, [MonoBindInfo])
tcMonoBinds is_rec sig_fn no_gen
           [ L b_loc (FunBind { fun_id = L nm_loc name,
                                fun_matches = matches, bind_fvs = fvs })]
                             -- Single function binding,
  | NonRecursive <- is_rec   -- ...binder isn't mentioned in RHS
  , Nothing <- sig_fn name   -- ...with no type signature
  =     -- Note [Single function non-recursive binding special-case]
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        -- In this very special case we infer the type of the
        -- right hand side first (it may have a higher-rank type)
        -- and *then* make the monomorphic Id for the LHS
        -- e.g.         f = \(x::forall a. a->a) -> <body>
        --      We want to infer a higher-rank type for f
    setSrcSpan b_loc    $
    do  { ((co_fn, matches'), rhs_ty)
            <- tcInferInst $ \ exp_ty ->
                  -- tcInferInst: see TcUnify,
                  -- Note [Deep instantiation of InferResult]
               tcExtendIdBndrs [TcIdBndr_ExpType name exp_ty NotTopLevel] $
                  -- We extend the error context even for a non-recursive
                  -- function so that in type error messages we show the
                  -- type of the thing whose rhs we are type checking
               tcMatchesFun (L nm_loc name) matches exp_ty

        ; mono_id <- newLetBndr no_gen name rhs_ty
        ; return (unitBag $ L b_loc $
                     FunBind { fun_id = L nm_loc mono_id,
                               fun_matches = matches', bind_fvs = fvs,
                               fun_co_fn = co_fn, fun_tick = [] },
                  [MBI { mbi_poly_name = name
                       , mbi_sig       = Nothing
                       , mbi_mono_id   = mono_id }]) }

tcMonoBinds _ sig_fn no_gen binds
  = do  { tc_binds <- mapM (wrapLocM (tcLhs sig_fn no_gen)) binds

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
        ; binds' <- tcExtendLetEnvIds NotTopLevel rhs_id_env $
                    mapM (wrapLocM tcRhs) tc_binds

        ; return (listToBag binds', mono_infos) }


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
  = TcFunBind  MonoBindInfo  SrcSpan (MatchGroup Name (LHsExpr Name))
  | TcPatBind [MonoBindInfo] (LPat TcId) (GRHSs Name (LHsExpr Name)) TcSigmaType

tcLhs :: TcSigFun -> LetBndrSpec -> HsBind Name -> TcM TcMonoBind
-- Only called with plan InferGen (LetBndrSpec = LetLclBndr)
--                    or NoGen    (LetBndrSpec = LetGblBndr)
-- CheckGen is used only for functions with a complete type signature,
--          and tcPolyCheck doesn't use tcMonoBinds at all

tcLhs sig_fn no_gen (FunBind { fun_id = L nm_loc name, fun_matches = matches })
  | Just (TcIdSig sig) <- sig_fn name
  = -- There is a type signature.
    -- It must be partial; if complete we'd be in tcPolyCheck!
    --    e.g.   f :: _ -> _
    --           f x = ...g...
    --           Just g = ...f...
    -- Hence always typechecked with InferGen
    do { mono_info <- tcLhsSigId no_gen (name, sig)
       ; return (TcFunBind mono_info nm_loc matches) }

  | otherwise  -- No type signature
  = do { mono_ty <- newOpenFlexiTyVarTy
       ; mono_id <- newLetBndr no_gen name mono_ty
       ; let mono_info = MBI { mbi_poly_name = name
                             , mbi_sig       = Nothing
                             , mbi_mono_id   = mono_id }
       ; return (TcFunBind mono_info nm_loc matches) }

tcLhs sig_fn no_gen (PatBind { pat_lhs = pat, pat_rhs = grhss })
  = -- See Note [Typechecking pattern bindings]
    do  { sig_mbis <- mapM (tcLhsSigId no_gen) sig_names

        ; let inst_sig_fun = lookupNameEnv $ mkNameEnv $
                             [ (mbi_poly_name mbi, mbi_mono_id mbi)
                             | mbi <- sig_mbis ]

            -- See Note [Existentials in pattern bindings]
        ; ((pat', nosig_mbis), pat_ty)
            <- addErrCtxt (patMonoBindsCtxt pat grhss) $
               tcInferNoInst $ \ exp_ty ->
               tcLetPat inst_sig_fun no_gen pat exp_ty $
               mapM lookup_info nosig_names

        ; let mbis = sig_mbis ++ nosig_mbis

        ; traceTc "tcLhs" (vcat [ ppr id <+> dcolon <+> ppr (idType id)
                                | mbi <- mbis, let id = mbi_mono_id mbi ]
                           $$ ppr no_gen)

        ; return (TcPatBind mbis pat' grhss pat_ty) }
  where
    bndr_names = collectPatBinders pat
    (nosig_names, sig_names) = partitionWith find_sig bndr_names

    find_sig :: Name -> Either Name (Name, TcIdSigInfo)
    find_sig name = case sig_fn name of
                      Just (TcIdSig sig) -> Right (name, sig)
                      _                  -> Left name

      -- After typechecking the pattern, look up the binder
      -- names that lack a signature, which the pattern has brought
      -- into scope.
    lookup_info :: Name -> TcM MonoBindInfo
    lookup_info name
      = do { mono_id <- tcLookupId name
           ; return (MBI { mbi_poly_name = name
                         , mbi_sig       = Nothing
                         , mbi_mono_id   = mono_id }) }

tcLhs _ _ other_bind = pprPanic "tcLhs" (ppr other_bind)
        -- AbsBind, VarBind impossible

-------------------
tcLhsSigId :: LetBndrSpec -> (Name, TcIdSigInfo) -> TcM MonoBindInfo
tcLhsSigId no_gen (name, sig)
  = do { inst_sig <- tcInstSig sig
       ; mono_id <- newSigLetBndr no_gen name inst_sig
       ; return (MBI { mbi_poly_name = name
                     , mbi_sig       = Just inst_sig
                     , mbi_mono_id   = mono_id }) }

------------
newSigLetBndr :: LetBndrSpec -> Name -> TcIdSigInst -> TcM TcId
newSigLetBndr (LetGblBndr prags) name (TISI { sig_inst_sig = id_sig })
  | CompleteSig { sig_bndr = poly_id } <- id_sig
  = addInlinePrags poly_id (lookupPragEnv prags name)
newSigLetBndr no_gen name (TISI { sig_inst_tau = tau })
  = newLetBndr no_gen name tau

-------------------
tcRhs :: TcMonoBind -> TcM (HsBind TcId)
tcRhs (TcFunBind info@(MBI { mbi_sig = mb_sig, mbi_mono_id = mono_id })
                 loc matches)
  = tcExtendIdBinderStackForRhs [info]  $
    tcExtendTyVarEnvForRhs mb_sig       $
    do  { traceTc "tcRhs: fun bind" (ppr mono_id $$ ppr (idType mono_id))
        ; (co_fn, matches') <- tcMatchesFun (L loc (idName mono_id))
                                 matches (mkCheckExpType $ idType mono_id)
        ; return ( FunBind { fun_id = L loc mono_id
                           , fun_matches = matches'
                           , fun_co_fn = co_fn
                           , bind_fvs = placeHolderNamesTc
                           , fun_tick = [] } ) }

tcRhs (TcPatBind infos pat' grhss pat_ty)
  = -- When we are doing pattern bindings we *don't* bring any scoped
    -- type variables into scope unlike function bindings
    -- Wny not?  They are not completely rigid.
    -- That's why we have the special case for a single FunBind in tcMonoBinds
    tcExtendIdBinderStackForRhs infos        $
    do  { traceTc "tcRhs: pat bind" (ppr pat' $$ ppr pat_ty)
        ; grhss' <- addErrCtxt (patMonoBindsCtxt pat' grhss) $
                    tcGRHSsPat grhss pat_ty
        ; return ( PatBind { pat_lhs = pat', pat_rhs = grhss'
                           , pat_rhs_ty = pat_ty
                           , bind_fvs = placeHolderNamesTc
                           , pat_ticks = ([],[]) } )}

tcExtendTyVarEnvForRhs :: Maybe TcIdSigInst -> TcM a -> TcM a
tcExtendTyVarEnvForRhs Nothing thing_inside
  = thing_inside
tcExtendTyVarEnvForRhs (Just sig) thing_inside
  = tcExtendTyVarEnvFromSig sig thing_inside

tcExtendTyVarEnvFromSig :: TcIdSigInst -> TcM a -> TcM a
tcExtendTyVarEnvFromSig sig_inst thing_inside
  | TISI { sig_inst_skols = skol_prs, sig_inst_wcs = wcs } <- sig_inst
  = tcExtendTyVarEnv2 wcs $
    tcExtendTyVarEnv2 skol_prs $
    thing_inside

tcExtendIdBinderStackForRhs :: [MonoBindInfo] -> TcM a -> TcM a
-- Extend the TcIdBinderStack for the RHS of the binding, with
-- the monomorphic Id.  That way, if we have, say
--     f = \x -> blah
-- and something goes wrong in 'blah', we get a "relevant binding"
-- looking like  f :: alpha -> beta
-- This applies if 'f' has a type signature too:
--    f :: forall a. [a] -> [a]
--    f x = True
-- We can't unify True with [a], and a relevant binding is f :: [a] -> [a]
-- If we had the *polymorphic* version of f in the TcIdBinderStack, it
-- would not be reported as relevant, because its type is closed
tcExtendIdBinderStackForRhs infos thing_inside
  = tcExtendIdBndrs [ TcIdBndr mono_id NotTopLevel
                    | MBI { mbi_mono_id = mono_id } <- infos ]
                    thing_inside
    -- NotTopLevel: it's a monomorphic binding

---------------------
getMonoBindInfo :: [Located TcMonoBind] -> [MonoBindInfo]
getMonoBindInfo tc_binds
  = foldr (get_info . unLoc) [] tc_binds
  where
    get_info (TcFunBind info _ _)    rest = info : rest
    get_info (TcPatBind infos _ _ _) rest = infos ++ rest


{- Note [Typechecking pattern bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Look at:
   - typecheck/should_compile/ExPat
   - Trac #12427, typecheck/should_compile/T12427{a,b}

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

* Even (E3) is fine.  The existential pattern binds a dictionary
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
       TcPat.tcLetPat, and put into the pc_lvl field of PatCtxt, in
       PatEnv.

     - When tcPat finds an existential constructor, it binds fresh
       type variables and dictionaries as usual, increments the TcLevel,
       and emits an implication constraint.

     - When we come to a binder (TcPat.tcPatBndr), it looks it up
       in the little environment (the pc_sig_fn field of PatCtxt).

         Success => There was a type signature, so just use it,
                    checking compatibility with the expected type.

         Failure => No type sigature.
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
       there is one; see Trac #7268). The mkExport part of the
       generalisation step will do the checking and impedance matching
       against the signature.

     - If for some some reason we are not generalising (plan = NoGen), the
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
The beta is untoucable, but floats out of the constraint and can
be solved absolutely fine.

************************************************************************
*                                                                      *
                Generalisation
*                                                                      *
********************************************************************* -}

data GeneralisationPlan
  = NoGen               -- No generalisation, no AbsBinds

  | InferGen            -- Implicit generalisation; there is an AbsBinds
       Bool             --   True <=> apply the MR; generalise only unconstrained type vars

  | CheckGen (LHsBind Name) TcIdSigInfo
                        -- One FunBind with a signature
                        -- Explicit generalisation; there is an AbsBindsSig

-- A consequence of the no-AbsBinds choice (NoGen) is that there is
-- no "polymorphic Id" and "monmomorphic Id"; there is just the one

instance Outputable GeneralisationPlan where
  ppr NoGen          = text "NoGen"
  ppr (InferGen b)   = text "InferGen" <+> ppr b
  ppr (CheckGen _ s) = text "CheckGen" <+> ppr s

decideGeneralisationPlan
   :: DynFlags -> [LHsBind Name] -> IsGroupClosed -> TcSigFun
   -> GeneralisationPlan
decideGeneralisationPlan dflags lbinds closed sig_fn
  | has_partial_sigs                         = InferGen (and partial_sig_mrs)
  | Just (bind, sig) <- one_funbind_with_sig = CheckGen bind sig
  | mono_local_binds closed                  = NoGen
  | otherwise                                = InferGen mono_restriction
  where
    binds = map unLoc lbinds

    partial_sig_mrs :: [Bool]
    -- One for each parital signature (so empty => no partial sigs)
    -- The Bool is True if the signature has no constraint context
    --      so we should apply the MR
    -- See Note [Partial type signatures and generalisation]
    partial_sig_mrs
      = [ null theta
        | TcIdSig (PartialSig { psig_hs_ty = hs_ty })
            <- mapMaybe sig_fn (collectHsBindListBinders lbinds)
        , let (_, L _ theta, _) = splitLHsSigmaTy (hsSigWcType hs_ty) ]

    has_partial_sigs   = not (null partial_sig_mrs)

    mono_restriction  = xopt LangExt.MonomorphismRestriction dflags
                     && any restricted binds

    mono_local_binds ClosedGroup = False
    mono_local_binds _           = xopt LangExt.MonoLocalBinds dflags

    -- With OutsideIn, all nested bindings are monomorphic
    -- except a single function binding with a signature
    one_funbind_with_sig
      | [lbind@(L _ (FunBind { fun_id = v }))] <- lbinds
      , Just (TcIdSig sig) <- sig_fn (unLoc v)
      = Just (lbind, sig)
      | otherwise
      = Nothing

    -- The Haskell 98 monomorphism restriction
    restricted (PatBind {})                              = True
    restricted (VarBind { var_id = v })                  = no_sig v
    restricted (FunBind { fun_id = v, fun_matches = m }) = restricted_match m
                                                           && no_sig (unLoc v)
    restricted (PatSynBind {}) = panic "isRestrictedGroup/unrestricted PatSynBind"
    restricted (AbsBinds {}) = panic "isRestrictedGroup/unrestricted AbsBinds"
    restricted (AbsBindsSig {}) = panic "isRestrictedGroup/unrestricted AbsBindsSig"

    restricted_match (MG { mg_alts = L _ (L _ (Match _ [] _ _) : _ )}) = True
    restricted_match _                                                 = False
        -- No args => like a pattern binding
        -- Some args => a function binding

    no_sig n = noCompleteSig (sig_fn n)

isClosedBndrGroup :: Bag (LHsBind Name) -> TcM IsGroupClosed
isClosedBndrGroup binds = do
    type_env <- getLclTypeEnv
    if foldUFM (is_closed_ns type_env) True fv_env
      then return ClosedGroup
      else return $ NonClosedGroup fv_env
  where
    fv_env :: NameEnv NameSet
    fv_env = mkNameEnv $ concatMap (bindFvs . unLoc) binds

    bindFvs :: HsBindLR Name idR -> [(Name, NameSet)]
    bindFvs (FunBind { fun_id = f, bind_fvs = fvs })
       = [(unLoc f, fvs)]
    bindFvs (PatBind { pat_lhs = pat, bind_fvs = fvs })
       = [(b, fvs) | b <- collectPatBinders pat]
    bindFvs _
       = []

    is_closed_ns :: TcTypeEnv -> NameSet -> Bool -> Bool
    is_closed_ns type_env ns b = b && nameSetAll (is_closed_id type_env) ns
        -- ns are the Names referred to from the RHS of this bind

    is_closed_id :: TcTypeEnv -> Name -> Bool
    -- See Note [Bindings with closed types] in TcRnTypes
    is_closed_id type_env name
      | Just thing <- lookupNameEnv type_env name
      = case thing of
          ATcId { tct_info = ClosedLet } -> True  -- This is the key line
          ATcId {}                       -> False
          ATyVar {}                      -> False -- In-scope type variables
          AGlobal {}                     -> True  --    are not closed!
          _                              -> pprPanic "is_closed_id" (ppr name)
      | otherwise
      = True
        -- The free-var set for a top level binding mentions
        -- imported things too, so that we can report unused imports
        -- These won't be in the local type env.
        -- Ditto class method etc from the current module

{- *********************************************************************
*                                                                      *
               Error contexts and messages
*                                                                      *
********************************************************************* -}

-- This one is called on LHS, when pat and grhss are both Name
-- and on RHS, when pat is TcId and grhss is still Name
patMonoBindsCtxt :: (OutputableBndrId id, Outputable body)
                 => LPat id -> GRHSs Name body -> SDoc
patMonoBindsCtxt pat grhss
  = hang (text "In a pattern binding:") 2 (pprPatBind pat grhss)
