%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcBinds]{TcBinds}

\begin{code}
module TcBinds ( tcLocalBinds, tcTopBinds, 
                 tcHsBootSigs, tcMonoBinds, tcPolyBinds,
                 TcPragFun, tcPrags, mkPragFun, 
                 TcSigInfo(..), TcSigFun, mkTcSigFun,
                 badBootDeclErr ) where

import {-# SOURCE #-} TcMatches ( tcGRHSsPat, tcMatchesFun )
import {-# SOURCE #-} TcExpr  ( tcMonoExpr )

import DynFlags
import HsSyn

import TcRnMonad
import Inst
import TcEnv
import TcUnify
import TcSimplify
import TcHsType
import TcPat
import TcMType
import TcType
import Coercion
import VarEnv
import TysPrim
import Id
import Var
import Name
import NameSet
import NameEnv
import VarSet
import SrcLoc
import Bag
import ErrUtils
import Digraph
import Maybes
import Util
import BasicTypes
import Outputable
import FastString

import Data.List( partition )
import Control.Monad
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Type-checking bindings}
%*                                                                      *
%************************************************************************

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

\begin{code}
tcTopBinds :: HsValBinds Name -> TcM (LHsBinds TcId, TcLclEnv)
        -- Note: returning the TcLclEnv is more than we really
        --       want.  The bit we care about is the local bindings
        --       and the free type variables thereof
tcTopBinds binds
  = do  { (ValBindsOut prs _, env) <- tcValBinds TopLevel binds getLclEnv
        ; return (foldr (unionBags . snd) emptyBag prs, env) }
        -- The top level bindings are flattened into a giant 
        -- implicitly-mutually-recursive LHsBinds

tcHsBootSigs :: HsValBinds Name -> TcM [Id]
-- A hs-boot file has only one BindGroup, and it only has type
-- signatures in it.  The renamer checked all this
tcHsBootSigs (ValBindsOut binds sigs)
  = do  { checkTc (null binds) badBootDeclErr
        ; mapM (addLocM tc_boot_sig) (filter isTypeLSig sigs) }
  where
    tc_boot_sig (TypeSig (L _ name) ty)
      = do { sigma_ty <- tcHsSigType (FunSigCtxt name) ty
           ; return (mkVanillaGlobal name sigma_ty) }
        -- Notice that we make GlobalIds, not LocalIds
    tc_boot_sig s = pprPanic "tcHsBootSigs/tc_boot_sig" (ppr s)
tcHsBootSigs groups = pprPanic "tcHsBootSigs" (ppr groups)

badBootDeclErr :: Message
badBootDeclErr = ptext (sLit "Illegal declarations in an hs-boot file")

------------------------
tcLocalBinds :: HsLocalBinds Name -> TcM thing
             -> TcM (HsLocalBinds TcId, thing)

tcLocalBinds EmptyLocalBinds thing_inside 
  = do  { thing <- thing_inside
        ; return (EmptyLocalBinds, thing) }

tcLocalBinds (HsValBinds binds) thing_inside
  = do  { (binds', thing) <- tcValBinds NotTopLevel binds thing_inside
        ; return (HsValBinds binds', thing) }

tcLocalBinds (HsIPBinds (IPBinds ip_binds _)) thing_inside
  = do  { (thing, lie) <- getLIE thing_inside
        ; (avail_ips, ip_binds') <- mapAndUnzipM (wrapLocSndM tc_ip_bind) ip_binds

        -- If the binding binds ?x = E, we  must now 
        -- discharge any ?x constraints in expr_lie
        ; dict_binds <- tcSimplifyIPs avail_ips lie
        ; return (HsIPBinds (IPBinds ip_binds' dict_binds), thing) }
  where
        -- I wonder if we should do these one at at time
        -- Consider     ?x = 4
        --              ?y = ?x + 1
    tc_ip_bind (IPBind ip expr) = do
        ty <- newFlexiTyVarTy argTypeKind
        (ip', ip_inst) <- newIPDict (IPBindOrigin ip) ip ty
        expr' <- tcMonoExpr expr ty
        return (ip_inst, (IPBind ip' expr'))

------------------------
tcValBinds :: TopLevelFlag 
           -> HsValBinds Name -> TcM thing
           -> TcM (HsValBinds TcId, thing) 

tcValBinds _ (ValBindsIn binds _) _
  = pprPanic "tcValBinds" (ppr binds)

tcValBinds top_lvl (ValBindsOut binds sigs) thing_inside
  = do  {       -- Typecheck the signature
        ; let { prag_fn = mkPragFun sigs (foldr (unionBags . snd) emptyBag binds)
              ; ty_sigs = filter isTypeLSig sigs
              ; sig_fn  = mkTcSigFun ty_sigs }

        ; poly_ids <- checkNoErrs (mapAndRecoverM tcTySig ty_sigs)
                -- No recovery from bad signatures, because the type sigs
                -- may bind type variables, so proceeding without them
                -- can lead to a cascade of errors
                -- ToDo: this means we fall over immediately if any type sig
                -- is wrong, which is over-conservative, see Trac bug #745

                -- Extend the envt right away with all 
                -- the Ids declared with type signatures
        ; poly_rec <- doptM Opt_RelaxedPolyRec
        ; (binds', thing) <- tcExtendIdEnv poly_ids $
                             tcBindGroups poly_rec top_lvl sig_fn prag_fn 
                                          binds thing_inside

        ; return (ValBindsOut binds' sigs, thing) }

------------------------
tcBindGroups :: Bool -> TopLevelFlag -> TcSigFun -> TcPragFun
             -> [(RecFlag, LHsBinds Name)] -> TcM thing
             -> TcM ([(RecFlag, LHsBinds TcId)], thing)
-- Typecheck a whole lot of value bindings,
-- one strongly-connected component at a time
-- Here a "strongly connected component" has the strightforward
-- meaning of a group of bindings that mention each other, 
-- ignoring type signatures (that part comes later)

tcBindGroups _ _ _ _ [] thing_inside
  = do  { thing <- thing_inside
        ; return ([], thing) }

tcBindGroups poly_rec top_lvl sig_fn prag_fn (group : groups) thing_inside
  = do  { (group', (groups', thing))
                <- tc_group poly_rec top_lvl sig_fn prag_fn group $ 
                   tcBindGroups poly_rec top_lvl sig_fn prag_fn groups thing_inside
        ; return (group' ++ groups', thing) }

------------------------
tc_group :: Bool -> TopLevelFlag -> TcSigFun -> TcPragFun
         -> (RecFlag, LHsBinds Name) -> TcM thing
         -> TcM ([(RecFlag, LHsBinds TcId)], thing)

-- Typecheck one strongly-connected component of the original program.
-- We get a list of groups back, because there may 
-- be specialisations etc as well

tc_group _ top_lvl sig_fn prag_fn (NonRecursive, binds) thing_inside
        -- A single non-recursive binding
        -- We want to keep non-recursive things non-recursive
        -- so that we desugar unlifted bindings correctly
 =  do  { (binds1, lie_binds, thing) <- tc_haskell98 top_lvl sig_fn prag_fn 
    	  	   	      	     		     NonRecursive binds thing_inside
        ; return ( [(NonRecursive, unitBag b) | b <- bagToList binds1]
	  	     ++ [(Recursive, lie_binds)]  -- TcDictBinds have scrambled dependency order
		 , thing) }

tc_group poly_rec top_lvl sig_fn prag_fn (Recursive, binds) thing_inside
  | not poly_rec        -- Recursive group, normal Haskell 98 route
  = do  { (binds1, lie_binds, thing) <- tc_haskell98 top_lvl sig_fn prag_fn 
    	  	   	      	     		     Recursive binds thing_inside
        ; return ([(Recursive, binds1 `unionBags` lie_binds)], thing) }

  | otherwise           -- Recursive group, with -XRelaxedPolyRec
  =     -- To maximise polymorphism (with -XRelaxedPolyRec), we do a new 
        -- strongly-connected-component analysis, this time omitting 
        -- any references to variables with type signatures.
        --
        -- Notice that the bindInsts thing covers *all* the bindings in
        -- the original group at once; an earlier one may use a later one!
    do  { traceTc (text "tc_group rec" <+> pprLHsBinds binds)
        ; (binds1,lie_binds,thing) <- bindLocalInsts top_lvl $
                            go (stronglyConnCompFromEdgedVertices (mkEdges sig_fn binds))
        ; return ([(Recursive, binds1 `unionBags` lie_binds)], thing) }
                -- Rec them all together
  where
--  go :: SCC (LHsBind Name) -> TcM (LHsBinds TcId, [TcId], thing)
    go (scc:sccs) = do  { (binds1, ids1) <- tc_scc scc
                        ; (binds2, ids2, thing) <- tcExtendIdEnv ids1 $ go sccs
                        ; return (binds1 `unionBags` binds2, ids1 ++ ids2, thing) }
    go []         = do  { thing <- thing_inside; return (emptyBag, [], thing) }

    tc_scc (AcyclicSCC bind) = tc_sub_group NonRecursive (unitBag bind)
    tc_scc (CyclicSCC binds) = tc_sub_group Recursive    (listToBag binds)

    tc_sub_group = tcPolyBinds top_lvl sig_fn prag_fn Recursive

tc_haskell98 :: TopLevelFlag -> TcSigFun -> TcPragFun -> RecFlag
             -> LHsBinds Name -> TcM a -> TcM (LHsBinds TcId, TcDictBinds, a)
tc_haskell98 top_lvl sig_fn prag_fn rec_flag binds thing_inside
  = bindLocalInsts top_lvl $ 
    do { (binds1, ids) <- tcPolyBinds top_lvl sig_fn prag_fn rec_flag rec_flag binds
       ; thing <- tcExtendIdEnv ids thing_inside
       ; return (binds1, ids, thing) }

------------------------
bindLocalInsts :: TopLevelFlag
	       -> TcM (LHsBinds TcId, [TcId],      a)
	       -> TcM (LHsBinds TcId, TcDictBinds, a)
bindLocalInsts top_lvl thing_inside
  | isTopLevel top_lvl
  = do { (binds, _, thing) <- thing_inside; return (binds, emptyBag, thing) }
        -- For the top level don't bother with all this bindInstsOfLocalFuns stuff. 
        -- All the top level things are rec'd together anyway, so it's fine to
        -- leave them to the tcSimplifyTop, and quite a bit faster too

  | otherwise   -- Nested case
  = do  { ((binds, ids, thing), lie) <- getLIE thing_inside
        ; lie_binds <- bindInstsOfLocalFuns lie ids
        ; return (binds, lie_binds, thing) }

------------------------
mkEdges :: TcSigFun -> LHsBinds Name
        -> [(LHsBind Name, BKey, [BKey])]

type BKey  = Int -- Just number off the bindings

mkEdges sig_fn binds
  = [ (bind, key, [key | n <- nameSetToList (bind_fvs (unLoc bind)),
                         Just key <- [lookupNameEnv key_map n], no_sig n ])
    | (bind, key) <- keyd_binds
    ]
  where
    no_sig :: Name -> Bool
    no_sig n = isNothing (sig_fn n)

    keyd_binds = bagToList binds `zip` [0::BKey ..]

    key_map :: NameEnv BKey     -- Which binding it comes from
    key_map = mkNameEnv [(bndr, key) | (L _ bind, key) <- keyd_binds
                                     , bndr <- bindersOfHsBind bind ]

bindersOfHsBind :: HsBind Name -> [Name]
bindersOfHsBind (PatBind { pat_lhs = pat })  = collectPatBinders pat
bindersOfHsBind (FunBind { fun_id = L _ f }) = [f]
bindersOfHsBind (AbsBinds {})                = panic "bindersOfHsBind AbsBinds"
bindersOfHsBind (VarBind {})                 = panic "bindersOfHsBind VarBind"

------------------------
tcPolyBinds :: TopLevelFlag -> TcSigFun -> TcPragFun
            -> RecFlag                  -- Whether the group is really recursive
            -> RecFlag                  -- Whether it's recursive after breaking
                                        -- dependencies based on type signatures
            -> LHsBinds Name
            -> TcM (LHsBinds TcId, [TcId])

-- Typechecks a single bunch of bindings all together, 
-- and generalises them.  The bunch may be only part of a recursive
-- group, because we use type signatures to maximise polymorphism
--
-- Returns a list because the input may be a single non-recursive binding,
-- in which case the dependency order of the resulting bindings is
-- important.  
-- 
-- Knows nothing about the scope of the bindings

tcPolyBinds top_lvl sig_fn prag_fn rec_group rec_tc binds
  = let 
        bind_list    = bagToList binds
        binder_names = collectHsBindsBinders binds
        loc          = getLoc (head bind_list)
                -- TODO: location a bit awkward, but the mbinds have been
                --       dependency analysed and may no longer be adjacent
    in
        -- SET UP THE MAIN RECOVERY; take advantage of any type sigs
    setSrcSpan loc                              $
    recoverM (recoveryCode binder_names sig_fn) $ do 

  { traceTc (ptext (sLit "------------------------------------------------"))
  ; traceTc (ptext (sLit "Bindings for") <+> ppr binder_names)

        -- TYPECHECK THE BINDINGS
  ; ((binds', mono_bind_infos), lie_req) 
        <- getLIE (tcMonoBinds bind_list sig_fn rec_tc)
  ; traceTc (text "temp" <+> (ppr binds' $$ ppr lie_req))

        -- CHECK FOR UNLIFTED BINDINGS
        -- These must be non-recursive etc, and are not generalised
        -- They desugar to a case expression in the end
  ; zonked_mono_tys <- zonkTcTypes (map getMonoType mono_bind_infos)
  ; is_strict <- checkStrictBinds top_lvl rec_group binds' 
                                  zonked_mono_tys mono_bind_infos
  ; if is_strict then
    do  { extendLIEs lie_req
        ; let exports = zipWith mk_export mono_bind_infos zonked_mono_tys
              mk_export (name, mb_sig,  mono_id) mono_ty 
                = ([], the_id, mono_id, noSpecPrags)
                              -- ToDo: prags for unlifted bindings
	      	where
                  the_id = case mb_sig of
                             Just sig -> sig_id sig
                             Nothing  -> mkLocalId name mono_ty

        ; return ( unitBag $ L loc $ AbsBinds [] [] exports binds',
                   [poly_id | (_, poly_id, _, _) <- exports]) } -- Guaranteed zonked

    else do     -- The normal lifted case: GENERALISE
  { dflags <- getDOpts 
  ; (tyvars_to_gen, dicts, dict_binds)
        <- addErrCtxt (genCtxt (bndrNames mono_bind_infos)) $
           generalise dflags top_lvl bind_list sig_fn mono_bind_infos lie_req

        -- BUILD THE POLYMORPHIC RESULT IDs
  ; let dict_vars = map instToVar dicts -- May include equality constraints
  ; exports <- mapM (mkExport top_lvl rec_group (length mono_bind_infos > 1)
                              prag_fn tyvars_to_gen (map varType dict_vars))
                    mono_bind_infos

  ; let poly_ids = [poly_id | (_, poly_id, _, _) <- exports]
  ; traceTc (text "binding:" <+> ppr (poly_ids `zip` map idType poly_ids))

  ; let abs_bind = L loc $ AbsBinds tyvars_to_gen
                                    dict_vars exports
                                    (dict_binds `unionBags` binds')

  ; return (unitBag abs_bind, poly_ids)       -- poly_ids are guaranteed zonked by mkExport
  } }


--------------
mkExport :: TopLevelFlag -> RecFlag
	 -> Bool	 -- More than one variable is bound, so we'll desugar to
	    		 -- a tuple, so INLINE pragmas won't work
         -> TcPragFun -> [TyVar] -> [TcType]
         -> MonoBindInfo
         -> TcM ([TyVar], Id, Id, TcSpecPrags)
-- mkExport generates exports with 
--      zonked type variables, 
--      zonked poly_ids
-- The former is just because no further unifications will change
-- the quantified type variables, so we can fix their final form
-- right now.
-- The latter is needed because the poly_ids are used to extend the
-- type environment; see the invariant on TcEnv.tcExtendIdEnv 

-- Pre-condition: the inferred_tvs are already zonked

mkExport top_lvl rec_group multi_bind prag_fn inferred_tvs dict_tys
         (poly_name, mb_sig, mono_id)
  = do  { warn_missing_sigs <- doptM Opt_WarnMissingSigs
        ; let warn = isTopLevel top_lvl && warn_missing_sigs
        ; (tvs, poly_id) <- mk_poly_id warn mb_sig
                -- poly_id has a zonked type

        ; (poly_id', spec_prags) <- tcPrags rec_group multi_bind (notNull dict_tys)
                                            poly_id (prag_fn poly_name)
                -- tcPrags requires a zonked poly_id

        ; return (tvs, poly_id', mono_id, SpecPrags spec_prags) }
  where
    poly_ty = mkForAllTys inferred_tvs (mkFunTys dict_tys (idType mono_id))

    mk_poly_id warn Nothing    = do { poly_ty' <- zonkTcType poly_ty
                                    ; missingSigWarn warn poly_name poly_ty'
                                    ; return (inferred_tvs, mkLocalId poly_name poly_ty') }
    mk_poly_id _    (Just sig) = do { tvs <- mapM zonk_tv (sig_tvs sig)
                                    ; return (tvs,  sig_id sig) }

    zonk_tv tv = do { ty <- zonkTcTyVar tv; return (tcGetTyVar "mkExport" ty) }

------------------------
type TcPragFun = Name -> [LSig Name]

mkPragFun :: [LSig Name] -> LHsBinds Name -> TcPragFun
mkPragFun sigs binds = \n -> lookupNameEnv prag_env n `orElse` []
  where
    prs = mapCatMaybes get_sig sigs

    get_sig :: LSig Name -> Maybe (Located Name, LSig Name)
    get_sig (L l (SpecSig nm ty inl)) = Just (nm, L l $ SpecSig  nm ty (add_arity nm inl))
    get_sig (L l (InlineSig nm inl))  = Just (nm, L l $ InlineSig nm   (add_arity nm inl))
    get_sig _                         = Nothing

    add_arity (L _ n) inl_prag   -- Adjust inl_sat field to match visible arity of function
      | Just ar <- lookupNameEnv ar_env n = inl_prag { inl_sat = Just ar }
      | otherwise                         = inl_prag

    prag_env :: NameEnv [LSig Name]
    prag_env = foldl add emptyNameEnv prs
    add env (L _ n,p) = extendNameEnv_Acc (:) singleton env n p

    -- ar_env maps a local to the arity of its definition
    ar_env :: NameEnv Arity
    ar_env = foldrBag lhsBindArity emptyNameEnv binds

lhsBindArity :: LHsBind Name -> NameEnv Arity -> NameEnv Arity
lhsBindArity (L _ (FunBind { fun_id = id, fun_matches = ms })) env
  = extendNameEnv env (unLoc id) (matchGroupArity ms)
lhsBindArity _ env = env	-- PatBind/VarBind

tcPrags :: RecFlag
	-> Bool     -- True <=> AbsBinds binds more than one variable
        -> Bool     -- True <=> function is overloaded
        -> Id -> [LSig Name]
        -> TcM (Id, [Located TcSpecPrag])
-- Add INLINE and SPECIALSE pragmas
--    INLINE prags are added to the (polymorphic) Id directly
--    SPECIALISE prags are passed to the desugarer via TcSpecPrags
-- Pre-condition: the poly_id is zonked
-- Reason: required by tcSubExp
tcPrags _rec_group _multi_bind is_overloaded_id poly_id prag_sigs
  = do { poly_id' <- tc_inl inl_sigs

       ; spec_prags <- mapM (wrapLocM (tcSpecPrag poly_id')) spec_sigs

       ; unless (null spec_sigs || is_overloaded_id) warn_discarded_spec

       ; unless (null bad_sigs) warn_discarded_sigs

       ; return (poly_id', spec_prags) }
  where
    (inl_sigs, other_sigs) = partition isInlineLSig prag_sigs
    (spec_sigs, bad_sigs)  = partition isSpecLSig   other_sigs

    warn_discarded_spec = warnPrags poly_id spec_sigs $
                          ptext (sLit "SPECIALISE pragmas for non-overloaded function")
    warn_dup_inline 	= warnPrags poly_id inl_sigs $
                    	  ptext (sLit "Duplicate INLINE pragmas for")
    warn_discarded_sigs = warnPrags poly_id bad_sigs $
                          ptext (sLit "Discarding unexpected pragmas for")

    -----------
    tc_inl [] = return poly_id
    tc_inl (L loc (InlineSig _ prag) : other_inls)
       = do { unless (null other_inls) (setSrcSpan loc warn_dup_inline)
            ; return (poly_id `setInlinePragma` prag) }
    tc_inl _ = panic "tc_inl"

{- Earlier we tried to warn about
   (a) INLINE for recursive function
   (b) INLINE for function that is part of a multi-binder group
   Code fragments below. But we want to allow
       {-# INLINE f #-}
       f x = x : g y
       g y = ....f...f....
   even though they are mutually recursive.  
   So I'm just omitting the warnings for now

       | multi_bind && isInlinePragma prag
       = do { setSrcSpan loc $ addWarnTc multi_bind_warn
            ; return poly_id }
       | otherwise
            ; when (isInlinePragma prag && isRec rec_group)
                   (setSrcSpan loc (addWarnTc rec_inline_warn))

    rec_inline_warn = ptext (sLit "INLINE pragma for recursive binder")
                      <+> quotes (ppr poly_id) <+> ptext (sLit "may be discarded")
 
    multi_bind_warn = hang (ptext (sLit "Discarding INLINE pragma for") <+> quotes (ppr poly_id))
		         2 (ptext (sLit "because it is bound by a pattern, or mutual recursion") )
-}


warnPrags :: Id -> [LSig Name] -> SDoc -> TcM ()
warnPrags id bad_sigs herald
  = addWarnTc (hang (herald <+> quotes (ppr id))
                  2 (ppr_sigs bad_sigs))
  where
    ppr_sigs sigs = vcat (map (ppr . getLoc) sigs)

--------------
tcSpecPrag :: TcId -> Sig Name -> TcM TcSpecPrag
tcSpecPrag poly_id prag@(SpecSig _ hs_ty inl) 
  = addErrCtxt (spec_ctxt prag) $
    do  { let name = idName poly_id
        ; spec_ty <- tcHsSigType (FunSigCtxt name) hs_ty
        ; co_fn <- tcSubExp (SpecPragOrigin name) (idType poly_id) spec_ty
        ; return (SpecPrag co_fn inl) }
  where
    spec_ctxt prag = hang (ptext (sLit "In the SPECIALISE pragma")) 2 (ppr prag)
tcSpecPrag _ sig = pprPanic "tcSpecPrag" (ppr sig)


--------------
-- If typechecking the binds fails, then return with each
-- signature-less binder given type (forall a.a), to minimise 
-- subsequent error messages
recoveryCode :: [Name] -> (Name -> Maybe [Name])
             -> TcM (LHsBinds TcId, [Id])
recoveryCode binder_names sig_fn
  = do  { traceTc (text "tcBindsWithSigs: error recovery" <+> ppr binder_names)
        ; poly_ids <- mapM mk_dummy binder_names
        ; return (emptyBag, poly_ids) }
  where
    mk_dummy name 
        | isJust (sig_fn name) = tcLookupId name        -- Had signature; look it up
        | otherwise            = return (mkLocalId name forall_a_a)    -- No signature

forall_a_a :: TcType
forall_a_a = mkForAllTy alphaTyVar (mkTyVarTy alphaTyVar)


-- Check that non-overloaded unlifted bindings are
--      a) non-recursive,
--      b) not top level, 
--      c) not a multiple-binding group (more or less implied by (a))

checkStrictBinds :: TopLevelFlag -> RecFlag
                 -> LHsBinds TcId -> [TcType] -> [MonoBindInfo]
                 -> TcM Bool
checkStrictBinds top_lvl rec_group mbind mono_tys infos
  | unlifted || bang_pat
  = do  { checkTc (isNotTopLevel top_lvl)
                  (strictBindErr "Top-level" unlifted mbind)
        ; checkTc (isNonRec rec_group)
                  (strictBindErr "Recursive" unlifted mbind)
        ; checkTc (isSingletonBag mbind)
                  (strictBindErr "Multiple" unlifted mbind) 
        -- This should be a checkTc, not a warnTc, but as of GHC 6.11
        -- the versions of alex and happy available have non-conforming
        -- templates, so the GHC build fails if it's an error:
        ; warnUnlifted <- doptM Opt_WarnLazyUnliftedBindings
        ; warnTc (warnUnlifted && not bang_pat)
                 (unliftedMustBeBang mbind)
        ; mapM_ check_sig infos
        ; return True }
  | otherwise
  = return False
  where
    unlifted = any isUnLiftedType mono_tys
    bang_pat = anyBag (isBangHsBind . unLoc) mbind
    check_sig (_, Just sig, _) = checkTc (null (sig_tvs sig) && null (sig_theta sig))
                                         (badStrictSig unlifted sig)
    check_sig _                = return ()

unliftedMustBeBang :: LHsBindsLR Var Var -> SDoc
unliftedMustBeBang mbind
  = hang (text "Bindings containing unlifted types must use an outermost bang pattern:")
         4 (pprLHsBinds mbind)
 $$ text "*** This will be an error in GHC 7.2! Fix your code now!"

strictBindErr :: String -> Bool -> LHsBindsLR Var Var -> SDoc
strictBindErr flavour unlifted mbind
  = hang (text flavour <+> msg <+> ptext (sLit "aren't allowed:")) 
         4 (pprLHsBinds mbind)
  where
    msg | unlifted  = ptext (sLit "bindings for unlifted types")
        | otherwise = ptext (sLit "bang-pattern bindings")

badStrictSig :: Bool -> TcSigInfo -> SDoc
badStrictSig unlifted sig
  = hang (ptext (sLit "Illegal polymorphic signature in") <+> msg)
         4 (ppr sig)
  where
    msg | unlifted  = ptext (sLit "an unlifted binding")
        | otherwise = ptext (sLit "a bang-pattern binding")
\end{code}


%************************************************************************
%*                                                                      *
\subsection{tcMonoBind}
%*                                                                      *
%************************************************************************

@tcMonoBinds@ deals with a perhaps-recursive group of HsBinds.
The signatures have been dealt with already.

\begin{code}
tcMonoBinds :: [LHsBind Name]
            -> TcSigFun
            -> RecFlag  -- Whether the binding is recursive for typechecking purposes
                        -- i.e. the binders are mentioned in their RHSs, and
                        --      we are not resuced by a type signature
            -> TcM (LHsBinds TcId, [MonoBindInfo])

tcMonoBinds [L b_loc (FunBind { fun_id = L nm_loc name, fun_infix = inf, 
                                fun_matches = matches, bind_fvs = fvs })]
            sig_fn              -- Single function binding,
            NonRecursive        -- binder isn't mentioned in RHS,
  | Nothing <- sig_fn name      -- ...with no type signature
  =     -- In this very special case we infer the type of the
        -- right hand side first (it may have a higher-rank type)
        -- and *then* make the monomorphic Id for the LHS
        -- e.g.         f = \(x::forall a. a->a) -> <body>
        --      We want to infer a higher-rank type for f
    setSrcSpan b_loc    $
    do  { ((co_fn, matches'), rhs_ty) <- tcInfer (tcMatchesFun name inf matches)

                -- Check for an unboxed tuple type
                --      f = (# True, False #)
                -- Zonk first just in case it's hidden inside a meta type variable
                -- (This shows up as a (more obscure) kind error 
                --  in the 'otherwise' case of tcMonoBinds.)
        ; zonked_rhs_ty <- zonkTcType rhs_ty
        ; checkTc (not (isUnboxedTupleType zonked_rhs_ty))
                  (unboxedTupleErr name zonked_rhs_ty)

        ; mono_name <- newLocalName name
        ; let mono_id = mkLocalId mono_name zonked_rhs_ty
        ; return (unitBag (L b_loc (FunBind { fun_id = L nm_loc mono_id, fun_infix = inf,
                                              fun_matches = matches', bind_fvs = fvs,
                                              fun_co_fn = co_fn, fun_tick = Nothing })),
                  [(name, Nothing, mono_id)]) }

tcMonoBinds [L b_loc (FunBind { fun_id = L nm_loc name, fun_infix = inf, 
                                fun_matches = matches })]
            sig_fn              -- Single function binding
            _
  | Just scoped_tvs <- sig_fn name      -- ...with a type signature
  =     -- When we have a single function binding, with a type signature
        -- we can (a) use genuine, rigid skolem constants for the type variables
        --        (b) bring (rigid) scoped type variables into scope
    setSrcSpan b_loc    $
    do  { tc_sig <- tcInstSig True name
        ; mono_name <- newLocalName name
        ; let mono_ty = sig_tau tc_sig
              mono_id = mkLocalId mono_name mono_ty
              rhs_tvs = [ (name, mkTyVarTy tv)
                        | (name, tv) <- scoped_tvs `zip` sig_tvs tc_sig ]
                        -- See Note [More instantiated than scoped]
                        -- Note that the scoped_tvs and the (sig_tvs sig) 
                        -- may have different Names. That's quite ok.

	; traceTc (text "tcMoonBinds" <+> ppr scoped_tvs $$ ppr tc_sig)
        ; (co_fn, matches') <- tcExtendTyVarEnv2 rhs_tvs $
                               tcMatchesFun mono_name inf matches mono_ty
	     -- Note that "mono_ty" might actually be a polymorphic type,
	     -- if the original function had a signature like
	     --    forall a. Eq a => forall b. Ord b => ....
	     -- But that's ok: tcMatchesFun can deal with that
	     -- It happens, too!  See Note [Polymorphic methods] in TcClassDcl.

        ; let fun_bind' = FunBind { fun_id = L nm_loc mono_id, 
                                    fun_infix = inf, fun_matches = matches',
                                    bind_fvs = placeHolderNames, fun_co_fn = co_fn, 
                                    fun_tick = Nothing }
        ; return (unitBag (L b_loc fun_bind'),
                  [(name, Just tc_sig, mono_id)]) }

tcMonoBinds binds sig_fn _
  = do  { tc_binds <- mapM (wrapLocM (tcLhs sig_fn)) binds

        -- Bring the monomorphic Ids, into scope for the RHSs
        ; let mono_info  = getMonoBindInfo tc_binds
              rhs_id_env = [(name,mono_id) | (name, Nothing, mono_id) <- mono_info]
                                -- A monomorphic binding for each term variable that lacks 
                                -- a type sig.  (Ones with a sig are already in scope.)

        ; binds' <- tcExtendIdEnv2 rhs_id_env $ do
                    traceTc (text "tcMonoBinds" <+> vcat [ ppr n <+> ppr id <+> ppr (idType id) 
                                                         | (n,id) <- rhs_id_env])
                    mapM (wrapLocM tcRhs) tc_binds
        ; return (listToBag binds', mono_info) }

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
  = TcFunBind  MonoBindInfo  (Located TcId) Bool (MatchGroup Name) 
  | TcPatBind [MonoBindInfo] (LPat TcId) (GRHSs Name) TcSigmaType

type MonoBindInfo = (Name, Maybe TcSigInfo, TcId)
        -- Type signature (if any), and
        -- the monomorphic bound things

bndrNames :: [MonoBindInfo] -> [Name]
bndrNames mbi = [n | (n,_,_) <- mbi]

getMonoType :: MonoBindInfo -> TcTauType
getMonoType (_,_,mono_id) = idType mono_id

tcLhs :: TcSigFun -> HsBind Name -> TcM TcMonoBind
tcLhs sig_fn (FunBind { fun_id = L nm_loc name, fun_infix = inf, fun_matches = matches })
  = do  { mb_sig <- tcInstSig_maybe sig_fn name
        ; mono_name <- newLocalName name
        ; mono_ty   <- mk_mono_ty mb_sig
        ; let mono_id = mkLocalId mono_name mono_ty
        ; return (TcFunBind (name, mb_sig, mono_id) (L nm_loc mono_id) inf matches) }
  where
    mk_mono_ty (Just sig) = return (sig_tau sig)
    mk_mono_ty Nothing    = newFlexiTyVarTy argTypeKind

tcLhs sig_fn (PatBind { pat_lhs = pat, pat_rhs = grhss })
  = do  { mb_sigs <- mapM (tcInstSig_maybe sig_fn) names
        ; mono_pat_binds <- doptM Opt_MonoPatBinds
                -- With -XMonoPatBinds, we do no generalisation of pattern bindings
                -- But the signature can still be polymoprhic!
                --      data T = MkT (forall a. a->a)
                --      x :: forall a. a->a
                --      MkT x = <rhs>
                -- The function get_sig_ty decides whether the pattern-bound variables
                -- should have exactly the type in the type signature (-XMonoPatBinds), 
                -- or the instantiated version (-XMonoPatBinds)

        ; let nm_sig_prs  = names `zip` mb_sigs
              get_sig_ty | mono_pat_binds = idType . sig_id
                         | otherwise      = sig_tau
              tau_sig_env = mkNameEnv [ (name, get_sig_ty sig) 
                                      | (name, Just sig) <- nm_sig_prs]
              sig_tau_fn  = lookupNameEnv tau_sig_env

              tc_pat exp_ty = tcLetPat sig_tau_fn pat exp_ty $
                              mapM lookup_info nm_sig_prs

                -- After typechecking the pattern, look up the binder
                -- names, which the pattern has brought into scope.
              lookup_info :: (Name, Maybe TcSigInfo) -> TcM MonoBindInfo
              lookup_info (name, mb_sig) = do { mono_id <- tcLookupId name
                                              ; return (name, mb_sig, mono_id) }

        ; ((pat', infos), pat_ty) <- addErrCtxt (patMonoBindsCtxt pat grhss) $
                                     tcInfer tc_pat

        ; return (TcPatBind infos pat' grhss pat_ty) }
  where
    names = collectPatBinders pat


tcLhs _ other_bind = pprPanic "tcLhs" (ppr other_bind)
        -- AbsBind, VarBind impossible

-------------------
tcRhs :: TcMonoBind -> TcM (HsBind TcId)
-- When we are doing pattern bindings, or multiple function bindings at a time
-- we *don't* bring any scoped type variables into scope
-- Wny not?  They are not completely rigid.
-- That's why we have the special case for a single FunBind in tcMonoBinds
tcRhs (TcFunBind (_,_,mono_id) fun' inf matches)
  = do  { (co_fn, matches') <- tcMatchesFun (idName mono_id) inf 
                                            matches (idType mono_id)
        ; return (FunBind { fun_id = fun', fun_infix = inf, fun_matches = matches',
                            bind_fvs = placeHolderNames, fun_co_fn = co_fn,
                            fun_tick = Nothing }) }

tcRhs (TcPatBind _ pat' grhss pat_ty)
  = do  { grhss' <- addErrCtxt (patMonoBindsCtxt pat' grhss) $
                    tcGRHSsPat grhss pat_ty
        ; return (PatBind { pat_lhs = pat', pat_rhs = grhss', pat_rhs_ty = pat_ty, 
                            bind_fvs = placeHolderNames }) }


---------------------
getMonoBindInfo :: [Located TcMonoBind] -> [MonoBindInfo]
getMonoBindInfo tc_binds
  = foldr (get_info . unLoc) [] tc_binds
  where
    get_info (TcFunBind info _ _ _)  rest = info : rest
    get_info (TcPatBind infos _ _ _) rest = infos ++ rest
\end{code}


%************************************************************************
%*                                                                      *
                Generalisation
%*                                                                      *
%************************************************************************

\begin{code}
generalise :: DynFlags -> TopLevelFlag 
           -> [LHsBind Name] -> TcSigFun 
           -> [MonoBindInfo] -> [Inst]
           -> TcM ([TyVar], [Inst], TcDictBinds)
-- The returned [TyVar] are all ready to quantify

generalise dflags top_lvl bind_list sig_fn mono_infos lie_req
  | isMonoGroup dflags top_lvl bind_list sigs
  = do  { extendLIEs lie_req
        ; return ([], [], emptyBag) }

  | isRestrictedGroup dflags bind_list sig_fn   -- RESTRICTED CASE
  =     -- Check signature contexts are empty 
    do  { checkTc (all is_mono_sig sigs)
                  (restrictedBindCtxtErr bndrs)

        -- Now simplify with exactly that set of tyvars
        -- We have to squash those Methods
        ; (qtvs, binds) <- tcSimplifyRestricted doc top_lvl bndrs 
                                                tau_tvs lie_req

        -- Check that signature type variables are OK
        ; final_qtvs <- checkSigsTyVars qtvs sigs

        ; return (final_qtvs, [], binds) }

  | null sigs   -- UNRESTRICTED CASE, NO TYPE SIGS
  = tcSimplifyInfer doc tau_tvs lie_req

  | otherwise   -- UNRESTRICTED CASE, WITH TYPE SIGS
  = do  { sig_lie <- unifyCtxts sigs    -- sigs is non-empty; sig_lie is zonked
        ; let   -- The "sig_avails" is the stuff available.  We get that from
                -- the context of the type signature, BUT ALSO the lie_avail
                -- so that polymorphic recursion works right (see Note [Polymorphic recursion])
                local_meths = [mkMethInst sig mono_id | (_, Just sig, mono_id) <- mono_infos]
                sig_avails = sig_lie ++ local_meths
                loc = sig_loc (head sigs)

        -- Check that the needed dicts can be
        -- expressed in terms of the signature ones
        ; (qtvs, binds) <- tcSimplifyInferCheck loc tau_tvs sig_avails lie_req
        
        -- Check that signature type variables are OK
        ; final_qtvs <- checkSigsTyVars qtvs sigs

        ; return (final_qtvs, sig_lie, binds) }
  where
    bndrs   = bndrNames mono_infos
    sigs    = [sig | (_, Just sig, _) <- mono_infos]
    get_tvs | isTopLevel top_lvl = tyVarsOfType  -- See Note [Silly type synonym] in TcType
            | otherwise          = exactTyVarsOfType
    tau_tvs = foldr (unionVarSet . get_tvs . getMonoType) emptyVarSet mono_infos
    is_mono_sig sig = null (sig_theta sig)
    doc = ptext (sLit "type signature(s) for") <+> pprBinders bndrs

    mkMethInst (TcSigInfo { sig_id = poly_id, sig_tvs = tvs, 
                            sig_theta = theta, sig_loc = loc }) mono_id
      = Method {tci_id = mono_id, tci_oid = poly_id, tci_tys = mkTyVarTys tvs,
                tci_theta = theta, tci_loc = loc}
\end{code}

unifyCtxts checks that all the signature contexts are the same
The type signatures on a mutually-recursive group of definitions
must all have the same context (or none).

The trick here is that all the signatures should have the same
context, and we want to share type variables for that context, so that
all the right hand sides agree a common vocabulary for their type
constraints

We unify them because, with polymorphic recursion, their types
might not otherwise be related.  This is a rather subtle issue.

\begin{code}
unifyCtxts :: [TcSigInfo] -> TcM [Inst]
-- Post-condition: the returned Insts are full zonked
unifyCtxts [] = panic "unifyCtxts []"
unifyCtxts (sig1 : sigs)        -- Argument is always non-empty
  = do  { traceTc $ text "unifyCtxts" <+> ppr (sig1 : sigs)
	; mapM_ unify_ctxt sigs
        ; theta <- zonkTcThetaType (sig_theta sig1)
        ; newDictBndrs (sig_loc sig1) theta }
  where
    theta1 = sig_theta sig1
    unify_ctxt :: TcSigInfo -> TcM ()
    unify_ctxt sig@(TcSigInfo { sig_theta = theta })
        = setSrcSpan (instLocSpan (sig_loc sig))        $
          addErrCtxt (sigContextsCtxt sig1 sig)         $
          do { cois <- unifyTheta theta1 theta
             ; -- Check whether all coercions are identity coercions
               -- That can happen if we have, say
               --         f :: C [a]   => ...
               --         g :: C (F a) => ...
               -- where F is a type function and (F a ~ [a])
               -- Then unification might succeed with a coercion.  But it's much
               -- much simpler to require that such signatures have identical contexts
               checkTc (all isIdentityCoI cois)
                       (ptext (sLit "Mutually dependent functions have syntactically distinct contexts"))
             }

checkSigsTyVars :: [TcTyVar] -> [TcSigInfo] -> TcM [TcTyVar]
checkSigsTyVars qtvs sigs 
  = do  { gbl_tvs <- tcGetGlobalTyVars
        ; sig_tvs_s <- mapM (check_sig gbl_tvs) sigs

        ; let   -- Sigh.  Make sure that all the tyvars in the type sigs
                -- appear in the returned ty var list, which is what we are
                -- going to generalise over.  Reason: we occasionally get
                -- silly types like
                --      type T a = () -> ()
                --      f :: T a
                --      f () = ()
                -- Here, 'a' won't appear in qtvs, so we have to add it
                sig_tvs = foldl extendVarSetList emptyVarSet sig_tvs_s
                all_tvs = varSetElems (extendVarSetList sig_tvs qtvs)
        ; return all_tvs }
  where
    check_sig gbl_tvs (TcSigInfo {sig_id = id, sig_tvs = tvs, 
                                  sig_theta = theta, sig_tau = tau})
      = addErrCtxt (ptext (sLit "In the type signature for") <+> quotes (ppr id))       $
        addErrCtxtM (sigCtxt id tvs theta tau)                                          $
        do { tvs' <- checkDistinctTyVars tvs
           ; when (any (`elemVarSet` gbl_tvs) tvs')
                  (bleatEscapedTvs gbl_tvs tvs tvs')
           ; return tvs' }

checkDistinctTyVars :: [TcTyVar] -> TcM [TcTyVar]
-- (checkDistinctTyVars tvs) checks that the tvs from one type signature
-- are still all type variables, and all distinct from each other.  
-- It returns a zonked set of type variables.
-- For example, if the type sig is
--      f :: forall a b. a -> b -> b
-- we want to check that 'a' and 'b' haven't 
--      (a) been unified with a non-tyvar type
--      (b) been unified with each other (all distinct)

checkDistinctTyVars sig_tvs
  = do  { zonked_tvs <- mapM zonkSigTyVar sig_tvs
        ; foldlM_ check_dup emptyVarEnv (sig_tvs `zip` zonked_tvs)
        ; return zonked_tvs }
  where
    check_dup :: TyVarEnv TcTyVar -> (TcTyVar, TcTyVar) -> TcM (TyVarEnv TcTyVar)
        -- The TyVarEnv maps each zonked type variable back to its
        -- corresponding user-written signature type variable
    check_dup acc (sig_tv, zonked_tv)
        = case lookupVarEnv acc zonked_tv of
                Just sig_tv' -> bomb_out sig_tv sig_tv'

                Nothing -> return (extendVarEnv acc zonked_tv sig_tv)

    bomb_out sig_tv1 sig_tv2
       = do { env0 <- tcInitTidyEnv
            ; let (env1, tidy_tv1) = tidyOpenTyVar env0 sig_tv1
                  (env2, tidy_tv2) = tidyOpenTyVar env1 sig_tv2
                  msg = ptext (sLit "Quantified type variable") <+> quotes (ppr tidy_tv1) 
                         <+> ptext (sLit "is unified with another quantified type variable") 
                         <+> quotes (ppr tidy_tv2)
            ; failWithTcM (env2, msg) }
\end{code}


@getTyVarsToGen@ decides what type variables to generalise over.

For a "restricted group" -- see the monomorphism restriction
for a definition -- we bind no dictionaries, and
remove from tyvars_to_gen any constrained type variables

*Don't* simplify dicts at this point, because we aren't going
to generalise over these dicts.  By the time we do simplify them
we may well know more.  For example (this actually came up)
        f :: Array Int Int
        f x = array ... xs where xs = [1,2,3,4,5]
We don't want to generate lots of (fromInt Int 1), (fromInt Int 2)
stuff.  If we simplify only at the f-binding (not the xs-binding)
we'll know that the literals are all Ints, and we can just produce
Int literals!

Find all the type variables involved in overloading, the
"constrained_tyvars".  These are the ones we *aren't* going to
generalise.  We must be careful about doing this:

 (a) If we fail to generalise a tyvar which is not actually
        constrained, then it will never, ever get bound, and lands
        up printed out in interface files!  Notorious example:
                instance Eq a => Eq (Foo a b) where ..
        Here, b is not constrained, even though it looks as if it is.
        Another, more common, example is when there's a Method inst in
        the LIE, whose type might very well involve non-overloaded
        type variables.
  [NOTE: Jan 2001: I don't understand the problem here so I'm doing 
        the simple thing instead]

 (b) On the other hand, we mustn't generalise tyvars which are constrained,
        because we are going to pass on out the unmodified LIE, with those
        tyvars in it.  They won't be in scope if we've generalised them.

So we are careful, and do a complete simplification just to find the
constrained tyvars. We don't use any of the results, except to
find which tyvars are constrained.

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



%************************************************************************
%*                                                                      *
                Signatures
%*                                                                      *
%************************************************************************

Type signatures are tricky.  See Note [Signature skolems] in TcType

@tcSigs@ checks the signatures for validity, and returns a list of
{\em freshly-instantiated} signatures.  That is, the types are already
split up, and have fresh type variables installed.  All non-type-signature
"RenamedSigs" are ignored.

The @TcSigInfo@ contains @TcTypes@ because they are unified with
the variable's type, and after that checked to see whether they've
been instantiated.

Note [Scoped tyvars]
~~~~~~~~~~~~~~~~~~~~
The -XScopedTypeVariables flag brings lexically-scoped type variables
into scope for any explicitly forall-quantified type variables:
        f :: forall a. a -> a
        f x = e
Then 'a' is in scope inside 'e'.

However, we do *not* support this 
  - For pattern bindings e.g
        f :: forall a. a->a
        (f,g) = e

  - For multiple function bindings, unless Opt_RelaxedPolyRec is on
        f :: forall a. a -> a
        f = g
        g :: forall b. b -> b
        g = ...f...
    Reason: we use mutable variables for 'a' and 'b', since they may
    unify to each other, and that means the scoped type variable would
    not stand for a completely rigid variable.

    Currently, we simply make Opt_ScopedTypeVariables imply Opt_RelaxedPolyRec


Note [More instantiated than scoped]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There may be more instantiated type variables than lexically-scoped 
ones.  For example:
        type T a = forall b. b -> (a,b)
        f :: forall c. T c
Here, the signature for f will have one scoped type variable, c,
but two instantiated type variables, c' and b'.  

We assume that the scoped ones are at the *front* of sig_tvs,
and remember the names from the original HsForAllTy in the TcSigFun.


\begin{code}
type TcSigFun = Name -> Maybe [Name]    -- Maps a let-binder to the list of
                                        -- type variables brought into scope
                                        -- by its type signature.
                                        -- Nothing => no type signature

mkTcSigFun :: [LSig Name] -> TcSigFun
-- Search for a particular type signature
-- Precondition: the sigs are all type sigs
-- Precondition: no duplicates
mkTcSigFun sigs = lookupNameEnv env
  where
    env = mkNameEnv (mapCatMaybes mk_pair sigs)
    mk_pair (L _ (TypeSig (L _ name) lhs_ty)) = Just (name, hsExplicitTvs lhs_ty)
    mk_pair (L _ (IdSig id))                  = Just (idName id, [])
    mk_pair _                                 = Nothing    
        -- The scoped names are the ones explicitly mentioned
        -- in the HsForAll.  (There may be more in sigma_ty, because
        -- of nested type synonyms.  See Note [More instantiated than scoped].)
        -- See Note [Only scoped tyvars are in the TyVarEnv]

---------------
data TcSigInfo
  = TcSigInfo {
        sig_id     :: TcId,             --  *Polymorphic* binder for this value...

        sig_tvs    :: [TcTyVar],        -- Instantiated type variables
                                        -- See Note [Instantiate sig]

        sig_theta  :: TcThetaType,      -- Instantiated theta
        sig_tau    :: TcTauType,        -- Instantiated tau
        sig_loc    :: InstLoc           -- The location of the signature
    }


--      Note [Only scoped tyvars are in the TyVarEnv]
-- We are careful to keep only the *lexically scoped* type variables in
-- the type environment.  Why?  After all, the renamer has ensured
-- that only legal occurrences occur, so we could put all type variables
-- into the type env.
--
-- But we want to check that two distinct lexically scoped type variables
-- do not map to the same internal type variable.  So we need to know which
-- the lexically-scoped ones are... and at the moment we do that by putting
-- only the lexically scoped ones into the environment.


--      Note [Instantiate sig]
-- It's vital to instantiate a type signature with fresh variables.
-- For example:
--      type S = forall a. a->a
--      f,g :: S
--      f = ...
--      g = ...
-- Here, we must use distinct type variables when checking f,g's right hand sides.
-- (Instantiation is only necessary because of type synonyms.  Otherwise,
-- it's all cool; each signature has distinct type variables from the renamer.)

instance Outputable TcSigInfo where
    ppr (TcSigInfo { sig_id = id, sig_tvs = tyvars, sig_theta = theta, sig_tau = tau})
        = ppr id <+> ptext (sLit "::") <+> ppr tyvars <+> ppr theta <+> ptext (sLit "=>") <+> ppr tau
\end{code}

\begin{code}
tcTySig :: LSig Name -> TcM TcId
tcTySig (L span (TypeSig (L _ name) ty))
  = setSrcSpan span             $
    do  { sigma_ty <- tcHsSigType (FunSigCtxt name) ty
        ; return (mkLocalId name sigma_ty) }
tcTySig (L _ (IdSig id))
  = return id
tcTySig s = pprPanic "tcTySig" (ppr s)

-------------------
tcInstSig_maybe :: TcSigFun -> Name -> TcM (Maybe TcSigInfo)
-- Instantiate with *meta* type variables; 
-- this signature is part of a multi-signature group
tcInstSig_maybe sig_fn name 
  = case sig_fn name of
        Nothing -> return Nothing
        Just _scoped_tvs -> do   { tc_sig <- tcInstSig False name
                                 ; return (Just tc_sig) }
        -- NB: the _scoped_tvs may be non-empty, but we can 
        -- just ignore them.  See Note [Scoped tyvars].

tcInstSig :: Bool -> Name -> TcM TcSigInfo
-- Instantiate the signature, with either skolems or meta-type variables
-- depending on the use_skols boolean.  This variable is set True
-- when we are typechecking a single function binding; and False for
-- pattern bindings and a group of several function bindings.
-- Reason: in the latter cases, the "skolems" can be unified together, 
--         so they aren't properly rigid in the type-refinement sense.
-- NB: unless we are doing H98, each function with a sig will be done
--     separately, even if it's mutually recursive, so use_skols will be True
--
-- We always instantiate with fresh uniques,
-- although we keep the same print-name
--      
--      type T = forall a. [a] -> [a]
--      f :: T; 
--      f = g where { g :: T; g = <rhs> }
--
-- We must not use the same 'a' from the defn of T at both places!!

tcInstSig use_skols name
  = do  { poly_id <- tcLookupId name    -- Cannot fail; the poly ids are put into 
                                        -- scope when starting the binding group
	; let skol_info = SigSkol (FunSigCtxt name)
        ; (tvs, theta, tau) <- tcInstSigType use_skols skol_info (idType poly_id)
        ; loc <- getInstLoc (SigOrigin skol_info)
        ; return (TcSigInfo { sig_id = poly_id,
                              sig_tvs = tvs, sig_theta = theta, sig_tau = tau, 
                              sig_loc = loc }) }

-------------------
isMonoGroup :: DynFlags -> TopLevelFlag -> [LHsBind Name]
            -> [TcSigInfo] ->  Bool
-- No generalisation at all
isMonoGroup dflags top_lvl binds sigs
  =  (dopt Opt_MonoPatBinds dflags && any is_pat_bind binds)
  || (dopt Opt_MonoLocalBinds dflags && null sigs && not (isTopLevel top_lvl))
  where
    is_pat_bind (L _ (PatBind {})) = True
    is_pat_bind _                  = False

-------------------
isRestrictedGroup :: DynFlags -> [LHsBind Name] -> TcSigFun -> Bool
isRestrictedGroup dflags binds sig_fn
  = mono_restriction && not all_unrestricted
  where 
    mono_restriction = dopt Opt_MonomorphismRestriction dflags
    all_unrestricted = all (unrestricted . unLoc) binds
    has_sig n = isJust (sig_fn n)

    unrestricted (PatBind {})                                    = False
    unrestricted (VarBind { var_id = v })                        = has_sig v
    unrestricted (FunBind { fun_id = v, fun_matches = matches }) = unrestricted_match matches 
                                                                 || has_sig (unLoc v)
    unrestricted (AbsBinds {})
        = panic "isRestrictedGroup/unrestricted AbsBinds"

    unrestricted_match (MatchGroup (L _ (Match [] _ _) : _) _) = False
        -- No args => like a pattern binding
    unrestricted_match _                                       = True
        -- Some args => a function binding
\end{code}


%************************************************************************
%*                                                                      *
\subsection[TcBinds-errors]{Error contexts and messages}
%*                                                                      *
%************************************************************************


\begin{code}
-- This one is called on LHS, when pat and grhss are both Name 
-- and on RHS, when pat is TcId and grhss is still Name
patMonoBindsCtxt :: OutputableBndr id => LPat id -> GRHSs Name -> SDoc
patMonoBindsCtxt pat grhss
  = hang (ptext (sLit "In a pattern binding:")) 4 (pprPatBind pat grhss)

-----------------------------------------------
sigContextsCtxt :: TcSigInfo -> TcSigInfo -> SDoc
sigContextsCtxt sig1 sig2
  = vcat [ptext (sLit "When matching the contexts of the signatures for"), 
          nest 2 (vcat [ppr id1 <+> dcolon <+> ppr (idType id1),
                        ppr id2 <+> dcolon <+> ppr (idType id2)]),
          ptext (sLit "The signature contexts in a mutually recursive group should all be identical")]
  where
    id1 = sig_id sig1
    id2 = sig_id sig2


-----------------------------------------------
unboxedTupleErr :: Name -> Type -> SDoc
unboxedTupleErr name ty
  = hang (ptext (sLit "Illegal binding of unboxed tuple"))
         4 (ppr name <+> dcolon <+> ppr ty)

-----------------------------------------------
restrictedBindCtxtErr :: [Name] -> SDoc
restrictedBindCtxtErr binder_names
  = hang (ptext (sLit "Illegal overloaded type signature(s)"))
       4 (vcat [ptext (sLit "in a binding group for") <+> pprBinders binder_names,
                ptext (sLit "that falls under the monomorphism restriction")])

genCtxt :: [Name] -> SDoc
genCtxt binder_names
  = ptext (sLit "When generalising the type(s) for") <+> pprBinders binder_names

missingSigWarn :: Bool -> Name -> Type -> TcM ()
missingSigWarn False _    _  = return ()
missingSigWarn True  name ty
  = do  { env0 <- tcInitTidyEnv
        ; let (env1, tidy_ty) = tidyOpenType env0 ty
        ; addWarnTcM (env1, mk_msg tidy_ty) }
  where
    mk_msg ty = vcat [ptext (sLit "Definition but no type signature for") <+> quotes (ppr name),
                      sep [ptext (sLit "Inferred type:") <+> pprHsVar name <+> dcolon <+> ppr ty]]
\end{code}
