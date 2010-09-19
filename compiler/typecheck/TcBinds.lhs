%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcBinds]{TcBinds}

\begin{code}
module TcBinds ( tcLocalBinds, tcTopBinds, 
                 tcHsBootSigs, tcPolyBinds,
                 PragFun, tcPrags, mkPragFun, 
                 TcSigInfo(..), SigFun, mkSigFun,
                 badBootDeclErr ) where

import {-# SOURCE #-} TcMatches ( tcGRHSsPat, tcMatchesFun )
import {-# SOURCE #-} TcExpr  ( tcMonoExpr )

import DynFlags
import HsSyn

import TcRnMonad
import TcEnv
import TcUnify
import TcSimplify
import TcHsType
import TcPat
import TcMType
import TcType
import Coercion
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
  = do  { (given_ips, ip_binds') <- mapAndUnzipM (wrapLocSndM tc_ip_bind) ip_binds
        ; let ip_tvs = foldr (unionVarSet . tyVarsOfType . idType) emptyVarSet given_ips

        -- If the binding binds ?x = E, we  must now 
        -- discharge any ?x constraints in expr_lie
        ; (ev_binds, result) <- checkConstraints (IPSkol ips) 
                                  ip_tvs  -- See Note [Implicit parameter untouchables]
                                  [] given_ips $
                                thing_inside

        ; return (HsIPBinds (IPBinds ip_binds' ev_binds), result) }
  where
    ips = [ip | L _ (IPBind ip _) <- ip_binds]

        -- I wonder if we should do these one at at time
        -- Consider     ?x = 4
        --              ?y = ?x + 1
    tc_ip_bind (IPBind ip expr) 
       = do { ty <- newFlexiTyVarTy argTypeKind
            ; ip_id <- newIP ip ty
            ; expr' <- tcMonoExpr expr ty
            ; return (ip_id, (IPBind (IPName ip_id) expr')) }
\end{code}

Note [Implicit parameter untouchables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We add the type variables in the types of the implicit parameters
as untouchables, not so much because we really must not unify them,
but rather because we otherwise end up with constraints like this
    Num alpha, Implic { wanted = alpha ~ Int }
The constraint solver solves alpha~Int by unification, but then
doesn't float that solved constraint out (it's not an unsolved 
wanted.  Result disaster: the (Num alpha) is again solved, this
time by defaulting.  No no no.

\begin{code}
tcValBinds :: TopLevelFlag 
           -> HsValBinds Name -> TcM thing
           -> TcM (HsValBinds TcId, thing) 

tcValBinds _ (ValBindsIn binds _) _
  = pprPanic "tcValBinds" (ppr binds)

tcValBinds top_lvl (ValBindsOut binds sigs) thing_inside
  = do  {       -- Typecheck the signature
        ; let { prag_fn = mkPragFun sigs (foldr (unionBags . snd) emptyBag binds)
              ; ty_sigs = filter isTypeLSig sigs
              ; sig_fn  = mkSigFun ty_sigs }

        ; poly_ids <- checkNoErrs (mapAndRecoverM tcTySig ty_sigs)
                -- No recovery from bad signatures, because the type sigs
                -- may bind type variables, so proceeding without them
                -- can lead to a cascade of errors
                -- ToDo: this means we fall over immediately if any type sig
                -- is wrong, which is over-conservative, see Trac bug #745

                -- Extend the envt right away with all 
                -- the Ids declared with type signatures
        ; (binds', thing) <- tcExtendIdEnv poly_ids $
                             tcBindGroups top_lvl sig_fn prag_fn 
                                          binds thing_inside

        ; return (ValBindsOut binds' sigs, thing) }

------------------------
tcBindGroups :: TopLevelFlag -> SigFun -> PragFun
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
  = do  { (group', (groups', thing))
                <- tc_group top_lvl sig_fn prag_fn group $ 
                   tcBindGroups top_lvl sig_fn prag_fn groups thing_inside
        ; return (group' ++ groups', thing) }

------------------------
tc_group :: forall thing. 
            TopLevelFlag -> SigFun -> PragFun
         -> (RecFlag, LHsBinds Name) -> TcM thing
         -> TcM ([(RecFlag, LHsBinds TcId)], thing)

-- Typecheck one strongly-connected component of the original program.
-- We get a list of groups back, because there may 
-- be specialisations etc as well

tc_group top_lvl sig_fn prag_fn (NonRecursive, binds) thing_inside
        -- A single non-recursive binding
        -- We want to keep non-recursive things non-recursive
        -- so that we desugar unlifted bindings correctly
 =  do { (binds1, ids) <- tcPolyBinds top_lvl sig_fn prag_fn NonRecursive NonRecursive
                                      (bagToList binds)
       ; thing <- tcExtendIdEnv ids thing_inside
       ; return ( [(NonRecursive, binds1)], thing) }

tc_group top_lvl sig_fn prag_fn (Recursive, binds) thing_inside
  =     -- To maximise polymorphism (assumes -XRelaxedPolyRec), we do a new 
        -- strongly-connected-component analysis, this time omitting 
        -- any references to variables with type signatures.
    do  { traceTc "tc_group rec" (pprLHsBinds binds)
        ; (binds1, _ids, thing) <- go sccs
    	     -- Here is where we should do bindInstsOfLocalFuns
	     -- if we start having Methods again
        ; return ([(Recursive, binds1)], thing) }
                -- Rec them all together
  where
    sccs :: [SCC (LHsBind Name)]
    sccs = stronglyConnCompFromEdgedVertices (mkEdges sig_fn binds)

    go :: [SCC (LHsBind Name)] -> TcM (LHsBinds TcId, [TcId], thing)
    go (scc:sccs) = do  { (binds1, ids1)        <- tc_scc scc
                        ; (binds2, ids2, thing) <- tcExtendIdEnv ids1 $ go sccs
                        ; return (binds1 `unionBags` binds2, ids1 ++ ids2, thing) }
    go []         = do  { thing <- thing_inside; return (emptyBag, [], thing) }

    tc_scc (AcyclicSCC bind) = tc_sub_group NonRecursive [bind]
    tc_scc (CyclicSCC binds) = tc_sub_group Recursive    binds

    tc_sub_group = tcPolyBinds top_lvl sig_fn prag_fn Recursive


------------------------
{-
bindLocalInsts :: TopLevelFlag
	       -> TcM (LHsBinds TcId, [TcId],    a)
	       -> TcM (LHsBinds TcId, TcEvBinds, a)
bindLocalInsts top_lvl thing_inside
  | isTopLevel top_lvl
  = do { (binds, _, thing) <- thing_inside; return (binds, emptyBag, thing) }
        -- For the top level don't bother with all this bindInstsOfLocalFuns stuff. 
        -- All the top level things are rec'd together anyway, so it's fine to
        -- leave them to the tcSimplifyTop, and quite a bit faster too

  | otherwise   -- Nested case
  = do  { ((binds, ids, thing), lie) <- getConstraints thing_inside
        ; lie_binds <- bindLocalMethods lie ids
        ; return (binds, lie_binds, thing) }
-}

------------------------
mkEdges :: SigFun -> LHsBinds Name
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
tcPolyBinds :: TopLevelFlag -> SigFun -> PragFun
  	    -> RecFlag       -- Whether the group is really recursive
  	    -> RecFlag       -- Whether it's recursive after breaking
  	                     -- dependencies based on type signatures
  	    -> [LHsBind Name]
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

tcPolyBinds top_lvl sig_fn prag_fn rec_group rec_tc bind_list
  = setSrcSpan loc                              $
    recoverM (recoveryCode binder_names sig_fn) $ do 
        -- Set up main recoer; take advantage of any type sigs

    { traceTc "------------------------------------------------" empty
    ; traceTc "Bindings for" (ppr binder_names)

    ; tc_sig_fn <- tcInstSigs sig_fn binder_names

    ; dflags <- getDOpts
    ; let plan = decideGeneralisationPlan dflags top_lvl binder_names bind_list tc_sig_fn
    ; traceTc "Generalisation plan" (ppr plan)
    ; (binds, poly_ids) <- case plan of
         NoGen         -> tcPolyNoGen tc_sig_fn prag_fn rec_group rec_tc bind_list
         InferGen mono -> tcPolyInfer top_lvl mono tc_sig_fn prag_fn rec_group rec_tc bind_list
         CheckGen sig  -> tcPolyCheck sig prag_fn rec_group rec_tc bind_list

	-- Check whether strict bindings are ok
        -- These must be non-recursive etc, and are not generalised
        -- They desugar to a case expression in the end
    ; checkStrictBinds top_lvl rec_group bind_list poly_ids

    ; return (binds, poly_ids) }
  where
    binder_names = collectHsBindListBinders bind_list
    loc = getLoc (head bind_list)
         -- TODO: location a bit awkward, but the mbinds have been
         --       dependency analysed and may no longer be adjacent

tcPolyNoGen 
  :: TcSigFun -> PragFun
  -> RecFlag       -- Whether the group is really recursive
  -> RecFlag       -- Whether it's recursive after breaking
                   -- dependencies based on type signatures
  -> [LHsBind Name]
  -> TcM (LHsBinds TcId, [TcId])
-- No generalisation whatsoever

tcPolyNoGen tc_sig_fn prag_fn rec_group rec_tc bind_list
  = do { (binds', mono_infos) <- tcMonoBinds tc_sig_fn True rec_tc bind_list
       ; mono_ids' <- mapM tc_mono_info mono_infos
       ; return (binds', mono_ids') }
  where
    tc_mono_info (name, _, mono_id)
      = do { mono_ty' <- zonkTcTypeCarefully (idType mono_id)
      	     -- Zonk, mainly to expose unboxed types to checkStrictBinds
           ; let mono_id' = setIdType mono_id mono_ty'
           ; (mono_id'', _specs) <- tcPrags rec_group False False
                                           mono_id' (prag_fn name)
           ; return mono_id'' } 
	   -- NB: tcPrags generates and error message for
	   --     specialisation pragmas for non-overloaded sigs
	   -- So we can safely ignore _specs

------------------
tcPolyCheck :: TcSigInfo -> PragFun
  	    -> RecFlag       -- Whether the group is really recursive
  	    -> RecFlag       -- Whether it's recursive after breaking
  	                     -- dependencies based on type signatures
  	    -> [LHsBind Name]
  	    -> TcM (LHsBinds TcId, [TcId])
-- There is just one binding, 
--   it binds a single variable,
--   it has a signature,
tcPolyCheck sig@(TcSigInfo { sig_id = id, sig_tvs = tvs, sig_scoped = scoped
                           , sig_theta = theta, sig_loc = loc })
    prag_fn rec_group rec_tc bind_list
  = do { ev_vars <- newEvVars theta

       ; let skol_info = SigSkol (FunSigCtxt (idName id))
       ; (ev_binds, (binds', [mono_info])) 
            <- checkConstraints skol_info emptyVarSet tvs ev_vars $
               tcExtendTyVarEnv2 (scoped `zip` mkTyVarTys tvs)    $
               tcMonoBinds (\_ -> Just sig) False rec_tc bind_list

       ; export <- mkExport rec_group False prag_fn tvs theta mono_info

       ; let (_, poly_id, _, _) = export
             abs_bind = L loc $ AbsBinds 
                        { abs_tvs = tvs
                        , abs_ev_vars = ev_vars, abs_ev_binds = ev_binds
                        , abs_exports = [export], abs_binds = binds' }
       ; return (unitBag abs_bind, [poly_id]) }

tcPolyInfer 
  :: TopLevelFlag 
  -> Bool	  -- True <=> apply the monomorphism restriction
  -> TcSigFun -> PragFun
  -> RecFlag       -- Whether the group is really recursive
  -> RecFlag       -- Whether it's recursive after breaking
                   -- dependencies based on type signatures
  -> [LHsBind Name]
  -> TcM (LHsBinds TcId, [TcId])
tcPolyInfer top_lvl mono sig_fn prag_fn rec_group rec_tc bind_list
  = do { ((binds', mono_infos), wanted) 
             <- getConstraints $
                tcMonoBinds sig_fn False rec_tc bind_list

       ; unifyCtxts [sig | (_, Just sig, _) <- mono_infos] 

       ; let get_tvs | isTopLevel top_lvl = tyVarsOfType  
                     | otherwise          = exactTyVarsOfType
		     -- See Note [Silly type synonym] in TcType
             tau_tvs = foldr (unionVarSet . get_tvs . getMonoType) emptyVarSet mono_infos

       ; (qtvs, givens, ev_binds) <- simplifyInfer mono tau_tvs wanted

       ; exports <- mapM (mkExport rec_group (length mono_infos > 1)
                                   prag_fn qtvs (map evVarPred givens))
                    mono_infos

       ; let poly_ids = [poly_id | (_, poly_id, _, _) <- exports]
       ; traceTc "Binding:" (ppr (poly_ids `zip` map idType poly_ids))

       ; loc <- getSrcSpanM
       ; let abs_bind = L loc $ AbsBinds { abs_tvs = qtvs
                                         , abs_ev_vars = givens, abs_ev_binds = ev_binds
                                         , abs_exports = exports, abs_binds = binds' }

       ; return (unitBag abs_bind, poly_ids)   -- poly_ids are guaranteed zonked by mkExport
  }


--------------
mkExport :: RecFlag
	 -> Bool	 -- More than one variable is bound, so we'll desugar to
	    		 -- a tuple, so INLINE pragmas won't work
         -> PragFun -> [TyVar] -> TcThetaType
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

mkExport rec_group multi_bind prag_fn inferred_tvs theta
         (poly_name, mb_sig, mono_id)
  = do  { (tvs, poly_id) <- mk_poly_id mb_sig
                -- poly_id has a zonked type

        ; (poly_id', spec_prags) <- tcPrags rec_group multi_bind (notNull theta)
                                        poly_id (prag_fn poly_name)
                -- tcPrags requires a zonked poly_id

        ; return (tvs, poly_id', mono_id, SpecPrags spec_prags) }
  where
    poly_ty = mkSigmaTy inferred_tvs theta (idType mono_id)

    mk_poly_id Nothing    = do { poly_ty' <- zonkTcTypeCarefully poly_ty
                               ; return (inferred_tvs, mkLocalId poly_name poly_ty') }
    mk_poly_id (Just sig) = do { tvs <- mapM zonk_tv (sig_tvs sig)
                               ; return (tvs,  sig_id sig) }

    zonk_tv tv = do { ty <- zonkTcTyVar tv; return (tcGetTyVar "mkExport" ty) }

------------------------
type PragFun = Name -> [LSig Name]

mkPragFun :: [LSig Name] -> LHsBinds Name -> PragFun
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
    do  { let name     = idName poly_id
              sig_ctxt = FunSigCtxt name
        ; spec_ty <- tcHsSigType sig_ctxt hs_ty
        ; wrap <- tcSubType (SpecPragOrigin name) (SigSkol sig_ctxt)
                            (idType poly_id) spec_ty
        ; return (SpecPrag wrap inl) }
  where
    spec_ctxt prag = hang (ptext (sLit "In the SPECIALISE pragma")) 2 (ppr prag)
tcSpecPrag _ sig = pprPanic "tcSpecPrag" (ppr sig)

--------------
-- If typechecking the binds fails, then return with each
-- signature-less binder given type (forall a.a), to minimise 
-- subsequent error messages
recoveryCode :: [Name] -> SigFun -> TcM (LHsBinds TcId, [Id])
recoveryCode binder_names sig_fn
  = do  { traceTc "tcBindsWithSigs: error recovery" (ppr binder_names)
        ; poly_ids <- mapM mk_dummy binder_names
        ; return (emptyBag, poly_ids) }
  where
    mk_dummy name 
        | isJust (sig_fn name) = tcLookupId name        -- Had signature; look it up
        | otherwise            = return (mkLocalId name forall_a_a)    -- No signature

forall_a_a :: TcType
forall_a_a = mkForAllTy openAlphaTyVar (mkTyVarTy openAlphaTyVar)
\end{code}


%************************************************************************
%*                                                                      *
\subsection{tcMonoBind}
%*                                                                      *
%************************************************************************

@tcMonoBinds@ deals with a perhaps-recursive group of HsBinds.
The signatures have been dealt with already.

\begin{code}
tcMonoBinds :: TcSigFun
            -> Bool	-- True <=> no generalisation will be done for this binding
            -> RecFlag  -- Whether the binding is recursive for typechecking purposes
                        -- i.e. the binders are mentioned in their RHSs, and
                        --      we are not resuced by a type signature
            -> [LHsBind Name]
            -> TcM (LHsBinds TcId, [MonoBindInfo])

tcMonoBinds sig_fn no_gen is_rec
           [ L b_loc (FunBind { fun_id = L nm_loc name, fun_infix = inf, 
                                fun_matches = matches, bind_fvs = fvs })]
                             -- Single function binding, 
  | NonRecursive <- is_rec   -- ...binder isn't mentioned in RHS
  , Nothing <- sig_fn name   -- ...with no type signature
  =     -- In this very special case we infer the type of the
        -- right hand side first (it may have a higher-rank type)
        -- and *then* make the monomorphic Id for the LHS
        -- e.g.         f = \(x::forall a. a->a) -> <body>
        --      We want to infer a higher-rank type for f
    setSrcSpan b_loc    $
    do  { ((co_fn, matches'), rhs_ty) <- tcInfer (tcMatchesFun name inf matches)

        ; mono_id <- newLetBndr no_gen name rhs_ty
        ; return (unitBag (L b_loc (FunBind { fun_id = L nm_loc mono_id, fun_infix = inf,
                                              fun_matches = matches', bind_fvs = fvs,
                                              fun_co_fn = co_fn, fun_tick = Nothing })),
                  [(name, Nothing, mono_id)]) }

tcMonoBinds sig_fn no_gen _ binds
  = do  { tc_binds <- mapM (wrapLocM (tcLhs sig_fn no_gen)) binds

        -- Bring the monomorphic Ids, into scope for the RHSs
        ; let mono_info  = getMonoBindInfo tc_binds
              rhs_id_env = [(name,mono_id) | (name, Nothing, mono_id) <- mono_info]
                    -- A monomorphic binding for each term variable that lacks 
                    -- a type sig.  (Ones with a sig are already in scope.)

        ; binds' <- tcExtendIdEnv2 rhs_id_env $ do
                    traceTc "tcMonoBinds" $  vcat [ ppr n <+> ppr id <+> ppr (idType id) 
                                                  | (n,id) <- rhs_id_env]
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

getMonoType :: MonoBindInfo -> TcTauType
getMonoType (_,_,mono_id) = idType mono_id

tcLhs :: TcSigFun -> Bool -> HsBind Name -> TcM TcMonoBind
tcLhs sig_fn no_gen (FunBind { fun_id = L nm_loc name, fun_infix = inf, fun_matches = matches })
  = do  { mono_id <- newLhsBndr mb_sig no_gen name
        ; return (TcFunBind (name, mb_sig, mono_id) (L nm_loc mono_id) inf matches) }
  where
    mb_sig = sig_fn name 

tcLhs sig_fn no_gen (PatBind { pat_lhs = pat, pat_rhs = grhss })
  = do  { let tc_pat exp_ty = tcLetPat sig_fn no_gen pat exp_ty $
                              mapM lookup_info (collectPatBinders pat)

                -- After typechecking the pattern, look up the binder
                -- names, which the pattern has brought into scope.
              lookup_info :: Name -> TcM MonoBindInfo
              lookup_info name = do { mono_id <- tcLookupId name
                                    ; return (name, sig_fn name, mono_id) }

        ; ((pat', infos), pat_ty) <- addErrCtxt (patMonoBindsCtxt pat grhss) $
                                     tcInfer tc_pat

        ; return (TcPatBind infos pat' grhss pat_ty) }

tcLhs _ _ other_bind = pprPanic "tcLhs" (ppr other_bind)
        -- AbsBind, VarBind impossible

-----------------
newLhsBndr :: Maybe TcSigInfo -> Bool -> Name -> TcM TcId
-- cf TcPat.tcPatBndr (LetPat case)
newLhsBndr (Just sig) no_gen name
  | no_gen    = return (sig_id sig)
  | otherwise = do { mono_name <- newLocalName name
                   ; return (mkLocalId mono_name (sig_tau sig)) }

newLhsBndr Nothing no_gen name
  = do { mono_ty <- newFlexiTyVarTy argTypeKind
       ; newLetBndr no_gen name mono_ty }

-------------------
tcRhs :: TcMonoBind -> TcM (HsBind TcId)
-- When we are doing pattern bindings, or multiple function bindings at a time
-- we *don't* bring any scoped type variables into scope
-- Wny not?  They are not completely rigid.
-- That's why we have the special case for a single FunBind in tcMonoBinds
tcRhs (TcFunBind (_,_,mono_id) fun' inf matches)
  = do  { (co_fn, matches') <- tcMatchesFun (idName mono_id) inf 
                                            matches (idType mono_id)
        ; return (FunBind { fun_id = fun', fun_infix = inf, fun_matches = matches'
                          , fun_co_fn = co_fn 
                          , bind_fvs = placeHolderNames, fun_tick = Nothing }) }

tcRhs (TcPatBind _ pat' grhss pat_ty)
  = do  { grhss' <- addErrCtxt (patMonoBindsCtxt pat' grhss) $
                    tcGRHSsPat grhss pat_ty
        ; return (PatBind { pat_lhs = pat', pat_rhs = grhss', pat_rhs_ty = pat_ty 
                          , bind_fvs = placeHolderNames }) }


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
unifyCtxts :: [TcSigInfo] -> TcM ()
-- Post-condition: the returned Insts are full zonked
unifyCtxts [] = return ()
unifyCtxts (sig1 : sigs)
  = do  { traceTc "unifyCtxts" (ppr (sig1 : sigs))
	; mapM_ unify_ctxt sigs }
  where
    theta1 = sig_theta sig1
    unify_ctxt :: TcSigInfo -> TcM ()
    unify_ctxt sig@(TcSigInfo { sig_theta = theta })
        = setSrcSpan (sig_loc sig)                      $
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

Note [Signature skolems]
~~~~~~~~~~~~~~~~~~~~~~~~
When instantiating a type signature, we do so with either skolems or
SigTv meta-type variables depending on the use_skols boolean.  This
variable is set True when we are typechecking a single function
binding; and False for pattern bindings and a group of several
function bindings.

Reason: in the latter cases, the "skolems" can be unified together, 
        so they aren't properly rigid in the type-refinement sense.
NB: unless we are doing H98, each function with a sig will be done
    separately, even if it's mutually recursive, so use_skols will be True


Note [Only scoped tyvars are in the TyVarEnv]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We are careful to keep only the *lexically scoped* type variables in
the type environment.  Why?  After all, the renamer has ensured
that only legal occurrences occur, so we could put all type variables
into the type env.

But we want to check that two distinct lexically scoped type variables
do not map to the same internal type variable.  So we need to know which
the lexically-scoped ones are... and at the moment we do that by putting
only the lexically scoped ones into the environment.

Note [Instantiate sig with fresh variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's vital to instantiate a type signature with fresh variables.
For example:
      type T = forall a. [a] -> [a]
      f :: T; 
      f = g where { g :: T; g = <rhs> }

 We must not use the same 'a' from the defn of T at both places!!
(Instantiation is only necessary because of type synonyms.  Otherwise,
it's all cool; each signature has distinct type variables from the renamer.)

\begin{code}
type SigFun = Name -> Maybe ([Name], SrcSpan)
         -- Maps a let-binder to the list of
         -- type variables brought into scope
         -- by its type signature, plus location
         -- Nothing => no type signature

mkSigFun :: [LSig Name] -> SigFun
-- Search for a particular type signature
-- Precondition: the sigs are all type sigs
-- Precondition: no duplicates
mkSigFun sigs = lookupNameEnv env
  where
    env = mkNameEnv (mapCatMaybes mk_pair sigs)
    mk_pair (L loc (TypeSig (L _ name) lhs_ty)) = Just (name, (hsExplicitTvs lhs_ty, loc))
    mk_pair (L loc (IdSig id))                  = Just (idName id, ([], loc))
    mk_pair _                                   = Nothing    
        -- The scoped names are the ones explicitly mentioned
        -- in the HsForAll.  (There may be more in sigma_ty, because
        -- of nested type synonyms.  See Note [More instantiated than scoped].)
        -- See Note [Only scoped tyvars are in the TyVarEnv]
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
tcInstSigs :: SigFun -> [Name] -> TcM TcSigFun
tcInstSigs sig_fn bndrs
  = do { prs <- mapMaybeM (tcInstSig sig_fn use_skols) bndrs
       ; return (lookupNameEnv (mkNameEnv prs)) }
  where
    use_skols = isSingleton bndrs	-- See Note [Signature skolems]

tcInstSig :: SigFun -> Bool -> Name -> TcM (Maybe (Name, TcSigInfo))
-- For use_skols :: Bool see Note [Signature skolems]
--
-- We must instantiate with fresh uniques, 
-- (see Note [Instantiate sig with fresh variables])
-- although we keep the same print-name.

tcInstSig sig_fn use_skols name
  | Just (scoped_tvs, loc) <- sig_fn name
  = do  { poly_id <- tcLookupId name    -- Cannot fail; the poly ids are put into 
                                        -- scope when starting the binding group
        ; (tvs, theta, tau) <- tcInstSigType use_skols name (idType poly_id)
        ; let sig = TcSigInfo { sig_id = poly_id
	  	 	      , sig_scoped = scoped_tvs
                              , sig_tvs = tvs, sig_theta = theta, sig_tau = tau
                              , sig_loc = loc }
        ; return (Just (name, sig)) } 
  | otherwise
  = return Nothing

-------------------------------
data GeneralisationPlan 
  = NoGen		-- No generalisation, no AbsBinds
  | InferGen Bool	-- Implicit generalisation; there is an AbsBinds
    	     		--   True <=> apply the MR; generalise only unconstrained type vars
  | CheckGen TcSigInfo	-- Explicit generalisation; there is an AbsBinds

-- A consequence of the no-AbsBinds choice (NoGen) is that there is
-- no "polymorphic Id" and "monmomorphic Id"; there is just the one

instance Outputable GeneralisationPlan where
  ppr NoGen        = ptext (sLit "NoGen")
  ppr (InferGen b) = ptext (sLit "InferGen") <+> ppr b
  ppr (CheckGen s) = ptext (sLit "CheckGen") <+> ppr s

decideGeneralisationPlan 
   :: DynFlags -> TopLevelFlag -> [Name] -> [LHsBind Name] -> TcSigFun -> GeneralisationPlan
decideGeneralisationPlan dflags top_lvl _bndrs binds sig_fn
  | mono_pat_binds                         = NoGen
  | Just sig <- one_funbind_with_sig binds = if null (sig_tvs sig) && null (sig_theta sig)
                                             then NoGen	      -- Optimise common case
                                             else CheckGen sig
  | (xopt Opt_MonoLocalBinds dflags 
      && isNotTopLevel top_lvl)      	   = NoGen
  | otherwise                              = InferGen mono_restriction

--  | all no_sig bndrs    	     	   = InferGen mono_restriction
--  | otherwise            	     	   = NoGen   -- A mixture of function 
--    				       		     -- and pattern bindings
  where
    mono_pat_binds = xopt Opt_MonoPatBinds dflags 
                  && any (is_pat_bind . unLoc) binds

    mono_restriction = xopt Opt_MonomorphismRestriction dflags 
                    && any (restricted . unLoc) binds

    no_sig n = isNothing (sig_fn n)

    -- With OutsideIn, all nested bindings are monomorphic
    -- except a single function binding with a signature
    one_funbind_with_sig [L _ FunBind { fun_id = v }] = sig_fn (unLoc v)
    one_funbind_with_sig _                            = Nothing

    -- The Haskell 98 monomorphism resetriction
    restricted (PatBind {})                              = True
    restricted (VarBind { var_id = v })                  = no_sig v
    restricted (FunBind { fun_id = v, fun_matches = m }) = restricted_match m
                                                           && no_sig (unLoc v)
    restricted (AbsBinds {}) = panic "isRestrictedGroup/unrestricted AbsBinds"

    restricted_match (MatchGroup (L _ (Match [] _ _) : _) _) = True
    restricted_match _                                       = False
        -- No args => like a pattern binding
        -- Some args => a function binding

    is_pat_bind (PatBind {}) = True
    is_pat_bind _            = False

-------------------
checkStrictBinds :: TopLevelFlag -> RecFlag
                 -> [LHsBind Name] -> [Id]
                 -> TcM ()
-- Check that non-overloaded unlifted bindings are
--      a) non-recursive,
--      b) not top level, 
--      c) not a multiple-binding group (more or less implied by (a))

checkStrictBinds top_lvl rec_group binds poly_ids
  | unlifted || bang_pat
  = do  { checkTc (isNotTopLevel top_lvl)
                  (strictBindErr "Top-level" unlifted binds)
        ; checkTc (isNonRec rec_group)
                  (strictBindErr "Recursive" unlifted binds)
        ; checkTc (isSingleton binds)
                  (strictBindErr "Multiple" unlifted binds) 
        -- This should be a checkTc, not a warnTc, but as of GHC 6.11
        -- the versions of alex and happy available have non-conforming
        -- templates, so the GHC build fails if it's an error:
        ; warnUnlifted <- doptM Opt_WarnLazyUnliftedBindings
        ; warnTc (warnUnlifted && not bang_pat)
                 (unliftedMustBeBang binds) }
  | otherwise
  = return ()
  where
    unlifted = any is_unlifted poly_ids
    bang_pat = any (isBangHsBind . unLoc) binds
    is_unlifted id = case tcSplitForAllTys (idType id) of
                       (_, rho) -> isUnLiftedType rho

unliftedMustBeBang :: [LHsBind Name] -> SDoc
unliftedMustBeBang binds
  = hang (text "Bindings containing unlifted types should use an outermost bang pattern:")
       2 (pprBindList binds)

strictBindErr :: String -> Bool -> [LHsBind Name] -> SDoc
strictBindErr flavour unlifted binds
  = hang (text flavour <+> msg <+> ptext (sLit "aren't allowed:")) 
       2 (pprBindList binds)
  where
    msg | unlifted  = ptext (sLit "bindings for unlifted types")
        | otherwise = ptext (sLit "bang-pattern bindings")

pprBindList :: [LHsBind Name] -> SDoc
pprBindList binds = vcat (map ppr binds)
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
  = hang (ptext (sLit "In a pattern binding:")) 2 (pprPatBind pat grhss)

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
\end{code}
