%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcBinds]{TcBinds}

\begin{code}
module TcBinds ( tcLocalBinds, tcTopBinds, tcRecSelBinds,
                 tcHsBootSigs, tcPolyCheck,
                 PragFun, tcSpecPrags, tcVectDecls, mkPragFun, 
                 TcSigInfo(..), TcSigFun, 
                 instTcTySig, instTcTySigFromId,
                 badBootDeclErr ) where

import {-# SOURCE #-} TcMatches ( tcGRHSsPat, tcMatchesFun )
import {-# SOURCE #-} TcExpr  ( tcMonoExpr )

import DynFlags
import HsSyn
import HscTypes( isHsBoot )
import TcRnMonad
import TcEnv
import TcUnify
import TcSimplify
import TcEvidence
import TcHsType
import TcPat
import TcMType
import Type( tidyOpenType )
import FunDeps( growThetaTyVars )
import TyCon
import TcType
import TysPrim
import Id
import Var
import VarSet
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
import FastString
import Type(mkStrLitTy)
import Class(classTyCon)
import PrelNames(ipClassName)

import Control.Monad

#include "HsVersions.h"
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

\begin{code}
tcTopBinds :: HsValBinds Name -> TcM (TcGblEnv, TcLclEnv)
-- The TcGblEnv contains the new tcg_binds and tcg_spects
-- The TcLclEnv has an extended type envt for the new bindings
tcTopBinds (ValBindsOut binds sigs)
  = do  { tcg_env <- getGblEnv
        ; (binds', tcl_env) <- tcValBinds TopLevel binds sigs getLclEnv
        ; specs <- tcImpPrags sigs   -- SPECIALISE prags for imported Ids

        ; let { tcg_env' = tcg_env { tcg_binds = foldr (unionBags . snd)
                                                       (tcg_binds tcg_env)
                                                       binds'
                                   , tcg_imp_specs = specs ++ tcg_imp_specs tcg_env } }

        ; return (tcg_env', tcl_env) }
        -- The top level bindings are flattened into a giant 
        -- implicitly-mutually-recursive LHsBinds
tcTopBinds (ValBindsIn {}) = panic "tcTopBinds"

tcRecSelBinds :: HsValBinds Name -> TcM TcGblEnv
tcRecSelBinds (ValBindsOut binds sigs)
  = tcExtendGlobalValEnv [sel_id | L _ (IdSig sel_id) <- sigs] $
    do { (rec_sel_binds, tcg_env) <- discardWarnings (tcValBinds TopLevel binds sigs getGblEnv)
       ; let tcg_env' 
              | isHsBoot (tcg_src tcg_env) = tcg_env
              | otherwise = tcg_env { tcg_binds = foldr (unionBags . snd)
                                                        (tcg_binds tcg_env)
                                                        rec_sel_binds }
              -- Do not add the code for record-selector bindings when 
              -- compiling hs-boot files
       ; return tcg_env' }
tcRecSelBinds (ValBindsIn {}) = panic "tcRecSelBinds"

tcHsBootSigs :: HsValBinds Name -> TcM [Id]
-- A hs-boot file has only one BindGroup, and it only has type
-- signatures in it.  The renamer checked all this
tcHsBootSigs (ValBindsOut binds sigs)
  = do  { checkTc (null binds) badBootDeclErr
        ; concat <$> mapM (addLocM tc_boot_sig) (filter isTypeLSig sigs) }
  where
    tc_boot_sig (TypeSig lnames ty) = mapM f lnames
      where
        f (L _ name) = do  { sigma_ty <- tcHsSigType (FunSigCtxt name) ty
                           ; return (mkVanillaGlobal name sigma_ty) }
        -- Notice that we make GlobalIds, not LocalIds
    tc_boot_sig s = pprPanic "tcHsBootSigs/tc_boot_sig" (ppr s)
tcHsBootSigs groups = pprPanic "tcHsBootSigs" (ppr groups)

badBootDeclErr :: MsgDoc
badBootDeclErr = ptext (sLit "Illegal declarations in an hs-boot file")

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
    ips = [ip | L _ (IPBind (Left ip) _) <- ip_binds]

        -- I wonder if we should do these one at at time
        -- Consider     ?x = 4
        --              ?y = ?x + 1
    tc_ip_bind ipClass (IPBind (Left ip) expr)
       = do { ty <- newFlexiTyVarTy openTypeKind
            ; let p = mkStrLitTy $ hsIPNameFS ip
            ; ip_id <- newDict ipClass [ p, ty ]
            ; expr' <- tcMonoExpr expr ty
            ; let d = toDict ipClass p ty `fmap` expr'
            ; return (ip_id, (IPBind (Right ip_id) d)) }
    tc_ip_bind _ (IPBind (Right {}) _) = panic "tc_ip_bind"

    -- Coerces a `t` into a dictionry for `IP "x" t`.
    -- co : t -> IP "x" t
    toDict ipClass x ty =
      case unwrapNewTyCon_maybe (classTyCon ipClass) of
        Just (_,_,ax) -> HsWrap $ WpCast $ mkTcSymCo $ mkTcUnbranchedAxInstCo ax [x,ty]
        Nothing       -> panic "The dictionary for `IP` is not a newtype?"


\end{code}

Note [Implicit parameter untouchables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

\begin{code}
tcValBinds :: TopLevelFlag 
           -> [(RecFlag, LHsBinds Name)] -> [LSig Name]
           -> TcM thing
           -> TcM ([(RecFlag, LHsBinds TcId)], thing) 

tcValBinds top_lvl binds sigs thing_inside
  = do  {       -- Typecheck the signature
          (poly_ids, sig_fn) <- tcTySigs sigs

        ; let prag_fn = mkPragFun sigs (foldr (unionBags . snd) emptyBag binds)

                -- Extend the envt right away with all 
                -- the Ids declared with type signatures
                -- Use tcExtendIdEnv2 to avoid extending the TcIdBinder stack
        ; (binds', thing) <- tcExtendIdEnv2 [(idName id, id) | id <- poly_ids] $
                             tcBindGroups top_lvl sig_fn prag_fn 
                                          binds thing_inside

        ; return (binds', thing) }

------------------------
tcBindGroups :: TopLevelFlag -> TcSigFun -> PragFun
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
            TopLevelFlag -> TcSigFun -> PragFun
         -> (RecFlag, LHsBinds Name) -> TcM thing
         -> TcM ([(RecFlag, LHsBinds TcId)], thing)

-- Typecheck one strongly-connected component of the original program.
-- We get a list of groups back, because there may 
-- be specialisations etc as well

tc_group top_lvl sig_fn prag_fn (NonRecursive, binds) thing_inside
        -- A single non-recursive binding
        -- We want to keep non-recursive things non-recursive
        -- so that we desugar unlifted bindings correctly
 =  do { (binds1, ids, closed) <- tcPolyBinds top_lvl sig_fn prag_fn 
                                              NonRecursive NonRecursive
                                             (bagToList binds)
       ; thing <- tcExtendLetEnv top_lvl closed ids thing_inside
       ; return ( [(NonRecursive, binds1)], thing) }

tc_group top_lvl sig_fn prag_fn (Recursive, binds) thing_inside
  =     -- To maximise polymorphism, we do a new 
        -- strongly-connected-component analysis, this time omitting 
        -- any references to variables with type signatures.
        -- (This used to be optional, but isn't now.)
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
    go (scc:sccs) = do  { (binds1, ids1, closed) <- tc_scc scc
                        ; (binds2, ids2, thing)  <- tcExtendLetEnv top_lvl closed ids1 $ 
                                                    go sccs
                        ; return (binds1 `unionBags` binds2, ids1 ++ ids2, thing) }
    go []         = do  { thing <- thing_inside; return (emptyBag, [], thing) }

    tc_scc (AcyclicSCC bind) = tc_sub_group NonRecursive [bind]
    tc_scc (CyclicSCC binds) = tc_sub_group Recursive    binds

    tc_sub_group = tcPolyBinds top_lvl sig_fn prag_fn Recursive

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
tcPolyBinds :: TopLevelFlag -> TcSigFun -> PragFun
            -> RecFlag       -- Whether the group is really recursive
            -> RecFlag       -- Whether it's recursive after breaking
                             -- dependencies based on type signatures
            -> [LHsBind Name]
            -> TcM (LHsBinds TcId, [TcId], TopLevelFlag)

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
        -- Set up main recover; take advantage of any type sigs

    { traceTc "------------------------------------------------" empty
    ; traceTc "Bindings for {" (ppr binder_names)
    ; dflags   <- getDynFlags
    ; type_env <- getLclTypeEnv
    ; let plan = decideGeneralisationPlan dflags type_env 
                         binder_names bind_list sig_fn
    ; traceTc "Generalisation plan" (ppr plan)
    ; result@(tc_binds, poly_ids, _) <- case plan of
         NoGen          -> tcPolyNoGen rec_tc prag_fn sig_fn bind_list 
         InferGen mn cl -> tcPolyInfer rec_tc prag_fn sig_fn mn cl bind_list
         CheckGen sig   -> tcPolyCheck rec_tc prag_fn sig bind_list

        -- Check whether strict bindings are ok
        -- These must be non-recursive etc, and are not generalised
        -- They desugar to a case expression in the end
    ; checkStrictBinds top_lvl rec_group bind_list tc_binds poly_ids
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

------------------
tcPolyNoGen     -- No generalisation whatsoever
  :: RecFlag       -- Whether it's recursive after breaking
                   -- dependencies based on type signatures
  -> PragFun -> TcSigFun
  -> [LHsBind Name]
  -> TcM (LHsBinds TcId, [TcId], TopLevelFlag)

tcPolyNoGen rec_tc prag_fn tc_sig_fn bind_list
  = do { (binds', mono_infos) <- tcMonoBinds rec_tc tc_sig_fn
                                             (LetGblBndr prag_fn) 
                                             bind_list
       ; mono_ids' <- mapM tc_mono_info mono_infos
       ; return (binds', mono_ids', NotTopLevel) }
  where
    tc_mono_info (name, _, mono_id)
      = do { mono_ty' <- zonkTcType (idType mono_id)
             -- Zonk, mainly to expose unboxed types to checkStrictBinds
           ; let mono_id' = setIdType mono_id mono_ty'
           ; _specs <- tcSpecPrags mono_id' (prag_fn name)
           ; return mono_id' }
           -- NB: tcPrags generates error messages for
           --     specialisation pragmas for non-overloaded sigs
           -- Indeed that is why we call it here!
           -- So we can safely ignore _specs

------------------
tcPolyCheck :: RecFlag       -- Whether it's recursive after breaking
                             -- dependencies based on type signatures
            -> PragFun -> TcSigInfo 
            -> [LHsBind Name]
            -> TcM (LHsBinds TcId, [TcId], TopLevelFlag)
-- There is just one binding, 
--   it binds a single variable,
--   it has a signature,
tcPolyCheck rec_tc prag_fn
            sig@(TcSigInfo { sig_id = poly_id, sig_tvs = tvs_w_scoped 
                           , sig_theta = theta, sig_tau = tau, sig_loc = loc })
            bind_list
  = do { ev_vars <- newEvVars theta
       ; let skol_info = SigSkol (FunSigCtxt (idName poly_id)) (mkPhiTy theta tau)
             prag_sigs = prag_fn (idName poly_id)
       ; tvs <- mapM (skolemiseSigTv . snd) tvs_w_scoped
       ; (ev_binds, (binds', [mono_info])) 
            <- setSrcSpan loc $  
               checkConstraints skol_info tvs ev_vars $
               tcExtendTyVarEnv2 [(n,tv) | (Just n, tv) <- tvs_w_scoped] $
               tcMonoBinds rec_tc (\_ -> Just sig) LetLclBndr bind_list

       ; spec_prags <- tcSpecPrags poly_id prag_sigs
       ; poly_id    <- addInlinePrags poly_id prag_sigs

       ; let (_, _, mono_id) = mono_info
             export = ABE { abe_wrap = idHsWrapper
                          , abe_poly = poly_id
                          , abe_mono = mono_id
                          , abe_prags = SpecPrags spec_prags }
             abs_bind = L loc $ AbsBinds 
                        { abs_tvs = tvs
                        , abs_ev_vars = ev_vars, abs_ev_binds = ev_binds
                        , abs_exports = [export], abs_binds = binds' }
             closed | isEmptyVarSet (tyVarsOfType (idType poly_id)) = TopLevel
                    | otherwise                                     = NotTopLevel
       ; return (unitBag abs_bind, [poly_id], closed) }

------------------
tcPolyInfer 
  :: RecFlag       -- Whether it's recursive after breaking
                   -- dependencies based on type signatures
  -> PragFun -> TcSigFun 
  -> Bool         -- True <=> apply the monomorphism restriction
  -> Bool         -- True <=> free vars have closed types
  -> [LHsBind Name]
  -> TcM (LHsBinds TcId, [TcId], TopLevelFlag)
tcPolyInfer rec_tc prag_fn tc_sig_fn mono closed bind_list
  = do { ((binds', mono_infos), wanted)
             <- captureConstraints $
                tcMonoBinds rec_tc tc_sig_fn LetLclBndr bind_list

       ; let name_taus = [(name, idType mono_id) | (name, _, mono_id) <- mono_infos]
       ; traceTc "simplifyInfer call" (ppr name_taus $$ ppr wanted)
       ; (qtvs, givens, mr_bites, ev_binds) <- 
                          simplifyInfer closed mono name_taus wanted

       ; theta <- zonkTcThetaType (map evVarPred givens)
       ; exports <- checkNoErrs $ mapM (mkExport prag_fn qtvs theta) mono_infos

       ; loc <- getSrcSpanM
       ; let poly_ids = map abe_poly exports
             final_closed | closed && not mr_bites = TopLevel
                          | otherwise              = NotTopLevel
             abs_bind = L loc $ 
                        AbsBinds { abs_tvs = qtvs
                                 , abs_ev_vars = givens, abs_ev_binds = ev_binds
                                 , abs_exports = exports, abs_binds = binds' }

       ; traceTc "Binding:" (ppr final_closed $$
                             ppr (poly_ids `zip` map idType poly_ids))
       ; return (unitBag abs_bind, poly_ids, final_closed) }
         -- poly_ids are guaranteed zonked by mkExport

--------------
mkExport :: PragFun 
         -> [TyVar] -> TcThetaType      -- Both already zonked
         -> MonoBindInfo
         -> TcM (ABExport Id)
-- mkExport generates exports with 
--      zonked type variables, 
--      zonked poly_ids
-- The former is just because no further unifications will change
-- the quantified type variables, so we can fix their final form
-- right now.
-- The latter is needed because the poly_ids are used to extend the
-- type environment; see the invariant on TcEnv.tcExtendIdEnv 

-- Pre-condition: the qtvs and theta are already zonked

mkExport prag_fn qtvs theta (poly_name, mb_sig, mono_id)
  = do  { mono_ty <- zonkTcType (idType mono_id)
        ; let poly_id  = case mb_sig of
                           Nothing  -> mkLocalId poly_name inferred_poly_ty
                           Just sig -> sig_id sig
                -- poly_id has a zonked type

              -- In the inference case (no signature) this stuff figures out
              -- the right type variables and theta to quantify over
              -- See Note [Impedence matching]
              my_tvs2 = closeOverKinds (growThetaTyVars theta (tyVarsOfType mono_ty))
                            -- Include kind variables!  Trac #7916
              my_tvs   = filter (`elemVarSet` my_tvs2) qtvs   -- Maintain original order
              my_theta = filter (quantifyPred my_tvs2) theta
              inferred_poly_ty = mkSigmaTy my_tvs my_theta mono_ty

        ; poly_id <- addInlinePrags poly_id prag_sigs
        ; spec_prags <- tcSpecPrags poly_id prag_sigs
                -- tcPrags requires a zonked poly_id

        ; let sel_poly_ty = mkSigmaTy qtvs theta mono_ty
        ; traceTc "mkExport: check sig" 
                  (ppr poly_name $$ ppr sel_poly_ty $$ ppr (idType poly_id)) 

        -- Perform the impedence-matching and ambiguity check
        -- right away.  If it fails, we want to fail now (and recover
        -- in tcPolyBinds).  If we delay checking, we get an error cascade.
        -- Remember we are in the tcPolyInfer case, so the type envt is 
        -- closed (unless we are doing NoMonoLocalBinds in which case all bets
        -- are off)
        -- See Note [Impedence matching]
        ; (wrap, wanted) <- addErrCtxtM (mk_msg poly_id) $
                            captureConstraints $
                            tcSubType origin sig_ctxt sel_poly_ty (idType poly_id)
        ; ev_binds <- simplifyAmbiguityCheck poly_name wanted

        ; return (ABE { abe_wrap = mkWpLet (EvBinds ev_binds) <.> wrap
                      , abe_poly = poly_id
                      , abe_mono = mono_id
                      , abe_prags = SpecPrags spec_prags }) }
  where
    inferred = isNothing mb_sig

    mk_msg poly_id tidy_env
      = return (tidy_env', msg)
      where
        msg | inferred  = hang (ptext (sLit "When checking that") <+> pp_name)
                             2 (ptext (sLit "has the inferred type") <+> pp_ty)
                          $$ ptext (sLit "Probable cause: the inferred type is ambiguous")
            | otherwise = hang (ptext (sLit "When checking that") <+> pp_name)
                             2 (ptext (sLit "has the specified type") <+> pp_ty)
        pp_name = quotes (ppr poly_name)
        pp_ty   = quotes (ppr tidy_ty)
        (tidy_env', tidy_ty) = tidyOpenType tidy_env (idType poly_id)
        

    prag_sigs = prag_fn poly_name
    origin    = AmbigOrigin sig_ctxt
    sig_ctxt  = InfSigCtxt poly_name
\end{code}

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

We can get these by "impedence matching":
   tuple :: forall a b. (Eq a, Num a) => (a -> Bool -> Bool, [b] -> Bool -> Bool)
   tuple a b d1 d1 = let ...bind f_mono, g_mono in (f_mono, g_mono)

   f a d1 d2 = case tuple a Any d1 d2 of (f, g) -> f
   g b = case tuple Integer b dEqInteger dNumInteger of (f,g) -> g

Suppose the shared quantified tyvars are qtvs and constraints theta.
Then we want to check that 
   f's polytype  is more polymorphic than   forall qtvs. theta => f_mono_ty
and the proof is the impedence matcher.  

Notice that the impedence matcher may do defaulting.  See Trac #7173.

It also cleverly does an ambiguity check; for example, rejecting
   f :: F a -> a
where F is a non-injective type function.


\begin{code}
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
      | Just ar <- lookupNameEnv ar_env n,
        Inline <- inl_inline inl_prag     = inl_prag { inl_sat = Just ar }
        -- add arity only for real INLINE pragmas, not INLINABLE
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
lhsBindArity _ env = env        -- PatBind/VarBind

------------------
tcSpecPrags :: Id -> [LSig Name]
            -> TcM [LTcSpecPrag]
-- Add INLINE and SPECIALSE pragmas
--    INLINE prags are added to the (polymorphic) Id directly
--    SPECIALISE prags are passed to the desugarer via TcSpecPrags
-- Pre-condition: the poly_id is zonked
-- Reason: required by tcSubExp
tcSpecPrags poly_id prag_sigs
  = do { unless (null bad_sigs) warn_discarded_sigs
       ; mapAndRecoverM (wrapLocM (tcSpec poly_id)) spec_sigs }
  where
    spec_sigs = filter isSpecLSig prag_sigs
    bad_sigs  = filter is_bad_sig prag_sigs
    is_bad_sig s = not (isSpecLSig s || isInlineLSig s)

    warn_discarded_sigs = warnPrags poly_id bad_sigs $
                          ptext (sLit "Discarding unexpected pragmas for")


--------------
tcSpec :: TcId -> Sig Name -> TcM TcSpecPrag
tcSpec poly_id prag@(SpecSig _ hs_ty inl) 
  -- The Name in the SpecSig may not be the same as that of the poly_id
  -- Example: SPECIALISE for a class method: the Name in the SpecSig is
  --          for the selector Id, but the poly_id is something like $cop
  = addErrCtxt (spec_ctxt prag) $
    do  { spec_ty <- tcHsSigType sig_ctxt hs_ty
        ; warnIf (not (isOverloadedTy poly_ty || isInlinePragma inl))
                 (ptext (sLit "SPECIALISE pragma for non-overloaded function") 
                  <+> quotes (ppr poly_id))
                  -- Note [SPECIALISE pragmas]
        ; wrap <- tcSubType origin sig_ctxt (idType poly_id) spec_ty
        ; return (SpecPrag poly_id wrap inl) }
  where
    name      = idName poly_id
    poly_ty   = idType poly_id
    origin    = SpecPragOrigin name
    sig_ctxt  = FunSigCtxt name
    spec_ctxt prag = hang (ptext (sLit "In the SPECIALISE pragma")) 2 (ppr prag)

tcSpec _ prag = pprPanic "tcSpec" (ppr prag)

--------------
tcImpPrags :: [LSig Name] -> TcM [LTcSpecPrag]
-- SPECIALISE pragamas for imported things
tcImpPrags prags
  = do { this_mod <- getModule
       ; dflags <- getDynFlags
       ; if (not_specialising dflags) then
            return []
         else
            mapAndRecoverM (wrapLocM tcImpSpec) 
            [L loc (name,prag) | (L loc prag@(SpecSig (L _ name) _ _)) <- prags
                               , not (nameIsLocalOrFrom this_mod name) ] }
  where
    -- Ignore SPECIALISE pragmas for imported things
    -- when we aren't specialising, or when we aren't generating
    -- code.  The latter happens when Haddocking the base library;
    -- we don't wnat complaints about lack of INLINABLE pragmas 
    not_specialising dflags
      | not (gopt Opt_Specialise dflags) = True
      | otherwise = case hscTarget dflags of
                      HscNothing -> True
                      HscInterpreted -> True
                      _other         -> False

tcImpSpec :: (Name, Sig Name) -> TcM TcSpecPrag
tcImpSpec (name, prag)
 = do { id <- tcLookupId name
      ; unless (isAnyInlinePragma (idInlinePragma id))
               (addWarnTc (impSpecErr name))
      ; tcSpec id prag }

impSpecErr :: Name -> SDoc
impSpecErr name
  = hang (ptext (sLit "You cannot SPECIALISE") <+> quotes (ppr name))
       2 (vcat [ ptext (sLit "because its definition has no INLINE/INLINABLE pragma")
               , parens $ sep 
                   [ ptext (sLit "or its defining module") <+> quotes (ppr mod)
                   , ptext (sLit "was compiled without -O")]])
  where
    mod = nameModule name

--------------
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
          ptext (sLit "Duplicate vectorisation declarations for") <+> ppr first
    reportVectDups _ = return ()

--------------
tcVect :: VectDecl Name -> TcM (VectDecl TcId)
-- FIXME: We can't typecheck the expression of a vectorisation declaration against the vectorised
--   type of the original definition as this requires internals of the vectoriser not available
--   during type checking.  Instead, constrain the rhs of a vectorisation declaration to be a single
--   identifier (this is checked in 'rnHsVectDecl').  Fix this by enabling the use of 'vectType'
--   from the vectoriser here.
tcVect (HsVect name rhs)
  = addErrCtxt (vectCtxt name) $
    do { var <- wrapLocM tcLookupId name
       ; let L rhs_loc (HsVar rhs_var_name) = rhs
       ; rhs_id <- tcLookupId rhs_var_name
       ; return $ HsVect var (L rhs_loc (HsVar rhs_id))
       }

{- OLD CODE:
         -- turn the vectorisation declaration into a single non-recursive binding
       ; let bind    = L loc $ mkTopFunBind name [mkSimpleMatch [] rhs] 
             sigFun  = const Nothing
             pragFun = mkPragFun [] (unitBag bind)

         -- perform type inference (including generalisation)
       ; (binds, [id'], _) <- tcPolyInfer False True sigFun pragFun NonRecursive [bind]
       
       ; traceTc "tcVect inferred type" $ ppr (varType id')
       ; traceTc "tcVect bindings"      $ ppr binds
       
         -- add all bindings, including the type variable and dictionary bindings produced by type
         -- generalisation to the right-hand side of the vectorisation declaration
       ; let [AbsBinds tvs evs _ evBinds actualBinds] = (map unLoc . bagToList) binds
       ; let [bind']                                  = bagToList actualBinds
             MatchGroup 
               [L _ (Match _ _ (GRHSs [L _ (GRHS _ rhs')] _))]
               _                                      = (fun_matches . unLoc) bind'
             rhsWrapped                               = mkHsLams tvs evs (mkHsDictLet evBinds rhs')
        
        -- We return the type-checked 'Id', to propagate the inferred signature
        -- to the vectoriser - see "Note [Typechecked vectorisation pragmas]" in HsDecls
       ; return $ HsVect (L loc id') (Just rhsWrapped)
       }
 -}
tcVect (HsNoVect name)
  = addErrCtxt (vectCtxt name) $
    do { var <- wrapLocM tcLookupId name
       ; return $ HsNoVect var
       }
tcVect (HsVectTypeIn isScalar lname rhs_name)
  = addErrCtxt (vectCtxt lname) $
    do { tycon <- tcLookupLocatedTyCon lname
       ; checkTc (   not isScalar             -- either    we have a non-SCALAR declaration
                 || isJust rhs_name           -- or        we explicitly provide a vectorised type
                 || tyConArity tycon == 0     -- otherwise the type constructor must be nullary
                 )
                 scalarTyConMustBeNullary

       ; rhs_tycon <- fmapMaybeM (tcLookupTyCon . unLoc) rhs_name
       ; return $ HsVectTypeOut isScalar tycon rhs_tycon
       }
tcVect (HsVectTypeOut _ _ _)
  = panic "TcBinds.tcVect: Unexpected 'HsVectTypeOut'"
tcVect (HsVectClassIn lname)
  = addErrCtxt (vectCtxt lname) $
    do { cls <- tcLookupLocatedClass lname
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
vectCtxt thing = ptext (sLit "When checking the vectorisation declaration for") <+> ppr thing

scalarTyConMustBeNullary :: MsgDoc
scalarTyConMustBeNullary = ptext (sLit "VECTORISE SCALAR type constructor must be nullary")

--------------
-- If typechecking the binds fails, then return with each
-- signature-less binder given type (forall a.a), to minimise 
-- subsequent error messages
recoveryCode :: [Name] -> TcSigFun -> TcM (LHsBinds TcId, [Id], TopLevelFlag)
recoveryCode binder_names sig_fn
  = do  { traceTc "tcBindsWithSigs: error recovery" (ppr binder_names)
        ; poly_ids <- mapM mk_dummy binder_names
        ; return (emptyBag, poly_ids, if all is_closed poly_ids
                                      then TopLevel else NotTopLevel) }
  where
    mk_dummy name 
        | isJust (sig_fn name) = tcLookupId name        -- Had signature; look it up
        | otherwise            = return (mkLocalId name forall_a_a)    -- No signature

    is_closed poly_id = isEmptyVarSet (tyVarsOfType (idType poly_id))

forall_a_a :: TcType
forall_a_a = mkForAllTy openAlphaTyVar (mkTyVarTy openAlphaTyVar)
\end{code}

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

%************************************************************************
%*                                                                      *
\subsection{tcMonoBind}
%*                                                                      *
%************************************************************************

@tcMonoBinds@ deals with a perhaps-recursive group of HsBinds.
The signatures have been dealt with already.

Note [Pattern bindings]
~~~~~~~~~~~~~~~~~~~~~~~
The rule for typing pattern bindings is this:

    ..sigs..
    p = e

where 'p' binds v1..vn, and 'e' may mention v1..vn, 
typechecks exactly like

    ..sigs..
    x = e       -- Inferred type
    v1 = case x of p -> v1
    ..
    vn = case x of p -> vn

Note that  
    (f :: forall a. a -> a) = id
should not typecheck because
       case id of { (f :: forall a. a->a) -> f }
will not typecheck.

\begin{code}
tcMonoBinds :: RecFlag  -- Whether the binding is recursive for typechecking purposes
                        -- i.e. the binders are mentioned in their RHSs, and
                        --      we are not rescued by a type signature
            -> TcSigFun -> LetBndrSpec 
            -> [LHsBind Name]
            -> TcM (LHsBinds TcId, [MonoBindInfo])

tcMonoBinds is_rec sig_fn no_gen
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
    do  { rhs_ty  <- newFlexiTyVarTy openTypeKind
        ; mono_id <- newNoSigLetBndr no_gen name rhs_ty
        ; (co_fn, matches') <- tcExtendIdBndrs [TcIdBndr mono_id NotTopLevel] $
                                 -- We extend the error context even for a non-recursive 
                                 -- function so that in type error messages we show the 
                                 -- type of the thing whose rhs we are type checking
                               tcMatchesFun name inf matches rhs_ty

        ; return (unitBag (L b_loc (FunBind { fun_id = L nm_loc mono_id, fun_infix = inf,
                                              fun_matches = matches', bind_fvs = fvs,
                                              fun_co_fn = co_fn, fun_tick = Nothing })),
                  [(name, Nothing, mono_id)]) }

tcMonoBinds _ sig_fn no_gen binds
  = do  { tc_binds <- mapM (wrapLocM (tcLhs sig_fn no_gen)) binds

        -- Bring the monomorphic Ids, into scope for the RHSs
        ; let mono_info  = getMonoBindInfo tc_binds
              rhs_id_env = [(name,mono_id) | (name, Nothing, mono_id) <- mono_info]
                    -- A monomorphic binding for each term variable that lacks 
                    -- a type sig.  (Ones with a sig are already in scope.)

        ; traceTc "tcMonoBinds" $ vcat [ ppr n <+> ppr id <+> ppr (idType id) 
                                       | (n,id) <- rhs_id_env]
        ; binds' <- tcExtendIdEnv2 rhs_id_env $ 
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
  = TcFunBind  MonoBindInfo  SrcSpan Bool (MatchGroup Name (LHsExpr Name)) 
  | TcPatBind [MonoBindInfo] (LPat TcId) (GRHSs Name (LHsExpr Name)) TcSigmaType

type MonoBindInfo = (Name, Maybe TcSigInfo, TcId)
        -- Type signature (if any), and
        -- the monomorphic bound things

tcLhs :: TcSigFun -> LetBndrSpec -> HsBind Name -> TcM TcMonoBind
tcLhs sig_fn no_gen (FunBind { fun_id = L nm_loc name, fun_infix = inf, fun_matches = matches })
  | Just sig <- sig_fn name
  = ASSERT2( case no_gen of { LetLclBndr -> True; LetGblBndr {} -> False }
           , ppr name )  -- { f :: ty; f x = e } is always done via CheckGen
                         -- which gives rise to LetLclBndr.  It wouldn't make
                         -- sense to have a *polymorphic* function Id at this point
    do  { mono_name <- newLocalName name
        ; let mono_id = mkLocalId mono_name (sig_tau sig)
        ; return (TcFunBind (name, Just sig, mono_id) nm_loc inf matches) }
  | otherwise
  = do  { mono_ty <- newFlexiTyVarTy openTypeKind
        ; mono_id <- newNoSigLetBndr no_gen name mono_ty
        ; return (TcFunBind (name, Nothing, mono_id) nm_loc inf matches) }

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

-------------------
tcRhs :: TcMonoBind -> TcM (HsBind TcId)
-- When we are doing pattern bindings, or multiple function bindings at a time
-- we *don't* bring any scoped type variables into scope
-- Wny not?  They are not completely rigid.
-- That's why we have the special case for a single FunBind in tcMonoBinds
tcRhs (TcFunBind (_,_,mono_id) loc inf matches)
  = tcExtendIdBndrs [TcIdBndr mono_id NotTopLevel] $
            -- NotTopLevel: it's a monomorphic binding
    do  { traceTc "tcRhs: fun bind" (ppr mono_id $$ ppr (idType mono_id))
        ; (co_fn, matches') <- tcMatchesFun (idName mono_id) inf 
                                            matches (idType mono_id)
        ; return (FunBind { fun_id = L loc mono_id, fun_infix = inf
                          , fun_matches = matches'
                          , fun_co_fn = co_fn 
                          , bind_fvs = placeHolderNames, fun_tick = Nothing }) }

tcRhs (TcPatBind infos pat' grhss pat_ty)
  = tcExtendIdBndrs [ TcIdBndr mono_id NotTopLevel | (_,_,mono_id) <- infos ] $
            -- NotTopLevel: it's a monomorphic binding
    do  { traceTc "tcRhs: pat bind" (ppr pat' $$ ppr pat_ty)
        ; grhss' <- addErrCtxt (patMonoBindsCtxt pat' grhss) $
                    tcGRHSsPat grhss pat_ty
        ; return (PatBind { pat_lhs = pat', pat_rhs = grhss', pat_rhs_ty = pat_ty 
                          , bind_fvs = placeHolderNames
                          , pat_ticks = (Nothing,[]) }) }


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

Note [Fail eagerly on bad signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If a type signaure is wrong, fail immediately:

 * the type sigs may bind type variables, so proceeding without them
   can lead to a cascade of errors

 * the type signature might be ambiguous, in which case checking
   the code against the signature will give a very similar error
   to the ambiguity error.

ToDo: this means we fall over if any type sig
is wrong (eg at the top level of the module), 
which is over-conservative

\begin{code}
tcTySigs :: [LSig Name] -> TcM ([TcId], TcSigFun)
tcTySigs hs_sigs
  = checkNoErrs $   -- See Note [Fail eagerly on bad signatures]
    do { ty_sigs_s<- mapAndRecoverM tcTySig hs_sigs
       ; let ty_sigs = concat ty_sigs_s
             env = mkNameEnv [(idName (sig_id sig), sig) | sig <- ty_sigs]
       ; return (map sig_id ty_sigs, lookupNameEnv env) }

tcTySig :: LSig Name -> TcM [TcSigInfo]
tcTySig (L loc (IdSig id))
  = do { sig <- instTcTySigFromId loc id
       ; return [sig] }
tcTySig (L loc (TypeSig names@(L _ name1 : _) hs_ty))
  = setSrcSpan loc $ 
    do { sigma_ty <- tcHsSigType (FunSigCtxt name1) hs_ty
       ; mapM (instTcTySig hs_ty sigma_ty) (map unLoc names) }
tcTySig _ = return []

instTcTySigFromId :: SrcSpan -> Id -> TcM TcSigInfo
instTcTySigFromId loc id
  = do { (tvs, theta, tau) <- tcInstType inst_sig_tyvars (idType id)
       ; return (TcSigInfo { sig_id = id, sig_loc = loc
                           , sig_tvs = [(Nothing, tv) | tv <- tvs]
                           , sig_theta = theta, sig_tau = tau }) }
  where
    -- Hack: in an instance decl we use the selector id as
    -- the template; but we do *not* want the SrcSpan on the Name of 
    -- those type variables to refer to the class decl, rather to
    -- the instance decl 
    inst_sig_tyvars tvs = tcInstSigTyVars (map set_loc tvs)
    set_loc tv = setTyVarName tv (mkInternalName (nameUnique n) (nameOccName n) loc)
      where
        n = tyVarName tv

instTcTySig :: LHsType Name -> TcType    -- HsType and corresponding TcType
            -> Name -> TcM TcSigInfo
instTcTySig hs_ty@(L loc _) sigma_ty name
  = do { (inst_tvs, theta, tau) <- tcInstType tcInstSigTyVars sigma_ty
       ; return (TcSigInfo { sig_id = poly_id, sig_loc = loc
                           , sig_tvs = zipEqual "instTcTySig" scoped_tvs inst_tvs
                           , sig_theta = theta, sig_tau = tau }) }
  where
    poly_id      = mkLocalId name sigma_ty

    scoped_names = hsExplicitTvs hs_ty
    (sig_tvs,_)  = tcSplitForAllTys sigma_ty

    scoped_tvs :: [Maybe Name]
    scoped_tvs = mk_scoped scoped_names sig_tvs

    mk_scoped :: [Name] -> [TyVar] -> [Maybe Name]
    mk_scoped []     tvs      = [Nothing | _ <- tvs]
    mk_scoped (n:ns) (tv:tvs) 
           | n == tyVarName tv = Just n  : mk_scoped ns     tvs
           | otherwise         = Nothing : mk_scoped (n:ns) tvs
    mk_scoped (n:ns) [] = pprPanic "mk_scoped" (ppr name $$ ppr (n:ns) $$ ppr hs_ty $$ ppr sigma_ty)

-------------------------------
data GeneralisationPlan 
  = NoGen               -- No generalisation, no AbsBinds

  | InferGen            -- Implicit generalisation; there is an AbsBinds
       Bool             --   True <=> apply the MR; generalise only unconstrained type vars
       Bool             --   True <=> bindings mention only variables with closed types
                        --            See Note [Bindings with closed types] in TcRnTypes

  | CheckGen TcSigInfo  -- One binding with a signature
                        -- Explicit generalisation; there is an AbsBinds

-- A consequence of the no-AbsBinds choice (NoGen) is that there is
-- no "polymorphic Id" and "monmomorphic Id"; there is just the one

instance Outputable GeneralisationPlan where
  ppr NoGen          = ptext (sLit "NoGen")
  ppr (InferGen b c) = ptext (sLit "InferGen") <+> ppr b <+> ppr c
  ppr (CheckGen s)   = ptext (sLit "CheckGen") <+> ppr s

decideGeneralisationPlan 
   :: DynFlags -> TcTypeEnv -> [Name]
   -> [LHsBind Name] -> TcSigFun -> GeneralisationPlan
decideGeneralisationPlan dflags type_env bndr_names lbinds sig_fn
  | bang_pat_binds                         = NoGen
  | Just sig <- one_funbind_with_sig binds = CheckGen sig
  | mono_local_binds                       = NoGen
  | otherwise                              = InferGen mono_restriction closed_flag

  where
    bndr_set = mkNameSet bndr_names
    binds = map unLoc lbinds

    bang_pat_binds = any isBangHsBind binds
       -- Bang patterns must not be polymorphic,
       -- because we are going to force them
       -- See Trac #4498

    mono_restriction  = xopt Opt_MonomorphismRestriction dflags 
                     && any restricted binds

    is_closed_ns :: NameSet -> Bool -> Bool
    is_closed_ns ns b = foldNameSet ((&&) . is_closed_id) b ns
        -- ns are the Names referred to from the RHS of this bind

    is_closed_id :: Name -> Bool
    -- See Note [Bindings with closed types] in TcRnTypes
    is_closed_id name 
      | name `elemNameSet` bndr_set
      = True              -- Ignore binders in this groups, of course
      | Just thing <- lookupNameEnv type_env name
      = case thing of
          ATcId { tct_closed = cl } -> isTopLevel cl  -- This is the key line
          ATyVar {}                 -> False          -- In-scope type variables
          AGlobal {}                -> True           --    are not closed!
          _                         -> pprPanic "is_closed_id" (ppr name)
      | otherwise
      = WARN( isInternalName name, ppr name ) True
        -- The free-var set for a top level binding mentions
        -- imported things too, so that we can report unused imports
        -- These won't be in the local type env.  
        -- Ditto class method etc from the current module
    
    closed_flag = foldr (is_closed_ns . bind_fvs) True binds

    mono_local_binds = xopt Opt_MonoLocalBinds dflags 
                    && not closed_flag

    no_sig n = isNothing (sig_fn n)

    -- With OutsideIn, all nested bindings are monomorphic
    -- except a single function binding with a signature
    one_funbind_with_sig [FunBind { fun_id = v }] = sig_fn (unLoc v)
    one_funbind_with_sig _                        = Nothing

    -- The Haskell 98 monomorphism resetriction
    restricted (PatBind {})                              = True
    restricted (VarBind { var_id = v })                  = no_sig v
    restricted (FunBind { fun_id = v, fun_matches = m }) = restricted_match m
                                                           && no_sig (unLoc v)
    restricted (AbsBinds {}) = panic "isRestrictedGroup/unrestricted AbsBinds"

    restricted_match (MG { mg_alts = L _ (Match [] _ _) : _ }) = True
    restricted_match _                                         = False
        -- No args => like a pattern binding
        -- Some args => a function binding

-------------------
checkStrictBinds :: TopLevelFlag -> RecFlag
                 -> [LHsBind Name]
                 -> LHsBinds TcId -> [Id]
                 -> TcM ()
-- Check that non-overloaded unlifted bindings are
--      a) non-recursive,
--      b) not top level, 
--      c) not a multiple-binding group (more or less implied by (a))

checkStrictBinds top_lvl rec_group orig_binds tc_binds poly_ids
  | unlifted || bang_pat
  = do  { checkTc (isNotTopLevel top_lvl)
                  (strictBindErr "Top-level" unlifted orig_binds)
        ; checkTc (isNonRec rec_group)
                  (strictBindErr "Recursive" unlifted orig_binds)

        ; checkTc (all is_monomorphic (bagToList tc_binds))
                  (polyBindErr orig_binds)
            -- data Ptr a = Ptr Addr#
            -- f x = let p@(Ptr y) = ... in ...
            -- Here the binding for 'p' is polymorphic, but does 
            -- not mix with an unlifted binding for 'y'.  You should
            -- use a bang pattern.  Trac #6078.
        
        ; checkTc (isSingleton orig_binds)
                  (strictBindErr "Multiple" unlifted orig_binds)

        -- Ensure that unlifted bindings which look lazy, like:
        --   f x = let I# y = x
        -- cause an error.
        ; when lifted_pat $
            checkTc bang_pat
                 -- No outer bang, but it's a compound pattern
                 -- E.g   (I# x#) = blah
                 -- Warn about this, but not about
                 --      x# = 4# +# 1#
                 --      (# a, b #) = ...
                 (unliftedMustBeBang orig_binds) }
  | otherwise
  = traceTc "csb2" (ppr poly_ids) >>
    return ()
  where
    unlifted    = any is_unlifted poly_ids
    bang_pat    = any (isBangHsBind    . unLoc) orig_binds
    lifted_pat  = any (isLiftedPatBind . unLoc) orig_binds

    is_unlifted id = case tcSplitForAllTys (idType id) of
                       (_, rho) -> isUnLiftedType rho

    is_monomorphic (L _ (AbsBinds { abs_tvs = tvs, abs_ev_vars = evs }))
                     = null tvs && null evs
    is_monomorphic _ = True

unliftedMustBeBang :: [LHsBind Name] -> SDoc
unliftedMustBeBang binds
  = hang (text "Pattern bindings containing unlifted types should use an outermost bang pattern:")
       2 (vcat (map ppr binds))

polyBindErr :: [LHsBind Name] -> SDoc
polyBindErr binds
  = hang (ptext (sLit "You can't mix polymorphic and unlifted bindings"))
       2 (vcat [vcat (map ppr binds), 
                ptext (sLit "Probable fix: use a bang pattern")])

strictBindErr :: String -> Bool -> [LHsBind Name] -> SDoc
strictBindErr flavour unlifted binds
  = hang (text flavour <+> msg <+> ptext (sLit "aren't allowed:")) 
       2 (vcat (map ppr binds))
  where
    msg | unlifted  = ptext (sLit "bindings for unlifted types")
        | otherwise = ptext (sLit "bang-pattern bindings")
\end{code}


%************************************************************************
%*                                                                      *
\subsection[TcBinds-errors]{Error contexts and messages}
%*                                                                      *
%************************************************************************


\begin{code}
-- This one is called on LHS, when pat and grhss are both Name 
-- and on RHS, when pat is TcId and grhss is still Name
patMonoBindsCtxt :: (OutputableBndr id, Outputable body) => LPat id -> GRHSs Name body -> SDoc
patMonoBindsCtxt pat grhss
  = hang (ptext (sLit "In a pattern binding:")) 2 (pprPatBind pat grhss)
\end{code}
