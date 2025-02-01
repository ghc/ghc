
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Pattern-matching bindings (HsBinds and MonoBinds)

Handles @HsBinds@; those at the top level require different handling,
in that the @Rec@/@NonRec@/etc structure is thrown away (whereas at
lower levels it is preserved with @let@/@letrec@s).
-}

module GHC.HsToCore.Binds
   ( dsTopLHsBinds, dsLHsBinds, decomposeRuleLhs, dsSpec
   , dsHsWrapper, dsHsWrappers
   , dsEvTerm, dsTcEvBinds, dsTcEvBinds_s, dsEvBinds
   , dsWarnOrphanRule
   )
where

import GHC.Prelude

import GHC.Driver.DynFlags
import GHC.Driver.Config
import qualified GHC.LanguageExtensions as LangExt
import GHC.Unit.Module

import {-# SOURCE #-}   GHC.HsToCore.Expr  ( dsLExpr )
import {-# SOURCE #-}   GHC.HsToCore.Match ( matchWrapper )

import GHC.HsToCore.Pmc.Utils( tracePm )

import GHC.HsToCore.Monad
import GHC.HsToCore.Errors.Types
import GHC.HsToCore.GuardedRHSs
import GHC.HsToCore.Utils
import GHC.HsToCore.Pmc ( addTyCs, pmcGRHSs )

import GHC.Hs             -- lots of things
import GHC.Core           -- lots of things
import GHC.Core.SimpleOpt    ( simpleOptExpr )
import GHC.Core.Opt.OccurAnal ( occurAnalyseExpr )
import GHC.Core.InstEnv ( CanonicalEvidence(..) )
import GHC.Core.Make
import GHC.Core.Utils
import GHC.Core.Opt.Arity     ( etaExpand )
import GHC.Core.Unfold.Make
import GHC.Core.FVs
import GHC.Core.Predicate
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Core.Rules
import GHC.Core.TyCo.Compare( eqType )

import GHC.Builtin.Names
import GHC.Builtin.Types ( naturalTy, typeSymbolKind, charTy )

import GHC.Tc.Types.Evidence

import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Var( EvVar )
import GHC.Types.SrcLoc
import GHC.Types.Basic
import GHC.Types.Unique.Set( nonDetEltsUniqSet )

import GHC.Data.Maybe
import GHC.Data.OrdList
import GHC.Data.Graph.Directed
import GHC.Data.Bag
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

import GHC.Utils.Constants (debugIsOn)
import GHC.Utils.Misc
import GHC.Utils.Monad
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Control.Monad

{-**********************************************************************
*                                                                      *
           Desugaring a MonoBinds
*                                                                      *
**********************************************************************-}

-- | Desugar top level binds, strict binds are treated like normal
-- binds since there is no good time to force before first usage.
dsTopLHsBinds :: LHsBinds GhcTc -> DsM (OrdList (Id,CoreExpr))
dsTopLHsBinds binds
     -- see Note [Strict binds checks]
  | not (null unlifted_binds) || not (null bang_binds)
  = do { mapM_ (top_level_err UnliftedTypeBinds) unlifted_binds
       ; mapM_ (top_level_err StrictBinds)       bang_binds
       ; return nilOL }

  | otherwise
  = do { (force_vars, prs) <- dsLHsBinds binds
       ; when debugIsOn $
         do { xstrict <- xoptM LangExt.Strict
            ; massertPpr (null force_vars || xstrict) (ppr binds $$ ppr force_vars) }
              -- with -XStrict, even top-level vars are listed as force vars.

       ; return (toOL prs) }

  where
    unlifted_binds = filter (isUnliftedHsBind . unLoc) binds
    bang_binds     = filter (isBangedHsBind   . unLoc) binds

    top_level_err bindsType (L loc bind)
      = putSrcSpanDs (locA loc) $
        diagnosticDs (DsTopLevelBindsNotAllowed bindsType bind)
{-
Note [Return non-recursive bindings in dependency order]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For recursive bindings, the desugarer has no choice: it returns a single big
Rec{...} group.

But for /non-recursive/ bindings, the desugarer guarantees to desugar them to
a sequence of non-recurive Core bindings, in dependency order.

Why is this important?  Partly it saves a bit of work in the first run of the
occurrence analyser. But more importantly, for linear types, non-recursive lets
can be linear whereas recursive-let can't. Since we check the output of the
desugarer for linearity (see also Note [Linting linearity]), desugaring
non-recursive lets to recursive lets would break linearity checks. An
alternative is to refine the typing rule for recursive lets so that we don't
have to care (see in particular #23218 and #18694), but the outcome of this line
of work is still unclear. In the meantime, being a little precise in the
desugarer is cheap. (paragraph written on 2023-06-09)

In dsLHSBinds (and dependencies), a single binding can be desugared to multiple
bindings. For instance because the source binding has the {-# SPECIALIZE #-}
pragma. In:

f _ = …
 where
  {-# SPECIALIZE g :: F Int -> F Int #-}
  g :: C a => F a -> F a
  g _ = …

The g binding desugars to

let {
  $sg = … } in

  g
  [RULES: "SPEC g" g @Int $dC = $sg]
  g = …

In order to avoid generating a letrec that will immediately be reordered, we
make sure to return the binding in dependency order [$sg, g].

-}

-- | Desugar all other kind of bindings, Ids of strict binds are returned to
-- later be forced in the binding group body, see Note [Desugar Strict binds]
--
-- Invariant: the desugared bindings are returned in dependency order,
-- see Note [Return non-recursive bindings in dependency order]
dsLHsBinds :: LHsBinds GhcTc -> DsM ([Id], [(Id,CoreExpr)])
dsLHsBinds binds
  = do { ds_bs <- mapM dsLHsBind binds
       ; return (foldr (\(a, a') (b, b') -> (a ++ b, a' ++ b'))
                         ([], []) ds_bs) }

------------------------
dsLHsBind :: LHsBind GhcTc
          -> DsM ([Id], [(Id,CoreExpr)])
dsLHsBind (L loc bind) = do dflags <- getDynFlags
                            putSrcSpanDs (locA loc) $ dsHsBind dflags bind

-- | Desugar a single binding (or group of recursive binds).
--
-- Invariant: the desugared bindings are returned in dependency order,
-- see Note [Return non-recursive bindings in dependency order]
dsHsBind :: DynFlags
         -> HsBind GhcTc
         -> DsM ([Id], [(Id,CoreExpr)])
         -- ^ The Ids of strict binds, to be forced in the body of the
         -- binding group see Note [Desugar Strict binds] and all
         -- bindings and their desugared right hand sides.

dsHsBind dflags (VarBind { var_id = var
                         , var_rhs = expr })
  = do  { core_expr <- dsLExpr expr
                -- Dictionary bindings are always VarBinds,
                -- so we only need do this here
        ; let core_bind@(id,_) = makeCorePair dflags var False 0 core_expr
              force_var = if xopt LangExt.Strict dflags
                          then [id]
                          else []
        ; return (force_var, [core_bind]) }

dsHsBind dflags b@(FunBind { fun_id = L loc fun
                           , fun_matches = matches
                           , fun_ext = (co_fn, tick)
                           })
 = dsHsWrapper co_fn $ \core_wrap ->
   do { (args, body) <- matchWrapper (mkPrefixFunRhs (L loc (idName fun)) noAnn) Nothing matches

      ; let body' = mkOptTickBox tick body
            rhs   = core_wrap (mkLams args body')
            core_binds@(id,_) = makeCorePair dflags fun False 0 rhs
            force_var
                -- Bindings are strict when -XStrict is enabled
              | xopt LangExt.Strict dflags
              , matchGroupArity matches == 0 -- no need to force lambdas
              = [id]
              | isBangedHsBind b
              = [id]
              | otherwise
              = []
      ; --pprTrace "dsHsBind" (vcat [ ppr fun <+> ppr (idInlinePragma fun)
        --                          , ppr (mg_alts matches)
        --                          , ppr args, ppr core_binds, ppr body']) $
        return (force_var, [core_binds]) }

dsHsBind dflags (PatBind { pat_lhs = pat, pat_rhs = grhss
                         , pat_ext = (ty, (rhs_tick, var_ticks))
                         })
  = do  { rhss_nablas <- pmcGRHSs PatBindGuards grhss
        ; body_expr <- dsGuarded grhss ty rhss_nablas
        ; let body' = mkOptTickBox rhs_tick body_expr
              pat'  = decideBangHood dflags pat
        ; (force_var,sel_binds) <- mkSelectorBinds var_ticks pat PatBindRhs body'
          -- We silently ignore inline pragmas; no makeCorePair
          -- Not so cool, but really doesn't matter
        ; let force_var' = if isBangedLPat pat'
                           then [force_var]
                           else []
        ; return (force_var', sel_binds) }

dsHsBind
  dflags
  (XHsBindsLR (AbsBinds { abs_tvs = tyvars, abs_ev_vars = dicts
                        , abs_exports = exports
                        , abs_ev_binds = ev_binds
                        , abs_binds = binds, abs_sig = has_sig }))
  = addTyCs FromSource (listToBag dicts) $
             -- addTyCs: push type constraints deeper
             --            for inner pattern match check
             -- See Check, Note [Long-distance information]
    dsTcEvBinds_s ev_binds $ \ds_ev_binds -> do
    do { ds_binds <- dsLHsBinds binds
         -- dsAbsBinds does the hard work
       ; dsAbsBinds dflags tyvars dicts exports ds_ev_binds ds_binds
                    (isSingleton binds) has_sig }

dsHsBind _ (PatSynBind{}) = panic "dsHsBind: PatSynBind"

-----------------------
dsAbsBinds :: DynFlags
           -> [TyVar] -> [EvVar] -> [ABExport]
           -> [CoreBind]                -- Desugared evidence bindings
           -> ([Id], [(Id,CoreExpr)])   -- Desugared value bindings
           -> Bool                      -- Single source binding
           -> Bool                      -- Single binding with signature
           -> DsM ([Id], [(Id,CoreExpr)])

dsAbsBinds dflags tyvars dicts exports
           ds_ev_binds (force_vars, bind_prs) is_singleton has_sig

    -- A very important common case: one exported variable
    -- Non-recursive bindings come through this way
    -- So do self-recursive bindings
    --    gbl_id = wrap (/\tvs \dicts. let ev_binds
    --                                 letrec bind_prs
    --                                 in lcl_id)
  | [export] <- exports
  , ABE { abe_poly = global_id, abe_mono = local_id
        , abe_wrap = wrap, abe_prags = prags } <- export
  , Just force_vars' <- case force_vars of
                           []                  -> Just []
                           [v] | v == local_id -> Just [global_id]
                           _                   -> Nothing
       -- If there is a variable to force, it's just the
       -- single variable we are binding here
  = do { dsHsWrapper wrap $ \core_wrap -> do -- Usually the identity
       { let rhs = core_wrap $
                   mkLams tyvars $ mkLams dicts $
                   mkCoreLets ds_ev_binds $
                   body

             body | has_sig
                  , [(_, lrhs)] <- bind_prs
                  = lrhs
                  | otherwise
                  = mkLetRec bind_prs (Var local_id)

       ; (spec_binds, rules) <- dsSpecs rhs prags

       ; let global_id' = addIdSpecialisations global_id rules
             main_bind  = makeCorePair dflags global_id'
                                       (isDefaultMethod prags)
                                       (dictArity dicts) rhs

       ; return (force_vars', fromOL spec_binds ++ [main_bind]) } }

    -- Another common case: no tyvars, no dicts
    -- In this case we can have a much simpler desugaring
    --    lcl_id{inl-prag} = rhs  -- Auxiliary binds
    --    gbl_id = lcl_id |> co   -- Main binds
    --
    -- See Note [The no-tyvar no-dict case]
  | null tyvars, null dicts
  = do { let wrap_first_bind f ((main, main_rhs):other_binds) =
               ((main, f main_rhs):other_binds)
             wrap_first_bind _ [] = panic "dsAbsBinds received an empty binding list"

             mk_main :: ABExport -> DsM (Id, CoreExpr)
             mk_main (ABE { abe_poly = gbl_id, abe_mono = lcl_id
                          , abe_wrap = wrap })
                     -- No SpecPrags (no dicts)
                     -- Can't be a default method (default methods are singletons)
               = do { dsHsWrapper wrap $ \core_wrap -> do
                    { return ( gbl_id `setInlinePragma` defaultInlinePragma
                             , core_wrap (Var lcl_id)) } }
       ; main_prs <- mapM mk_main exports
       ; let bind_prs' = map mk_aux_bind bind_prs
             -- When there's a single source binding, we wrap the evidence binding in a
             -- separate let-rec (DSB1) inside the first desugared binding (DSB2).
             -- See Note [The no-tyvar no-dict case].
             final_prs | is_singleton = wrap_first_bind (mkCoreLets ds_ev_binds) bind_prs'
                       | otherwise = flattenBinds ds_ev_binds ++ bind_prs'
       ; return (force_vars, final_prs ++ main_prs ) }

    -- The general case
    -- See Note [Desugaring AbsBinds]
  | otherwise
  = do { let aux_binds = Rec (map mk_aux_bind bind_prs)
                -- Monomorphic recursion possible, hence Rec

             new_force_vars = get_new_force_vars force_vars
             locals       = map abe_mono exports
             all_locals   = locals ++ new_force_vars
             tup_expr     = mkBigCoreVarTup all_locals
             tup_ty       = exprType tup_expr
       ; let poly_tup_rhs = mkLams tyvars $ mkLams dicts $
                            mkCoreLets ds_ev_binds $
                            mkLet aux_binds $
                            tup_expr

       ; poly_tup_id <- newSysLocalMDs (exprType poly_tup_rhs)

        -- Find corresponding global or make up a new one: sometimes
        -- we need to make new export to desugar strict binds, see
        -- Note [Desugar Strict binds]
       ; (exported_force_vars, extra_exports) <- get_exports force_vars

       ; let mk_bind (ABE { abe_wrap = wrap
                          , abe_poly = global
                          , abe_mono = local, abe_prags = spec_prags })
                          -- See Note [ABExport wrapper] in "GHC.Hs.Binds"
                = do { tup_id  <- newSysLocalMDs tup_ty
                     ; dsHsWrapper wrap $ \core_wrap -> do
                     { let rhs = core_wrap $ mkLams tyvars $ mkLams dicts $
                                 mkBigTupleSelector all_locals local tup_id $
                                 mkVarApps (Var poly_tup_id) (tyvars ++ dicts)
                           rhs_for_spec = Let (NonRec poly_tup_id poly_tup_rhs) rhs
                     ; (spec_binds, rules) <- dsSpecs rhs_for_spec spec_prags
                     ; let global' = (global `setInlinePragma` defaultInlinePragma)
                                             `addIdSpecialisations` rules
                           -- Kill the INLINE pragma because it applies to
                           -- the user written (local) function.  The global
                           -- Id is just the selector.  Hmm.
                     ; return (fromOL spec_binds ++ [(global', rhs)]) } }

       ; export_binds_s <- mapM mk_bind (exports ++ extra_exports)

       ; return ( exported_force_vars
                , (poly_tup_id, poly_tup_rhs) :
                   concat export_binds_s) }
  where
    mk_aux_bind :: (Id,CoreExpr) -> (Id,CoreExpr)
    mk_aux_bind (lcl_id, rhs) = let lcl_w_inline = lookupVarEnv inline_env lcl_id
                                                   `orElse` lcl_id
                                 in
                                 makeCorePair dflags lcl_w_inline False 0 rhs

    inline_env :: IdEnv Id -- Maps a monomorphic local Id to one with
                           -- the inline pragma from the source
                           -- The type checker put the inline pragma
                           -- on the *global* Id, so we need to transfer it
    inline_env
      = mkVarEnv [ (lcl_id, setInlinePragma lcl_id prag)
                 | ABE { abe_mono = lcl_id, abe_poly = gbl_id } <- exports
                 , let prag = idInlinePragma gbl_id ]

    global_env :: IdEnv Id -- Maps local Id to its global exported Id
    global_env =
      mkVarEnv [ (local, global)
               | ABE { abe_mono = local, abe_poly = global } <- exports
               ]

    -- find variables that are not exported
    get_new_force_vars lcls =
      foldr (\lcl acc -> case lookupVarEnv global_env lcl of
                           Just _ -> acc
                           Nothing -> lcl:acc)
            [] lcls

    -- find exports or make up new exports for force variables
    get_exports :: [Id] -> DsM ([Id], [ABExport])
    get_exports lcls =
      foldM (\(glbls, exports) lcl ->
              case lookupVarEnv global_env lcl of
                Just glbl -> return (glbl:glbls, exports)
                Nothing   -> do export <- mk_export lcl
                                let glbl = abe_poly export
                                return (glbl:glbls, export:exports))
            ([],[]) lcls

    mk_export local =
      do global <- newSysLocalMDs
                     (exprType (mkLams tyvars (mkLams dicts (Var local))))
         return (ABE { abe_poly  = global
                     , abe_mono  = local
                     , abe_wrap  = WpHole
                     , abe_prags = SpecPrags [] })

-- | This is where we apply INLINE and INLINABLE pragmas. All we need to
-- do is to attach the unfolding information to the Id.
--
-- Other decisions about whether to inline are made in
-- `calcUnfoldingGuidance` but the decision about whether to then expose
-- the unfolding in the interface file is made in `GHC.Iface.Tidy.addExternal`
-- using this information.
------------------------
makeCorePair :: DynFlags -> Id -> Bool -> Arity -> CoreExpr
             -> (Id, CoreExpr)
makeCorePair dflags gbl_id is_default_method dict_arity rhs
  | is_default_method    -- Default methods are *always* inlined
                         -- See Note [INLINE and default methods] in GHC.Tc.TyCl.Instance
  = (gbl_id `setIdUnfolding` mkCompulsoryUnfolding' simpl_opts rhs, rhs)

  | otherwise
  = case inlinePragmaSpec inline_prag of
          NoUserInlinePrag -> (gbl_id, rhs)
          NoInline  {}     -> (gbl_id, rhs)
          Opaque    {}     -> (gbl_id, rhs)
          Inlinable {}     -> (gbl_id `setIdUnfolding` inlinable_unf, rhs)
          Inline    {}     -> inline_pair
  where
    simpl_opts    = initSimpleOpts dflags
    inline_prag   = idInlinePragma gbl_id
    inlinable_unf = mkInlinableUnfolding simpl_opts StableUserSrc rhs
    inline_pair
       | Just arity <- inlinePragmaSat inline_prag
        -- Add an Unfolding for an INLINE (but not for NOINLINE)
        -- And eta-expand the RHS; see Note [Eta-expanding INLINE things]
       , let real_arity = dict_arity + arity
        -- NB: The arity passed to mkInlineUnfoldingWithArity
        --     must take account of the dictionaries
       = ( gbl_id `setIdUnfolding` mkInlineUnfoldingWithArity simpl_opts StableUserSrc real_arity rhs
         , etaExpand real_arity rhs)

       | otherwise
       = pprTrace "makeCorePair: arity missing" (ppr gbl_id) $
         (gbl_id `setIdUnfolding` mkInlineUnfoldingNoArity simpl_opts StableUserSrc rhs, rhs)

dictArity :: [Var] -> Arity
-- Don't count coercion variables in arity
dictArity dicts = count isId dicts

{-
Note [Desugaring AbsBinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~
In the general AbsBinds case we desugar the binding to this:

       tup a (d:Num a) = let fm = ...gm...
                             gm = ...fm...
                         in (fm,gm)
       f a d = case tup a d of { (fm,gm) -> fm }
       g a d = case tup a d of { (fm,gm) -> fm }

Note [Rules and inlining]
~~~~~~~~~~~~~~~~~~~~~~~~~
Common special case: no type or dictionary abstraction
This is a bit less trivial than you might suppose
The naive way would be to desugar to something like
        f_lcl = ...f_lcl...     -- The "binds" from AbsBinds
        M.f = f_lcl             -- Generated from "exports"
But we don't want that, because if M.f isn't exported,
it'll be inlined unconditionally at every call site (its rhs is
trivial).  That would be ok unless it has RULES, which would
thereby be completely lost.  Bad, bad, bad.

Instead we want to generate
        M.f = ...f_lcl...
        f_lcl = M.f
Now all is cool. The RULES are attached to M.f (by SimplCore),
and f_lcl is rapidly inlined away.

This does not happen in the same way to polymorphic binds,
because they desugar to
        M.f = /\a. let f_lcl = ...f_lcl... in f_lcl
Although I'm a bit worried about whether full laziness might
float the f_lcl binding out and then inline M.f at its call site

Note [Specialising in no-dict case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Even if there are no tyvars or dicts, we may have specialisation pragmas.
Class methods can generate
      AbsBinds [] [] [( ... spec-prag]
         { AbsBinds [tvs] [dicts] ...blah }
So the overloading is in the nested AbsBinds. A good example is in GHC.Float:

  class  (Real a, Fractional a) => RealFrac a  where
    round :: (Integral b) => a -> b

  instance  RealFrac Float  where
    {-# SPECIALIZE round :: Float -> Int #-}

The top-level AbsBinds for $cround has no tyvars or dicts (because the
instance does not).  But the method is locally overloaded!

Note [The no-tyvar no-dict case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are desugaring
    AbsBinds { tyvars   = []
             , dicts    = []
             , exports  = [ ABE f fm, ABE g gm ]
             , binds    = B
             , ev_binds = EB }
That is: no type variables or dictionary abstractions.  Here, `f` and `fm` are
the polymorphic and monomorphic versions of `f`; in this special case they will
both have the same type.

Specialising Note [Desugaring AbsBinds] for this case gives the desugaring

    tup = letrec EB' in letrec B' in (fm,gm)
    f = case tup of { (fm,gm) -> fm }
    g = case tup of { (fm,gm) -> fm }

where B' is the result of desugaring B. This desugaring is a little silly: we
don't need the intermediate tuple (contrast with the general case where fm and f
have different types). So instead, in this case, we desugar to

    EB'; B'; f=fm; g=gm

This is done in the `null tyvars, null dicts` case of `dsAbsBinds`.

But there is a wrinkle (DSB1).  If the original binding group was
/non-recursive/, we want to return a bunch of non-recursive bindings in
dependency order: see Note [Return non-recursive bindings in dependency order].

But there is no guarantee that EB', the desugared evidence bindings, will be
non-recursive.  Happily, in the non-recursive case, B will have just a single
binding (f = rhs), so we can wrap EB' around its RHS, thus:

   fm = letrec EB' in rhs; f = fm

There is a sub-wrinkle (DSB2).  If B is a /pattern/ bindings, it will desugar to
a "main" binding followed by a bunch of selectors. The main binding always
comes first, so we can pick it out and wrap EB' around its RHS.  For example

    AbsBinds { tyvars   = []
             , dicts    = []
             , exports  = [ ABE p pm, ABE q qm ]
             , binds    = PatBind (pm, Just qm) rhs
             , ev_binds = EB }

can desguar to

   pt = let EB' in
        case rhs of
          (pm,Just qm) -> (pm,qm)
   pm = case pt of (pm,qm) -> pm
   qm = case pt of (pm,qm) -> qm

   p = pm
   q = qm

The first three bindings come from desugaring the PatBind, and subsequently
wrapping the RHS of the main binding in EB'.

Why got to this trouble?  It's a common case, and it removes the
quadratic-sized tuple desugaring.  Less clutter, hopefully faster
compilation, especially in a case where there are a *lot* of
bindings.

Note [Eta-expanding INLINE things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   foo :: Eq a => a -> a
   {-# INLINE foo #-}
   foo x = ...

If (foo d) ever gets floated out as a common sub-expression (which can
happen as a result of method sharing), there's a danger that we never
get to do the inlining, which is a Terribly Bad thing given that the
user said "inline"!

To avoid this we preemptively eta-expand the definition, so that foo
has the arity with which it is declared in the source code.  In this
example it has arity 2 (one for the Eq and one for x). Doing this
should mean that (foo d) is a PAP and we don't share it.

Note [Nested arities]
~~~~~~~~~~~~~~~~~~~~~
For reasons that are not entirely clear, method bindings come out looking like
this:

  AbsBinds [] [] [$cfromT <= [] fromT]
    $cfromT [InlPrag=INLINE] :: T Bool -> Bool
    { AbsBinds [] [] [fromT <= [] fromT_1]
        fromT :: T Bool -> Bool
        { fromT_1 ((TBool b)) = not b } } }

Note the nested AbsBind.  The arity for the unfolding on $cfromT should be
gotten from the binding for fromT_1.

It might be better to have just one level of AbsBinds, but that requires more
thought!


Note [Desugar Strict binds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
See https://gitlab.haskell.org/ghc/ghc/wikis/strict-pragma

Desugaring strict variable bindings looks as follows (core below ==>)

  let !x = rhs
  in  body
==>
  let x = rhs
  in x `seq` body -- seq the variable

and if it is a pattern binding the desugaring looks like

  let !pat = rhs
  in body
==>
  let x = rhs -- bind the rhs to a new variable
      pat = x
  in x `seq` body -- seq the new variable

if there is no variable in the pattern desugaring looks like

  let False = rhs
  in body
==>
  let x = case rhs of {False -> (); _ -> error "Match failed"}
  in x `seq` body

In order to force the Ids in the binding group they are passed around
in the dsHsBind family of functions, and later seq'ed in GHC.HsToCore.Expr.ds_val_bind.

Consider a recursive group like this

  letrec
     f : g = rhs[f,g]
  in <body>

Without `Strict`, we get a translation like this:

  let t = /\a. letrec tm = rhs[fm,gm]
                      fm = case t of fm:_ -> fm
                      gm = case t of _:gm -> gm
                in
                (fm,gm)

  in let f = /\a. case t a of (fm,_) -> fm
  in let g = /\a. case t a of (_,gm) -> gm
  in <body>

Here `tm` is the monomorphic binding for `rhs`.

With `Strict`, we want to force `tm`, but NOT `fm` or `gm`.
Alas, `tm` isn't in scope in the `in <body>` part.

The simplest thing is to return it in the polymorphic
tuple `t`, thus:

  let t = /\a. letrec tm = rhs[fm,gm]
                      fm = case t of fm:_ -> fm
                      gm = case t of _:gm -> gm
                in
                (tm, fm, gm)

  in let f = /\a. case t a of (_,fm,_) -> fm
  in let g = /\a. case t a of (_,_,gm) -> gm
  in let tm = /\a. case t a of (tm,_,_) -> tm
  in tm `seq` <body>


See https://gitlab.haskell.org/ghc/ghc/wikis/strict-pragma for a more
detailed explanation of the desugaring of strict bindings.

Wrinkle 1: forcing linear variables

Consider

  let %1 !x = rhs in <body>
==>
  let x = rhs in x `seq` <body>

In the desugared version x is used in both arguments of seq. This isn't
recognised a linear. So we can't strictly speaking use seq. Instead, the code is
really desugared as

  let x = rhs in case x of x { _ -> <body> }

The shadowing with the case-binder is crucial. The linear linter (see
Note [Linting linearity] in GHC.Core.Lint) understands this as linear. This is
what the seqVar function does.

To be more precise, suppose x has multiplicity p, the fully annotated seqVar (in
Core, p is really stored inside x) is

  case x of %p x { _ -> <body> }

In linear Core, case u of %p y { _ -> v } consumes u with multiplicity p, and
makes y available with multiplicity p in v. Which is exactly what we want.

Wrinkle 2: linear patterns

Consider the following linear binding (linear lets are always non-recursive):

  let
     %1 f : g = rhs
  in <body>

The general case would desugar it to

  let t = let tm = rhs
              fm = case tm of fm:_ -> fm
              gm = case tm of _:gm -> gm
           in
           (tm, fm, gm)

  in let f = case t a of (_,fm,_) -> fm
  in let g = case t a of (_,_,gm) -> gm
  in let tm = case t a of (tm,_,_) -> tm
  in tm `seq` <body>

But all the case expression drop variables, which is prohibited by
linearity. But because this is a non-recursive let (in particular we're
desugaring a single binding), we can (and do) desugar the binding as a simple
case-expression instead:

  case rhs of {
    (f:g) -> <body>
  }

This is handled by the special case: a non-recursive PatBind in
GHC.HsToCore.Expr.ds_val_bind.

Note [Strict binds checks]
~~~~~~~~~~~~~~~~~~~~~~~~~~
There are several checks around properly formed strict bindings. They
all link to this Note. These checks must be here in the desugarer because
we cannot know whether or not a type is unlifted until after zonking, due
to representation polymorphism. These checks all used to be handled in the
typechecker in checkStrictBinds (before Jan '17).

We define an "unlifted bind" to be any bind that binds an unlifted id. Note that

  x :: Char
  (# True, x #) = blah

is *not* an unlifted bind. Unlifted binds are detected by GHC.Hs.Utils.isUnliftedHsBind.

Define a "banged bind" to have a top-level bang. Detected by GHC.Hs.Pat.isBangedHsBind.
Define a "strict bind" to be either an unlifted bind or a banged bind.

The restrictions are:
  1. Strict binds may not be top-level. Checked in dsTopLHsBinds.

  2. Unlifted binds must also be banged. (There is no trouble to compile an unbanged
     unlifted bind, but an unbanged bind looks lazy, and we don't want users to be
     surprised by the strictness of an unlifted bind.) Checked in first clause
     of GHC.HsToCore.Expr.ds_val_bind.

  3. Unlifted binds may not have polymorphism (#6078). (That is, no quantified type
     variables or constraints.) Checked in first clause
     of GHC.HsToCore.Expr.ds_val_bind.

  4. Unlifted binds may not be recursive. Checked in second clause of ds_val_bind.

-}

------------------------
dsSpecs :: CoreExpr     -- Its rhs
        -> TcSpecPrags
        -> DsM ( OrdList (Id,CoreExpr)  -- Binding for specialised Ids
               , [CoreRule] )           -- Rules for the Global Ids
-- See Note [Handling SPECIALISE pragmas] in GHC.Tc.Gen.Bind
dsSpecs _ IsDefaultMethod = return (nilOL, [])
dsSpecs poly_rhs (SpecPrags sps)
  = do { pairs <- mapMaybeM (dsSpec (Just poly_rhs)) sps
       ; let (spec_binds_s, rules) = unzip pairs
       ; return (concatOL spec_binds_s, rules) }

dsSpec :: Maybe CoreExpr        -- Just rhs => RULE is for a local binding
                                -- Nothing => RULE is for an imported Id
                                --            rhs is in the Id's unfolding
       -> Located TcSpecPrag
       -> DsM (Maybe (OrdList (Id,CoreExpr), CoreRule))
dsSpec mb_poly_rhs (L loc (SpecPrag poly_id spec_co spec_inl))
  | isJust (isClassOpId_maybe poly_id)
  = putSrcSpanDs loc $
    do { diagnosticDs (DsUselessSpecialiseForClassMethodSelector poly_id)
       ; return Nothing  }  -- There is no point in trying to specialise a class op
                            -- Moreover, classops don't (currently) have an inl_sat arity set
                            -- (it would be Just 0) and that in turn makes makeCorePair bleat

  | no_act_spec && isNeverActive rule_act
  = putSrcSpanDs loc $
    do { diagnosticDs (DsUselessSpecialiseForNoInlineFunction poly_id)
       ; return Nothing  }  -- Function is NOINLINE, and the specialisation inherits that
                            -- See Note [Activation pragmas for SPECIALISE]

  | otherwise
  = putSrcSpanDs loc $
    do { uniq <- newUnique
       ; let poly_name = idName poly_id
             spec_occ  = mkSpecOcc (getOccName poly_name)
             spec_name = mkInternalName uniq spec_occ (getSrcSpan poly_name)
             (spec_bndrs, spec_app) = collectHsWrapBinders spec_co
               -- spec_co looks like
               --         \spec_bndrs. [] spec_args
               -- perhaps with the body of the lambda wrapped in some WpLets
               -- E.g. /\a \(d:Eq a). let d2 = $df d in [] (Maybe a) d2

       ; dsHsWrapper spec_app $ \core_app -> do

       { let ds_lhs  = core_app (Var poly_id)
             spec_ty = mkLamTypes spec_bndrs (exprType ds_lhs)
       ; -- pprTrace "dsRule" (vcat [ text "Id:" <+> ppr poly_id
         --                         , text "spec_co:" <+> ppr spec_co
         --                         , text "ds_rhs:" <+> ppr ds_lhs ]) $
         dflags <- getDynFlags
       ; case decomposeRuleLhs dflags spec_bndrs ds_lhs (mkVarSet spec_bndrs) of {
           Left msg -> do { diagnosticDs msg; return Nothing } ;
           Right (rule_bndrs, _fn, rule_lhs_args) -> do

       { this_mod <- getModule
       ; let fn_unf    = realIdUnfolding poly_id
             simpl_opts = initSimpleOpts dflags
             spec_unf   = specUnfolding simpl_opts spec_bndrs core_app rule_lhs_args fn_unf
             spec_id    = mkLocalId spec_name ManyTy spec_ty -- Specialised binding is toplevel, hence Many.
                            `setInlinePragma` inl_prag
                            `setIdUnfolding`  spec_unf

             rule = mkSpecRule dflags this_mod False rule_act (text "USPEC")
                               poly_id rule_bndrs rule_lhs_args
                               (mkVarApps (Var spec_id) spec_bndrs)
             spec_rhs = mkLams spec_bndrs (core_app poly_rhs)

       ; dsWarnOrphanRule rule

       ; tracePm "dsSpec" (vcat
            [ text "fun:" <+> ppr poly_id
            , text "spec_co:" <+> ppr spec_co
            , text "spec_bndrs:" <+>  ppr spec_bndrs
            , text "ds_lhs:" <+> ppr ds_lhs
            , text "args:" <+>  ppr rule_lhs_args ])
       ; return (Just (unitOL (spec_id, spec_rhs), rule))
            -- NB: do *not* use makeCorePair on (spec_id,spec_rhs), because
            --     makeCorePair overwrites the unfolding, which we have
            --     just created using specUnfolding
       } } } }
  where
    is_local_id = isJust mb_poly_rhs
    poly_rhs | Just rhs <-  mb_poly_rhs
             = rhs          -- Local Id; this is its rhs
             | Just unfolding <- maybeUnfoldingTemplate (realIdUnfolding poly_id)
             = unfolding    -- Imported Id; this is its unfolding
                            -- Use realIdUnfolding so we get the unfolding
                            -- even when it is a loop breaker.
                            -- We want to specialise recursive functions!
             | otherwise = pprPanic "dsImpSpecs" (ppr poly_id)
                            -- The type checker has checked that it *has* an unfolding

    id_inl = idInlinePragma poly_id

    -- See Note [Activation pragmas for SPECIALISE]
    inl_prag | not (isDefaultInlinePragma spec_inl)    = spec_inl
             | not is_local_id  -- See Note [Specialising imported functions]
                                 -- in OccurAnal
             , isStrongLoopBreaker (idOccInfo poly_id) = neverInlinePragma
             | otherwise                               = id_inl
     -- Get the INLINE pragma from SPECIALISE declaration, or,
     -- failing that, from the original Id

    spec_prag_act = inlinePragmaActivation spec_inl

    -- See Note [Activation pragmas for SPECIALISE]
    -- no_act_spec is True if the user didn't write an explicit
    -- phase specification in the SPECIALISE pragma
    no_act_spec = case inlinePragmaSpec spec_inl of
                    NoInline _   -> isNeverActive  spec_prag_act
                    Opaque _     -> isNeverActive  spec_prag_act
                    _            -> isAlwaysActive spec_prag_act
    rule_act | no_act_spec = inlinePragmaActivation id_inl   -- Inherit
             | otherwise   = spec_prag_act                   -- Specified by user


dsWarnOrphanRule :: CoreRule -> DsM ()
dsWarnOrphanRule rule
  = when (ruleIsOrphan rule) $
    diagnosticDs (DsOrphanRule rule)

{- Note [SPECIALISE on INLINE functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used to warn that using SPECIALISE for a function marked INLINE
would be a no-op; but it isn't!  Especially with worker/wrapper split
we might have
   {-# INLINE f #-}
   f :: Ord a => Int -> a -> ...
   f d x y = case x of I# x' -> $wf d x' y

We might want to specialise 'f' so that we in turn specialise '$wf'.
We can't even /name/ '$wf' in the source code, so we can't specialise
it even if we wanted to.  #10721 is a case in point.

Note [Activation pragmas for SPECIALISE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
From a user SPECIALISE pragma for f, we generate
  a) A top-level binding    spec_fn = rhs
  b) A RULE                 f dOrd = spec_fn

We need two pragma-like things:

* spec_fn's inline pragma: inherited from f's inline pragma (ignoring
                           activation on SPEC), unless overridden by SPEC INLINE

* Activation of RULE: from SPECIALISE pragma (if activation given)
                      otherwise from f's inline pragma

This is not obvious (see #5237)!

Examples      Rule activation   Inline prag on spec'd fn
---------------------------------------------------------------------
SPEC [n] f :: ty            [n]   Always, or NOINLINE [n]
                                  copy f's prag

NOINLINE f
SPEC [n] f :: ty            [n]   NOINLINE
                                  copy f's prag

NOINLINE [k] f
SPEC [n] f :: ty            [n]   NOINLINE [k]
                                  copy f's prag

INLINE [k] f
SPEC [n] f :: ty            [n]   INLINE [k]
                                  copy f's prag

SPEC INLINE [n] f :: ty     [n]   INLINE [n]
                                  (ignore INLINE prag on f,
                                  same activation for rule and spec'd fn)

NOINLINE [k] f
SPEC f :: ty                [n]   INLINE [k]


************************************************************************
*                                                                      *
\subsection{Adding inline pragmas}
*                                                                      *
************************************************************************
-}

decomposeRuleLhs :: DynFlags -> [Var] -> CoreExpr
                 -> VarSet   -- Free vars of the RHS
                 -> Either DsMessage ([Var], Id, [CoreExpr])
-- (decomposeRuleLhs bndrs lhs) takes apart the LHS of a RULE,
-- The 'bndrs' are the quantified binders of the rules, but decomposeRuleLhs
-- may add some extra dictionary binders (see Note [Free dictionaries on rule LHS])
--
-- Returns an error message if the LHS isn't of the expected shape
-- Note [Decomposing the left-hand side of a RULE]
decomposeRuleLhs dflags orig_bndrs orig_lhs rhs_fvs
  | Var funId <- fun2
  , Just con <- isDataConId_maybe funId
  = Left (DsRuleIgnoredDueToConstructor con) -- See Note [No RULES on datacons]

  | otherwise = case decompose fun2 args2 of
        Nothing -> -- pprTrace "decomposeRuleLhs 3" (vcat [ text "orig_bndrs:" <+> ppr orig_bndrs
                   --                                    , text "orig_lhs:" <+> ppr orig_lhs
                   --                                    , text "rhs_fvs:" <+> ppr rhs_fvs
                   --                                    , text "orig_lhs:" <+> ppr orig_lhs
                   --                                    , text "lhs1:" <+> ppr lhs1
                   --                                    , text "lhs2:" <+> ppr lhs2
                   --                                    , text "fun2:" <+> ppr fun2
                   --                                    , text "args2:" <+> ppr args2
                   --                                    ]) $
                   Left (DsRuleLhsTooComplicated orig_lhs lhs2)
        Just (fn_id, args)
          | not (null unbound) ->
            -- Check for things unbound on LHS
            -- See Note [Unused spec binders]
            -- pprTrace "decomposeRuleLhs 1" (vcat [ text "orig_bndrs:" <+> ppr orig_bndrs
            --                                     , text "orig_lhs:" <+> ppr orig_lhs
            --                                     , text "lhs_fvs:" <+> ppr lhs_fvs
            --                                     , text "rhs_fvs:" <+> ppr rhs_fvs
            --                                     , text "unbound:" <+> ppr unbound
            --                                     ]) $
            Left (DsRuleBindersNotBound unbound orig_bndrs orig_lhs lhs2)
          | otherwise ->
            -- pprTrace "decomposeRuleLhs 2" (vcat [ text "orig_bndrs:" <+> ppr orig_bndrs
            --                                    , text "orig_lhs:" <+> ppr orig_lhs
            --                                    , text "lhs1:"     <+> ppr lhs1
            --                                    , text "extra_bndrs:" <+> ppr extra_bndrs
            --                                    , text "fn_id:" <+> ppr fn_id
            --                                    , text "args:"   <+> ppr args
            --                                    , text "args fvs:" <+> ppr (exprsFreeVarsList args)
            --                                    ]) $
            Right (trimmed_bndrs ++ extra_bndrs, fn_id, args)

          where -- See Note [Variables unbound on the LHS]
                lhs_fvs = exprsFreeVars args
                all_fvs       = lhs_fvs `unionVarSet` rhs_fvs
                trimmed_bndrs = filter (`elemVarSet` all_fvs) orig_bndrs
                unbound       = filterOut (`elemVarSet` lhs_fvs) trimmed_bndrs
                    -- Needed on RHS but not bound on LHS

                -- Add extra tyvar binders: Note [Free tyvars on rule LHS]
                -- and extra dict binders: Note [Free dictionaries on rule LHS]
                extra_bndrs = scopedSort extra_tvs ++ extra_dicts
                  where
                    extra_tvs   = [ v | v <- extra_vars, isTyVar v ]

                -- isEvVar: this includes coercions, matching what
                --          happens in `split_lets` (isDictId, isCoVar)
                extra_dicts =
                  [ mkLocalIdOrCoVar (localiseName (idName d)) ManyTy (idType d)
                    | d <- extra_vars, isEvVar d ]
                extra_vars  =
                  [ v
                  | v <- exprsFreeVarsList args
                  , not (v `elemVarSet` orig_bndr_set)
                  , not (v == fn_id) ]
                    -- fn_id: do not quantify over the function itself, which may
                    -- itself be a dictionary (in pathological cases, #10251)

 where
   simpl_opts    = initSimpleOpts dflags
   orig_bndr_set = mkVarSet orig_bndrs

   lhs1         = drop_dicts orig_lhs
   lhs2         = simpleOptExpr simpl_opts lhs1  -- See Note [Simplify rule LHS]
   (fun2,args2) = collectArgs lhs2

   decompose (Var fn_id) args
      | not (fn_id `elemVarSet` orig_bndr_set)
      = Just (fn_id, args)

   decompose _ _ = Nothing

   drop_dicts :: CoreExpr -> CoreExpr
   drop_dicts e
       = wrap_lets needed bnds body
     where
       needed = orig_bndr_set `minusVarSet` exprFreeVars body
       (bnds, body) = split_lets (occurAnalyseExpr e)
           -- The occurAnalyseExpr drops dead bindings which is
           -- crucial to ensure that every binding is used later;
           -- which in turn makes wrap_lets work right

   split_lets :: CoreExpr -> ([(DictId,CoreExpr)], CoreExpr)
   split_lets (Let (NonRec d r) body)
     | isDictId d  -- Catches dictionaries, yes, but also catches dictionary
                   -- /functions/ arising from solving a
                   -- quantified contraint (#24370)
     = ((d,r):bs, body')
     where (bs, body') = split_lets body

    -- handle "unlifted lets" too, needed for "map/coerce"
   split_lets (Case r d _ [Alt DEFAULT _ body])
     | isCoVar d
     = ((d,r):bs, body')
     where (bs, body') = split_lets body

   split_lets e = ([], e)

   wrap_lets :: VarSet -> [(DictId,CoreExpr)] -> CoreExpr -> CoreExpr
   wrap_lets _ [] body = body
   wrap_lets needed ((d, r) : bs) body
     | rhs_fvs `intersectsVarSet` needed = mkCoreLet (NonRec d r) (wrap_lets needed' bs body)
     | otherwise                         = wrap_lets needed bs body
     where
       rhs_fvs = exprFreeVars r
       needed' = (needed `minusVarSet` rhs_fvs) `extendVarSet` d

{-
Note [Variables unbound on the LHS]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We obviously want to complain about
   RULE   forall x. f True = not x
because the forall'd variable `x` is not bound on the LHS.

It can be a bit delicate when dictionaries are involved.
Consider #22471
  {-# RULES "foo" forall (f :: forall a. [a] -> Int).
                  foo (\xs. 1 + f xs) = 2 + foo f #-}

We get two dicts on the LHS, one from `1` and one from `+`.
For reasons described in Note [The SimplifyRule Plan] in
GHC.Tc.Gen.Rule, we quantify separately over those dictionaries:
   forall f (d1::Num Int) (d2 :: Num Int).
   foo (\xs. (+) d1 (fromInteger d2 1) xs) = ...

Now the desugarer shortcircuits (fromInteger d2 1) to (I# 1); so d2 is
not mentioned at all (on LHS or RHS)! We don't want to complain about
and unbound d2.  Hence the trimmed_bndrs.

Note [Decomposing the left-hand side of a RULE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are several things going on here.
* drop_dicts: see Note [Drop dictionary bindings on rule LHS]
* simpleOptExpr: see Note [Simplify rule LHS]
* extra_dict_bndrs: see Note [Free dictionaries on rule LHS]

Note [Free tyvars on rule LHS]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data T a = C

  foo :: T a -> Int
  foo C = 1

  {-# RULES "myrule"  foo C = 1 #-}

After type checking the LHS becomes (foo alpha (C alpha)), where alpha
is an unbound meta-tyvar.  The zonker in GHC.Tc.Zonk.Type is careful not to
turn the free alpha into Any (as it usually does).  Instead it turns it
into a TyVar 'a'.  See Note [Zonking the LHS of a RULE] in "GHC.Tc.Zonk.Type".

Now we must quantify over that 'a'.  It's /really/ inconvenient to do that
in the zonker, because the HsExpr data type is very large.  But it's /easy/
to do it here in the desugarer.

Moreover, we have to do something rather similar for dictionaries;
see Note [Free dictionaries on rule LHS].   So that's why we look for
type variables free on the LHS, and quantify over them.

This relies on there not being any in-scope tyvars, which is true for
user-defined RULEs, which are always top-level.

Note [Free dictionaries on rule LHS]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the LHS of a specialisation rule, (/\as\ds. f es) has a free dict,
which is presumably in scope at the function definition site, we can quantify
over it too.  *Any* dict with that type will do.

So for example when you have
        f :: Eq a => a -> a
        f = <rhs>
        ... SPECIALISE f :: Int -> Int ...

Then we get the SpecPrag
        SpecPrag (f Int dInt)

And from that we want the rule

        RULE forall dInt. f Int dInt = f_spec
        f_spec = let f = <rhs> in f Int dInt

But be careful!  That dInt might be GHC.Base.$fOrdInt, which is an External
Name, and you can't bind them in a lambda or forall without getting things
confused.   Likewise it might have a stable unfolding or something, which would be
utterly bogus. So we really make a fresh Id, with the same unique and type
as the old one, but with an Internal name and no IdInfo.

Note [Drop dictionary bindings on rule LHS]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
drop_dicts drops dictionary bindings on the LHS where possible.
   E.g.  let d:Eq [Int] = $fEqList $fEqInt in f d
     --> f d
   Reasoning here is that there is only one d:Eq [Int], and so we can
   quantify over it. That makes 'd' free in the LHS, but that is later
   picked up by extra_dict_bndrs (see Note [Unused spec binders]).

   NB 1: We can only drop the binding if the RHS doesn't bind
         one of the orig_bndrs, which we assume occur on RHS.
         Example
            f :: (Eq a) => b -> a -> a
            {-# SPECIALISE f :: Eq a => b -> [a] -> [a] #-}
         Here we want to end up with
            RULE forall d:Eq a.  f ($dfEqList d) = f_spec d
         Of course, the ($dfEqlist d) in the pattern makes it less likely
         to match, but there is no other way to get d:Eq a

   NB 2: We do drop_dicts *before* simplOptEpxr, so that we expect all
         the evidence bindings to be wrapped around the outside of the
         LHS.  (After simplOptExpr they'll usually have been inlined.)
         dsHsWrapper does dependency analysis, so that civilised ones
         will be simple NonRec bindings.  We don't handle recursive
         dictionaries!

    NB3: In the common case of a non-overloaded, but perhaps-polymorphic
         specialisation, we don't need to bind *any* dictionaries for use
         in the RHS. For example (#8331)
             {-# SPECIALIZE INLINE useAbstractMonad :: ReaderST s Int #-}
             useAbstractMonad :: MonadAbstractIOST m => m Int
         Here, deriving (MonadAbstractIOST (ReaderST s)) is a lot of code
         but the RHS uses no dictionaries, so we want to end up with
             RULE forall s (d :: MonadAbstractIOST (ReaderT s)).
                useAbstractMonad (ReaderT s) d = $suseAbstractMonad s

   #8848 is a good example of where there are some interesting
   dictionary bindings to discard.

The drop_dicts algorithm is based on these observations:

  * Given (let d = rhs in e) where d is a DictId,
    matching 'e' will bind e's free variables.

  * So we want to keep the binding if one of the needed variables (for
    which we need a binding) is in fv(rhs) but not already in fv(e).

  * The "needed variables" are simply the orig_bndrs.  Consider
       f :: (Eq a, Show b) => a -> b -> String
       ... SPECIALISE f :: (Show b) => Int -> b -> String ...
    Then orig_bndrs includes the *quantified* dictionaries of the type
    namely (dsb::Show b), but not the one for Eq Int

So we work inside out, applying the above criterion at each step.


Note [Simplify rule LHS]
~~~~~~~~~~~~~~~~~~~~~~~~
simplOptExpr occurrence-analyses and simplifies the LHS:

   (a) Inline any remaining dictionary bindings (which hopefully
       occur just once)

   (b) Substitute trivial lets, so that they don't get in the way.
       Note that we substitute the function too; we might
       have this as a LHS:  let f71 = M.f Int in f71

   (c) Do eta reduction.  To see why, consider the fold/build rule,
       which without simplification looked like:
          fold k z (build (/\a. g a))  ==>  ...
       This doesn't match unless you do eta reduction on the build argument.
       Similarly for a LHS like
         augment g (build h)
       we do not want to get
         augment (\a. g a) (build h)
       otherwise we don't match when given an argument like
          augment (\a. h a a) (build h)

Note [Unused spec binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
        f :: a -> a
        ... SPECIALISE f :: Eq a => a -> a ...
It's true that this *is* a more specialised type, but the rule
we get is something like this:
        f_spec d = f
        RULE: f = f_spec d
Note that the rule is bogus, because it mentions a 'd' that is
not bound on the LHS!  But it's a silly specialisation anyway, because
the constraint is unused.  We could bind 'd' to (error "unused")
but it seems better to reject the program because it's almost certainly
a mistake.  That's what the isDeadBinder call detects.

Note [No RULES on datacons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Previously, `RULES` like

    "JustNothing" forall x . Just x = Nothing

were allowed. Simon Peyton Jones says this seems to have been a
mistake, that such rules have never been supported intentionally,
and that he doesn't know if they can break in horrible ways.
Furthermore, Ben Gamari and Reid Barton are considering trying to
detect the presence of "static data" that the simplifier doesn't
need to traverse at all. Such rules do not play well with that.
So for now, we ban them altogether as requested by #13290. See also #7398.


************************************************************************
*                                                                      *
                Desugaring evidence
*                                                                      *
************************************************************************

Note [Desugaring WpFun]
~~~~~~~~~~~~~~~~~~~~~~~
See comments on WpFun in GHC.Tc.Types.Evidence for what WpFun means.
Roughly:

  (WpFun w_arg w_res)[ e ] = \x. w_res[ e w_arg[x] ]

This eta-expansion risk duplicating work, if `e` is not in HNF.
At one stage I thought we could avoid that by desugaring to
      let f = e in \x. w_res[ f w_arg[x] ]
But that /fundamentally/ doesn't work, because `w_res` may bind
evidence that is used in `e`.

This question arose when thinking about deep subsumption; see
https://github.com/ghc-proposals/ghc-proposals/pull/287#issuecomment-1125419649).
-}

dsHsWrappers :: [HsWrapper] -> ([CoreExpr -> CoreExpr] -> DsM a) -> DsM a
dsHsWrappers (wp:wps) k = dsHsWrapper wp $ \wrap -> dsHsWrappers wps $ \wraps -> k (wrap:wraps)
dsHsWrappers []       k = k []

dsHsWrapper :: HsWrapper -> ((CoreExpr -> CoreExpr) -> DsM a) -> DsM a
dsHsWrapper hs_wrap thing_inside
  = ds_hs_wrapper hs_wrap $ \ core_wrap ->
    addTyCs FromSource (hsWrapDictBinders hs_wrap) $
       -- addTyCs: Add type evidence to the refinement type
       --            predicate of the coverage checker
       --   See Note [Long-distance information] in "GHC.HsToCore.Pmc"
       -- FromSource might not be accurate (we don't have any
       -- origin annotations for things in this module), but at
       -- worst we do superfluous calls to the pattern match
       -- oracle.
    thing_inside core_wrap

ds_hs_wrapper :: HsWrapper
              -> ((CoreExpr -> CoreExpr) -> DsM a)
              -> DsM a
ds_hs_wrapper wrap = go wrap
  where
    go WpHole            k = k $ \e -> e
    go (WpTyApp ty)      k = k $ \e -> App e (Type ty)
    go (WpEvLam ev)      k = k $ Lam ev
    go (WpTyLam tv)      k = k $ Lam tv
    go (WpCast co)       k = assert (coercionRole co == Representational) $
                             k $ \e -> mkCastDs e co
    go (WpEvApp tm)      k = do { core_tm <- dsEvTerm tm
                                ; k $ \e -> e `App` core_tm }
    go (WpLet ev_binds)  k = dsTcEvBinds ev_binds $ \bs ->
                             k (mkCoreLets bs)
    go (WpCompose c1 c2) k = go c1 $ \w1 ->
                             go c2 $ \w2 ->
                             k (w1 . w2)
    go (WpFun c1 c2 st)  k = -- See Note [Desugaring WpFun]
                             do { x <- newSysLocalDs st
                                ; go c1 $ \w1 ->
                                  go c2 $ \w2 ->
                                  let app f a = mkCoreApp (text "dsHsWrapper") f a
                                      arg     = w1 (Var x)
                                  in k (\e -> (Lam x (w2 (app e arg)))) }

--------------------------------------
dsTcEvBinds_s :: [TcEvBinds] -> ([CoreBind] -> DsM a) -> DsM a
dsTcEvBinds_s []       k = k []
dsTcEvBinds_s (b:rest) k = assert (null rest) $  -- Zonker ensures null
                           dsTcEvBinds b k

dsTcEvBinds :: TcEvBinds -> ([CoreBind] -> DsM a) -> DsM a
dsTcEvBinds (TcEvBinds {}) = panic "dsEvBinds"    -- Zonker has got rid of this
dsTcEvBinds (EvBinds bs)   = dsEvBinds bs

--   * Desugars the ev_binds, sorts them into dependency order, and
--     passes the resulting [CoreBind] to thing_inside
--   * Extends the DsM (dsl_unspecable field) with specialisability information
--     for each binder in ev_binds, before invoking thing_inside
dsEvBinds :: Bag EvBind -> ([CoreBind] -> DsM a) -> DsM a
dsEvBinds ev_binds thing_inside
  = do { ds_binds <- mapBagM dsEvBind ev_binds
       ; let comps = sort_ev_binds ds_binds
       ; go comps thing_inside }
  where
    go ::[SCC (Node EvVar (CanonicalEvidence, CoreExpr))] -> ([CoreBind] -> DsM a) -> DsM a
    go (comp:comps) thing_inside
      = do { unspecables <- getUnspecables
           ; let (core_bind, new_unspecables) = ds_component unspecables comp
           ; addUnspecables new_unspecables $ go comps $ \ core_binds ->
               thing_inside (core_bind:core_binds) }
    go [] thing_inside = thing_inside []

    ds_component unspecables (AcyclicSCC node) = (NonRec v rhs, new_unspecables)
      where
        ((v, rhs), (this_canonical, deps)) = unpack_node node
        transitively_unspecable = is_unspecable this_canonical || any is_unspecable_dep deps
        is_unspecable_dep dep = dep `S.member` unspecables
        new_unspecables
            | transitively_unspecable = S.singleton v
            | otherwise = mempty
    ds_component unspecables (NECyclicSCC nodes) = (Rec pairs, new_unspecables)
      where
        (pairs, direct_canonicity) = unzip $ map unpack_node $ NE.toList nodes

        is_unspecable_remote dep = dep `S.member` unspecables
        transitively_unspecable = or [ is_unspecable this_canonical || any is_unspecable_remote deps
                                     | (this_canonical, deps) <- direct_canonicity ]
            -- Bindings from a given SCC are transitively specialisable if
            -- all are specialisable and all their remote dependencies are
            -- also specialisable; see Note [Desugaring non-canonical evidence]

        new_unspecables
            | transitively_unspecable = S.fromList [ v | (v, _) <- pairs]
            | otherwise = mempty

    unpack_node DigraphNode { node_key = v, node_payload = (canonical, rhs), node_dependencies = deps }
       = ((v, rhs), (canonical, deps))

    is_unspecable :: CanonicalEvidence -> Bool
    is_unspecable EvNonCanonical = True
    is_unspecable EvCanonical    = False

sort_ev_binds :: Bag (Id, CanonicalEvidence, CoreExpr) -> [SCC (Node EvVar (CanonicalEvidence, CoreExpr))]
-- We do SCC analysis of the evidence bindings, /after/ desugaring
-- them. This is convenient: it means we can use the GHC.Core
-- free-variable functions rather than having to do accurate free vars
-- for EvTerm.
sort_ev_binds ds_binds = stronglyConnCompFromEdgedVerticesUniqR edges
  where
    edges :: [ Node EvVar (CanonicalEvidence, CoreExpr) ]
    edges = foldr ((:) . mk_node) [] ds_binds

    mk_node :: (Id, CanonicalEvidence, CoreExpr) -> Node EvVar (CanonicalEvidence, CoreExpr)
    mk_node (var, canonical, rhs)
      = DigraphNode { node_payload = (canonical, rhs)
                    , node_key = var
                    , node_dependencies = nonDetEltsUniqSet $
                                          exprFreeVars rhs `unionVarSet`
                                          coVarsOfType (varType var) }
      -- It's OK to use nonDetEltsUniqSet here as graphFromEdgedVerticesUniq
      -- is still deterministic even if the edges are in nondeterministic order
      -- as explained in Note [Deterministic SCC] in GHC.Data.Graph.Directed.

dsEvBind :: EvBind -> DsM (Id, CanonicalEvidence, CoreExpr)
dsEvBind (EvBind { eb_lhs = v, eb_rhs = r, eb_info = info }) = do
    e <- dsEvTerm r
    let canonical = case info of
            EvBindGiven{} -> EvCanonical
            EvBindWanted{ ebi_canonical = canonical } -> canonical
    return (v, canonical, e)


{-**********************************************************************
*                                                                      *
           Desugaring EvTerms
*                                                                      *
**********************************************************************-}

dsEvTerm :: EvTerm -> DsM CoreExpr
dsEvTerm (EvExpr e)          = return e
dsEvTerm (EvTypeable ty ev)  = dsEvTypeable ty ev
dsEvTerm (EvFun { et_tvs = tvs, et_given = given
                , et_binds = ev_binds, et_body = wanted_id })
  = do { dsTcEvBinds ev_binds $ \ds_ev_binds -> do
       { return $ (mkLams (tvs ++ given) $
                   mkCoreLets ds_ev_binds $
                   Var wanted_id) } }


{-**********************************************************************
*                                                                      *
           Desugaring Typeable dictionaries
*                                                                      *
**********************************************************************-}

dsEvTypeable :: Type -> EvTypeable -> DsM CoreExpr
-- Return a CoreExpr :: Typeable ty
-- This code is tightly coupled to the representation
-- of TypeRep, in base library Data.Typeable.Internal
dsEvTypeable ty ev
  = do { tyCl <- dsLookupTyCon typeableClassName    -- Typeable
       ; let kind = typeKind ty
             typeable_data_con = tyConSingleDataCon tyCl  -- "Data constructor"
                                                    -- for Typeable

       ; rep_expr <- ds_ev_typeable ty ev           -- :: TypeRep a

       -- Package up the method as `Typeable` dictionary
       ; return $ mkConApp typeable_data_con [Type kind, Type ty, rep_expr] }

type TypeRepExpr = CoreExpr

-- | Returns a @CoreExpr :: TypeRep ty@
ds_ev_typeable :: Type -> EvTypeable -> DsM CoreExpr
ds_ev_typeable ty (EvTypeableTyCon tc kind_ev)
  = do { mkTrCon <- dsLookupGlobalId mkTrConName
                    -- mkTrCon :: forall k (a :: k). TyCon -> TypeRep k -> TypeRep a
       ; someTypeRepTyCon <- dsLookupTyCon someTypeRepTyConName
       ; someTypeRepDataCon <- dsLookupDataCon someTypeRepDataConName
                    -- SomeTypeRep :: forall k (a :: k). TypeRep a -> SomeTypeRep

       ; tc_rep <- tyConRep tc                      -- :: TyCon
       ; let ks = tyConAppArgs ty
             -- Construct a SomeTypeRep
             toSomeTypeRep :: Type -> EvTerm -> DsM CoreExpr
             toSomeTypeRep t ev = do
                 rep <- getRep ev t
                 return $ mkCoreConApps someTypeRepDataCon [Type (typeKind t), Type t, rep]
       ; kind_arg_reps <- sequence $ zipWith toSomeTypeRep ks kind_ev   -- :: TypeRep t
       ; let -- :: [SomeTypeRep]
             kind_args = mkListExpr (mkTyConTy someTypeRepTyCon) kind_arg_reps

         -- Note that we use the kind of the type, not the TyCon from which it
         -- is constructed since the latter may be kind polymorphic whereas the
         -- former we know is not (we checked in the solver).
       ; let expr = mkApps (Var mkTrCon) [ Type (typeKind ty)
                                         , Type ty
                                         , tc_rep
                                         , kind_args ]
       -- ; pprRuntimeTrace "Trace mkTrTyCon" (ppr expr) expr
       ; return expr
       }

ds_ev_typeable ty (EvTypeableTyApp ev1 ev2)
  | Just (t1,t2) <- splitAppTy_maybe ty
  = do { e1  <- getRep ev1 t1
       ; e2  <- getRep ev2 t2
       ; mkTrApp <- dsLookupGlobalId mkTrAppName
                    -- mkTrApp :: forall k1 k2 (a :: k1 -> k2) (b :: k1).
                    --            TypeRep a -> TypeRep b -> TypeRep (a b)
       ; let (_, k1, k2) = splitFunTy (typeKind t1)  -- drop the multiplicity,
                                                     -- since it's a kind
       ; let expr =  mkApps (mkTyApps (Var mkTrApp) [ k1, k2, t1, t2 ])
                            [ e1, e2 ]
       -- ; pprRuntimeTrace "Trace mkTrApp" (ppr expr) expr
       ; return expr
       }

ds_ev_typeable ty (EvTypeableTrFun evm ev1 ev2)
  | Just (_af,m,t1,t2) <- splitFunTy_maybe ty
  = do { e1 <- getRep ev1 t1
       ; e2 <- getRep ev2 t2
       ; em <- getRep evm m
       ; mkTrFun <- dsLookupGlobalId mkTrFunName
                    -- mkTrFun :: forall (m :: Multiplicity) r1 r2 (a :: TYPE r1) (b :: TYPE r2).
                    --            TypeRep m -> TypeRep a -> TypeRep b -> TypeRep (a % m -> b)
       ; let r1 = getRuntimeRep t1
             r2 = getRuntimeRep t2
       ; return $ mkApps (mkTyApps (Var mkTrFun) [m, r1, r2, t1, t2])
                         [ em, e1, e2 ]
       }

ds_ev_typeable ty (EvTypeableTyLit ev)
  = -- See Note [Typeable for Nat and Symbol] in GHC.Tc.Instance.Class
    do { fun  <- dsLookupGlobalId tr_fun
       ; dict <- dsEvTerm ev       -- Of type KnownNat/KnownSymbol
       ; return (mkApps (mkTyApps (Var fun) [ty]) [ dict ]) }
  where
    ty_kind = typeKind ty

    -- tr_fun is the Name of
    --       typeNatTypeRep    :: KnownNat    a => TypeRep a
    -- of    typeSymbolTypeRep :: KnownSymbol a => TypeRep a
    tr_fun | ty_kind `eqType` naturalTy      = typeNatTypeRepName
           | ty_kind `eqType` typeSymbolKind = typeSymbolTypeRepName
           | ty_kind `eqType` charTy         = typeCharTypeRepName
           | otherwise = panic "dsEvTypeable: unknown type lit kind"

ds_ev_typeable ty ev
  = pprPanic "dsEvTypeable" (ppr ty $$ ppr ev)

getRep :: EvTerm          -- ^ EvTerm for @Typeable ty@
       -> Type            -- ^ The type @ty@
       -> DsM TypeRepExpr -- ^ Return @CoreExpr :: TypeRep ty@
                          -- namely @typeRep# dict@
-- Remember that
--   typeRep# :: forall k (a::k). Typeable k a -> TypeRep a
getRep ev ty
  = do { typeable_expr <- dsEvTerm ev
       ; typeRepId     <- dsLookupGlobalId typeRepIdName
       ; let ty_args = [typeKind ty, ty]
       ; return (mkApps (mkTyApps (Var typeRepId) ty_args) [ typeable_expr ]) }

tyConRep :: TyCon -> DsM CoreExpr
-- Returns CoreExpr :: TyCon
tyConRep tc
  | Just tc_rep_nm <- tyConRepName_maybe tc
  = do { tc_rep_id <- dsLookupGlobalId tc_rep_nm
       ; return (Var tc_rep_id) }
  | otherwise
  = pprPanic "tyConRep" (ppr tc)
