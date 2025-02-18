{-# LANGUAGE TypeFamilies, DataKinds, GADTs, FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
 -- To permit: type instance XLet 'InferTaggedBinders = XLet 'SomePass

{-# OPTIONS_GHC -Wname-shadowing #-}
module GHC.Stg.EnforceEpt ( enforceEpt ) where

import GHC.Prelude hiding (id)

import GHC.Core.DataCon
import GHC.Core.Type
import GHC.Types.Id
import GHC.Types.Id.Info (tagSigInfo)
import GHC.Types.Name
import GHC.Stg.Syntax
import GHC.Types.Basic ( CbvMark (..) )
import GHC.Types.Demand (isDeadEndAppSig)
import GHC.Types.Unique.Supply (mkSplitUniqSupply)
import GHC.Types.RepType (dataConRuntimeRepStrictness)
import GHC.Core (AltCon(..))
import Data.List (mapAccumL)
import GHC.Utils.Outputable
import GHC.Utils.Misc( zipWithEqual, zipEqual, notNull )

import GHC.Stg.EnforceEpt.Types
import GHC.Stg.EnforceEpt.Rewrite (rewriteTopBinds)
import Data.Maybe
import GHC.Types.Name.Env (mkNameEnv, NameEnv)
import GHC.Driver.DynFlags
import GHC.Utils.Logger
import qualified GHC.Unit.Types

{- Note [Evaluated and Properly Tagged]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A pointer is Evaluated and Properly Tagged (EPT) when the pointer

  (a) points directly to the value (not to an indirection, and not to a thunk)
  (b) is tagged with the tag corresponding to said value (e.g. constructor tag
      or arity of a function).

A binder is EPT when all the runtime pointers it binds are EPT.

Note that a lifted EPT pointer will never point to a thunk, nor will it be
tagged `000` (meaning "might be a thunk").

See https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/haskell-execution/pointer-tagging
for more information on pointer tagging.

Examples:
* Case binders are always EPT; hence an eval
    case x of x' { __DEFAULT -> ... }
  ensures that x' is EPT even if x was not.
* Data constructor bindings
    let x = Just y in ...
  are EPT: x will point to the heap-allocated constructor closure for (Just y),
  and the tag-bits of the pointer will encode the tag for Just (i.e. `010`).
* In practice, GHC also guarantees that strict fields (and others) are EPT;
  see Note [EPT enforcement].

Caveat:
Currently, the proper tag for builtin *unlifted* data types such as `Array#` is
not `001` but `000`, which is not a proper tag for lifted data.
This means that UnliftedRep is not a proper sub-rep of LiftedRep.
SG thinks it would be good to fix this; see #21792.

Note [EPT enforcement]
~~~~~~~~~~~~~~~~~~~~~~
The goal of EnforceEPT pass is to mark as many binders as possible as EPT
(see Note [Evaluated and Properly Tagged]).
To find more EPT binders, it establishes the following

EPT INVARIANT:
> Any binder of
>   * a strict field (see Note [Strict fields in Core]), or
>   * a CBV argument (see Note [CBV Function Ids])
> is EPT.

(Note that prior to EPT enforcement, this invariant may *not* always be upheld.
An example can be found at the end of this Note.)
This is all to optimise code such as the following:

  data SPair a b = SP !a !b
  case p :: SP Bool Bool of
    SP x y ->
      case x of
        True  -> ...
        False -> ...

We can infer that the strict field x is EPT and hence may safely
omit the code to enter x and the check for the presence of a tag that goes along
with it. However we still branch on the tag as usual to jump to the True or
False case.

Note that for every example involving strict fields we could find a similar
example using CBV functions, e.g.

  $wf x[EPT] y =
    case x of
      True  -> ...
      False -> ...

is the above example translated to use a CBV function $wf.
Note that /any/ strict function can in principle be chosen as a CBV function;
however, we presently only promote worker functions such as $wf to CBV because
we see all its call sites and can use the proper by-value calling convention.
More precisely, with -O0, we guarantee that no CBV functions are visible in
the interface file, so that naïve clients do not need to know how to call CBV
functions. See Note [CBV Function Ids] for more details.

Specification
-------------
EPT enforcement works like implicit type conversions in C, such as from int to
float, only much simpler (no overloaded operations such as +).
For EPT enforcement, the "type system" in question is whether a binder is
statically EPT. We differentiate "EPT binder" from "non-EPT binder", where the
latter means "might be EPT, but we could not prove it so".
In this sense, EPT binders form a subtype of non-EPT binders.
We differentiate two conversion directions:

  * Downcast: EPT binders can be converted into non-EPT binders for free.
  * Upcast: non-EPT binders can be converted into EPT binders by inserting an eval.

The EPT invariant expresses type signatures. In particular, these type
signatures entail two things:

  * A _precondition_: Any binder that is passed as a CBV arg/strict field
    must be EPT (i.e. must have type "EPT binder").
  * A _postcondition_: Any binder of a CBV arg/strict field is EPT.

EPT enforcement is then simply a matter of figuring out where to insert
Upcasts (remember that Downcasts are free).
Since Upcasts (evals!) are not free, it is desirable to insert as few as possible.
To this end, we run a static *EPT analysis*, the purpose of which is to identify
as many EPT binders as possible.
Beyond discovering case binders and value bindings, EPT analysis exploits the
type signatures provided by the EPT invariant, looks inside returned tuples and
does some limited amount of fixpointing.
Afterwards, the *EPT rewriter* inserts the actual evals realising Upcasts.

Implementation
--------------

* EPT analysis is implemented in GHC.Stg.EnforceEpt.inferTags.
  It attaches its result to /binders/, not occurrence sites.
* The EPT rewriter establishes the EPT invariant by inserting evals. That is, if
    (a) a binder x is used to
          * construct a strict field (`SP x y`), or
          * passed as a CBV argument (`$wf x`),
        and
    (b) x was not inferred EPT,
  then the EPT rewriter inserts an eval prior to the call, e.g.
    case x of x' { __ DEFAULT -> SP x' y }.
    case x of x' { __ DEFAULT -> $wf x' }.
  (Recall that the case binder x' is always EPT.)
  This is implemented in GHC.Stg.EnforceEpt.Rewrite.rewriteTopBinds.
  This pass also propagates the EPTness from binders to occurrences.
  It is sound to insert evals on strict fields (Note [Strict fields in Core]),
  and on CBV arguments as well (Note [CBV Function Ids]).
* We also export the EPTness of top level bindings to allow this optimisation
  to work across module boundaries.
  NB: The EPT Invariant *must* be upheld, regardless of the optimisation level;
  hence EPTness is practically part of the internal ABI of a strict data
  constructor or CBV function. Note [CBV Function Ids] contains the details.
* Finally, code generation skips the thunk check when branching on binders that
  are EPT. This is done by `cgExpr`/`cgCase` in the backend.

Evaluation
----------
EPT enforcement can have large impact on spine-strict tree data structure
performance. For containers the reduction in runtimes with this optimization
was as follows:

intmap-benchmarks:    89.30%
intset-benchmarks:    90.87%
map-benchmarks:       88.00%
sequence-benchmarks:  99.84%
set-benchmarks:       85.00%
set-operations-intmap:88.64%
set-operations-map:   74.23%
set-operations-set:   76.50%
lookupge-intmap:      89.57%
lookupge-map:         70.95%

With nofib being ~0.3% faster as well.

Note that EPT enforcement may cause regressions in rare cases.
For example consider this code:

  foo x = ...
    let c = StrictJust x
    in ...

When x cannot be inferred EPT, the rewriter transforms to

  foo x = ...
    let c = case x of x' -> StrictJust x'
    in ...

which allocates an additional thunk for `c` that returns the constructor.  Boo!

Note [EPT enforcement lowers strict constructor worker semantics]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In Core, a saturated application of a strict constructor worker evaluates its
strict fields and thus is *not* a value; see Note [Strict fields in Core].
This is also the semantics of strict constructor workers in STG *before* EPT
enforcement (see Note [EPT enforcement])

However, after enforcing the EPT Invariant, all constructor workers can
effectively be lazy. That is, when actually generating code to allocate the
data constructor, the code generator does not need to evaluate the argument;
that has already been done by the EPT pass.

Thus for code-gen reasons (StgToX), all constructor workers are considered lazy
after EPT enforcement.

Note [Why isn't the EPT Invariant enforced during Core passes?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Recall the definition of the EPT Invariant from Note [EPT enforcement].
Why can't it be established as an invariant right while desugaring to Core?
The reason is that some Core optimisations, such as FloatOut, will drop or delay
evals whenever they think it useful and thus destroy the Invariant.  Example:

  data Set a = Tip | Bin !a (Set a) (Set a)

We start with

  thk = f ()
  g x = ...(case thk of xv -> Bin xv Tip Tip)...

So far so good; the argument to Bin (which is strict) is evaluated.
Now we do float-out. And in doing so we do a reverse binder-swap (see
Note [Binder-swap during float-out] in SetLevels) thus

  g x = ...(case thk of xv -> Bin thk Nil Nil)...

The goal of the reverse binder-swap is to allow more floating -- and
indeed it does! We float the Bin to top level:

  lvl = Bin thk Tip Tip
  g x = ...(case thk of xv -> lvl)...

Now you can see that the argument of Bin, namely thk, points to the
thunk, not to the value as it did before.

In short, although it may be rare, the output of Core optimisation passes
might destroy the EPT Invariant, hence we need to enforce the EPT invariant
*after* passes such as FloatOut.
-}

{- Note [TagInfo of functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The purpose of tag inference is really to figure out when we don't have to enter
value closures. There the meaning of the tag is fairly obvious.
For functions we never make use of the tag info so we have two choices:
* Treat them as TagDunno
* Treat them as TagProper (as they *are* tagged with their arity) and be really
  careful to make sure we still enter them when needed.
As it makes little difference for runtime performance I've treated functions as TagDunno in a few places where
it made the code simpler. But besides implementation complexity there isn't any reason
why we couldn't be more rigorous in dealing with functions.

NB: It turned in #21193 that PAPs get tag zero, so the tag check can't be omitted for functions.
So option two isn't really an option without reworking this anyway.

Note [EPT enforcement debugging]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is a flag -dtag-inference-checks which inserts various
compile/runtime checks in order to ensure the EPT Invariant
holds. It should cover all places
where tags matter and disable optimizations which interfere with checking
the invariant like generation of AP-Thunks.

Note [Polymorphic StgPass for inferTagExpr]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In order to reach a fixpoint we sometimes have to re-analyse an expression
multiple times. But after the initial run the Ast will be parameterized by
a different StgPass! To handle this a large part of the analysis is polymorphic
over the exact StgPass we are using. Which allows us to run the analysis on
the output of itself.

Note [EPT enforcement for interpreted code]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The bytecode interpreter has a different behaviour when it comes
to the tagging of binders in certain situations than the StgToCmm code generator.

a) Tags for let-bindings:

  When compiling a binding for a constructor like `let x = Just True`
  Whether `x` will be properly tagged depends on the backend.
  For the interpreter x points to a BCO which once
  evaluated returns a properly tagged pointer to the heap object.
  In the Cmm backend for the same binding we would allocate the constructor right
  away and x will immediately be represented by a tagged pointer.
  This means for interpreted code we can not assume let bound constructors are
  properly tagged. Hence we distinguish between targeting bytecode and native in
  the analysis.
  We make this differentiation in `mkLetSig` where we simply never assume
  lets are tagged when targeting bytecode.

b) When referencing ids from other modules the Cmm backend will try to put a
   proper tag on these references through various means. When doing analysis we
   usually predict these cases to improve precision of the analysis.
   But to my knowledge the bytecode generator makes no such attempts so we must
   not infer imported bindings as tagged.
   This is handled in GHC.Stg.EnforceEpt.Types.lookupInfo


-}

{- *********************************************************************
*                                                                      *
                         EPT enforcement pass
*                                                                      *
********************************************************************* -}

enforceEpt :: StgPprOpts -> Bool -> Logger -> (GHC.Unit.Types.Module) -> [CgStgTopBinding] -> IO ([TgStgTopBinding], NameEnv TagSig)
enforceEpt ppr_opts !for_bytecode logger this_mod stg_binds = do
    -- pprTraceM "enforceEpt for " (ppr this_mod <> text " bytecode:" <> ppr for_bytecode)
    -- Annotate binders with tag information.
    let (!stg_binds_w_tags) = {-# SCC "StgEptInfer" #-}
                                        inferTags for_bytecode stg_binds
    putDumpFileMaybe logger Opt_D_dump_stg_tags "CodeGenAnal STG:" FormatSTG (pprGenStgTopBindings ppr_opts stg_binds_w_tags)

    let export_tag_info = collectExportInfo stg_binds_w_tags

    -- Rewrite STG to uphold the strict field invariant
    us_t <- mkSplitUniqSupply 't'
    let rewritten_binds = {-# SCC "StgEptRewrite" #-} rewriteTopBinds this_mod us_t stg_binds_w_tags :: [TgStgTopBinding]

    return (rewritten_binds,export_tag_info)

{- *********************************************************************
*                                                                      *
                         Main inference algorithm
*                                                                      *
********************************************************************* -}

type OutputableInferPass p = (Outputable (TagEnv p)
                              , Outputable (GenStgExpr p)
                              , Outputable (BinderP p)
                              , Outputable (GenStgRhs p))

-- | This constraint encodes the fact that no matter what pass
-- we use the Let/Closure extension points are the same as these for
-- 'InferTaggedBinders.
type InferExtEq i = ( XLet i ~ XLet 'InferTaggedBinders
                    , XLetNoEscape i ~ XLetNoEscape 'InferTaggedBinders
                    , XRhsClosure i ~ XRhsClosure 'InferTaggedBinders)

inferTags :: Bool -> [GenStgTopBinding 'CodeGen] -> [GenStgTopBinding 'InferTaggedBinders]
inferTags for_bytecode binds =
  -- pprTrace "Binds" (pprGenStgTopBindings shortStgPprOpts $ binds) $
  snd (mapAccumL inferTagTopBind (initEnv for_bytecode) binds)

-----------------------
inferTagTopBind :: TagEnv 'CodeGen -> GenStgTopBinding 'CodeGen
                -> (TagEnv 'CodeGen, GenStgTopBinding 'InferTaggedBinders)
inferTagTopBind env (StgTopStringLit id bs)
  = (env, StgTopStringLit id bs)
inferTagTopBind env (StgTopLifted bind)
  = (env', StgTopLifted bind')
  where
    (env', bind') = inferTagBind env bind


-- Why is this polymorphic over the StgPass? See Note [Polymorphic StgPass for inferTagExpr]
-----------------------
inferTagExpr :: forall p. (OutputableInferPass p, InferExtEq p)
  => TagEnv p -> GenStgExpr p -> (TagInfo, GenStgExpr 'InferTaggedBinders)
inferTagExpr env (StgApp fun args)
  =  --pprTrace "inferTagExpr1"
      -- (ppr fun <+> ppr args $$ ppr info $$
      --  text "deadEndInfo:" <> ppr (isDeadEndId fun, idArity fun, length args)
      -- )
    (info, StgApp fun args)
  where
    !fun_arity = idArity fun
    info
         -- It's important that we check for bottoms before all else.
         -- See Note [Bottom functions are TagTagged] and #24806 for why.
         | isDeadEndAppSig (idDmdSig fun) (length args)
         = TagTagged

         | fun_arity == 0 -- Unknown arity => Thunk or unknown call
         = TagDunno

         | Just (TagSig res_info) <- tagSigInfo (idInfo fun)
         , fun_arity == length args  -- Saturated
         = res_info

         | Just (TagSig res_info) <- lookupSig env fun
         , fun_arity == length args  -- Saturated
         = res_info

         | otherwise
         = --pprTrace "inferAppUnknown" (ppr fun) $
           TagDunno

inferTagExpr env (StgConApp con cn args tys)
  = (inferConTag env con args, StgConApp con cn args tys)

inferTagExpr _ (StgLit l)
  = (TagTagged, StgLit l)

inferTagExpr env (StgTick tick body)
  = (info, StgTick tick body')
  where
    (info, body') = inferTagExpr env body

inferTagExpr _ (StgOpApp op args ty)
  -- Which primops guarantee to return a properly tagged value?
  -- Probably none, and that is the conservative assumption anyway.
  -- (And foreign calls definitely need not make promises.)
  = (TagDunno, StgOpApp op args ty)

inferTagExpr env (StgLet ext bind body)
  = (info, StgLet ext bind' body')
  where
    (env', bind') = inferTagBind env bind
    (info, body') = inferTagExpr env' body

inferTagExpr env (StgLetNoEscape ext bind body)
  = (info, StgLetNoEscape ext bind' body')
  where
    (env', bind') = inferTagBind env bind
    (info, body') = inferTagExpr env' body

inferTagExpr in_env (StgCase scrut bndr ty alts)
  -- Unboxed tuples get their info from the expression we scrutinise if any
  | [GenStgAlt{alt_con=DataAlt con, alt_bndrs=bndrs, alt_rhs=rhs}] <- alts
  , isUnboxedTupleDataCon con
  , Just infos <- scrut_infos bndrs
  , let bndrs' = zipWithEqual mk_bndr bndrs infos
        mk_bndr :: BinderP p -> TagInfo -> (Id, TagSig)
        mk_bndr tup_bndr tup_info =
            --  pprTrace "mk_ubx_bndr_info" ( ppr bndr <+> ppr info ) $
            (getBinderId in_env tup_bndr, TagSig tup_info)
        -- no case binder in alt_env here, unboxed tuple binders are dead after unarise
        alt_env = extendSigEnv in_env bndrs'
        (info, rhs') = inferTagExpr alt_env rhs
  =
    -- pprTrace "inferCase1" (
    --   text "scrut:" <> ppr scrut $$
    --   text "bndr:" <> ppr bndr $$
    --   text "infos" <> ppr infos $$
    --   text "out_bndrs" <> ppr bndrs') $
    (info, StgCase scrut' (noSig in_env bndr) ty [GenStgAlt{ alt_con=DataAlt con
                                                           , alt_bndrs=bndrs'
                                                           , alt_rhs=rhs'}])

  | null alts -- Empty case, but I might just be paranoid.
  = -- pprTrace "inferCase2" empty $
    (TagDunno, StgCase scrut' bndr' ty [])
  -- More than one alternative OR non-TagTuple single alternative.
  | otherwise
  =
    let
        case_env = extendSigEnv in_env [bndr']

        (infos, alts')
          = unzip [ (info, g {alt_bndrs=bndrs', alt_rhs=rhs'})
                  | g@GenStgAlt{ alt_con = con
                               , alt_bndrs = bndrs
                               , alt_rhs   = rhs
                               } <- alts
                  , let (alt_env,bndrs') = addAltBndrInfo case_env con bndrs
                        (info, rhs') = inferTagExpr alt_env rhs
                  ]
        alt_info = foldr combineAltInfo TagTagged infos
    in ( alt_info, StgCase scrut' bndr' ty alts')
  where
    -- Single unboxed tuple alternative
    scrut_infos bndrs = case scrut_info of
      TagTagged -> Just $ replicate (length bndrs) TagProper
      TagTuple infos -> Just infos
      _ -> Nothing
    (scrut_info, scrut') = inferTagExpr in_env scrut
    bndr' = (getBinderId in_env bndr, TagSig TagProper)

-- Compute binder sigs based on the constructors strict fields.
-- NB: Not used if we have tuple info from the scrutinee.
addAltBndrInfo :: forall p. TagEnv p -> AltCon -> [BinderP p] -> (TagEnv p, [BinderP 'InferTaggedBinders])
addAltBndrInfo env (DataAlt con) bndrs
  | not (isUnboxedTupleDataCon con || isUnboxedSumDataCon con)
  = (out_env, out_bndrs)
  where
    marks = dataConRuntimeRepStrictness con :: [StrictnessMark]
    out_bndrs = zipWith mk_bndr bndrs marks
    out_env = extendSigEnv env out_bndrs

    mk_bndr :: (BinderP p -> StrictnessMark -> (Id, TagSig))
    mk_bndr bndr mark
      | isUnliftedType (idType id) || isMarkedStrict mark
      = (id, TagSig TagProper)
      | otherwise
      = noSig env bndr
        where
          id = getBinderId env bndr

addAltBndrInfo env _ bndrs = (env, map (noSig env) bndrs)

-----------------------------
inferTagBind :: (OutputableInferPass p, InferExtEq p)
  => TagEnv p -> GenStgBinding p -> (TagEnv p, GenStgBinding 'InferTaggedBinders)
inferTagBind in_env (StgNonRec bndr rhs)
  =
    -- pprTrace "inferBindNonRec" (
    --   ppr bndr $$
    --   ppr (isDeadEndId id) $$
    --   ppr sig)
    (env', StgNonRec (id, out_sig) rhs')
  where
    id   = getBinderId in_env bndr
    (in_sig,rhs') = inferTagRhs id in_env rhs
    out_sig = mkLetSig in_env in_sig
    env' = extendSigEnv in_env [(id, out_sig)]

inferTagBind in_env (StgRec pairs)
  = -- pprTrace "rec" (ppr (map fst pairs) $$ ppr (in_env { te_env = out_env }, StgRec pairs')) $
    (in_env { te_env = out_env }, StgRec pairs')
  where
    (bndrs, rhss)     = unzip pairs
    in_ids            = map (getBinderId in_env) bndrs
    init_sigs         = map (initSig) $ zip in_ids rhss
    (out_env, pairs') = go in_env init_sigs rhss

    go :: forall q. (OutputableInferPass q , InferExtEq q) => TagEnv q -> [TagSig] -> [GenStgRhs q]
                 -> (TagSigEnv, [((Id,TagSig), GenStgRhs 'InferTaggedBinders)])
    go go_env in_sigs go_rhss
      --   | pprTrace "go" (ppr in_ids $$ ppr in_sigs $$ ppr out_sigs $$ ppr rhss') False
      --  = undefined
       | in_sigs == out_sigs = (te_env rhs_env, out_bndrs `zip` rhss')
       | otherwise     = go env' out_sigs rhss'
       where
         in_bndrs = in_ids `zip` in_sigs
         out_bndrs = map updateBndr in_bndrs -- TODO: Keeps in_ids alive
         rhs_env = extendSigEnv go_env in_bndrs
         (out_sigs, rhss') = unzip (zipWithEqual anaRhs in_ids go_rhss)
         env' = makeTagged go_env

         anaRhs :: Id -> GenStgRhs q -> (TagSig, GenStgRhs 'InferTaggedBinders)
         anaRhs bnd rhs =
            let (sig_rhs,rhs') = inferTagRhs bnd rhs_env rhs
            in (mkLetSig go_env sig_rhs, rhs')


         updateBndr :: (Id,TagSig) -> (Id,TagSig)
         updateBndr (v,sig) = (setIdTagSig v sig, sig)

initSig :: forall p. (Id, GenStgRhs p) -> TagSig
-- Initial signature for the fixpoint loop
initSig (_bndr, StgRhsCon {})               = TagSig TagTagged
initSig (bndr, StgRhsClosure _ _ _ _ _ _) =
  fromMaybe defaultSig (idTagSig_maybe bndr)
  where defaultSig = (TagSig TagTagged)

{- Note [Bottom functions are TagTagged]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have a function with two branches with one
being bottom, and the other returning a tagged
unboxed tuple what is the result? We give it TagTagged!
To answer why consider this function:

foo :: Bool -> (# Bool, Bool #)
foo x = case x of
    True -> (# True,True #)
    False -> undefined

The true branch is obviously tagged. The other branch isn't.
We want to treat the *result* of foo as tagged as well so that
the combination of the branches also is tagged if all non-bottom
branches are tagged.
This is safe because the function is still always called/entered as long
as it's applied to arguments. Since the function will never return we can give
it safely any tag sig we like.
So we give it TagTagged, as it allows the combined tag sig of the case expression
to be the combination of all non-bottoming branches.

NB: After the analysis is done we go back to treating bottoming functions as
untagged to ensure they are evaluated as expected in code like:

  case bottom_id of { ...}

-}

-----------------------------
inferTagRhs :: forall p.
     (OutputableInferPass p, InferExtEq p)
  => Id -- ^ Id we are binding to.
  -> TagEnv p -- ^
  -> GenStgRhs p -- ^
  -> (TagSig, GenStgRhs 'InferTaggedBinders)
inferTagRhs bnd_id in_env (StgRhsClosure ext cc upd bndrs body typ)
  | isDeadEndId bnd_id && (notNull) bndrs
  -- See Note [Bottom functions are TagTagged]
  = (TagSig TagTagged, StgRhsClosure ext cc upd out_bndrs body' typ)
  | otherwise
  = --pprTrace "inferTagRhsClosure" (ppr (_top, _grp_ids, env,info')) $
    (TagSig info', StgRhsClosure ext cc upd out_bndrs body' typ)
  where
    out_bndrs
      | Just marks <- idCbvMarks_maybe bnd_id
      -- Sometimes an we eta-expand foo with additional arguments after ww, and we also trim
      -- the list of marks to the last strict entry. So we can conservatively
      -- assume these are not strict
      = zipWith (mkArgSig) bndrs (marks ++ repeat NotMarkedCbv)
      | otherwise = map (noSig env') bndrs :: [(Id,TagSig)]

    env' = extendSigEnv in_env out_bndrs
    (info, body') = inferTagExpr env' body
    info'
      -- It's a thunk
      | null bndrs
      = TagDunno
      -- TODO: We could preserve tuple fields for thunks
      -- as well. But likely not worth the complexity.

      | otherwise  = info

    mkArgSig :: BinderP p -> CbvMark -> (Id,TagSig)
    mkArgSig bndp mark =
      let id = getBinderId in_env bndp
          tag = case mark of
            MarkedCbv -> TagProper
            _
              | isUnliftedType (idType id) -> TagProper
              | otherwise -> TagDunno
      in (id, TagSig tag)

inferTagRhs _ env _rhs@(StgRhsCon cc con cn ticks args typ)
-- Constructors, which have untagged arguments to strict fields
-- become thunks. We encode this by giving changing RhsCon nodes the info TagDunno
  = --pprTrace "inferTagRhsCon" (ppr grp_ids) $
    (TagSig (inferConTag env con args), StgRhsCon cc con cn ticks args typ)

-- Adjust let semantics to the targeted backend.
-- See Note [EPT enforcement for interpreted code]
mkLetSig :: TagEnv p -> TagSig -> TagSig
mkLetSig env in_sig
  | for_bytecode = TagSig TagDunno
  | otherwise = in_sig
  where
    for_bytecode = te_bytecode env

{- Note [Constructor TagSigs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@inferConTag@ will infer the proper tag signature for a binding who's RHS is a constructor
or a StgConApp expression.
Usually these will simply be TagProper. But there are exceptions.
If any of the fields in the constructor are strict, but any argument to these
fields is not tagged then we will have to case on the argument before storing
in the constructor. Which means for let bindings the RHS turns into a thunk
which obviously is no longer properly tagged.
For example we might start with:

    let x<TagDunno> = f ...
    let c<TagProper> = StrictPair x True

But we know during the rewrite stage x will need to be evaluated in the RHS
of `c` so we will infer:

    let x<TagDunno> = f ...
    let c<TagDunno> = StrictPair x True

Which in the rewrite stage will then be rewritten into:

    let x<TagDunno> = f ...
    let c<TagDunno> = case x of x' -> StrictPair x' True

The other exception is unboxed tuples. These will get a TagTuple
signature with a list of TagInfo about their individual binders
as argument. As example:

    let c<TagProper> = True
    let x<TagDunno> = ...
    let f<?> z = case z of z'<TagProper> -> (# c, x #)

Here we will infer for f the Signature <TagTuple[TagProper,TagDunno]>.
This information will be used if we scrutinize a saturated application of
`f` in order to determine the taggedness of the result.
That is for `case f x of (# r1,r2 #) -> rhs` we can infer
r1<TagProper> and r2<TagDunno> which allows us to skip all tag checks on `r1`
in `rhs`.

Things get a bit more complicated with nesting:

    let closeFd<TagTuple[...]> = ...
    let f x = ...
        case x of
          _ -> Solo# closeFd

The "natural" signature for the Solo# branch in `f` would be <TagTuple[TagTuple[...]]>.
But we flatten this out to <TagTuple[TagDunno]> for the time being as it improves compile
time and there doesn't seem to huge benefit to doing differently.

  -}

-- See Note [Constructor TagSigs]
inferConTag :: TagEnv p -> DataCon -> [StgArg] -> TagInfo
inferConTag env con args
  | isUnboxedTupleDataCon con
  = TagTuple $ map (flatten_arg_tag . lookupInfo env) args
  | otherwise =
    -- pprTrace "inferConTag"
    --   ( text "con:" <> ppr con $$
    --     text "args:" <> ppr args $$
    --     text "marks:" <> ppr (dataConRuntimeRepStrictness con) $$
    --     text "arg_info:" <> ppr (map (lookupInfo env) args) $$
    --     text "info:" <> ppr info) $
    info
  where
    info = if any arg_needs_eval strictArgs then TagDunno else TagProper
    strictArgs = zipEqual args (dataConRuntimeRepStrictness con) :: ([(StgArg, StrictnessMark)])
    arg_needs_eval (arg,strict)
      -- lazy args
      | not (isMarkedStrict strict) = False
      | tag <- (lookupInfo env arg)
      -- banged args need to be tagged, or require eval
      = not (isTaggedInfo tag)

    flatten_arg_tag (TagTagged) = TagProper
    flatten_arg_tag (TagProper ) = TagProper
    flatten_arg_tag (TagTuple _) = TagDunno -- See Note [Constructor TagSigs]
    flatten_arg_tag (TagDunno) = TagDunno


collectExportInfo :: [GenStgTopBinding 'InferTaggedBinders] -> NameEnv TagSig
collectExportInfo binds =
  mkNameEnv bndr_info
  where
    bndr_info = concatMap collect binds :: [(Name,TagSig)]

    collect (StgTopStringLit {}) = []
    collect (StgTopLifted bnd) =
      case bnd of
        StgNonRec (id,sig) _rhs
          | TagSig TagDunno <- sig -> []
          | otherwise -> [(idName id,sig)]
        StgRec bnds -> collectRec bnds

    collectRec :: [(BinderP 'InferTaggedBinders, rhs)] -> [(Name,TagSig)]
    collectRec [] = []
    collectRec (bnd:bnds)
      | (p,_rhs)  <- bnd
      , (id,sig) <- p
      , TagSig TagDunno <- sig
      = (idName id,sig) : collectRec bnds
      | otherwise = collectRec bnds
