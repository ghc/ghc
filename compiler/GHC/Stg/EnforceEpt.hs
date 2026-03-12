{-# LANGUAGE TypeFamilies #-}
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
import GHC.Types.Unique.Supply (mkSplitUniqSupply, UniqueTag(StgTag))
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

Wrinkles:

EPT1:  The proper tag for builtin *unlifted* data types such as `Array#` is
       not `001` but `000`, which is not a proper tag for lifted data.
       This means that UnliftedRep is not a proper sub-rep of LiftedRep.
       SG thinks it would be good to fix this; see #21792.
EPT2:  The proper tag for PAPs is `000`, so there are values from lifted data types
       that are EPT with a zero tag.
EPT3:  Non-pointer values don't have a tag at all, but we still treat them as EPT.

Note [EPT enforcement]
~~~~~~~~~~~~~~~~~~~~~~
The goal of EnforceEPT pass is to mark as many binders as possible as EPT
(see Note [Evaluated and Properly Tagged]). It establishes the following
invariant:

EPT INVARIANT:
> Any binder of
>   * a strict field (see Note [Strict fields in Core]), or
>   * a CBV argument (see Note [CBV Function Ids: overview])
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
functions. See Note [CBV Function Ids: overview] for more details.

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

(EPT-anal) EPT analysis is implemented in `GHC.Stg.EnforceEpt.inferTags.`
  It attaches its result to /binders/, not occurrence sites.

(EPT-rewrite) The EPT rewriter, `GHC.Stg.EnforceEpt.Rewrite.rewriteTopBinds`,
   establishes the EPT invariant by inserting evals. That is, if
    (a) a binder x is used to
          * construct a strict field (`SP x y`), or
          * passed as a CBV argument (`$wf x`),
        and
    (b) x was not inferred EPT,
  then the EPT rewriter inserts an eval prior to the call, e.g.
    case x of x' { __ DEFAULT -> SP x' y }.
    case x of x' { __ DEFAULT -> $wf x' }.
  (Recall that the case binder x' is always EPT.)

  This pass also propagates the EPTness from binders to occurrences.

  It is sound to insert evals on strict fields (Note [Strict fields in Core]),
  and on CBV arguments as well (Note [CBV Function Ids: overview]).

(EPT-codegen) Finally, code generation for (case x of alts) skips the thunk check
  when `x` is EPT. This is done (a bit indirectly) thus:
   * GHC.StgToCmm.Expr.cgCase: builds a `sequel`, and recurses into `cgExpr` on `x`.
   * When `cgExpr` sees a `x` goes to `cgIdApp`, which uses `getCallMethod`.
   * Then `getCallMethod` sees that `x` is EPT (via `idTagSigMaybe`), and
     returns `InferredReturnIt`.
   * Now `cgIdApp` can jump straight to the case-alternative switch in the `sequel`
     constructed by `cgCase`.

(EPT-export) We also export the EPTness of top level bindings to allow this optimisation
  to work across module boundaries.

  NB: The EPT Invariant *must* be upheld, regardless of the optimisation level;
  hence EPTness is practically part of the internal ABI of a strict data
  constructor or CBV function. Note [CBV Function Ids: overview] has the details.

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
'TagFun' represents an evaluated function and carries the return-value TagInfo
for saturated calls. See Note [TagSig and TagInfo] in GHC.Stg.EnforceEpt.TagSig.

Join points (StgLetNoEscape) are treated like functions: they are jumped to,
never entered via a closure pointer. Even nullary join points get TagFun
rather than TagVal TagDunno, so their return-value info flows through.

Additionally we also special case bottoming functions.
See Note [Bottom functions are TagBottoming].

Function values when properly tagged have either their arity, 1 for very large
arities, or zero for PAPs (#21193) as tag. However the runtime currently doesn't
maintain the EPT status of function pointers. So while we keep track of
evaluated function binders using 'TagFun' this has a weaker guarantee than for
values. For functions it only represents a pointer to an evaluated function, but
doesn't give any guarantee about the function's pointer tag.

Sadly because PAPs get tag zero (#21193), other issues in the RTS (#21193) and
the potential of over/underapplication of arguments, even evaluated function
binders still require inspection of their closure before they can be applied.
That is, even if we have

    case f of f' -> ... f' x ...

the code for `f' x` can't be made meaningfully more efficient by knowing `f'` is
already evaluated.

However tracking evaluation status for functions is still worthwhile, as we rely
on the EPT pass to force strict constructor fields. For an application
`StrictJust f` we would usually insert an eval of `f` around the constructor
application. However if we infer `f` to be already evaluated we can avoid this,
which was a significant performance benefit in #27005. Similarly we can still
turn redundant eval-cases on functions into a no-op.

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
    us_t <- mkSplitUniqSupply StgTag
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
         -- See Note [Bottom functions are TagBottoming] and #24806 for why.
         | isDeadEndAppSig (idDmdSig fun) (length args)
         = TagBottoming

         | fun_arity == 0 -- Unknown arity => Thunk or unknown call
         = TagDunno

         -- Imported function with known return tag
         | Just (TagFun res_info) <- tagSigInfo (idInfo fun)
         , fun_arity == length args  -- Saturated
         = res_info

         -- Local function with known return tag
         | Just res_info <- lookupReturnInfo env fun
         , fun_arity == length args  -- Saturated
         = res_info

         | otherwise
         = --pprTrace "inferAppUnknown" (ppr fun) $
           TagDunno

inferTagExpr env (StgConApp con cn args tys)
  = (inferConTag env con args, StgConApp con cn args tys)

inferTagExpr _ (StgLit l)
  = (TagEPT, StgLit l)

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
            (getBinderId in_env tup_bndr, TagVal tup_info)
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
        alt_info = foldr combineAltInfo TagBottoming infos
    in ( alt_info, StgCase scrut' bndr' ty alts')
  where
    -- Single unboxed tuple alternative
    scrut_infos bndrs = case scrut_info of
      TagBottoming -> Just $ replicate (length bndrs) TagBottoming
      TagTuple infos -> Just infos
      _ -> Nothing
    (scrut_info, scrut') = inferTagExpr in_env scrut
    bndr' = (getBinderId in_env bndr, TagVal TagEPT)

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
      = (id, TagVal TagEPT)
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
    (in_sig, rhs') = inferTagRhs id in_env rhs
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

    go :: forall q. (OutputableInferPass q , InferExtEq q)
       => TagEnv q -> [TagSig] -> [GenStgRhs q]
       -> (TagSigEnv, [((Id,TagSig), GenStgRhs 'InferTaggedBinders)])
    go go_env in_sigs go_rhss
       | in_sigs == out_sigs = (te_env rhs_env, out_bndrs `zip` rhss')
       | otherwise           = go env' out_sigs rhss'
       where
         in_bndrs = in_ids `zip` in_sigs
         out_bndrs = map updateBndr in_bndrs -- TODO: Keeps in_ids alive
         rhs_env = extendSigEnv go_env in_bndrs
         (out_sigs, rhss') = unzip (zipWithEqual anaRhs in_ids go_rhss)
         env' = makeTagged go_env

         anaRhs :: Id -> GenStgRhs q -> (TagSig, GenStgRhs 'InferTaggedBinders)
         anaRhs bnd rhs =
            let (sig_rhs, rhs') = inferTagRhs bnd rhs_env rhs
            in (mkLetSig go_env sig_rhs, rhs')


         updateBndr :: (Id,TagSig) -> (Id,TagSig)
         updateBndr (v,sig) = (setIdTagSig v sig, sig)

-- Initial signature for the fixpoint loop.
initSig :: forall p. (Id, GenStgRhs p) -> TagSig
initSig (_bndr, StgRhsCon {})
  = TagVal TagBottoming
initSig (bndr, StgRhsClosure _ _ _ bndrs _ _)
  | notNull bndrs || isJoinId bndr
  = fromMaybe (TagFun TagBottoming) (idTagSig_maybe bndr)
  | otherwise  -- thunk
  = fromMaybe (TagVal TagBottoming) (idTagSig_maybe bndr)

{- Note [Bottom functions are TagBottoming]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have a function with two branches with one
being bottom, and the other returning a tagged
unboxed tuple what is the result? We give it TagBottoming!
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
So we give it TagBottoming, as it allows the combined tag sig of the case expression
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
  | isDeadEndId bnd_id && is_fun_or_join
  -- See Note [Bottom functions are TagBottoming]
  = ( TagFun TagBottoming
    , StgRhsClosure ext cc upd out_bndrs body' typ)
  | is_fun_or_join
  = ( TagFun info
    , StgRhsClosure ext cc upd out_bndrs body' typ)
  | otherwise  -- thunk (never a join point)
  = ( TagVal TagDunno
    , StgRhsClosure ext cc upd out_bndrs body' typ)
  where
    -- Join points are treated like functions. See Note [TagInfo of functions]
    is_fun_or_join = notNull bndrs || isJoinId bnd_id

    out_bndrs
      | Just marks <- idCbvMarks_maybe bnd_id
      -- Sometimes an we eta-expand foo with additional arguments after ww, and we also trim
      -- the list of marks to the last strict entry. So we can conservatively
      -- assume these are not strict
      = zipWith (mkArgSig) bndrs (marks ++ repeat NotMarkedCbv)
      | otherwise = map (noSig env') bndrs :: [(Id,TagSig)]

    env' = extendSigEnv in_env out_bndrs
    (info, body') = inferTagExpr env' body

    mkArgSig :: BinderP p -> CbvMark -> (Id,TagSig)
    mkArgSig bndp mark =
      let id = getBinderId in_env bndp
          tag = case mark of
            MarkedCbv -> TagEPT
            _
              | isUnliftedType (idType id) -> TagEPT
              | otherwise -> TagDunno
      in (id, TagVal tag)

inferTagRhs _ env _rhs@(StgRhsCon cc con cn ticks args typ)
-- Constructors, which have untagged arguments to strict fields
-- become thunks. We encode this by giving changing RhsCon nodes the info TagDunno
  = --pprTrace "inferTagRhsCon" (ppr grp_ids) $
    (TagVal (inferConTag env con args), StgRhsCon cc con cn ticks args typ)

-- Adjust let semantics to the targeted backend.
-- See Note [EPT enforcement for interpreted code]
mkLetSig :: TagEnv p -> TagSig -> TagSig
mkLetSig env in_sig
  | for_bytecode = TagVal TagDunno
  | otherwise    = in_sig
  where
    for_bytecode = te_bytecode env

{- Note [Constructor TagSigs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@inferConTag@ will infer the proper tag signature for a binding who's RHS is a constructor
or a StgConApp expression.
Usually these will simply be TagEPT. But there are exceptions.
If any of the fields in the constructor are strict, but any argument to these
fields is not tagged then we will have to case on the argument before storing
in the constructor. Which means for let bindings the RHS turns into a thunk
which obviously is no longer properly tagged.
For example we might start with:

    let x<TagVal[TagDunno]> = f ...
    let c<TagVal[TagEPT]> = StrictPair x True

But we know during the rewrite stage x will need to be evaluated in the RHS
of `c` so we will infer:

    let x<TagVal[TagDunno]> = f ...
    let c<TagVal[TagDunno]> = StrictPair x True

Which in the rewrite stage will then be rewritten into:

    let x<TagVal[TagDunno]> = f ...
    let c<TagVal[TagDunno]> = case x of x' -> StrictPair x' True

The other exception is unboxed tuples. These will get a TagTuple
signature with a list of TagInfo about their individual binders
as argument. As example:

    let c<TagVal[TagEPT]> = True
    let x<TagVal[TagDunno]> = ...
    let f<?> z = case z of z'<TagVal[TagEPT]> -> (# c, x #)

Here we will infer for f the Signature <TagFun[TagTuple[TagEPT,TagDunno]]>.
This information will be used if we scrutinize a saturated application of
`f` in order to determine the taggedness of the result.
That is for `case f x of (# r1,r2 #) -> rhs` we can infer
r1<TagVal[TagEPT]> and r2<TagVal[TagDunno]> which allows us to skip all tag checks on `r1`
in `rhs`.

Things get a bit more complicated with nesting:

    let closeFd<TagFun[...]> = ...
    let f x = ...
        case x of
          _ -> Solo# closeFd

We only keep track of function return types for bindings, not values:
TagInfo has no constructor for "function with return info". Therefore the
signature of `f` becomes <TagFun[TagTuple[TagEPT]]>. We lose the information
about the return type of `closeFd`.

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
    info = if any arg_needs_eval strictArgs then TagDunno else TagEPT
    strictArgs = zipEqual args (dataConRuntimeRepStrictness con) :: ([(StgArg, StrictnessMark)])
    arg_needs_eval (arg,strict)
      -- lazy args
      | not (isMarkedStrict strict) = False
      | tag <- (lookupInfo env arg)
      -- banged args need to be tagged, or require eval
      = not (isTaggedInfo tag)

    flatten_arg_tag TagBottoming    = TagEPT
    flatten_arg_tag TagEPT          = TagEPT
    flatten_arg_tag (TagTuple _)    = TagDunno -- See Note [Constructor TagSigs]
    flatten_arg_tag TagDunno        = TagDunno


collectExportInfo :: [GenStgTopBinding 'InferTaggedBinders] -> NameEnv TagSig
collectExportInfo binds =
  mkNameEnv bndr_info
  where
    bndr_info = concatMap collect binds :: [(Name,TagSig)]

    collect (StgTopStringLit {}) = []
    collect (StgTopLifted bnd) =
      case bnd of
        StgNonRec (id,sig) _rhs
          | TagVal TagDunno     <- sig -> []
          | TagVal TagBottoming <- sig -> []
          | otherwise -> [(idName id,sig)]
        StgRec bnds -> collectRec bnds

    collectRec :: [(BinderP 'InferTaggedBinders, rhs)] -> [(Name,TagSig)]
    collectRec [] = []
    collectRec (((id,sig), _rhs) : bnds)
      | TagVal TagDunno     <- sig = collectRec bnds
      | TagVal TagBottoming <- sig = collectRec bnds
      | otherwise                  = (idName id, sig) : collectRec bnds
