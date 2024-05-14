{-# LANGUAGE TypeFamilies, DataKinds, GADTs, FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
 -- To permit: type instance XLet 'InferTaggedBinders = XLet 'SomePass

{-# OPTIONS_GHC -Wname-shadowing #-}
module GHC.Stg.InferTags ( inferTags ) where

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

import GHC.Stg.InferTags.Types
import GHC.Stg.InferTags.Rewrite (rewriteTopBinds)
import Data.Maybe
import GHC.Types.Name.Env (mkNameEnv, NameEnv)
import GHC.Driver.DynFlags
import GHC.Utils.Logger
import qualified GHC.Unit.Types

{- Note [Tag Inference]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The purpose of this pass is to attach to every binder a flag
to indicate whether or not it is "properly tagged".  A binder
is properly tagged if it is guaranteed:
 - to point to a heap-allocated *value*
 - and to have the tag of the value encoded in the pointer

For example
  let x = Just y in ...

Here x will be properly tagged: it will point to the heap-allocated
values for (Just y), and the tag-bits of the pointer will encode
the tag for Just so there is no need to re-enter the closure or even
check for the presence of tag bits. The impacts of this can be very large.

For containers the reduction in runtimes with this optimization was as follows:

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

See Note [Tag inference passes] for how we proceed to generate and use this information.

Note [Strict Field Invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As part of tag inference we introduce the Strict Field Invariant.
Which consists of us saying that:

* Pointers in strict fields must (a) point directly to the value, and
  (b) must be properly tagged.

For example, given
  data T = MkT ![Int]

the Strict Field Invariant guarantees that the first field of any `MkT` constructor
will either point directly to nil, or directly to a cons cell;
and will be tagged with `001` or `010` respectively.
It will never point to a thunk, nor will it be tagged `000` (meaning "might be a thunk").
NB: Note that the proper tag for some objects is indeed `000`. Currently this is the case for PAPs.

This works analogous to how `WorkerLikeId`s work. See also Note [CBV Function Ids].

Why do we care? Because if we have code like:

case strictPair of
  SP x y ->
    case x of ...

It allows us to safely omit the code to enter x and the check
for the presence of a tag that goes along with it.
However we might still branch on the tag as usual.
See Note [Tag Inference] for how much impact this can have for
some code.

This is enforced by the code GHC.Stg.InferTags.Rewrite
where we:

* Look at all constructor allocations.
* Check if arguments to their strict fields are known to be properly tagged
* If not we convert `StrictJust x` into `case x of x' -> StrictJust x'`

This is usually very beneficial but can cause regressions in rare edge cases where
we fail to proof that x is properly tagged, or where it simply isn't.
See Note [How untagged pointers can end up in strict fields] for how the second case
can arise.

For a full example of the worst case consider this code:

foo ... = ...
  let c = StrictJust x
  in ...

Here we would rewrite `let c = StrictJust x` into `let c = case x of x' -> StrictJust x'`
However that is horrible! We end up allocating a thunk for `c` first, which only when
evaluated will allocate the constructor.

So we do our best to establish that `x` is already tagged (which it almost always is)
to avoid this cost. In my benchmarks I haven't seen any cases where this causes regressions.

Note that there are similar constraints around Note [CBV Function Ids].

Note [How untagged pointers can end up in strict fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data Set a = Tip | Bin !a (Set a) (Set a)

We make a wrapper for Bin that evaluates its arguments
  $WBin x a b = case x of xv -> Bin xv a b
Here `xv` will always be evaluated and properly tagged, just as the
Strict Field Invariant requires.

But alas the Simplifier can destroy the invariant: see #15696.
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

In short, although it may be rare, the output of optimisation passes
cannot guarantee to obey the Strict Field Invariant. For this reason
we run tag inference. See Note [Tag inference passes].

Note [Tag inference passes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Tag inference proceeds in two passes:
* The first pass is an analysis to compute which binders are properly tagged.
  The result is then attached to /binders/.
  This is implemented by `inferTagsAnal` in GHC.Stg.InferTags
* The second pass walks over the AST checking if the Strict Field Invariant is upheld.
  See Note [Strict Field Invariant].
  If required this pass modifies the program to uphold this invariant.
  Tag information is also moved from /binders/ to /occurrences/ during this pass.
  This is done by `GHC.Stg.InferTags.Rewrite (rewriteTopBinds)`.
* Finally the code generation uses this information to skip the thunk check when branching on
  values. This is done by `cgExpr`/`cgCase` in the backend.

Last but not least we also export the tag sigs of top level bindings to allow this optimization
 to work across module boundaries.

Note [TagInfo of functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

Note [Tag inference debugging]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is a flag -dtag-inference-checks which inserts various
compile/runtime checks in order to ensure the Strict Field Invariant
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

Note [Tag inference for interpreted code]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
   This is handled in GHC.Stg.InferTags.Types.lookupInfo


-}

{- *********************************************************************
*                                                                      *
                         Tag inference pass
*                                                                      *
********************************************************************* -}

inferTags :: StgPprOpts -> Bool -> Logger -> (GHC.Unit.Types.Module) -> [CgStgTopBinding] -> IO ([TgStgTopBinding], NameEnv TagSig)
inferTags ppr_opts !for_bytecode logger this_mod stg_binds = do
    -- pprTraceM "inferTags for " (ppr this_mod <> text " bytecode:" <> ppr for_bytecode)
    -- Annotate binders with tag information.
    let (!stg_binds_w_tags) = {-# SCC "StgTagFields" #-}
                                        inferTagsAnal for_bytecode stg_binds
    putDumpFileMaybe logger Opt_D_dump_stg_tags "CodeGenAnal STG:" FormatSTG (pprGenStgTopBindings ppr_opts stg_binds_w_tags)

    let export_tag_info = collectExportInfo stg_binds_w_tags

    -- Rewrite STG to uphold the strict field invariant
    us_t <- mkSplitUniqSupply 't'
    let rewritten_binds = {-# SCC "StgTagRewrite" #-} rewriteTopBinds this_mod us_t stg_binds_w_tags :: [TgStgTopBinding]

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

inferTagsAnal :: Bool -> [GenStgTopBinding 'CodeGen] -> [GenStgTopBinding 'InferTaggedBinders]
inferTagsAnal for_bytecode binds =
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
  , let bndrs' = zipWithEqual "inferTagExpr" mk_bndr bndrs infos
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
         (out_sigs, rhss') = unzip (zipWithEqual "inferTagBind" anaRhs in_ids go_rhss)
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
-- See Note [Tag inference for interpreted code]
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
    strictArgs = zipEqual "inferTagRhs" args (dataConRuntimeRepStrictness con) :: ([(StgArg, StrictnessMark)])
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
