{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | Unfolding creation
module GHC.Core.Unfold.Make
   ( noUnfolding
   , mkUnfolding
   , mkCoreUnfolding
   , mkFinalUnfolding
   , mkSimpleUnfolding
   , mkWorkerUnfolding
   , mkInlineUnfolding
   , mkInlineUnfoldingWithArity
   , mkInlinableUnfolding
   , mkWrapperUnfolding
   , mkCompulsoryUnfolding
   , mkCompulsoryUnfolding'
   , mkDFunUnfolding
   , specUnfolding
   , certainlyWillInline
   )
where

import GHC.Prelude
import GHC.Core
import GHC.Core.Unfold
import GHC.Core.Opt.OccurAnal ( occurAnalyseExpr )
import GHC.Core.Opt.Arity   ( manifestArity )
import GHC.Core.DataCon
import GHC.Core.Utils
import GHC.Types.Basic
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Demand ( DmdSig, isDeadEndSig )

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic

-- the very simple optimiser is used to optimise unfoldings
import {-# SOURCE #-} GHC.Core.SimpleOpt



mkFinalUnfolding :: UnfoldingOpts -> UnfoldingSource -> DmdSig -> CoreExpr -> Unfolding
-- "Final" in the sense that this is a GlobalId that will not be further
-- simplified; so the unfolding should be occurrence-analysed
mkFinalUnfolding opts src strict_sig expr
  = mkUnfolding opts src
                True {- Top level -}
                (isDeadEndSig strict_sig)
                expr

-- | Used for things that absolutely must be unfolded
mkCompulsoryUnfolding :: SimpleOpts -> CoreExpr -> Unfolding
mkCompulsoryUnfolding opts expr = mkCompulsoryUnfolding' (simpleOptExpr opts expr)

-- | Same as 'mkCompulsoryUnfolding' but no simple optimiser pass is performed
-- on the unfolding.
mkCompulsoryUnfolding' :: CoreExpr -> Unfolding
mkCompulsoryUnfolding' expr
  = mkCoreUnfolding InlineCompulsory True
                    expr
                    (UnfWhen { ug_arity = 0    -- Arity of unfolding doesn't matter
                             , ug_unsat_ok = unSaturatedOk, ug_boring_ok = boringCxtOk })

-- Note [Top-level flag on inline rules]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Slight hack: note that mk_inline_rules conservatively sets the
-- top-level flag to True.  It gets set more accurately by the simplifier
-- Simplify.simplUnfolding.

mkSimpleUnfolding :: UnfoldingOpts -> CoreExpr -> Unfolding
mkSimpleUnfolding !opts rhs
  = mkUnfolding opts InlineRhs False False rhs

mkDFunUnfolding :: [Var] -> DataCon -> [CoreExpr] -> Unfolding
mkDFunUnfolding bndrs con ops
  = DFunUnfolding { df_bndrs = bndrs
                  , df_con = con
                  , df_args = map occurAnalyseExpr ops }
                  -- See Note [Occurrence analysis of unfoldings]

mkWrapperUnfolding :: SimpleOpts -> CoreExpr -> Arity -> Unfolding
-- Make the unfolding for the wrapper in a worker/wrapper split
-- after demand/CPR analysis
mkWrapperUnfolding opts expr arity
  = mkCoreUnfolding InlineStable True
                    (simpleOptExpr opts expr)
                    (UnfWhen { ug_arity     = arity
                             , ug_unsat_ok  = unSaturatedOk
                             , ug_boring_ok = boringCxtNotOk })

mkWorkerUnfolding :: SimpleOpts -> (CoreExpr -> CoreExpr) -> Unfolding -> Unfolding
-- See Note [Worker/wrapper for INLINABLE functions] in GHC.Core.Opt.WorkWrap
mkWorkerUnfolding opts work_fn
                  (CoreUnfolding { uf_src = src, uf_tmpl = tmpl
                                 , uf_is_top = top_lvl })
  | isStableSource src
  = mkCoreUnfolding src top_lvl new_tmpl guidance
  where
    new_tmpl = simpleOptExpr opts (work_fn tmpl)
    guidance = calcUnfoldingGuidance (so_uf_opts opts) False new_tmpl

mkWorkerUnfolding _ _ _ = noUnfolding

-- | Make an unfolding that may be used unsaturated
-- (ug_unsat_ok = unSaturatedOk) and that is reported as having its
-- manifest arity (the number of outer lambdas applications will
-- resolve before doing any work).
mkInlineUnfolding :: SimpleOpts -> CoreExpr -> Unfolding
mkInlineUnfolding opts expr
  = mkCoreUnfolding InlineStable
                    True         -- Note [Top-level flag on inline rules]
                    expr' guide
  where
    expr' = simpleOptExpr opts expr
    guide = UnfWhen { ug_arity = manifestArity expr'
                    , ug_unsat_ok = unSaturatedOk
                    , ug_boring_ok = boring_ok }
    boring_ok = inlineBoringOk expr'

-- | Make an unfolding that will be used once the RHS has been saturated
-- to the given arity.
mkInlineUnfoldingWithArity :: Arity -> SimpleOpts -> CoreExpr -> Unfolding
mkInlineUnfoldingWithArity arity opts expr
  = mkCoreUnfolding InlineStable
                    True         -- Note [Top-level flag on inline rules]
                    expr' guide
  where
    expr' = simpleOptExpr opts expr
    guide = UnfWhen { ug_arity = arity
                    , ug_unsat_ok = needSaturated
                    , ug_boring_ok = boring_ok }
    -- See Note [INLINE pragmas and boring contexts] as to why we need to look
    -- at the arity here.
    boring_ok | arity == 0 = True
              | otherwise  = inlineBoringOk expr'

mkInlinableUnfolding :: SimpleOpts -> CoreExpr -> Unfolding
mkInlinableUnfolding opts expr
  = mkUnfolding (so_uf_opts opts) InlineStable False False expr'
  where
    expr' = simpleOptExpr opts expr

specUnfolding :: SimpleOpts
              -> [Var] -> (CoreExpr -> CoreExpr)
              -> [CoreArg]   -- LHS arguments in the RULE
              -> Unfolding -> Unfolding
-- See Note [Specialising unfoldings]
-- specUnfolding spec_bndrs spec_args unf
--   = \spec_bndrs. unf spec_args
--
specUnfolding opts spec_bndrs spec_app rule_lhs_args
              df@(DFunUnfolding { df_bndrs = old_bndrs, df_con = con, df_args = args })
  = assertPpr (rule_lhs_args `equalLength` old_bndrs)
              (ppr df $$ ppr rule_lhs_args) $
           -- For this ASSERT see Note [DFunUnfoldings] in GHC.Core.Opt.Specialise
    mkDFunUnfolding spec_bndrs con (map spec_arg args)
      -- For DFunUnfoldings we transform
      --       \obs. MkD <op1> ... <opn>
      -- to
      --       \sbs. MkD ((\obs. <op1>) spec_args) ... ditto <opn>
  where
    spec_arg arg = simpleOptExpr opts $
                   spec_app (mkLams old_bndrs arg)
                   -- The beta-redexes created by spec_app will be
                   -- simplified away by simplOptExpr

specUnfolding opts spec_bndrs spec_app rule_lhs_args
              (CoreUnfolding { uf_src = src, uf_tmpl = tmpl
                             , uf_is_top = top_lvl
                             , uf_guidance = old_guidance })
 | isStableSource src  -- See Note [Specialising unfoldings]
 , UnfWhen { ug_arity = old_arity } <- old_guidance
 = mkCoreUnfolding src top_lvl new_tmpl
                   (old_guidance { ug_arity = old_arity - arity_decrease })
 where
   new_tmpl = simpleOptExpr opts $
              mkLams spec_bndrs  $
              spec_app tmpl  -- The beta-redexes created by spec_app
                             -- will be simplified away by simplOptExpr
   arity_decrease = count isValArg rule_lhs_args - count isId spec_bndrs


specUnfolding _ _ _ _ _ = noUnfolding

{- Note [Specialising unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we specialise a function for some given type-class arguments, we use
specUnfolding to specialise its unfolding.  Some important points:

* If the original function has a DFunUnfolding, the specialised one
  must do so too!  Otherwise we lose the magic rules that make it
  interact with ClassOps

* There is a bit of hack for INLINABLE functions:
     f :: Ord a => ....
     f = <big-rhs>
     {- INLINABLE f #-}
  Now if we specialise f, should the specialised version still have
  an INLINABLE pragma?  If it does, we'll capture a specialised copy
  of <big-rhs> as its unfolding, and that probably won't inline.  But
  if we don't, the specialised version of <big-rhs> might be small
  enough to inline at a call site. This happens with Control.Monad.liftM3,
  and can cause a lot more allocation as a result (nofib n-body shows this).

  Moreover, keeping the INLINABLE thing isn't much help, because
  the specialised function (probably) isn't overloaded any more.

  Conclusion: drop the INLINEALE pragma.  In practice what this means is:
     if a stable unfolding has UnfoldingGuidance of UnfWhen,
        we keep it (so the specialised thing too will always inline)
     if a stable unfolding has UnfoldingGuidance of UnfIfGoodArgs
        (which arises from INLINABLE), we discard it

Note [Honour INLINE on 0-ary bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

   x = <expensive>
   {-# INLINE x #-}

   f y = ...x...

The semantics of an INLINE pragma is

  inline x at every call site, provided it is saturated;
  that is, applied to at least as many arguments as appear
  on the LHS of the Haskell source definition.

(This source-code-derived arity is stored in the `ug_arity` field of
the `UnfoldingGuidance`.)

In the example, x's ug_arity is 0, so we should inline it at every use
site.  It's rare to have such an INLINE pragma (usually INLINE is on
functions), but it's occasionally very important (#15578, #15519).
In #15519 we had something like
   x = case (g a b) of I# r -> T r
   {-# INLINE x #-}
   f y = ...(h x)....

where h is strict.  So we got
   f y = ...(case g a b of I# r -> h (T r))...

and that in turn allowed SpecConstr to ramp up performance.

How do we deliver on this?  By adjusting the ug_boring_ok
flag in mkInlineUnfoldingWithArity; see
Note [INLINE pragmas and boring contexts]

NB: there is a real risk that full laziness will float it right back
out again. Consider again
  x = factorial 200
  {-# INLINE x #-}
  f y = ...x...

After inlining we get
  f y = ...(factorial 200)...

but it's entirely possible that full laziness will do
  lvl23 = factorial 200
  f y = ...lvl23...

That's a problem for another day.

Note [INLINE pragmas and boring contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An INLINE pragma uses mkInlineUnfoldingWithArity to build the
unfolding.  That sets the ug_boring_ok flag to False if the function
is not tiny (inlineBoringOK), so that even INLINE functions are not
inlined in an utterly boring context.  E.g.
     \x y. Just (f y x)
Nothing is gained by inlining f here, even if it has an INLINE
pragma.

But for 0-ary bindings, we want to inline regardless; see
Note [Honour INLINE on 0-ary bindings].

I'm a bit worried that it's possible for the same kind of problem
to arise for non-0-ary functions too, but let's wait and see.
-}

mkUnfolding :: UnfoldingOpts
            -> UnfoldingSource
            -> Bool       -- Is top-level
            -> Bool       -- Definitely a bottoming binding
                          -- (only relevant for top-level bindings)
            -> CoreExpr
            -> Unfolding
-- Calculates unfolding guidance
-- Occurrence-analyses the expression before capturing it
mkUnfolding opts src top_lvl is_bottoming expr
  = mkCoreUnfolding src top_lvl expr guidance
  where
    is_top_bottoming = top_lvl && is_bottoming
    guidance         = calcUnfoldingGuidance opts is_top_bottoming expr
        -- NB: *not* (calcUnfoldingGuidance (occurAnalyseExpr expr))!
        -- See Note [Calculate unfolding guidance on the non-occ-anal'd expression]

mkCoreUnfolding :: UnfoldingSource -> Bool -> CoreExpr
                -> UnfoldingGuidance -> Unfolding
-- Occurrence-analyses the expression before capturing it
mkCoreUnfolding src top_lvl expr guidance
  =

  let is_value = exprIsHNF expr
      is_conlike = exprIsConLike expr
      is_work_free = exprIsWorkFree expr
      is_expandable = exprIsExpandable expr
  in
  -- See #20905 for what is going on here. We are careful to make sure we only
  -- have one copy of an unfolding around at once.
  -- Note [Thoughtful forcing in mkCoreUnfolding]
  CoreUnfolding { uf_tmpl         = is_value `seq`
                                    is_conlike `seq`
                                    is_work_free `seq`
                                    is_expandable `seq`
                                      occurAnalyseExpr expr,
                      -- See Note [Occurrence analysis of unfoldings]
                    uf_src          = src,
                    uf_is_top       = top_lvl,
                    uf_is_value     = is_value,
                    uf_is_conlike   = is_conlike,
                    uf_is_work_free = is_work_free,
                    uf_expandable   = is_expandable,
                    uf_guidance     = guidance }

----------------
certainlyWillInline :: UnfoldingOpts -> IdInfo -> CoreExpr -> Maybe Unfolding
-- ^ Sees if the unfolding is pretty certain to inline.
-- If so, return a *stable* unfolding for it, that will always inline.
-- The CoreExpr is the WW'd and simplified RHS. In contrast, the unfolding
-- template might not have been WW'd yet.
certainlyWillInline opts fn_info rhs'
  = case fn_unf of
      CoreUnfolding { uf_guidance = guidance, uf_src = src }
        | noinline -> Nothing       -- See Note [Worker/wrapper for NOINLINE functions]
        | otherwise
        -> case guidance of
             UnfNever   -> Nothing
             UnfWhen {} -> Just (fn_unf { uf_src = src', uf_tmpl = tmpl' })
                             -- INLINE functions have UnfWhen
             UnfIfGoodArgs { ug_size = size, ug_args = args }
                        -> do_cunf size args src' tmpl'
        where
          src' = -- Do not change InlineCompulsory!
                 case src of
                   InlineCompulsory -> InlineCompulsory
                   _                -> InlineStable
          tmpl' = -- Do not overwrite stable unfoldings!
                  case src of
                    InlineRhs -> occurAnalyseExpr rhs'
                    _         -> uf_tmpl fn_unf

      DFunUnfolding {} -> Just fn_unf  -- Don't w/w DFuns; it never makes sense
                                       -- to do so, and even if it is currently a
                                       -- loop breaker, it may not be later

      _other_unf       -> Nothing

  where
    noinline = isNoInlinePragma (inlinePragInfo fn_info)
    fn_unf   = unfoldingInfo fn_info -- NB: loop-breakers never inline

        -- The UnfIfGoodArgs case seems important.  If we w/w small functions
        -- binary sizes go up by 10%!  (This is with SplitObjs.)
        -- I'm not totally sure why.
        -- INLINABLE functions come via this path
        --    See Note [certainlyWillInline: INLINABLE]
    do_cunf size args src' tmpl'
      | arityInfo fn_info > 0  -- See Note [certainlyWillInline: be careful of thunks]
      , not (isDeadEndSig (dmdSigInfo fn_info))
              -- Do not unconditionally inline a bottoming functions even if
              -- it seems smallish. We've carefully lifted it out to top level,
              -- so we don't want to re-inline it.
      , let unf_arity = length args
      , size - (10 * (unf_arity + 1)) <= unfoldingUseThreshold opts
      = Just (fn_unf { uf_src      = src'
                     , uf_tmpl     = tmpl'
                     , uf_guidance = UnfWhen { ug_arity     = unf_arity
                                             , ug_unsat_ok  = unSaturatedOk
                                             , ug_boring_ok = inlineBoringOk tmpl' } })
             -- Note the "unsaturatedOk". A function like  f = \ab. a
             -- will certainly inline, even if partially applied (f e), so we'd
             -- better make sure that the transformed inlining has the same property
      | otherwise
      = Nothing

{- Note [certainlyWillInline: be careful of thunks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Don't claim that thunks will certainly inline, because that risks work
duplication.  Even if the work duplication is not great (eg is_cheap
holds), it can make a big difference in an inner loop In #5623 we
found that the WorkWrap phase thought that
       y = case x of F# v -> F# (v +# v)
was certainlyWillInline, so the addition got duplicated.

Note that we check arityInfo instead of the arity of the unfolding to detect
this case. This is so that we don't accidentally fail to inline small partial
applications, like `f = g 42` (where `g` recurses into `f`) where g has arity 2
(say). Here there is no risk of work duplication, and the RHS is tiny, so
certainlyWillInline should return True. But `unf_arity` is zero! However f's
arity, gotten from `arityInfo fn_info`, is 1.

Failing to say that `f` will inline forces W/W to generate a potentially huge
worker for f that will immediately cancel with `g`'s wrapper anyway, causing
unnecessary churn in the Simplifier while arriving at the same result.

Note [certainlyWillInline: INLINABLE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
certainlyWillInline /must/ return Nothing for a large INLINABLE thing,
even though we have a stable inlining, so that strictness w/w takes
place.  It makes a big difference to efficiency, and the w/w pass knows
how to transfer the INLINABLE info to the worker; see WorkWrap
Note [Worker/wrapper for INLINABLE functions]

Note [Thoughtful forcing in mkCoreUnfolding]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Core expressions retained in unfoldings is one of biggest uses of memory when compiling
a program. Therefore we have to be careful about retaining copies of old or redundant
templates (see !6202 for a particularlly bad case).

With that in mind we want to maintain the invariant that each unfolding only references
a single CoreExpr. One place where we have to be careful is in mkCoreUnfolding.

* The template of the unfolding is the result of performing occurence analysis
  (Note [Occurrence analysis of unfoldings])
* Predicates are applied to the unanalysed expression

Therefore if we are not thoughtful about forcing you can end up in a situation where the
template is forced but not all the predicates are forced so the unfolding will retain
both the old and analysed expressions.

I investigated this using ghc-debug and it was clear this situation did often arise:

```
(["ghc:GHC.Core:Lam","ghc-prim:GHC.Types:True","THUNK_1_0","THUNK_1_0","THUNK_1_0"],Count 4307)
```

Here the predicates are unforced but the template is forced.

Therefore we basically had two options in order to fix this:

1. Perform the predicates on the analysed expression.
2. Force the predicates to remove retainer to the old expression if we force the template.

Option 1 is bad because occurence analysis is expensive and destroys any sharing of the unfolding
with the actual program. (Testing this approach showed peak 25G memory usage)

Therefore we got for Option 2 which performs a little more work but compensates by
reducing memory pressure.

The result of fixing this led to a 1G reduction in peak memory usage (12G -> 11G) when
compiling a very large module (peak 3 million terms). For more discussion see #20905.
-}

