{-# LANGUAGE CPP #-}

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
   , mkWwInlineRule
   , mkCompulsoryUnfolding
   , mkCompulsoryUnfolding'
   , mkDFunUnfolding
   , specUnfolding
   )
where

#include "HsVersions.h"

import GHC.Prelude
import GHC.Core
import GHC.Core.Unfold
import GHC.Core.Opt.OccurAnal ( occurAnalyseExpr )
import GHC.Core.Opt.Arity   ( manifestArity )
import GHC.Core.DataCon
import GHC.Core.Utils
import GHC.Types.Basic
import GHC.Types.Id
import GHC.Types.Demand ( StrictSig, isDeadEndSig )

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic

-- the very simple optimiser is used to optimise unfoldings
import {-# SOURCE #-} GHC.Core.SimpleOpt



mkFinalUnfolding :: UnfoldingOpts -> UnfoldingSource -> StrictSig -> CoreExpr -> Unfolding
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

mkWwInlineRule :: SimpleOpts -> CoreExpr -> Arity -> Unfolding
mkWwInlineRule opts expr arity
  = mkCoreUnfolding InlineStable True
                   (simpleOptExpr opts expr)
                   (UnfWhen { ug_arity = arity, ug_unsat_ok = unSaturatedOk
                            , ug_boring_ok = boringCxtNotOk })

mkWorkerUnfolding :: SimpleOpts -> (CoreExpr -> CoreExpr) -> Unfolding -> Unfolding
-- See Note [Worker-wrapper for INLINABLE functions] in GHC.Core.Opt.WorkWrap
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
  = ASSERT2( rule_lhs_args `equalLength` old_bndrs
           , ppr df $$ ppr rule_lhs_args )
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
 , UnfWhen { ug_arity     = old_arity } <- old_guidance
 = mkCoreUnfolding src top_lvl new_tmpl
                   (old_guidance { ug_arity = old_arity - arity_decrease })
 where
   new_tmpl = simpleOptExpr opts $
              mkLams spec_bndrs    $
              spec_app tmpl  -- The beta-redexes created by spec_app
                             -- will besimplified away by simplOptExpr
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
site.  It's rare to have such an INLINE pragma (usually INLINE Is on
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
  = CoreUnfolding { uf_tmpl         = occurAnalyseExpr expr,
                      -- See Note [Occurrence analysis of unfoldings]
                    uf_src          = src,
                    uf_is_top       = top_lvl,
                    uf_is_value     = exprIsHNF        expr,
                    uf_is_conlike   = exprIsConLike    expr,
                    uf_is_work_free = exprIsWorkFree   expr,
                    uf_expandable   = exprIsExpandable expr,
                    uf_guidance     = guidance }


