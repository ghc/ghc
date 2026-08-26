module GHC.Driver.Config.Core.Opt.Simplify
  ( initSimplifyExprOpts
  , initSimplifyOpts
  , initSimplMode
  , initGentleSimplMode
  ) where

import GHC.Prelude

import GHC.Core.Rules ( RuleBase )
import GHC.Core.Opt.Pipeline.Types ( CoreToDo(..) )
import GHC.Core.Opt.Simplify ( SimplifyExprOpts(..), SimplifyOpts(..) )
import GHC.Core.Opt.Simplify.Env ( FloatEnable(..), SimplMode(..), SimplPhase(..) )
import GHC.Core.Opt.Simplify.Monad ( TopEnvConfig(..) )

import GHC.Driver.Config ( initOptCoercionOpts )
import GHC.Driver.Config.Core.Lint ( initLintPassResultConfig )
import GHC.Driver.Config.Core.Rules ( initRuleOpts )
import GHC.Driver.Config.Core.Opt.Arity ( initArityOpts )
import GHC.Driver.DynFlags ( DynFlags(..), GeneralFlag(..), gopt )

import GHC.Runtime.Context ( InteractiveContext(..) )

import GHC.Types.InlinePragma ( CompilerPhase(..) )
import GHC.Types.Var ( Var )

initSimplifyExprOpts :: DynFlags -> InteractiveContext -> SimplifyExprOpts
initSimplifyExprOpts dflags ic = SimplifyExprOpts
  { se_fam_inst = snd $ ic_instances ic
  , se_mode = (initSimplMode dflags InitialPhase "GHCi")
    { sm_inline = False
      -- Do not do any inlining, in case we expose some
      -- unboxed tuple stuff that confuses the bytecode
      -- interpreter
    }
  , se_top_env_cfg = TopEnvConfig
    { te_history_size = historySize dflags
    , te_tick_factor = simplTickFactor dflags
    }
  }

initSimplifyOpts :: DynFlags -> [Var] -> Int -> SimplMode -> RuleBase -> SimplifyOpts
initSimplifyOpts dflags extra_vars iterations mode hpt_rule_base = let
  -- This is a particularly ugly construction, but we will get rid of it in !8341.
  opts = SimplifyOpts
    { so_dump_core_sizes = not $ gopt Opt_SuppressCoreSizes dflags
    , so_iterations      = iterations
    , so_mode            = mode
    , so_pass_result_cfg = if gopt Opt_DoCoreLinting dflags
                           then Just $ initLintPassResultConfig dflags extra_vars
                                                            (CoreDoSimplify opts)
                           else Nothing
    , so_hpt_rules       = hpt_rule_base
    , so_top_env_cfg     = TopEnvConfig { te_history_size = historySize dflags
                                        , te_tick_factor = simplTickFactor dflags }
    }
  in opts

initSimplMode :: DynFlags -> CompilerPhase -> String -> SimplMode
initSimplMode dflags phase name = SimplMode
  { sm_names = [name]
  , sm_phase = SimplPhase phase
  , sm_rules = gopt Opt_EnableRewriteRules dflags
  , sm_eta_expand = gopt Opt_DoLambdaEtaExpansion dflags
  , sm_cast_swizzle = True
  , sm_inline = True
  , sm_uf_opts = unfoldingOpts dflags
  , sm_case_case = True
  , sm_pre_inline = gopt Opt_SimplPreInlining dflags
  , sm_float_enable = floatEnable dflags
  , sm_do_eta_reduction = gopt Opt_DoEtaReduction dflags
  , sm_arity_opts = initArityOpts dflags
  , sm_rule_opts = initRuleOpts dflags
  , sm_case_folding = gopt Opt_CaseFolding dflags
  , sm_case_merge = gopt Opt_CaseMerge dflags
  , sm_co_opt_opts = initOptCoercionOpts dflags
  }

initGentleSimplMode :: DynFlags -> SimplMode
initGentleSimplMode dflags = (initSimplMode dflags InitialPhase "Gentle")
  { -- Don't do case-of-case transformations.
    -- This makes full laziness work better
    -- See Note [Case-of-case and full laziness]
    sm_case_case = False
  }

floatEnable :: DynFlags -> FloatEnable
floatEnable dflags =
  case (gopt Opt_LocalFloatOut dflags, gopt Opt_LocalFloatOutTopLevel dflags) of
    (True, True) -> FloatEnabled
    (True, False)-> FloatNestedOnly
    (False, _)   -> FloatDisabled


{- Note [Case-of-case and full laziness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Case-of-case can hide opportunities for let-floating (full laziness).
For example
   rec { f = \y. case (expensive x) of (a,b) -> blah }
We might hope to float the (expensive x) out of the \y-loop.
But if we inline `expensive` we might get
   \y. case (case x of I# x' -> body) of (a,b) -> blah
Now if we do case-of-case we get
   \y. case x if I# x2 ->
       case body of (a,b) -> blah

Sadly, at this point `body` mentions `x2`, so we can't float it out of the
\y-loop.

Solution: don't do case-of-case in the "gentle" simplification phase that
precedes the first float-out transformation.  Implementation:

  * `sm_case_case` field in SimplMode

  * If `sm_case_case` is False (tested via `seCaseCase`), then:
    (COC1) When simplifying (case scrut of alts), do not make a Select continuation.
           This happens in the `Case` case of `simplExprF`.
    (COC2) When simplifying (f arg), where `f` is strict, do not make a StrictArg
           continuation.  See the `ApplyToVal` case of `rebuildArg`.

    ...more to come...
           in GHC.Core.Opt.Simplify.Iteration.rebuildCall.

    (COC3) This applies equally to the case-of-runRW# transformation:
           case (runRW# (\s. body)) of (a,b) -> blah
           --->
           runRW# (\s. case body of (a,b) -> blah)
           Again, don't do this when `sm_case_case` is off.
           See #25055 for a motivating example.

Note [Join points with -fno-case-of-case]   FIX ME
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Supose case-of-case is switched off, and we are simplifying

    case (join j x = <j-rhs> in
          case y of
             A -> j 1
             B -> j 2
             C -> e) of <outer-alts>

Usually, we'd push the outer continuation (case . of <outer-alts>) into
both the RHS and the body of the join point j.  But since we aren't doing
case-of-case we may then end up with this totally bogus result

    join x = case <j-rhs> of <outer-alts> in
    case (case y of
             A -> j 1
             B -> j 2
             C -> e) of <outer-alts>

This would be OK in the language of the paper, but not in GHC: j is no longer
a join point.  We can only do the "push continuation into the RHS of the
join point j" if we also push the continuation right down to the /jumps/ to
j, so that it can evaporate there (trimJoinCont). Then, if we are doing
case-of-case, we'll get to:

    join x = case <j-rhs> of <outer-alts> in
    case y of
      A -> j 1
      B -> j 2
      C -> case e of <outer-alts>

which is great.

Bottom line: if case-of-case is off, we must stop pushing the continuation
inwards altogether at any join point.  Instead simplify the (join ... in ...)
with a Stop continuation, and wrap the original continuation around the
outside.  Surprisingly tricky!
-}
