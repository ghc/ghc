module GHC.Core.Opt.Pipeline.Types (
    -- * Configuration of the core-to-core passes
    CorePluginPass, CoreToDo(..),
    bindsOnlyPass, pprPassDetails,
  ) where

import GHC.Prelude

import GHC.Core ( CoreProgram )
import GHC.Core.Opt.Monad ( CoreM, FloatOutSwitches )
import GHC.Core.Opt.Simplify ( SimplifyOpts(..) )

import GHC.Hs.InlinePragma  ( CompilerPhase(..) )
import GHC.Unit.Module.ModGuts
import GHC.Utils.Outputable as Outputable

{-
************************************************************************
*                                                                      *
              The CoreToDo type and related types
          Abstraction of core-to-core passes to run.
*                                                                      *
************************************************************************
-}

-- | A description of the plugin pass itself
type CorePluginPass = ModGuts -> CoreM ModGuts

bindsOnlyPass :: (CoreProgram -> CoreM CoreProgram) -> ModGuts -> CoreM ModGuts
bindsOnlyPass pass guts
  = do { binds' <- pass (mg_binds guts)
       ; return (guts { mg_binds = binds' }) }

data CoreToDo           -- These are diff core-to-core passes,
                        -- which may be invoked in any order,
                        -- as many times as you like.

  = CoreDoSimplify !SimplifyOpts
  -- ^ The core-to-core simplifier.
  | CoreDoPluginPass String CorePluginPass
  | CoreDoFloatInwards
  | CoreDoFloatOutwards FloatOutSwitches
  | CoreLiberateCase
  | CoreDoPrintCore
  | CoreDoStaticArgs
  | CoreDoCallArity
  | CoreDoExitify
  | CoreDoDemand Bool  -- Bool: Do worker/wrapper afterwards?
                       -- See Note [Don't change boxity without worker/wrapper]
  | CoreDoCpr
  | CoreDoWorkerWrapper
  | CoreDoSpecialising
  | CoreDoSpecConstr
  | CoreCSE
  | CoreDoRuleCheck CompilerPhase String   -- Check for non-application of rules
                                           -- matching this string
  | CoreDoNothing                -- Useful when building up
  | CoreDoPasses [CoreToDo]      -- lists of these things

  | CoreDesugar    -- Right after desugaring, no simple optimisation yet!
  | CoreDesugarOpt -- CoreDesugarXXX: Not strictly a core-to-core pass, but produces
                       --                 Core output, and hence useful to pass to endPass

  | CoreTidy
  | CorePrep
  | CoreAddCallerCcs
  | CoreAddLateCcs

instance Outputable CoreToDo where
  ppr (CoreDoSimplify _)       = text "Simplifier"
  ppr (CoreDoPluginPass s _)   = text "Core plugin: " <+> text s
  ppr CoreDoFloatInwards       = text "Float inwards"
  ppr (CoreDoFloatOutwards f)  = text "Float out" <> parens (ppr f)
  ppr CoreLiberateCase         = text "Liberate case"
  ppr CoreDoStaticArgs         = text "Static argument"
  ppr CoreDoCallArity          = text "Called arity analysis"
  ppr CoreDoExitify            = text "Exitification transformation"
  ppr (CoreDoDemand True)      = text "Demand analysis (including Boxity)"
  ppr (CoreDoDemand False)     = text "Demand analysis"
  ppr CoreDoCpr                = text "Constructed Product Result analysis"
  ppr CoreDoWorkerWrapper      = text "Worker Wrapper binds"
  ppr CoreDoSpecialising       = text "Specialise"
  ppr CoreDoSpecConstr         = text "SpecConstr"
  ppr CoreCSE                  = text "Common sub-expression"
  ppr CoreDesugar              = text "Desugar (before optimization)"
  ppr CoreDesugarOpt           = text "Desugar (after optimization)"
  ppr CoreTidy                 = text "Tidy Core"
  ppr CoreAddCallerCcs         = text "Add caller cost-centres"
  ppr CoreAddLateCcs           = text "Add late core cost-centres"
  ppr CorePrep                 = text "CorePrep"
  ppr CoreDoPrintCore          = text "Print core"
  ppr (CoreDoRuleCheck {})     = text "Rule check"
  ppr CoreDoNothing            = text "CoreDoNothing"
  ppr (CoreDoPasses passes)    = text "CoreDoPasses" <+> ppr passes

pprPassDetails :: CoreToDo -> SDoc
pprPassDetails (CoreDoSimplify cfg) = vcat [ text "Max iterations =" <+> int n
                                           , ppr md ]
  where
    n = so_iterations cfg
    md = so_mode cfg

pprPassDetails _ = Outputable.empty
