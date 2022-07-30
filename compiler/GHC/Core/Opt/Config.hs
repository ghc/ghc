module GHC.Core.Opt.Config (
    -- * Configuration of the core-to-core passes
    CorePluginPass, CoreToDo(..),
    pprPassDetails,
  ) where

import GHC.Prelude

import GHC.Core.Opt.CallerCC ( CallerCCOpts )
import GHC.Core.Opt.DmdAnal ( DmdAnalOpts )
import GHC.Core.Opt.LiberateCase ( LibCaseOpts )
import GHC.Core.Opt.RuleCheck ( RuleCheckOpts )
import GHC.Core.Opt.Simplify ( SimplifyOpts(..) )
import GHC.Core.Opt.Specialise.Config ( SpecialiseOpts(..) )
import GHC.Core.Opt.SpecConstr ( SpecConstrOpts )
import GHC.Core.Opt.WorkWrap ( WwOpts )

import GHC.Core.Opt.FloatOutSwitches ( FloatOutSwitches )

import GHC.Platform ( Platform )
import GHC.Plugins.Monad ( CoreM )
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

data CoreToDo           -- These are diff core-to-core passes,
                        -- which may be invoked in any order,
                        -- as many times as you like.

  = -- | The core-to-core simplifier.
    CoreDoSimplify !SimplifyOpts
  | CoreDoPluginPass String CorePluginPass
  | CoreDoFloatInwards !Platform
  | CoreDoFloatOutwards !FloatOutSwitches
  | CoreLiberateCase !LibCaseOpts
  | CoreDoPrintCore
  | CoreDoStaticArgs
  | CoreDoCallArity
  | CoreDoExitify
  | CoreDoDemand !DmdAnalOpts
  | CoreDoCpr
  | CoreDoWorkerWrapper !WwOpts
  | CoreDoSpecialising (PrintUnqualified -> SpecialiseOpts)
  | CoreDoSpecConstr !SpecConstrOpts
  | CoreCSE
  | CoreDoRuleCheck !RuleCheckOpts
  | -- | Useful when building up
    CoreDoNothing
  | -- | lists of these things
    CoreDoPasses [CoreToDo]

  | CoreAddCallerCcs !CallerCCOpts
  | CoreAddLateCcs !Bool -- ^ '-fprof-count-entries'

instance Outputable CoreToDo where
  ppr (CoreDoSimplify _)       = text "Simplifier"
  ppr (CoreDoPluginPass s _)   = text "Core plugin: " <+> text s
  ppr (CoreDoFloatInwards _)   = text "Float inwards"
  ppr (CoreDoFloatOutwards f)  = text "Float out" <> parens (ppr f)
  ppr (CoreLiberateCase _)     = text "Liberate case"
  ppr CoreDoStaticArgs         = text "Static argument"
  ppr CoreDoCallArity          = text "Called arity analysis"
  ppr CoreDoExitify            = text "Exitification transformation"
  ppr (CoreDoDemand {})        = text "Demand analysis"
  ppr CoreDoCpr                = text "Constructed Product Result analysis"
  ppr (CoreDoWorkerWrapper _)  = text "Worker Wrapper binds"
  ppr (CoreDoSpecialising _)   = text "Specialise"
  ppr (CoreDoSpecConstr _)     = text "SpecConstr"
  ppr CoreCSE                  = text "Common sub-expression"
  ppr (CoreAddCallerCcs _)     = text "Add caller cost-centres"
  ppr (CoreAddLateCcs _)       = text "Add late core cost-centres"
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
