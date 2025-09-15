module DefaultInvalid(plugin) where

import GHC.Driver.Plugins
import GHC.Tc.Plugin
import GHC.Tc.Types
import GHC.Tc.Types.Constraint
import GHC.Builtin.Types (doubleTy)

plugin :: Plugin
plugin = defaultPlugin
    { defaultingPlugin = \_ -> Just DefaultingPlugin
        { dePluginInit = pure ()
        , dePluginRun = \ _ -> defaultInvalid
        , dePluginStop = \ _ -> pure ()
        }
    }

defaultInvalid :: WantedConstraints -> TcPluginM [DefaultingProposal]
defaultInvalid wanteds = pure [DefaultingProposal [[(tv, doubleTy) | tv <- tvs]] []]
  where
    tvs = varsOfWC wanteds

    varsOfWC WC{ wc_impl = implications } = concatMap varsOfImpl implications
    varsOfImpl Implic{ ic_wanted = wanted } = tyCoVarsOfWCList wanted
        -- Deliberately buggy to trigger error GHC-45625
