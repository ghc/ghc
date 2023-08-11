module DefaultInterference(plugin) where

import GHC.Driver.Plugins
import GHC.Tc.Plugin
import GHC.Tc.Types
import GHC.Tc.Utils.TcType
import GHC.Tc.Types.Constraint
import GHC.Core.Predicate
import GHC.Tc.Solver
import GHC.Core.Type
import GHC.Core.Class
import GHC.Data.Bag
import GHC.Builtin.Types (intTy)

plugin :: Plugin
plugin = defaultPlugin
    { defaultingPlugin = \_ -> Just DefaultingPlugin
        { dePluginInit = pure ()
        , dePluginRun = \ _ -> defaultEverythingToInt
        , dePluginStop = \ _ -> pure ()
        }
    }

defaultEverythingToInt :: WantedConstraints -> TcPluginM [DefaultingProposal]
defaultEverythingToInt wanteds = pure
    [ DefaultingProposal [[(tv, intTy)]] [ct]
    | ct <- bagToList $ approximateWC True wanteds
    , Just (cls, tys) <- pure $ getClassPredTys_maybe (ctPred ct)
    , [ty] <- pure $ filterOutInvisibleTypes (classTyCon cls) tys
    , Just tv <- pure $ getTyVar_maybe ty
    , isMetaTyVar tv
    ]
