module DefaultMultiParam(plugin) where

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
import GHC.Builtin.Types (doubleTy, intTy)
import Data.Maybe (mapMaybe)

plugin :: Plugin
plugin = defaultPlugin
    { defaultingPlugin = \_ -> Just DefaultingPlugin
        { dePluginInit = pure ()
        , dePluginRun = \ _ -> defaultBinaryClassesToDoubleInt
        , dePluginStop = \ _ -> pure ()
        }
    }

-- Default every class constraint of form `C a b` to `C Double Int`
defaultBinaryClassesToDoubleInt :: WantedConstraints -> TcPluginM [DefaultingProposal]
defaultBinaryClassesToDoubleInt wanteds = pure
    [ DefaultingProposal [[(tv1, doubleTy), (tv2, intTy)]] [ct]
    | ct <- bagToList $ approximateWC True wanteds
    , Just (cls, tys) <- pure $ getClassPredTys_maybe (ctPred ct)
    , tys'@[_, _] <- pure $ filterOutInvisibleTypes (classTyCon cls) tys
    , tvs@[tv1, tv2] <- pure $ mapMaybe getTyVar_maybe tys'
    , all isMetaTyVar tvs
    ]
