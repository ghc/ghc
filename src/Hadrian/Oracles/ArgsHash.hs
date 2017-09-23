{-# LANGUAGE TypeFamilies #-}
module Hadrian.Oracles.ArgsHash (
    TrackArgument, trackAllArguments, trackArgsHash, argsHashOracle
    ) where

import Control.Monad
import Development.Shake
import Development.Shake.Classes

import Hadrian.Expression hiding (inputs, outputs)
import Hadrian.Target

-- | 'TrackArgument' is used to specify the arguments that should be tracked by
-- the @ArgsHash@ oracle. The safest option is to track all arguments, but some
-- arguments, such as @-jN@, do not change the build results, hence there is no
-- need to initiate unnecessary rebuild if they are added to or removed from a
-- command line. If all arguments should be tracked, use 'trackAllArguments'.
type TrackArgument c b = Target c b -> String -> Bool

-- | Returns 'True' for all targets and arguments, hence can be used a safe
-- default for 'argsHashOracle'.
trackAllArguments :: TrackArgument c b
trackAllArguments _ _ = True

-- | Given a 'Target' this 'Action' determines the corresponding argument list
-- and computes its hash. The resulting value is tracked in a Shake oracle,
-- hence initiating rebuilds when the hash changes (a hash change indicates
-- changes in the build command for the given target).
-- Note: for efficiency we replace the list of input files with its hash to
-- avoid storing long lists of source files passed to some builders (e.g. ar)
-- in the Shake database. This optimisation is normally harmless, because
-- argument list constructors are assumed not to examine target sources, but
-- only append them to argument lists where appropriate.
trackArgsHash :: (ShakeValue c, ShakeValue b) => Target c b -> Action ()
trackArgsHash t = do
    let hashedInputs  = [ show $ hash (inputs t) ]
        hashedTarget = target (context t) (builder t) hashedInputs (outputs t)
    void (askOracle $ ArgsHash hashedTarget :: Action Int)

newtype ArgsHash c b = ArgsHash (Target c b)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult (ArgsHash c b) = Int

-- | This oracle stores per-target argument list hashes in the Shake database,
-- allowing the user to track them between builds using 'trackArgsHash' queries.
argsHashOracle :: (ShakeValue c, ShakeValue b) => TrackArgument c b -> Args c b -> Rules ()
argsHashOracle trackArgument args = void $
    addOracle $ \(ArgsHash target) -> do
        argList <- interpret target args
        let trackedArgList = filter (trackArgument target) argList
        return $ hash trackedArgList
