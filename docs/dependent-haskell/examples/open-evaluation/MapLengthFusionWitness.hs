{-# LANGUAGE GADTs #-}
module MapLengthFusionWitness where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (build)
import qualified HaskLib.List as H
import qualified MapLengthFusionDemand as Example

-- | Ask for the witness on the same filtered source used by the counts.
{-# NOINLINE witness #-}
witness :: (Bool -> Bool) -> ()
witness p =
    let source = build Example.producer
        f = Example.mapper source
        xs = filter p source
    in case H.lengthMap f xs of Refl -> ()

-- | Observe the two counts, the shared input and the witness in one executable.
main :: IO ()
main = do
    Example.main
    result <- try (evaluate (witness (const False))) :: IO (Either ErrorCall ())
    putStrLn (either (const "bottom") (const "Refl") result)
