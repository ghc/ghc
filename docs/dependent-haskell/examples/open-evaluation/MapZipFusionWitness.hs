{-# LANGUAGE GADTs #-}
module MapZipFusionWitness (main) where

import Control.Exception (ErrorCall, evaluate, try)
import GHC.Exts (build)
import HaskLib (lengthMapZip, (:~:)(Refl))
import MapZipFusionCounts (mapper, producer, report)

-- | Observe whether the installed law returns its equality constructor.
{-# NOINLINE witness #-}
witness :: (Bool -> Bool) -> [()] -> ()
witness p ys =
    let source = build producer
        f = mapper source
        xs = filter p source
    in case lengthMapZip f xs ys of Refl -> ()

-- | Run the independent counts, shared counts and witness on two input shapes.
main :: IO ()
main = mapM_ run [[()], []]
  where
    run ys = do
        let p = const False
        report p ys
        proof <- try (evaluate (witness p ys)) :: IO (Either ErrorCall ())
        putStrLn (either (const "bottom") (const "Refl") proof)
