{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import GHC.Exts (build)

newtype Producer = Producer (forall r. (Bool -> r -> r) -> r -> r)

-- | The list-producing map equations, with a call that remains opaque.
{-# NOINLINE noFusionMap #-}
noFusionMap :: (a -> b) -> [a] -> [b]
noFusionMap _ [] = []
noFusionMap f (x : xs) = f x : noFusionMap f xs

{-# NOINLINE consumeMap #-}
consumeMap :: Producer -> Bool
consumeMap (Producer produce) =
    foldr (\_ _ -> True) (error "empty seed") (map not (build produce))

{-# NOINLINE consumeNoFusionMap #-}
consumeNoFusionMap :: Producer -> Bool
consumeNoFusionMap (Producer produce) =
    foldr (\_ _ -> True) (error "empty seed") (noFusionMap not (build produce))

{-# NOINLINE mapEscapes #-}
mapEscapes :: Producer -> [Bool]
mapEscapes (Producer produce) = map not (build produce)

{-# NOINLINE consumeZip #-}
consumeZip :: Producer -> [Bool] -> Bool
consumeZip (Producer produce) ys =
    foldr (\_ _ -> True) (error "empty seed") (zipWith (&&) (map not (build produce)) ys)

main :: IO ()
main = do
    let withSeq = Producer (\c n -> n `seq` c True n)
        withoutSeq = Producer (\c n -> c True n)
    observe (consumeMap withSeq)
    observe (consumeNoFusionMap withSeq)
    observe (consumeMap withoutSeq)
    observe (not (null (mapEscapes withSeq)))
    observe (consumeZip withSeq [False])
  where
    observe result = do
        value <- try (evaluate result) :: IO (Either ErrorCall Bool)
        putStrLn (either (takeWhile (/= '\n') . displayException) show value)
