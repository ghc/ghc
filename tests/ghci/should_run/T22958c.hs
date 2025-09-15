-- Test extracted from text-builder-linear, ticket #23355
{-# LANGUAGE UnliftedDatatypes #-}
module Main (main) where

import GHC.Exts (UnliftedType)

type Buffer :: UnliftedType
data Buffer = Buffer

main :: IO ()
main = case i Buffer of Buffer -> putStrLn "good"

{-# NOINLINE i #-}
i :: forall (a :: UnliftedType). a -> a
i x = x
