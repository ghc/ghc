{-# language UnliftedDatatypes #-}
{-# language BangPatterns #-}

module T20663 where

import GHC.Exts (UnliftedType)

type Gram :: UnliftedType
data Gram = Gram !Int !Int

{-# NOINLINE yeah #-}
yeah :: Gram -> (Int, Int)
yeah g = (case g of Gram a _ -> a, case g of Gram _ b -> b)

data Foo = Foo !Int !Int

{-# NOINLINE bam #-}
bam :: Foo -> (Int, Int)
bam !f = (case f of Foo a _ -> a, case f of Foo _ b -> b)

