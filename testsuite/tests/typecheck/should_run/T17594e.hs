{-# LANGUAGE TypeAbstractions #-}

import Data.Int

higherRank :: (forall a. (Num a, Bounded a) => a -> a) -> (Int8, Int16)
higherRank f = (f 42, f 42)

ex :: (Int8, Int16)
ex = higherRank (\ @a x -> maxBound @a - x )

main = print ex
