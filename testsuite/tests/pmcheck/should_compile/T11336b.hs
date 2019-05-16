{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Derived from T11336. Note that the pattern synonym makes it so that the
-- pattern match checker can't see any complete sets: It only sees @s a@
-- instead of @Proxy a@ for the match in 'PProxy'.
module Bug where

import Data.Proxy

class Prj s where
  prj :: Proxy a -> s a

instance Prj Proxy where
  prj = id

pattern PProxy :: Prj s => s a -> Proxy a
pattern PProxy s <- (prj -> s)

-- | Although this is technically a complete match, the pattern match checker
-- can't in general look through the pattern synonym. So, it should warn that
-- some pattern wasn't matched. It should still flag the redundant second
-- clause, though!
fun :: Proxy a -> ()
fun (PProxy Proxy) = ()
fun (PProxy Proxy) = ()
