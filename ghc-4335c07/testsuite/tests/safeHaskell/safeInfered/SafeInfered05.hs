{-# LANGUAGE Unsafe #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | We allow this overlap to succeed since the module is regarded as
-- `-XUnsafe`.
module SafeInfered05 where

import safe SafeInfered05_A

instance C [a] where
  f _ = "[a]"

test2 :: String
test2 = f ([1,2,3,4] :: [Int])

