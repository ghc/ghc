{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -fno-specialise-incoherents #-}
module T25160 where

class C a where
  op :: a -> String

instance {-# OVERLAPPABLE #-} C a where
  op _ = "C a"
  {-# NOINLINE op #-}

instance {-# INCOHERENT #-} C () where
  op _ = "C ()"
  {-# NOINLINE op #-}

-- | Inhibit inlining, but keep specialize-ability
large :: a -> a
large x = x
{-# NOINLINE large #-}

bar :: C a => a -> String
bar x = large (large (large (large (large (large (large (large (large (large (large (large (large (large (op x))))))))))))))

{-# SPECIALISE bar :: a -> String #-}
-- The RULE for this specialisation was bogus!
