{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

class Sing a where sing :: Bool
instance Sing Int  where sing = True
instance Sing Char where sing = False

f :: forall a. Sing a => Int -> (Bool -> Bool) -> Bool
f 0 k = k (sing @a)
f n k = f @a (n-1) k

g :: forall a. Sing a => Int -> (Bool -> Bool) -> Bool
g = case sing @a of
      True  -> f @Int
      False -> f @a
{-# NOINLINE g #-}

main = print $ g @Int 0 id -- True is the correct output
