{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module T17267 where

class a ~ b => Thing a b
instance a ~ b => Thing a b

unsafeCoerce :: forall a b. a -> b
unsafeCoerce a = oops a where
  oops :: (a ~ b => Thing a b) => (Thing a b => r) -> r
  oops r = r


{-
-- Now rejected
class C a b where
  op :: a -> b

uc :: a -> b
uc = oops where
  oops :: (C a b => C a b) => a -> b
  oops x = op x
-}

{-
-- Now rejected
uc :: a -> b
uc = oops where
  oops :: (a ~ b => a ~ b) => a -> b
  oops x = x
-}


{-
-- Now rejected
class C a b where
  op :: a -> b

class C a b => Thing a b
instance C a b => Thing a b

unsafeCoerce :: forall a b. a -> b
unsafeCoerce a = oops (op a :: Thing a b => b)
  where
    oops :: (C a b => Thing a b) => (Thing a b => x) -> x
    oops r = r
-}

