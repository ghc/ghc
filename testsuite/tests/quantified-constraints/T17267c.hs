{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module T17267c where

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

