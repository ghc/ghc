{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
module Bug where

newtype List a = MkList [a]
deriving via forall a. [a] instance Eq a => Eq (List a)

class C a where
  m :: forall b. a -> b -> b
  m _ = undefined @_ @a `seq` id @b

instance C [a] where
  m :: forall b. [a] -> b -> b
  m _ = undefined @_ @a `seq` id @b
