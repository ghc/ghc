{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DerivingViaFail4 where

class C a b where
  c :: a -> b -> Bool

instance C a a where
  c _ _ = True

newtype F1 = F1 Int
  deriving Eq via Char

newtype F2 a = MkF2 a
  deriving (C a) via forall a. a
