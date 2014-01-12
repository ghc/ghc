{-# LANGUAGE RankNTypes, MultiParamTypeClasses #-}

-- Tests a bug spotted by Claus in which the type
-- of c3 was wrongly displayed in GHCi as
--	c3 :: C a b => a -> b
-- Should be
--	c3 :: C a b => a1 -> b

module A where

class C a b where
  c1 :: Num b => a -> b
  c2 :: (Num b,Show b) => a -> b
  c3 :: forall a. a -> b
