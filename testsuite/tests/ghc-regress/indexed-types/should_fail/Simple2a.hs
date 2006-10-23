{-# OPTIONS -findexed-types #-}

class C a where
  data    Sd a :: *
  newtype Sn a :: *
  type    St a :: *

instance C Int where
  data    Sd a :: *   -- must fail: multiple declarations
  data    Sd Int = SdC Char
  newtype Sn Int = SnC Char
  type    St Int = Char
