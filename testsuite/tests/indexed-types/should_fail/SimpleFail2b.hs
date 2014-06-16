{-# LANGUAGE TypeFamilies #-}

class C a where
  data Sd a :: *
  data Sn a :: *
  type St a :: *

instance C Int where
  data    Sd Int = SdC1 Char	-- must fail: conflicting
  data    Sd Int = SdC2 Char	--            declarations
  newtype Sn Int = SnC Char
  type    St Int = Char
