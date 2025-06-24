{-# LANGUAGE TypeFamilies #-}

-- | Module
module T26114 where

-- | C1
class C1 t where
  type C2 t

-- | A
data A = A

instance C1 A where
  type C2 A = B

-- | B
data B = B

instance C1 B where
  type C2 B = C

-- | C
data C = C
