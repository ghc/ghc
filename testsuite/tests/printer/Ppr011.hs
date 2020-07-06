{-# Language DatatypeContexts #-}
{-# Language ExistentialQuantification #-}
{-# LAnguage GADTs #-}
{-# LAnguage KindSignatures #-}

data Foo = A
         | B
         | C

--         | data_or_newtype capi_ctype tycl_hdr constrs deriving
data {-# Ctype  "Foo"   "bar" #-}  F1             = F1
data {-# Ctype          "baz" #-}  Eq  a =>  F2 a = F2 a

data (Eq a,Ord a) => F3 a = F3 Int a

data F4 a = forall x y. (Eq x,Eq y) => F4 a x y
          | forall x y. (Eq x,Eq y) => F4b a x y


data G1 a :: * where
  G1A,  G1B  ::  Int  ->  G1  a
  G1C  ::  G1 a ->  G1 a
  G1D  ::  G1 a -> (Int -> G1 a)

data G2 a ::  * where
  G2A :: { g2a :: a, g2b :: Int } -> G2 a
  G2C :: Double -> G2 a



data (Eq a,Ord a) => G3 a = G3
  { g3A :: Int
  , g3B :: Bool
  , g3a :: a
  } deriving (Eq,Ord)

data G4 a :: * where
  G4A,  G4B  ::  Int  ->  G4  a
  G4C  :: {- A -} G4 {- B -}a {- C -} -> {- D -} G4 {- E -}a
  G4D  ::  {- A -}G4 {- B -}a {- C -} -> {- D -}( {- E -}Int{- F -} -> {- G -}G4 {- H -}a {- I -})

ff x =
  case  x  of
    1 -> True
    _ -> False
