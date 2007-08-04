{-# LANGUAGE TypeFamilies #-}

module ShouldCompile where

class C3 a where
  data S3  a		-- kind is optional
  data S3n a		-- kind is optional
  foo3  :: a -> S3 a
  foo3n :: a -> S3n a
  bar3  :: S3 a -> a
  bar3n :: S3n a -> a

instance C3 Int where
  data    S3  Int  = D3Int
  newtype S3n Int  = D3Intn ()
  foo3  _          = D3Int
  foo3n _          = D3Intn ()
  bar3  D3Int      = 1
  bar3n (D3Intn _) = 1

instance C3 Char where
  data S3 Char = D3Char
  foo3 _       = D3Char
  bar3 D3Char  = 'c'

bar3' :: S3 Char -> Char
bar3' D3Char = 'a'

instance C3 Bool where
  data S3 Bool = S3_1 | S3_2
  foo3 False = S3_1
  foo3 True  = S3_2
  bar3 S3_1 = False
  bar3 S3_2 = True

-- It's ok to omit ATs in instances, as it is ok to omit method definitions,
-- but similar to methods, "undefined" is the only inhabitant of these types,
-- then.
instance C3 Float where
  foo3 1.0 = undefined
  bar3 _   = 1.0
