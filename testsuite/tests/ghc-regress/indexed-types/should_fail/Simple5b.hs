{-# OPTIONS -findexed-types #-}

module ShouldFail where

class C3 a where
  data    S3  a :: *
  newtype S3n a :: *
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

-- must fail: Can't match Int against Char
bar3wrong' D3Int  = 1
bar3wrong' D3Char = 'a'
