{-# LANGUAGE ExistentialQuantification #-}
module Main (main) where

main :: IO ()
main = r `seq` return ()

r :: Rec
r = Rec{ a = error "xxx", b = 3, c = True }

class C t
instance C Bool

data Rec = forall t. C t => Rec
  { a :: ()
  , b :: !Int
  , c :: t
  }
