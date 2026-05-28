{-# LANGUAGE GADTs #-}
module Test10309 where

data H1 a b where
  C3 :: (Num a) => { field :: a -- ^ hello docs
                   } -> H1 Int Int

data H2 a b where
  { -- a comment, no leading semi (yet)
  C4 :: (Num a) => { fielda :: a -- ^ hello docs2
                   } -> H2 Int Int
  ; -- another comment
  }

data H3 a =
  H3 { fieldr :: a -- ^ hello docs3
     } deriving Eq
