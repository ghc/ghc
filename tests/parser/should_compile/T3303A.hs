
module T3303A where

{-# DEPRECATED foo
         ["This is a multi-line",
          "deprecation message",
          "for foo"] #-}
foo :: Int
foo = 4

-- Empty list should work too (#11044).
{-# DEPRECATED foo2 [] #-}

foo2 :: Int
foo2 = 4
