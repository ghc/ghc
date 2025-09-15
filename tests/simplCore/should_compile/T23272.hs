module T23272 where

class C a where
instance C () where

bug :: (forall a. C a => a -> a) -> ()
bug g = f ()
  where
    f x = seq (g x) undefined
