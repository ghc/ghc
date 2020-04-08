{-# LANGUAGE LinearTypes, GADTs #-}
newtype A where
   A :: Int -> A
