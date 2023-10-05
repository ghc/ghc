{-# LANGUAGE FlexibleInstances, GADTs #-}
module T5858 where

class InferOverloaded a where
  infer :: a -> String

-- instance (t1 ~ String, t2 ~ String) => InferOverloaded (t1,t2) where
instance (t1 ~ String) => InferOverloaded (t1,t1) where
  infer = show . fst
  
foo = infer ([],[])
