{-# LANGUAGE DefaultSignatures #-}
module T5001b where

class GEnum a where
     genum :: [a]
     default genum :: [a]
     genum = undefined

instance GEnum Int where
     {-# INLINE genum #-}
