{-# LANGUAGE GADTs #-}
module T16585 where
data F a where
  X :: !Int -- ^ comment
    -> F Int

