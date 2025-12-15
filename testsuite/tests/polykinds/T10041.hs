{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE GADTs #-}

module T10041 where

data family Sing (a :: k)
data instance Sing (xs :: [k]) where
  SNil :: Sing '[]

class SingI (a :: ĸ) where