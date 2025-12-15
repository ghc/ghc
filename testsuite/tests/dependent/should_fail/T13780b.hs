{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module T13780b where

data family Sing (a :: k)

data instance Sing (z :: Bool) =
    z ~ False => SFalse
  | z ~ True  => STrue
