{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wunused-top-binds #-}

module T18470 () where

type family Closed x where
  Closed Int = Bool
