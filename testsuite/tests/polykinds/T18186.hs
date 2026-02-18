{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Data.Kind (Type)

type family F :: k -> Type
type family B (a :: k) (b :: k) :: k

main :: IO ()
main =
  \(_ :: F n) -> (undefined :: B n 1)
