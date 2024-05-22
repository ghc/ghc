{-# LANGUAGE TypeFamilies #-}

module T24824 where

import GHC.Hs hiding (DataConCantHappen)

main :: IO ()
main = do
  let hsModule = undefined :: HsModule GhcPs
  let _ = hsmodImports $ hsModule -- warns
  let _ = hsmodImports hsModule -- does not warn
  pure ()

data S a where
  S1 :: S Int
  S2 :: { x::Int } -> S a
  S3 :: { x::Int } -> S a

-- x :: forall a. S a -> Int
-- A partial function

g :: S Bool -> Int
g s = (x @Bool) $ s

data W a where
  W1 :: !(F a) -> W a
  W2 :: { y::Int } -> W a
  W3 :: { y::Int } -> W a

data DataConCantHappen

type family F a
type instance F Bool = DataConCantHappen

h :: W Bool -> Int
h w = y @Bool $ w
