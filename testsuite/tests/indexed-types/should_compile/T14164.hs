{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module T14164 where

data G (x :: a) = GNil | GCons (G x)

type family F (xs :: [a]) (g :: G (z :: a)) = (res :: [a]) | res -> a where
  F (x:xs) GNil         = x:xs
  F (x:xs) (GCons rest) = x:F xs rest
