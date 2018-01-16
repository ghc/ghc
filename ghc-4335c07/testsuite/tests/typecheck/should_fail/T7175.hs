{-# LANGUAGE TypeFamilies, GADTs #-}

module T7175 where

type family F a

data G1 a where
   G1C :: F Int

data G2 a where
   G2C :: F Int
