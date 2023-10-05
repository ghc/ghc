{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import GHC.Exts
import Data.Type.Equality

type family F x :: Constraint
type instance F Int = (?x :: String)

data Box where MkBox :: (?x :: String) => Box
data Box2 a where MkBox2 :: F a => Box2 a

f :: Box2 a -> Box -> a :~: Int -> String
f MkBox2 MkBox Refl = ?x

main :: IO ()
main = do { let mb  = let ?x = "right"  in MkBox
          ; let mb2 = let ?x = "wrong" in MkBox2
          ; print (f mb2 mb Refl) }
