{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
module T17442 where

import Control.Monad
import GHC.Arr (Ix(..))
import GHC.Base (getTag)
import GHC.Exts

data family D
data instance D = MkD
  deriving (Eq, Ord, Show)

instance Ix D where
  range (a, b) =
    let a# = getTag a
        b# = getTag b
    in map (\(I# i#) -> tagToEnum# i# :: D)
           (enumFromTo (I# a#) (I# b#))
  unsafeIndex (a, _) c =
    let a# = getTag a
        c# = getTag c
        d# = c# -# a#
    in I# d#
  inRange (a, b) c =
    let a# = getTag a
        b# = getTag b
        c# = getTag c
    in tagToEnum# (c# >=# a#) && tagToEnum# (c# <=# b#)

shouldBe :: (Eq a, Show a) => a -> a -> IO ()
shouldBe x y =
  unless (x == y) $ fail $ show x ++ " is not equal to " ++ show y

ixLaws :: (Ix a, Show a) => a -> a -> a -> IO ()
ixLaws l u i = do
    inRange (l,u) i                 `shouldBe` elem i (range (l,u))
    range (l,u) !! index (l,u) i    `shouldBe` i
    map (index (l,u)) (range (l,u)) `shouldBe` [0..rangeSize (l,u)-1]
    rangeSize (l,u)                 `shouldBe` length (range (l,u))

dIsLawfulIx :: IO ()
dIsLawfulIx = ixLaws MkD MkD MkD
