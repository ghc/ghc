{-# LANGUAGE GHC2021, DataKinds, MagicHash, TypeFamilies, TypeData #-}

module DataToTagFails where

import Data.Functor.Identity
import GHC.Exts
import Prelude hiding (Left)

hiddenConstructor1 :: Either a b -> Int#
hiddenConstructor1 = dataToTag#

hiddenConstructor2 :: [a] -> Int#
hiddenConstructor2 = dataToTag#

hiddenConstructor3 :: (a, b, c) -> Int#
hiddenConstructor3 = dataToTag#

newtypesDon'tWork :: Identity (Maybe a) -> Int#
newtypesDon'tWork = dataToTag#

data family X (a :: Bool)
data instance X False = X1 | X2 Int
data instance X True where {}

multipleDataInstances :: X a -> Int#
multipleDataInstances = dataToTag#

functionsDon'tWork :: Int
-- from test T12076sat, broken by the DataToTag change
functionsDon'tWork = I# (dataToTag# timesWord#)

type data Y = Y1 | Y2

typeDataDoesn'tWork :: Int
typeDataDoesn'tWork = I# (dataToTag# (undefined :: Y))
