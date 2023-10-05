{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}

module T15175 (
    gun,        -- :: Position2 -> Object
) where

import Control.Arrow
import Control.Category (Category)

data Point2 a = RealFloat a => Point2 !a !a

gun :: Point2 Double -> Object
gun (Point2 x0 y0) = proc (ObjInput {oiGameInput = gi}) -> do
    (Point2 xd _) <- ptrPos -< gi               -- This line can't be removed

    let x = undefined
        v = undefined
        fire = undefined :: Double

    returnA -< ObjOutput {
                   ooSpawnReq    =
                       fire `tag` [missile (Point2 x (y0 + (0/2)))
                                           (vector2 v (200 :: Double))]
               }

vector2 = undefined

tag = undefined

ptrPos = undefined

missile = undefined

-- | Creates a feedback loop without delay.
instance Category SF where

instance ArrowLoop SF where

instance Arrow SF where

data SF' a b where
    SF' :: !(DTime -> a -> Transition a b) -> SF' a b

type Transition a b = (SF' a b, b)

data SF a b = SF {sfTF :: a -> Transition a b}

type DTime = Double     -- [s]

data Event a = Event a deriving (Show)

type Object = SF ObjInput ObjOutput

data ObjInput = ObjInput {
    oiGameInput :: GameInput
}

data ObjOutput = ObjOutput {
    ooSpawnReq    :: Event [Object]
}

data GameInput = GameInput
