{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TemplateHaskell, TypeFamilies #-}

-- | A named sensor has an integer reading. A missing sensor has no reading.
module QuotedSensorRow (readings) where

import GHC.TypeLits (Symbol)
import TypeLambda (Fields(..), Map)

$([d|
    type SensorRow (labels :: [Maybe Symbol]) =
        Map (\label -> case label of { Nothing -> (); Just _ -> Int }) labels
    |])

row :: Fields (SensorRow '[ 'Just "temperature", 'Nothing, 'Just "humidity"])
row = Field 21 (Field () (Field 54 End))

-- | Read the three slots in declaration order.
readings :: (Int, (), Int)
readings = case row of
    Field temperature (Field missing (Field humidity End)) -> (temperature, missing, humidity)
