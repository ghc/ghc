{-# LANGUAGE NoExperimentalDependentTypes #-}
module NativeClosedFamilyProjection (pick, pickRight, Optional(..), choose, Packet(..), choosePacket, makePacket) where

import Numeric.Natural (Natural)

pick :: (a, b) -> a
pick (x, _) = x

-- | Select the other field when both have the same type.
pickRight :: (a, a) -> a
pickRight (_, y) = y

-- | Keep the inner constructor as a separate demand.
data Optional a = Optional (Maybe a)

-- | Return the fallback only when the inner constructor is Nothing.
choose :: Optional a -> a -> a
choose (Optional (Just x)) _ = x
choose (Optional Nothing) fallback = fallback

data Packet a = Packet (Maybe a) a

choosePacket :: Packet a -> a -> a
choosePacket (Packet (Just x) _) _ = x
choosePacket (Packet Nothing _) fallback = fallback

makePacket :: Natural -> Packet Natural
makePacket n = Packet (Just n) (error "unselected packet field forced")
