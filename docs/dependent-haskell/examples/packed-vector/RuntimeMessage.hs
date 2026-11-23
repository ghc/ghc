{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies #-}

module RuntimeMessage (Message, Payload (..), Allowed, Format (Configured), Permit (..), make) where

import Data.Kind (Type)

-- | The stored mode selects the payload type.
data Payload flag where
    Number :: Int -> Payload False
    Text :: String -> Payload True

-- | A message stores its mode alongside the payload.
type Message = some (flag :: Bool) | Payload flag

class Allowed (tag :: Type)
instance Allowed Bool

data Permit (tag :: Type) where
    Permit :: Allowed tag => Permit tag

class Format tag where
    type Configured tag

instance Format Bool where
    type Configured Bool = Message

instance Allowed tag => Format [tag] where
    type Configured [tag] = Message

-- | Construct the payload at the supplied mode.
make :: foreach (flag :: Bool) -> Payload flag
make False = Number 42
make True = Text "forty-two"
