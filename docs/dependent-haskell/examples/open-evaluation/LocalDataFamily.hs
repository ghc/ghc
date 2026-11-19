{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures, TypeFamilies #-}
module LocalDataFamily (result) where

import Data.Kind (Type)

-- | Each instance supplies a stamp and can inspect it.
class Gate a where
    data Stamp a
    enabled :: a -> Bool

-- | The token selects all three family instances.
data Token = Token

-- | The stamp and the packet store the same Boolean payload.
type Payload = Bool

-- | Choose the field type of a packet.
type family Flag a
type instance Flag Token = Payload

-- | Store a payload for the selected token.
data family Packet a
data instance Packet Token = Packet { contents :: Flag Token } deriving Eq

instance Gate Token where
    data Stamp Token = StampToken { marked :: Payload } deriving (Eq, Show)
    enabled _ = (marked (StampToken True) :: Kept Bool)

-- | Typechecking the ignored expression needs both derived instances.
keep :: forall a. a -> a
keep x = const x (StampToken True == StampToken False &&
                  Packet True == Packet False && contents (Packet True))

-- | The instance method checks its result using this local producer.
type Kept :: Type -> Type
type Kept a = LocalDataFamily.keep a

-- | Run the method and the derived instances from an ordinary client.
result :: Kept (Bool, Bool, String)
result = (enabled Token, Packet True == Packet False, show (StampToken False))
