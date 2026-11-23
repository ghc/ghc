{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, FlexibleInstances, GADTs, LambdaCase, TypeFamilies, UndecidableInstances #-}
module NativeAssociatedPayloadSumProvider (PayloadFormat (Body, Packet), Allowed, Permit (Permit), makeGlobal, makePermitted) where

import Data.Kind (Type)
import Data.Proxy (Proxy)

class Allowed (a :: Type)
instance Allowed Bool

data Permit a where
    Permit :: Allowed a => Permit a

class PayloadFormat a where
    type Body a (tag :: Bool)
    type Packet a

instance PayloadFormat Bool where
    type Body Bool False = Int
    type Body Bool True = Char
    type Packet Bool = some (tag :: Bool) | Body Bool tag

instance Allowed a => PayloadFormat [a] where
    type Body [a] False = Int
    type Body [a] True = Char
    type Packet [a] = some (tag :: Bool) | Body [a] tag

-- | Build the payload selected by the globally configured tag.
makeGlobal :: foreach (tag :: Bool) -> Body Bool tag
makeGlobal False = 42
makeGlobal True = 'x'

-- | Build the payload selected by a tag under an in-scope permit.
makePermitted :: Allowed a => Proxy a -> foreach (tag :: Bool) -> Body [a] tag
makePermitted _ = \case
    False -> 43
    True -> 'y'

#ifdef WRONG_OWNER
class Other a where
    type OtherPacket a

instance Allowed a => Other [a] where
    type OtherPacket [a] = some (tag :: Bool) | Body [a] tag
#endif
