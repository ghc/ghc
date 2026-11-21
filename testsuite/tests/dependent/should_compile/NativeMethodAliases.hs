{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeMethodAliases (Bit(..), Turn(..)) where

data Bit = Off | On

class Turn a where
    forward, backward :: a -> a

-- Both targets stay private. Their equations stay fixed when the method changes.
turn, stay :: Bit -> Bit
turn Off = On
turn On  = Off
stay Off = Off
stay On  = On

instance Turn Bit where
    forward = turn
    backward = stay

combine :: Bit -> Bit -> Bit
combine Off b = b
combine On b = turn b

instance Semigroup Bit where
    (<>) = combine
