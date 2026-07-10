{-# LANGUAGE ViewPatterns #-}

-- | The endpoints on the cloud server
module Development.Shake.Internal.History.Bloom(
    Bloom, bloomTest, bloomCreate
    ) where

import Data.Word
import Data.Bits
import Data.Hashable
import Data.Semigroup
import Foreign.Storable
import Foreign.Ptr
import Prelude


-- | Given an Int hash we store
data Bloom a = Bloom
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    deriving (Eq,Show)

instance Storable (Bloom a) where
    sizeOf _ = 4 * sizeOf (0 :: Word64)
    alignment _ = alignment (0 :: Word64)
    peek (castPtr -> ptr) = Bloom <$> peekElemOff ptr 0 <*> peekElemOff ptr 1 <*> peekElemOff ptr 2 <*> peekElemOff ptr 3
    poke (castPtr -> ptr) (Bloom x1 x2 x3 x4) = do
        pokeElemOff ptr 0 x1
        pokeElemOff ptr 1 x2
        pokeElemOff ptr 2 x3
        pokeElemOff ptr 3 x4

instance Semigroup (Bloom a) where
    Bloom x1 x2 x3 x4 <> Bloom y1 y2 y3 y4 =
        Bloom (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4)

instance Monoid (Bloom a) where
    mempty = Bloom 0 0 0 0
    mappend = (<>)

-- Should the cloud need to know about Key's? It only needs to do Eq on them...
-- If you Key has a smart Eq your build tree might be more diverse
-- Have the Id resolved in Server.

bloomTest :: Hashable a => Bloom a -> a -> Bool
bloomTest bloom x = bloomCreate x <> bloom == bloom

bloomCreate :: Hashable a => a -> Bloom a
bloomCreate (fromIntegral . hash -> x) =
    Bloom (f 1) (f 2) (f 3) (f 4)
    where f i = x `xor` rotate x i
