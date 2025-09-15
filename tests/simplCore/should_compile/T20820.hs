module T20820 ( ) where

import Prelude hiding (concat)
import Data.Semigroup           (Semigroup (sconcat, stimes))
import Data.List.NonEmpty       (NonEmpty ((:|)))

data ByteString = BS

instance Semigroup ByteString where
    (<>)    = undefined
    sconcat (b:|bs) = concat (b:bs)
    stimes  = stimesPolymorphic
instance Monoid ByteString where
    mempty  = undefined

concat :: [ByteString] -> ByteString
concat = undefined
{-# NOINLINE concat #-}

{-# RULES
"ByteString concat [] -> mempty"
   concat [] = mempty
 #-}

stimesPolymorphic :: Integral a => a -> ByteString -> ByteString
stimesPolymorphic nRaw bs = stimesInt (fromIntegral nRaw) bs

stimesInt :: Int -> ByteString -> ByteString
stimesInt _ BS = mempty
