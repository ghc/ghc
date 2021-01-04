{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module T13393 ( ) where

import Control.Monad.Trans.RWS.Strict (RWST)
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Word (Word16)

data Rate
data Audio (sampleRate :: Rate) (channelLayout :: Type) (encoding :: Type)
data EncodeResult = MkEncodeResult
  { encodeResultLeftOverInput :: !(Maybe [Word16])
  }
data EncodeFailure
data AacEncErrorCode
data Aac (aot :: AacCodec)
data AacCodec
newtype AacEncSt (rate :: Rate) channels (codec :: AacCodec) = MkAacEncSt
  { _leftOvers :: Maybe [Word16]
  }

-- makeLenses ''AacEncSt

type Iso s t a b = forall f. (Functor f) => (a -> f b) -> s -> (f t)
instance (Monad m, Monoid w) => MonadState s (RWST r w s m) where

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt x = fmap bt . x . sa
{-# INLINE iso #-}

leftOvers ::
  forall rate_a750
         channels_a753
         codec_a757
         rate_aaYK
         channels_aaYL
         codec_aaYM.
  Iso (AacEncSt rate_a750 channels_a753 codec_a757) (AacEncSt rate_aaYK channels_aaYL codec_aaYM) (Maybe [Word16]) (Maybe [Word16])
leftOvers = (iso (\ (MkAacEncSt x_aaYN) -> x_aaYN)) MkAacEncSt
{-# INLINE leftOvers #-}

type ASetter s t a b = (a -> Identity b) -> s -> Identity t
class Monad m => MonadState s m | m -> s where

(.=) :: MonadState s m => ASetter s s a b -> b -> m ()
l .= b = undefined
{-# INLINE (.=) #-}

type AacEncT rate channels codec m a = RWST Int () (AacEncSt rate channels codec) m a

encodeLinearToAac
  :: AacEncT rate channels codec IO (Either EncodeFailure (Maybe (Audio rate channels (Aac codec))))
encodeLinearToAac = do
  mapM putBackLeftOverInputAndReturnOutput undefined
  undefined
  where
    putBackLeftOverInputAndReturnOutput (MkEncodeResult x) = do
      leftOvers .= x
      undefined
