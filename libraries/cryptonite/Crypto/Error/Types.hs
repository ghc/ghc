-- |
-- Module      : Crypto.Error.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : Good
--
-- Cryptographic Error enumeration and handling
--
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
module Crypto.Error.Types
    ( CryptoError(..)
    , CryptoFailable(..)
    , throwCryptoErrorIO
    , throwCryptoError
    , onCryptoFailure
    , eitherCryptoError
    , maybeCryptoError
    ) where

import qualified Control.Exception as E
import           Data.Data

import           Basement.Monad (MonadFailure(..))

-- | Enumeration of all possible errors that can be found in this library
data CryptoError =
    -- symmetric cipher errors
      CryptoError_KeySizeInvalid
    | CryptoError_IvSizeInvalid
    | CryptoError_SeedSizeInvalid
    | CryptoError_AEADModeNotSupported
    -- public key cryptography error
    | CryptoError_SecretKeySizeInvalid
    | CryptoError_SecretKeyStructureInvalid
    | CryptoError_PublicKeySizeInvalid
    | CryptoError_SharedSecretSizeInvalid
    -- elliptic cryptography error
    | CryptoError_EcScalarOutOfBounds
    | CryptoError_PointSizeInvalid
    | CryptoError_PointFormatInvalid
    | CryptoError_PointFormatUnsupported
    | CryptoError_PointCoordinatesInvalid
    | CryptoError_ScalarMultiplicationInvalid
    -- Message authentification error
    | CryptoError_MacKeyInvalid
    | CryptoError_AuthenticationTagSizeInvalid
    -- Prime generation error
    | CryptoError_PrimeSizeInvalid
    -- Parameter errors
    | CryptoError_SaltTooSmall
    | CryptoError_OutputLengthTooSmall
    | CryptoError_OutputLengthTooBig
    deriving (Show,Eq,Enum,Data)

instance E.Exception CryptoError

-- | A simple Either like type to represent a computation that can fail
--
-- 2 possibles values are:
--
-- * 'CryptoPassed' : The computation succeeded, and contains the result of the computation
--
-- * 'CryptoFailed' : The computation failed, and contains the cryptographic error associated
--
data CryptoFailable a =
      CryptoPassed a
    | CryptoFailed CryptoError
    deriving (Show)

instance Eq a => Eq (CryptoFailable a) where
    (==) (CryptoPassed a)  (CryptoPassed b)  = a == b
    (==) (CryptoFailed e1) (CryptoFailed e2) = e1 == e2
    (==) _                 _                 = False

instance Functor CryptoFailable where
    fmap f (CryptoPassed a) = CryptoPassed (f a)
    fmap _ (CryptoFailed r) = CryptoFailed r

instance Applicative CryptoFailable where
    pure a     = CryptoPassed a
    (<*>) fm m = fm >>= \p -> m >>= \r2 -> return (p r2)
instance Monad CryptoFailable where
    return = pure
    (>>=) m1 m2 = do
        case m1 of
            CryptoPassed a -> m2 a
            CryptoFailed e -> CryptoFailed e

instance MonadFailure CryptoFailable where
    type Failure CryptoFailable = CryptoError
    mFail = CryptoFailed

-- | Throw an CryptoError as exception on CryptoFailed result,
-- otherwise return the computed value
throwCryptoErrorIO :: CryptoFailable a -> IO a
throwCryptoErrorIO (CryptoFailed e) = E.throwIO e
throwCryptoErrorIO (CryptoPassed r) = return r

-- | Same as 'throwCryptoErrorIO' but throw the error asynchronously.
throwCryptoError :: CryptoFailable a -> a
throwCryptoError (CryptoFailed e) = E.throw e
throwCryptoError (CryptoPassed r) = r

-- | Simple 'either' like combinator for CryptoFailable type
onCryptoFailure :: (CryptoError -> r) -> (a -> r) -> CryptoFailable a -> r
onCryptoFailure onError _         (CryptoFailed e) = onError e
onCryptoFailure _       onSuccess (CryptoPassed r) = onSuccess r

-- | Transform a CryptoFailable to an Either
eitherCryptoError :: CryptoFailable a -> Either CryptoError a
eitherCryptoError (CryptoFailed e) = Left e
eitherCryptoError (CryptoPassed a) = Right a

-- | Transform a CryptoFailable to a Maybe
maybeCryptoError :: CryptoFailable a -> Maybe a
maybeCryptoError (CryptoFailed _) = Nothing
maybeCryptoError (CryptoPassed r) = Just r
