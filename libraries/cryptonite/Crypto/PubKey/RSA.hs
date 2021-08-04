-- |
-- Module      : Crypto.PubKey.RSA
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
module Crypto.PubKey.RSA
    ( Error(..)
    , PublicKey(..)
    , PrivateKey(..)
    , Blinder(..)
    -- * Generation function
    , generateWith
    , generate
    , generateBlinder
    ) where

import Crypto.Random.Types
import Crypto.Number.ModArithmetic (inverse, inverseCoprimes)
import Crypto.Number.Generate (generateMax)
import Crypto.Number.Prime (generatePrime)
import Crypto.PubKey.RSA.Types

{-
-- some bad implementation will not serialize ASN.1 integer properly, leading
-- to negative modulus.
-- TODO : Find a better place for this
toPositive :: Integer -> Integer
toPositive int
    | int < 0   = uintOfBytes $ bytesOfInt int
    | otherwise = int
  where uintOfBytes = foldl (\acc n -> (acc `shiftL` 8) + fromIntegral n) 0
        bytesOfInt :: Integer -> [Word8]
        bytesOfInt n = if testBit (head nints) 7 then nints else 0xff : nints
          where nints = reverse $ plusOne $ reverse $ map complement $ bytesOfUInt (abs n)
                plusOne []     = [1]
                plusOne (x:xs) = if x == 0xff then 0 : plusOne xs else (x+1) : xs
                bytesOfUInt x = reverse (list x)
                  where list i = if i <= 0xff then [fromIntegral i] else (fromIntegral i .&. 0xff) : list (i `shiftR` 8)
-}

-- | Generate a key pair given p and q.
--
-- p and q need to be distinct prime numbers.
--
-- e need to be coprime to phi=(p-1)*(q-1). If that's not the
-- case, the function will not return a key pair.
-- A small hamming weight results in better performance.
--
-- * e=0x10001 is a popular choice
--
-- * e=3 is popular as well, but proven to not be as secure for some cases.
--
generateWith :: (Integer, Integer) -- ^ chosen distinct primes p and q
             -> Int                -- ^ size in bytes
             -> Integer            -- ^ RSA public exponent 'e'
             -> Maybe (PublicKey, PrivateKey)
generateWith (p,q) size e =
    case inverse e phi of
        Nothing -> Nothing
        Just d  -> Just (pub,priv d)
  where n   = p*q
        phi = (p-1)*(q-1)
        -- q and p should be *distinct* *prime* numbers, hence always coprime
        qinv = inverseCoprimes q p
        pub = PublicKey { public_size = size
                        , public_n    = n
                        , public_e    = e
                        }
        priv d = PrivateKey { private_pub  = pub
                            , private_d    = d
                            , private_p    = p
                            , private_q    = q
                            , private_dP   = d `mod` (p-1)
                            , private_dQ   = d `mod` (q-1)
                            , private_qinv = qinv
                            }

-- | generate a pair of (private, public) key of size in bytes.
generate :: MonadRandom m
         => Int     -- ^ size in bytes
         -> Integer -- ^ RSA public exponent 'e'
         -> m (PublicKey, PrivateKey)
generate size e = loop
  where
    loop = do -- loop until we find a valid key pair given e
        pq <- generatePQ
        case generateWith pq size e of
            Nothing -> loop
            Just pp -> return pp
    generatePQ = do
        p <- generatePrime (8 * (size `div` 2))
        q <- generateQ p
        return (p,q)
    generateQ p = do
        q <- generatePrime (8 * (size - (size `div` 2)))
        if p == q then generateQ p else return q

-- | Generate a blinder to use with decryption and signing operation
--
-- the unique parameter apart from the random number generator is the
-- public key value N.
generateBlinder :: MonadRandom m
                => Integer -- ^ RSA public N parameter.
                -> m Blinder
generateBlinder n =
    (\r -> Blinder r (inverseCoprimes r n)) <$> generateMax n
