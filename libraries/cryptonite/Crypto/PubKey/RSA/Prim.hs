-- |
-- Module      : Crypto.PubKey.RSA.Prim
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
module Crypto.PubKey.RSA.Prim
    (
    -- * Decrypt primitive
      dp
    -- * Encrypt primitive
    , ep
    ) where

import           Crypto.PubKey.RSA.Types
import           Crypto.Number.ModArithmetic (expFast, expSafe)
import           Crypto.Number.Serialize (os2ip, i2ospOf_)
import           Crypto.Internal.ByteArray (ByteArray)

{- dpSlow computes the decrypted message not using any precomputed cache value.
   only n and d need to valid. -}
dpSlow :: ByteArray ba => PrivateKey -> ba -> ba
dpSlow pk c = i2ospOf_ (private_size pk) $ expSafe (os2ip c) (private_d pk) (private_n pk)

{- dpFast computes the decrypted message more efficiently if the
   precomputed private values are available. mod p and mod q are faster
   to compute than mod pq -}
dpFast :: ByteArray ba => Blinder -> PrivateKey -> ba -> ba
dpFast (Blinder r rm1) pk c =
    i2ospOf_ (private_size pk) (multiplication rm1 (m2 + h * (private_q pk)) (private_n pk))
    where
        re  = expFast r (public_e $ private_pub pk) (private_n pk)
        iC  = multiplication re (os2ip c) (private_n pk)
        m1  = expSafe iC (private_dP pk) (private_p pk)
        m2  = expSafe iC (private_dQ pk) (private_q pk)
        h   = ((private_qinv pk) * (m1 - m2)) `mod` (private_p pk)

dpFastNoBlinder :: ByteArray ba => PrivateKey -> ba -> ba
dpFastNoBlinder pk c = i2ospOf_ (private_size pk) (m2 + h * (private_q pk))
     where iC = os2ip c
           m1 = expSafe iC (private_dP pk) (private_p pk)
           m2 = expSafe iC (private_dQ pk) (private_q pk)
           h  = ((private_qinv pk) * (m1 - m2)) `mod` (private_p pk)

-- | Compute the RSA decrypt primitive.
-- if the p and q numbers are available, then dpFast is used
-- otherwise, we use dpSlow which only need d and n.
dp :: ByteArray ba => Maybe Blinder -> PrivateKey -> ba -> ba
dp blinder pk
    | private_p pk /= 0 && private_q pk /= 0 = maybe dpFastNoBlinder dpFast blinder $ pk
    | otherwise                              = dpSlow pk

-- | Compute the RSA encrypt primitive
ep :: ByteArray ba => PublicKey -> ba -> ba
ep pk m = i2ospOf_ (public_size pk) $ expFast (os2ip m) (public_e pk) (public_n pk)

-- | multiply 2 integers in Zm only performing the modulo operation if necessary
multiplication :: Integer -> Integer -> Integer -> Integer
multiplication a b m = (a * b) `mod` m
