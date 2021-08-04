{-# LANGUAGE ScopedTypeVariables #-}

-- | One-time password implementation as defined by the
-- <http://tools.ietf.org/html/rfc4226 HOTP> and <http://tools.ietf.org/html/rfc6238 TOTP>
-- specifications.
--
-- Both implementations use a shared key between the client and the server. HOTP passwords
-- are based on a synchronized counter. TOTP passwords use the same approach but calculate
-- the counter as a number of time steps from the Unix epoch to the current time, thus
-- requiring that both client and server have synchronized clocks.
--
-- Probably the best-known use of TOTP is in Google's 2-factor authentication.
--
-- The TOTP API doesn't depend on any particular time package, so the user needs to supply
-- the current @OTPTime@ value, based on the system time. For example, using the @hourglass@
-- package, you could create a @getOTPTime@ function:
--
-- >>> import Time.System
-- >>> import Time.Types
-- >>>
-- >>> let getOTPTime = timeCurrent >>= \(Elapsed t) -> return (fromIntegral t :: OTPTime)
--
-- Or if you prefer, the @time@ package could be used:
--
-- >>> import Data.Time.Clock.POSIX
-- >>>
-- >>> let getOTPTime = getPOSIXTime >>= \t -> return (floor t :: OTPTime)
--

module Crypto.OTP
    ( OTP
    , OTPDigits (..)
    , OTPTime
    , hotp
    , resynchronize
    , totp
    , totpVerify
    , TOTPParams
    , ClockSkew (..)
    , defaultTOTPParams
    , mkTOTPParams
    )
where

import           Data.Bits (shiftL, (.&.), (.|.))
import           Data.ByteArray.Mapping (fromW64BE)
import           Data.List (elemIndex)
import           Data.Word
import           Control.Monad (unless)
import           Crypto.Hash (HashAlgorithm, SHA1(..))
import           Crypto.MAC.HMAC
import           Crypto.Internal.ByteArray (ByteArrayAccess, Bytes)
import qualified Crypto.Internal.ByteArray as B


-- | A one-time password which is a sequence of 4 to 9 digits.
type OTP = Word32

-- | The strength of the calculated HOTP value, namely
-- the number of digits (between 4 and 9) in the extracted value.
data OTPDigits = OTP4 | OTP5 | OTP6 | OTP7 | OTP8 | OTP9 deriving (Show)

-- | An integral time value in seconds.
type OTPTime = Word64

hotp :: forall hash key. (HashAlgorithm hash, ByteArrayAccess key)
    => hash
    -> OTPDigits
    -- ^ Number of digits in the HOTP value extracted from the calculated HMAC
    -> key
    -- ^ Shared secret between the client and server
    -> Word64
    -- ^ Counter value synchronized between the client and server
    -> OTP
    -- ^ The HOTP value
hotp _ d k c = dt `mod` digitsPower d
  where
    mac = hmac k (fromW64BE c :: Bytes) :: HMAC hash
    offset = fromIntegral (B.index mac (B.length mac - 1) .&. 0xf)
    dt = (fromIntegral (B.index mac offset       .&. 0x7f) `shiftL` 24) .|.
         (fromIntegral (B.index mac (offset + 1) .&. 0xff) `shiftL` 16) .|.
         (fromIntegral (B.index mac (offset + 2) .&. 0xff) `shiftL`  8) .|.
         fromIntegral  (B.index mac (offset + 3) .&. 0xff)

-- | Attempt to resynchronize the server's counter value
-- with the client, given a sequence of HOTP values.
resynchronize :: (HashAlgorithm hash, ByteArrayAccess key)
    => hash
    -> OTPDigits
    -> Word16
    -- ^ The look-ahead window parameter. Up to this many values will
    -- be calculated and checked against the value(s) submitted by the client
    -> key
    -- ^ The shared secret
    -> Word64
    -- ^ The current server counter value
    -> (OTP, [OTP])
    -- ^ The first OTP submitted by the client and a list of additional
    -- sequential OTPs (which may be empty)
    -> Maybe Word64
    -- ^ The new counter value, synchronized with the client's current counter
    -- or Nothing if the submitted OTP values didn't match anywhere within the window
resynchronize h d s k c (p1, extras) = do
    offBy <- fmap fromIntegral (elemIndex p1 range)
    checkExtraOtps (c + offBy + 1) extras
  where
    checkExtraOtps ctr [] = Just ctr
    checkExtraOtps ctr (p:ps)
        | hotp h d k ctr /= p = Nothing
        | otherwise           = checkExtraOtps (ctr + 1) ps

    range = map (hotp h d k)[c..c + fromIntegral s]

digitsPower :: OTPDigits -> Word32
digitsPower OTP4 = 10000
digitsPower OTP5 = 100000
digitsPower OTP6 = 1000000
digitsPower OTP7 = 10000000
digitsPower OTP8 = 100000000
digitsPower OTP9 = 1000000000


data TOTPParams h = TP !h !OTPTime !Word16 !OTPDigits !ClockSkew deriving (Show)

data ClockSkew = NoSkew | OneStep | TwoSteps | ThreeSteps | FourSteps deriving (Enum, Show)

-- | The default TOTP configuration.
defaultTOTPParams :: TOTPParams SHA1
defaultTOTPParams = TP SHA1 0 30 OTP6 TwoSteps

-- | Create a TOTP configuration with customized parameters.
mkTOTPParams :: (HashAlgorithm hash)
    => hash
    -> OTPTime
    -- ^ The T0 parameter in seconds. This is the Unix time from which to start
    -- counting steps (default 0). Must be before the current time.
    -> Word16
    -- ^ The time step parameter X in seconds (default 30, maximum allowed 300)
    -> OTPDigits
    -- ^ Number of required digits in the OTP (default 6)
    -> ClockSkew
    -- ^ The number of time steps to check either side of the current value
    -- to allow for clock skew between client and server and or delay in
    -- submitting the value. The default is two time steps.
    -> Either String (TOTPParams hash)
mkTOTPParams h t0 x d skew = do
    unless (x > 0) (Left "Time step must be greater than zero")
    unless (x <= 300) (Left "Time step cannot be greater than 300 seconds")
    return (TP h t0 x d skew)

-- | Calculate a totp value for the given time.
totp :: (HashAlgorithm hash, ByteArrayAccess key)
    => TOTPParams hash
    -> key
    -- ^ The shared secret
    -> OTPTime
    -- ^ The time for which the OTP should be calculated.
    -- This is usually the current time as returned by @Data.Time.Clock.POSIX.getPOSIXTime@
    -> OTP
totp (TP h t0 x d _) k now = hotp h d k (timeToCounter now t0 x)

-- | Check a supplied TOTP value is valid for the given time,
-- within the window defined by the skew parameter.
totpVerify :: (HashAlgorithm hash, ByteArrayAccess key)
    => TOTPParams hash
    -> key
    -> OTPTime
    -> OTP
    -> Bool
totpVerify (TP h t0 x d skew) k now otp = otp `elem` map (hotp h d k) (range window [])
  where
    t = timeToCounter now t0 x
    window = fromIntegral (fromEnum skew)
    range 0 acc = t : acc
    range n acc = range (n-1) ((t-n) : (t+n) : acc)

timeToCounter :: Word64 -> Word64 -> Word16 -> Word64
timeToCounter now t0 x = (now - t0) `div` fromIntegral x
