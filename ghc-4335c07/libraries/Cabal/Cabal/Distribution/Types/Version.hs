{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Distribution.Types.Version (
    -- * Package versions
    Version,
    mkVersion,
    mkVersion',
    versionNumbers,
    nullVersion,
    alterVersion,
    version0,
    -- * Internal
    validVersion,
    ) where

import Data.Bits                   (shiftL, shiftR, (.&.), (.|.))
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.Text

import qualified Data.Version               as Base
import qualified Distribution.Compat.Parsec as P
import qualified Distribution.Compat.ReadP  as Parse
import qualified Text.PrettyPrint           as Disp
import qualified Text.Read                  as Read

-- | A 'Version' represents the version of a software entity.
--
-- Instances of 'Eq' and 'Ord' are provided, which gives exact
-- equality and lexicographic ordering of the version number
-- components (i.e. 2.1 > 2.0, 1.2.3 > 1.2.2, etc.).
--
-- This type is opaque and distinct from the 'Base.Version' type in
-- "Data.Version" since @Cabal-2.0@. The difference extends to the
-- 'Binary' instance using a different (and more compact) encoding.
--
-- @since 2.0.0.2
data Version = PV0 {-# UNPACK #-} !Word64
             | PV1 !Int [Int]
             -- NOTE: If a version fits into the packed Word64
             -- representation (i.e. at most four version components
             -- which all fall into the [0..0xfffe] range), then PV0
             -- MUST be used. This is essential for the 'Eq' instance
             -- to work.
             deriving (Data,Eq,Generic,Typeable)

instance Ord Version where
    compare (PV0 x)    (PV0 y)    = compare x y
    compare (PV1 x xs) (PV1 y ys) = case compare x y of
        EQ -> compare xs ys
        c  -> c
    compare (PV0 w)    (PV1 y ys) = case compare x y of
        EQ -> compare [x2,x3,x4] ys
        c  -> c
      where
        x  = fromIntegral ((w `shiftR` 48) .&. 0xffff) - 1
        x2 = fromIntegral ((w `shiftR` 32) .&. 0xffff) - 1
        x3 = fromIntegral ((w `shiftR` 16) .&. 0xffff) - 1
        x4 = fromIntegral               (w .&. 0xffff) - 1
    compare (PV1 x xs) (PV0 w)    = case compare x y of
        EQ -> compare xs [y2,y3,y4]
        c  -> c
      where
        y  = fromIntegral ((w `shiftR` 48) .&. 0xffff) - 1
        y2 = fromIntegral ((w `shiftR` 32) .&. 0xffff) - 1
        y3 = fromIntegral ((w `shiftR` 16) .&. 0xffff) - 1
        y4 = fromIntegral               (w .&. 0xffff) - 1

instance Show Version where
    showsPrec d v = showParen (d > 10)
        $ showString "mkVersion "
        . showsPrec 11 (versionNumbers v)

instance Read Version where
    readPrec = Read.parens $ do
        Read.Ident "mkVersion" <- Read.lexP
        v <- Read.step Read.readPrec
        return (mkVersion v)

instance Binary Version

instance NFData Version where
    rnf (PV0 _) = ()
    rnf (PV1 _ ns) = rnf ns

instance Pretty Version where
  pretty ver
    = Disp.hcat (Disp.punctuate (Disp.char '.')
                                (map Disp.int $ versionNumbers ver))

instance Parsec Version where
    parsec = mkVersion <$> P.sepBy1 P.integral (P.char '.') <* tags
      where
        tags = do
            ts <- many $ P.char '-' *> some (P.satisfy isAlphaNum)
            case ts of
                []      -> pure ()
                (_ : _) -> parsecWarning PWTVersionTag "version with tags"

instance Text Version where
  parse = do
      branch <- Parse.sepBy1 parseNat (Parse.char '.')
                -- allow but ignore tags:
      _tags  <- Parse.many (Parse.char '-' >> Parse.munch1 isAlphaNum)
      return (mkVersion branch)
    where
      parseNat = read `fmap` Parse.munch1 isDigit

-- | Construct 'Version' from list of version number components.
--
-- For instance, @mkVersion [3,2,1]@ constructs a 'Version'
-- representing the version @3.2.1@.
--
-- All version components must be non-negative. @mkVersion []@
-- currently represents the special /null/ version; see also 'nullVersion'.
--
-- @since 2.0.0.2
mkVersion :: [Int] -> Version
-- TODO: add validity check; disallow 'mkVersion []' (we have
-- 'nullVersion' for that)
mkVersion []                    = nullVersion
mkVersion (v1:[])
  | inWord16VerRep1 v1          = PV0 (mkWord64VerRep1 v1)
  | otherwise                   = PV1 v1 []
  where
    inWord16VerRep1 x1 = inWord16 (x1 .|. (x1+1))
    mkWord64VerRep1 y1 = mkWord64VerRep (y1+1) 0 0 0

mkVersion (v1:vs@(v2:[]))
  | inWord16VerRep2 v1 v2       = PV0 (mkWord64VerRep2 v1 v2)
  | otherwise                   = PV1 v1 vs
  where
    inWord16VerRep2 x1 x2 = inWord16 (x1 .|. (x1+1)
                                  .|. x2 .|. (x2+1))
    mkWord64VerRep2 y1 y2 = mkWord64VerRep (y1+1) (y2+1) 0 0

mkVersion (v1:vs@(v2:v3:[]))
  | inWord16VerRep3 v1 v2 v3    = PV0 (mkWord64VerRep3 v1 v2 v3)
  | otherwise                   = PV1 v1 vs
  where
    inWord16VerRep3 x1 x2 x3 = inWord16 (x1 .|. (x1+1)
                                     .|. x2 .|. (x2+1)
                                     .|. x3 .|. (x3+1))
    mkWord64VerRep3 y1 y2 y3 = mkWord64VerRep (y1+1) (y2+1) (y3+1) 0

mkVersion (v1:vs@(v2:v3:v4:[]))
  | inWord16VerRep4 v1 v2 v3 v4 = PV0 (mkWord64VerRep4 v1 v2 v3 v4)
  | otherwise                   = PV1 v1 vs
  where
    inWord16VerRep4 x1 x2 x3 x4 = inWord16 (x1 .|. (x1+1)
                                        .|. x2 .|. (x2+1)
                                        .|. x3 .|. (x3+1)
                                        .|. x4 .|. (x4+1))
    mkWord64VerRep4 y1 y2 y3 y4 = mkWord64VerRep (y1+1) (y2+1) (y3+1) (y4+1)

mkVersion (v1:vs)               = PV1 v1 vs

-- | Version 0. A lower bound of 'Version'.
--
-- @since 2.2
version0 :: Version
version0 = mkVersion [0]

{-# INLINE mkWord64VerRep #-}
mkWord64VerRep :: Int -> Int -> Int -> Int -> Word64
mkWord64VerRep v1 v2 v3 v4 =
      (fromIntegral v1 `shiftL` 48)
  .|. (fromIntegral v2 `shiftL` 32)
  .|. (fromIntegral v3 `shiftL` 16)
  .|.  fromIntegral v4

{-# INLINE inWord16 #-}
inWord16 :: Int -> Bool
inWord16 x = (fromIntegral x :: Word) <= 0xffff

-- | Variant of 'Version' which converts a "Data.Version" 'Version'
-- into Cabal's 'Version' type.
--
-- @since 2.0.0.2
mkVersion' :: Base.Version -> Version
mkVersion' = mkVersion . Base.versionBranch

-- | Unpack 'Version' into list of version number components.
--
-- This is the inverse to 'mkVersion', so the following holds:
--
-- > (versionNumbers . mkVersion) vs == vs
--
-- @since 2.0.0.2
versionNumbers :: Version -> [Int]
versionNumbers (PV1 n ns) = n:ns
versionNumbers (PV0 w)
  | v1 < 0    = []
  | v2 < 0    = [v1]
  | v3 < 0    = [v1,v2]
  | v4 < 0    = [v1,v2,v3]
  | otherwise = [v1,v2,v3,v4]
  where
    v1 = fromIntegral ((w `shiftR` 48) .&. 0xffff) - 1
    v2 = fromIntegral ((w `shiftR` 32) .&. 0xffff) - 1
    v3 = fromIntegral ((w `shiftR` 16) .&. 0xffff) - 1
    v4 = fromIntegral (w .&. 0xffff) - 1


-- | Constant representing the special /null/ 'Version'
--
-- The 'nullVersion' compares (via 'Ord') as less than every proper
-- 'Version' value.
--
-- @since 2.0.0.2
nullVersion :: Version
-- TODO: at some point, 'mkVersion' may disallow creating /null/
-- 'Version's
nullVersion = PV0 0

-- | Apply function to list of version number components
--
-- > alterVersion f == mkVersion . f . versionNumbers
--
-- @since 2.0.0.2
alterVersion :: ([Int] -> [Int]) -> Version -> Version
alterVersion f = mkVersion . f . versionNumbers

-- internal helper
validVersion :: Version -> Bool
validVersion v = v /= nullVersion && all (>=0) (versionNumbers v)


