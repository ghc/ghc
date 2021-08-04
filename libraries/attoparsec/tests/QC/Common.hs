{-# LANGUAGE CPP, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module QC.Common
    (
      ASCII(..)
    , parseBS
    , parseT
    , toLazyBS
    , toStrictBS
    , Repack
    , repackBS
    , repackBS_
    , repackT
    , repackT_
    , liftOp
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>), (<$>))
#endif
import Data.Char (isAlpha)
import Test.QuickCheck
import Test.QuickCheck.Unicode (shrinkChar, string)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Attoparsec.ByteString.Lazy as BL
import qualified Data.Attoparsec.Text.Lazy as TL

#if !MIN_VERSION_base(4,4,0)
-- This should really be a dependency on the random package :-(
instance Random Word8 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Arbitrary Word8 where
    arbitrary = choose (minBound, maxBound)
#endif

parseBS :: BL.Parser r -> BL.ByteString -> Maybe r
parseBS p = BL.maybeResult . BL.parse p

parseT :: TL.Parser r -> TL.Text -> Maybe r
parseT p = TL.maybeResult . TL.parse p

toStrictBS :: BL.ByteString -> B.ByteString
toStrictBS = B.concat . BL.toChunks

toLazyBS :: B.ByteString -> BL.ByteString
toLazyBS = BL.fromChunks . (:[])

instance Arbitrary B.ByteString where
    arbitrary = B.pack <$> arbitrary
    shrink = map B.pack . shrink . B.unpack

instance Arbitrary BL.ByteString where
    arbitrary = repackBS <$> arbitrary <*> arbitrary
    shrink = map BL.pack . shrink . BL.unpack

newtype ASCII a = ASCII { fromASCII :: a }
                  deriving (Eq, Ord, Show)

instance Arbitrary (ASCII B.ByteString) where
    arbitrary = (ASCII . B.pack) <$> listOf (choose (0,127))
    shrink = map (ASCII . B.pack) . shrink . B.unpack . fromASCII

instance Arbitrary (ASCII BL.ByteString) where
    arbitrary = ASCII <$> (repackBS <$> arbitrary <*> (fromASCII <$> arbitrary))
    shrink = map (ASCII . BL.pack) . shrink . BL.unpack . fromASCII

type Repack = NonEmptyList (Positive (Small Int))

repackBS :: Repack -> B.ByteString -> BL.ByteString
repackBS (NonEmpty bs) =
    BL.fromChunks . repackBS_ (map (getSmall . getPositive) bs)

repackBS_ :: [Int] -> B.ByteString -> [B.ByteString]
repackBS_ = go . cycle
  where go (b:bs) s
          | B.null s = []
          | otherwise = let (h,t) = B.splitAt b s
                        in h : go bs t
        go _ _ = error "unpossible"

instance Arbitrary T.Text where
    arbitrary = T.pack <$> string
    shrink    = map T.pack . shrinkList shrinkChar . T.unpack

instance Arbitrary TL.Text where
    arbitrary = TL.pack <$> string
    shrink    = map TL.pack . shrinkList shrinkChar . TL.unpack

repackT :: Repack -> T.Text -> TL.Text
repackT (NonEmpty bs) =
    TL.fromChunks . repackT_ (map (getSmall . getPositive) bs)

repackT_ :: [Int] -> T.Text -> [T.Text]
repackT_ = go . cycle
  where go (b:bs) s
          | T.null s = []
          | otherwise = let (h,t) = T.splitAt b s
                        in h : go bs t
        go _ _ = error "unpossible"

liftOp :: (Show a, Testable prop) =>
          String -> (a -> a -> prop) -> a -> a -> Property
liftOp name f x y = counterexample desc (f x y)
  where op = case name of
               (c:_) | isAlpha c -> " `" ++ name ++ "` "
                     | otherwise -> " " ++ name ++ " "
               _ -> " ??? "
        desc = "not (" ++ show x ++ op ++ show y ++ ")"
