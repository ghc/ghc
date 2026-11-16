-- Avoid some warnings in case the LLVM backend isn't being used
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}
{-# LANGUAGE CPP #-}

-- Issue #67: Invalid compilation with LLVM backend.
--
-- Reported in the wild, and cribbed from https://github.com/fpco/serial-bench
module Tests.Regress.Issue67
  ( testTree -- :: TestTree
  ) where

import           Data.Int
import           Data.Monoid                ((<>))
import           Data.Word
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative        ((<$>), (<*>))
#endif

import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as V
import           Codec.Serialise
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.HUnit

--------------------------------------------------------------------------------
-- Tests and properties

data SomeData = SomeData !Int64 !Word8 !Double
  deriving (Eq, Show)

instance Serialise SomeData where
  decode = SomeData <$> decode <*> decode <*> decode
  {-# INLINE decode #-}

  encode (SomeData a b c) = encode a <> encode b <> encode c
  {-# INLINE encode #-}

newtype ArbSomeData = ArbSomeData { toSomeData :: SomeData }
  deriving (Show, Eq)

instance Arbitrary ArbSomeData where
  arbitrary = fmap ArbSomeData $ SomeData
           <$> arbitrary
           <*> arbitrary
           <*> arbitrary

--------------------------------------------------------------------------------
-- TestTree API

to :: V.Vector SomeData -> L.ByteString
to = serialise

from :: L.ByteString -> Maybe (V.Vector SomeData)
from = Just . deserialise

repro1 :: Bool
repro1 =
  let v = V.fromList [SomeData 53169 70 55.3817683321392]
  in from (to v) == Just v

prop_vectorRoundtrip :: [ArbSomeData] -> Bool
prop_vectorRoundtrip list =
  let v = V.fromList (map toSomeData list)
  in from (to v) == Just v

testTree :: TestTree
testTree =
#if defined(__GLASGOW_HASKELL_LLVM__)
  testGroup "Issue 67 - LLVM bogons"
    [ testCase "simple reproduction case"   (True @=? repro1)
    , testProperty "vector roundtrip works" prop_vectorRoundtrip
    ]
#else
  testGroup "Issue 67 - LLVM bogons (NO LLVM - SKIPPING)"
    [ testCase "simple reproduction case (SKIPPED)" (True @=? True)
    , testCase "vector roundtrip works (SKIPPED)"   (True @=? True)
    ]
#endif
