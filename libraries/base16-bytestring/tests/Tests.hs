{-# LANGUAGE OverloadedStrings #-}
module Main
( main
) where


import Control.Monad (liftM)

import qualified Data.ByteString as BS
import Data.ByteString.Internal (c2w, w2c)
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base16.Lazy as LB16
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Char8 ()
import Data.Char (toUpper)
import Data.String

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)
import Test.QuickCheck (Arbitrary(..))



main :: IO ()
main = defaultMain tests

tests =
  [ testGroup "property tests"
    [ properties b16
    , properties lb16
    ]
  , testGroup "unit tests"
    [ units b16
    , units lb16
    , lenientUnits b16
    , lenientUnits lb16
    ]
  ]

properties
  :: ( IsString bs
     , Show bs
     , Eq bs
     , Arbitrary bs
     )
  => Impl bs
  -> Test
properties (Impl label e d l _ u) = testGroup label
  [ testProperty "decode-encode-lower" $ \a -> Right a == d (e a)
  , testProperty "decode-encode-upper" $ \a -> Right a == d (u . e $ a)
  , testProperty "lenient-encode-lower" $ \a -> a == l (e a)
  , testProperty "lenient-encode-upper" $ \a -> a == l (u . e $ a)
  , testProperty "decode-encode-encode" $ \a -> Right (e a) == d (e (e a))
  , testProperty "lenient-encode-encode" $ \a -> e a == l (e (e a))
  ]

units
  :: ( IsString bs
     , Show bs
     , Eq bs
     )
  => Impl bs
  -> Test
units (Impl label e d l td u) = testGroup label $ encs ++ decs ++ lens
  where
    encs =
      [ testCase ("encode: " ++ show raw) $ do enc @?= rawEnc
      | (raw, rawEnc) <- td
      , let enc = e raw
      ]

    decs =
      [ testCase ("decode: " ++ show rawEnc) $ do dec_enc @?= Right raw; dec_upp @?= Right raw
      | (raw, rawEnc) <- td
      , let dec_enc = d rawEnc
      , let dec_upp = d (u rawEnc)
      ]

    lens =
      [ testCase ("lenient: " ++ show rawEnc) $ do len_enc @?= raw; len_upp @?= raw
      | (raw, rawEnc) <- td
      , let len_enc = l rawEnc
      , let len_upp = l (u rawEnc)
      ]

lenientUnits :: (IsString bs, Show bs, Eq bs) => Impl bs -> Test
lenientUnits (Impl label e d l _ _) = testGroup (label ++ " lenient unit tests")
  [ testCaseB16 "" ""
  , testCaseB16 "f" "6+++++++____++++++======*%$@#%#^*$^6"
  , testCaseB16 "fo" "6$6+6|f"
  , testCaseB16 "foo" "==========6$$66()*f6f"
  , testCaseB16 "foob" "66^%$&^6f6f62"
  , testCaseB16 "fooba" "666f()*#@6f#)(@*)6()*)2()61"
  , testCaseB16 "foobar" "6@6@6@f@6@f@6@2@6@1@7@2++++++++++++++++++++++++"
  ]
  where
    testCaseB16 s t = testCase (show $ if s == "" then "empty" else s) $ do
      let t0 = d (e s)
          t1 = l t

      (d (e s)) @=? Right (l t)

-- ------------------------------------------------------------------ --
-- Test data

rfcVectors :: IsString bs => [(bs,bs)]
rfcVectors =
  [ ("","")
  , ("fo", "666f")
  , ("foo", "666f6f")
  , ("foob", "666f6f62")
  , ("fooba", "666f6f6261")
  , ("foobar", "666f6f626172")
  ]

data Impl bs = Impl
  { _label :: String
  , _encode :: bs -> bs
  , _decode :: bs -> Either String bs
  , _lenient :: bs -> bs
  , _data :: [(bs, bs)]
  , _upper :: bs -> bs
  }

b16 :: Impl BS.ByteString
b16 = Impl "base16-strict" B16.encode B16.decode B16.decodeLenient rfcVectors (BS.map (c2w . toUpper . w2c))


lb16 :: Impl LBS.ByteString
lb16 = Impl "base16-lazy" LB16.encode LB16.decode LB16.decodeLenient rfcVectors (LBS.map (c2w . toUpper . w2c))

instance Arbitrary BS.ByteString where
  arbitrary = liftM BS.pack arbitrary

instance Arbitrary LBS.ByteString where
  arbitrary = liftM LBS.pack arbitrary
