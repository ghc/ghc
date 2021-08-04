{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Imports
import           Foundation.Check.Main
import           Utils
import           Data.Char                    (chr)
import           Data.Word
import qualified Data.ByteString         as BS
import           Data.ByteArray               (Bytes, ScrubbedBytes, ByteArray)
import qualified Data.ByteArray          as B
import qualified Data.ByteArray.Encoding as B
import qualified Data.ByteArray.Parse    as Parse

import qualified SipHash

#ifdef WITH_BASEMENT_SUPPORT
import           Basement.Block (Block)
import           Basement.UArray (UArray)
#endif

newtype Positive = Positive Word
  deriving (Show, Eq, Ord)
instance Arbitrary Positive where
    arbitrary = Positive <$> between (0, 255)

data Backend = BackendByte | BackendScrubbedBytes
#ifdef WITH_BASEMENT_SUPPORT
#if MIN_VERSION_basement(0,0,5)
    | BackendBlock
#endif
    | BackendUArray
#endif
    deriving (Show,Eq,Bounded,Enum)

allBackends :: NonEmpty [Backend]
allBackends = nonEmpty_ $ enumFrom BackendByte

data ArbitraryBS = forall a . ByteArray a => ArbitraryBS a

arbitraryBS :: Word -> Gen ArbitraryBS
arbitraryBS n = do
    backend <- elements allBackends
    case backend of
        BackendByte          -> ArbitraryBS `fmap` ((B.pack `fmap` replicateM (fromIntegral n) arbitrary) :: Gen Bytes)
        BackendScrubbedBytes -> ArbitraryBS `fmap` ((B.pack `fmap` replicateM (fromIntegral n) arbitrary) :: Gen ScrubbedBytes)
#ifdef WITH_BASEMENT_SUPPORT
#if MIN_VERSION_basement(0,0,5)
        BackendBlock         -> ArbitraryBS `fmap` ((B.pack `fmap` replicateM (fromIntegral n) arbitrary) :: Gen (Block Word8))
#endif
        BackendUArray        -> ArbitraryBS `fmap` ((B.pack `fmap` replicateM (fromIntegral n) arbitrary) :: Gen (UArray Word8))
#endif

arbitraryBSof :: Word -> Word -> Gen ArbitraryBS
arbitraryBSof minBytes maxBytes = between (minBytes, maxBytes) >>= arbitraryBS

newtype SmallList a = SmallList [a]
    deriving (Show,Eq)

instance Arbitrary a => Arbitrary (SmallList a) where
    arbitrary = between (0,8) >>= \n -> SmallList `fmap` replicateM (fromIntegral n) arbitrary

instance Arbitrary ArbitraryBS where
    arbitrary = arbitraryBSof 0 259

newtype Words8 = Words8 { unWords8 :: [Word8] }
    deriving (Show,Eq)

instance Arbitrary Words8 where
    arbitrary = between (0, 259) >>= \n -> Words8 <$> replicateM (fromIntegral n) arbitrary

testGroupBackends :: String -> (forall ba . (Show ba, Eq ba, Typeable ba, ByteArray ba) => (ba -> ba) -> [Test]) -> Test
testGroupBackends x l =
    Group x
        [ Group "Bytes" (l withBytesWitness)
        , Group "ScrubbedBytes" (l withScrubbedBytesWitness)
#ifdef WITH_BASEMENT_SUPPORT
        , Group "Block" (l withBlockWitness)
        , Group "UArray" (l withUArrayWitness)
#endif
        ]

testShowProperty :: IsProperty a
                 => String
                 -> (forall ba . (Show ba, Eq ba, Typeable ba, ByteArray ba) => (ba -> ba) -> ([Word8] -> String) -> a)
                 -> Test
testShowProperty x p =
    Group x
        [ Property "Bytes" (p withBytesWitness showLikeString)
        , Property "ScrubbedBytes" (p withScrubbedBytesWitness showLikeEmptySB)
        ]
  where
    showLikeString  l = show $ (chr . fromIntegral) <$> l
    showLikeEmptySB _ = show (withScrubbedBytesWitness B.empty)

base64Kats =
    [ ("pleasure.", "cGxlYXN1cmUu")
    , ("leasure.", "bGVhc3VyZS4=")
    , ("easure.", "ZWFzdXJlLg==")
    , ("asure.", "YXN1cmUu")
    , ("sure.", "c3VyZS4=")
    , ("", "")
    ]

base64URLKats =
    [ ("pleasure.", "cGxlYXN1cmUu")
    , ("leasure.", "bGVhc3VyZS4")
    , ("easure.", "ZWFzdXJlLg")
    , ("asure.", "YXN1cmUu")
    , ("sure.", "c3VyZS4")
    , ("\DC4\251\156\ETX\217~", "FPucA9l-") -- From RFC4648
    , ("\DC4\251\156\ETX\217\DEL", "FPucA9l_")
    , ("", "")
    ]

base16Kats =
    [ ("this is a string", "74686973206973206120737472696e67") ]

base32Kats =
    [ ("-pleasure.", "FVYGYZLBON2XEZJO")
    , ("pleasure.",  "OBWGKYLTOVZGKLQ=")
    , ("leasure.",   "NRSWC43VOJSS4===")
    , ("easure.",    "MVQXG5LSMUXA====")
    , ("asure.",     "MFZXK4TFFY======")
    , ("sure.",      "ON2XEZJO")
    , ("ure.",       "OVZGKLQ=")
    , ("re.",        "OJSS4===")
    , ("e.",         "MUXA====")
    , (".",          "FY======")
    , ("",           "")
    ]

encodingTests witnessID =
    [ Group "BASE64"
        [ Group "encode-KAT" encodeKats64
        , Group "decode-KAT" decodeKats64
        ]
    , Group "BASE64URL"
        [ Group "encode-KAT" encodeKats64URLUnpadded
        , Group "decode-KAT" decodeKats64URLUnpadded
        ]
    , Group "BASE32"
        [ Group "encode-KAT" encodeKats32
        , Group "decode-KAT" decodeKats32
        ]
    , Group "BASE16"
        [ Group "encode-KAT" encodeKats16
        , Group "decode-KAT" decodeKats16
        ]
    ]
  where
        encodeKats64 = fmap (toTest B.Base64) $ zip [1..] base64Kats
        decodeKats64 = fmap (toBackTest B.Base64) $ zip [1..] base64Kats
        encodeKats32 = fmap (toTest B.Base32) $ zip [1..] base32Kats
        decodeKats32 = fmap (toBackTest B.Base32) $ zip [1..] base32Kats
        encodeKats16 = fmap (toTest B.Base16) $ zip [1..] base16Kats
        decodeKats16 = fmap (toBackTest B.Base16) $ zip [1..] base16Kats
        encodeKats64URLUnpadded = fmap (toTest B.Base64URLUnpadded) $ zip [1..] base64URLKats
        decodeKats64URLUnpadded = fmap (toBackTest B.Base64URLUnpadded) $ zip [1..] base64URLKats

        toTest :: B.Base -> (Int, (LString, LString)) -> Test
        toTest base (i, (inp, out)) = Property (show i) $
            let inpbs = witnessID $ B.convertToBase base $ witnessID $ B.pack $ unS inp
                outbs = witnessID $ B.pack $ unS out
             in outbs === inpbs
        toBackTest :: B.Base -> (Int, (LString, LString)) -> Test
        toBackTest base (i, (inp, out)) = Property (show i) $
            let inpbs = witnessID $ B.pack $ unS inp
                outbs = B.convertFromBase base $ witnessID $ B.pack $ unS out
             in Right inpbs === outbs

-- check not to touch internal null pointer of the empty ByteString
bsNullEncodingTest =
    Group "BS-null"
      [ Group "BASE64"
        [ Property "encode-KAT" $ toTest B.Base64
        , Property "decode-KAT" $ toBackTest B.Base64
        ]
      , Group "BASE32"
        [ Property "encode-KAT" $ toTest B.Base32
        , Property "decode-KAT" $ toBackTest B.Base32
        ]
      , Group "BASE16"
        [ Property "encode-KAT" $ toTest B.Base16
        , Property "decode-KAT" $ toBackTest B.Base16
        ]
      ]
  where
    toTest base =
      B.convertToBase base BS.empty === BS.empty
    toBackTest base =
      B.convertFromBase base BS.empty === Right BS.empty

parsingTests witnessID =
    [ CheckPlan "parse" $
        let input = witnessID $ B.pack $ unS "xx abctest"
            abc   = witnessID $ B.pack $ unS "abc"
            est   = witnessID $ B.pack $ unS "est"
            result = Parse.parse ((,,) <$> Parse.take 2 <*> Parse.byte 0x20 <*> (Parse.bytes abc *> Parse.anyByte)) input
         in case result of
                Parse.ParseOK remaining (_,_,_) -> validate "remaining" $ est === remaining
                _                               -> validate "unexpected result" False
    ]

main = defaultMain $ Group "memory"
    [ testGroupBackends "basic" basicProperties
    , bsNullEncodingTest
    , testGroupBackends "encoding" encodingTests
    , testGroupBackends "parsing" parsingTests
    , testGroupBackends "hashing" $ \witnessID ->
        [ Group "SipHash" $ SipHash.tests witnessID
        ]
    , testShowProperty "showing" $ \witnessID expectedShow (Words8 l) ->
          (show . witnessID . B.pack $ l) == expectedShow l
#ifdef WITH_BASEMENT_SUPPORT
    , testFoundationTypes
#endif
    ]
  where
    basicProperties witnessID =
        [ Property "unpack . pack == id" $ \(Words8 l) -> l == (B.unpack . witnessID . B.pack $ l)
        , Property "self-eq" $ \(Words8 l) -> let b = witnessID . B.pack $ l in b == b
        , Property "add-empty-eq" $ \(Words8 l) ->
            let b = witnessID $ B.pack l
             in B.append b B.empty == b
        , Property "zero" $ \(Positive n) ->
            let expected = witnessID $ B.pack $ replicate (fromIntegral n) 0
             in expected == B.zero (fromIntegral n)
        , Property "Ord" $ \(Words8 l1) (Words8 l2) ->
            compare l1 l2 == compare (witnessID $ B.pack l1) (B.pack l2)
        , Property "Monoid(mappend)" $ \(Words8 l1) (Words8 l2) ->
            mappend l1 l2 == (B.unpack $ mappend (witnessID $ B.pack l1) (B.pack l2))
        , Property "Monoid(mconcat)" $ \(SmallList l) ->
            mconcat (fmap unWords8 l) == (B.unpack $ mconcat $ fmap (witnessID . B.pack . unWords8) l)
        , Property "append (append a b) c == append a (append b c)" $ \(Words8 la) (Words8 lb) (Words8 lc) ->
            let a = witnessID $ B.pack la
                b = witnessID $ B.pack lb
                c = witnessID $ B.pack lc
             in B.append (B.append a b) c == B.append a (B.append b c)
        , Property "concat l" $ \(SmallList l) ->
            let chunks   = fmap (witnessID . B.pack . unWords8) l
                expected = concatMap unWords8 l
             in B.pack expected == witnessID (B.concat chunks)
        , Property "reverse" $ \(Words8 l) ->
            let b = witnessID (B.pack l)
             in reverse l == B.unpack (B.reverse b)
        , Property "cons b (reverse bs) == reverse (snoc bs b)" $ \(Words8 l) b ->
            let a = witnessID (B.pack l)
             in B.cons b (B.reverse a) == B.reverse (B.snoc a b)
        , Property "all == Prelude.all" $ \(Words8 l) b ->
            let b1 = witnessID (B.pack l)
                p  = (/= b)
             in B.all p b1 == all p l
        , Property "any == Prelude.any" $ \(Words8 l) b ->
            let b1 = witnessID (B.pack l)
                p  = (== b)
             in B.any p b1 == any p l
        , Property "singleton b == pack [b]" $ \b ->
            witnessID (B.singleton b) == B.pack [b]
        , Property "span" $ \x (Words8 l) ->
            let c = witnessID (B.pack l)
                (a, b) = B.span (== x) c
             in c == B.append a b
        , Property "span (const True)" $ \(Words8 l) ->
            let a = witnessID (B.pack l)
             in B.span (const True) a == (a, B.empty)
        , Property "span (const False)" $ \(Words8 l) ->
            let b = witnessID (B.pack l)
             in B.span (const False) b == (B.empty, b)
        ]

#ifdef WITH_BASEMENT_SUPPORT
testFoundationTypes = Group "Basement"
  [ CheckPlan "allocRet 4 _ :: UArray Int8 === 4" $ do
      x <- pick "allocateRet 4 _" $ (B.length :: UArray Int8 -> Int) . snd <$> B.allocRet 4 (const $ return ())
      validate "4 === x" $ x === 4
  , CheckPlan "allocRet 4 _ :: UArray Int16 === 4" $ do
      x <- pick "allocateRet 4 _" $ (B.length :: UArray Int16 -> Int) . snd <$> B.allocRet 4 (const $ return ())
      validate "4 === x" $ x === 4
  , CheckPlan "allocRet 4 _ :: UArray Int32 === 4" $ do
      x <- pick "allocateRet 4 _" $ (B.length :: UArray Int32 -> Int) . snd <$> B.allocRet 4 (const $ return ())
      validate "4 === x" $ x === 4
  , CheckPlan "allocRet 4 _ :: UArray Int64 === 8" $ do
      x <- pick "allocateRet 4 _" $ (B.length :: UArray Int64 -> Int) . snd <$> B.allocRet 4 (const $ return ())
      validate "8 === x" $ x === 8
  ]
#endif
