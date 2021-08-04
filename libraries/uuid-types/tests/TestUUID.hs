{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Data.Binary                (encode)
import qualified Data.ByteString.Char8      as BC8
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Char                  (ord)
import           Data.Functor               ((<$>))
import qualified Data.UUID.Types            as U
import           Data.Word
import           Foreign                    (alloca, castPtr, peek, poke,
                                             sizeOf)
import           GHC.ByteOrder              (ByteOrder(..), targetByteOrder)
import           System.IO.Unsafe           (unsafePerformIO)

import           Test.QuickCheck            (Arbitrary (arbitrary), choose)

import           Test.Tasty                 (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit           (assertBool, testCase, (@=?), (@?=))
import           Test.Tasty.QuickCheck      (testProperty)


-- orphan
instance Arbitrary U.UUID where
    -- the UUID random instance ignores bounds
    arbitrary = choose (U.nil, U.nil)

type Test = TestTree

test_null :: Test
test_null =
  testCase "nil is null" $
  assertBool "" (U.null U.nil)

test_nil :: Test
test_nil = testGroup "nil" [
    testCase "nil string" $ U.toString U.nil @?= "00000000-0000-0000-0000-000000000000",
    testCase "nil bytes"  $ U.toByteString U.nil @?= BL.pack (replicate 16 0)
    ]

test_lift :: Test
test_lift = testCase "TH.Lift" $ do
    let uuid = U.fromWords64 123456789 987654321
    uuid @?= $( [| uuid |] )

test_conv :: Test
test_conv = testGroup "conversions" [
    testCase "conv bytes to string" $
        maybe "" (U.toString) (U.fromByteString b16) @?= s16,
    testCase "conv string to bytes" $
        maybe BL.empty (U.toByteString) (U.fromString s16) @?= b16
    ]
    where b16 = BL.pack [1..16]
          s16 = "01020304-0506-0708-090a-0b0c0d0e0f10"

-- | Test fromByteString with a fixed-input.
test_fromByteString :: Test
test_fromByteString =
    testCase "UUID fromByteString" $
        Just inputUUID @=?
             U.fromByteString (BL8.pack "\xa5\xca\x85\x66\xd9\xc5\x48\x35\x99\xc8\xe1\xf1\x3e\x73\xb5\xe2")

-- | Test fromWords with a fixed-input
test_fromWords :: Test
test_fromWords =
    testCase "UUID fromWords" $
        inputUUID @=? U.fromWords 0xa5ca8566 0xd9c54835 0x99c8e1f1 0x3e73b5e2


test_Storeable :: Test
test_Storeable =
    testCase "UUID Storeable(poke)" $
        case targetByteOrder of
          LittleEndian -> BC8.pack "\x66\x85\xca\xa5\xc5\xd9\x35\x48\x99\xc8\xe1\xf1\x3e\x73\xb5\xe2" @=? pokeAsBS inputUUID
          BigEndian    -> BC8.pack "\xa5\xca\x85\x66\xd9\xc5\x48\x35\x99\xc8\xe1\xf1\x3e\x73\xb5\xe2" @=? pokeAsBS inputUUID
  where
    pokeAsBS :: U.UUID -> BC8.ByteString
    pokeAsBS uuid = unsafePerformIO $ do
      alloca $ \ptr -> do
        poke ptr uuid
        BC8.packCStringLen (castPtr ptr, sizeOf uuid)

test_Binary :: Test
test_Binary =
    testCase "UUID Binary(encode)" $
        (BL8.pack "\xa5\xca\x85\x66\xd9\xc5\x48\x35\x99\xc8\xe1\xf1\x3e\x73\xb5\xe2") @=? encode inputUUID

inputUUID :: U.UUID
inputUUID = read "a5ca8566-d9c5-4835-99c8-e1f13e73b5e2"

prop_stringRoundTrip :: Test
prop_stringRoundTrip = testProperty "String round trip" stringRoundTrip
    where stringRoundTrip :: U.UUID -> Bool
          stringRoundTrip u = maybe False (== u) $ U.fromString (U.toString u)

prop_byteStringRoundTrip :: Test
prop_byteStringRoundTrip = testProperty "ByteString round trip" byteStringRoundTrip
    where byteStringRoundTrip :: U.UUID -> Bool
          byteStringRoundTrip u = maybe False (== u)
                                    $ U.fromByteString (U.toByteString u)

prop_stringLength :: Test
prop_stringLength = testProperty "String length" stringLength
    where stringLength :: U.UUID -> Bool
          stringLength u = length (U.toString u) == 36

prop_byteStringLength :: Test
prop_byteStringLength = testProperty "ByteString length" byteStringLength
    where byteStringLength :: U.UUID -> Bool
          byteStringLength u = BL.length (U.toByteString u) == 16

prop_randomsDiffer :: Test
prop_randomsDiffer = testProperty "Randoms differ" randomsDiffer
    where randomsDiffer :: (U.UUID, U.UUID) -> Bool
          randomsDiffer (u1, u2) = u1 /= u2

prop_randomNotNull :: Test
prop_randomNotNull = testProperty "Random not null" randomNotNull
    where randomNotNull :: U.UUID -> Bool
          randomNotNull = not. U.null

prop_readShowRoundTrip :: Test
prop_readShowRoundTrip = testProperty "Read/Show round-trip" prop
    where -- we're using 'Maybe UUID' to add a bit of
          -- real-world complexity.
          prop :: U.UUID -> Bool
          prop uuid = read (show (Just uuid)) == Just uuid

-- Mostly going to test for wrong UUIDs
fromASCIIBytes_fromString1 :: String -> Bool
fromASCIIBytes_fromString1 s =
    if all (\c -> ord c < 256) s
    then U.fromString s == U.fromASCIIBytes (BC8.pack s)
    else True

fromASCIIBytes_fromString2 :: U.UUID -> Bool
fromASCIIBytes_fromString2 (U.toString -> s) =
    U.fromString s == U.fromASCIIBytes (BC8.pack s)

toASCIIBytes_toString :: U.UUID -> Bool
toASCIIBytes_toString uuid =
    U.toString uuid == BC8.unpack (U.toASCIIBytes uuid)

fromASCIIBytes_toASCIIBytes :: U.UUID -> Bool
fromASCIIBytes_toASCIIBytes (BC8.pack . U.toString -> bs) =
    Just bs == (U.toASCIIBytes <$> U.fromASCIIBytes bs)

toASCIIBytes_fromASCIIBytes :: U.UUID -> Bool
toASCIIBytes_fromASCIIBytes uuid =
    Just uuid == U.fromASCIIBytes (U.toASCIIBytes uuid)

toWords_fromWords :: U.UUID -> Bool
toWords_fromWords uuid =
    uuid == myUncurry4 U.fromWords (U.toWords uuid)

fromWords_toWords :: (Word32, Word32, Word32, Word32) -> Bool
fromWords_toWords wds =
    wds == U.toWords (myUncurry4 U.fromWords wds)

myUncurry4 :: (x1 -> x2 -> x3 -> x4 -> y) -> (x1, x2, x3, x4) -> y
myUncurry4 f (a,b,c,d) = f a b c d

prop_storableRoundTrip :: Test
prop_storableRoundTrip =
    testProperty "Storeable round-trip" $ unsafePerformIO . prop
  where
    prop :: U.UUID -> IO Bool
    prop uuid =
        alloca $ \ptr -> do
          poke ptr uuid
          uuid2 <- peek ptr
          return $ uuid == uuid2

main :: IO ()
main = do
    defaultMain $
     testGroup "tests" $
     concat $
     [ [
        test_null,
        test_nil,
        test_lift,
        test_conv,
        test_fromByteString,
        test_fromWords,
        test_Storeable,
        test_Binary
        ]
     , [ prop_stringRoundTrip,
         prop_readShowRoundTrip,
         prop_byteStringRoundTrip,
         prop_storableRoundTrip,
         prop_stringLength,
         prop_byteStringLength,
         prop_randomsDiffer,
         prop_randomNotNull
         ]
     , [ testProperty "fromASCIIBytes_fromString1"  fromASCIIBytes_fromString1
       , testProperty "fromASCIIBytes_fromString2"  fromASCIIBytes_fromString2
       , testProperty "fromASCIIBytes_toString"     toASCIIBytes_toString
       , testProperty "fromASCIIBytes_toASCIIBytes" fromASCIIBytes_toASCIIBytes
       , testProperty "toASCIIBytes_fromASCIIBytes" toASCIIBytes_fromASCIIBytes
       , testProperty "toWords_fromWords" toWords_fromWords
       , testProperty "fromWords_toWords" fromWords_toWords
       ]
     ]
