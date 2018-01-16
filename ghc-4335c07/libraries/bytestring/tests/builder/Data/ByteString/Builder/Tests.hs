{-# LANGUAGE CPP, FlexibleContexts, ScopedTypeVariables, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Testing composition of 'Builders'.

module Data.ByteString.Builder.Tests (tests) where


import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Writer

import           Foreign (Word, Word8, minusPtr)
import           System.IO.Unsafe (unsafePerformIO)

import           Data.Char (ord, chr)
import qualified Data.DList      as D
import           Data.Foldable (asum, foldMap)

import qualified Data.ByteString          as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy     as L
import qualified Data.ByteString.Short    as Sh

import           Data.ByteString.Builder
import           Data.ByteString.Builder.Extra
import           Data.ByteString.Builder.Internal (Put, putBuilder, fromPut)
import qualified Data.ByteString.Builder.Internal   as BI
import qualified Data.ByteString.Builder.Prim       as BP
import           Data.ByteString.Builder.Prim.TestUtils

import           Control.Exception (evaluate)
import           System.IO (openTempFile, hPutStr, hClose, hSetBinaryMode)
import           System.IO (hSetEncoding, utf8)
import           System.Directory
import           Foreign (ForeignPtr, withForeignPtr, castPtr)

#if defined(HAVE_TEST_FRAMEWORK)
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
#else
import           TestFramework
#endif

import           Test.QuickCheck
                   ( Arbitrary(..), oneof, choose, listOf, elements )
import           Test.QuickCheck.Property
                   ( printTestCase, morallyDubiousIOProperty )


tests :: [Test]
tests =
  [ testBuilderRecipe
  , testHandlePutBuilder
  , testHandlePutBuilderChar8
  , testPut
  , testRunBuilder
  ] ++
  testsEncodingToBuilder ++
  testsBinary ++
  testsASCII ++
  testsChar8 ++
  testsUtf8


------------------------------------------------------------------------------
-- Testing 'Builder' execution
------------------------------------------------------------------------------

testBuilderRecipe :: Test
testBuilderRecipe =
    testProperty "toLazyByteStringWith" $ testRecipe <$> arbitrary
  where
    testRecipe r =
        printTestCase msg $ x1 == x2
      where
        x1 = renderRecipe r
        x2 = buildRecipe r
        toString = map (chr . fromIntegral)
        msg = unlines
          [ "recipe: " ++ show r
          , "render: " ++ toString x1
          , "build : " ++ toString x2
          , "diff  : " ++ show (dropWhile (uncurry (==)) $ zip x1 x2)
          ]

testHandlePutBuilder :: Test
testHandlePutBuilder =
    testProperty "hPutBuilder" testRecipe
  where
    testRecipe :: (String, String, String, Recipe) -> Bool
    testRecipe args@(before, between, after, recipe) = unsafePerformIO $ do
        tempDir <- getTemporaryDirectory
        (tempFile, tempH) <- openTempFile tempDir "TestBuilder"
        -- switch to UTF-8 encoding
        hSetEncoding tempH utf8
        -- output recipe with intermediate direct writing to handle
        let b = fst $ recipeComponents recipe
        hPutStr tempH before
        hPutBuilder tempH b
        hPutStr tempH between
        hPutBuilder tempH b
        hPutStr tempH after
        hClose tempH
        -- read file
        lbs <- L.readFile tempFile
        _ <- evaluate (L.length $ lbs)
        removeFile tempFile
        -- compare to pure builder implementation
        let lbsRef = toLazyByteString $ mconcat
              [stringUtf8 before, b, stringUtf8 between, b, stringUtf8 after]
        -- report
        let msg = unlines
              [ "task:     " ++ show args
              , "via file: " ++ show lbs
              , "direct :  " ++ show lbsRef
              -- , "diff  : " ++ show (dropWhile (uncurry (==)) $ zip x1 x2)
              ]
            success = lbs == lbsRef
        unless success (error msg)
        return success

testHandlePutBuilderChar8 :: Test
testHandlePutBuilderChar8 =
    testProperty "char8 hPutBuilder" testRecipe
  where
    testRecipe :: (String, String, String, Recipe) -> Bool
    testRecipe args@(before, between, after, recipe) = unsafePerformIO $ do
        tempDir <- getTemporaryDirectory
        (tempFile, tempH) <- openTempFile tempDir "TestBuilder"
        -- switch to binary / latin1 encoding
        hSetBinaryMode tempH True
        -- output recipe with intermediate direct writing to handle
        let b = fst $ recipeComponents recipe
        hPutStr tempH before
        hPutBuilder tempH b
        hPutStr tempH between
        hPutBuilder tempH b
        hPutStr tempH after
        hClose tempH
        -- read file
        lbs <- L.readFile tempFile
        _ <- evaluate (L.length $ lbs)
        removeFile tempFile
        -- compare to pure builder implementation
        let lbsRef = toLazyByteString $ mconcat
              [string8 before, b, string8 between, b, string8 after]
        -- report
        let msg = unlines
              [ "task:     " ++ show args
              , "via file: " ++ show lbs
              , "direct :  " ++ show lbsRef
              -- , "diff  : " ++ show (dropWhile (uncurry (==)) $ zip x1 x2)
              ]
            success = lbs == lbsRef
        unless success (error msg)
        return success


-- Recipes with which to test the builder functions
---------------------------------------------------

data Mode =
       Threshold Int
     | Insert
     | Copy
     | Smart
     | Hex
     deriving( Eq, Ord, Show )

data Action =
       SBS Mode S.ByteString
     | LBS Mode L.ByteString
     | ShBS Sh.ShortByteString
     | W8  Word8
     | W8S [Word8]
     | String String
     | FDec Float
     | DDec Double
     | Flush
     | EnsureFree Word
     | ModState Int
     deriving( Eq, Ord, Show )

data Strategy = Safe | Untrimmed
     deriving( Eq, Ord, Show )

data Recipe = Recipe Strategy Int Int L.ByteString [Action]
     deriving( Eq, Ord, Show )

renderRecipe :: Recipe -> [Word8]
renderRecipe (Recipe _ firstSize _ cont as) =
    D.toList $ execWriter (evalStateT (mapM_ renderAction as) firstSize)
                 `mappend` renderLBS cont
  where
    renderAction (SBS Hex bs)   = tell $ foldMap hexWord8 $ S.unpack bs
    renderAction (SBS _ bs)     = tell $ D.fromList $ S.unpack bs
    renderAction (LBS Hex lbs)  = tell $ foldMap hexWord8 $ L.unpack lbs
    renderAction (LBS _ lbs)    = tell $ renderLBS lbs
    renderAction (ShBS sbs)     = tell $ D.fromList $ Sh.unpack sbs
    renderAction (W8 w)         = tell $ return w
    renderAction (W8S ws)       = tell $ D.fromList ws
    renderAction (String cs)    = tell $ foldMap (D.fromList . charUtf8_list) cs
    renderAction Flush          = tell $ mempty
    renderAction (EnsureFree _) = tell $ mempty
    renderAction (FDec f)       = tell $ D.fromList $ encodeASCII $ show f
    renderAction (DDec d)       = tell $ D.fromList $ encodeASCII $ show d
    renderAction (ModState i)   = do
        s <- get
        tell (D.fromList $ encodeASCII $ show s)
        put (s - i)


    renderLBS = D.fromList . L.unpack
    hexWord8  = D.fromList . wordHexFixed_list

buildAction :: Action -> StateT Int Put ()
buildAction (SBS Hex bs)            = lift $ putBuilder $ byteStringHex bs
buildAction (SBS Smart bs)          = lift $ putBuilder $ byteString bs
buildAction (SBS Copy bs)           = lift $ putBuilder $ byteStringCopy bs
buildAction (SBS Insert bs)         = lift $ putBuilder $ byteStringInsert bs
buildAction (SBS (Threshold i) bs)  = lift $ putBuilder $ byteStringThreshold i bs
buildAction (LBS Hex lbs)           = lift $ putBuilder $ lazyByteStringHex lbs
buildAction (LBS Smart lbs)         = lift $ putBuilder $ lazyByteString lbs
buildAction (LBS Copy lbs)          = lift $ putBuilder $ lazyByteStringCopy lbs
buildAction (LBS Insert lbs)        = lift $ putBuilder $ lazyByteStringInsert lbs
buildAction (LBS (Threshold i) lbs) = lift $ putBuilder $ lazyByteStringThreshold i lbs
buildAction (ShBS sbs)              = lift $ putBuilder $ shortByteString sbs
buildAction (W8 w)                  = lift $ putBuilder $ word8 w
buildAction (W8S ws)                = lift $ putBuilder $ BP.primMapListFixed BP.word8 ws
buildAction (String cs)             = lift $ putBuilder $ stringUtf8 cs
buildAction (FDec f)                = lift $ putBuilder $ floatDec f
buildAction (DDec d)                = lift $ putBuilder $ doubleDec d
buildAction Flush                   = lift $ putBuilder $ flush
buildAction (EnsureFree minFree)    = lift $ putBuilder $ ensureFree $ fromIntegral minFree
buildAction (ModState i)            = do
    s <- get
    lift $ putBuilder $ intDec s
    put (s - i)

buildRecipe :: Recipe -> [Word8]
buildRecipe recipe =
    L.unpack $ toLBS b
  where
    (b, toLBS) = recipeComponents recipe


recipeComponents :: Recipe -> (Builder, Builder -> L.ByteString)
recipeComponents (Recipe how firstSize otherSize cont as) =
    (b, toLBS)
  where
    toLBS = toLazyByteStringWith (strategy how firstSize otherSize) cont
      where
        strategy Safe      = safeStrategy
        strategy Untrimmed = untrimmedStrategy

    b = fromPut $ evalStateT (mapM_ buildAction as) firstSize


-- 'Arbitary' instances
-----------------------

instance Arbitrary L.ByteString where
    arbitrary = L.fromChunks <$> listOf arbitrary
    shrink lbs
      | L.null lbs = []
      | otherwise = pure $ L.take (L.length lbs `div` 2) lbs

instance Arbitrary S.ByteString where
    arbitrary =
        trim S.drop =<< trim S.take =<< S.pack <$> listOf arbitrary
      where
        trim f bs = oneof [pure bs, f <$> choose (0, S.length bs) <*> pure bs]

    shrink bs
      | S.null bs = []
      | otherwise = pure $ S.take (S.length bs `div` 2) bs

instance Arbitrary Mode where
    arbitrary = oneof
        [Threshold <$> arbitrary, pure Smart, pure Insert, pure Copy, pure Hex]

    shrink (Threshold i) = Threshold <$> shrink i
    shrink _             = []

instance Arbitrary Action where
    arbitrary = oneof
      [ SBS <$> arbitrary <*> arbitrary
      , LBS <$> arbitrary <*> arbitrary
      , ShBS . Sh.toShort <$> arbitrary
      , W8  <$> arbitrary
      , W8S <$> listOf arbitrary
        -- ensure that larger character codes are also tested
      , String <$> listOf ((\c -> chr (ord c * ord c)) <$> arbitrary)
      , pure Flush
        -- never request more than 64kb free space
      , (EnsureFree . (`mod` 0xffff)) <$> arbitrary
      , FDec <$> arbitrary
      , DDec <$> arbitrary
      , ModState <$> arbitrary
      ]
      where

    shrink (SBS m bs) =
      (SBS <$> shrink m <*> pure bs) <|>
      (SBS <$> pure m   <*> shrink bs)
    shrink (LBS m lbs) =
      (LBS <$> shrink m <*> pure lbs) <|>
      (LBS <$> pure m   <*> shrink lbs)
    shrink (ShBS sbs) =
      ShBS . Sh.toShort <$> shrink (Sh.fromShort sbs)
    shrink (W8 w)         = W8 <$> shrink w
    shrink (W8S ws)       = W8S <$> shrink ws
    shrink (String cs)    = String <$> shrink cs
    shrink Flush          = []
    shrink (EnsureFree i) = EnsureFree <$> shrink i
    shrink (FDec f)       = FDec <$> shrink f
    shrink (DDec d)       = DDec <$> shrink d
    shrink (ModState i)   = ModState <$> shrink i

instance Arbitrary Strategy where
    arbitrary = elements [Safe, Untrimmed]
    shrink _  = []

instance Arbitrary Recipe where
    arbitrary =
        Recipe <$> arbitrary
               <*> ((`mod` 33333) <$> arbitrary)  -- bound max chunk-sizes
               <*> ((`mod` 33337) <$> arbitrary)
               <*> arbitrary
               <*> listOf arbitrary

    -- shrinking the actions first is desirable
    shrink (Recipe a b c d e) = asum
      [ (\x -> Recipe a b c d x) <$> shrink e
      , (\x -> Recipe a b c x e) <$> shrink d
      , (\x -> Recipe a b x d e) <$> shrink c
      , (\x -> Recipe a x c d e) <$> shrink b
      , (\x -> Recipe x b c d e) <$> shrink a
      ]


------------------------------------------------------------------------------
-- Creating Builders from basic encodings
------------------------------------------------------------------------------

testsEncodingToBuilder :: [Test]
testsEncodingToBuilder =
  [ test_encodeUnfoldrF
  , test_encodeUnfoldrB
  ]


-- Unfoldr fused with encoding
------------------------------

test_encodeUnfoldrF :: Test
test_encodeUnfoldrF =
    compareImpls "encodeUnfoldrF word8" id encode
  where
    toLBS = toLazyByteStringWith (safeStrategy 23 101) L.empty
    encode =
        L.unpack . toLBS . BP.primUnfoldrFixed BP.word8 go
      where
        go []     = Nothing
        go (w:ws) = Just (w, ws)


test_encodeUnfoldrB :: Test
test_encodeUnfoldrB =
    compareImpls "encodeUnfoldrB charUtf8" (concatMap charUtf8_list) encode
  where
    toLBS = toLazyByteStringWith (safeStrategy 23 101) L.empty
    encode =
        L.unpack . toLBS . BP.primUnfoldrBounded BP.charUtf8 go
      where
        go []     = Nothing
        go (c:cs) = Just (c, cs)


------------------------------------------------------------------------------
-- Testing the Put monad
------------------------------------------------------------------------------

testPut :: Test
testPut = testGroup "Put monad"
  [ testLaw "identity" (\v -> (pure id <*> putInt v) `eqPut` (putInt v))

  , testLaw "composition" $ \(u, v, w) ->
        (pure (.) <*> minusInt u <*> minusInt v <*> putInt w) `eqPut`
        (minusInt u <*> (minusInt v <*> putInt w))

  , testLaw "homomorphism" $ \(f, x) ->
        (pure (f -) <*> pure x) `eqPut` (pure (f - x))

  , testLaw "interchange" $ \(u, y) ->
        (minusInt u <*> pure y) `eqPut` (pure ($ y) <*> minusInt u)

  , testLaw "ignore left value" $ \(u, v) ->
        (putInt u *> putInt v) `eqPut` (pure (const id) <*> putInt u <*> putInt v)

  , testLaw "ignore right value" $ \(u, v) ->
        (putInt u <* putInt v) `eqPut` (pure const <*> putInt u <*> putInt v)

  , testLaw "functor" $ \(f, x) ->
        (fmap (f -) (putInt x)) `eqPut` (pure (f -) <*> putInt x)

  ]
  where
    putInt i    = putBuilder (integerDec i) >> return i
    minusInt i  = (-) <$> putInt i
    run p       = toLazyByteString $ fromPut (do i <- p; _ <- putInt i; return ())
    eqPut p1 p2 = (run p1, run p2)

    testLaw name f = compareImpls name (fst . f) (snd . f)


------------------------------------------------------------------------------
-- Testing the Driver <-> Builder protocol
------------------------------------------------------------------------------

-- | Ensure that there are at least 'n' free bytes for the following 'Builder'.
{-# INLINE ensureFree #-}
ensureFree :: Int -> Builder
ensureFree minFree =
    BI.builder step
  where
    step k br@(BI.BufferRange op ope)
      | ope `minusPtr` op < minFree = return $ BI.bufferFull minFree op next
      | otherwise                   = k br
      where
        next br'@(BI.BufferRange op' ope')
          |  freeSpace < minFree =
              error $ "ensureFree: requested " ++ show minFree ++ " bytes, " ++
                      "but got only " ++ show freeSpace ++ " bytes"
          | otherwise = k br'
          where
            freeSpace = ope' `minusPtr` op'


------------------------------------------------------------------------------
-- Testing the Builder runner
------------------------------------------------------------------------------

testRunBuilder :: Test
testRunBuilder =
    testProperty "runBuilder" prop
  where
    prop actions =
        morallyDubiousIOProperty $ do
          let (builder, _) = recipeComponents recipe
              expected     = renderRecipe recipe
          actual <- bufferWriterOutput (runBuilder builder)
          return (S.unpack actual == expected)
      where
        recipe = Recipe Safe 0 0 mempty actions

bufferWriterOutput :: BufferWriter -> IO S.ByteString
bufferWriterOutput bwrite0 = do
    let len0 = 8
    buf <- S.mallocByteString len0
    bss <- go [] buf len0 bwrite0
    return (S.concat (reverse bss))
  where
    go :: [S.ByteString] -> ForeignPtr Word8 -> Int -> BufferWriter -> IO [S.ByteString]
    go bss !buf !len bwrite = do
      (wc, next) <- withForeignPtr buf $ \ptr -> bwrite ptr len
      bs <- getBuffer buf wc
      case next of
        Done                        -> return (bs:bss)
        More  m bwrite' | m <= len  -> go (bs:bss)   buf len bwrite'
                        | otherwise -> do let len' = m
                                          buf' <- S.mallocByteString len'
                                          go (bs:bss) buf' len' bwrite'
        Chunk c bwrite'             -> go (c:bs:bss) buf len bwrite'

    getBuffer :: ForeignPtr Word8 -> Int -> IO S.ByteString
    getBuffer buf len = withForeignPtr buf $ \ptr ->
                          S.packCStringLen (castPtr ptr, len)


------------------------------------------------------------------------------
-- Testing the pre-defined builders
------------------------------------------------------------------------------

testBuilderConstr :: (Arbitrary a, Show a)
                  => TestName -> (a -> [Word8]) -> (a -> Builder) -> Test
testBuilderConstr name ref mkBuilder =
    testProperty name check
  where
    check x =
        (ws ++ ws) ==
        (L.unpack $ toLazyByteString $ mkBuilder x `mappend` mkBuilder x)
      where
        ws = ref x


testsBinary :: [Test]
testsBinary =
  [ testBuilderConstr "word8"     bigEndian_list    word8
  , testBuilderConstr "int8"      bigEndian_list    int8

  --  big-endian
  , testBuilderConstr "int16BE"   bigEndian_list    int16BE
  , testBuilderConstr "int32BE"   bigEndian_list    int32BE
  , testBuilderConstr "int64BE"   bigEndian_list    int64BE

  , testBuilderConstr "word16BE"  bigEndian_list    word16BE
  , testBuilderConstr "word32BE"  bigEndian_list    word32BE
  , testBuilderConstr "word64BE"  bigEndian_list    word64BE

  , testBuilderConstr "floatLE"     (float_list  littleEndian_list) floatLE
  , testBuilderConstr "doubleLE"    (double_list littleEndian_list) doubleLE

  --  little-endian
  , testBuilderConstr "int16LE"   littleEndian_list int16LE
  , testBuilderConstr "int32LE"   littleEndian_list int32LE
  , testBuilderConstr "int64LE"   littleEndian_list int64LE

  , testBuilderConstr "word16LE"  littleEndian_list word16LE
  , testBuilderConstr "word32LE"  littleEndian_list word32LE
  , testBuilderConstr "word64LE"  littleEndian_list word64LE

  , testBuilderConstr "floatBE"     (float_list  bigEndian_list)   floatBE
  , testBuilderConstr "doubleBE"    (double_list bigEndian_list)   doubleBE

  --  host dependent
  , testBuilderConstr "int16Host"   hostEndian_list  int16Host
  , testBuilderConstr "int32Host"   hostEndian_list  int32Host
  , testBuilderConstr "int64Host"   hostEndian_list  int64Host
  , testBuilderConstr "intHost"     hostEndian_list  intHost

  , testBuilderConstr "word16Host"  hostEndian_list  word16Host
  , testBuilderConstr "word32Host"  hostEndian_list  word32Host
  , testBuilderConstr "word64Host"  hostEndian_list  word64Host
  , testBuilderConstr "wordHost"    hostEndian_list  wordHost

  , testBuilderConstr "floatHost"   (float_list  hostEndian_list)   floatHost
  , testBuilderConstr "doubleHost"  (double_list hostEndian_list)   doubleHost
  ]

testsASCII :: [Test]
testsASCII =
  [ testBuilderConstr "char7" char7_list char7
  , testBuilderConstr "string7" (concatMap char7_list) string7

  , testBuilderConstr "int8Dec"   dec_list int8Dec
  , testBuilderConstr "int16Dec"  dec_list int16Dec
  , testBuilderConstr "int32Dec"  dec_list int32Dec
  , testBuilderConstr "int64Dec"  dec_list int64Dec
  , testBuilderConstr "intDec"    dec_list intDec

  , testBuilderConstr "word8Dec"  dec_list word8Dec
  , testBuilderConstr "word16Dec" dec_list word16Dec
  , testBuilderConstr "word32Dec" dec_list word32Dec
  , testBuilderConstr "word64Dec" dec_list word64Dec
  , testBuilderConstr "wordDec"   dec_list wordDec

  , testBuilderConstr "integerDec" (dec_list . enlarge) (integerDec . enlarge)
  , testBuilderConstr "floatDec"   dec_list floatDec
  , testBuilderConstr "doubleDec"  dec_list doubleDec

  , testBuilderConstr "word8Hex"  hex_list word8Hex
  , testBuilderConstr "word16Hex" hex_list word16Hex
  , testBuilderConstr "word32Hex" hex_list word32Hex
  , testBuilderConstr "word64Hex" hex_list word64Hex
  , testBuilderConstr "wordHex"   hex_list wordHex

  , testBuilderConstr "word8HexFixed"  wordHexFixed_list word8HexFixed
  , testBuilderConstr "word16HexFixed" wordHexFixed_list word16HexFixed
  , testBuilderConstr "word32HexFixed" wordHexFixed_list word32HexFixed
  , testBuilderConstr "word64HexFixed" wordHexFixed_list word64HexFixed

  , testBuilderConstr "int8HexFixed"  int8HexFixed_list  int8HexFixed
  , testBuilderConstr "int16HexFixed" int16HexFixed_list int16HexFixed
  , testBuilderConstr "int32HexFixed" int32HexFixed_list int32HexFixed
  , testBuilderConstr "int64HexFixed" int64HexFixed_list int64HexFixed

  , testBuilderConstr "floatHexFixed"  floatHexFixed_list  floatHexFixed
  , testBuilderConstr "doubleHexFixed" doubleHexFixed_list doubleHexFixed
  ]
  where
    enlarge (n, e) = n ^ (abs (e `mod` (50 :: Integer)))

testsChar8 :: [Test]
testsChar8 =
  [ testBuilderConstr "charChar8" char8_list char8
  , testBuilderConstr "stringChar8" (concatMap char8_list) string8
  ]

testsUtf8 :: [Test]
testsUtf8 =
  [ testBuilderConstr "charUtf8" charUtf8_list charUtf8
  , testBuilderConstr "stringUtf8" (concatMap charUtf8_list) stringUtf8
  ]
