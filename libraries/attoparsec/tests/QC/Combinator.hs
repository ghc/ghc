{-# LANGUAGE CPP, OverloadedStrings #-}

module QC.Combinator where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>), (<$>), (<*), (*>))
#endif
import Data.Maybe (fromJust, isJust)
import Data.Word (Word8)
import QC.Common (Repack, parseBS, repackBS, toLazyBS)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.Combinator as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

choice :: NonEmptyList (NonEmptyList Word8) -> Gen Property
choice (NonEmpty xs) = do
  let ys = map (B.pack . getNonEmpty) xs
  return . forAll (repackBS <$> arbitrary <*> elements ys) $
    maybe False (`elem` ys) . parseBS (C.choice (map P.string ys))

count :: Positive (Small Int) -> Repack -> B.ByteString -> Bool
count (Positive (Small n)) rs s =
    (length <$> parseBS (C.count n (P.string s)) input) == Just n
  where input = repackBS rs (B.concat (replicate (n+1) s))

lookAhead :: NonEmptyList Word8 -> Bool
lookAhead (NonEmpty xs) =
  let ys = B.pack xs
      withLookAheadThenConsume = (\x y -> (x, y)) <$> C.lookAhead (P.string ys) <*> P.string ys
      mr = parseBS withLookAheadThenConsume $ toLazyBS ys
  in isJust mr && fst (fromJust mr) == snd (fromJust mr)

match :: Int -> NonNegative Int -> NonNegative Int -> Repack -> Bool
match n (NonNegative x) (NonNegative y) rs =
    parseBS (P.match parser) (repackBS rs input) == Just (input, n)
  where parser = P.skipWhile (=='x') *> P.signed P.decimal <*
                 P.skipWhile (=='y')
        input = B.concat [
            B8.replicate x 'x', B8.pack (show n), B8.replicate y 'y'
          ]

tests :: [TestTree]
tests = [
    testProperty "choice" choice
  , testProperty "count" count
  , testProperty "lookAhead" lookAhead
  , testProperty "match" match
  ]
