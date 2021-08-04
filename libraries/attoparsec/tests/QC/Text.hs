{-# LANGUAGE BangPatterns, CPP, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module QC.Text (tests) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>), (<$>))
#endif
import Data.Int (Int64)
import Data.Word (Word8)
import Prelude hiding (take, takeWhile)
import QC.Common (liftOp, parseT)
import qualified QC.Text.FastSet as FastSet
import qualified QC.Text.Regressions as Regressions
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import qualified Data.Attoparsec.Text as P
import qualified Data.Attoparsec.Text.Lazy as PL
import qualified Data.Attoparsec.Text.FastSet as S
import qualified Data.ByteString as BS
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as L

-- Basic byte-level combinators.

satisfy :: Char -> L.Text -> Property
satisfy w s = parseT (P.satisfy (<=w)) (L.cons w s) === Just w

satisfyWith :: Char -> L.Text -> Property
satisfyWith c s = parseT (P.satisfyWith id (<=c)) (L.cons c s) === Just c

char :: Char -> L.Text -> Property
char w s = parseT (P.char w) (L.cons w s) === Just w

skip :: Char -> L.Text -> Property
skip w s =
  case (parseT (P.skip (<w)) s, L.uncons s) of
    (Nothing, mcs) -> maybe (property True) (expectFailure . it) mcs
    (Just _,  mcs) -> maybe (property False) it mcs
  where it cs = liftOp "<" (<) (fst cs) w

anyChar :: L.Text -> Property
anyChar s
    | L.null s  = p === Nothing
    | otherwise = p === Just (L.head s)
  where p = parseT P.anyChar s

notChar :: Char -> NonEmptyList Char -> Property
notChar w (NonEmpty s) = parseT (P.notChar w) bs === if v == w
                                                      then Nothing
                                                      else Just v
    where v = L.head bs
          bs = L.pack s

peekChar :: L.Text -> Property
peekChar s
    | L.null s  = p === Just (Nothing, s)
    | otherwise = p === Just (Just (L.head s), s)
  where p = parseT ((,) <$> P.peekChar <*> P.takeLazyText) s

peekChar' :: L.Text -> Property
peekChar' s = parseT P.peekChar' s === (fst <$> L.uncons s)

string :: L.Text -> L.Text -> Property
string s t = parseT (P.string s') (s `L.append` t) === Just s'
  where s' = toStrict s

strings :: L.Text -> L.Text -> L.Text -> Property
strings s t u =
    parseT (P.string (toStrict s) >> P.string t') (L.concat [s,t,u])
    === Just t'
  where t' = toStrict t

-- | Note: "simple, and efficient" works for well formed input...
-- i.e. e.g. Latin1 texts
stringCI :: [Word8] -> Property
stringCI ws = P.parseOnly (P.stringCI fs) s === Right s
  where fs = T.toCaseFold s
        s  = TE.decodeLatin1 (BS.pack ws)

asciiCI :: T.Text -> Gen Bool
asciiCI x =
  (\s i -> P.parseOnly (P.asciiCI s) i == Right i)
    <$> maybeModifyCase x
    <*> maybeModifyCase x
  where
    maybeModifyCase s = elements [s, toLower s, toUpper s]
    toLower = T.map (\c -> if c < Char.chr 127 then Char.toLower c else c)
    toUpper = T.map (\c -> if c < Char.chr 127 then Char.toUpper c else c)

toStrict :: L.Text -> T.Text
toStrict = T.concat . L.toChunks

skipWhile :: Char -> L.Text -> Property
skipWhile w s =
    let t = L.dropWhile (<=w) s
    in case PL.parse (P.skipWhile (<=w)) s of
         PL.Done t' () -> t === t'
         _             -> property False

take :: Int -> L.Text -> Property
take n s = maybe (liftOp "<" (<) (L.length s) (fromIntegral n))
           (=== T.take n (toStrict s)) $
           parseT (P.take n) s

takeText :: L.Text -> Property
takeText s = maybe (property False) (=== toStrict s) . parseT P.takeText $ s

takeLazyText :: L.Text -> Property
takeLazyText s = maybe (property False) (=== s) . parseT P.takeLazyText $ s

takeCount :: Positive Int -> L.Text -> Property
takeCount (Positive k) s =
    case parseT (P.take k) s of
      Nothing -> liftOp ">" (>) (fromIntegral k) (L.length s)
      Just _s -> liftOp "<=" (<=) (fromIntegral k) (L.length s)

takeWhile :: Char -> L.Text -> Property
takeWhile w s =
    let (h,t) = L.span (==w) s
    in case PL.parse (P.takeWhile (==w)) s of
         PL.Done t' h' -> t === t' .&&. toStrict h === h'
         _             -> property False

takeWhile1 :: Char -> L.Text -> Property
takeWhile1 w s =
  let s'    = L.cons w s
      (h,t) = L.span (<=w) s'
    in case PL.parse (P.takeWhile1 (<=w)) s' of
         PL.Done t' h' -> t === t' .&&. toStrict h === h'
         _             -> property False

takeTill :: Char -> L.Text -> Property
takeTill w s =
    let (h,t) = L.break (==w) s
    in case PL.parse (P.takeTill (==w)) s of
         PL.Done t' h' -> t === t' .&&. toStrict h === h'
         _             -> property False

takeWhile1_empty :: Property
takeWhile1_empty = parseT (P.takeWhile1 undefined) L.empty === Nothing

endOfInput :: L.Text -> Property
endOfInput s = parseT P.endOfInput s === if L.null s
                                         then Just ()
                                         else Nothing

endOfLine :: L.Text -> Property
endOfLine s =
  case (parseT P.endOfLine s, L.uncons s) of
    (Nothing, mcs) -> maybe (property True) (expectFailure . eol) mcs
    (Just _,  mcs) -> maybe (property False) eol mcs
  where eol (c,s') = c === '\n' .||.
                     (c, fst <$> L.uncons s') === ('\r', Just '\n')

scan :: L.Text -> Positive Int64 -> Property
-- for some reason, if counterexample is removed, this test fails?
scan s (Positive k) = counterexample (show s)
                    $ parseT p s === Just (toStrict $ L.take k s)
  where p = P.scan k $ \ n _ ->
            if n > 0 then let !n' = n - 1 in Just n' else Nothing

members :: String -> Property
members s = property $ all (`S.member` set) s
    where set = S.fromList s

nonmembers :: String -> String -> Property
nonmembers s s' = property . not . any (`S.member` set) $ filter (not . (`elem` s)) s'
    where set = S.fromList s

tests :: [TestTree]
tests = [
      testProperty "anyChar" anyChar
    , testProperty "asciiCI" asciiCI
    , testProperty "char" char
    , testProperty "endOfInput" endOfInput
    , testProperty "endOfLine" endOfLine
    , testProperty "notChar" notChar
    , testProperty "peekChar" peekChar
    , testProperty "peekChar'" peekChar'
    , testProperty "satisfy" satisfy
    , testProperty "satisfyWith" satisfyWith
    , testProperty "scan" scan
    , testProperty "skip" skip
    , testProperty "skipWhile" skipWhile
    , testProperty "string" string
    , testProperty "strings" strings
    , testProperty "stringCI" stringCI
    , testProperty "take" take
    , testProperty "takeText" takeText
    , testProperty "takeCount" takeCount
    , testProperty "takeLazyText" takeLazyText
    , testProperty "takeTill" takeTill
    , testProperty "takeWhile" takeWhile
    , testProperty "takeWhile1" takeWhile1
    , testProperty "takeWhile1_empty" takeWhile1_empty
    , testProperty "members" members
    , testProperty "nonmembers" nonmembers
    , testGroup "FastSet" FastSet.tests
    , testGroup "Regressions" Regressions.tests
  ]
