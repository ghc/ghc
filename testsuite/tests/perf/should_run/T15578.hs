{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main where

import qualified Data.Set  as Set
import qualified Data.Text as Text

import Data.Set              (Set)
import Data.Text             (Text)
import System.IO             (BufferMode (NoBuffering), hSetBuffering, stdout)
import GHC.Generics          (Generic)
import Control.DeepSeq       (force, NFData)
import Control.Exception     (evaluate)


--------------------------------
-- === Running benchmarks === --
--------------------------------

iters :: Int
iters = 100000000

src1 :: Text
src1 = Text.replicate iters "tttt"

data Grammar a
    = Tokens !(Set a) !(a -> Bool)
    | Many   !(Grammar a)
    | X      !(Grammar a)

instance Ord a => Semigroup (Grammar a) where
    Tokens s f <> Tokens s' g = Tokens (s <> s') $ \c -> f c || g c
    {-# INLINE (<>) #-}

token :: Eq a => a -> Grammar a
token = \a -> Tokens (Set.singleton a) (a ==)
{-# INLINE token #-}

many :: Grammar a -> Grammar a
many = Many
{-# INLINE many #-}

data Result
    = Success Text Text
    | Fail
    deriving (Show, Generic)

instance NFData Result

runTokenParser :: Grammar Char -> Text -> Result
runTokenParser = \grammar stream -> case grammar of
    Tokens _ tst -> let
        head = Text.head stream
        in if tst head
            then Success (Text.tail stream) (Text.singleton head)
            else Fail
    Many (Tokens _ tst) -> let
        (!consumed, !rest) = Text.span tst stream
        in Success rest consumed
    X !grammar -> runTokenParser grammar stream

testGrammar1 :: Grammar Char
testGrammar1 = let
    s1 = token 't'
    in many s1
{-# INLINE testGrammar1 #-}

test3 :: Text -> Result
test3 src =
  runTokenParser testGrammar1 src
{-# NOINLINE test3 #-}

main :: IO ()
main = do
    srcx <- evaluate $ force src1
    evaluate $ force $ test3 srcx
    pure ()
