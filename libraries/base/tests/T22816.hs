module Main (main) where

import Data.Functor.Classes
import Data.Functor.Compose
import Text.ParserCombinators.ReadP as P
import Text.ParserCombinators.ReadPrec (ReadPrec, lift, minPrec, readPrec_to_S)

readEither' :: ReadPrec a -> String -> Either String a
readEither' rp s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    []  -> Left "read1: no parse"
    _   -> Left "read1: ambiguous parse"
 where
  read' =
    do x <- rp
       lift P.skipSpaces
       return x

-- | Like 'read', but tailored to 'Read1'.
read1 :: (Read1 f, Read a) => String -> f a
read1 s = either errorWithoutStackTrace id (readEither' readPrec1 s)

exRead, exRead1 :: Compose Maybe Maybe Int
exRead  = read  "Compose Nothing"
exRead1 = read1 "Compose Nothing"

main :: IO ()
main = do
  putStrLn $ showsPrec  0 exRead  ""
  putStrLn $ showsPrec1 0 exRead1 ""
