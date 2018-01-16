
-- This encapsulates the pattern of programs that gradually (and lazily)
-- build a bytestring. If append is too strict in its second argument then
-- we get a stack overflow for large n.

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as LC
import System.Environment

main :: IO ()
main = do xs <- getArgs
          case xs of
              [x] ->
                  case reads x of
                      [(n, "")] ->
                          LC.putStr $ foo n
                      _ -> error "Bad argument"
              _ -> error "Need exactly 1 argument (number of times to loop)"

foo :: Int -> LC.ByteString
foo 0 = LC.empty
foo (n+1) = LC.pack "foo\n" `LC.append` foo n

