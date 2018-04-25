{-# OPTIONS_GHC -cpp #-}
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.Internal  as P
import qualified Data.ByteString.Unsafe    as P
import System.IO
import Control.Monad
import Data.HashTable as Hash
import Data.Maybe
import Data.Int (Int32)
import Data.Char (ord)

#define STRING_SETS             0
#define PACKEDSTRING_SETS       0
#define BYTESTRING_HASHTABLES   1

-- -----------------------------------------------------------------------------
-- Sets

#if STRING_SETS
main = do
  ps <- readFile "Usr.Dict.Words"
  loop (Set.fromList (lines ps)) `catch` \_ -> return ()

loop set = do
  ps <- hGetLine stdin
  when (not (ps `Set.member` set)) $ hPutStrLn stdout ps
  loop set
#endif

-- -----------------------------------------------------------------------------
-- PackedString with Sets

#if PACKEDSTRING_SETS
main = do
  ps <- P.readFile "Usr.Dict.Words"
  loop (Set.fromList (P.lines ps)) `catch` \_ -> return ()

loop set = do
  ps <- P.getLine
  when (not (ps `Set.member` set)) $ P.putStrLn ps
  loop set
#endif

#if BYTESTRING_HASHTABLES
main = do
    ps <- P.readFile "Usr.Dict.Words"
    ht <- Hash.new (==) hash
    zipWithM (Hash.insert ht) (P.lines ps) (repeat True)
    loop ht `catch` \_ -> return ()

loop ht = do
    ps <- P.getLine
    b  <- Hash.lookup ht ps
    when (isNothing b) $ P.putStrLn ps
    loop ht

hash :: P.ByteString -> Int32
hash ps = go 0 0
  where
    len = P.length ps

    go :: Int -> Int -> Int32
    go n acc | acc `seq` False = undefined
             | n == len  = fromIntegral acc
             | otherwise = go (n+1)
                              ((fromIntegral)(ps `P.unsafeIndex` n) +
                                  (acc * 128 `rem` fromIntegral prime))
#endif
