{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main (main) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8
import Data.ByteString.Char8()
import Data.ByteString.Unsafe as B
import Data.ByteString.Internal (memcpy)
import Data.Typeable (Typeable)
import Data.Word

import Foreign

import GHC.IO.Buffer
import GHC.IO.BufferedIO
import GHC.IO.Device
import GHC.IO.Handle

import System.IO

-- | Create a seakable read-handle from a bytestring
bsHandle :: ByteString -> FilePath -> IO Handle
bsHandle bs fp
    = newBsDevice bs >>= \dev ->
      mkFileHandle dev fp ReadMode Nothing noNewlineTranslation

data BSIODevice
    = BSIODevice
       ByteString
       (MVar Int) -- Position
 deriving Typeable

newBsDevice :: ByteString -> IO BSIODevice
newBsDevice bs = BSIODevice bs <$> newMVar 0

remaining :: BSIODevice -> IO Int
remaining (BSIODevice bs mPos)
    = do
  let bsLen = B.length bs
  withMVar mPos $ \pos -> return (bsLen - pos)

sizeBS :: BSIODevice -> Int
sizeBS (BSIODevice bs _) = B.length bs

seekBS :: BSIODevice -> SeekMode -> Int -> IO ()
seekBS dev AbsoluteSeek pos
    | pos < 0 = error "Cannot seek to a negative position!"
    | pos > sizeBS dev = error "Cannot seek past end of handle!"
    | otherwise = case dev of
                    BSIODevice _ mPos
                        -> modifyMVar_ mPos $ \_ -> return pos
seekBS dev SeekFromEnd pos = seekBS dev AbsoluteSeek (sizeBS dev - pos)
seekBS dev RelativeSeek pos
    = case dev of
        BSIODevice _bs mPos
            -> modifyMVar_ mPos $ \curPos ->
               let newPos = curPos + pos
               in if newPos < 0 || newPos > sizeBS dev
                  then error "Cannot seek outside of handle!"
                  else return newPos

tellBS :: BSIODevice -> IO Int
tellBS (BSIODevice _ mPos) = readMVar mPos

dupBS :: BSIODevice -> IO BSIODevice
dupBS (BSIODevice bs mPos) = BSIODevice bs <$> (readMVar mPos >>= newMVar)

readBS :: BSIODevice -> Ptr Word8 -> Int -> IO Int
readBS dev@(BSIODevice bs mPos) buff amount
    = do
  rem <- remaining dev
  if amount > rem
   then readBS dev buff rem
   else B.unsafeUseAsCString bs $ \ptr ->
       do
         memcpy buff (castPtr ptr) (fromIntegral amount)
         modifyMVar_ mPos (return . (+amount))
         return amount

instance BufferedIO BSIODevice where
    newBuffer dev buffState = newByteBuffer (sizeBS dev) buffState
    fillReadBuffer dev buff = readBuf dev buff
    fillReadBuffer0 dev buff
        = do
      (amount, buff') <- fillReadBuffer dev buff
      return (if amount == 0 then Nothing else Just amount, buff')

instance RawIO BSIODevice where
    read = readBS
    readNonBlocking dev buff n = Just `liftM` readBS dev buff n

instance IODevice BSIODevice where
    ready _ True _ = return False -- read only
    ready _ False _ = return True -- always ready

    close _ = return ()
    isTerminal _ = return False
    isSeekable _ = return True
    seek dev seekMode pos = seekBS dev seekMode (fromIntegral pos)
    tell dev = fromIntegral <$> tellBS dev
    getSize dev = return $ fromIntegral $ sizeBS dev
    setEcho _ _ = error "Not a terminal device"
    getEcho _ = error "Not a terminal device"
    setRaw _ _ = error "Raw mode not supported"
    devType _ = return RegularFile
    dup = dupBS
    dup2 _ _ = error "Dup2 not supported"


main = bsHandle "test" "<fake file>" >>= Data.ByteString.Char8.hGetContents >>= print
