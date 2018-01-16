{-# LANGUAGE NoMonomorphismRestriction #-}
import System.Posix
import Control.Monad
import Foreign
import Control.Concurrent
import Data.Char
import System.Exit

size  = 10000
block = 512

main = do
  (rd,wr) <- createPipe
  let bytes = take size (map (fromIntegral.ord) (cycle ['a'..'z']))
  allocaBytes size $ \p -> do
    pokeArray p bytes
    forkIO $ do r <- fdWriteBuf wr p (fromIntegral size)
                when (fromIntegral r /= size) $ error "fdWriteBuf failed"
  allocaBytes block $ \p -> do
    let loop text = do
           r <- fdReadBuf rd p block
           let (chunk,rest) = splitAt (fromIntegral r) text
           chars <- peekArray (fromIntegral r) p
           when (chars /= chunk) $ error $ "mismatch: expected="++show chunk++", found="++show chars
           when (null rest) $ exitWith ExitSuccess
           loop rest
    loop bytes
