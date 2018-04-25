import System.IO
import Foreign
import Foreign.C
import Control.Monad

main = do test True; test False

test blocking = do
  h <- openBinaryFile "hGetBuf003.hs" ReadMode

  let sz = 42
      loop = do
         -- mix ordinary char buffering with hGetBuf
         eof <- hIsEOF h
         when (not eof) $ hGetChar h >>= putChar
         b <- allocaBytes sz $ \ptr -> do
                r <- (if blocking then hGetBuf else hGetBufNonBlocking) h ptr sz
                if (r == 0)
                   then return True
                   else do s <- peekCStringLen (ptr,r)
                           putStr s
                           return False
         if b then return () else loop -- tail call

  loop

