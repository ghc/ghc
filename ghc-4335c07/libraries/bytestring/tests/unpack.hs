{-# OPTIONS -O #-}

import qualified Data.ByteString as P
import Data.ByteString.Base
import System.IO.Unsafe
import Data.Word
import Foreign
import GHC.Base (build)

--
-- head (my_unpack ..) should fail in 6.5 due to some rules issue, I think
--
main = do
    print x
    print $      (my_unpack x)
    print $ last (my_unpack x)
    print $ head (my_unpack x)  -- should be '1', but bug causes an exception
    where
      x = P.pack [1..10]

--
-- a list-fuseable unpack
--

my_unpack ps = build (unpackFoldr ps)
{-# INLINE my_unpack #-}

unpackFoldr :: ByteString -> (Word8 -> a -> a) -> a -> a
unpackFoldr (PS fp off len) f ch =
    unsafePerformIO $ withForeignPtr fp $ \p -> do
        let loop a b c | a `seq` b `seq` False = undefined -- needs the strictness
            loop _ (-1) acc = return acc
            loop q n    acc = do
               a <- peekByteOff q n
               loop q (n-1) (a `f` acc)
        loop (p `plusPtr` off) (len-1) ch
