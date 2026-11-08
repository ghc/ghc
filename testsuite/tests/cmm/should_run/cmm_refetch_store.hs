{-# LANGUAGE MagicHash, GHCForeignImportPrim, UnliftedFFITypes #-}
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts

foreign import prim "fs_call" fs_call# :: Word# -> Word#
foreign import prim "fs_own"  fs_own#  :: Word# -> Word#

run :: (Word# -> Word#) -> Word -> Word
run f (W# a) = W# (f a)
{-# NOINLINE run #-}

main :: IO ()
main = allocaBytes 24 $ \p -> do
  -- three words, the second read and written through the pointer tagged 1
  let tagged = fromIntegral (ptrToWordPtr p) + 1 :: Word
      fill = pokeElemOff (p :: Ptr Word) 0 0 >> pokeElemOff p 1 1
             >> pokeElemOff p 2 0
  fill
  print (run fs_call# tagged)
  fill
  print (run fs_own# tagged)
