{-# LANGUAGE MagicHash, GHCForeignImportPrim, UnliftedFFITypes #-}
import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts

foreign import prim "fr_block" fr_block# :: Word# -> Word#
foreign import prim "fr_merge" fr_merge# :: Word# -> Word#
foreign import prim "fr_ctl"   fr_ctl#   :: Word# -> Word#

run :: (Word# -> Word#) -> Word -> Word
run f (W# a) = W# (f a)
{-# NOINLINE run #-}

main :: IO ()
main = allocaBytes 72 $ \p -> do
  -- three cells of three words: a header, a word and the next cell, tagged 1
  let cell i = p `plusPtr` (24 * i) :: Ptr Word
      tagged i = fromIntegral (ptrToWordPtr (cell i)) + 1 :: Word
  forM_ [0, 1, 2] $ \i -> do
    pokeElemOff (cell i) 0 0
    pokeElemOff (cell i) 1 (fromIntegral i + 1)
    pokeElemOff (cell i) 2 (if i < 2 then tagged (i + 1) else 0)
  print (run fr_block# (tagged 0))
  print (run fr_merge# (tagged 0))
  print (run fr_ctl#   (tagged 0))
