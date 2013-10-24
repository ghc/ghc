{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
import Control.Monad
import GHC.Conc
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import GHC.IO
import GHC.Exts

foreign import ccall unsafe "&puts" puts :: FunPtr (Ptr CChar -> IO ())

main :: IO ()
main = do
  ptr@(Ptr p) <- malloc
  poke (ptr :: Ptr CChar) 0
  setNumCapabilities 2
  let !(FunPtr puts#) = puts
  replicateM_ 10000 $ IO $ \s -> let
    !(# s1, w #) = mkWeakNoFinalizer# () () s
    !(# s2, _ #) = addCFinalizerToWeak# puts# p 0# nullAddr# w s1
    !(# s3, _ #) = addCFinalizerToWeak# puts# p 0# nullAddr# w s2
    !(# s4, _ #) = addCFinalizerToWeak# puts# p 0# nullAddr# w s3
    !(# s5, _ #) = addCFinalizerToWeak# puts# p 0# nullAddr# w s4
    !(# s6, _ #) = addCFinalizerToWeak# puts# p 0# nullAddr# w s5
    !(# s7, _ #) = addCFinalizerToWeak# puts# p 0# nullAddr# w s6
    !(# s8, _ #) = addCFinalizerToWeak# puts# p 0# nullAddr# w s7
    !(# s9, _ #) = addCFinalizerToWeak# puts# p 0# nullAddr# w s8
    !(# s10, _ #) = addCFinalizerToWeak# puts# p 0# nullAddr# w s9
    !(# s11, _ #) = addCFinalizerToWeak# puts# p 0# nullAddr# w s10
    !(# s12, _ #) = addCFinalizerToWeak# puts# p 0# nullAddr# w s11
    !(# s13, _ #) = addCFinalizerToWeak# puts# p 0# nullAddr# w s12
    !(# s14, _ #) = addCFinalizerToWeak# puts# p 0# nullAddr# w s13
    !(# s15, _ #) = addCFinalizerToWeak# puts# p 0# nullAddr# w s14
    !(# s16, _ #) = addCFinalizerToWeak# puts# p 0# nullAddr# w s15
    !(# s17, _ #) = addCFinalizerToWeak# puts# p 0# nullAddr# w s16
    !(# s18, _ #) = addCFinalizerToWeak# puts# p 0# nullAddr# w s17
    !(# s19, _ #) = addCFinalizerToWeak# puts# p 0# nullAddr# w s18
    !(# s20, _ #) = addCFinalizerToWeak# puts# p 0# nullAddr# w s19
    in (# s20, () #)
