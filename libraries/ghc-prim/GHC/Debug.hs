
module GHC.Debug (debugLn) where

import GHC.Prim
import GHC.Types
import GHC.Unit ()

debugLn :: [Char] -> IO ()
debugLn xs = IO (\s0 ->
                 -- Start with 1 so that we have space to put in a \0 at
                 -- the end
                 case len 1# xs of
                 l ->
                     case newByteArray# l s0 of
                     (# s1, mba #) ->
                         case write mba 0# xs s1 of
                         s2 ->
                             case c_debugLn mba of
                             IO f -> f s2)
    where len l [] = l
          len l (_ : xs') = len (l +# 1#) xs'

          write mba offset [] s = writeCharArray# mba offset '\0'# s
          write mba offset (C# x : xs') s
              = case writeCharArray# mba offset x s of
                s' ->
                    write mba (offset +# 1#) xs' s'

foreign import ccall unsafe "debugLn"
    c_debugLn :: MutableByteArray# RealWorld -> IO ()

