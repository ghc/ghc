{-# LANGUAGE MagicHash, NoImplicitPrelude, UnboxedTuples, UnliftedFFITypes, Trustworthy #-}

module GHC.Debug ( debugLn, debugErrLn ) where

import GHC.Prim
import GHC.Types
import GHC.Tuple ()

debugLn :: [Char] -> IO ()
debugLn xs = IO (\s0 ->
                 case mkMBA s0 xs of
                 (# s1, mba #) ->
                     case c_debugLn mba of
                     IO f -> f s1)

debugErrLn :: [Char] -> IO ()
debugErrLn xs = IO (\s0 ->
                    case mkMBA s0 xs of
                    (# s1, mba #) ->
                        case c_debugErrLn mba of
                        IO f -> f s1)

foreign import ccall unsafe "debugLn"
    c_debugLn :: MutableByteArray# RealWorld -> IO ()

foreign import ccall unsafe "debugErrLn"
    c_debugErrLn :: MutableByteArray# RealWorld -> IO ()

mkMBA :: State# RealWorld -> [Char] ->
         (# State# RealWorld, MutableByteArray# RealWorld #)
mkMBA s0 xs = -- Start with 1 so that we have space to put in a \0 at
              -- the end
              case len 1# xs of
              l ->
                  case newByteArray# l s0 of
                  (# s1, mba #) ->
                      case write mba 0# xs s1 of
                      s2 -> (# s2, mba #)
    where len l [] = l
          len l (_ : xs') = len (l +# 1#) xs'

          write mba offset [] s = writeCharArray# mba offset '\0'# s
          write mba offset (C# x : xs') s
              = case writeCharArray# mba offset x s of
                s' ->
                    write mba (offset +# 1#) xs' s'

