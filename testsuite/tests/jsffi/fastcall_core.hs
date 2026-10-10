module FastcallCore (
  js_exported,
  fast_exported,
  fast_private,
  fast_polymorphic,
  fast_instantiated,
  Wrapped(..),
  fast_newtype,
  fast_io,
  fast_nullary
) where

import GHC.Wasm.Prim
import Foreign.StablePtr

foreign import javascript unsafe "$1 + 1"
  js_exported :: Int -> Int

foreign import javascript unsafe "$1 + 2"
  js_private :: Int -> Int

foreign import javascript unsafe "$1 + 3"
  js_unused :: Int -> Int

foreign import javascript unsafe "console.log($1)"
  js_polymorphic :: StablePtr a -> IO ()

newtype Wrapped = Wrapped Int

foreign import javascript unsafe "$1 + 4"
  js_newtype :: Wrapped -> Wrapped

foreign import javascript unsafe "console.log($1)"
  js_io :: Int -> IO ()

foreign import javascript unsafe "42"
  js_nullary :: IO Int

fast_exported :: Int -> Int
fast_exported = fastcall js_exported

fast_private :: Int -> Int
fast_private = fastcall js_private

fast_polymorphic :: StablePtr a -> IO ()
fast_polymorphic = fastcall js_polymorphic

fast_instantiated :: StablePtr Int -> IO ()
fast_instantiated = fastcall js_polymorphic

fast_newtype :: Wrapped -> Wrapped
fast_newtype = fastcall js_newtype

fast_io :: Int -> IO ()
fast_io = fastcall js_io

fast_nullary :: IO Int
fast_nullary = fastcall js_nullary
