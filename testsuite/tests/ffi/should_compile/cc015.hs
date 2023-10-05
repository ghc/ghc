
{-# LANGUAGE TypeFamilies #-}

module Cc015 where

import Foreign
import Foreign.C.Types

type S a = a
type IOS a = IO a
type SFunPtr a = FunPtr a

type family F a
type instance F Int = Int
type instance F Bool = G2
type instance F Char = FunPtr (Int -> IO Int)

newtype G1  = G1  Int
newtype G1F = G1F (F (S Int))
newtype G2  = G2  Char
newtype G3  = G3  (IO Int)
newtype G4  = G4  G3
newtype NIO a = NIO (IO a)

-- Type synonyms should be transparent to the typechecker
foreign import ccall f1       ::          S Int -> IOS Int
foreign export ccall "g1"  f1 ::          S Int -> IOS Int
foreign import ccall "&h1" h1 :: SFunPtr (S Int -> IOS Int)
-- As should type functions
foreign import ccall f2         ::         F Int -> IO (F Int)
foreign export ccall "g2"   f2  ::         F Int -> IO (F Int)
foreign import ccall "&h2"  h2  :: F Char
foreign import ccall "&h2b" h2b :: FunPtr (F Int -> IO (F Int))
-- And newtype
foreign import ccall f3       ::         G1 -> G2 -> G4
foreign export ccall "g3" f3  ::         G1 -> G2 -> G4
foreign import ccall "&h3" h3 :: FunPtr (G1 -> G2 -> G4)
-- And a combination
foreign import ccall f4       ::         G1F -> F Bool -> S G4
foreign export ccall "g4" f4  ::         G1F -> F Bool -> S G4
foreign import ccall "&h4" h4 :: FunPtr (G1F -> F Bool -> S G4)
-- And a newtyped IO
foreign import ccall f5       ::         NIO Int
foreign export ccall "g5" f5  ::         NIO Int
foreign import ccall "&h5" h5 :: FunPtr (NIO Int)

