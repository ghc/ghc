-- test Lifting instances

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}

module TH_Lift where

import Language.Haskell.TH.Syntax
import Data.Ratio
import Data.Word
import Data.Int
import Numeric.Natural
import Data.List.NonEmpty
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

a :: Integer
a = $( (\x -> [| x |]) (5 :: Integer) )

b :: Int
b = $( (\x -> [| x |]) (5 :: Int) )

b1 :: Int8
b1 = $( (\x -> [| x |]) (5 :: Int8) )

b2 :: Int16
b2 = $( (\x -> [| x |]) (5 :: Int16) )

b3 :: Int32
b3 = $( (\x -> [| x |]) (5 :: Int32) )

b4 :: Int64
b4 = $( (\x -> [| x |]) (5 :: Int64) )

c :: Word
c = $( (\x -> [| x |]) (5 :: Word) )

d :: Word8
d = $( (\x -> [| x |]) (5 :: Word8) )

e :: Word16
e = $( (\x -> [| x |]) (5 :: Word16) )

f :: Word32
f = $( (\x -> [| x |]) (5 :: Word32) )

g :: Word64
g = $( (\x -> [| x |]) (5 :: Word64) )

g1 :: Natural
g1 = $( (\x -> [| x |]) (5 :: Natural) )

h :: Rational
h = $( (\x -> [| x |]) (5 % 3 :: Rational) )

h1 :: Float
h1 = $( (\x -> [| x |]) (pi :: Float) )

h2 :: Double
h2 = $( (\x -> [| x |]) (pi :: Double) )

i :: Char
i = $( (\x -> [| x |]) 'x' )

j :: Bool
j = $( (\x -> [| x |]) True )

k :: Maybe Char
k = $( (\x -> [| x |]) (Just 'x') )

l :: Either Char Bool
l = $( (\x -> [| x |]) (Right False :: Either Char Bool) )

m :: [Char]
m = $( (\x -> [| x |]) "hi!")

n :: ()
n = $( (\x -> [| x |]) () )

o :: (Bool, Char, Int)
o = $( (\x -> [| x |]) (True, 'x', 4 :: Int) )

p :: NonEmpty Char
p = $( (\x -> [| x |])  ('a' :| "bcde") )

exp :: Exp
exp = $( [| 3 + 4 |] >>= lift )

texp :: TExp Int
texp = $$( examineCode [|| 3 + 4 ||] `bindCode` liftTyped )

bytes :: Bytes
bytes = $(do
  let (fp, offset, size) = B.toForeignPtr (B.pack [72, 101, 108, 108, 111]) -- "hello"#
  let bytes = Bytes { bytesPtr = fp
                    , bytesOffset = fromIntegral offset
                    , bytesSize = fromIntegral size
                    }
  lift bytes)
