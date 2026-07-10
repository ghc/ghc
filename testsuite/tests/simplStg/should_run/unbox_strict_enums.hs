-- | Test that strict enumeration fields are unpacked automatically
-- by -funbox-strict-enums (on by default with -O), without UNPACK pragmas.
-- See Note [UNPACK for enum types] in GHC.Types.Id.Make.

module Main where

import GHC.Exts.Heap.Closures (closureSize, asBox)
import Control.Exception (evaluate)

data Color = Red | Green | Blue | Yellow | Purple
  deriving (Show, Eq)

-- No pragmas: all eight fields should be unpacked to a Word8# each,
-- fitting in a single payload word.
data Auto = Auto !Bool !Color !Bool !Color !Bool !Color !Bool !Color
  deriving Show

-- Explicit UNPACK on every field: reference for the fully packed size.
data Unpacked = Unpacked
  {-# UNPACK #-} !Bool  {-# UNPACK #-} !Color
  {-# UNPACK #-} !Bool  {-# UNPACK #-} !Color
  {-# UNPACK #-} !Bool  {-# UNPACK #-} !Color
  {-# UNPACK #-} !Bool  {-# UNPACK #-} !Color
  deriving Show

-- NOUNPACK on every field: reference for the pointer-per-field size,
-- and checks that the pragma still overrides -funbox-strict-enums.
data Boxed = Boxed
  {-# NOUNPACK #-} !Bool  {-# NOUNPACK #-} !Color
  {-# NOUNPACK #-} !Bool  {-# NOUNPACK #-} !Color
  {-# NOUNPACK #-} !Bool  {-# NOUNPACK #-} !Color
  {-# NOUNPACK #-} !Bool  {-# NOUNPACK #-} !Color
  deriving Show

sizeOf :: a -> IO Int
sizeOf x = closureSize . asBox <$> evaluate x

main :: IO ()
main = do
    -- Round-trip: construction and pattern matching (via Show) still work.
    print (Auto True Red False Purple True Green False Yellow)
    print (Auto False Blue True Yellow False Red True Purple)

    s_auto     <- sizeOf (Auto     True Red False Purple True Green False Yellow)
    s_unpacked <- sizeOf (Unpacked True Red False Purple True Green False Yellow)
    s_boxed    <- sizeOf (Boxed    True Red False Purple True Green False Yellow)
    putStrLn $ "auto as small as explicit UNPACK: " ++ show (s_auto == s_unpacked)
    putStrLn $ "NOUNPACK larger than auto:        " ++ show (s_boxed > s_auto)
