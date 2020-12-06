--
-- This is a minimal test for :doc command.
--
-- To avoid depending haddock's pretty-printing,
-- this test is constructed with simple text (without markup) only.
--

{-# LANGUAGE DuplicateRecordFields #-}
module Test where

-- | This is the haddock comment of a data declaration for Data1.
data Data1 = Val1a | Val1b

data Data2 = Val2a  -- ^ This is the haddock comment of a data value for Val2a
           | Val2b  -- ^ This is the haddock comment of a data value for Val2b

-- | This is the haddock comment of a data declaration for Data3.
newtype Data3 =
  Data3 { getData3 :: Int }

newtype Data4 =
  -- | This is the haddock comment of a data constructor for Data4.
  Data4 { getData4 :: Int }

data DupeFields1 =
  DF1 { dupeField :: Int -- ^ This is the first haddock comment of a duplicate record field.
      }

data DupeFields2 =
  DF2 { dupeField :: Int -- ^ This is the second haddock comment of a duplicate record field.
      }

data DupeFields3 =
  DF3 { dupeField :: Int -- No haddock
      }

-- | This is the haddock comment of a function declaration for func1.
func1 :: Int -> Int -> Int
func1 x y = x + y

-- This is NOT a haddock comment.
func2 :: Int -> Int -> Int
func2 x y = x + y

-- | This is the haddock comment of a function declaration for func3.
-- Here's multiple line comment for func3.
func3 :: Int -> Int -> Int
func3 x y = x + y
