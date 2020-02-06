--
-- This is a minimal test for :doc command.
--
-- To avoid depending haddock's pretty-printing,
-- this test is constructed with simple text (without markup) only.
--

module Test where

-- | This is the haddock comment of a data declaration for Data1.
data Data1 = Val1a | Val1b

data Data2 = Val2a  -- ^ This is the haddock comment of a data value for Val2a
           | Val2b  -- ^ This is the haddock comment of a data value for Val2b


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
