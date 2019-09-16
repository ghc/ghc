-- Tests that the noinline doesn't hide functions' demand signatures from the
-- demand analysis. See #16588.

module T16588 where
  
import GHC.Magic

func :: a -> a
func x = x

-- This should place a strict demand on its argument.
func2 :: Integer -> Integer
func2 x = noinline func x

