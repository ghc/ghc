{-# LANGUAGE UndecidableInstances #-}

-- | Planning the generation of arbitrary nested arrays.
module DPH.Arbitrary.ArrayExp
        (ArrayExp(..), ArbitraryLen(..))
where
import Test.QuickCheck        
import Control.Monad
import Data.Array.Parallel.Array                (Array)
import qualified Data.Array.Parallel.Array      as A

-- ArbitraryLen -------------------------------------------------------------------
-- | Generate an arbitrary thing of a specific size.
class ArbitraryLen a where
 arbitraryLen :: Int -> Gen a


-- ArrayExp ------------------------------------------------------------------------
-- | Generate a plan for building an arbitrary array.
--
--   If we create an array directly from a list, then the internal structure 
--   is simpler than if it had been constructed by appending or concatenating
--   several other arrays. In our tests, we want to use arrays with complicated
--   internal structure, as these have more change of showing up bugs.
--
--   We split the plan generation from the actual array, so we can check
--   that the plan is covering the cases we want. We can also use the plan to 
--   generate multiple arrays with identical structure.
--   
data ArrayExp
        -- Generate a flat array of the given size.
        = XArbitrary Int

        -- Append two arbitrary arrays.
        | XAppend    ArrayExp ArrayExp
        deriving Show


-- | Generate a plan to build an array of a particular size.
instance ArbitraryLen ArrayExp where
 arbitraryLen s
  = let aFlat
         = do   return  $ XArbitrary s
         
        aAppend
         = do   split   <- choose (0, s)
                liftM2 XAppend
                        (arbitraryLen split)
                        (arbitraryLen (s - split))
                        
    in  choose (0, 10) >>= \(n :: Int) -> 
        if (s == 0
         || n < 5)
                then aFlat
                else aAppend


instance ArbitraryLen ArrayExp => Arbitrary ArrayExp where
 arbitrary      = sized arbitraryLen


