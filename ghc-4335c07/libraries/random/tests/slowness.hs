
-- http://hackage.haskell.org/trac/ghc/ticket/427

-- Two (performance) problems in one:

{-# OPTIONS -fffi #-}
module Main (main) where

import Control.Monad
import System.Random

foreign import ccall unsafe "random" _crandom :: IO Int
foreign import ccall unsafe "stdlib.hs" rand :: IO Int

randomInt :: (Int, Int) -> IO Int
randomInt (min,max) = do
--    n <- _crandom
    n <- rand
    return $ min + n `rem` range
    where
        range = max - min + 1

main = replicateM_ (5*10^6) $ do
    x <- randomRIO (0::Int,1000) :: IO Int
--    x <- randomInt (0::Int,1000) :: IO Int
    x `seq` return ()
    return ()

-- First, without the "seq" at the end, hardly anything is
-- evaluated and we're building huge amounts of thunks.
-- Three ideas about this one:
-- - Blame the user :)
-- - data StdGen = StdGen !Int !Int
--   Use strict fields in StdGen. Doesn't actually help
-- (at least in this example).
-- - Force evaluation of the StdGen in getStdRandom.
--   Does help in this example, but also changes behaviour
-- of the library:
--   x <- randomRIO undefined
--   currently dies only when x (or the result of a later
-- randomRIO) is evaluated. This change causes it to die
-- immediately.

-- Second, even _with_ the "seq", replacing "randomRIO" by
-- "randomInt" speeds the thing up with a factor of about
-- 30. (2 to 3.6, in a "real world" university practicum
-- exercise of 900 lines of code)
-- Even given the fact that they're not really doing the
-- same thing, this seems rather much :(

--------------------------------------------------------------------------------

-- [2011.06.28] RRN:
-- I'm currently seeing 1425 ms vs 43 ms for the above.  33X
-- difference. If I use rand() instead it's about 52ms.
