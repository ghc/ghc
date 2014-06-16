

-- use of mdo requires an extension,
-- so let's try not enabling it

module Main (main) where

import Control.Monad.Fix

main :: IO ()
main = mdo x <- return (1:x)
	   return ()
