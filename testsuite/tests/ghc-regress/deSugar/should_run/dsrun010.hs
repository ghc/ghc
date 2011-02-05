-- Check that pattern match failure in do-notation
-- is reflected by calling the monadic 'fail', not by a
-- runtime exception

import Control.Monad
import Data.Maybe

test :: (MonadPlus m) => [a] -> m Bool
test xs
  =   do
        (_:_) <- return xs
		-- Should fail here
        return True
    `mplus`
	-- Failure in LH arg should trigger RH arg
      do
        return False

main :: IO ()
main
  = do  let x = fromJust (test [])
        putStrLn (show x)
