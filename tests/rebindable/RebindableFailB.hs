-- Test that RebindableSyntax and the new MonadFail interact correctly.
--
-- This should print "Just ()"

{-# LANGUAGE RebindableSyntax #-}

import Prelude hiding (fail)

fail :: String -> a
fail _ = error "Failed with error"

f :: Maybe Int -> Maybe ()
f x = do
  42 <- x
  return ()
{-# NOINLINE f #-}

main = print (f (Just 42))


