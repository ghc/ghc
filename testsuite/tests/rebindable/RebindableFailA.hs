-- Test that RebindableSyntax and the new MonadFail interact correctly.
--
-- This should fail with the message "Failed with error".

{-# LANGUAGE RebindableSyntax #-}

import Prelude hiding (fail)

fail :: String -> a
fail _ = error "Failed with error"

f :: Maybe Int -> Maybe ()
f x = do
  42 <- x
  return ()
{-# NOINLINE f #-}

main = print (f (Just 55))

