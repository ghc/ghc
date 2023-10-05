-- Test that static pointers still work when the users try
-- to unpack StaticPtr fields.
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE LambdaCase #-}

import GHC.StaticPtr
import T12622_A

g = True

main :: IO ()
main = do
  let T s = sg :: T (Bool -> Bool)
  lookupKey s >>= \f -> print (f True)

lookupKey :: StaticPtr a -> IO a
lookupKey p = unsafeLookupStaticPtr (staticKey p) >>= \case
  Just p -> return $ deRefStaticPtr p
  Nothing -> error $ "couldn't find " ++ show (staticPtrInfo p)
