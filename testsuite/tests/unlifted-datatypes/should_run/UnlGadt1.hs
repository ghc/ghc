{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

import GHC.Exts
import GHC.Types

data T a :: UnliftedType where
  TInt :: T Int

f :: T a -> Int
f _ = 0

g :: T a -> T a
g TInt = TInt
{-# NOINLINE g #-}

main = do
  case g TInt of TInt -> putStrLn "should see this"
  print (f (error "boom")) -- crashes!
