module Test (mkTy) where

import Lib

-- The bytecode interpreter will fail to tag the Just correctly here.
mkTy :: Int -> Ty
mkTy n = Ty (Just (n+1))

