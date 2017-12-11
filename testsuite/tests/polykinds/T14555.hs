{-# Language TypeInType #-}
{-# Language TypeOperators, DataKinds, PolyKinds, GADTs #-}

module T14555 where

import Data.Kind
import GHC.Types (TYPE)

data Exp :: [TYPE rep] -> TYPE rep -> Type where
--data Exp (x :: [TYPE rep]) (y :: TYPE rep) where
--data Exp (x :: [TYPE rep]) y where
  Lam :: Exp (a:xs) b -> Exp xs (a -> b)
