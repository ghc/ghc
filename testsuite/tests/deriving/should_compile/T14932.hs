{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module T14932 where

import GHC.Exts

class Zero a where
 zero :: a
 default zero :: (Code a ~ '[xs], All Zero xs) => a
 zero = undefined

type family All c xs :: Constraint where
 All c '[] = ()
 All c (x : xs) = (c x, All c xs)

type family Code (a :: *) :: [[*]]
type instance Code B1 = '[ '[ ] ]

data B1 = B1
 deriving Zero
