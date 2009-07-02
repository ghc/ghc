{-# LANGUAGE TypeFamilies #-}

-- This is actually perfectly ok!

module NonLinearSigErr where

type family E a b
type instance E a (a :: *) = [a]
