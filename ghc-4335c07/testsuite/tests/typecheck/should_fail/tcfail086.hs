{-# LANGUAGE ExistentialQuantification #-}

-- !!! Check that we can't derive instances of existential types
module ShouldFail where

data Ex = forall a. Ex [a] deriving( Eq )

