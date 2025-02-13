{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module T25672 where

data IntOrWord (isInt :: Bool) where
    Int :: !Int -> IntOrWord True
    Word :: !Word -> IntOrWord False

data WrapIntOrWord (isInt :: Bool)
    = WrapIntOrWord {lit :: {-# UNPACK #-} !(IntOrWord isInt)}

boom :: WrapIntOrWord True
boom = WrapIntOrWord (Int 1)
