{-# LANGUAGE UndecidableInstances,
             MultiParamTypeClasses, FunctionalDependencies #-}

-- The Show instance for S would have form
--	instance X T c => Show S
-- which is hard to deal with.  It sent GHC 5.01 into
-- an infinite loop; now it should be rejected.

module ShouldFail where

data T = T Integer

class X a b | a -> b where
    f :: a -> b

instance X T c => Show T where
    show _ = ""

data S = S T deriving Show

