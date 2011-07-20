{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module ShouldFail where

class C a b | a -> b

instance C [p] [q]
	-- Coverage condition fails
