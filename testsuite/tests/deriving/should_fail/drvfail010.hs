{-# LANGUAGE DeriveDataTypeable #-}

module ShouldFail where
import Data.Typeable

data A a b c d e f g h i j = A deriving (Typeable)
	-- Too many args

data B a b = B (a b) deriving (Typeable)
	-- Non type-kind args