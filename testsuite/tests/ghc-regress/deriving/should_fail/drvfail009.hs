{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

-- Various newtype-deriving failures

module ShouldFail where


class C a b 

newtype T1 = T1 Int deriving( C )
	-- Wrong arity

newtype T2 = T2 Int deriving( Monad )
	-- Type constructor has wrong kind

newtype T3 a = T3 Int deriving( Monad )
	-- Rep type has wrong kind

newtype T4 a = T4 (Either a a) deriving( Monad )
	-- Eta fails
