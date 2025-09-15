{-# LANGUAGE ImplicitParams #-}

module ShouldFail where

class (?imp :: Int) => D t where
    methodD :: t -> t

-- Don't repeat implicit parameter constraint on the instance
instance D Int where
    methodD x = x + ?imp
