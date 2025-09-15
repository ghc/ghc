{-# LANGUAGE ImplicitParams, FlexibleContexts #-}

module ShouldFail where

class (?imp :: Int) => D t where
    methodD :: t -> t

instance (?imp :: Int) => D Int where
    methodD x = x + ?imp

test :: D Int => Int -- Requires FlexibleContexts
test = methodD ?imp

-- Should get reasonable error about unbound ?imp
use :: IO ()
use = print test
