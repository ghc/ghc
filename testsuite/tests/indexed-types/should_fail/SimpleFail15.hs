{-# LANGUAGE TypeFamilies #-}

module ShouldFail where

foo :: (a,b) -> (a~b => t) -> (a,b)
foo p x = p 
