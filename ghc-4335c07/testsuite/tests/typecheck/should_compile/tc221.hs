{-# LANGUAGE GADTs #-}

-- A program very like this triggered a kind error with GHC 6.6

module Foo where

data PatchSeq p a b where
    Nil   :: PatchSeq p a b
    U  :: p a b -> PatchSeq p a b
    (:-)  :: PatchSeq p a b -> PatchSeq p b c -> PatchSeq p a c

-- is_normal :: PatchSeq p a b -> Bool 
is_normal Nil = True 
is_normal (U _) = True 
is_normal (U _ :- _) = True 
is_normal _ = False 
