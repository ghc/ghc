{-# LANGUAGE GADTs #-}

-- This one made GHC 6.6 give the very unhelpful error
-- Foo8.hs:11:10:
--    Couldn't match kind `?' against `* -> * -> *'
--    When matching the kinds of `t :: ?' and `t1 :: * -> * -> *'
--      Expected type: t1
--      Inferred type: t
--    In the pattern: Nil

module ShouldFail where

data PatchSeq p a b where
    Nil   :: PatchSeq p a a
    U  :: p a b -> PatchSeq p a b
    (:-)  :: PatchSeq p a b -> PatchSeq p b c -> PatchSeq p a c

-- is_normal :: PatchSeq p a b -> Bool 
is_normal Nil = True 
is_normal (U _) = True 
is_normal (U _ :- _) = True 
is_normal _ = False 
