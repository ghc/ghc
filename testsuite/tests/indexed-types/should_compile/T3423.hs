{-# LANGUAGE TypeFamilies, UndecidableInstances, StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module T3423 where

newtype Trie m k a = Trie (Maybe a, m (SubKey k) (Trie m k a))

type family SubKey k
type instance SubKey [k] = k

deriving instance (Eq (m k (Trie m [k] a)), Eq a) 
               => Eq (Trie m [k] a)
