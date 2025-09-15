{-# LANGUAGE GADTs #-}

module STree where

data STree a where
    STreeIM :: {
        l :: v a ,
        stree :: a
    } -> STree a

insert :: STree a -> STree a
insert s = s { stree = undefined }
