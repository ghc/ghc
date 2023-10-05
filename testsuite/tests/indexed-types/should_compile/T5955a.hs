{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module T5955a where


class (Eq (Multi a)) => MultiPrim a where
    data Multi a

instance MultiPrim Int where
    newtype Multi Int = MultiInt Int deriving (Eq)


