{-# LANGUAGE TypeFamilies #-}
-- Test Trac #2851

module T2851 where

type family F a :: *

data D a = D (F a)
    deriving (Show)
