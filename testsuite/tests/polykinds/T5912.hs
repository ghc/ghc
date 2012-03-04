{-# LANGUAGE DataKinds #-}

-- This bug related to type trimming, and 
-- hence showed up only with -O0

module Bug() where

data UnaryTypeC a = UnaryDataC a

type Bug = 'UnaryDataC


