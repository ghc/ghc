{-# OPTIONS_GHC -ddump-tc-ast #-}

module T15242 where

f = (((const) 3)) ((((seq) 'a')) 'b')
g = ((((((((((id id)) id) id) id))) id))) id
