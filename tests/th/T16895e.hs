{-# LANGUAGE TemplateHaskell #-}

module T16895e where

import Language.Haskell.TH

main = print $(infixE Nothing [|id id|] Nothing)
