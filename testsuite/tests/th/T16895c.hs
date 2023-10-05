{-# LANGUAGE TemplateHaskell #-}

module T16895c where

import Language.Haskell.TH

main = print $(infixE (Just [|1|]) [|id id|] Nothing)
