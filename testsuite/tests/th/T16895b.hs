{-# LANGUAGE TemplateHaskell #-}

module T16895b where

import Language.Haskell.TH

main = print $(infixE Nothing [|id id|] (Just [|2|]))
