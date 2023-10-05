{-# LANGUAGE TemplateHaskell #-}

module T16895d where

import Language.Haskell.TH

main = print $(infixE (Just [|1|]) [|(id id)|] (Just [|2|]))
