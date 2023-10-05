{-# LANGUAGE TemplateHaskell #-}

module T5508 where

import Language.Haskell.TH

thb = $(do { let x = mkName "x"
                 v = return (LamE [VarP x] $ VarE x)
           ; [| $v . id |] })
