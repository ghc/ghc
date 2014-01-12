{-# LANGUAGE TemplateHaskell #-}
module T3395 where

import Language.Haskell.TH

foo = $(return $
     CompE
         [NoBindS (VarE $ mkName "undefined")
         ,BindS (VarP $ mkName "r1") (VarE $ mkName "undefined") ])
