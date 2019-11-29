{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module T1476 where

import Language.Haskell.TH

foo $( return $ VarP $ mkName "x" ) = x
bar $( [p| x |] ) = x

baz = [| \ $( return $ VarP $ mkName "x" ) -> $(dyn "x") |]
