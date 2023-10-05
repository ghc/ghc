{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module T1476b where

import Language.Haskell.TH

baz = [| \ $( return $ VarP $ mkName "x" ) -> x |]

