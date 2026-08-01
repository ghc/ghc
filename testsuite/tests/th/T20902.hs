{-# LANGUAGE TemplateHaskell #-}

module T20902 where

import Language.Haskell.TH

data T = FU | FUN deriving Show

expr1 = $( conE (mkName "FU") )
expr2 = $( conE (mkName "FUN") )
expr3 = $( [| FU |] )
expr4 = $( [| FUN |] )

