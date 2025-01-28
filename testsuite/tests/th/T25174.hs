{-# LANGUAGE TemplateHaskell #-}

module T25174 where

import Language.Haskell.TH

data FUN a b = MkFUN (a -> b)

evenFUN :: $(conT (mkName "FUN")) Int Bool
evenFUN = MkFUN even

