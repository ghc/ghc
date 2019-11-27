{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module T2386_Lib(ExportedAbstract, makeOne) where

data ExportedAbstract = Yay String | NonYay Bool

makeOne = [| Yay "Yep" |]
