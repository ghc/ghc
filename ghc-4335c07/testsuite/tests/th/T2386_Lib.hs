{-# LANGUAGE TemplateHaskell #-}

module T2386_Lib(ExportedAbstract, makeOne) where

data ExportedAbstract = Yay String | NonYay Bool

makeOne = [| Yay "Yep" |]
