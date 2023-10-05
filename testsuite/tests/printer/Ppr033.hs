{-# LANGUAGE TemplateHaskell            #-}

import Language.Haskell.TH

makeLenses '' PostscriptFont

ty :: Q Type
ty = [t| Int |]

f :: $ty
f = undefined

g :: $(ty)
g = undefined

thb = $(do { let x = mkName "x"
                 v = return (LamE [VarP x] $ VarE x)
           ; [| $v . id |] })

foo2 :: A Bool
foo2 = $$(y)
