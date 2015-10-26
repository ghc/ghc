{-# LANGUAGE TemplateHaskell #-}

module T10267 where

import Language.Haskell.TH
import T10267a

[d| i :: a -> a
    i = _foo

    j :: a -> a
    j x = _ |]

$(return [
   SigD (mkName "k")
        (ForallT [PlainTV (mkName "a")]
                 []
                 (AppT (AppT ArrowT (VarT (mkName "a"))) (VarT (mkName "a"))))
 , FunD (mkName "k")
        [Clause [] (NormalB (UnboundVarE (mkName "_foo"))) []]
 ])

$(return [
   SigD (mkName "l")
        (ForallT [PlainTV (mkName "a")]
                 []
                 (AppT (AppT ArrowT (VarT (mkName "a"))) (VarT (mkName "a"))))
 , FunD (mkName "l")
        [Clause [VarP (mkName "x")] (NormalB (UnboundVarE (mkName "_"))) []]
 ])

foo :: a -> a
foo x = $varX
