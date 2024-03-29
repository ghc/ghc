{-# LANGUAGE TemplateHaskell #-}

module T10945 where

import Language.Haskell.TH

$$(return [
   SigD (mkName "m")
        (ForallT [PlainTV (mkName "a") SpecifiedSpec]
                 []
                 (AppT (AppT ArrowT (VarT (mkName "a"))) (VarT (mkName "a"))))
 , FunD (mkName "m")
        [Clause [VisAP $ VarP (mkName "x")] (NormalB (VarE (mkName "x"))) []]
 ])
