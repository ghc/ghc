{-# LANGUAGE TemplateHaskell #-}

module T10945 where

import Language.Haskell.TH

$$(return [
   SigD (mkName "m")
        (ForallT [PlainTV (mkName "a")]
                 []
                 (AppT (AppT ArrowT (VarT (mkName "a"))) (VarT (mkName "a"))))
 , FunD (mkName "m")
        [Clause [VarP (mkName "x")] (NormalB (VarE (mkName "x"))) []]
 ])
