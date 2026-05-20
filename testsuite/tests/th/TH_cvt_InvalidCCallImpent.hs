{-# LANGUAGE TemplateHaskell #-}

module TH_cvt_InvalidCCallImpent where

import Language.Haskell.TH

$(return [ForeignD (ImportF CCall Safe "foo bar baz"
    (mkName "myFun") (ConT (mkName "T")))])
