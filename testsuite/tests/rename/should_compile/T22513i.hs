{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module T22513i where

import Language.Haskell.TH

sp :: Q ()
sp =
    $(do
        instances <- reifyInstances ''Show [ VarT (mkName "id") ]
        [e| return () |])