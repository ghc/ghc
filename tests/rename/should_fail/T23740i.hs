{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE RequiredTypeArguments #-}

module T23740i where

import Language.Haskell.TH

sp :: Q ()
sp =
    $(do
        instances <- reifyInstances ''Show [ VarT (mkName "id") ]
        [e| return () |])