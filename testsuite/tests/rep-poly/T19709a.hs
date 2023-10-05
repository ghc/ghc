{-# LANGUAGE TemplateHaskell, ExplicitForAll, PolyKinds #-}

module T19709a where

import GHC.Exts

$( let levid :: forall (r :: RuntimeRep) (a :: TYPE r). a -> a
       levid x = x
   in return [] )
