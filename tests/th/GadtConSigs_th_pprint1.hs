{-# LANGUAGE TemplateHaskell #-}

module GadtConSigs_th_pprint1 where

import System.IO
import Language.Haskell.TH

do decls <-
     [d| data G a where
           G1 :: forall a b. (a, b) -> G a
           G2 :: forall a. forall b. (a, b) -> G a
           G3 :: forall a {b}. forall c d. forall e {f} g. G (a, b, c, d, e, f, g)
           -- G4 :: forall a -> G a   -- Uncomment later (see T25127_fail_th_quote)
       |]
   runIO $ hPutStrLn stderr $ pprint decls
   return []
