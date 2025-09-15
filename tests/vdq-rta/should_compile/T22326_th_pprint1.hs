{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module T22326_th_pprint1 where

import System.IO
import Language.Haskell.TH

do decls <-
     [d|
         -- Definition:
         f :: Integer -> forall a -> Num a => a
         f n (type _) = fromInteger n

         -- Usage:
         x = 42 `f` (type Double)
         n = f 42 (type Integer)
       |]
   runIO $ hPutStrLn stderr $ pprint decls
   return []
