{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoExplicitNamespaces #-}

module T23739_th_pprint1 where

import System.IO
import Language.Haskell.TH

do decls <-
     [d|
         f :: Integer -> forall a -> Num a => a
         f n t = fromInteger @t n
       |]
   runIO $ hPutStrLn stderr $ pprint decls
   return []
