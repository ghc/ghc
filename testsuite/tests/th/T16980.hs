{-# LANGUAGE TemplateHaskell #-}

module T16980 where

import Language.Haskell.TH
import Language.Haskell.TH.Ppr

import System.IO

aNumber = 5

do VarI name1 t1 _ <- reify 'aNumber
   runIO . print $ ppr_sig name1 t1
   runIO . print =<< reifyType 'aNumber
   runIO $ hFlush stdout
   return []
