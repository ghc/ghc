{-# LANGUAGE TemplateHaskell #-}

module TH_fun_par where

import Data.Foldable (for_)
import System.IO
import Language.Haskell.TH

do let eLam = [e| \a b -> (b,a) |]
       eOp  = [e| even . length |]
   e1 <- [e| const @Int @Bool (1 + 2) True |]
   e2 <- [e| $eLam (Just 'x') False |]
   e3 <- [e| $eOp "Hello" |]
   for_ [e1, e2, e3] $ \e -> do
     runIO $ hPutStrLn stderr $ pprint e
   return []
