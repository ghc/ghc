{-# LANGUAGE TemplateHaskell #-}

module T27013th where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List ()

-- Check that the implicitly imported GHC.Essentials doesn't leak into 'reifyModule'.
x :: String
x = $(do ModuleInfo ms <- reifyModule =<< thisModule
         let mod_names = [ m | Module _ (ModName m) <- ms ]
         if "GHC.Essentials" `elem` mod_names
           then fail ("reifyModule reported an implicit import: " ++ show mod_names)
           else stringE "ok")
