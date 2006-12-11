{-# OPTIONS_GHC -fglasgow-exts #-}

-- See Trac #1033

module Pointful' where

import Data.Generics
import Control.Monad.State

data HsExp = HsWildCard deriving( Typeable, Data )
data HsName = HsName deriving( Typeable, Data )

-- rename :: () -> HsExp -> State (HsName, [HsName]) HsExp  
-- Type sig commented out
rename1 = \_ -> everywhereM (mkM (\e -> case e of HsWildCard -> return e))

rename2 _ = everywhereM (mkM (\e -> case e of  HsWildCard -> return e))

uncomb1 :: State (HsName, [HsName]) HsExp  
uncomb1 = rename1 () undefined  

uncomb2 :: State (HsName, [HsName]) HsExp  
uncomb2 = rename2 () undefined  



