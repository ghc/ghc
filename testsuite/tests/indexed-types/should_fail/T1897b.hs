{-# LANGUAGE TypeFamilies #-}

module T1897b where

import Control.Monad
import Data.Maybe

class Bug s where
  type Depend s 
 
  next  :: s -> Depend s -> Maybe s
  start :: s
  
-- isValid :: (Bug s) => [Depend s] -> Bool
-- Inferred type should be rejected as ambiguous
isValid ds = isJust $ foldM next start ds
