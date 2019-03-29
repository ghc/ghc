{-# LANGUAGE TopLevelKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module TLKS_028 where

import Data.Kind
import Language.Haskell.TH hiding (Type)

type Functor' :: (Type -> Type) -> Constraint
class Functor' f

do Just sig <- reifyKiSig ('' Functor')
   runIO $ putStrLn $ pprint sig
   return []
