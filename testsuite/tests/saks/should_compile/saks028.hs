{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module SAKS_028 where

import Data.Kind
import Language.Haskell.TH hiding (Type)

type Functor' :: (Type -> Type) -> Constraint
class Functor' f

do sig <- reifyType ('' Functor')
   runIO $ putStrLn $ pprint sig
   return []
