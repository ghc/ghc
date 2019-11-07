{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module SAKS_028 where

import System.IO
import Data.Kind
import Language.Haskell.TH hiding (Type)

type Functor' :: (Type -> Type) -> Constraint
class Functor' f

do sig <- reifyType ('' Functor')
   runIO $ hPutStrLn stderr $ pprint sig
   return []
