{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module T12646 where

import Language.Haskell.TH hiding (Type)
import System.IO
import Data.Kind (Type)

type F :: k -> Type
type family F a where
    F (a :: Type -> Type) = Int
    F (a :: k) = Char

$(do info <- reify ''F
     runIO $ putStrLn $ pprint info
     runIO $ hFlush stdout
     return [])
