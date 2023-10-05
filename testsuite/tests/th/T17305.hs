{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module T17305 where

import Data.Kind
import Language.Haskell.TH hiding (Type)
import System.IO

data family Foo a
data instance Foo :: Type -> Type where
  MkFoo :: Foo a

$(do i <- reify ''Foo
     runIO $ hPutStrLn stderr $ pprint i
     pure [])
