{-# LANGUAGE TemplateHaskell #-}

module T9738 where

import System.IO
import Language.Haskell.TH

data Foo = MkFoo

$( do decs <- [d| {-# ANN type Foo "hi" #-}
                  {-# ANN MkFoo "there" #-}
                  {-# ANN module "Charley" #-}
                   |]
      runIO $ print decs
      runIO $ hFlush stdout
      return [] )
