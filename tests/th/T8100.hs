{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}

module T8100 where

import Language.Haskell.TH

data Foo a = Foo a
data Bar = Bar Int

$( do decs <- [d| deriving instance Eq a => Eq (Foo a)
                  deriving instance Ord a => Ord (Foo a) |]
      return ( StandaloneDerivD Nothing [] (ConT ''Eq `AppT` ConT ''Bar)
             : StandaloneDerivD Nothing [] (ConT ''Ord `AppT` ConT ''Bar)
             : decs ) )

blah :: Ord a => Foo a -> Foo a -> Ordering
blah = compare

buzz :: Bar -> Bar -> Ordering
buzz = compare
