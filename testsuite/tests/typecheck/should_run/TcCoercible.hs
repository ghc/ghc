{-# LANGUAGE RoleAnnotations #-}

import GHC.Prim (coerce)
import Data.Monoid (mempty, First(First), Last())

newtype Age = Age Int deriving Show
newtype Foo = Foo Age deriving Show
newtype Bar = Bar Age deriving Show
newtype Baz = Baz Bar deriving Show

type role Map nominal representational
data Map a b = Map a b deriving Show

main = do
    print (coerce $ one                       :: Age)
    print (coerce $ Age 1                     :: Int)
    print (coerce $ Baz (Bar (Age 1))         :: Foo)

    print (coerce (id::Bar->Bar) (Age 1)      :: Foo)
    print (coerce Baz (Age 1)                 :: Foo)
    print (coerce $ (Age 1, Foo (Age 1))      :: (Baz, Baz))

    print (coerce $ Map one one               :: Map Int Age)

    print (coerce $ Just one                  :: First Int)
    print (coerce $ (mempty :: Last Age)      :: Last Int)

  where one = 1 :: Int



