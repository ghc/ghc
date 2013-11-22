{-# LANGUAGE RoleAnnotations, StandaloneDeriving, FlexibleContexts, UndecidableInstances, GADTs, TypeFamilies #-}

import GHC.Prim (Coercible, coerce)
import Data.Monoid (mempty, First(First), Last())

newtype Age = Age Int deriving Show
newtype Foo = Foo Age deriving Show
newtype Bar = Bar Age deriving Show
newtype Baz = Baz Bar deriving Show

type role Map nominal representational
data Map a b = Map a b deriving Show

-- Higher kinded coercions (#8541)
newtype List a = List [a] deriving Show
data T f = T (f Int)

-- It should be possible to coerce _under_ undersaturated newtypes
newtype NonEtad a b = NonEtad (Either b a) deriving Show

-- It should be possible to coerce recursive newtypes, in some situations
-- (#8503)
newtype Fix f = Fix (f (Fix f))
deriving instance Show (f (Fix f)) => Show (Fix f)

newtype FixEither a = FixEither (Either a (FixEither a)) deriving Show

-- This ensures that explicitly given constraints are consulted, even 
-- at higher depths
data Oracle where Oracle :: Coercible (Fix (Either Age)) (Fix  (Either Int)) => Oracle
foo :: Oracle -> Either Age (Fix (Either Age)) -> Fix (Either Int)
foo Oracle = coerce

-- This ensures that Coercible looks into newtype instances (#8548)
data family Fam k
newtype instance Fam Int = FamInt Bool deriving Show

main = do
    print (coerce $ one                       :: Age)
    print (coerce $ age                       :: Int)
    print (coerce $ Baz (Bar age)             :: Foo)

    print (coerce (id::Bar->Bar) age          :: Foo)
    print (coerce Baz age                     :: Foo)
    print (coerce $ (Age 1, Foo age)          :: (Baz, Baz))

    print (coerce $ Map one one               :: Map Int Age)

    print (coerce $ Just one                  :: First Int)
    print (coerce $ (mempty :: Last Age)      :: Last Int)

    printT (coerce $ (T (Left age)     :: T (Either Age))  :: T (Either Int))
    printT (coerce $ (T (Left one)     :: T (Either Int))  :: T (Either Age))
    printT (coerce $ (T [one]          :: T [])            :: T List)
    printT (coerce $ (T (List [one])   :: T List)          :: T [])

    printT (coerce $ (T (NonEtad (Right age)) :: T (NonEtad Age)) :: T (NonEtad Int))

    print (coerce $ (Fix (Left ()) :: Fix (Either ())) :: Either () (Fix (Either ())))
    print (coerce $ (Left () :: Either () (Fix (Either ()))) :: Fix (Either ()))

    print (coerce $ (FixEither (Left age) :: FixEither Age) :: Either Int (FixEither Int))
    print (coerce $ (Left one :: Either Int (FixEither Age)) :: FixEither Age)

    print (coerce $ True :: Fam Int)
    print (coerce $ FamInt True :: Bool)

    foo `seq` return ()


  where one = 1 :: Int
        age = Age one
        printT (T x) = print x
