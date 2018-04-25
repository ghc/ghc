{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
-- | Per Conor McBride, the 'Newtype' typeclass represents the packing and
-- unpacking of a newtype, and allows you to operatate under that newtype with
-- functions such as 'ala'.
module Distribution.Compat.Newtype (
    Newtype (..),
    ala,
    alaf,
    pack',
    unpack',
    ) where

import Data.Functor.Identity (Identity (..))
import Data.Monoid (Sum (..), Product (..), Endo (..))

-- | The @FunctionalDependencies@ version of 'Newtype' type-class.
--
-- /Note:/ for actual newtypes the implementation can be
-- @pack = coerce; unpack = coerce@. We don't have default implementation,
-- because @Cabal@ have to support older than @base >= 4.7@ compilers.
-- Also, 'Newtype' could witness a non-structural isomorphism.
class Newtype n o | n -> o where
    pack   :: o -> n
    unpack :: n -> o

instance Newtype (Identity a) a where
    pack   = Identity
    unpack = runIdentity

instance Newtype (Sum a) a where
    pack   = Sum
    unpack = getSum

instance Newtype (Product a) a where
    pack   = Product
    unpack = getProduct

instance Newtype (Endo a) (a -> a) where
    pack   = Endo
    unpack = appEndo

-- |
--
-- >>> ala Sum foldMap [1, 2, 3, 4 :: Int]
-- 10
--
-- /Note:/ the user supplied function for the newtype is /ignored/.
--
-- >>> ala (Sum . (+1)) foldMap [1, 2, 3, 4 :: Int]
-- 10
ala :: (Newtype n o, Newtype n' o') => (o -> n) -> ((o -> n) -> b -> n') -> (b -> o')
ala pa hof = alaf pa hof id

-- |
--
-- >>> alaf Sum foldMap length ["cabal", "install"]
-- 12
--
-- /Note:/ as with 'ala', the user supplied function for the newtype is /ignored/.
alaf :: (Newtype n o, Newtype n' o') => (o -> n) -> ((a -> n) -> b -> n') -> (a -> o) -> (b -> o')
alaf _ hof f = unpack . hof (pack . f)

-- | Variant of 'pack', which takes a phantom type.
pack' :: Newtype n o => (o -> n) -> o -> n
pack' _ = pack

-- | Variant of 'pack', which takes a phantom type.
unpack' :: Newtype n o => (o -> n) -> n -> o
unpack' _ = unpack
