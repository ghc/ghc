{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.Exts (Any, withDict)
import GHC.TypeLits
import Type.Reflection (TypeRep, typeRep, withTypeable)

-----
-- reflection
-----

class Reifies s a | s -> a where
  reflect :: proxy s -> a

instance KnownNat n => Reifies n Integer where
  reflect = natVal

reify :: forall a r. a -> (forall (s :: Type). Reifies s a => Proxy s -> r) -> r
{-# NOINLINE reify #-} -- See Note [NOINLINE someNatVal] in GHC.TypeNats
reify a k = withDict @(Reifies (Any @Type) a)
                     @(forall (proxy :: Type -> Type). proxy Any -> a)
                     (const a) (k @Any) Proxy

class Given a where
  given :: a

withGift :: forall a b.
            (Given a => Proxy a -> b)
         ->        a -> Proxy a -> b
withGift f x y = withDict @(Given a) @a x f y

give :: forall a r. a -> (Given a => r) -> r
give a k = withGift (\_ -> k) a Proxy

foo :: Given Int => Bool -> Int
foo True  = 42
foo False = given

-----
-- singletons
-----

type family Sing :: k -> Type
data SBool :: Bool -> Type where
  SFalse :: SBool False
  STrue  :: SBool True
deriving instance Show (SBool z)
type instance Sing @Bool = SBool

class SingI a where
  sing :: Sing a

data SingInstance (a :: k) where
  SingInstance :: SingI a => SingInstance a

singInstance :: forall k (a :: k). Sing a -> SingInstance a
singInstance s = with_sing_i SingInstance
  where
    with_sing_i :: (SingI a => SingInstance a) -> SingInstance a
    with_sing_i si = withDict @(SingI a) @(Sing a) s si

withSingI :: Sing n -> (SingI n => r) -> r
withSingI sn r =
  case singInstance sn of
    SingInstance -> r

-----

main :: IO ()
main = do
  -- Type.Reflection
  let f :: forall a. TypeRep a -> IO ()
      f tr = withTypeable tr $ print $ typeRep @a
   in f $ typeRep @Int

  -- GHC.TypeLits
  print $ someCharVal 'a'
  print $ someNatVal 42
  print $ someSymbolVal "Hello World"

  -- reflection
  print $ reify 6 (\p -> reflect p + reflect p)
  print $ give (23 :: Int) foo True

  -- singletons
  print $ withSingI SFalse (sing @False)
