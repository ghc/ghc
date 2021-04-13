{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.Exts (Any, magicDict)
import GHC.TypeLits

-----
-- reflection
-----

class Reifies s a | s -> a where
  reflect :: proxy s -> a

reify :: forall a r. a -> (forall (s :: Type). Reifies s a => Proxy s -> r) -> r
reify a k = magicDict @(Reifies (Any @Type) a) (k @Any) (const a) Proxy

class Given a where
  given :: a

withGift :: forall a b.
            (Given a => Proxy a -> b)
         ->        a -> Proxy a -> b
withGift f x y = magicDict @(Given a) f x y

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
    with_sing_i si = magicDict @(SingI a) si s

withSingI :: Sing n -> (SingI n => r) -> r
withSingI sn r =
  case singInstance sn of
    SingInstance -> r

-----

main :: IO ()
main = do
  -- GHC.TypeLits
  -- print $ someCharVal 'a'
  -- print $ someNatVal 42
  -- print $ someSymbolVal "Hello World"

  -- reflection
  print $ give (23 :: Int) foo True

  -- singletons
  print $ withSingI SFalse (sing @False)
