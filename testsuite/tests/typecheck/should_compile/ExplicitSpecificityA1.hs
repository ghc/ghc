{-# LANGUAGE RankNTypes
           , PolyKinds
           , GADTs
           , TypeApplications
           , PatternSynonyms
           , ExistentialQuantification
           , StandaloneKindSignatures
           , DataKinds
           , ExistentialQuantification
#-}

module ExplicitSpecificityA1 where

import Data.Proxy
import Data.Kind

-- Type variables bound in RULES
{-# RULES "parametricity" forall (f :: forall {a}. a -> a). map f = id #-}

-- Type signatures
foo1 :: a -> a
foo1 x = x

foo2 :: forall a. a -> a
foo2 x = x

foo3 :: forall {a}. a -> a
foo3 x = x

foo4 :: forall a {b}. a -> b -> b
foo4 _ x = x

foo5 :: forall {a} b. a -> b -> b
foo5 _ x = x

bar1 :: ()
bar1 = let { x1 = foo1 42
           ; x2 = foo2 @Int 42
           ; x3 = foo3 42
           ; x4 = foo4 @Bool True 42
           ; x5 = foo5 @Int True 42
           }
       in ()

-- Data declarations
data T1 a = C1 a

data T2 (a :: k) = C2 { f2 :: Proxy a }

data T3 a where C3 :: forall k (a::k). Proxy a -> T3 a

data T4 a where C4 :: forall {k} (a::k). Proxy a -> T4 a

data T5 k (a :: k) where C5 :: forall k (a::k). Proxy a -> T5 k a

data T6 k a where C6 :: forall {k} (a::k). Proxy a -> T6 k a

bar2 :: ()
bar2 = let { x1 = C1 @Int 42
           ; x2 = C2 @Type @Int Proxy
           ; x3 = C3 @Type @Int Proxy
           ; x4 = C4 @Int Proxy
           ; x5 = C5 @Type @Int Proxy
           ; x6 = C6 @Int Proxy
           }
       in ()

-- Pattern synonyms
data T7 a where C7 :: forall a b. a -> b -> T7 a

data T8 a where C8 :: forall a {b}. a -> b -> T8 a

pattern Pat1 :: forall a. () => forall b. a -> b -> T7 a
pattern Pat1 x y = C7 x y

pattern Pat2 :: forall {a}. () => forall b. a -> b -> T7 a
pattern Pat2 x y = C7 x y

pattern Pat3 :: forall a. () => forall b. a -> b -> T8 a
pattern Pat3 x y = C8 x y

pattern Pat4 :: forall {a}. () => forall b. a -> b -> T8 a
pattern Pat4 x y = C8 x y

pattern Pat5 :: forall {a}. () => forall {b}. a -> b -> T7 a
pattern Pat5 x y = C7 x y

bar3 :: (T7 a) -> ()
bar3 (Pat1 x y) = ()
bar3 (Pat2 x y) = ()

bar4 :: (T8 a) -> ()
bar4 (Pat3 x y) = ()
bar4 (Pat4 x y) = ()

-- Existential variable quantification
data HList = HNil
           | forall {a}. HCons a HList

-- Type synonyms
type TySy = forall a {b}. Either a b

-- Standalone kind signatures
type Foo :: forall a {b}. a -> b -> b
type Foo x y = y

type Bar = Foo @Bool True 42

