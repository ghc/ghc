{-# LANGUAGE RankNTypes, PolyKinds, GADTs, TypeApplications, PatternSynonyms #-}

module ExplicitSpecificity where

import Data.Proxy
import Data.Kind

-- GJ : Write tests that fail
-- should also include tests for {} in kind families, etc

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

  -- GJ : TODO Explicit Specificity in pattern synonyms
-- pattern Pat1 a = C2 Type a Proxy
-- pattern Pat2 {k} a = C2 k a Proxy
-- pattern Pat3 = C2 Type a Proxy
-- pattern Pat4 = forall {a}. C2 Type a Proxy

-- bar3 :: (T2 a) -> ()
-- bar3 (Pat1 Int) = ()

-- bar4 :: (T2 a) -> ()
-- bar4 (Pat2 Int) = ()

-- bar5 :: (T2 a) -> ()
-- bar5 Pat3 = ()

-- bar6 :: (T2 a) -> ()
-- bar6 Pat4 = ()
