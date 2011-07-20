{-# LANGUAGE RankNTypes #-}

-- A test for rank-3 types

module ShouldCompile where

data Fork a                   =  ForkC a a

mapFork                       :: forall a1 a2 . (a1 -> a2) -> (Fork a1 -> Fork a2)
mapFork mapA (ForkC a1 a2)    =  ForkC (mapA a1) (mapA a2)

data SequF s a	  =  EmptyF | ZeroF (s (Fork a)) | OneF a (s (Fork a))
newtype HFix h a  =  HIn (h (HFix h) a)

type Sequ =  HFix SequF

mapSequF  :: forall s1 s2 . (forall b1 b2 . (b1 -> b2) -> (s1 b1 -> s2 b2))
                         -> (forall a1 a2 . (a1 -> a2) -> (SequF s1 a1 -> SequF s2 a2))
mapSequF mapS mapA EmptyF     =  EmptyF
mapSequF mapS mapA (ZeroF as) =  ZeroF (mapS (mapFork mapA) as)
mapSequF mapS mapA (OneF a as)=  OneF (mapA a) (mapS (mapFork mapA) as)

mapHFix :: forall h1 h2 . (forall f1 f2 . (forall c1 c2 . (c1 -> c2) -> (f1 c1 -> f2 c2))
                                          -> (forall b1 b2 . (b1 -> b2) -> (h1 f1 b1 -> h2 f2 b2)))
                          -> (forall a1 a2 . (a1 -> a2) -> (HFix h1 a1 -> HFix h2 a2))
mapHFix mapH mapA (HIn v)     =  HIn (mapH (mapHFix mapH) mapA v)

mapSequ  :: forall a1 a2 . (a1 -> a2) -> (Sequ a1 -> Sequ a2)
mapSequ	 =  mapHFix mapSequF

