{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeFamilies, TypeOperators, EmptyDataDecls #-}

module T4174 where

import Data.Kind (Type)

data True
data False

data Minor1

data GHC6'8 m
data GHC6'10 m

type family a :<=: b :: {-Bool-} Type
type instance GHC6'10 m1 :<=: GHC6'8 m2 = False

type a :>=: b = b :<=: a

data Way ghcVersion tablesNextToCode profiling threaded

type family GHCVersion way :: {-GHCVersion-} Type
type instance GHCVersion (Way v n p t) = v

type family Threaded way :: {-Bool-} Type
type instance Threaded (Way v n p t) = t

data Field w s t
data SmStep
data RtsSpinLock

field :: String -> m (Field w a b)
field = undefined

type family WayOf (m :: Type -> Type) :: Type

sync_large_objects :: (Monad m, 
                       (GHCVersion (WayOf m) :>=: GHC6'10 Minor1) ~ True, 
                       Threaded (WayOf m) ~ True) 
                   => m (Field (WayOf m) SmStep RtsSpinLock)
sync_large_objects = field "sync_large_objects"

testcase :: Monad m => m (Field (Way (GHC6'8 minor) n t p) a b)
testcase = sync_large_objects

{- Wanted constraints from the occurrence of sync_large_objects

   (WayOf m) ~ (Way (GHC6'8 minor) n t p)
   a ~ SmStep
   b ~ RtsSpinLock

   Threaded (WayOf m) ~ True 
     == Threaded (Way (GHC6'8 minor) n t p) ~ True 
     == p ~ True   

   (GHCVersion (WayOf m) :>=: GHC6'10 Minor1) ~ True, 
     == (GHC6'10 Minor1 :<=: GHCVersion (WayOf m)) ~ True, 
     == (GHC6'10 Minor1 :<=: GHCVersion (Way (GHC6'8 minor) n t p))) ~ True, 
     == (GHC6'10 Minor1 :<=: GHC6'8 minor) ~ True
     == False ~ True

-}
