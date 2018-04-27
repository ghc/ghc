{-# LANGUAGE UndecidableInstances, OverlappingInstances, Rank2Types,
    KindSignatures, EmptyDataDecls, MultiParamTypeClasses, CPP #-}

module T1735_Help.Basics where

data Proxy a = Proxy

class Data ctx a where
     gunfold :: Proxy ctx
             -> (forall b r. Data ctx b => c (b -> r) -> c r)
             -> (forall r. r -> c r)
             -> Constr
             -> c a


newtype ID x = ID { unID :: x }

fromConstrB :: Data ctx a
            => Proxy ctx
            -> (forall b. Data ctx b => b)
            -> Constr
            -> a
fromConstrB ctx f = unID . gunfold ctx k z
 where
  k c = ID (unID c f)
  z = ID

data Constr = Constr
