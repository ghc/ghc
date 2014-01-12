{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE EmptyDataDecls #-}
module T6055 where

data Int1  = Int1
data Word1 = Word1

data D1
data D2


class Succ x y | x -> y
instance Succ D1 D2


class Add' x y z | x y -> z

instance Succ y z => Add' D1 y z


class (Add' x y z) => Add x y z | x y -> z
instance (Add' D1 y z) => Add D1 y z


class IsSized a s | a -> s where

instance IsSized Int1  D1
instance IsSized Word1 D1

instance (IsSized a s, Add s s ns) =>
   IsSized (Pair a) ns where

data Pair a = Pair a a


switchFPPred ::
   (IsSized v0 s, IsSized v1 s) =>
   v0 -> v1
switchFPPred = undefined

cmpss :: Pair Word1 -> Pair Int1
cmpss = switchFPPred
