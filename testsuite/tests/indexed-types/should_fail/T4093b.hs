{-# LANGUAGE GADTs, EmptyDataDecls, ScopedTypeVariables, TypeFamilies #-}

module T4093b where

data C
data O

type family   EitherCO e a b :: *
type instance EitherCO C a b = a
type instance EitherCO O a b = b

data MaybeC ex t where
  JustC    :: t -> MaybeC C t
  NothingC ::      MaybeC O t

data Block (n :: * -> * -> *) e x


blockToNodeList ::
  forall n e x. (EitherCO e (A C O n) (A O O n) ~ A e O n,
                 EitherCO x (A C C n) (A C O n) ~ A C x n) =>
    Block n e x -> A e x n

type A e x n = (MaybeC e (n C O), MaybeC x (n O C))
blockToNodeList b = foldBlockNodesF (f, l) b z 
  where
    z :: EitherCO e (EitherCO e (A C O n) (A O O n)) (EitherCO e (A C O n) (A O O n))
    z = undefined

    f :: n C O -> EitherCO e (A C O n) (A O O n) -> EitherCO e (A C O n) (A O O n)
    f n _ = (JustC n, NothingC)

    l :: n O C -> EitherCO e (A C O n) (A O O n) -> EitherCO e (A C C n) (A O C n)
    l _ = undefined

foldBlockNodesF  :: forall n a b c e x .
                   ( n C O       -> a -> b
                   , n O C       -> b -> c)
                 -> (Block n e x -> EitherCO e a b -> EitherCO x c b)
foldBlockNodesF _ = undefined
