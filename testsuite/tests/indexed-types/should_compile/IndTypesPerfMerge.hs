{-# LANGUAGE EmptyDataDecls, TypeFamilies, UndecidableInstances,
             ScopedTypeVariables, OverlappingInstances, TypeOperators,
             FlexibleInstances, NoMonomorphismRestriction,
             MultiParamTypeClasses #-}
module IndTypesPerfMerge where

data a :* b = a :* b
infixr 6 :*

data TRUE
data FALSE
data Zero
data Succ a

type family Equals m n
type instance Equals Zero Zero = TRUE
type instance Equals (Succ a) Zero = FALSE
type instance Equals Zero (Succ a) = FALSE
type instance Equals (Succ a) (Succ b) = Equals a b

type family LessThan m n
type instance LessThan Zero Zero = FALSE
type instance LessThan (Succ n) Zero = FALSE
type instance LessThan Zero (Succ n) = TRUE
type instance LessThan (Succ m) (Succ n) = LessThan m n

newtype Tagged n a = Tagged a deriving (Show,Eq)

type family Cond p a b

type instance Cond TRUE a b = a
type instance Cond FALSE a b = b

class Merger a where
    type Merged a
    type UnmergedLeft a
    type UnmergedRight a
    mkMerge :: a -> UnmergedLeft a -> UnmergedRight a -> Merged a

class Mergeable a b where
    type MergerType a b
    merger :: a -> b -> MergerType a b

merge x y = mkMerge (merger x y) x y

data TakeRight a
data TakeLeft a
data DiscardRightHead a b c d
data LeftHeadFirst a b c d
data RightHeadFirst a b c d
data EndMerge

instance Mergeable () () where
    type MergerType () () = EndMerge
    merger = undefined

instance Mergeable () (a :* b) where
    type MergerType () (a :* b) = TakeRight (a :* b)
    merger = undefined
instance Mergeable (a :* b) () where
    type MergerType (a :* b) () = TakeLeft (a :* b)
    merger = undefined

instance Mergeable (Tagged m a :* t1) (Tagged n b :* t2) where
    type MergerType (Tagged m a :* t1) (Tagged n b :* t2) =
        Cond (Equals m n) (DiscardRightHead (Tagged m a) t1 (Tagged n b) t2)
           (Cond (LessThan m n) (LeftHeadFirst (Tagged m a) t1 (Tagged n b) t2)
               (RightHeadFirst (Tagged m a ) t1 (Tagged n b) t2))
    merger = undefined

instance Merger EndMerge where
    type Merged EndMerge = ()
    type UnmergedLeft EndMerge = ()
    type UnmergedRight EndMerge = ()
    mkMerge _ () () = ()

instance Merger (TakeRight a) where
    type Merged (TakeRight a) = a
    type UnmergedLeft (TakeRight a) = ()
    type UnmergedRight (TakeRight a) = a
    mkMerge _ () a = a

instance Merger (TakeLeft a) where
    type Merged (TakeLeft a) = a
    type UnmergedLeft (TakeLeft a) = a
    type UnmergedRight (TakeLeft a) = ()
    mkMerge _ a () = a

instance
    (Mergeable t1 t2,
     Merger (MergerType t1 t2),
     t1 ~ UnmergedLeft (MergerType t1 t2),
     t2 ~ UnmergedRight (MergerType t1 t2)) =>
    Merger (DiscardRightHead h1 t1 h2 t2) where
    type Merged (DiscardRightHead h1 t1 h2 t2) = h1 :* Merged (MergerType t1 t2)
    type UnmergedLeft (DiscardRightHead h1 t1 h2 t2) = h1 :* t1
    type UnmergedRight (DiscardRightHead h1 t1 h2 t2) = h2 :* t2
    mkMerge _ (h1 :* t1) (h2 :* t2) = h1 :* mkMerge (merger t1 t2) t1 t2

instance
    (Mergeable t1 (h2 :* t2),
     Merger (MergerType t1 (h2 :* t2)),
     t1 ~ UnmergedLeft (MergerType t1 (h2 :* t2)),
     (h2 :* t2) ~ UnmergedRight (MergerType t1 (h2 :* t2))) =>
    Merger (LeftHeadFirst h1 t1 h2 t2) where
    type Merged (LeftHeadFirst h1 t1 h2 t2) = h1 :* Merged (MergerType t1 (h2 :* t2))
    type UnmergedLeft (LeftHeadFirst h1 t1 h2 t2) = h1 :* t1
    type UnmergedRight (LeftHeadFirst h1 t1 h2 t2) = h2 :* t2
    mkMerge _ (h1 :* t1) (h2 :* t2) = h1 :* mkMerge (merger t1 (h2 :* t2)) t1 (h2 :* t2)

instance
    (Mergeable (h1 :* t1) t2,
     Merger (MergerType (h1 :* t1) t2),
     (h1 :* t1) ~ UnmergedLeft (MergerType (h1 :* t1) t2),
     t2 ~ UnmergedRight (MergerType (h1 :* t1) t2)) =>
    Merger (RightHeadFirst h1 t1 h2 t2) where
    type Merged (RightHeadFirst h1 t1 h2 t2) = h2 :* Merged (MergerType (h1 :* t1) t2)
    type UnmergedLeft (RightHeadFirst h1 t1 h2 t2) = h1 :* t1
    type UnmergedRight (RightHeadFirst h1 t1 h2 t2) = h2 :* t2
    mkMerge _ (h1 :* t1) (h2 :* t2) = h2 :* mkMerge (merger (h1 :* t1) t2) (h1 :* t1) t2