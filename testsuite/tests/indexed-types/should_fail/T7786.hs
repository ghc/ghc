{-# LANGUAGE GADTs, ConstraintKinds,
             PolyKinds, KindSignatures, DataKinds, TypeOperators,
             TypeFamilies, UndecidableInstances,
             FlexibleContexts, ScopedTypeVariables #-}
module T7786 where

import GHC.TypeLits(Symbol,Nat)
data family Sing (a :: k)

data Inventory a = Empty | More (Inventory a) a

data instance Sing (l :: Inventory a) where
  Nil :: Sing Empty
  Snoc :: Sing bs -> Sing b -> Sing (More bs b)

data KeySegment = Numic Nat | Symic Symbol

data instance Sing (n :: KeySegment) where
  Numic' :: Sing n -> Sing (Numic n)
  Symic' :: Sing s -> Sing (Symic s)

data instance Sing (k :: [KeySegment]) where
  Root' :: Sing ('[] :: [KeySegment])
  Symic'' :: Sing p -> Sing k -> Sing (Symic k ': p)
  Numic'' :: Sing p -> Sing k -> Sing (Numic k ': p)

type family Under (pre :: [KeySegment]) (post :: [KeySegment]) :: [KeySegment]
type instance Under '[] post = post
type instance Under (x ': xs) post = x ': xs `Under` post

under :: Sing (pre :: [KeySegment]) -> Sing (post :: [KeySegment]) -> Sing (pre `Under` post)
under Root' post = post
under (Symic'' ks k) post = under ks post `Symic''` k
under (Numic'' ks k) post = under ks post `Numic''` k

data Database :: Inventory [KeySegment] -> * where
  Clean :: Database Empty
  Record :: (k `KeyNotIn` i) => Database i -> Sing k -> () -> Database (More i k)
  Sub :: ((sub `UnderDisjoint` k) i) => Database i -> Sing k -> Database sub -> Database ((sub `BuriedUnder` k) i)

dbKeys :: Database inv -> Sing inv
dbKeys Clean = Nil
dbKeys (Record db k _) = dbKeys db `Snoc` k
dbKeys (Sub db k sub) = (dbKeys sub `buryUnder` k) (dbKeys db)

buryUnder :: Sing sub -> Sing post -> Sing acc -> Sing ((sub `BuriedUnder` post) acc)
buryUnder Nil _ acc = acc
buryUnder (ps `Snoc` p) post acc = (ps `buryUnder` post) acc `Snoc` (p `under` post)

type key `KeyNotIn` inv = Intersect (More Empty key) inv ~ Empty
type (lhs `UnderDisjoint` post) rhs = Intersect ((lhs `BuriedUnder` post) Empty) rhs ~ Empty

type family Intersect (l :: Inventory a) (r :: Inventory a) :: Inventory a where
  Intersect Empty r = Empty
  Intersect l Empty = Empty
  Intersect (More ls l) r = InterAppend (Intersect ls r) r l

type family InterAppend (l :: Inventory a) 
                        (r :: Inventory a) 
                        (one :: a) 
            :: Inventory a where
  InterAppend acc Empty one = acc
  InterAppend acc (More rs one) one = More acc one
  InterAppend acc (More rs r) one = InterAppend acc rs one

type family BuriedUnder (sub :: Inventory [KeySegment]) 
                        (post :: [KeySegment]) 
                        (inv :: Inventory [KeySegment]) 
            :: Inventory [KeySegment] where
  BuriedUnder Empty post inv = inv
  BuriedUnder (More ps p) post inv = More ((ps `BuriedUnder` post) inv) (p `Under` post)


intersectPaths :: Sing (lhs :: Inventory [KeySegment]) -> Sing (rhs :: Inventory [KeySegment]) -> Sing (lhs `Intersect` rhs)
intersectPaths = undefined

{- This foo is ambiguous
foo :: Database inv
    -> Sing post
    -> Database sub
    -> Sing (Intersect (BuriedUnder sub post 'Empty) rhs)
foo db k sub = buryUnder (dbKeys sub) k Nil `intersectPaths` dbKeys db
-}

addSub :: Database inv -> Sing k -> Database sub -> Maybe (Database ((sub `BuriedUnder` k) inv))
addSub db k sub = do Nil :: Sing xxx <- return (buryUnder (dbKeys sub) k Nil `intersectPaths` dbKeys db)
                     -- Nil :: Sing ((sub `BuriedUnder` k) Empty `Intersect` inv) <- return (buryUnder (dbKeys sub) k Nil `intersectPaths` dbKeys db)
                     -- Nil :: Sing Empty <- return (buryUnder (dbKeys sub) k Nil `intersectPaths` dbKeys db)
                     -- Nil <- return (buryUnder (dbKeys sub) k Nil `intersectPaths` dbKeys db)
                     return $ Sub db k sub
