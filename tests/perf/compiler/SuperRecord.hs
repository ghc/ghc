{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module SuperRecord where

import Data.Proxy
import Data.Typeable
import Data.Kind
import GHC.Base ( IO(..) )
import GHC.Exts
import GHC.Generics
import GHC.OverloadedLabels
import GHC.TypeLits
import System.IO.Unsafe ( unsafePerformIO )

-- | Field named @l@ labels value of type @t@ adapted from the awesome /labels/ package.
-- Example: @(#name := \"Chris\") :: (\"name\" := String)@
data label := value = KnownSymbol label => FldProxy label := !value
infix 6 :=

instance (Eq value) => Eq (label := value) where
  (_ := x) == (_ := y) = x == y
  {-# INLINE (==) #-}

instance (Ord value) => Ord (label := value) where
  compare (_ := x) (_ := y) = x `compare` y
  {-# INLINE compare #-}

instance (Show t) =>
         Show (l := t) where
  showsPrec p (l := t) =
      showParen (p > 10) (showString ("#" ++ symbolVal l ++ " := " ++ show t))

-- | A proxy witness for a label. Very similar to 'Proxy', but needed to implement
-- a non-orphan 'IsLabel' instance
data FldProxy (t :: Symbol)
    = FldProxy
    deriving (Show, Read, Eq, Ord, Typeable)

instance l ~ l' => IsLabel (l :: Symbol) (FldProxy l') where
    fromLabel = FldProxy


-- | The core record type. Prefer this type when manually writing type
-- signatures
type Record lts = Rec (Sort lts)

-- | Internal record type. When manually writing an explicit type signature for
-- a record, use 'Record' instead. For abstract type signatures 'Rec' will work
-- well.
data Rec (lts :: [Type])
   = Rec { _unRec :: SmallArray# Any } -- Note that the values are physically in reverse order

instance (RecApply lts lts Show) => Show (Rec lts) where
    show = show . showRec

instance RecEq lts lts => Eq (Rec lts) where
    (==) (a :: Rec lts) (b :: Rec lts) = recEq a b (Proxy :: Proxy lts)
    {-# INLINE (==) #-}

-- | An empty record
rnil :: Rec '[]
rnil = unsafeRnil 0
{-# INLINE rnil #-}

-- | An empty record with an initial size for the record
unsafeRnil :: Int -> Rec '[]
unsafeRnil (I# n#) =
    unsafePerformIO $! IO $ \s# ->
    case newSmallArray# n# (error "No Value") s# of
      (# s'#, arr# #) ->
          case unsafeFreezeSmallArray# arr# s'# of
            (# s''#, a# #) -> (# s''# , Rec a# #)
{-# INLINE unsafeRnil #-}

-- | Prepend a record entry to a record 'Rec'
rcons ::
    forall l t lts s.
    ( RecSize lts ~ s
    , KnownNat s
    , KnownNat (RecVecIdxPos l (Sort (l := t ': lts)))
    , KeyDoesNotExist l lts
    , RecCopy lts lts (Sort (l := t ': lts))
    )
    => l := t -> Rec lts -> Rec (Sort (l := t ': lts))
rcons (_ := val) lts =
    unsafePerformIO $! IO $ \s# ->
    case newSmallArray# newSize# (error "No value") s# of
      (# s'#, arr# #) ->
          case recCopyInto (Proxy :: Proxy lts) lts (Proxy :: Proxy (Sort (l := t ': lts))) arr# s'# of
            s''# ->
                case writeSmallArray# arr# setAt# (unsafeCoerce# val) s''# of
                  s'''# ->
                      case unsafeFreezeSmallArray# arr# s'''# of
                        (# s''''#, a# #) -> (# s''''#, Rec a# #)
    where
        !(I# setAt#) =
            fromIntegral (natVal' (proxy# :: Proxy# (RecVecIdxPos l (Sort (l := t ': lts)))))
        newSize# = size# +# 1#
        !(I# size#) = fromIntegral $ natVal' (proxy# :: Proxy# s)
{-# INLINE rcons #-}

class RecCopy (pts :: [Type]) (lts :: [Type]) (rts :: [Type]) where
    recCopyInto ::
        Proxy pts -> Rec lts -> Proxy rts
        -> SmallMutableArray# RealWorld Any
        -> State# RealWorld
        -> State# RealWorld

instance RecCopy '[] lts rts where
    recCopyInto _ _ _ _ s# = s#

instance
    ( Has l rts t
    , Has l lts t
    , RecCopy (RemoveAccessTo l (l := t ': pts)) lts rts
    ) => RecCopy (l := t ': pts) lts rts where
    recCopyInto _ lts prxy tgt# s# =
        let lbl :: FldProxy l
            lbl = FldProxy
            val = get lbl lts
            pNext :: Proxy (RemoveAccessTo l (l := t ': pts))
            pNext = Proxy
            !(I# setAt#) =
                fromIntegral (natVal' (proxy# :: Proxy# (RecVecIdxPos l rts)))
        in case writeSmallArray# tgt# setAt# (unsafeCoerce# val) s# of
             s'# -> recCopyInto pNext lts prxy tgt# s'#

-- | Prepend a record entry to a record 'Rec'. Assumes that the record was created with
-- 'unsafeRnil' and still has enough free slots, mutates the original 'Rec' which should
-- not be reused after
unsafeRCons ::
    forall l t lts s.
    (RecSize lts ~ s, KnownNat s, KeyDoesNotExist l lts)
    => l := t -> Rec lts -> Rec (l := t ': lts)
unsafeRCons (_ := val) (Rec vec#) =
    unsafePerformIO $! IO $ \s# ->
    case unsafeThawSmallArray# vec# s# of
      (# s'#, arr# #) ->
          case writeSmallArray# arr# size# (unsafeCoerce# val) s'# of
            s''# ->
                case unsafeFreezeSmallArray# arr# s''# of
                  (# s'''#, a# #) -> (# s'''#, Rec a# #)
    where
        !(I# size#) = fromIntegral $ natVal' (proxy# :: Proxy# s)
{-# INLINE unsafeRCons #-}

-- | Alias for 'rcons'
(&) ::
    forall l t lts s.
    ( RecSize lts ~ s
    , KnownNat s
    , KnownNat (RecVecIdxPos l (Sort (l := t ': lts)))
    , KeyDoesNotExist l lts
    , RecCopy lts lts (Sort (l := t ': lts))
    )
    => l := t -> Rec lts -> Rec (Sort (l := t ': lts))
(&) = rcons
{-# INLINE (&) #-}

infixr 5 &

type family Sort (lts :: [Type]) where
    Sort '[] = '[]
    Sort (x := t ': xs) = SortInsert (x := t) (Sort xs)

type family SortInsert (x :: Type) (xs :: [Type]) where
    SortInsert x '[] = x ': '[]
    SortInsert (x := t) ((y := u) ': ys) = SortInsert' (CmpSymbol x y) (x := t) (y := u) ys

type family SortInsert' (b :: Ordering) (x :: Type) (y :: Type) (ys :: [Type]) where
    SortInsert' 'LT  x y ys = x ': (y ': ys)
    SortInsert' _    x y ys = y ': SortInsert x ys

type family KeyDoesNotExist (l :: Symbol) (lts :: [Type]) :: Constraint where
    KeyDoesNotExist l '[] = 'True ~ 'True
    KeyDoesNotExist l (l := t ': lts) =
        TypeError
        ( 'Text "Duplicate key " ':<>: 'Text l
        )
    KeyDoesNotExist q (l := t ': lts) = KeyDoesNotExist q lts

type RecAppend lhs rhs = RecAppendH lhs rhs rhs '[]

type family ListConcat (xs :: [Type]) (ys :: [Type]) :: [Type] where
    ListConcat '[] ys = ys
    ListConcat xs '[] = xs
    ListConcat (x ': xs) ys = x ': (ListConcat xs ys)

type family ListReverse (xs :: [Type]) :: [Type] where
    ListReverse (x ': xs) = ListConcat (ListReverse xs) '[x]
    ListReverse '[] = '[]

type family RecAppendH (lhs ::[Type]) (rhs :: [Type]) (rhsall :: [Type]) (accum :: [Type]) :: [Type] where
    RecAppendH (l := t ': lhs) (m := u ': rhs) rhsall acc = RecAppendH (l := t ': lhs) rhs rhsall acc
    RecAppendH (l := t ': lhs) '[] rhsall acc = RecAppendH lhs rhsall rhsall (l := t ': acc)
    RecAppendH '[] rhs rhsall acc = ListConcat (ListReverse acc) rhsall

type family RecSize (lts :: [Type]) :: Nat where
    RecSize '[] = 0
    RecSize (l := t ': lts) = 1 + RecSize lts

type RecVecIdxPos l lts = RecSize lts - RecTyIdxH 0 l lts - 1

type family RecTyIdxH (i :: Nat) (l :: Symbol) (lts :: [Type]) :: Nat where
    RecTyIdxH idx l (l := t ': lts) = idx
    RecTyIdxH idx m (l := t ': lts) = RecTyIdxH (1 + idx) m lts
    RecTyIdxH idx m '[] =
        TypeError
        ( 'Text "Could not find label "
          ':<>: 'Text m
        )

type RecTy :: forall k. Symbol -> [Type] -> k
type family RecTy l lts where
    RecTy l (l := t ': lts) = t
    RecTy q (l := t ': lts) = RecTy q lts

-- | Require a record to contain at least the listed labels
type family HasOf (req :: [Type]) (lts :: [Type]) :: Constraint where
    HasOf (l := t ': req) lts = (Has l lts t, HasOf req lts)
    HasOf '[] lts = 'True ~ 'True

-- | Require a record to contain a label
type Has l lts v =
   ( RecTy l lts ~ v
   , KnownNat (RecSize lts)
   , KnownNat (RecVecIdxPos l lts)
   )

-- | Get an existing record field
get ::
    forall l v lts.
    ( Has l lts v )
    => FldProxy l -> Rec lts -> v
get _ (Rec vec#) =
    let !(I# readAt#) =
            fromIntegral (natVal' (proxy# :: Proxy# (RecVecIdxPos l lts)))
        anyVal :: Any
        anyVal =
           case indexSmallArray# vec# readAt# of
             (# a# #) -> a#
    in unsafeCoerce# anyVal
{-# INLINE get #-}

-- | Alias for 'get'
(&.) :: forall l v lts. (Has l lts v) => Rec lts -> FldProxy l -> v
(&.) = flip get
infixl 3 &.

-- | Update an existing record field
set ::
    forall l v lts.
    (Has l lts v)
    => FldProxy l -> v -> Rec lts -> Rec lts
set _ !val (Rec vec#) =
    let !(I# size#) = fromIntegral $ natVal' (proxy# :: Proxy# (RecSize lts))
        !(I# setAt#) = fromIntegral (natVal' (proxy# :: Proxy# (RecVecIdxPos l lts)))
        dynVal :: Any
        !dynVal = unsafeCoerce# val
        r2 =
            unsafePerformIO $! IO $ \s# ->
            case newSmallArray# size# (error "No value") s# of
              (# s'#, arr# #) ->
                  case copySmallArray# vec# 0# arr# 0# size# s'# of
                    s''# ->
                        case writeSmallArray# arr# setAt# dynVal s''# of
                          s'''# ->
                              case unsafeFreezeSmallArray# arr# s'''# of
                                (# s''''#, a# #) -> (# s''''#, Rec a# #)
    in r2
{-# INLINE set #-}

-- | Update an existing record field
modify ::
    forall l v lts.
    (Has l lts v)
    => FldProxy l -> (v -> v) -> Rec lts -> Rec lts
modify lbl fun r = set lbl (fun $ get lbl r) r
{-# INLINE modify #-}

-- | Constructor for field accessor paths
data lbl :& more = FldProxy lbl :& more
infixr 8 :&

-- | Constructor for field accessor paths
(&:) :: FldProxy q -> more -> q :& more
(&:) = (:&)
{-# INLINE (&:) #-}

infixr 8 &:

-- | Specialized version of (&:) to help writing the last piece of the path w/o
-- confusing the type checker
(&:-) :: FldProxy q -> FldProxy r -> q :& FldProxy r
(&:-) = (:&)
{-# INLINE (&:-) #-}

infixr 8 &:-

-- | Helper function to allow to clearing specify unknown 'IsLabel' cases
fld :: FldProxy l -> FldProxy l
fld = id

type RecDeepTy :: forall r. r -> [Type] -> Type
type family RecDeepTy ps lts where
    RecDeepTy (l :& more) (l := Rec t ': lts) = RecDeepTy more t
    RecDeepTy (l :& more) (l := t ': lts) = t
    RecDeepTy (l :& more) (q := t ': lts) = RecDeepTy (l :& more) lts
    RecDeepTy (FldProxy l) '[l := t] = t
    RecDeepTy l '[l := t] = t

class RecApplyPath p x where
    -- | Perform a deep update, setting the key along the path to the
    -- desired value
    setPath' :: p -> (RecDeepTy p x -> RecDeepTy p x) -> Rec x -> Rec x

    -- | Perform a deep read
    getPath' :: p -> Rec x -> RecDeepTy p x

instance (Has l lts t, t ~ RecDeepTy (FldProxy l) lts) => RecApplyPath (FldProxy l) lts where
    setPath' = modify
    {-# INLINE setPath' #-}

    getPath' = get
    {-# INLINE getPath' #-}

instance
    ( RecDeepTy (l :& more) lts ~ RecDeepTy more rts
    , RecTy l lts ~ Rec rts
    , Has l lts v
    , v ~ Rec rts
    , RecApplyPath more rts
    ) => RecApplyPath (l :& more) lts where
    setPath' (x :& more) v r =
        let innerVal :: Rec rts
            innerVal = get x r
        in set x (setPath' more v innerVal) r
    {-# INLINE setPath' #-}

    getPath' (x :& more) r = getPath' more (get x r)
    {-# INLINE getPath' #-}

-- | Perform a deep update, setting the key along the path to the
-- desired value
setPath :: RecApplyPath k x => k -> RecDeepTy k x -> Rec x -> Rec x
setPath s v = setPath' s (const v)
{-# INLINE setPath #-}

-- | Perform a deep update, transforming the value at the final key
modifyPath :: RecApplyPath k x => k -> (RecDeepTy k x -> RecDeepTy k x) -> Rec x -> Rec x
modifyPath = setPath'
{-# INLINE modifyPath #-}

-- | Perform a deep read. This is somewhat similar to using (&.), but is useful
-- when you want to share a 'RecPath' between 'getPath', 'modifyPath' and/or 'setPath'
getPath :: RecApplyPath k x => k -> Rec x -> RecDeepTy k x
getPath = getPath'
{-# INLINE getPath #-}

-- | Combine two records
combine ::
    forall lhs rhs.
    ( KnownNat (RecSize lhs)
    , KnownNat (RecSize rhs)
    , KnownNat (RecSize lhs + RecSize rhs)
    , RecCopy lhs lhs (Sort (RecAppend lhs rhs))
    , RecCopy rhs rhs (Sort (RecAppend lhs rhs))
    )
    => Rec lhs
    -> Rec rhs
    -> Rec (Sort (RecAppend lhs rhs))
combine lts rts =
    let !(I# size#) =
            fromIntegral $ natVal' (proxy# :: Proxy# (RecSize lhs + RecSize rhs))
    in unsafePerformIO $! IO $ \s# ->
            case newSmallArray# size# (error "No value") s# of
              (# s'#, arr# #) ->
                  case recCopyInto (Proxy :: Proxy lhs) lts (Proxy :: Proxy (Sort (RecAppend lhs rhs))) arr# s'# of
                    s''# ->
                        case recCopyInto (Proxy :: Proxy rhs) rts (Proxy :: Proxy (Sort (RecAppend lhs rhs))) arr# s''# of
                          s'''# ->
                              case unsafeFreezeSmallArray# arr# s'''# of
                                (# s''''#, a# #) -> (# s''''#, Rec a# #)
{-# INLINE combine #-}

-- | Alias for 'combine'
(++:) ::
    forall lhs rhs.
    ( KnownNat (RecSize lhs)
    , KnownNat (RecSize rhs)
    , KnownNat (RecSize lhs + RecSize rhs)
    , RecCopy lhs lhs (Sort (RecAppend lhs rhs))
    , RecCopy rhs rhs (Sort (RecAppend lhs rhs))
    )
    => Rec lhs
    -> Rec rhs
    -> Rec (Sort (RecAppend lhs rhs))
(++:) = combine
{-# INLINE (++:) #-}

data RecFields (flds :: [Symbol]) where
    RFNil :: RecFields '[]
    RFCons :: KnownSymbol f => FldProxy f -> RecFields xs -> RecFields (f ': xs)

recKeys :: forall t (lts :: [Type]). RecKeys lts => t lts -> [String]
recKeys = recKeys' . recFields

recKeys' :: RecFields lts -> [String]
recKeys' x =
    case x of
      RFNil -> []
      RFCons q qs -> symbolVal q : recKeys' qs

-- | Get keys of a record on value and type level
class RecKeys (lts :: [Type]) where
    type RecKeysT lts :: [Symbol]
    recFields :: t lts -> RecFields (RecKeysT lts)

instance RecKeys '[] where
    type RecKeysT '[] = '[]
    recFields _ = RFNil

instance (KnownSymbol l, RecKeys lts) => RecKeys (l := t ': lts) where
    type RecKeysT (l := t ': lts) = (l ': RecKeysT lts)
    recFields (_ :: f (l := t ': lts)) =
        let lbl :: FldProxy l
            lbl = FldProxy
            more :: Proxy lts
            more = Proxy
        in (lbl `RFCons` recFields more)

-- | Apply a function to each key element pair for a record
reflectRec ::
    forall c r lts. (RecApply lts lts c)
    => Proxy c
    -> (forall a. c a => String -> a -> r)
    -> Rec lts
    -> [r]
reflectRec _ f r =
    reverse $
    recApply (\(Dict :: Dict (c a)) s v xs -> (f s v : xs)) r (Proxy :: Proxy lts) []
{-# INLINE reflectRec #-}

-- | Fold over all elements of a record
reflectRecFold ::
    forall c r lts. (RecApply lts lts c)
    => Proxy c
    -> (forall a. c a => String -> a -> r -> r)
    -> Rec lts
    -> r
    -> r
reflectRecFold _ f r =
    recApply (\(Dict :: Dict (c a)) s v x -> f s v x) r (Proxy :: Proxy lts)
{-# INLINE reflectRecFold #-}

-- | Convert all elements of a record to a 'String'
showRec :: forall lts. (RecApply lts lts Show) => Rec lts -> [(String, String)]
showRec = reflectRec @Show Proxy (\k v -> (k, show v))

-- | Machinery needed to implement 'reflectRec'
class RecApply (rts :: [Type]) (lts :: [Type]) c where
    recApply :: (forall a. Dict (c a) -> String -> a -> b -> b) -> Rec rts -> Proxy lts -> b -> b

instance RecApply rts '[] c where
    recApply _ _ _ b = b

instance
    ( KnownSymbol l
    , RecApply rts (RemoveAccessTo l lts) c
    , Has l rts v
    , c v
    ) => RecApply rts (l := t ': lts) c where
    recApply f r (_ :: Proxy (l := t ': lts)) b =
        let lbl :: FldProxy l
            lbl = FldProxy
            val = get lbl r
            res = f Dict (symbolVal lbl) val b
            pNext :: Proxy (RemoveAccessTo l (l := t ': lts))
            pNext = Proxy
        in recApply f r pNext res

-- | Machinery to implement equality
class RecEq (rts :: [Type]) (lts :: [Type]) where
    recEq :: Rec rts -> Rec rts -> Proxy lts -> Bool

instance RecEq rts '[] where
    recEq _ _ _ = True

instance
    ( RecEq rts (RemoveAccessTo l lts)
    , Has l rts v
    , Eq v
    ) => RecEq rts (l := t ': lts) where
    recEq r1 r2 (_ :: Proxy (l := t ': lts)) =
       let lbl :: FldProxy l
           lbl = FldProxy
           val = get lbl r1
           val2 = get lbl r2
           res = val == val2
           pNext :: Proxy (RemoveAccessTo l (l := t ': lts))
           pNext = Proxy
       in res && recEq r1 r2 pNext

type family RemoveAccessTo (l :: Symbol) (lts :: [Type]) :: [Type] where
    RemoveAccessTo l (l := t ': lts) = RemoveAccessTo l lts
    RemoveAccessTo q (l := t ': lts) = (l := t ': RemoveAccessTo l lts)
    RemoveAccessTo q '[] = '[]

-- | Conversion helper to bring a Haskell type to a record. Note that the
-- native Haskell type must be an instance of 'Generic'
class FromNative a lts | a -> lts where
    fromNative' :: a x -> Rec lts

instance FromNative cs lts => FromNative (D1 m cs) lts where
    fromNative' (M1 xs) = fromNative' xs

instance FromNative cs lts => FromNative (C1 m cs) lts where
    fromNative' (M1 xs) = fromNative' xs

instance
    KnownSymbol name
    => FromNative (S1 ('MetaSel ('Just name) p s l) (Rec0 t)) '[name := t]
    where
    fromNative' (M1 (K1 t)) = (FldProxy :: FldProxy name) := t & rnil

instance
    ( FromNative l lhs
    , FromNative r rhs
    , lts ~ Sort (RecAppend lhs rhs)
    , RecCopy lhs lhs lts
    , RecCopy rhs rhs lts
    , KnownNat (RecSize lhs)
    , KnownNat (RecSize rhs)
    , KnownNat (RecSize lhs + RecSize rhs)
    )
    => FromNative (l :*: r) lts where
    fromNative' (l :*: r) = fromNative' l ++: fromNative' r

-- | Convert a native Haskell type to a record
fromNative :: (Generic a, FromNative (Rep a) lts) => a -> Rec lts
fromNative = fromNative' . from
{-# INLINE fromNative #-}

-- | Conversion helper to bring a record back into a Haskell type. Note that the
-- native Haskell type must be an instance of 'Generic'
class ToNative a lts where
    toNative' :: Rec lts -> a x

instance ToNative cs lts => ToNative (D1 m cs) lts where
    toNative' xs = M1 $ toNative' xs

instance ToNative cs lts => ToNative (C1 m cs) lts where
    toNative' xs = M1 $ toNative' xs

instance
    (Has name lts t)
    => ToNative (S1 ('MetaSel ('Just name) p s l) (Rec0 t)) lts
    where
    toNative' r =
        M1 $ K1 (get (FldProxy :: FldProxy name) r)

instance
    ( ToNative l lts
    , ToNative r lts
    )
    => ToNative (l :*: r) lts where
    toNative' r = toNative' r :*: toNative' r

-- | Convert a record to a native Haskell type
toNative :: (Generic a, ToNative (Rep a) lts) => Rec lts -> a
toNative = to . toNative'
{-# INLINE toNative #-}

type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

-- | Convert a field label to a lens
lens ::
    Has l lts v => FldProxy l -> Lens (Rec lts) (Rec lts) v v
lens lbl f r =
    fmap (\v -> set lbl v r) (f (get lbl r))
{-# INLINE lens #-}

data Dict c where
  Dict :: c => Dict c
