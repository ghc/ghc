{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE DeriveGeneric, DefaultSignatures, KindSignatures, FlexibleContexts, TypeOperators, StandaloneDeriving, DeriveAnyClass, BangPatterns, TypeApplications, AllowAmbiguousTypes, DataKinds, TypeFamilies, MagicHash, ScopedTypeVariables, UndecidableInstances, FlexibleInstances, CPP, MultiWayIf, PolyKinds #-}

module Binary (

  Binary(..),

  putAt, getAt,
  putTo, getFrom,

  lazyPut, lazyGet,

  encode, decode,

  module Binary.Internal,

) where

import Binary.Internal

import Data.Char
import Foreign
import GHC.Generics

import BasicTypes
import Data.Array
import Data.Kind (Type)
import Data.Time
import SrcLoc
import Fingerprint
import GHC.Serialized
import Type.Reflection
import Type.Reflection.Unsafe
import GHC.Real (Ratio(..))

import Control.Monad
import Data.List

import GhcPrelude
import FastString
import PlainPanic

import qualified Data.ByteString      as Strict
import qualified Data.ByteString.Lazy as Lazy

import GHC.TypeLits
import GHC.Exts

-- -----------------------------------------------------------------------------
-- Class
-- -----------------------------------------------------------------------------

class Binary a where
  put :: a -> Put ()
  get :: Get a

  default put :: (Generic a, GBinary (Rep a)) => a -> Put ()
  put = gput . from

  default get :: (Generic a, GBinary (Rep a)) => Get a
  get = to <$> gget

-- -----------------------------------------------------------------------------
-- Convenience functions
-- -----------------------------------------------------------------------------

encode :: Binary a => a -> BinData
encode = runPut . put

decode :: Binary a => BinData -> a
decode bd = runGet bd get

-- Put the argument at the specified pointer, leaving the current index
-- at the location after that.
putAt :: Binary a => Bin a -> a -> Put ()
putAt ptr x = seekP ptr >> put x

-- Get data from the specified pointer, leaving the current index at the
-- location after that.
getAt :: Binary a => Bin a -> Get a
getAt ptr = seekG ptr >> get

-- Put the argument at the specified pointer, and return to the current
-- location afterwards.
putTo :: Binary a => Bin a -> a -> Put ()
putTo ptr x = do
  here <- tellP
  seekP ptr
  put x
  seekP here

-- Get data from the specified pointer, and return to the current location
-- afterwards.
getFrom :: Binary a => Bin a -> Get a
getFrom ptr = do
  here <- tellG
  seekG ptr
  x <- get
  seekG here
  return x

-- -----------------------------------------------------------------------------
-- Lazy reading and writing
-- -----------------------------------------------------------------------------

lazyPut :: Binary a => a -> Put ()
lazyPut a = do
  p_a <- tellP
  put p_a
  put a
  q <- tellP
  putAt p_a q
  seekP q

lazyGet :: Binary a => Get a
lazyGet = do
  p   <- get
  p_a <- tellG
  a   <- getSlice p . interleaveG $ getAt p_a
  seekG p
  return a

-- -----------------------------------------------------------------------------
-- Generics
-- -----------------------------------------------------------------------------

class GBinary (f :: * -> *) where
  gput :: f a -> Put ()
  gget :: Get (f a)

instance GBinary U1 where
  gput U1 = return ()
  gget    = return U1

instance GBinary a => GBinary (M1 i c a) where
  gput (M1 x) = gput x
  gget        = M1 <$> gget

instance Binary a => GBinary (K1 i a) where
  gput (K1 x) = put x
  gget        = K1 <$> get

instance (GBinary a, GBinary b) => GBinary (a :*: b) where
  gput (x :*: y) = gput x >> gput y
  gget           = (:*:) <$> gget <*> gget

instance (GSumBinary (a :+: b)) => GBinary (a :+: b) where
  gput = gsput (maxIndex @(a :+: b))
  gget = gsget =<< get

class KnownNat (SumSize f) => GSumBinary (f :: * -> *) where
  type SumSize f :: Nat
  gsput :: Int8 -> f a -> Put ()
  gsget :: Int8 -> Get (f a)

instance (GSumBinary a, GSumBinary b, KnownNat (SumSize (a :+: b)))
       => GSumBinary (a :+: b) where
  type SumSize (a :+: b) = SumSize a + SumSize b
  gsput n (L1 x) = gsput (n - sumSize @b) x
  gsput n (R1 x) = gsput  n               x
  gsget n | n <= maxIndex @a = L1 <$> gsget  n
          | otherwise        = R1 <$> gsget (n - sumSize @a)

instance GBinary (M1 i c a) => GSumBinary (M1 i c a) where
  type SumSize (M1 i c a) = 1
  gsput n x = put n >> gput x
  gsget _   = gget

sumSize :: forall f. GSumBinary f => Int8
sumSize = fromIntegral $ natVal' (proxy# @Nat @(SumSize f))

maxIndex :: forall f. GSumBinary f => Int8
maxIndex = sumSize @f - 1

-- -----------------------------------------------------------------------------
-- Standard instances
-- -----------------------------------------------------------------------------

instance Binary () where
    put () = return ()
    get    = return ()

instance Binary Bool where
    put b = putByte (fromIntegral (fromEnum b))
    get   = do x <- getWord8; return $! (toEnum (fromIntegral x))

instance Binary Char where
  put c = put (fromIntegral (ord c) :: Word32)
  get   = do x <- get; return $! chr (fromIntegral (x :: Word32))

instance Binary Int where
  put = putInt
  get = getInt

instance Binary a => Binary [a] where
  put xs = do
    put (length xs)
    mapM_ put xs
  get = do
    loop =<< (get :: Get Int)
    where
      loop 0 = return []
      loop n = (:) <$> get <*> loop (pred n)

instance (Ix a, Binary a, Binary b) => Binary (Array a b) where
    put arr = do
        put $ bounds arr
        put $ elems arr
    get = do
        bounds <- get
        xs <- get
        return $ listArray bounds xs

instance (Binary a, Binary b) => Binary (a,b) where
    put (a,b) = do put a; put b
    get       = do a <- get
                   b <- get
                   return (a,b)

instance (Binary a, Binary b, Binary c) => Binary (a,b,c) where
    put (a,b,c) = do put a; put b; put c
    get         = do a <- get
                     b <- get
                     c <- get
                     return (a,b,c)

instance (Binary a, Binary b, Binary c, Binary d) => Binary (a,b,c,d) where
    put (a,b,c,d) = do put a; put b; put c; put d
    get           = do a <- get
                       b <- get
                       c <- get
                       d <- get
                       return (a,b,c,d)

instance (Binary a, Binary b, Binary c, Binary d, Binary e) => Binary (a,b,c,d, e) where
    put (a,b,c,d, e) = do put a; put b; put c; put d; put e;
    get              = do a <- get
                          b <- get
                          c <- get
                          d <- get
                          e <- get
                          return (a,b,c,d,e)

instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f) => Binary (a,b,c,d, e, f) where
    put (a,b,c,d, e, f) = do put a; put b; put c; put d; put e; put f;
    get                 = do a <- get
                             b <- get
                             c <- get
                             d <- get
                             e <- get
                             f <- get
                             return (a,b,c,d,e,f)

instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f, Binary g) => Binary (a,b,c,d,e,f,g) where
    put (a,b,c,d,e,f,g) = do put a; put b; put c; put d; put e; put f; put g
    get                 = do a <- get
                             b <- get
                             c <- get
                             d <- get
                             e <- get
                             f <- get
                             g <- get
                             return (a,b,c,d,e,f,g)

instance Binary a => Binary (Maybe a) where
    put Nothing  = putByte 0
    put (Just a) = do putByte 1; put a
    get          = do h <- getWord8
                      case h of
                        0 -> return Nothing
                        _ -> do x <- get; return (Just x)

instance (Binary a, Binary b) => Binary (Either a b) where
    put (Left  a) = do putByte 0; put a
    put (Right b) = do putByte 1; put b
    get           = do h <- getWord8
                       case h of
                         0 -> do a <- get; return (Left a)
                         _ -> do b <- get; return (Right b)

instance Binary UTCTime where
    put u = do put (utctDay u)
               put (utctDayTime u)
    get = do day <- get
             dayTime <- get
             return $ UTCTime { utctDay = day, utctDayTime = dayTime }

instance Binary Day where
    put d = put (toModifiedJulianDay d)
    get = do i <- get
             return $ ModifiedJulianDay { toModifiedJulianDay = i }

instance Binary DiffTime where
    put dt = put (toRational dt)
    get = do r <- get
             return $ fromRational r

instance (Binary a) => Binary (Ratio a) where
    put (a :% b) = do put a; put b
    get = do a <- get; b <- get; return (a :% b)

-- -----------------------------------------------------------------------------
-- Primitives
-- -----------------------------------------------------------------------------

instance Binary Word8 where
  put !w = putWord8 w
  get    = getWord8

instance Binary Word16 where
  put = putULEB128
  get = getULEB128

instance Binary Word32 where
  put = putULEB128
  get = getULEB128

instance Binary Word64 where
  put = putULEB128
  get = getULEB128

instance Binary Int8 where
  put w = put (fromIntegral w :: Word8)
  get   = do w <- get; return $! (fromIntegral (w :: Word8))

instance Binary Int16 where
  put = putSLEB128
  get = getSLEB128

instance Binary Int32 where
  put = putSLEB128
  get = getSLEB128

instance Binary Int64 where
  put = putSLEB128
  get = getSLEB128

instance Binary FastString where
  put = putAFastString
  get = getAFastString

instance Binary (Bin a) where
  get = getBin
  put = putBin

instance Binary Strict.ByteString where
  put = putByteString
  get = getByteString

instance Binary Lazy.ByteString where
  put = put . Lazy.toStrict
  get = Lazy.fromStrict <$> get

-- -----------------------------------------------------------------------------
-- Integer
-- -----------------------------------------------------------------------------

{-
We used to encode values in the Int32 range as such,
falling back to a string of all things. In either case
we stored a tag byte to discriminate between the two cases.

This made some sense as it's highly portable but also not very
efficient.

However GHC stores a surprisingly large number off large Integer
values. In the examples looked at between 25% and 50% of Integers
serialized were outside of the Int32 range.

Consider a valie like `2724268014499746065`, some sort of hash
actually generated by GHC.
In the old scheme this was encoded as a list of 19 chars. This
gave a size of 77 Bytes, one for the length of the list and 76
since we encod chars as Word32 as well.

We can easily do better. The new plan is:

* Start with a tag byte
  * 0 => Int64 (LEB128 encoded)
  * 1 => Negative large interger
  * 2 => Positive large integer
* Followed by the value:
  * Int64 is encoded as usual
  * Large integers are encoded as a list of bytes (Word8).
    We use Data.Bits which defines a bit order independent of the representation.
    Values are stored LSB first.

This means our example value `2724268014499746065` is now only 10 bytes large.
* One byte tag
* One byte for the length of the [Word8] list.
* 8 bytes for the actual date.

The new scheme also does not depend in any way on
architecture specific details.

We still use this scheme even with LEB128 available,
as it has less overhead for truely large numbers. (> maxBound :: Int64)

The instance is used for in Binary Integer and Binary Rational in basicTypes/Literal.hs
-}

instance Binary Integer where
    put i
      | i >= lo64 && i <= hi64 = do
          putWord8 0
          put (fromIntegral i :: Int64)
      | otherwise = do
          if i < 0
            then putWord8 1
            else putWord8 2
          put (unroll $ abs i)
      where
        lo64 = fromIntegral (minBound :: Int64)
        hi64 = fromIntegral (maxBound :: Int64)
    get = do
      int_kind <- getWord8
      case int_kind of
        0 -> fromIntegral <$!> (get :: Get Int64)
        -- Large integer
        1 -> negate <$!> getI
        2 -> getI
        _ -> panic "Binary Integer - Invalid byte"
        where
          getI :: Get Integer
          getI = roll <$!> (get :: Get [Word8])

unroll :: Integer -> [Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

roll :: [Word8] -> Integer
roll   = foldl' unstep 0 . reverse
  where
    unstep a b = a `shiftL` 8 .|. fromIntegral b

-- -----------------------------------------------------------------------------
-- Data.Typeable instances
-- -----------------------------------------------------------------------------

instance Binary TyCon where
    put tc = do
        put (tyConPackage tc)
        put (tyConModule tc)
        put (tyConName tc)
        put (tyConKindArgs tc)
        put (tyConKindRep tc)
    get =
        mkTyCon <$> get <*> get <*> get <*> get <*> get

instance Binary VecCount where
    put = putByte . fromIntegral . fromEnum
    get = toEnum . fromIntegral <$> getByte

instance Binary VecElem where
    put = putByte . fromIntegral . fromEnum
    get = toEnum . fromIntegral <$> getByte

instance Binary RuntimeRep where
    put (VecRep a b)    = putByte 0 >> put a >> put b
    put (TupleRep reps) = putByte 1 >> put reps
    put (SumRep reps)   = putByte 2 >> put reps
    put LiftedRep       = putByte 3
    put UnliftedRep     = putByte 4
    put IntRep          = putByte 5
    put WordRep         = putByte 6
    put Int64Rep        = putByte 7
    put Word64Rep       = putByte 8
    put AddrRep         = putByte 9
    put FloatRep        = putByte 10
    put DoubleRep       = putByte 11
#if __GLASGOW_HASKELL__ >= 807
    put Int8Rep         = putByte 12
    put Word8Rep        = putByte 13
    put Int16Rep        = putByte 14
    put Word16Rep       = putByte 15
#endif
#if __GLASGOW_HASKELL__ >= 809
    put Int32Rep        = putByte 16
    put Word32Rep       = putByte 17
#endif

    get = do
        tag <- getByte
        case tag of
          0  -> VecRep <$> get <*> get
          1  -> TupleRep <$> get
          2  -> SumRep <$> get
          3  -> pure LiftedRep
          4  -> pure UnliftedRep
          5  -> pure IntRep
          6  -> pure WordRep
          7  -> pure Int64Rep
          8  -> pure Word64Rep
          9  -> pure AddrRep
          10 -> pure FloatRep
          11 -> pure DoubleRep
#if __GLASGOW_HASKELL__ >= 807
          12 -> pure Int8Rep
          13 -> pure Word8Rep
          14 -> pure Int16Rep
          15 -> pure Word16Rep
#endif
#if __GLASGOW_HASKELL__ >= 809
          16 -> pure Int32Rep
          17 -> pure Word32Rep
#endif
          _  -> fail "Binary.putRuntimeRep: invalid tag"

instance Binary KindRep where
    put (KindRepTyConApp tc k) = putByte 0 >> put tc >> put k
    put (KindRepVar bndr) = putByte 1 >> put bndr
    put (KindRepApp a b) = putByte 2 >> put a >> put b
    put (KindRepFun a b) = putByte 3 >> put a >> put b
    put (KindRepTYPE r) = putByte 4 >> put r
    put (KindRepTypeLit sort r) = putByte 5 >> put sort >> put r

    get = do
        tag <- getByte
        case tag of
          0 -> KindRepTyConApp <$> get <*> get
          1 -> KindRepVar <$> get
          2 -> KindRepApp <$> get <*> get
          3 -> KindRepFun <$> get <*> get
          4 -> KindRepTYPE <$> get
          5 -> KindRepTypeLit <$> get <*> get
          _ -> fail "Binary.putKindRep: invalid tag"

instance Binary TypeLitSort where
    put TypeLitSymbol = putByte 0
    put TypeLitNat = putByte 1
    get = do
        tag <- getByte
        case tag of
          0 -> pure TypeLitSymbol
          1 -> pure TypeLitNat
          _ -> fail "Binary.putTypeLitSort: invalid tag"

putTypeRep :: TypeRep a -> Put ()
-- Special handling for TYPE, (->), and RuntimeRep due to recursive kind
-- relations.
-- See Note [Mutually recursive representations of primitive types]
putTypeRep rep
  | Just HRefl <- rep `eqTypeRep` (typeRep :: TypeRep Type)
  = put (0 :: Word8)
putTypeRep (Con' con ks) = do
    put (1 :: Word8)
    put con
    put ks
putTypeRep (App f x) = do
    put (2 :: Word8)
    putTypeRep f
    putTypeRep x
putTypeRep (Fun arg res) = do
    put (3 :: Word8)
    putTypeRep arg
    putTypeRep res

getSomeTypeRep :: Get SomeTypeRep
getSomeTypeRep = do
    tag <- get :: Get Word8
    case tag of
        0 -> return $ SomeTypeRep (typeRep :: TypeRep Type)
        1 -> do con <- get :: Get TyCon
                ks  <- get :: Get [SomeTypeRep]
                return $ SomeTypeRep $ mkTrCon con ks

        2 -> do SomeTypeRep f <- getSomeTypeRep
                SomeTypeRep x <- getSomeTypeRep
                case typeRepKind f of
                  Fun arg res ->
                      case arg `eqTypeRep` typeRepKind x of
                        Just HRefl ->
                            case typeRepKind res `eqTypeRep` (typeRep :: TypeRep Type) of
                              Just HRefl -> return $ SomeTypeRep $ mkTrApp f x
                              _ -> failure "Kind mismatch in type application" []
                        _ -> failure "Kind mismatch in type application"
                             [ "    Found argument of kind: " ++ show (typeRepKind x)
                             , "    Where the constructor:  " ++ show f
                             , "    Expects kind:           " ++ show arg
                             ]
                  _ -> failure "Applied non-arrow"
                       [ "    Applied type: " ++ show f
                       , "    To argument:  " ++ show x
                       ]
        3 -> do SomeTypeRep arg <- getSomeTypeRep
                SomeTypeRep res <- getSomeTypeRep
                if
                  | App argkcon _ <- typeRepKind arg
                  , App reskcon _ <- typeRepKind res
                  , Just HRefl <- argkcon `eqTypeRep` tYPErep
                  , Just HRefl <- reskcon `eqTypeRep` tYPErep
                  -> return $ SomeTypeRep $ Fun arg res
                  | otherwise -> failure "Kind mismatch" []
        _ -> failure "Invalid SomeTypeRep" []
  where
    tYPErep :: TypeRep TYPE
    tYPErep = typeRep

    failure description info =
        fail $ unlines $ [ "Binary.getSomeTypeRep: "++description ]
                      ++ map ("    "++) info

instance Typeable a => Binary (TypeRep (a :: k)) where
    put = putTypeRep
    get = do
        SomeTypeRep rep <- getSomeTypeRep
        case rep `eqTypeRep` expected of
            Just HRefl -> pure rep
            Nothing    -> fail $ unlines
                               [ "Binary: Type mismatch"
                               , "    Deserialized type: " ++ show rep
                               , "    Expected type:     " ++ show expected
                               ]
     where expected = typeRep :: TypeRep a

instance Binary SomeTypeRep where
    put (SomeTypeRep rep) = putTypeRep rep
    get = getSomeTypeRep

-- -----------------------------------------------------------------------------
-- Other instances
-- -----------------------------------------------------------------------------

instance Binary LeftOrRight where
   put CLeft  = putByte 0
   put CRight = putByte 1

   get = do { h <- getByte
            ; case h of
                0 -> return CLeft
                _ -> return CRight }

instance Binary PromotionFlag where
   put NotPromoted = putByte 0
   put IsPromoted  = putByte 1

   get = do
       n <- getByte
       case n of
         0 -> return NotPromoted
         1 -> return IsPromoted
         _ -> fail "Binary(IsPromoted): fail)"

instance Binary Fingerprint where
  put (Fingerprint w1 w2) = do put w1; put w2
  get = do w1 <- get ; w2 <- get; return (Fingerprint w1 w2)

instance Binary FunctionOrData where
    put IsFunction = putByte 0
    put IsData     = putByte 1
    get = do
        h <- getByte
        case h of
          0 -> return IsFunction
          1 -> return IsData
          _ -> panic "Binary FunctionOrData"

instance Binary TupleSort where
    put BoxedTuple      = putByte 0
    put UnboxedTuple    = putByte 1
    put ConstraintTuple = putByte 2
    get = do
      h <- getByte
      case h of
        0 -> do return BoxedTuple
        1 -> do return UnboxedTuple
        _ -> do return ConstraintTuple

instance Binary Activation where
    put NeverActive = do
            putByte 0
    put AlwaysActive = do
            putByte 1
    put (ActiveBefore src aa) = do
            putByte 2
            put src
            put aa
    put (ActiveAfter src ab) = do
            putByte 3
            put src
            put ab
    get = do
            h <- getByte
            case h of
              0 -> do return NeverActive
              1 -> do return AlwaysActive
              2 -> do src <- get
                      aa  <- get
                      return (ActiveBefore src aa)
              _ -> do src <- get
                      ab  <- get
                      return (ActiveAfter src ab)

instance Binary InlinePragma where
    put (InlinePragma s a b c d) = do
            put s
            put a
            put b
            put c
            put d

    get = do
           s <- get
           a <- get
           b <- get
           c <- get
           d <- get
           return (InlinePragma s a b c d)

instance Binary RuleMatchInfo where
    put FunLike = putByte 0
    put ConLike = putByte 1
    get = do
            h <- getByte
            if h == 1 then return ConLike
                      else return FunLike

instance Binary InlineSpec where
    put NoUserInline    = putByte 0
    put Inline          = putByte 1
    put Inlinable       = putByte 2
    put NoInline        = putByte 3

    get = do h <- getByte
             case h of
               0 -> return NoUserInline
               1 -> return Inline
               2 -> return Inlinable
               _ -> return NoInline

instance Binary RecFlag where
    put Recursive = do
            putByte 0
    put NonRecursive = do
            putByte 1
    get = do
            h <- getByte
            case h of
              0 -> do return Recursive
              _ -> do return NonRecursive

instance Binary OverlapMode where
    put (NoOverlap    s) = putByte 0 >> put s
    put (Overlaps     s) = putByte 1 >> put s
    put (Incoherent   s) = putByte 2 >> put s
    put (Overlapping  s) = putByte 3 >> put s
    put (Overlappable s) = putByte 4 >> put s
    get = do
        h <- getByte
        case h of
            0 -> NoOverlap    <$> get
            1 -> Overlaps     <$> get
            2 -> Incoherent   <$> get
            3 -> Overlapping  <$> get
            4 -> Overlappable <$> get
            _ -> panic ("get OverlapMode" ++ show h)


instance Binary OverlapFlag where
    put flag = do put (overlapMode flag)
                  put (isSafeOverlap flag)
    get = do
        h <- get
        b <- get
        return OverlapFlag { overlapMode = h, isSafeOverlap = b }

instance Binary FixityDirection where
    put InfixL = do
        putByte 0
    put InfixR = do
        putByte 1
    put InfixN = do
        putByte 2
    get = do
        h <- getByte
        case h of
          0 -> do return InfixL
          1 -> do return InfixR
          _ -> do return InfixN

instance Binary BasicTypes.Fixity where
    put (Fixity src aa ab) = do
            put src
            put aa
            put ab
    get = do
          src <- get
          aa  <- get
          ab  <- get
          return (Fixity src aa ab)

instance Binary WarningTxt where
    put (WarningTxt s w) = do
            putByte 0
            put s
            put w
    put (DeprecatedTxt s d) = do
            putByte 1
            put s
            put d

    get = do
            h <- getByte
            case h of
              0 -> do s <- get
                      w <- get
                      return (WarningTxt s w)
              _ -> do s <- get
                      d <- get
                      return (DeprecatedTxt s d)

instance Binary StringLiteral where
  put (StringLiteral st fs) = do
            put st
            put fs
  get = do
            st <- get
            fs <- get
            return (StringLiteral st fs)

instance Binary a => Binary (Located a) where
    put (L l x) = do
            put l
            put x

    get = do
            l <- get
            x <- get
            return (L l x)

instance Binary RealSrcSpan where
  put ss = do
           put (srcSpanFile ss)
           put (srcSpanStartLine ss)
           put (srcSpanStartCol ss)
           put (srcSpanEndLine ss)
           put (srcSpanEndCol ss)

  get = do f  <- get
           sl <- get
           sc <- get
           el <- get
           ec <- get
           return (mkRealSrcSpan (mkRealSrcLoc f sl sc)
                                 (mkRealSrcLoc f el ec))

instance Binary SrcSpan where
  put (RealSrcSpan ss) = do
          putByte 0
          put ss

  put (UnhelpfulSpan s) = do
          putByte 1
          put s

  get = do
          h <- getByte
          case h of
            0 -> do ss <- get
                    return (RealSrcSpan ss)
            _ -> do s <- get
                    return (UnhelpfulSpan s)

instance Binary Serialized where
    put (Serialized the_type bytes) = do
        put the_type
        put bytes
    get = do
        the_type <- get
        bytes <- get
        return (Serialized the_type bytes)

instance Binary SourceText where
  put NoSourceText = putByte 0
  put (SourceText s) = do
        putByte 1
        put s

  get = do
    h <- getByte
    case h of
      0 -> return NoSourceText
      1 -> do
        s <- get
        return (SourceText s)
      _ -> panic $ "Binary SourceText:" ++ show h
