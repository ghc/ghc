{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE EmptyCase #-}
#endif

{-|
Module:      Data.Functor.Classes.Generic
Copyright:   (C) 2015-2016 Edward Kmett, Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Internal functionality for "Data.Functor.Classes.Generic".

This is an internal module and, as such, the API is not guaranteed to remain the
same between any given release.
-}
module Data.Functor.Classes.Generic.Internal
  ( -- * Options
    Options(..)
  , defaultOptions
  , latestGHCOptions
    -- * 'Eq1'
#if defined(TRANSFORMERS_FOUR)
  , eq1Default
  , eq1Options
#else
  , liftEqDefault
  , liftEqOptions
#endif
  , GEq1(..)
  , Eq1Args(..)
    -- * 'Ord1'
#if defined(TRANSFORMERS_FOUR)
  , compare1Default
  , compare1Options
#else
  , liftCompareDefault
  , liftCompareOptions
#endif
  , GOrd1(..)
  , Ord1Args(..)
    -- * 'Read1'
#if defined(TRANSFORMERS_FOUR)
  , readsPrec1Default
  , readsPrec1Options
#else
  , liftReadsPrecDefault
  , liftReadsPrecOptions
#endif
  , GRead1(..)
  , GRead1Con(..)
  , Read1Args(..)
    -- * 'Show1'
#if defined(TRANSFORMERS_FOUR)
  , showsPrec1Default
  , showsPrec1Options
#else
  , liftShowsPrecDefault
  , liftShowsPrecOptions
#endif
  , GShow1(..)
  , GShow1Con(..)
  , Show1Args(..)
    -- * 'FunctorClassesDefault'
  , FunctorClassesDefault(..)
  -- * Miscellaneous types
  , V4
  , NonV4
  , ConType(..)
  , IsNullaryDataType(..)
  , IsNullaryCon(..)
  ) where

import Data.Char (isSymbol, ord)
import Data.Functor.Classes
#ifdef GENERIC_DERIVING
import Generics.Deriving.Base hiding (prec)
#else
import GHC.Generics hiding (prec)
#endif
import GHC.Read (paren, parens)
import GHC.Show (appPrec, appPrec1, showSpace)
import Text.ParserCombinators.ReadPrec
import Text.Read (Read(..))
import Text.Read.Lex (Lexeme(..))

#if !defined(TRANSFORMERS_FOUR)
import GHC.Read (list)
import Text.Show (showListWith)
#endif

#if MIN_VERSION_base(4,7,0)
import GHC.Read (expectP)
#else
import GHC.Read (lexP)
import Unsafe.Coerce (unsafeCoerce)
#endif

#if MIN_VERSION_base(4,7,0) || defined(GENERIC_DERIVING)
import GHC.Exts
#endif

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid
#endif

-------------------------------------------------------------------------------
-- * Options
-------------------------------------------------------------------------------

-- | Options that further configure how the functions in
-- "Data.Functor.Classes.Generic" should behave.
newtype Options = Options
  { ghc8ShowBehavior :: Bool
    -- ^ If 'True', a default 'Show1' implementation will show hash signs
    -- (@#@) when showing unlifted types.
  }

-- | Options that match the behavior of the installed version of GHC.
defaultOptions :: Options
defaultOptions = Options
  {
#if __GLASGOW_HASKELL__ >= 800
  ghc8ShowBehavior = True
#else
  ghc8ShowBehavior = False
#endif
  }

-- | Options that match the behavior of the most recent GHC release.
latestGHCOptions :: Options
latestGHCOptions = Options { ghc8ShowBehavior = True }

-- | A type-level indicator that the @transformers-0.4@ version of a class method
-- is being derived generically.
data V4

-- | A type-level indicator that the non-@transformers-0.4@ version of a class
-- method is being derived generically.
data NonV4

-------------------------------------------------------------------------------
-- * Eq1
-------------------------------------------------------------------------------

-- | An 'Eq1Args' value either stores an @Eq a@ dictionary (for the
-- @transformers-0.4@ version of 'Eq1'), or it stores the function argument that
-- checks the equality of occurrences of the type parameter (for the
-- non-@transformers-0.4@ version of 'Eq1').
data Eq1Args v a b where
    V4Eq1Args    :: Eq a             => Eq1Args V4    a a
    NonV4Eq1Args :: (a -> b -> Bool) -> Eq1Args NonV4 a b

#if defined(TRANSFORMERS_FOUR)
-- | A sensible default 'eq1' implementation for 'Generic1' instances.
eq1Default :: (GEq1 V4 (Rep1 f), Generic1 f, Eq a)
           => f a -> f a -> Bool
eq1Default = eq1Options defaultOptions

-- | Like 'eq1Default', but with configurable 'Options'. Currently,
-- the 'Options' have no effect (but this may change in the future).
eq1Options :: (GEq1 V4 (Rep1 f), Generic1 f, Eq a)
           => Options -> f a -> f a -> Bool
eq1Options _ m n = gliftEq V4Eq1Args (from1 m) (from1 n)
#else
-- | A sensible default 'liftEq' implementation for 'Generic1' instances.
liftEqDefault :: (GEq1 NonV4 (Rep1 f), Generic1 f)
              => (a -> b -> Bool) -> f a -> f b -> Bool
liftEqDefault = liftEqOptions defaultOptions

-- | Like 'liftEqDefault', but with configurable 'Options'. Currently,
-- the 'Options' have no effect (but this may change in the future).
liftEqOptions :: (GEq1 NonV4 (Rep1 f), Generic1 f)
              => Options -> (a -> b -> Bool) -> f a -> f b -> Bool
liftEqOptions _ f m n = gliftEq (NonV4Eq1Args f) (from1 m) (from1 n)
#endif

-- | Class of generic representation types that can be checked for equality.
class GEq1 v t where
  gliftEq :: Eq1Args v a b -> t a -> t b -> Bool

instance Eq c => GEq1 v (K1 i c) where
  gliftEq _ (K1 c) (K1 d) = c == d

instance (GEq1 v f, GEq1 v g) => GEq1 v (f :*: g) where
  gliftEq f (a :*: b) (c :*: d) = gliftEq f a c && gliftEq f b d

instance (GEq1 v f, GEq1 v g) => GEq1 v (f :+: g) where
  gliftEq f (L1 a) (L1 c) = gliftEq f a c
  gliftEq f (R1 b) (R1 d) = gliftEq f b d
  gliftEq _ _      _      = False

instance GEq1 v f => GEq1 v (M1 i c f) where
  gliftEq f (M1 a) (M1 b) = gliftEq f a b

instance GEq1 v U1 where
  gliftEq _ U1 U1 = True

instance GEq1 v V1 where
  gliftEq _ _ _ = True

#if defined(TRANSFORMERS_FOUR)
instance GEq1 V4 Par1 where
  gliftEq V4Eq1Args (Par1 a) (Par1 b) = a == b

instance Eq1 f => GEq1 V4 (Rec1 f) where
  gliftEq V4Eq1Args (Rec1 a) (Rec1 b) = eq1 a b

instance (Functor f, Eq1 f, GEq1 V4 g) => GEq1 V4 (f :.: g) where
  gliftEq V4Eq1Args (Comp1 m) (Comp1 n) = eq1 (fmap Apply m) (fmap Apply n)
#else
instance GEq1 NonV4 Par1 where
  gliftEq (NonV4Eq1Args f) (Par1 a) (Par1 b) = f a b

instance Eq1 f => GEq1 NonV4 (Rec1 f) where
  gliftEq (NonV4Eq1Args f) (Rec1 a) (Rec1 b) = liftEq f a b

instance (Eq1 f, GEq1 NonV4 g) => GEq1 NonV4 (f :.: g) where
  gliftEq (NonV4Eq1Args f) (Comp1 m) (Comp1 n) =
    liftEq (gliftEq (NonV4Eq1Args f)) m n
#endif

#if MIN_VERSION_base(4,9,0) || defined(GENERIC_DERIVING)
-- Unboxed types
instance GEq1 v UAddr where
  gliftEq _ (UAddr a1) (UAddr a2) = isTrue# (eqAddr# a1 a2)

instance GEq1 v UChar where
  gliftEq _ (UChar c1) (UChar c2) = isTrue# (eqChar# c1 c2)

instance GEq1 v UDouble where
  gliftEq _ (UDouble d1) (UDouble d2) = isTrue# (d1 ==## d2)

instance GEq1 v UFloat where
  gliftEq _ (UFloat f1) (UFloat f2) = isTrue# (eqFloat# f1 f2)

instance GEq1 v UInt where
  gliftEq _ (UInt i1) (UInt i2) = isTrue# (i1 ==# i2)

instance GEq1 v UWord where
  gliftEq _ (UWord w1) (UWord w2) = isTrue# (eqWord# w1 w2)
#endif

-------------------------------------------------------------------------------
-- * Ord1
-------------------------------------------------------------------------------

-- | An 'Ord1Args' value either stores an @Ord a@ dictionary (for the
-- @transformers-0.4@ version of 'Ord1'), or it stores the function argument that
-- compares occurrences of the type parameter (for the non-@transformers-0.4@
-- version of 'Ord1').
data Ord1Args v a b where
    V4Ord1Args    :: Ord a                => Ord1Args V4    a a
    NonV4Ord1Args :: (a -> b -> Ordering) -> Ord1Args NonV4 a b

#if defined(TRANSFORMERS_FOUR)
-- | A sensible default 'compare1' implementation for 'Generic1' instances.
compare1Default :: (GOrd1 V4 (Rep1 f), Generic1 f, Ord a)
                => f a -> f a -> Ordering
compare1Default = compare1Options defaultOptions

-- | Like 'compare1Default', but with configurable 'Options'. Currently,
-- the 'Options' have no effect (but this may change in the future).
compare1Options :: (GOrd1 V4 (Rep1 f), Generic1 f, Ord a)
                => Options -> f a -> f a -> Ordering
compare1Options _ m n = gliftCompare V4Ord1Args (from1 m) (from1 n)
#else
-- | A sensible default 'liftCompare' implementation for 'Generic1' instances.
liftCompareDefault :: (GOrd1 NonV4 (Rep1 f), Generic1 f)
                   => (a -> b -> Ordering) -> f a -> f b -> Ordering
liftCompareDefault = liftCompareOptions defaultOptions

-- | Like 'liftCompareDefault', but with configurable 'Options'. Currently,
-- the 'Options' have no effect (but this may change in the future).
liftCompareOptions :: (GOrd1 NonV4 (Rep1 f), Generic1 f)
                   => Options -> (a -> b -> Ordering) -> f a -> f b -> Ordering
liftCompareOptions _ f m n = gliftCompare (NonV4Ord1Args f) (from1 m) (from1 n)
#endif

-- | Class of generic representation types that can be totally ordered.
class GEq1 v t => GOrd1 v t where
  gliftCompare :: Ord1Args v a b -> t a -> t b -> Ordering

instance Ord c => GOrd1 v (K1 i c) where
  gliftCompare _ (K1 c) (K1 d) = compare c d

instance (GOrd1 v f, GOrd1 v g) => GOrd1 v (f :*: g) where
  gliftCompare f (a :*: b) (c :*: d) =
    gliftCompare f a c `mappend` gliftCompare f b d

instance (GOrd1 v f, GOrd1 v g) => GOrd1 v (f :+: g) where
  gliftCompare f (L1 a) (L1 c) = gliftCompare f a c
  gliftCompare _ L1{}   R1{}   = LT
  gliftCompare _ R1{}   L1{}   = GT
  gliftCompare f (R1 b) (R1 d) = gliftCompare f b d

instance GOrd1 v f => GOrd1 v (M1 i c f) where
  gliftCompare f (M1 a) (M1 b) = gliftCompare f a b

instance GOrd1 v U1 where
  gliftCompare _ U1 U1 = EQ

instance GOrd1 v V1 where
  gliftCompare _ _ _ = EQ

#if defined(TRANSFORMERS_FOUR)
instance GOrd1 V4 Par1 where
  gliftCompare V4Ord1Args (Par1 a) (Par1 b) = compare a b

instance Ord1 f => GOrd1 V4 (Rec1 f) where
  gliftCompare V4Ord1Args (Rec1 a) (Rec1 b) = compare1 a b

instance (Functor f, Ord1 f, GOrd1 V4 g) => GOrd1 V4 (f :.: g) where
  gliftCompare V4Ord1Args (Comp1 m) (Comp1 n) =
    compare1 (fmap Apply m) (fmap Apply n)
#else
instance GOrd1 NonV4 Par1 where
  gliftCompare (NonV4Ord1Args f) (Par1 a) (Par1 b) = f a b

instance Ord1 f => GOrd1 NonV4 (Rec1 f) where
  gliftCompare (NonV4Ord1Args f) (Rec1 a) (Rec1 b) = liftCompare f a b

instance (Ord1 f, GOrd1 NonV4 g) => GOrd1 NonV4 (f :.: g) where
  gliftCompare (NonV4Ord1Args f) (Comp1 m) (Comp1 n) =
    liftCompare (gliftCompare (NonV4Ord1Args f)) m n
#endif

#if MIN_VERSION_base(4,9,0) || defined(GENERIC_DERIVING)
-- Unboxed types
instance GOrd1 v UAddr where
  gliftCompare _ (UAddr a1) (UAddr a2) = primCompare (eqAddr# a1 a2) (leAddr# a1 a2)

instance GOrd1 v UChar where
  gliftCompare _ (UChar c1) (UChar c2) = primCompare (eqChar# c1 c2) (leChar# c1 c2)

instance GOrd1 v UDouble where
  gliftCompare _ (UDouble d1) (UDouble d2) = primCompare (d1 ==## d2) (d1 <=## d2)

instance GOrd1 v UFloat where
  gliftCompare _ (UFloat f1) (UFloat f2) = primCompare (eqFloat# f1 f2) (leFloat# f1 f2)

instance GOrd1 v UInt where
  gliftCompare _ (UInt i1) (UInt i2) = primCompare (i1 ==# i2) (i1 <=# i2)

instance GOrd1 v UWord where
  gliftCompare _ (UWord w1) (UWord w2) = primCompare (eqWord# w1 w2) (leWord# w1 w2)

# if __GLASGOW_HASKELL__ >= 708
primCompare :: Int# -> Int# -> Ordering
# else
primCompare :: Bool -> Bool -> Ordering
# endif
primCompare eq le = if isTrue# eq then EQ
                    else if isTrue# le then LT
                    else GT
#endif

-------------------------------------------------------------------------------
-- * Read1
-------------------------------------------------------------------------------

-- | A 'Read1Args' value either stores a @Read a@ dictionary (for the
-- @transformers-0.4@ version of 'Read1'), or it stores the two function arguments
-- that parse occurrences of the type parameter (for the non-@transformers-0.4@
-- version of 'Read1').
data Read1Args v a where
    V4Read1Args    :: Read a                     => Read1Args V4    a
    NonV4Read1Args :: ReadPrec a -> ReadPrec [a] -> Read1Args NonV4 a

#if defined(TRANSFORMERS_FOUR)
-- | A sensible default 'readsPrec1' implementation for 'Generic1' instances.
readsPrec1Default :: (GRead1 V4 (Rep1 f), Generic1 f, Read a)
                  => Int -> ReadS (f a)
readsPrec1Default = readsPrec1Options defaultOptions

-- | Like 'readsPrec1Default', but with configurable 'Options'. Currently,
-- the 'Options' have no effect (but this may change in the future).
readsPrec1Options :: (GRead1 V4 (Rep1 f), Generic1 f, Read a)
                  => Options -> Int -> ReadS (f a)
readsPrec1Options _ p =
  readPrec_to_S (fmap to1 $ gliftReadPrec V4Read1Args) p
#else
-- | A sensible default 'liftReadsPrec' implementation for 'Generic1' instances.
liftReadsPrecDefault :: (GRead1 NonV4 (Rep1 f), Generic1 f)
                     => (Int -> ReadS a) -> ReadS [a] -> Int -> ReadS (f a)
liftReadsPrecDefault = liftReadsPrecOptions defaultOptions

-- | Like 'liftReadsPrecDefault', but with configurable 'Options'. Currently,
-- the 'Options' have no effect (but this may change in the future).
liftReadsPrecOptions :: (GRead1 NonV4 (Rep1 f), Generic1 f)
                     => Options -> (Int -> ReadS a) -> ReadS [a] -> Int -> ReadS (f a)
liftReadsPrecOptions _ rp rl p =
  readPrec_to_S (fmap to1 $ gliftReadPrec
                      (NonV4Read1Args (readS_to_Prec rp)
                                      (readS_to_Prec (const rl)))) p
#endif

#if !(MIN_VERSION_base(4,7,0))
coerce :: a -> b
coerce = unsafeCoerce

expectP :: Lexeme -> ReadPrec ()
expectP lexeme = do
  thing <- lexP
  if thing == lexeme then return () else pfail
#endif

coerceM1 :: ReadPrec (f p) -> ReadPrec (M1 i c f p)
coerceM1 = coerce

coercePar1 :: ReadPrec p -> ReadPrec (Par1 p)
coercePar1 = coerce

coerceRec1 :: ReadPrec (f a) -> ReadPrec (Rec1 f a)
coerceRec1 = coerce

coerceComp1 :: ReadPrec (f (g a)) -> ReadPrec ((f :.: g) a)
coerceComp1 = coerce

isSymVar :: String -> Bool
isSymVar ""    = False
isSymVar (c:_) = startsVarSym c

startsVarSym :: Char -> Bool
startsVarSym c = startsVarSymASCII c || (ord c > 0x7f && isSymbol c) -- Infix Ids

startsVarSymASCII :: Char -> Bool
startsVarSymASCII c = c `elem` "!#$%&*+./<=>?@\\^|~-"

snocView :: [a] -> Maybe ([a],a)
        -- Split off the last element
snocView [] = Nothing
snocView xs = go [] xs
  where
      -- Invariant: second arg is non-empty
    go acc [a]    = Just (reverse acc, a)
    go acc (a:as) = go (a:acc) as
    go _ [] = error "Util: snocView"

identHLexemes :: String -> [Lexeme]
identHLexemes s | Just (ss, '#') <- snocView s = [Ident ss, Symbol "#"]
                | otherwise                    = [Ident s]

-- | Class of generic representation types that can be parsed from a 'String'.
class GRead1 v f where
  gliftReadPrec :: Read1Args v a -> ReadPrec (f a)

instance (GRead1 v f, IsNullaryDataType f) => GRead1 v (D1 d f) where
  gliftReadPrec = coerceM1 . parensIfNonNullary . gliftReadPrec
    where
      x :: f p
      x = undefined

      parensIfNonNullary :: ReadPrec a -> ReadPrec a
      parensIfNonNullary = if isNullaryDataType x
                              then id
                              else parens

instance GRead1 v V1 where
  gliftReadPrec _ = pfail

instance (GRead1 v f, GRead1 v g) => GRead1 v (f :+: g) where
  gliftReadPrec ras =
    fmap L1 (gliftReadPrec ras) +++ fmap R1 (gliftReadPrec ras)

instance (Constructor c, GRead1Con v f, IsNullaryCon f) => GRead1 v (C1 c f) where
  gliftReadPrec ras = coerceM1 $ case fixity of
      Prefix -> precIfNonNullary $ do
                  if conIsTuple c
                     then return ()
                     else let cn = conName c
                          in if isInfixDataCon cn
                                then readSurround '(' (expectP (Symbol cn)) ')'
                                else mapM_ expectP $ identHLexemes cn
                  readBraces t (gliftReadPrecCon t ras)
      Infix _ m -> prec m $ gliftReadPrecCon t ras
    where
      c :: C1 c f p
      c = undefined

      x :: f p
      x = undefined

      fixity :: Fixity
      fixity = conFixity c

      precIfNonNullary :: ReadPrec a -> ReadPrec a
      precIfNonNullary = if isNullaryCon x
                            then id
                            else prec (if conIsRecord c
                                          then appPrec1
                                          else appPrec)

      t :: ConType
      t = if conIsRecord c
          then Rec
          else case conIsTuple c of
              True  -> Tup
              False -> case fixity of
                  Prefix    -> Pref
                  Infix _ _ -> Inf $ conName c

readBraces :: ConType -> ReadPrec a -> ReadPrec a
readBraces Rec     r = readSurround '{' r '}'
readBraces Tup     r = paren r
readBraces Pref    r = r
readBraces (Inf _) r = r

readSurround :: Char -> ReadPrec a -> Char -> ReadPrec a
readSurround c1 r c2 = do
  expectP (Punc [c1])
  r' <- r
  expectP (Punc [c2])
  return r'

-- | Class of generic representation types that can be parsed from a 'String', and
-- for which the 'ConType' has been determined.
class GRead1Con v f where
  gliftReadPrecCon :: ConType -> Read1Args v a -> ReadPrec (f a)

instance GRead1Con v U1 where
  gliftReadPrecCon _ _ = return U1

instance Read c => GRead1Con v (K1 i c) where
  gliftReadPrecCon _ _ = coerceK1 readPrec
    where
      coerceK1 :: ReadPrec c -> ReadPrec (K1 i c p)
      coerceK1 = coerce

instance (Selector s, GRead1Con v f) => GRead1Con v (S1 s f) where
  gliftReadPrecCon t ras
    | selectorName == "" = coerceM1 $ step $ gliftReadPrecCon t ras
    | otherwise          = coerceM1 $ do
                              mapM_ expectP $ readLblLexemes selectorName
                              expectP (Punc "=")
                              reset $ gliftReadPrecCon t ras
    where
      selectorName :: String
      selectorName = selName (undefined :: S1 s f p)

      readLblLexemes :: String -> [Lexeme]
      readLblLexemes lbl | isSymVar lbl
                         = [Punc "(", Symbol lbl, Punc ")"]
                         | otherwise
                         = identHLexemes lbl

instance (GRead1Con v f, GRead1Con v g) => GRead1Con v (f :*: g) where
  gliftReadPrecCon t ras = do
      l <- gliftReadPrecCon t ras
      case t of
           Rec   -> expectP (Punc ",")
           Inf o -> infixPrec o
           Tup   -> expectP (Punc ",")
           Pref  -> return ()
      r <- gliftReadPrecCon t ras
      return (l :*: r)
    where
      infixPrec :: String -> ReadPrec ()
      infixPrec o = if isInfixDataCon o
                       then expectP (Symbol o)
                       else mapM_ expectP $
                                [Punc "`"] ++ identHLexemes o ++ [Punc "`"]

#if defined(TRANSFORMERS_FOUR)
instance GRead1Con V4 Par1 where
  gliftReadPrecCon _ V4Read1Args = coercePar1 readPrec

instance Read1 f => GRead1Con V4 (Rec1 f) where
  gliftReadPrecCon _ V4Read1Args = coerceRec1 $ readS_to_Prec readsPrec1

instance (Functor f, Read1 f, GRead1Con V4 g) => GRead1Con V4 (f :.: g) where
  gliftReadPrecCon _ (V4Read1Args :: Read1Args V4 a) =
      coerceComp1 $ fmap (fmap getApply) $ readS_to_Prec crp1
    where
      crp1 :: Int -> ReadS (f (Apply g a))
      crp1 = readsPrec1
#else
instance GRead1Con NonV4 Par1 where
  gliftReadPrecCon _ (NonV4Read1Args rp _) = coercePar1 rp

instance Read1 f => GRead1Con NonV4 (Rec1 f) where
  gliftReadPrecCon _ (NonV4Read1Args rp rl) = coerceRec1 $ readS_to_Prec $
      liftReadsPrec (readPrec_to_S rp) (readPrec_to_S rl 0)

instance (Read1 f, GRead1Con NonV4 g) => GRead1Con NonV4 (f :.: g) where
  gliftReadPrecCon t (NonV4Read1Args rp rl) = coerceComp1 $ readS_to_Prec $
      liftReadsPrec (readPrec_to_S       grpc)
                    (readPrec_to_S (list grpc) 0)
    where
      grpc = gliftReadPrecCon t (NonV4Read1Args rp rl)
#endif

-------------------------------------------------------------------------------
-- * Show1
-------------------------------------------------------------------------------

-- | A 'Show1Args' value either stores a @Show a@ dictionary (for the
-- @transformers-0.4@ version of 'Show1'), or it stores the two function arguments
-- that show occurrences of the type parameter (for the non-@transformers-0.4@
-- version of 'Show1').
data Show1Args v a where
    V4Show1Args    :: Show a                                => Show1Args V4    a
    NonV4Show1Args :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Show1Args NonV4 a

#if defined(TRANSFORMERS_FOUR)
-- | A sensible default 'showsPrec1' implementation for 'Generic1' instances.
showsPrec1Default :: (GShow1 V4 (Rep1 f), Generic1 f, Show a)
                  => Int -> f a -> ShowS
showsPrec1Default = showsPrec1Options defaultOptions

-- | Like 'showsPrec1Default', but with configurable 'Options'.
showsPrec1Options :: (GShow1 V4 (Rep1 f), Generic1 f, Show a)
                  => Options -> Int -> f a -> ShowS
showsPrec1Options opts p = gliftShowsPrec opts V4Show1Args p . from1
#else
-- | A sensible default 'liftShowsPrec' implementation for 'Generic1' instances.
liftShowsPrecDefault :: (GShow1 NonV4 (Rep1 f), Generic1 f)
                     => (Int -> a -> ShowS) -> ([a] -> ShowS)
                     -> Int -> f a -> ShowS
liftShowsPrecDefault = liftShowsPrecOptions defaultOptions

-- | Like 'liftShowsPrecDefault', but with configurable 'Options'.
liftShowsPrecOptions :: (GShow1 NonV4 (Rep1 f), Generic1 f)
                     => Options -> (Int -> a -> ShowS) -> ([a] -> ShowS)
                     -> Int -> f a -> ShowS
liftShowsPrecOptions opts sp sl p = gliftShowsPrec opts (NonV4Show1Args sp sl) p . from1
#endif

-- | Class of generic representation types that can be converted to a 'String'.
class GShow1 v f where
  gliftShowsPrec :: Options -> Show1Args v a -> Int -> f a -> ShowS

instance GShow1 v f => GShow1 v (D1 d f) where
  gliftShowsPrec opts sas p (M1 x) = gliftShowsPrec opts sas p x

instance GShow1 v V1 where
#if __GLASGOW_HASKELL__ >= 708
  gliftShowsPrec _ _ _  x = case x of {}
#else
  gliftShowsPrec _ _ _ !_ = undefined
#endif

instance (GShow1 v f, GShow1 v g) => GShow1 v (f :+: g) where
  gliftShowsPrec opts sas p (L1 x) = gliftShowsPrec opts sas p x
  gliftShowsPrec opts sas p (R1 x) = gliftShowsPrec opts sas p x

instance (Constructor c, GShow1Con v f, IsNullaryCon f) => GShow1 v (C1 c f) where
  gliftShowsPrec opts sas p c@(M1 x) = case fixity of
      Prefix -> showParen ( p > appPrec
                             && not (isNullaryCon x || conIsTuple c)
                           ) $
             (if conIsTuple c
                 then id
                 else let cn = conName c
                      in showParen (isInfixDataCon cn) (showString cn))
           . (if isNullaryCon x || conIsTuple c
                 then id
                 else showChar ' ')
           . showBraces t (gliftShowsPrecCon opts t sas appPrec1 x)
      Infix _ m -> showParen (p > m) $ gliftShowsPrecCon opts t sas (m+1) x
    where
      fixity :: Fixity
      fixity = conFixity c

      t :: ConType
      t = if conIsRecord c
          then Rec
          else case conIsTuple c of
              True  -> Tup
              False -> case fixity of
                  Prefix    -> Pref
                  Infix _ _ -> Inf $ conName c

showBraces :: ConType -> ShowS -> ShowS
showBraces Rec     b = showChar '{' . b . showChar '}'
showBraces Tup     b = showChar '(' . b . showChar ')'
showBraces Pref    b = b
showBraces (Inf _) b = b

-- | Class of generic representation types that can be converted to a 'String', and
-- for which the 'ConType' has been determined.
class GShow1Con v f where
  gliftShowsPrecCon :: Options -> ConType -> Show1Args v a
                    -> Int -> f a -> ShowS

instance GShow1Con v U1 where
  gliftShowsPrecCon _ _ _ _ U1 = id

instance Show c => GShow1Con v (K1 i c) where
  gliftShowsPrecCon _ _ _ p (K1 x) = showsPrec p x

instance (Selector s, GShow1Con v f) => GShow1Con v (S1 s f) where
  gliftShowsPrecCon opts t sas p sel@(M1 x)
    | selName sel == "" =   gliftShowsPrecCon opts t sas p x
    | otherwise         =   infixRec
                          . showString " = "
                          . gliftShowsPrecCon opts t sas 0 x
    where
      infixRec :: ShowS
      infixRec | isSymVar selectorName
               = showChar '(' . showString selectorName . showChar ')'
               | otherwise
               = showString selectorName

      selectorName :: String
      selectorName = selName sel

instance (GShow1Con v f, GShow1Con v g) => GShow1Con v (f :*: g) where
  gliftShowsPrecCon opts t sas p (a :*: b) =
    case t of
         Rec ->     gliftShowsPrecCon opts t sas 0 a
                  . showString ", "
                  . gliftShowsPrecCon opts t sas 0 b

         Inf o ->   gliftShowsPrecCon opts t sas p a
                  . showSpace
                  . infixOp o
                  . showSpace
                  . gliftShowsPrecCon opts t sas p b

         Tup ->     gliftShowsPrecCon opts t sas 0 a
                  . showChar ','
                  . gliftShowsPrecCon opts t sas 0 b

         Pref ->    gliftShowsPrecCon opts t sas p a
                  . showSpace
                  . gliftShowsPrecCon opts t sas p b
    where
      infixOp :: String -> ShowS
      infixOp o = if isInfixDataCon o
                     then showString o
                     else showChar '`' . showString o . showChar '`'

#if defined(TRANSFORMERS_FOUR)
instance GShow1Con V4 Par1 where
  gliftShowsPrecCon _ _ V4Show1Args p (Par1 x) = showsPrec p x

instance Show1 f => GShow1Con V4 (Rec1 f) where
  gliftShowsPrecCon _ _ V4Show1Args p (Rec1 x) = showsPrec1 p x

instance (Functor f, Show1 f, GShow1Con V4 g) => GShow1Con V4 (f :.: g) where
  gliftShowsPrecCon _ _ V4Show1Args p (Comp1 x) = showsPrec1 p (fmap Apply x)
#else
instance GShow1Con NonV4 Par1 where
  gliftShowsPrecCon _ _ (NonV4Show1Args sp _) p (Par1 x) = sp p x

instance Show1 f => GShow1Con NonV4 (Rec1 f) where
  gliftShowsPrecCon _ _ (NonV4Show1Args sp sl) p (Rec1 x) = liftShowsPrec sp sl p x

instance (Show1 f, GShow1Con NonV4 g) => GShow1Con NonV4 (f :.: g) where
  gliftShowsPrecCon opts t (NonV4Show1Args sp sl) p (Comp1 x) =
    let glspc = gliftShowsPrecCon opts t (NonV4Show1Args sp sl)
    in liftShowsPrec glspc (showListWith (glspc 0)) p x
#endif

#if MIN_VERSION_base(4,9,0) || defined(GENERIC_DERIVING)
instance GShow1Con v UChar where
  gliftShowsPrecCon opts _ _ p (UChar c) =
    showsPrec (hashPrec opts p) (C# c) . oneHash opts

instance GShow1Con v UDouble where
  gliftShowsPrecCon opts _ _ p (UDouble d) =
    showsPrec (hashPrec opts p) (D# d) . twoHash opts

instance GShow1Con v UFloat where
  gliftShowsPrecCon opts _ _ p (UFloat f) =
    showsPrec (hashPrec opts p) (F# f) . oneHash opts

instance GShow1Con v UInt where
  gliftShowsPrecCon opts _ _ p (UInt i) =
    showsPrec (hashPrec opts p) (I# i) . oneHash opts

instance GShow1Con v UWord where
  gliftShowsPrecCon opts _ _ p (UWord w) =
    showsPrec (hashPrec opts p) (W# w) . twoHash opts

oneHash, twoHash :: Options -> ShowS
hashPrec         :: Options -> Int -> Int
oneHash  opts = if ghc8ShowBehavior opts then showChar   '#'  else id
twoHash  opts = if ghc8ShowBehavior opts then showString "##" else id
hashPrec opts = if ghc8ShowBehavior opts then const 0         else id
#endif

-------------------------------------------------------------------------------
-- * GenericFunctorClasses
-------------------------------------------------------------------------------

-- | An adapter newtype, suitable for @DerivingVia@. Its 'Eq1', 'Ord1',
-- 'Read1', and 'Show1' instances leverage 'Generic1'-based defaults.
newtype FunctorClassesDefault f a =
  FunctorClassesDefault { getFunctorClassesDefault :: f a }

#if defined(TRANSFORMERS_FOUR)
instance (GEq1 V4 (Rep1 f), Generic1 f) => Eq1 (FunctorClassesDefault f) where
   eq1 (FunctorClassesDefault x) (FunctorClassesDefault y) = eq1Default x y
instance (GOrd1 V4 (Rep1 f), Generic1 f) => Ord1 (FunctorClassesDefault f) where
   compare1 (FunctorClassesDefault x) (FunctorClassesDefault y) = compare1Default x y
instance (GRead1 V4 (Rep1 f), Generic1 f) => Read1 (FunctorClassesDefault f) where
   readsPrec1 p = coerceFCD (readsPrec1Default p)
instance (GShow1 V4 (Rep1 f), Generic1 f) => Show1 (FunctorClassesDefault f) where
   showsPrec1 p (FunctorClassesDefault x) = showsPrec1Default p x
#else
instance (GEq1 NonV4 (Rep1 f), Generic1 f) => Eq1 (FunctorClassesDefault f) where
   liftEq f (FunctorClassesDefault x) (FunctorClassesDefault y) = liftEqDefault f x y
instance (GOrd1 NonV4 (Rep1 f), Generic1 f) => Ord1 (FunctorClassesDefault f) where
   liftCompare f (FunctorClassesDefault x) (FunctorClassesDefault y) = liftCompareDefault f x y
instance (GRead1 NonV4 (Rep1 f), Generic1 f) => Read1 (FunctorClassesDefault f) where
   liftReadsPrec rp rl p = coerceFCD (liftReadsPrecDefault rp rl p)
instance (GShow1 NonV4 (Rep1 f), Generic1 f) => Show1 (FunctorClassesDefault f) where
   liftShowsPrec sp sl p (FunctorClassesDefault x) = liftShowsPrecDefault sp sl p x
#endif

coerceFCD :: ReadS (f a) -> ReadS (FunctorClassesDefault f a)
coerceFCD = coerce

-------------------------------------------------------------------------------
-- * Shared code
-------------------------------------------------------------------------------

#if defined(TRANSFORMERS_FOUR)
newtype Apply g a = Apply { getApply :: g a }

instance (GEq1 V4 g, Eq a) => Eq (Apply g a) where
    Apply x == Apply y = gliftEq V4Eq1Args x y

instance (GOrd1 V4 g, Ord a) => Ord (Apply g a) where
    compare (Apply x) (Apply y) = gliftCompare V4Ord1Args x y

-- Passing defaultOptions and Pref below is OK, since it's guaranteed that the
-- Options and ConType won't actually have any effect on how (g a) is shown.
-- If we augment Options or ConType with more features in the future, this
-- decision will need to be revisited.

instance (GRead1Con V4 g, Read a) => Read (Apply g a) where
    readPrec = fmap Apply $ gliftReadPrecCon Pref V4Read1Args

instance (GShow1Con V4 g, Show a) => Show (Apply g a) where
    showsPrec d = gliftShowsPrecCon defaultOptions Pref V4Show1Args d . getApply
#endif

-- | Whether a constructor is a record ('Rec'), a tuple ('Tup'), is prefix ('Pref'),
-- or infix ('Inf').
data ConType = Rec | Tup | Pref | Inf String

conIsTuple :: Constructor c => C1 c f p -> Bool
conIsTuple = isTupleString . conName

isTupleString :: String -> Bool
isTupleString ('(':',':_) = True
isTupleString _           = False

isInfixDataCon :: String -> Bool
isInfixDataCon (':':_) = True
isInfixDataCon _       = False

-- | Class of generic representation types that represent a data type with
-- zero or more constructors.
class IsNullaryDataType f where
    -- | Returns 'True' if the data type has no constructors.
    isNullaryDataType :: f a -> Bool

instance IsNullaryDataType (f :+: g) where
    isNullaryDataType _ = False

instance IsNullaryDataType (C1 c f) where
    isNullaryDataType _ = False

-- | Class of generic representation types that represent a constructor with
-- zero or more fields.
class IsNullaryCon f where
    -- | Returns 'True' if the constructor has no fields.
    isNullaryCon :: f a -> Bool

instance IsNullaryDataType V1 where
    isNullaryDataType _ = True

instance IsNullaryCon U1 where
    isNullaryCon _ = True

instance IsNullaryCon Par1 where
    isNullaryCon _ = False

instance IsNullaryCon (K1 i c) where
    isNullaryCon _ = False

instance IsNullaryCon f => IsNullaryCon (S1 s f) where
    isNullaryCon (M1 x) = isNullaryCon x

instance IsNullaryCon (Rec1 f) where
    isNullaryCon _ = False

instance IsNullaryCon (f :*: g) where
    isNullaryCon _ = False

instance IsNullaryCon (f :.: g) where
    isNullaryCon _ = False

#if MIN_VERSION_base(4,9,0) || defined(GENERIC_DERIVING)
instance IsNullaryCon UChar where
    isNullaryCon _ = False

instance IsNullaryCon UDouble where
    isNullaryCon _ = False

instance IsNullaryCon UFloat where
    isNullaryCon _ = False

instance IsNullaryCon UInt where
    isNullaryCon _ = False

instance IsNullaryCon UWord where
    isNullaryCon _ = False

# if __GLASGOW_HASKELL__ < 708
isTrue# :: Bool -> Bool
isTrue# = id
# endif
#endif
