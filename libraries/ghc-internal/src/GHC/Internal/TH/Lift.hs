{-# OPTIONS_HADDOCK not-home #-} -- we want users to import Language.Haskell.TH.Syntax instead
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-inline-rule-shadowing #-}

-- | This module gives the definition of the 'Lift' class.
--
-- This is an internal module.
-- Please import "Language.Haskell.TH" or "Language.Haskell.TH.Syntax" instead!

module GHC.Internal.TH.Lift
  ( Lift(..)
  -- * Generic Lift implementations
  , dataToQa
  , dataToExpQ
  , liftData
  , dataToPatQ
  -- * Wired-in names
  , liftString
  , trueName
  , falseName
  , nothingName
  , justName
  , leftName
  , rightName
  , nonemptyName
  )
  where

import GHC.Internal.TH.Syntax
import GHC.Internal.TH.Lib ()  -- See wrinkle (W4) of Note [Tracking dependencies on primitives]
import GHC.Internal.Lexeme ( startsVarSym, startsVarId )

import GHC.Internal.Data.Either
import GHC.Internal.Type.Reflection
import GHC.Internal.Data.Bool
import GHC.Internal.Base hiding (Type, Module, inline)
import GHC.Internal.Data.Foldable
import GHC.Internal.Data.Functor
import GHC.Internal.Integer
import GHC.Internal.Real
import GHC.Internal.Word
import GHC.Internal.Int
import GHC.Internal.Data.Data
import GHC.Internal.Natural

-- | A 'Lift' instance can have any of its values turned into a Template
-- Haskell expression. This is needed when a value used within a Template
-- Haskell quotation is bound outside the Oxford brackets (@[| ... |]@ or
-- @[|| ... ||]@) but not at the top level. As an example:
--
-- > add1 :: Int -> Code Q Int
-- > add1 x = [|| x + 1 ||]
--
-- Template Haskell has no way of knowing what value @x@ will take on at
-- splice-time, so it requires the type of @x@ to be an instance of 'Lift'.
--
-- A 'Lift' instance must satisfy @$(lift x) ≡ x@ and @$$(liftTyped x) ≡ x@
-- for all @x@, where @$(...)@ and @$$(...)@ are Template Haskell splices.
-- It is additionally expected that @'lift' x ≡ 'unTypeCode' ('liftTyped' x)@.
--
-- 'Lift' instances can be derived automatically by use of the @-XDeriveLift@
-- GHC language extension:
--
-- > {-# LANGUAGE DeriveLift #-}
-- > module Foo where
-- >
-- > import Language.Haskell.TH.Syntax
-- >
-- > data Bar a = Bar1 a (Bar a) | Bar2 String
-- >   deriving Lift
--
-- Representation-polymorphic since /template-haskell-2.16.0.0/.
class Lift (t :: TYPE r) where
  -- | Turn a value into a Template Haskell expression, suitable for use in
  -- a splice.
  lift :: Quote m => t -> m Exp
  default lift :: (r ~ ('BoxedRep 'Lifted), Quote m) => t -> m Exp
  lift = unTypeCode . liftTyped

  -- | Turn a value into a Template Haskell typed expression, suitable for use
  -- in a typed splice.
  --
  -- @since template-haskell-2.16.0.0
  liftTyped :: Quote m => t -> Code m t


-- If you add any instances here, consider updating test th/TH_Lift
instance Lift Integer where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL x))

instance Lift Int where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

-- | @since template-haskell-2.16.0.0
instance Lift Int# where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntPrimL (fromIntegral (I# x))))

instance Lift Int8 where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Int16 where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Int32 where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Int64 where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

-- | @since template-haskell-2.16.0.0
instance Lift Word# where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (WordPrimL (fromIntegral (W# x))))

instance Lift Word where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word8 where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word16 where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word32 where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word64 where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Natural where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Integral a => Lift (Ratio a) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (RationalL (toRational x)))

instance Lift Float where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (RationalL (toRational x)))

-- | @since template-haskell-2.16.0.0
instance Lift Float# where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (FloatPrimL (toRational (F# x))))

instance Lift Double where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (RationalL (toRational x)))

-- | @since template-haskell-2.16.0.0
instance Lift Double# where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (DoublePrimL (toRational (D# x))))

instance Lift Char where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (CharL x))

-- | @since template-haskell-2.16.0.0
instance Lift Char# where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (CharPrimL (C# x)))

instance Lift Bool where
  liftTyped x = unsafeCodeCoerce (lift x)

  lift True  = return (ConE trueName)
  lift False = return (ConE falseName)

-- | Produces an 'Addr#' literal from the NUL-terminated C-string starting at
-- the given memory address.
--
-- @since template-haskell-2.16.0.0
instance Lift Addr# where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x
    = return (LitE (StringPrimL (map (fromIntegral . ord) (unpackCString# x))))

instance Lift a => Lift (Maybe a) where
  liftTyped x = unsafeCodeCoerce (lift x)

  lift Nothing  = return (ConE nothingName)
  lift (Just x) = liftM (ConE justName `AppE`) (lift x)

instance (Lift a, Lift b) => Lift (Either a b) where
  liftTyped x = unsafeCodeCoerce (lift x)

  lift (Left x)  = liftM (ConE leftName  `AppE`) (lift x)
  lift (Right y) = liftM (ConE rightName `AppE`) (lift y)

instance Lift a => Lift [a] where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift xs = do { xs' <- mapM lift xs; return (ListE xs') }

liftString :: Quote m => String -> m Exp
-- Used in GHC.Tc.Gen.Expr to short-circuit the lifting for strings
liftString s = return (LitE (StringL s))

-- | @since template-haskell-2.15.0.0
instance Lift a => Lift (NonEmpty a) where
  liftTyped x = unsafeCodeCoerce (lift x)

  lift (x :| xs) = do
    x' <- lift x
    xs' <- lift xs
    return (InfixE (Just x') (ConE nonemptyName) (Just xs'))

-- | @since template-haskell-2.15.0.0
instance Lift Void where
  liftTyped = liftCode . absurd
  lift = pure . absurd

instance Lift () where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift () = return (ConE (tupleDataName 0))

instance (Lift a, Lift b) => Lift (a, b) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift (a, b)
    = liftM TupE $ sequence $ map (fmap Just) [lift a, lift b]

instance (Lift a, Lift b, Lift c) => Lift (a, b, c) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift (a, b, c)
    = liftM TupE $ sequence $ map (fmap Just) [lift a, lift b, lift c]

instance (Lift a, Lift b, Lift c, Lift d) => Lift (a, b, c, d) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift (a, b, c, d)
    = liftM TupE $ sequence $ map (fmap Just) [lift a, lift b, lift c, lift d]

instance (Lift a, Lift b, Lift c, Lift d, Lift e)
      => Lift (a, b, c, d, e) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift (a, b, c, d, e)
    = liftM TupE $ sequence $ map (fmap Just) [ lift a, lift b
                                              , lift c, lift d, lift e ]

instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f)
      => Lift (a, b, c, d, e, f) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift (a, b, c, d, e, f)
    = liftM TupE $ sequence $ map (fmap Just) [ lift a, lift b, lift c
                                              , lift d, lift e, lift f ]

instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g)
      => Lift (a, b, c, d, e, f, g) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift (a, b, c, d, e, f, g)
    = liftM TupE $ sequence $ map (fmap Just) [ lift a, lift b, lift c
                                              , lift d, lift e, lift f, lift g ]

-- | @since template-haskell-2.16.0.0
instance Lift (# #) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift (# #) = return (ConE (unboxedTupleTypeName 0))

-- | @since template-haskell-2.16.0.0
instance (Lift a) => Lift (# a #) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift (# a #)
    = liftM UnboxedTupE $ sequence $ map (fmap Just) [lift a]

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b) => Lift (# a, b #) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift (# a, b #)
    = liftM UnboxedTupE $ sequence $ map (fmap Just) [lift a, lift b]

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c)
      => Lift (# a, b, c #) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift (# a, b, c #)
    = liftM UnboxedTupE $ sequence $ map (fmap Just) [lift a, lift b, lift c]

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c, Lift d)
      => Lift (# a, b, c, d #) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift (# a, b, c, d #)
    = liftM UnboxedTupE $ sequence $ map (fmap Just) [ lift a, lift b
                                                     , lift c, lift d ]

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c, Lift d, Lift e)
      => Lift (# a, b, c, d, e #) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift (# a, b, c, d, e #)
    = liftM UnboxedTupE $ sequence $ map (fmap Just) [ lift a, lift b
                                                     , lift c, lift d, lift e ]

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f)
      => Lift (# a, b, c, d, e, f #) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift (# a, b, c, d, e, f #)
    = liftM UnboxedTupE $ sequence $ map (fmap Just) [ lift a, lift b, lift c
                                                     , lift d, lift e, lift f ]

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g)
      => Lift (# a, b, c, d, e, f, g #) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift (# a, b, c, d, e, f, g #)
    = liftM UnboxedTupE $ sequence $ map (fmap Just) [ lift a, lift b, lift c
                                                     , lift d, lift e, lift f
                                                     , lift g ]

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b) => Lift (# a | b #) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x
    = case x of
        (# y | #) -> UnboxedSumE <$> lift y <*> pure 1 <*> pure 2
        (# | y #) -> UnboxedSumE <$> lift y <*> pure 2 <*> pure 2

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c)
      => Lift (# a | b | c #) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x
    = case x of
        (# y | | #) -> UnboxedSumE <$> lift y <*> pure 1 <*> pure 3
        (# | y | #) -> UnboxedSumE <$> lift y <*> pure 2 <*> pure 3
        (# | | y #) -> UnboxedSumE <$> lift y <*> pure 3 <*> pure 3

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c, Lift d)
      => Lift (# a | b | c | d #) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x
    = case x of
        (# y | | | #) -> UnboxedSumE <$> lift y <*> pure 1 <*> pure 4
        (# | y | | #) -> UnboxedSumE <$> lift y <*> pure 2 <*> pure 4
        (# | | y | #) -> UnboxedSumE <$> lift y <*> pure 3 <*> pure 4
        (# | | | y #) -> UnboxedSumE <$> lift y <*> pure 4 <*> pure 4

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c, Lift d, Lift e)
      => Lift (# a | b | c | d | e #) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x
    = case x of
        (# y | | | | #) -> UnboxedSumE <$> lift y <*> pure 1 <*> pure 5
        (# | y | | | #) -> UnboxedSumE <$> lift y <*> pure 2 <*> pure 5
        (# | | y | | #) -> UnboxedSumE <$> lift y <*> pure 3 <*> pure 5
        (# | | | y | #) -> UnboxedSumE <$> lift y <*> pure 4 <*> pure 5
        (# | | | | y #) -> UnboxedSumE <$> lift y <*> pure 5 <*> pure 5

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f)
      => Lift (# a | b | c | d | e | f #) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x
    = case x of
        (# y | | | | | #) -> UnboxedSumE <$> lift y <*> pure 1 <*> pure 6
        (# | y | | | | #) -> UnboxedSumE <$> lift y <*> pure 2 <*> pure 6
        (# | | y | | | #) -> UnboxedSumE <$> lift y <*> pure 3 <*> pure 6
        (# | | | y | | #) -> UnboxedSumE <$> lift y <*> pure 4 <*> pure 6
        (# | | | | y | #) -> UnboxedSumE <$> lift y <*> pure 5 <*> pure 6
        (# | | | | | y #) -> UnboxedSumE <$> lift y <*> pure 6 <*> pure 6

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g)
      => Lift (# a | b | c | d | e | f | g #) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x
    = case x of
        (# y | | | | | | #) -> UnboxedSumE <$> lift y <*> pure 1 <*> pure 7
        (# | y | | | | | #) -> UnboxedSumE <$> lift y <*> pure 2 <*> pure 7
        (# | | y | | | | #) -> UnboxedSumE <$> lift y <*> pure 3 <*> pure 7
        (# | | | y | | | #) -> UnboxedSumE <$> lift y <*> pure 4 <*> pure 7
        (# | | | | y | | #) -> UnboxedSumE <$> lift y <*> pure 5 <*> pure 7
        (# | | | | | y | #) -> UnboxedSumE <$> lift y <*> pure 6 <*> pure 7
        (# | | | | | | y #) -> UnboxedSumE <$> lift y <*> pure 7 <*> pure 7

-- TH has a special form for literal strings,
-- which we should take advantage of.
-- NB: the lhs of the rule has no args, so that
--     the rule will apply to a 'lift' all on its own
--     which happens to be the way the type checker
--     creates it.
{-# RULES "TH:liftString" lift = \s -> return (LitE (StringL s)) #-}


trueName, falseName :: Name
trueName  = 'True
falseName = 'False

nothingName, justName :: Name
nothingName = 'Nothing
justName    = 'Just

leftName, rightName :: Name
leftName  = 'Left
rightName = 'Right

nonemptyName :: Name
nonemptyName = '(:|)

-----------------------------------------------------
--
--              Generic Lift implementations
--
-----------------------------------------------------

-- | 'dataToQa' is an internal utility function for constructing generic
-- conversion functions from types with 'Data' instances to various
-- quasi-quoting representations.  See the source of 'dataToExpQ' and
-- 'dataToPatQ' for two example usages: @mkCon@, @mkLit@
-- and @appQ@ are overloadable to account for different syntax for
-- expressions and patterns; @antiQ@ allows you to override type-specific
-- cases, a common usage is just @const Nothing@, which results in
-- no overloading.
dataToQa  ::  forall m a k q. (Quote m, Data a)
          =>  (Name -> k)
          ->  (Lit -> m q)
          ->  (k -> [m q] -> m q)
          ->  (forall b . Data b => b -> Maybe (m q))
          ->  a
          ->  m q
dataToQa mkCon mkLit appCon antiQ t =
    case antiQ t of
      Nothing ->
          case constrRep constr of
            AlgConstr _ ->
                appCon (mkCon funOrConName) conArgs
              where
                funOrConName :: Name
                funOrConName =
                    case showConstr constr of
                      "(:)"       -> Name (mkOccName ":")
                                          (NameG DataName
                                                (mkPkgName "ghc-prim")
                                                (mkModName "GHC.Types"))
                      con@"[]"    -> Name (mkOccName con)
                                          (NameG DataName
                                                (mkPkgName "ghc-prim")
                                                (mkModName "GHC.Types"))
                      con@('(':_) -> Name (mkOccName con)
                                          (NameG DataName
                                                (mkPkgName "ghc-prim")
                                                (mkModName "GHC.Tuple"))

                      -- Tricky case: see Note [Data for non-algebraic types]
                      fun@(x:_)   | startsVarSym x || startsVarId x
                                  -> mkNameG_v tyconPkg tyconMod fun
                      con         -> mkNameG_d tyconPkg tyconMod con

                  where
                    tycon :: TyCon
                    tycon = (typeRepTyCon . typeOf) t

                    tyconPkg, tyconMod :: String
                    tyconPkg = tyConPackage tycon
                    tyconMod = tyConModule  tycon

                conArgs :: [m q]
                conArgs = gmapQ (dataToQa mkCon mkLit appCon antiQ) t
            IntConstr n ->
                mkLit $ IntegerL n
            FloatConstr n ->
                mkLit $ RationalL n
            CharConstr c ->
                mkLit $ CharL c
        where
          constr :: Constr
          constr = toConstr t

      Just y -> y


{- Note [Data for non-algebraic types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Class Data was originally intended for algebraic data types.  But
it is possible to use it for abstract types too.  For example, in
package `text` we find

  instance Data Text where
    ...
    toConstr _ = packConstr

  packConstr :: Constr
  packConstr = mkConstr textDataType "pack" [] Prefix

Here `packConstr` isn't a real data constructor, it's an ordinary
function.  Two complications

* In such a case, we must take care to build the Name using
  mkNameG_v (for values), not mkNameG_d (for data constructors).
  See #10796.

* The pseudo-constructor is named only by its string, here "pack".
  But 'dataToQa' needs the TyCon of its defining module, and has
  to assume it's defined in the same module as the TyCon itself.
  But nothing enforces that; #12596 shows what goes wrong if
  "pack" is defined in a different module than the data type "Text".
  -}

-- | 'dataToExpQ' converts a value to a 'Exp' representation of the
-- same value, in the SYB style. It is generalized to take a function
-- override type-specific cases; see 'liftData' for a more commonly
-- used variant.
dataToExpQ  ::  (Quote m, Data a)
            =>  (forall b . Data b => b -> Maybe (m Exp))
            ->  a
            ->  m Exp
dataToExpQ = dataToQa varOrConE litE (foldl appE)
    where
          -- Make sure that VarE is used if the Constr value relies on a
          -- function underneath the surface (instead of a constructor).
          -- See #10796.
          varOrConE s =
            case nameSpace s of
                 Just VarName      -> return (VarE s)
                 Just (FldName {}) -> return (VarE s)
                 Just DataName     -> return (ConE s)
                 _ -> error $ "Can't construct an expression from name "
                           ++ showName s
          appE x y = do { a <- x; b <- y; return (AppE a b)}
          litE c = return (LitE c)

-- | 'liftData' is a variant of 'lift' in the 'Lift' type class which
-- works for any type with a 'Data' instance.
liftData :: (Quote m, Data a) => a -> m Exp
liftData = dataToExpQ (const Nothing)

-- | 'dataToPatQ' converts a value to a 'Pat' representation of the same
-- value, in the SYB style. It takes a function to handle type-specific cases,
-- alternatively, pass @const Nothing@ to get default behavior.
dataToPatQ  ::  (Quote m, Data a)
            =>  (forall b . Data b => b -> Maybe (m Pat))
            ->  a
            ->  m Pat
dataToPatQ = dataToQa id litP conP
    where litP l = return (LitP l)
          conP n ps =
            case nameSpace n of
                Just DataName -> do
                    ps' <- sequence ps
                    return (ConP n [] ps')
                _ -> error $ "Can't construct a pattern from name "
                          ++ showName n
