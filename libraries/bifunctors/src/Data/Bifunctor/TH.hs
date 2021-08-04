{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif

#ifndef MIN_VERSION_template_haskell
#define MIN_VERSION_template_haskell(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2008-2016 Edward Kmett, (C) 2015-2016 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions to mechanically derive 'Bifunctor', 'Bifoldable',
-- or 'Bitraversable' instances, or to splice their functions directly into
-- source code. You need to enable the @TemplateHaskell@ language extension
-- in order to use this module.
----------------------------------------------------------------------------

module Data.Bifunctor.TH (
    -- * @derive@- functions
    -- $derive
    -- * @make@- functions
    -- $make
    -- * 'Bifunctor'
    deriveBifunctor
  , deriveBifunctorOptions
  , makeBimap
  , makeBimapOptions
    -- * 'Bifoldable'
  , deriveBifoldable
  , deriveBifoldableOptions
  , makeBifold
  , makeBifoldOptions
  , makeBifoldMap
  , makeBifoldMapOptions
  , makeBifoldr
  , makeBifoldrOptions
  , makeBifoldl
  , makeBifoldlOptions
    -- * 'Bitraversable'
  , deriveBitraversable
  , deriveBitraversableOptions
  , makeBitraverse
  , makeBitraverseOptions
  , makeBisequenceA
  , makeBisequenceAOptions
  , makeBimapM
  , makeBimapMOptions
  , makeBisequence
  , makeBisequenceOptions
    -- * 'Options'
  , Options(..)
  , defaultOptions
  ) where

import           Control.Monad (guard, unless, when)

import           Data.Bifunctor.TH.Internal
import qualified Data.List as List
import qualified Data.Map as Map ((!), fromList, keys, lookup, member, size)
import           Data.Maybe

import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.Datatype.TyVarBndr
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Ppr
import           Language.Haskell.TH.Syntax

-------------------------------------------------------------------------------
-- User-facing API
-------------------------------------------------------------------------------

-- | Options that further configure how the functions in "Data.Bifunctor.TH"
-- should behave.
newtype Options = Options
  { emptyCaseBehavior :: Bool
    -- ^ If 'True', derived instances for empty data types (i.e., ones with
    --   no data constructors) will use the @EmptyCase@ language extension.
    --   If 'False', derived instances will simply use 'seq' instead.
    --   (This has no effect on GHCs before 7.8, since @EmptyCase@ is only
    --   available in 7.8 or later.)
  } deriving (Eq, Ord, Read, Show)

-- | Conservative 'Options' that doesn't attempt to use @EmptyCase@ (to
-- prevent users from having to enable that extension at use sites.)
defaultOptions :: Options
defaultOptions = Options { emptyCaseBehavior = False }

{- $derive

'deriveBifunctor', 'deriveBifoldable', and 'deriveBitraversable' automatically
generate their respective class instances for a given data type, newtype, or data
family instance that has at least two type variable. Examples:

@
&#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
import Data.Bifunctor.TH

data Pair a b = Pair a b
$('deriveBifunctor' ''Pair) -- instance Bifunctor Pair where ...

data WrapLeftPair f g a b = WrapLeftPair (f a) (g a b)
$('deriveBifoldable' ''WrapLeftPair)
-- instance (Foldable f, Bifoldable g) => Bifoldable (WrapLeftPair f g) where ...
@

If you are using @template-haskell-2.7.0.0@ or later (i.e., GHC 7.4 or later),
the @derive@ functions can be used data family instances (which requires the
@-XTypeFamilies@ extension). To do so, pass the name of a data or newtype instance
constructor (NOT a data family name!) to a @derive@ function.  Note that the
generated code may require the @-XFlexibleInstances@ extension. Example:

@
&#123;-&#35; LANGUAGE FlexibleInstances, TemplateHaskell, TypeFamilies &#35;-&#125;
import Data.Bifunctor.TH

class AssocClass a b c where
    data AssocData a b c
instance AssocClass Int b c where
    data AssocData Int b c = AssocDataInt1 Int | AssocDataInt2 b c
$('deriveBitraversable' 'AssocDataInt1) -- instance Bitraversable (AssocData Int) where ...
-- Alternatively, one could use $(deriveBitraversable 'AssocDataInt2)
@

Note that there are some limitations:

* The 'Name' argument to a @derive@ function must not be a type synonym.

* With a @derive@ function, the last two type variables must both be of kind @*@.
  Other type variables of kind @* -> *@ are assumed to require a 'Functor',
  'Foldable', or 'Traversable' constraint (depending on which @derive@ function is
  used), and other type variables of kind @* -> * -> *@ are assumed to require an
  'Bifunctor', 'Bifoldable', or 'Bitraversable' constraint. If your data type
  doesn't meet these assumptions, use a @make@ function.

* If using the @-XDatatypeContexts@, @-XExistentialQuantification@, or @-XGADTs@
  extensions, a constraint cannot mention either of the last two type variables. For
  example, @data Illegal2 a b where I2 :: Ord a => a -> b -> Illegal2 a b@ cannot
  have a derived 'Bifunctor' instance.

* If either of the last two type variables is used within a constructor argument's
  type, it must only be used in the last two type arguments. For example,
  @data Legal a b = Legal (Int, Int, a, b)@ can have a derived 'Bifunctor' instance,
  but @data Illegal a b = Illegal (a, b, a, b)@ cannot.

* Data family instances must be able to eta-reduce the last two type variables. In other
  words, if you have a instance of the form:

  @
  data family Family a1 ... an t1 t2
  data instance Family e1 ... e2 v1 v2 = ...
  @

  Then the following conditions must hold:

  1. @v1@ and @v2@ must be distinct type variables.
  2. Neither @v1@ not @v2@ must be mentioned in any of @e1@, ..., @e2@.

-}

{- $make

There may be scenarios in which you want to, say, 'bimap' over an arbitrary data type
or data family instance without having to make the type an instance of 'Bifunctor'. For
these cases, this module provides several functions (all prefixed with @make@-) that
splice the appropriate lambda expression into your source code.

This is particularly useful for creating instances for sophisticated data types. For
example, 'deriveBifunctor' cannot infer the correct type context for
@newtype HigherKinded f a b c = HigherKinded (f a b c)@, since @f@ is of kind
@* -> * -> * -> *@. However, it is still possible to create a 'Bifunctor' instance for
@HigherKinded@ without too much trouble using 'makeBimap':

@
&#123;-&#35; LANGUAGE FlexibleContexts, TemplateHaskell &#35;-&#125;
import Data.Bifunctor
import Data.Bifunctor.TH

newtype HigherKinded f a b c = HigherKinded (f a b c)

instance Bifunctor (f a) => Bifunctor (HigherKinded f a) where
    bimap = $(makeBimap ''HigherKinded)
@

-}

-- | Generates a 'Bifunctor' instance declaration for the given data type or data
-- family instance.
deriveBifunctor :: Name -> Q [Dec]
deriveBifunctor = deriveBifunctorOptions defaultOptions

-- | Like 'deriveBifunctor', but takes an 'Options' argument.
deriveBifunctorOptions :: Options -> Name -> Q [Dec]
deriveBifunctorOptions = deriveBiClass Bifunctor

-- | Generates a lambda expression which behaves like 'bimap' (without requiring a
-- 'Bifunctor' instance).
makeBimap :: Name -> Q Exp
makeBimap = makeBimapOptions defaultOptions

-- | Like 'makeBimap', but takes an 'Options' argument.
makeBimapOptions :: Options -> Name -> Q Exp
makeBimapOptions = makeBiFun Bimap

-- | Generates a 'Bifoldable' instance declaration for the given data type or data
-- family instance.
deriveBifoldable :: Name -> Q [Dec]
deriveBifoldable = deriveBifoldableOptions defaultOptions

-- | Like 'deriveBifoldable', but takes an 'Options' argument.
deriveBifoldableOptions :: Options -> Name -> Q [Dec]
deriveBifoldableOptions = deriveBiClass Bifoldable

--- | Generates a lambda expression which behaves like 'bifold' (without requiring a
-- 'Bifoldable' instance).
makeBifold :: Name -> Q Exp
makeBifold = makeBifoldOptions defaultOptions

-- | Like 'makeBifold', but takes an 'Options' argument.
makeBifoldOptions :: Options -> Name -> Q Exp
makeBifoldOptions opts name = appsE [ makeBifoldMapOptions opts name
                                    , varE idValName
                                    , varE idValName
                                    ]

-- | Generates a lambda expression which behaves like 'bifoldMap' (without requiring
-- a 'Bifoldable' instance).
makeBifoldMap :: Name -> Q Exp
makeBifoldMap = makeBifoldMapOptions defaultOptions

-- | Like 'makeBifoldMap', but takes an 'Options' argument.
makeBifoldMapOptions :: Options -> Name -> Q Exp
makeBifoldMapOptions = makeBiFun BifoldMap

-- | Generates a lambda expression which behaves like 'bifoldr' (without requiring a
-- 'Bifoldable' instance).
makeBifoldr :: Name -> Q Exp
makeBifoldr = makeBifoldrOptions defaultOptions

-- | Like 'makeBifoldr', but takes an 'Options' argument.
makeBifoldrOptions :: Options -> Name -> Q Exp
makeBifoldrOptions = makeBiFun Bifoldr

-- | Generates a lambda expression which behaves like 'bifoldl' (without requiring a
-- 'Bifoldable' instance).
makeBifoldl :: Name -> Q Exp
makeBifoldl = makeBifoldlOptions defaultOptions

-- | Like 'makeBifoldl', but takes an 'Options' argument.
makeBifoldlOptions :: Options -> Name -> Q Exp
makeBifoldlOptions opts name = do
  f <- newName "f"
  g <- newName "g"
  z <- newName "z"
  t <- newName "t"
  lamE [varP f, varP g, varP z, varP t] $
    appsE [ varE appEndoValName
          , appsE [ varE getDualValName
                  , appsE [ makeBifoldMapOptions opts name
                          , foldFun f
                          , foldFun g
                          , varE t]
                  ]
          , varE z
          ]
  where
    foldFun :: Name -> Q Exp
    foldFun n = infixApp (conE dualDataName)
                         (varE composeValName)
                         (infixApp (conE endoDataName)
                                   (varE composeValName)
                                   (varE flipValName `appE` varE n)
                         )

-- | Generates a 'Bitraversable' instance declaration for the given data type or data
-- family instance.
deriveBitraversable :: Name -> Q [Dec]
deriveBitraversable = deriveBitraversableOptions defaultOptions

-- | Like 'deriveBitraversable', but takes an 'Options' argument.
deriveBitraversableOptions :: Options -> Name -> Q [Dec]
deriveBitraversableOptions = deriveBiClass Bitraversable

-- | Generates a lambda expression which behaves like 'bitraverse' (without
-- requiring a 'Bitraversable' instance).
makeBitraverse :: Name -> Q Exp
makeBitraverse = makeBitraverseOptions defaultOptions

-- | Like 'makeBitraverse', but takes an 'Options' argument.
makeBitraverseOptions :: Options -> Name -> Q Exp
makeBitraverseOptions = makeBiFun Bitraverse

-- | Generates a lambda expression which behaves like 'bisequenceA' (without
-- requiring a 'Bitraversable' instance).
makeBisequenceA :: Name -> Q Exp
makeBisequenceA = makeBisequenceAOptions defaultOptions

-- | Like 'makeBitraverseA', but takes an 'Options' argument.
makeBisequenceAOptions :: Options -> Name -> Q Exp
makeBisequenceAOptions opts name = appsE [ makeBitraverseOptions opts name
                                         , varE idValName
                                         , varE idValName
                                         ]

-- | Generates a lambda expression which behaves like 'bimapM' (without
-- requiring a 'Bitraversable' instance).
makeBimapM :: Name -> Q Exp
makeBimapM = makeBimapMOptions defaultOptions

-- | Like 'makeBimapM', but takes an 'Options' argument.
makeBimapMOptions :: Options -> Name -> Q Exp
makeBimapMOptions opts name = do
  f <- newName "f"
  g <- newName "g"
  lamE [varP f, varP g] . infixApp (varE unwrapMonadValName) (varE composeValName) $
                          appsE [ makeBitraverseOptions opts name
                                , wrapMonadExp f
                                , wrapMonadExp g
                                ]
  where
    wrapMonadExp :: Name -> Q Exp
    wrapMonadExp n = infixApp (conE wrapMonadDataName) (varE composeValName) (varE n)

-- | Generates a lambda expression which behaves like 'bisequence' (without
-- requiring a 'Bitraversable' instance).
makeBisequence :: Name -> Q Exp
makeBisequence = makeBisequenceOptions defaultOptions

-- | Like 'makeBisequence', but takes an 'Options' argument.
makeBisequenceOptions :: Options -> Name -> Q Exp
makeBisequenceOptions opts name = appsE [ makeBimapMOptions opts name
                                        , varE idValName
                                        , varE idValName
                                        ]

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

-- | Derive a class instance declaration (depending on the BiClass argument's value).
deriveBiClass :: BiClass -> Options -> Name -> Q [Dec]
deriveBiClass biClass opts name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
                 , datatypeInstTypes = instTys
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } -> do
      (instanceCxt, instanceType)
          <- buildTypeInstance biClass parentName ctxt instTys variant
      (:[]) `fmap` instanceD (return instanceCxt)
                             (return instanceType)
                             (biFunDecs biClass opts parentName instTys cons)

-- | Generates a declaration defining the primary function(s) corresponding to a
-- particular class (bimap for Bifunctor, bifoldr and bifoldMap for Bifoldable, and
-- bitraverse for Bitraversable).
--
-- For why both bifoldr and bifoldMap are derived for Bifoldable, see Trac #7436.
biFunDecs :: BiClass -> Options -> Name -> [Type] -> [ConstructorInfo] -> [Q Dec]
biFunDecs biClass opts parentName instTys cons =
  map makeFunD $ biClassToFuns biClass
  where
    makeFunD :: BiFun -> Q Dec
    makeFunD biFun =
      funD (biFunName biFun)
           [ clause []
                    (normalB $ makeBiFunForCons biFun opts parentName instTys cons)
                    []
           ]

-- | Generates a lambda expression which behaves like the BiFun argument.
makeBiFun :: BiFun -> Options -> Name -> Q Exp
makeBiFun biFun opts name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
                 , datatypeInstTypes = instTys
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } ->
      -- We force buildTypeInstance here since it performs some checks for whether
      -- or not the provided datatype can actually have bimap/bifoldr/bitraverse/etc.
      -- implemented for it, and produces errors if it can't.
      buildTypeInstance (biFunToClass biFun) parentName ctxt instTys variant
        >> makeBiFunForCons biFun opts parentName instTys cons

-- | Generates a lambda expression for the given constructors.
-- All constructors must be from the same type.
makeBiFunForCons :: BiFun -> Options -> Name -> [Type] -> [ConstructorInfo] -> Q Exp
makeBiFunForCons biFun opts _parentName instTys cons = do
  map1  <- newName "f"
  map2  <- newName "g"
  z     <- newName "z" -- Only used for deriving bifoldr
  value <- newName "value"
  let argNames   = catMaybes [ Just map1
                             , Just map2
                             , guard (biFun == Bifoldr) >> Just z
                             , Just value
                             ]
      lastTyVars = map varTToName $ drop (length instTys - 2) instTys
      tvMap      = Map.fromList $ zip lastTyVars [map1, map2]
  lamE (map varP argNames)
      . appsE
      $ [ varE $ biFunConstName biFun
        , makeFun z value tvMap
        ] ++ map varE argNames
  where
    makeFun :: Name -> Name -> TyVarMap -> Q Exp
    makeFun z value tvMap = do
#if MIN_VERSION_template_haskell(2,9,0)
      roles <- reifyRoles _parentName
#endif
      case () of
        _

#if MIN_VERSION_template_haskell(2,9,0)
          | Just (rs, PhantomR) <- unsnoc roles
          , Just (_,  PhantomR) <- unsnoc rs
         -> biFunPhantom z value
#endif

          | null cons && emptyCaseBehavior opts && ghc7'8OrLater
         -> biFunEmptyCase biFun z value

          | null cons
         -> biFunNoCons biFun z value

          | otherwise
         -> caseE (varE value)
                  (map (makeBiFunForCon biFun z tvMap) cons)

    ghc7'8OrLater :: Bool
#if __GLASGOW_HASKELL__ >= 708
    ghc7'8OrLater = True
#else
    ghc7'8OrLater = False
#endif

#if MIN_VERSION_template_haskell(2,9,0)
    biFunPhantom :: Name -> Name -> Q Exp
    biFunPhantom z value =
        biFunTrivial coerce
                     (varE pureValName `appE` coerce)
                     biFun z
      where
        coerce :: Q Exp
        coerce = varE coerceValName `appE` varE value
#endif

-- | Generates a match for a single constructor.
makeBiFunForCon :: BiFun -> Name -> TyVarMap -> ConstructorInfo -> Q Match
makeBiFunForCon biFun z tvMap
  con@(ConstructorInfo { constructorName    = conName
                       , constructorContext = ctxt }) = do
    when ((any (`predMentionsName` Map.keys tvMap) ctxt
             || Map.size tvMap < 2)
             && not (allowExQuant (biFunToClass biFun))) $
      existentialContextError conName
    case biFun of
      Bimap      -> makeBimapMatch tvMap con
      Bifoldr    -> makeBifoldrMatch z tvMap con
      BifoldMap  -> makeBifoldMapMatch tvMap con
      Bitraverse -> makeBitraverseMatch tvMap con

-- | Generates a match whose right-hand side implements @bimap@.
makeBimapMatch :: TyVarMap -> ConstructorInfo -> Q Match
makeBimapMatch tvMap con@(ConstructorInfo{constructorName = conName}) = do
  parts <- foldDataConArgs tvMap ft_bimap con
  match_for_con conName parts
  where
    ft_bimap :: FFoldType (Exp -> Q Exp)
    ft_bimap = FT { ft_triv = return
                  , ft_var  = \v x -> return $ VarE (tvMap Map.! v) `AppE` x
                  , ft_fun  = \g h x -> mkSimpleLam $ \b -> do
                      gg <- g b
                      h $ x `AppE` gg
                  , ft_tup  = mkSimpleTupleCase match_for_con
                  , ft_ty_app = \argGs x -> do
                      let inspect :: (Type, Exp -> Q Exp) -> Q Exp
                          inspect (argTy, g)
                            -- If the argument type is a bare occurrence of one
                            -- of the data type's last type variables, then we
                            -- can generate more efficient code.
                            -- This was inspired by GHC#17880.
                            | Just argVar <- varTToName_maybe argTy
                            , Just f <- Map.lookup argVar tvMap
                            = return $ VarE f
                            | otherwise
                            = mkSimpleLam g
                      appsE $ varE (fmapArity (length argGs))
                            : map inspect argGs
                           ++ [return x]
                  , ft_forall  = \_ g x -> g x
                  , ft_bad_app = \_ -> outOfPlaceTyVarError conName
                  , ft_co_var  = \_ _ -> contravarianceError conName
                  }

    -- Con a1 a2 ... -> Con (f1 a1) (f2 a2) ...
    match_for_con :: Name -> [Exp -> Q Exp] -> Q Match
    match_for_con = mkSimpleConMatch $ \conName' xs ->
       appsE (conE conName':xs) -- Con x1 x2 ..

-- | Generates a match whose right-hand side implements @bifoldr@.
makeBifoldrMatch :: Name -> TyVarMap -> ConstructorInfo -> Q Match
makeBifoldrMatch z tvMap con@(ConstructorInfo{constructorName = conName}) = do
  parts  <- foldDataConArgs tvMap ft_bifoldr con
  parts' <- sequence parts
  match_for_con (VarE z) conName parts'
  where
    -- The Bool is True if the type mentions of the last two type parameters,
    -- False otherwise. Later, match_for_con uses mkSimpleConMatch2 to filter
    -- out expressions that do not mention the last parameters by checking for
    -- False.
    ft_bifoldr :: FFoldType (Q (Bool, Exp))
    ft_bifoldr = FT { -- See Note [ft_triv for Bifoldable and Bitraversable]
                      ft_triv = do lam <- mkSimpleLam2 $ \_ z' -> return z'
                                   return (False, lam)
                    , ft_var  = \v -> return (True, VarE $ tvMap Map.! v)
                    , ft_tup  = \t gs -> do
                        gg  <- sequence gs
                        lam <- mkSimpleLam2 $ \x z' ->
                          mkSimpleTupleCase (match_for_con z') t gg x
                        return (True, lam)
                    , ft_ty_app = \gs -> do
                        lam <- mkSimpleLam2 $ \x z' ->
                                 appsE $ varE (foldrArity (length gs))
                                       : map (\(_, hs) -> fmap snd hs) gs
                                      ++ map return [z', x]
                        return (True, lam)
                    , ft_forall  = \_ g -> g
                    , ft_co_var  = \_ -> contravarianceError conName
                    , ft_fun     = \_ _ -> noFunctionsError conName
                    , ft_bad_app = outOfPlaceTyVarError conName
                    }

    match_for_con :: Exp -> Name -> [(Bool, Exp)] -> Q Match
    match_for_con zExp = mkSimpleConMatch2 $ \_ xs -> return $ mkBifoldr xs
      where
        -- g1 v1 (g2 v2 (.. z))
        mkBifoldr :: [Exp] -> Exp
        mkBifoldr = foldr AppE zExp

-- | Generates a match whose right-hand side implements @bifoldMap@.
makeBifoldMapMatch :: TyVarMap -> ConstructorInfo -> Q Match
makeBifoldMapMatch tvMap con@(ConstructorInfo{constructorName = conName}) = do
  parts  <- foldDataConArgs tvMap ft_bifoldMap con
  parts' <- sequence parts
  match_for_con conName parts'
  where
    -- The Bool is True if the type mentions of the last two type parameters,
    -- False otherwise. Later, match_for_con uses mkSimpleConMatch2 to filter
    -- out expressions that do not mention the last parameters by checking for
    -- False.
    ft_bifoldMap :: FFoldType (Q (Bool, Exp))
    ft_bifoldMap = FT { -- See Note [ft_triv for Bifoldable and Bitraversable]
                        ft_triv = do lam <- mkSimpleLam $ \_ -> return $ VarE memptyValName
                                     return (False, lam)
                      , ft_var  = \v -> return (True, VarE $ tvMap Map.! v)
                      , ft_tup  = \t gs -> do
                          gg  <- sequence gs
                          lam <- mkSimpleLam $ mkSimpleTupleCase match_for_con t gg
                          return (True, lam)
                      , ft_ty_app = \gs -> do
                          e <- appsE $ varE (foldMapArity (length gs))
                                     : map (\(_, hs) -> fmap snd hs) gs
                          return (True, e)
                      , ft_forall  = \_ g -> g
                      , ft_co_var  = \_ -> contravarianceError conName
                      , ft_fun     = \_ _ -> noFunctionsError conName
                      , ft_bad_app = outOfPlaceTyVarError conName
                      }

    match_for_con :: Name -> [(Bool, Exp)] -> Q Match
    match_for_con = mkSimpleConMatch2 $ \_ xs -> return $ mkBifoldMap xs
      where
        -- mappend v1 (mappend v2 ..)
        mkBifoldMap :: [Exp] -> Exp
        mkBifoldMap [] = VarE memptyValName
        mkBifoldMap es = foldr1 (AppE . AppE (VarE mappendValName)) es

-- | Generates a match whose right-hand side implements @bitraverse@.
makeBitraverseMatch :: TyVarMap -> ConstructorInfo -> Q Match
makeBitraverseMatch tvMap con@(ConstructorInfo{constructorName = conName}) = do
  parts  <- foldDataConArgs tvMap ft_bitrav con
  parts' <- sequence parts
  match_for_con conName parts'
  where
    -- The Bool is True if the type mentions of the last two type parameters,
    -- False otherwise. Later, match_for_con uses mkSimpleConMatch2 to filter
    -- out expressions that do not mention the last parameters by checking for
    -- False.
    ft_bitrav :: FFoldType (Q (Bool, Exp))
    ft_bitrav = FT { -- See Note [ft_triv for Bifoldable and Bitraversable]
                     ft_triv = return (False, VarE pureValName)
                   , ft_var  = \v -> return (True, VarE $ tvMap Map.! v)
                   , ft_tup  = \t gs -> do
                       gg  <- sequence gs
                       lam <- mkSimpleLam $ mkSimpleTupleCase match_for_con t gg
                       return (True, lam)
                   , ft_ty_app = \gs -> do
                       e <- appsE $ varE (traverseArity (length gs))
                                  : map (\(_, hs) -> fmap snd hs) gs
                       return (True, e)
                   , ft_forall  = \_ g -> g
                   , ft_co_var  = \_ -> contravarianceError conName
                   , ft_fun     = \_ _ -> noFunctionsError conName
                   , ft_bad_app = outOfPlaceTyVarError conName
                   }

    -- Con a1 a2 ... -> liftA2 (\b1 b2 ... -> Con b1 b2 ...) (g1 a1)
    --                    (g2 a2) <*> ...
    match_for_con :: Name -> [(Bool, Exp)] -> Q Match
    match_for_con = mkSimpleConMatch2 $ \conExp xs -> return $ mkApCon conExp xs
      where
        -- liftA2 (\b1 b2 ... -> Con b1 b2 ...) x1 x2 <*> ..
        mkApCon :: Exp -> [Exp] -> Exp
        mkApCon conExp []  = VarE pureValName `AppE` conExp
        mkApCon conExp [e] = VarE fmapValName `AppE` conExp `AppE` e
        mkApCon conExp (e1:e2:es) = List.foldl' appAp
          (VarE liftA2ValName `AppE` conExp `AppE` e1 `AppE` e2) es
          where appAp se1 se2 = InfixE (Just se1) (VarE apValName) (Just se2)

-------------------------------------------------------------------------------
-- Template Haskell reifying and AST manipulation
-------------------------------------------------------------------------------

-- For the given Types, generate an instance context and head. Coming up with
-- the instance type isn't as simple as dropping the last types, as you need to
-- be wary of kinds being instantiated with *.
-- See Note [Type inference in derived instances]
buildTypeInstance :: BiClass
                  -- ^ Bifunctor, Bifoldable, or Bitraversable
                  -> Name
                  -- ^ The type constructor or data family name
                  -> Cxt
                  -- ^ The datatype context
                  -> [Type]
                  -- ^ The types to instantiate the instance with
                  -> DatatypeVariant
                  -- ^ Are we dealing with a data family instance or not
                  -> Q (Cxt, Type)
buildTypeInstance biClass tyConName dataCxt instTysOrig variant = do
    -- Make sure to expand through type/kind synonyms! Otherwise, the
    -- eta-reduction check might get tripped up over type variables in a
    -- synonym that are actually dropped.
    -- (See GHC Trac #11416 for a scenario where this actually happened.)
    varTysExp <- mapM resolveTypeSynonyms instTysOrig

    let remainingLength :: Int
        remainingLength = length instTysOrig - 2

        droppedTysExp :: [Type]
        droppedTysExp = drop remainingLength varTysExp

        droppedStarKindStati :: [StarKindStatus]
        droppedStarKindStati = map canRealizeKindStar droppedTysExp

    -- Check there are enough types to drop and that all of them are either of
    -- kind * or kind k (for some kind variable k). If not, throw an error.
    when (remainingLength < 0 || any (== NotKindStar) droppedStarKindStati) $
      derivingKindError biClass tyConName

    let droppedKindVarNames :: [Name]
        droppedKindVarNames = catKindVarNames droppedStarKindStati

        -- Substitute kind * for any dropped kind variables
        varTysExpSubst :: [Type]
        varTysExpSubst = map (substNamesWithKindStar droppedKindVarNames) varTysExp

        remainingTysExpSubst, droppedTysExpSubst :: [Type]
        (remainingTysExpSubst, droppedTysExpSubst) =
          splitAt remainingLength varTysExpSubst

        -- All of the type variables mentioned in the dropped types
        -- (post-synonym expansion)
        droppedTyVarNames :: [Name]
        droppedTyVarNames = freeVariables droppedTysExpSubst

    -- If any of the dropped types were polykinded, ensure that they are of kind *
    -- after substituting * for the dropped kind variables. If not, throw an error.
    unless (all hasKindStar droppedTysExpSubst) $
      derivingKindError biClass tyConName

    let preds    :: [Maybe Pred]
        kvNames  :: [[Name]]
        kvNames' :: [Name]
        -- Derive instance constraints (and any kind variables which are specialized
        -- to * in those constraints)
        (preds, kvNames) = unzip $ map (deriveConstraint biClass) remainingTysExpSubst
        kvNames' = concat kvNames

        -- Substitute the kind variables specialized in the constraints with *
        remainingTysExpSubst' :: [Type]
        remainingTysExpSubst' =
          map (substNamesWithKindStar kvNames') remainingTysExpSubst

        -- We now substitute all of the specialized-to-* kind variable names with
        -- *, but in the original types, not the synonym-expanded types. The reason
        -- we do this is a superficial one: we want the derived instance to resemble
        -- the datatype written in source code as closely as possible. For example,
        -- for the following data family instance:
        --
        --   data family Fam a
        --   newtype instance Fam String = Fam String
        --
        -- We'd want to generate the instance:
        --
        --   instance C (Fam String)
        --
        -- Not:
        --
        --   instance C (Fam [Char])
        remainingTysOrigSubst :: [Type]
        remainingTysOrigSubst =
          map (substNamesWithKindStar (List.union droppedKindVarNames kvNames'))
            $ take remainingLength instTysOrig

        isDataFamily :: Bool
        isDataFamily = case variant of
                         Datatype        -> False
                         Newtype         -> False
                         DataInstance    -> True
                         NewtypeInstance -> True

        remainingTysOrigSubst' :: [Type]
        -- See Note [Kind signatures in derived instances] for an explanation
        -- of the isDataFamily check.
        remainingTysOrigSubst' =
          if isDataFamily
             then remainingTysOrigSubst
             else map unSigT remainingTysOrigSubst

        instanceCxt :: Cxt
        instanceCxt = catMaybes preds

        instanceType :: Type
        instanceType = AppT (ConT $ biClassName biClass)
                     $ applyTyCon tyConName remainingTysOrigSubst'

    -- If the datatype context mentions any of the dropped type variables,
    -- we can't derive an instance, so throw an error.
    when (any (`predMentionsName` droppedTyVarNames) dataCxt) $
      datatypeContextError tyConName instanceType
    -- Also ensure the dropped types can be safely eta-reduced. Otherwise,
    -- throw an error.
    unless (canEtaReduce remainingTysExpSubst' droppedTysExpSubst) $
      etaReductionError instanceType
    return (instanceCxt, instanceType)

-- | Attempt to derive a constraint on a Type. If successful, return
-- Just the constraint and any kind variable names constrained to *.
-- Otherwise, return Nothing and the empty list.
--
-- See Note [Type inference in derived instances] for the heuristics used to
-- come up with constraints.
deriveConstraint :: BiClass -> Type -> (Maybe Pred, [Name])
deriveConstraint biClass t
  | not (isTyVar t) = (Nothing, [])
  | otherwise = case hasKindVarChain 1 t of
      Just ns -> ((`applyClass` tName) `fmap` biClassConstraint biClass 1, ns)
      _ -> case hasKindVarChain 2 t of
                Just ns -> ((`applyClass` tName) `fmap` biClassConstraint biClass 2, ns)
                _       -> (Nothing, [])
  where
    tName :: Name
    tName = varTToName t

{-
Note [Kind signatures in derived instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is possible to put explicit kind signatures into the derived instances, e.g.,

  instance C a => C (Data (f :: * -> *)) where ...

But it is preferable to avoid this if possible. If we come up with an incorrect
kind signature (which is entirely possible, since our type inferencer is pretty
unsophisticated - see Note [Type inference in derived instances]), then GHC will
flat-out reject the instance, which is quite unfortunate.

Plain old datatypes have the advantage that you can avoid using any kind signatures
at all in their instances. This is because a datatype declaration uses all type
variables, so the types that we use in a derived instance uniquely determine their
kinds. As long as we plug in the right types, the kind inferencer can do the rest
of the work. For this reason, we use unSigT to remove all kind signatures before
splicing in the instance context and head.

Data family instances are trickier, since a data family can have two instances that
are distinguished by kind alone, e.g.,

  data family Fam (a :: k)
  data instance Fam (a :: * -> *)
  data instance Fam (a :: *)

If we dropped the kind signatures for C (Fam a), then GHC will have no way of
knowing which instance we are talking about. To avoid this scenario, we always
include explicit kind signatures in data family instances. There is a chance that
the inferred kind signatures will be incorrect, but if so, we can always fall back
on the make- functions.

Note [Type inference in derived instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Type inference is can be tricky to get right, and we want to avoid recreating the
entirety of GHC's type inferencer in Template Haskell. For this reason, we will
probably never come up with derived instance contexts that are as accurate as
GHC's. But that doesn't mean we can't do anything! There are a couple of simple
things we can do to make instance contexts that work for 80% of use cases:

1. If one of the last type parameters is polykinded, then its kind will be
   specialized to * in the derived instance. We note what kind variable the type
   parameter had and substitute it with * in the other types as well. For example,
   imagine you had

     data Data (a :: k) (b :: k) (c :: k)

   Then you'd want to derived instance to be:

     instance C (Data (a :: *))

   Not:

     instance C (Data (a :: k))

2. We naïvely come up with instance constraints using the following criteria:

   (i)  If there's a type parameter n of kind k1 -> k2 (where k1/k2 are * or kind
        variables), then generate a Functor n constraint, and if k1/k2 are kind
        variables, then substitute k1/k2 with * elsewhere in the types. We must
        consider the case where they are kind variables because you might have a
        scenario like this:

          newtype Compose (f :: k3 -> *) (g :: k1 -> k2 -> k3) (a :: k1) (b :: k2)
            = Compose (f (g a b))

        Which would have a derived Bifunctor instance of:

          instance (Functor f, Bifunctor g) => Bifunctor (Compose f g) where ...
   (ii) If there's a type parameter n of kind k1 -> k2 -> k3 (where k1/k2/k3 are
        * or kind variables), then generate a Bifunctor n constraint and perform
        kind substitution as in the other case.
-}

{-
Note [Matching functions with GADT type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When deriving Bifoldable, there is a tricky corner case to consider:

  data Both a b where
    BothCon :: x -> x -> Both x x

Which fold functions should be applied to which arguments of BothCon? We have a
choice, since both the function of type (a -> m) and of type (b -> m) can be
applied to either argument. In such a scenario, the second fold function takes
precedence over the first fold function, so the derived Bifoldable instance would be:

  instance Bifoldable Both where
    bifoldMap _ g (BothCon x1 x2) = g x1 <> g x2

This is not an arbitrary choice, as this definition ensures that
bifoldMap id = Foldable.foldMap for a derived Bifoldable instance for Both.
-}

-------------------------------------------------------------------------------
-- Error messages
-------------------------------------------------------------------------------

-- | Either the given data type doesn't have enough type variables, or one of
-- the type variables to be eta-reduced cannot realize kind *.
derivingKindError :: BiClass -> Name -> Q a
derivingKindError biClass tyConName = fail
  . showString "Cannot derive well-kinded instance of form ‘"
  . showString className
  . showChar ' '
  . showParen True
    ( showString (nameBase tyConName)
    . showString " ..."
    )
  . showString "‘\n\tClass "
  . showString className
  . showString " expects an argument of kind * -> * -> *"
  $ ""
  where
    className :: String
    className = nameBase $ biClassName biClass

-- | One of the last two type variables appeard in a contravariant position
-- when deriving Bifoldable or Bitraversable.
contravarianceError :: Name -> Q a
contravarianceError conName = fail
  . showString "Constructor ‘"
  . showString (nameBase conName)
  . showString "‘ must not use the last type variable(s) in a function argument"
  $ ""

-- | A constructor has a function argument in a derived Bifoldable or Bitraversable
-- instance.
noFunctionsError :: Name -> Q a
noFunctionsError conName = fail
  . showString "Constructor ‘"
  . showString (nameBase conName)
  . showString "‘ must not contain function types"
  $ ""

-- | The data type has a DatatypeContext which mentions one of the eta-reduced
-- type variables.
datatypeContextError :: Name -> Type -> Q a
datatypeContextError dataName instanceType = fail
  . showString "Can't make a derived instance of ‘"
  . showString (pprint instanceType)
  . showString "‘:\n\tData type ‘"
  . showString (nameBase dataName)
  . showString "‘ must not have a class context involving the last type argument(s)"
  $ ""

-- | The data type has an existential constraint which mentions one of the
-- eta-reduced type variables.
existentialContextError :: Name -> Q a
existentialContextError conName = fail
  . showString "Constructor ‘"
  . showString (nameBase conName)
  . showString "‘ must be truly polymorphic in the last argument(s) of the data type"
  $ ""

-- | The data type mentions one of the n eta-reduced type variables in a place other
-- than the last nth positions of a data type in a constructor's field.
outOfPlaceTyVarError :: Name -> Q a
outOfPlaceTyVarError conName = fail
  . showString "Constructor ‘"
  . showString (nameBase conName)
  . showString "‘ must only use its last two type variable(s) within"
  . showString " the last two argument(s) of a data type"
  $ ""

-- | One of the last type variables cannot be eta-reduced (see the canEtaReduce
-- function for the criteria it would have to meet).
etaReductionError :: Type -> Q a
etaReductionError instanceType = fail $
  "Cannot eta-reduce to an instance of form \n\tinstance (...) => "
  ++ pprint instanceType

-------------------------------------------------------------------------------
-- Class-specific constants
-------------------------------------------------------------------------------

-- | A representation of which class is being derived.
data BiClass = Bifunctor | Bifoldable | Bitraversable

-- | A representation of which function is being generated.
data BiFun = Bimap | Bifoldr | BifoldMap | Bitraverse
  deriving Eq

biFunConstName :: BiFun -> Name
biFunConstName Bimap      = bimapConstValName
biFunConstName Bifoldr    = bifoldrConstValName
biFunConstName BifoldMap  = bifoldMapConstValName
biFunConstName Bitraverse = bitraverseConstValName

biClassName :: BiClass -> Name
biClassName Bifunctor     = bifunctorTypeName
biClassName Bifoldable    = bifoldableTypeName
biClassName Bitraversable = bitraversableTypeName

biFunName :: BiFun -> Name
biFunName Bimap      = bimapValName
biFunName Bifoldr    = bifoldrValName
biFunName BifoldMap  = bifoldMapValName
biFunName Bitraverse = bitraverseValName

biClassToFuns :: BiClass -> [BiFun]
biClassToFuns Bifunctor     = [Bimap]
biClassToFuns Bifoldable    = [Bifoldr, BifoldMap]
biClassToFuns Bitraversable = [Bitraverse]

biFunToClass :: BiFun -> BiClass
biFunToClass Bimap      = Bifunctor
biFunToClass Bifoldr    = Bifoldable
biFunToClass BifoldMap  = Bifoldable
biFunToClass Bitraverse = Bitraversable

biClassConstraint :: BiClass -> Int -> Maybe Name
biClassConstraint Bifunctor     1 = Just functorTypeName
biClassConstraint Bifoldable    1 = Just foldableTypeName
biClassConstraint Bitraversable 1 = Just traversableTypeName
biClassConstraint biClass       2 = Just $ biClassName biClass
biClassConstraint _             _ = Nothing

fmapArity :: Int -> Name
fmapArity 1 = fmapValName
fmapArity 2 = bimapValName
fmapArity n = arityErr n

foldrArity :: Int -> Name
foldrArity 1 = foldrValName
foldrArity 2 = bifoldrValName
foldrArity n = arityErr n

foldMapArity :: Int -> Name
foldMapArity 1 = foldMapValName
foldMapArity 2 = bifoldMapValName
foldMapArity n = arityErr n

traverseArity :: Int -> Name
traverseArity 1 = traverseValName
traverseArity 2 = bitraverseValName
traverseArity n = arityErr n

arityErr :: Int -> a
arityErr n = error $ "Unsupported arity: " ++ show n

allowExQuant :: BiClass -> Bool
allowExQuant Bifoldable = True
allowExQuant _          = False

biFunEmptyCase :: BiFun -> Name -> Name -> Q Exp
biFunEmptyCase biFun z value =
    biFunTrivial emptyCase
                 (varE pureValName `appE` emptyCase)
                 biFun z
  where
    emptyCase :: Q Exp
    emptyCase = caseE (varE value) []

biFunNoCons :: BiFun -> Name -> Name -> Q Exp
biFunNoCons biFun z value =
    biFunTrivial seqAndError
                 (varE pureValName `appE` seqAndError)
                 biFun z
  where
    seqAndError :: Q Exp
    seqAndError = appE (varE seqValName) (varE value) `appE`
                  appE (varE errorValName)
                        (stringE $ "Void " ++ nameBase (biFunName biFun))

biFunTrivial :: Q Exp -> Q Exp -> BiFun -> Name -> Q Exp
biFunTrivial bimapE bitraverseE biFun z = go biFun
  where
    go :: BiFun -> Q Exp
    go Bimap      = bimapE
    go Bifoldr    = varE z
    go BifoldMap  = varE memptyValName
    go Bitraverse = bitraverseE

{-
Note [ft_triv for Bifoldable and Bitraversable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When deriving Bifoldable and Bitraversable, we filter out any subexpressions whose
type does not mention one of the last two type parameters. From this, you might
think that we don't need to implement ft_triv for bifoldr, bifoldMap, or
bitraverse at all, but in fact we do need to. Imagine the following data type:

    data T a b = MkT a (T Int b)

In a derived Bifoldable T instance, you would generate the following bifoldMap
definition:

    bifoldMap f g (MkT a1 a2) = f a1 <> bifoldMap (\_ -> mempty) g arg2

You need to fill in bi_triv (\_ -> mempty) as the first argument to the recursive
call to bifoldMap, since that is how the algorithm handles polymorphic recursion.
-}

-------------------------------------------------------------------------------
-- Generic traversal for functor-like deriving
-------------------------------------------------------------------------------

-- Much of the code below is cargo-culted from the TcGenFunctor module in GHC.

data FFoldType a      -- Describes how to fold over a Type in a functor like way
   = FT { ft_triv    :: a
          -- ^ Does not contain variables
        , ft_var     :: Name -> a
          -- ^ A bare variable
        , ft_co_var  :: Name -> a
          -- ^ A bare variable, contravariantly
        , ft_fun     :: a -> a -> a
          -- ^ Function type
        , ft_tup     :: TupleSort -> [a] -> a
          -- ^ Tuple type. The [a] is the result of folding over the
          --   arguments of the tuple.
        , ft_ty_app  :: [(Type, a)] -> a
          -- ^ Type app, variables only in last argument. The [(Type, a)]
          --   represents the last argument types. That is, they form the
          --   argument parts of @fun_ty arg_ty_1 ... arg_ty_n@.
        , ft_bad_app :: a
          -- ^ Type app, variable other than in last arguments
        , ft_forall  :: [TyVarBndrSpec] -> a -> a
          -- ^ Forall type
     }

-- Note that in GHC, this function is pure. It must be monadic here since we:
--
-- (1) Expand type synonyms
-- (2) Detect type family applications
--
-- Which require reification in Template Haskell, but are pure in Core.
functorLikeTraverse :: forall a.
                       TyVarMap    -- ^ Variables to look for
                    -> FFoldType a -- ^ How to fold
                    -> Type        -- ^ Type to process
                    -> Q a
functorLikeTraverse tvMap (FT { ft_triv = caseTrivial,     ft_var = caseVar
                              , ft_co_var = caseCoVar,     ft_fun = caseFun
                              , ft_tup = caseTuple,        ft_ty_app = caseTyApp
                              , ft_bad_app = caseWrongArg, ft_forall = caseForAll })
                    ty
  = do ty' <- resolveTypeSynonyms ty
       (res, _) <- go False ty'
       return res
  where
    go :: Bool        -- Covariant or contravariant context
       -> Type
       -> Q (a, Bool) -- (result of type a, does type contain var)
    go co t@AppT{}
      | (ArrowT, [funArg, funRes]) <- unapplyTy t
      = do (funArgR, funArgC) <- go (not co) funArg
           (funResR, funResC) <- go      co  funRes
           if funArgC || funResC
              then return (caseFun funArgR funResR, True)
              else trivial
    go co t@AppT{} = do
      let (f, args) = unapplyTy t
      (_,   fc)  <- go co f
      (xrs, xcs) <- fmap unzip $ mapM (go co) args
      let numLastArgs, numFirstArgs :: Int
          numLastArgs  = min 2 $ length args
          numFirstArgs = length args - numLastArgs

          tuple :: TupleSort -> Q (a, Bool)
          tuple tupSort = return (caseTuple tupSort xrs, True)

          wrongArg :: Q (a, Bool)
          wrongArg = return (caseWrongArg, True)

      case () of
        _ |  not (or xcs)
          -> trivial -- Variable does not occur
          -- At this point we know that xrs, xcs is not empty,
          -- and at least one xr is True
          |  TupleT len <- f
          -> tuple $ Boxed len
#if MIN_VERSION_template_haskell(2,6,0)
          |  UnboxedTupleT len <- f
          -> tuple $ Unboxed len
#endif
          |  fc || or (take numFirstArgs xcs)
          -> wrongArg                    -- T (..var..)    ty_1 ... ty_n
          |  otherwise                   -- T (..no var..) ty_1 ... ty_n
          -> do itf <- isInTypeFamilyApp tyVarNames f args
                if itf -- We can't decompose type families, so
                       -- error if we encounter one here.
                   then wrongArg
                   else return ( caseTyApp $ drop numFirstArgs $ zip args xrs
                               , True )
    go co (SigT t k) = do
      (_, kc) <- go_kind co k
      if kc
         then return (caseWrongArg, True)
         else go co t
    go co (VarT v)
      | Map.member v tvMap
      = return (if co then caseCoVar v else caseVar v, True)
      | otherwise
      = trivial
    go co (ForallT tvbs _ t) = do
      (tr, tc) <- go co t
      let tvbNames = map tvName tvbs
      if not tc || any (`elem` tvbNames) tyVarNames
         then trivial
         else return (caseForAll tvbs tr, True)
    go _ _ = trivial

    go_kind :: Bool
            -> Kind
            -> Q (a, Bool)
#if MIN_VERSION_template_haskell(2,9,0)
    go_kind = go
#else
    go_kind _ _ = trivial
#endif

    trivial :: Q (a, Bool)
    trivial = return (caseTrivial, False)

    tyVarNames :: [Name]
    tyVarNames = Map.keys tvMap

-- Fold over the arguments of a data constructor in a Functor-like way.
foldDataConArgs :: forall a. TyVarMap -> FFoldType a -> ConstructorInfo -> Q [a]
foldDataConArgs tvMap ft con = do
  fieldTys <- mapM resolveTypeSynonyms $ constructorFields con
  mapM foldArg fieldTys
  where
    foldArg :: Type -> Q a
    foldArg = functorLikeTraverse tvMap ft

-- Make a 'LamE' using a fresh variable.
mkSimpleLam :: (Exp -> Q Exp) -> Q Exp
mkSimpleLam lam = do
  -- Use an underscore in front of the variable name, as it's possible for
  -- certain Bifoldable instances to generate code like this (see #89):
  --
  -- @
  -- bifoldMap (\\_n -> mempty) ...
  -- @
  --
  -- Without the underscore, that code would trigger -Wunused-matches warnings.
  n <- newName "_n"
  body <- lam (VarE n)
  return $ LamE [VarP n] body

-- Make a 'LamE' using two fresh variables.
mkSimpleLam2 :: (Exp -> Exp -> Q Exp) -> Q Exp
mkSimpleLam2 lam = do
  -- Use an underscore in front of the variable name, as it's possible for
  -- certain Bifoldable instances to generate code like this (see #89):
  --
  -- @
  -- bifoldr (\\_n1 n2 -> n2) ...
  -- @
  --
  -- Without the underscore, that code would trigger -Wunused-matches warnings.
  n1 <- newName "_n1"
  n2 <- newName "n2"
  body <- lam (VarE n1) (VarE n2)
  return $ LamE [VarP n1, VarP n2] body

-- "Con a1 a2 a3 -> fold [x1 a1, x2 a2, x3 a3]"
--
-- @mkSimpleConMatch fold conName insides@ produces a match clause in
-- which the LHS pattern-matches on @extraPats@, followed by a match on the
-- constructor @conName@ and its arguments. The RHS folds (with @fold@) over
-- @conName@ and its arguments, applying an expression (from @insides@) to each
-- of the respective arguments of @conName@.
mkSimpleConMatch :: (Name -> [a] -> Q Exp)
                 -> Name
                 -> [Exp -> a]
                 -> Q Match
mkSimpleConMatch fold conName insides = do
  varsNeeded <- newNameList "_arg" $ length insides
  let pat = conPCompat conName (map VarP varsNeeded)
  rhs <- fold conName (zipWith (\i v -> i $ VarE v) insides varsNeeded)
  return $ Match pat (NormalB rhs) []

-- "Con a1 a2 a3 -> fmap (\b2 -> Con a1 b2 a3) (traverse f a2)"
--
-- @mkSimpleConMatch2 fold conName insides@ behaves very similarly to
-- 'mkSimpleConMatch', with two key differences:
--
-- 1. @insides@ is a @[(Bool, Exp)]@ instead of a @[Exp]@. This is because it
--    filters out the expressions corresponding to arguments whose types do not
--    mention the last type variable in a derived 'Foldable' or 'Traversable'
--    instance (i.e., those elements of @insides@ containing @False@).
--
-- 2. @fold@ takes an expression as its first argument instead of a
--    constructor name. This is because it uses a specialized
--    constructor function expression that only takes as many parameters as
--    there are argument types that mention the last type variable.
mkSimpleConMatch2 :: (Exp -> [Exp] -> Q Exp)
                  -> Name
                  -> [(Bool, Exp)]
                  -> Q Match
mkSimpleConMatch2 fold conName insides = do
  varsNeeded <- newNameList "_arg" lengthInsides
  let pat = conPCompat conName (map VarP varsNeeded)
      -- Make sure to zip BEFORE invoking catMaybes. We want the variable
      -- indicies in each expression to match up with the argument indices
      -- in conExpr (defined below).
      exps = catMaybes $ zipWith (\(m, i) v -> if m then Just (i `AppE` VarE v)
                                                    else Nothing)
                                 insides varsNeeded
      -- An element of argTysTyVarInfo is True if the constructor argument
      -- with the same index has a type which mentions the last type
      -- variable.
      argTysTyVarInfo = map (\(m, _) -> m) insides
      (asWithTyVar, asWithoutTyVar) = partitionByList argTysTyVarInfo varsNeeded

      conExpQ
        | null asWithTyVar = appsE (conE conName:map varE asWithoutTyVar)
        | otherwise = do
            bs <- newNameList "b" lengthInsides
            let bs'  = filterByList  argTysTyVarInfo bs
                vars = filterByLists argTysTyVarInfo
                                     (map varE bs) (map varE varsNeeded)
            lamE (map varP bs') (appsE (conE conName:vars))

  conExp <- conExpQ
  rhs <- fold conExp exps
  return $ Match pat (NormalB rhs) []
  where
    lengthInsides = length insides

-- Indicates whether a tuple is boxed or unboxed, as well as its number of
-- arguments. For instance, (a, b) corresponds to @Boxed 2@, and (# a, b, c #)
-- corresponds to @Unboxed 3@.
data TupleSort
  = Boxed   Int
#if MIN_VERSION_template_haskell(2,6,0)
  | Unboxed Int
#endif

-- "case x of (a1,a2,a3) -> fold [x1 a1, x2 a2, x3 a3]"
mkSimpleTupleCase :: (Name -> [a] -> Q Match)
                  -> TupleSort -> [a] -> Exp -> Q Exp
mkSimpleTupleCase matchForCon tupSort insides x = do
  let tupDataName = case tupSort of
                      Boxed   len -> tupleDataName len
#if MIN_VERSION_template_haskell(2,6,0)
                      Unboxed len -> unboxedTupleDataName len
#endif
  m <- matchForCon tupDataName insides
  return $ CaseE x [m]

-- Adapt to the type of ConP changing in template-haskell-2.18.0.0.
conPCompat :: Name -> [Pat] -> Pat
conPCompat n pats = ConP n
#if MIN_VERSION_template_haskell(2,18,0)
                         []
#endif
                         pats
