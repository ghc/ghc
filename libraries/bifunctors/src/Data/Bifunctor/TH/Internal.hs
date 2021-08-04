{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif

{-|
Module:      Data.Bifunctor.TH.Internal
Copyright:   (C) 2008-2016 Edward Kmett, (C) 2015-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Edward Kmett
Portability: Template Haskell

Template Haskell-related utilities.
-}
module Data.Bifunctor.TH.Internal where

import           Data.Foldable (foldr')
import qualified Data.List as List
import qualified Data.Map as Map (singleton)
import           Data.Map (Map)
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import           Data.Set (Set)

import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax

-- Ensure, beyond a shadow of a doubt, that the instances are in-scope
import           Data.Bifunctor ()
import           Data.Bifoldable ()
import           Data.Bitraversable ()

#ifndef CURRENT_PACKAGE_KEY
import           Data.Version (showVersion)
import           Paths_bifunctors (version)
#endif

-------------------------------------------------------------------------------
-- Expanding type synonyms
-------------------------------------------------------------------------------

applySubstitutionKind :: Map Name Kind -> Type -> Type
#if MIN_VERSION_template_haskell(2,8,0)
applySubstitutionKind = applySubstitution
#else
applySubstitutionKind _ t = t
#endif

substNameWithKind :: Name -> Kind -> Type -> Type
substNameWithKind n k = applySubstitutionKind (Map.singleton n k)

substNamesWithKindStar :: [Name] -> Type -> Type
substNamesWithKindStar ns t = foldr' (flip substNameWithKind starK) t ns

-------------------------------------------------------------------------------
-- Type-specialized const functions
-------------------------------------------------------------------------------

bimapConst :: p b d -> (a -> b) -> (c -> d) -> p a c -> p b d
bimapConst = const . const . const
{-# INLINE bimapConst #-}

bifoldrConst :: c -> (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
bifoldrConst = const . const . const . const
{-# INLINE bifoldrConst #-}

bifoldMapConst :: m -> (a -> m) -> (b -> m) -> p a b -> m
bifoldMapConst = const . const . const
{-# INLINE bifoldMapConst #-}

bitraverseConst :: f (t c d) -> (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
bitraverseConst = const . const . const
{-# INLINE bitraverseConst #-}

-------------------------------------------------------------------------------
-- StarKindStatus
-------------------------------------------------------------------------------

-- | Whether a type is not of kind *, is of kind *, or is a kind variable.
data StarKindStatus = NotKindStar
                    | KindStar
                    | IsKindVar Name
  deriving Eq

-- | Does a Type have kind * or k (for some kind variable k)?
canRealizeKindStar :: Type -> StarKindStatus
canRealizeKindStar t
  | hasKindStar t = KindStar
  | otherwise = case t of
#if MIN_VERSION_template_haskell(2,8,0)
                     SigT _ (VarT k) -> IsKindVar k
#endif
                     _               -> NotKindStar

-- | Returns 'Just' the kind variable 'Name' of a 'StarKindStatus' if it exists.
-- Otherwise, returns 'Nothing'.
starKindStatusToName :: StarKindStatus -> Maybe Name
starKindStatusToName (IsKindVar n) = Just n
starKindStatusToName _             = Nothing

-- | Concat together all of the StarKindStatuses that are IsKindVar and extract
-- the kind variables' Names out.
catKindVarNames :: [StarKindStatus] -> [Name]
catKindVarNames = mapMaybe starKindStatusToName

-------------------------------------------------------------------------------
-- Assorted utilities
-------------------------------------------------------------------------------

-- filterByList, filterByLists, and partitionByList taken from GHC (BSD3-licensed)

-- | 'filterByList' takes a list of Bools and a list of some elements and
-- filters out these elements for which the corresponding value in the list of
-- Bools is False. This function does not check whether the lists have equal
-- length.
filterByList :: [Bool] -> [a] -> [a]
filterByList (True:bs)  (x:xs) = x : filterByList bs xs
filterByList (False:bs) (_:xs) =     filterByList bs xs
filterByList _          _      = []

-- | 'filterByLists' takes a list of Bools and two lists as input, and
-- outputs a new list consisting of elements from the last two input lists. For
-- each Bool in the list, if it is 'True', then it takes an element from the
-- former list. If it is 'False', it takes an element from the latter list.
-- The elements taken correspond to the index of the Bool in its list.
-- For example:
--
-- @
-- filterByLists [True, False, True, False] \"abcd\" \"wxyz\" = \"axcz\"
-- @
--
-- This function does not check whether the lists have equal length.
filterByLists :: [Bool] -> [a] -> [a] -> [a]
filterByLists (True:bs)  (x:xs) (_:ys) = x : filterByLists bs xs ys
filterByLists (False:bs) (_:xs) (y:ys) = y : filterByLists bs xs ys
filterByLists _          _      _      = []

-- | 'partitionByList' takes a list of Bools and a list of some elements and
-- partitions the list according to the list of Bools. Elements corresponding
-- to 'True' go to the left; elements corresponding to 'False' go to the right.
-- For example, @partitionByList [True, False, True] [1,2,3] == ([1,3], [2])@
-- This function does not check whether the lists have equal
-- length.
partitionByList :: [Bool] -> [a] -> ([a], [a])
partitionByList = go [] []
  where
    go trues falses (True  : bs) (x : xs) = go (x:trues) falses bs xs
    go trues falses (False : bs) (x : xs) = go trues (x:falses) bs xs
    go trues falses _ _ = (reverse trues, reverse falses)

-- | Returns True if a Type has kind *.
hasKindStar :: Type -> Bool
hasKindStar VarT{}         = True
#if MIN_VERSION_template_haskell(2,8,0)
hasKindStar (SigT _ StarT) = True
#else
hasKindStar (SigT _ StarK) = True
#endif
hasKindStar _              = False

-- Returns True is a kind is equal to *, or if it is a kind variable.
isStarOrVar :: Kind -> Bool
#if MIN_VERSION_template_haskell(2,8,0)
isStarOrVar StarT  = True
isStarOrVar VarT{} = True
#else
isStarOrVar StarK  = True
#endif
isStarOrVar _      = False

-- | @hasKindVarChain n kind@ Checks if @kind@ is of the form
-- k_0 -> k_1 -> ... -> k_(n-1), where k0, k1, ..., and k_(n-1) can be * or
-- kind variables.
hasKindVarChain :: Int -> Type -> Maybe [Name]
hasKindVarChain kindArrows t =
  let uk = uncurryKind (tyKind t)
  in if (length uk - 1 == kindArrows) && all isStarOrVar uk
        then Just (freeVariables uk)
        else Nothing

-- | If a Type is a SigT, returns its kind signature. Otherwise, return *.
tyKind :: Type -> Kind
tyKind (SigT _ k) = k
tyKind _          = starK

-- | A mapping of type variable Names to their map function Names. For example, in a
-- Bifunctor declaration, a TyVarMap might look like (a ~> f, b ~> g), where
-- a and b are the last two type variables of the datatype, and f and g are the two
-- functions which map their respective type variables.
type TyVarMap = Map Name Name

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

unsnoc :: [a] -> Maybe ([a], a)
unsnoc []     = Nothing
unsnoc (x:xs) = case unsnoc xs of
                  Nothing    -> Just ([], x)
                  Just (a,b) -> Just (x:a, b)

-- | Generate a list of fresh names with a common prefix, and numbered suffixes.
newNameList :: String -> Int -> Q [Name]
newNameList prefix n = mapM (newName . (prefix ++) . show) [1..n]

-- | Applies a typeclass constraint to a type.
applyClass :: Name -> Name -> Pred
#if MIN_VERSION_template_haskell(2,10,0)
applyClass con t = AppT (ConT con) (VarT t)
#else
applyClass con t = ClassP con [VarT t]
#endif

-- | Checks to see if the last types in a data family instance can be safely eta-
-- reduced (i.e., dropped), given the other types. This checks for three conditions:
--
-- (1) All of the dropped types are type variables
-- (2) All of the dropped types are distinct
-- (3) None of the remaining types mention any of the dropped types
canEtaReduce :: [Type] -> [Type] -> Bool
canEtaReduce remaining dropped =
       all isTyVar dropped
    && allDistinct droppedNames -- Make sure not to pass something of type [Type], since Type
                                -- didn't have an Ord instance until template-haskell-2.10.0.0
    && not (any (`mentionsName` droppedNames) remaining)
  where
    droppedNames :: [Name]
    droppedNames = map varTToName dropped

-- | Extract Just the Name from a type variable. If the argument Type is not a
-- type variable, return Nothing.
varTToName_maybe :: Type -> Maybe Name
varTToName_maybe (VarT n)   = Just n
varTToName_maybe (SigT t _) = varTToName_maybe t
varTToName_maybe _          = Nothing

-- | Extract the Name from a type variable. If the argument Type is not a
-- type variable, throw an error.
varTToName :: Type -> Name
varTToName = fromMaybe (error "Not a type variable!") . varTToName_maybe

-- | Peel off a kind signature from a Type (if it has one).
unSigT :: Type -> Type
unSigT (SigT t _) = t
unSigT t          = t

-- | Is the given type a variable?
isTyVar :: Type -> Bool
isTyVar (VarT _)   = True
isTyVar (SigT t _) = isTyVar t
isTyVar _          = False

-- | Detect if a Name in a list of provided Names occurs as an argument to some
-- type family. This makes an effort to exclude /oversaturated/ arguments to
-- type families. For instance, if one declared the following type family:
--
-- @
-- type family F a :: Type -> Type
-- @
--
-- Then in the type @F a b@, we would consider @a@ to be an argument to @F@,
-- but not @b@.
isInTypeFamilyApp :: [Name] -> Type -> [Type] -> Q Bool
isInTypeFamilyApp names tyFun tyArgs =
  case tyFun of
    ConT tcName -> go tcName
    _           -> return False
  where
    go :: Name -> Q Bool
    go tcName = do
      info <- reify tcName
      case info of
#if MIN_VERSION_template_haskell(2,11,0)
        FamilyI (OpenTypeFamilyD (TypeFamilyHead _ bndrs _ _)) _
          -> withinFirstArgs bndrs
#elif MIN_VERSION_template_haskell(2,7,0)
        FamilyI (FamilyD TypeFam _ bndrs _) _
          -> withinFirstArgs bndrs
#else
        TyConI (FamilyD TypeFam _ bndrs _)
          -> withinFirstArgs bndrs
#endif

#if MIN_VERSION_template_haskell(2,11,0)
        FamilyI (ClosedTypeFamilyD (TypeFamilyHead _ bndrs _ _) _) _
          -> withinFirstArgs bndrs
#elif MIN_VERSION_template_haskell(2,9,0)
        FamilyI (ClosedTypeFamilyD _ bndrs _ _) _
          -> withinFirstArgs bndrs
#endif

        _ -> return False
      where
        withinFirstArgs :: [a] -> Q Bool
        withinFirstArgs bndrs =
          let firstArgs = take (length bndrs) tyArgs
              argFVs    = freeVariables firstArgs
          in return $ any (`elem` argFVs) names

-- | Are all of the items in a list (which have an ordering) distinct?
--
-- This uses Set (as opposed to nub) for better asymptotic time complexity.
allDistinct :: Ord a => [a] -> Bool
allDistinct = allDistinct' Set.empty
  where
    allDistinct' :: Ord a => Set a -> [a] -> Bool
    allDistinct' uniqs (x:xs)
        | x `Set.member` uniqs = False
        | otherwise            = allDistinct' (Set.insert x uniqs) xs
    allDistinct' _ _           = True

-- | Does the given type mention any of the Names in the list?
mentionsName :: Type -> [Name] -> Bool
mentionsName = go
  where
    go :: Type -> [Name] -> Bool
    go (AppT t1 t2) names = go t1 names || go t2 names
    go (SigT t _k)  names = go t names
#if MIN_VERSION_template_haskell(2,8,0)
                              || go _k names
#endif
    go (VarT n)     names = n `elem` names
    go _            _     = False

-- | Does an instance predicate mention any of the Names in the list?
predMentionsName :: Pred -> [Name] -> Bool
#if MIN_VERSION_template_haskell(2,10,0)
predMentionsName = mentionsName
#else
predMentionsName (ClassP n tys) names = n `elem` names || any (`mentionsName` names) tys
predMentionsName (EqualP t1 t2) names = mentionsName t1 names || mentionsName t2 names
#endif

-- | Construct a type via curried application.
applyTy :: Type -> [Type] -> Type
applyTy = List.foldl' AppT

-- | Fully applies a type constructor to its type variables.
applyTyCon :: Name -> [Type] -> Type
applyTyCon = applyTy . ConT

-- | Split an applied type into its individual components. For example, this:
--
-- @
-- Either Int Char
-- @
--
-- would split to this:
--
-- @
-- [Either, Int, Char]
-- @
unapplyTy :: Type -> (Type, [Type])
unapplyTy ty = go ty ty []
  where
    go :: Type -> Type -> [Type] -> (Type, [Type])
    go _      (AppT ty1 ty2)     args = go ty1 ty1 (ty2:args)
    go origTy (SigT ty' _)       args = go origTy ty' args
#if MIN_VERSION_template_haskell(2,11,0)
    go origTy (InfixT ty1 n ty2) args = go origTy (ConT n `AppT` ty1 `AppT` ty2) args
    go origTy (ParensT ty')      args = go origTy ty' args
#endif
    go origTy _                  args = (origTy, args)

-- | Split a type signature by the arrows on its spine. For example, this:
--
-- @
-- forall a b. (a ~ b) => (a -> b) -> Char -> ()
-- @
--
-- would split to this:
--
-- @
-- (a ~ b, [a -> b, Char, ()])
-- @
uncurryTy :: Type -> (Cxt, [Type])
uncurryTy (AppT (AppT ArrowT t1) t2) =
  let (ctxt, tys) = uncurryTy t2
  in (ctxt, t1:tys)
uncurryTy (SigT t _) = uncurryTy t
uncurryTy (ForallT _ ctxt t) =
  let (ctxt', tys) = uncurryTy t
  in (ctxt ++ ctxt', tys)
uncurryTy t = ([], [t])

-- | Like uncurryType, except on a kind level.
uncurryKind :: Kind -> [Kind]
#if MIN_VERSION_template_haskell(2,8,0)
uncurryKind = snd . uncurryTy
#else
uncurryKind (ArrowK k1 k2) = k1:uncurryKind k2
uncurryKind k              = [k]
#endif

-------------------------------------------------------------------------------
-- Manually quoted names
-------------------------------------------------------------------------------

-- By manually generating these names we avoid needing to use the
-- TemplateHaskell language extension when compiling the bifunctors library.
-- This allows the library to be used in stage1 cross-compilers.

bifunctorsPackageKey :: String
#ifdef CURRENT_PACKAGE_KEY
bifunctorsPackageKey = CURRENT_PACKAGE_KEY
#else
bifunctorsPackageKey = "bifunctors-" ++ showVersion version
#endif

mkBifunctorsName_tc :: String -> String -> Name
mkBifunctorsName_tc = mkNameG_tc bifunctorsPackageKey

mkBifunctorsName_v :: String -> String -> Name
mkBifunctorsName_v = mkNameG_v bifunctorsPackageKey

bimapConstValName :: Name
bimapConstValName = mkBifunctorsName_v "Data.Bifunctor.TH.Internal" "bimapConst"

bifoldrConstValName :: Name
bifoldrConstValName = mkBifunctorsName_v "Data.Bifunctor.TH.Internal" "bifoldrConst"

bifoldMapConstValName :: Name
bifoldMapConstValName = mkBifunctorsName_v "Data.Bifunctor.TH.Internal" "bifoldMapConst"

coerceValName :: Name
coerceValName = mkNameG_v "ghc-prim" "GHC.Prim" "coerce"

bitraverseConstValName :: Name
bitraverseConstValName = mkBifunctorsName_v "Data.Bifunctor.TH.Internal" "bitraverseConst"

wrapMonadDataName :: Name
wrapMonadDataName = mkNameG_d "base" "Control.Applicative" "WrapMonad"

functorTypeName :: Name
functorTypeName = mkNameG_tc "base" "GHC.Base" "Functor"

foldableTypeName :: Name
foldableTypeName = mkNameG_tc "base" "Data.Foldable" "Foldable"

traversableTypeName :: Name
traversableTypeName = mkNameG_tc "base" "Data.Traversable" "Traversable"

composeValName :: Name
composeValName = mkNameG_v "base" "GHC.Base" "."

idValName :: Name
idValName = mkNameG_v "base" "GHC.Base" "id"

errorValName :: Name
errorValName = mkNameG_v "base" "GHC.Err" "error"

flipValName :: Name
flipValName = mkNameG_v "base" "GHC.Base" "flip"

fmapValName :: Name
fmapValName = mkNameG_v "base" "GHC.Base" "fmap"

foldrValName :: Name
foldrValName = mkNameG_v "base" "Data.Foldable" "foldr"

foldMapValName :: Name
foldMapValName = mkNameG_v "base" "Data.Foldable" "foldMap"

seqValName :: Name
seqValName = mkNameG_v "ghc-prim" "GHC.Prim" "seq"

traverseValName :: Name
traverseValName = mkNameG_v "base" "Data.Traversable" "traverse"

unwrapMonadValName :: Name
unwrapMonadValName = mkNameG_v "base" "Control.Applicative" "unwrapMonad"

#if MIN_VERSION_base(4,8,0)
bifunctorTypeName :: Name
bifunctorTypeName = mkNameG_tc "base" "Data.Bifunctor" "Bifunctor"

bimapValName :: Name
bimapValName = mkNameG_v "base" "Data.Bifunctor" "bimap"

pureValName :: Name
pureValName = mkNameG_v "base" "GHC.Base" "pure"

apValName :: Name
apValName = mkNameG_v "base" "GHC.Base" "<*>"

liftA2ValName :: Name
liftA2ValName = mkNameG_v "base" "GHC.Base" "liftA2"

mappendValName :: Name
mappendValName = mkNameG_v "base" "GHC.Base" "mappend"

memptyValName :: Name
memptyValName = mkNameG_v "base" "GHC.Base" "mempty"
#else
bifunctorTypeName :: Name
bifunctorTypeName = mkBifunctorsName_tc "Data.Bifunctor" "Bifunctor"

bimapValName :: Name
bimapValName = mkBifunctorsName_v "Data.Bifunctor" "bimap"

pureValName :: Name
pureValName = mkNameG_v "base" "Control.Applicative" "pure"

apValName :: Name
apValName = mkNameG_v "base" "Control.Applicative" "<*>"

liftA2ValName :: Name
liftA2ValName = mkNameG_v "base" "Control.Applicative" "liftA2"

mappendValName :: Name
mappendValName = mkNameG_v "base" "Data.Monoid" "mappend"

memptyValName :: Name
memptyValName = mkNameG_v "base" "Data.Monoid" "mempty"
#endif

#if MIN_VERSION_base(4,10,0)
bifoldableTypeName :: Name
bifoldableTypeName = mkNameG_tc "base" "Data.Bifoldable" "Bifoldable"

bitraversableTypeName :: Name
bitraversableTypeName = mkNameG_tc "base" "Data.Bitraversable" "Bitraversable"

bifoldrValName :: Name
bifoldrValName = mkNameG_v "base" "Data.Bifoldable" "bifoldr"

bifoldMapValName :: Name
bifoldMapValName = mkNameG_v "base" "Data.Bifoldable" "bifoldMap"

bitraverseValName :: Name
bitraverseValName = mkNameG_v "base" "Data.Bitraversable" "bitraverse"
#else
bifoldableTypeName :: Name
bifoldableTypeName = mkBifunctorsName_tc "Data.Bifoldable" "Bifoldable"

bitraversableTypeName :: Name
bitraversableTypeName = mkBifunctorsName_tc "Data.Bitraversable" "Bitraversable"

bifoldrValName :: Name
bifoldrValName = mkBifunctorsName_v "Data.Bifoldable" "bifoldr"

bifoldMapValName :: Name
bifoldMapValName = mkBifunctorsName_v "Data.Bifoldable" "bifoldMap"

bitraverseValName :: Name
bitraverseValName = mkBifunctorsName_v "Data.Bitraversable" "bitraverse"
#endif

#if MIN_VERSION_base(4,11,0)
appEndoValName :: Name
appEndoValName = mkNameG_v "base" "Data.Semigroup.Internal" "appEndo"

dualDataName :: Name
dualDataName = mkNameG_d "base" "Data.Semigroup.Internal" "Dual"

endoDataName :: Name
endoDataName = mkNameG_d "base" "Data.Semigroup.Internal" "Endo"

getDualValName :: Name
getDualValName = mkNameG_v "base" "Data.Semigroup.Internal" "getDual"
#else
appEndoValName :: Name
appEndoValName = mkNameG_v "base" "Data.Monoid" "appEndo"

dualDataName :: Name
dualDataName = mkNameG_d "base" "Data.Monoid" "Dual"

endoDataName :: Name
endoDataName = mkNameG_d "base" "Data.Monoid" "Endo"

getDualValName :: Name
getDualValName = mkNameG_v "base" "Data.Monoid" "getDual"
#endif
