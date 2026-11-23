{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers, PolyKinds, RankNTypes, ScopedTypeVariables, StandaloneKindSignatures,
             TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
-- | Apply a type lambda to a list of type constructors.
module TypeLambda
  ( L, Captured, Shadowed, WithError, Prefixes, ScopedRow, Mixed, Held, CaseShadow, Rebuild
  , Fields(..), CheckedRow(..), Map, Columns, Optional, withErrors, IndexedTag(..), IndexedCase, IndexedCaseInferred
  , withScopedErrors, taggedCell, RankedReader, rankedReader, rankedReaderAt
  , roadmap, partial, optionalColumns, sameColumnReader, prefixes, mixed, indexedFields
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (U)

-- | Apply a type function to each list element.
type Map :: (a %U -> b) -> [a] -> [b]
type family Map f xs where
  Map _ '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

-- | A schema assigns a result type to each field in a row.
type Columns :: Type -> [Type] -> [Type]
type family Columns schema fields

-- | A schema with one optional value in each field.
data Optional
type instance Columns Optional fields = Map (\a -> Maybe a) fields

-- | A heterogeneous row indexed by the types of its fields.
data Fields (xs :: [Type]) where
  End :: Fields '[]
  Field :: a -> Fields xs -> Fields (a ': xs)

-- | Store either an error or a value of the declared type in each cell.
data CheckedRow e (xs :: [Type]) = CheckedRow
  { checkedCells :: Fields (Map (\a -> Either e a) xs) }

-- | Give each field the same error type.
withErrors :: Proxy e -> Fields xs -> Fields (Map (\a -> Either e a) xs)
withErrors _ End = End
withErrors p (Field x xs) = Field (Right x) (withErrors p xs)

-- | Wrap each cell using the error type from the enclosing signature.
withScopedErrors :: forall (e :: Type) xs.
                    Proxy e -> Fields xs -> Fields (Map (\cell -> Either e cell) xs)
withScopedErrors witness = go
  where
    go :: Fields xs -> Fields (Map (\cell -> Either e cell) xs)
    go End = End
    go (Field x rest) = Field (Right x) (withScopedErrors witness rest)

-- | Tag a row cell with the index from the enclosing signature.
taggedCell :: forall k tag. Proxy (tag :: k) -> Bool -> Fields '[(Proxy tag, Bool)]
taggedCell proxy value = row
  where
    row :: Fields (Map (\cell -> (Proxy (tag :: k), cell)) '[Bool])
    row = Field (proxy, value) End

-- | Give a cell the table and column indices.
type RankedReader (k :: Type) (table :: k) =
  forall (column :: k). Proxy column ->
    Fields (Map (\k -> (Proxy table, Proxy column, k)) '[Char])

-- | Read a cell from a fixed table.
rankedReader :: RankedReader Bool 'True
rankedReader _ = Field (Proxy, Proxy, 'r') End

-- | Choose the table before supplying the column.
rankedReaderAt :: forall (k :: Type) (table :: k). Proxy table ->
  (forall (column :: k). Proxy column ->
    Fields (Map (\cell -> (Proxy table, Proxy column, cell)) '[Char]))
rankedReaderAt _ _ = Field (Proxy, Proxy, 's') End

-- | A column read can fail or return a missing cell.
type Column err a = Either err (Maybe a)

-- | Keep the same reader when the last argument is forwarded unchanged.
type ReadColumn err a = Column err a

-- | Compare the readers before choosing the cell type.
sameColumnReader :: ReadColumn String :~: Column String
sameColumnReader = Refl

-- | Apply one error type to both column readers.
optionalColumns :: Map (Column String) '[Int, Char]
                :~: '[Either String (Maybe Int), Either String (Maybe Char)]
optionalColumns = Refl

-- | Apply each type constructor to Int.
type L = Map (\f -> f Int) [Maybe, Either String, IO]

-- | Supply the error type now and the result type at the use site.
type WithError :: Type %U -> Type
type WithError = (\err result -> Either err result) String

-- | Capture the first argument of Either from the enclosing synonym.
type Captured (a :: Type) = Map (\b -> Either a b) '[Bool, Char]

-- | Keep the outer kind of a while the lambda binds its own q.
type Shadowed (q :: Type) (a :: q) = Map (\q -> Either q (Proxy a)) '[Char]

-- | Select a wrapper and capture the error type from the matching row.
type Prefixes :: Maybe Type -> [Type] -> [Type]
type family Prefixes prefix xs where
  Prefixes ('Just e) xs = Map (\a -> Either e a) xs
  Prefixes 'Nothing xs = Map (\e -> Maybe e) xs

-- | Capture the outer error type in the callback and its local abbreviation in the row.
type ScopedRow e xs =
  let wrap = \a -> Either e a
  in let e = Maybe e
     in Map (\a -> (e, wrap a)) xs

-- | Check both callbacks with the same list.
prefixes :: ( Prefixes ('Just String) '[Int] :~: '[Either String Int]
            , Prefixes 'Nothing '[Int] :~: '[Maybe Int] )
prefixes = (Refl, Refl)

-- | Check the full result of the roadmap example.
roadmap :: L :~: '[Maybe Int, Either String Int, IO Int]
roadmap = Refl

-- | Apply the partially applied lambda to Bool.
partial :: WithError Bool :~: Either String Bool
partial = Refl

-- | Match each element with a tail case on the lambda parameter.
type Mixed = Map (\b -> case b of { True -> Int; False -> Bool }) '[ 'True, 'False]

-- | A rigid scrutinee leaves the constructor match stuck.
type Held :: Bool -> Type
type Held b = (\tag -> case tag of { True -> Int; False -> Int }) b

-- | The Just field shadows a in that branch. Nothing uses the outer a.
type CaseShadow (a :: k) =
  Map (\b -> case b of { Just a -> Proxy a; Nothing -> Proxy a }) '[ 'Just 'True, 'Nothing]

-- | Check both alternatives in the mapped result.
mixed :: Mixed :~: '[Int, Bool]
mixed = Refl

-- | Preserve the entire scrutinee after matching its constructor.
type Rebuild :: forall k. Maybe k -> Type
type Rebuild m = (\tag -> case tag of { Nothing -> Proxy tag; Just _ -> Proxy tag }) m

-- | Each constructor fixes the index and the type of its field.
data IndexedTag k where
  BooleanTag :: Bool -> IndexedTag Bool
  OrderingTag :: Ordering -> IndexedTag Ordering

-- | Check both branches under the written polymorphic kind.
type IndexedCase =
  ((\tag -> case tag of { BooleanTag field -> Proxy field; OrderingTag field -> Proxy field })
    :: forall k. @U IndexedTag k %U -> Type)

-- | Keep the inferred visibility of the written kind binder.
type IndexedCaseInferred =
  ((\tag -> case tag of { BooleanTag field -> Proxy field; OrderingTag field -> Proxy field })
    :: forall {k}. @U IndexedTag k %U -> Type)

-- | Observe the actual field in each indexed branch.
indexedFields :: ( IndexedCase @Bool ('BooleanTag 'False) :~: Proxy 'False
                 , IndexedCase @Ordering ('OrderingTag 'GT) :~: Proxy 'GT )
indexedFields = (Refl, Refl)
