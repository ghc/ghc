{- Copyright (c) 2016 Richard Eisenberg
 -}

{-# LANGUAGE TypeOperators, TypeFamilies, TypeApplications, AllowAmbiguousTypes,
             ExplicitForAll, ScopedTypeVariables, GADTs, TypeFamilyDependencies,
             TypeInType, ConstraintKinds, UndecidableInstances,
             FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies,
             FlexibleContexts, StandaloneDeriving, InstanceSigs,
             RankNTypes, UndecidableSuperClasses #-}

module RaeJobTalk where

import Data.Type.Bool
import Data.Type.Equality hiding ((:~~:)(..))
import GHC.TypeLits
import Data.Proxy
import GHC.Exts
import Data.Kind
import Unsafe.Coerce
import Data.Char
import Data.Maybe

-------------------------------
-- Utilities

-- Heterogeneous propositional equality
data (a :: k1) :~~: (b :: k2) where
  HRefl :: a :~~: a

-- Type-level inequality
type a /= b = Not (a == b)

-- append type-level lists (schemas)
type family s1 ++ s2 where
  '[]       ++ s2 = s2
  (s ': s1) ++ s2 = s ': (s1 ++ s2)

-- This is needed in order to prove disjointness, because GHC can't
-- handle inequality well.
assumeEquality :: forall a b r. ((a ~ b) => r) -> r
assumeEquality thing = case unsafeCoerce Refl :: a :~: b of
  Refl -> thing

-- Shorter name for shorter example
eq :: TestEquality f => f a -> f b -> Maybe (a :~: b)
eq = testEquality

-------------------------------
-- Singleton lists

-- Unlike in the singletons paper, we now have injective type
-- families, so we use that to model singletons; it's a bit
-- cleaner than the original approach
type family Sing = (r :: k -> Type) | r -> k

-- Cute type synonym.
type Π = Sing

-- Really, just singleton lists. Named Schema for better output
-- during example.
data Schema :: forall k. [k] -> Type where
  Nil :: Schema '[]
  (:>>) :: Sing h -> Schema t -> Schema (h ': t)
infixr 5 :>>
type instance Sing = Schema

-- Append singleton lists
(%:++) :: Schema a -> Schema b -> Schema (a ++ b)
Nil %:++ x = x
(a :>> b) %:++ c = a :>> (b %:++ c)

--------------------------------
-- Type-indexed type representations
-- Based on "A reflection on types"

data TyCon (a :: k) where
  Int :: TyCon Int
  Bool :: TyCon Bool
  Char :: TyCon Char
  List :: TyCon []
  Maybe :: TyCon Maybe
  Arrow :: TyCon (->)
  TYPE  :: TyCon TYPE
  RuntimeRep :: TyCon RuntimeRep
  LiftedRep' :: TyCon 'LiftedRep
  -- If extending, add to eqTyCon too

eqTyCon :: TyCon a -> TyCon b -> Maybe (a :~~: b)
eqTyCon Int Int = Just HRefl
eqTyCon Bool Bool = Just HRefl
eqTyCon Char Char = Just HRefl
eqTyCon List List = Just HRefl
eqTyCon Maybe Maybe = Just HRefl
eqTyCon Arrow Arrow = Just HRefl
eqTyCon TYPE TYPE = Just HRefl
eqTyCon RuntimeRep RuntimeRep = Just HRefl
eqTyCon LiftedRep' LiftedRep' = Just HRefl
eqTyCon _ _ = Nothing

-- Check whether or not a type is really a plain old tycon;
-- necessary to avoid warning in kindRep
type family Primitive (a :: k) :: Constraint where
  Primitive (_ _) = ('False ~ 'True)
  Primitive _     = (() :: Constraint)

data TypeRep (a :: k) where
  TyCon :: forall (a :: k). (Primitive a, Typeable k) => TyCon a -> TypeRep a
  TyApp :: TypeRep a -> TypeRep b -> TypeRep (a b)

-- Equality on TypeReps
eqT :: TypeRep a -> TypeRep b -> Maybe (a :~~: b)
eqT (TyCon tc1) (TyCon tc2) = eqTyCon tc1 tc2
eqT (TyApp f1 a1) (TyApp f2 a2) = do
  HRefl <- eqT f1 f2
  HRefl <- eqT a1 a2
  return HRefl
eqT _ _ = Nothing


--------------------------------------
-- Existentials

data TyConX where
  TyConX :: forall (a :: k). (Primitive a, Typeable k) => TyCon a -> TyConX

instance Read TyConX where
  readsPrec _ "Int" = [(TyConX Int, "")]
  readsPrec _ "Bool" = [(TyConX Bool, "")]
  readsPrec _ "List" = [(TyConX List, "")]
  readsPrec _ _ = []

-- This variant of SomeTypeRep allows you to specify an arbitrary
-- constraint on the inner TypeRep
data SomeTypeRep :: (forall k. k -> Constraint) -> Type where
  SomeTypeRep :: forall k (c :: forall k'. k' -> Constraint) (a :: k).
              c a => TypeRep a -> SomeTypeRep c

-- This constraint is always satisfied
class ConstTrue (a :: k) -- needs the :: k to make it a specified tyvar
instance ConstTrue a

instance Show (SomeTypeRep ConstTrue) where
  show (SomeTypeRep tr) = show tr

-- can't write Show (SomeTypeRep c) because c's kind mentions a forall,
-- and the impredicativity check gets nervous. See #11519
instance Show (SomeTypeRep IsType) where
  show (SomeTypeRep tr) = show tr

-- Just enough functionality to get through example. No parentheses
-- or other niceties.
instance Read (SomeTypeRep ConstTrue) where
  readsPrec p s = do
    let tokens = words s
    tyreps <- mapM read_token tokens
    return (foldl1 mk_app tyreps, "")

    where
      read_token :: String -> [SomeTypeRep ConstTrue]
      read_token "String" = return (SomeTypeRep $ typeRep @String)
      read_token other = do
        (TyConX tc, _) <- readsPrec p other
        return (SomeTypeRep (TyCon tc))

      mk_app :: SomeTypeRep ConstTrue -> SomeTypeRep ConstTrue -> SomeTypeRep ConstTrue
      mk_app (SomeTypeRep f) (SomeTypeRep a) = case kindRep f of
        TyCon Arrow `TyApp` k1 `TyApp` _
          | Just HRefl <- k1 `eqT` kindRep a -> SomeTypeRep (TyApp f a)
        _ -> error "ill-kinded type"

-- instance Read (SomeTypeRep ((~~) Type))  RAE: need (~~) :: forall k1. k1 -> forall k2. k2 -> Constraint
-- RAE: need kind signatures on classes

-- SomeTypeRep ((~~) Type)
-- (~~) :: forall k1 k2. k1 -> k2 -> Constraint
-- I need: (~~) :: forall k1. k1 -> forall k2. k2 -> Constraint

class k ~~ Type => IsType (x :: k)
instance k ~~ Type => IsType (x :: k)

instance Read (SomeTypeRep IsType) where
  readsPrec p s = case readsPrec @(SomeTypeRep ConstTrue) p s of
    [(SomeTypeRep tr, "")]
      | Just HRefl <- eqT (kindRep tr) (typeRep @Type)
      -> [(SomeTypeRep tr, "")]
    _ -> error "wrong kind"

-----------------------------
-- Get the kind of a type

kindRep :: TypeRep (a :: k) -> TypeRep k
kindRep (TyCon _) = typeRep
kindRep (TyApp (f :: TypeRep (tf :: k1 -> k)) _) = case kindRep f :: TypeRep (k1 -> k) of
  TyApp _ res -> res

-- Convert an explicit TypeRep into an implicit one. Doesn't require unsafeCoerce
-- in Core
withTypeable :: forall a r. TypeRep a -> (Typeable a => r) -> r
withTypeable tr thing = unsafeCoerce (Don'tInstantiate thing :: DI a r) tr
newtype DI a r = Don'tInstantiate (Typeable a => r)

-----------------------------
-- Implicit TypeReps (Typeable)

class (Primitive a, Typeable k) => TyConAble (a :: k) where
  tyCon :: TyCon a

instance TyConAble Int       where tyCon = Int
instance TyConAble Bool      where tyCon = Bool
instance TyConAble Char      where tyCon = Char
instance TyConAble []        where tyCon = List
instance TyConAble Maybe     where tyCon = Maybe
instance TyConAble (->)      where tyCon = Arrow
instance TyConAble TYPE      where tyCon = TYPE
instance TyConAble 'LiftedRep   where tyCon = LiftedRep'
instance TyConAble RuntimeRep    where tyCon = RuntimeRep

-- Can't just define Typeable the way we want, because the instances
-- overlap. So we have to mock up instance chains via closed type families.
class Typeable' a (b :: Bool) where
  typeRep' :: TypeRep a

type family CheckPrim a where
  CheckPrim (_ _) = 'False
  CheckPrim _     = 'True

-- NB: We need the ::k and the ::Constraint so that this has a CUSK, allowing
-- the polymorphic recursion with TypeRep. See also #9200, though the requirement
-- for the constraints is correct.
type Typeable (a :: k) = (Typeable' a (CheckPrim a) :: Constraint)

instance TyConAble a => Typeable' a 'True where
  typeRep' = TyCon tyCon

instance (Typeable a, Typeable b) => Typeable' (a b) 'False where
  typeRep' = TyApp typeRep typeRep

typeRep :: forall a. Typeable a => TypeRep a
typeRep = typeRep' @_ @(CheckPrim a)

-----------------------------
-- Useful instances

instance Show (TypeRep a) where
  show (TyCon tc) = show tc
  show (TyApp tr1 tr2) = show tr1 ++ " " ++ show tr2

deriving instance Show (TyCon a)

instance TestEquality TypeRep where
  testEquality tr1 tr2
    | Just HRefl <- eqT tr1 tr2
    = Just Refl
    | otherwise
    = Nothing

---------------------------
-- More singletons

-- a TypeRep really is a singleton
type instance Sing = (TypeRep :: Type -> Type)

data SSymbol :: Symbol -> Type where
  SSym :: KnownSymbol s => SSymbol s
type instance Sing = SSymbol

instance TestEquality SSymbol where
  testEquality :: forall s1 s2. SSymbol s1 -> SSymbol s2 -> Maybe (s1 :~: s2)
  testEquality SSym SSym = sameSymbol @s1 @s2 Proxy Proxy

instance Show (SSymbol name) where
  show s@SSym = symbolVal s


--------------
-- Finds Read and Show instances for the example
getReadShowInstances :: TypeRep a
                     -> ((Show a, Read a) => r)
                     -> r
getReadShowInstances tr thing
  | Just HRefl <- eqT tr (typeRep @Int) = thing
  | Just HRefl <- eqT tr (typeRep @Bool) = thing
  | Just HRefl <- eqT tr (typeRep @Char) = thing

  | TyApp list_rep elt_rep <- tr
  , Just HRefl <- eqT list_rep (typeRep @[])
  = getReadShowInstances elt_rep $ thing

  | otherwise = error $ "I don't know how to read or show " ++ show tr

-------------------------
-- A named column in our database
data Column = Attr Symbol Type
type Col = 'Attr

-- Singleton for columns
data SColumn :: Column -> Type where
  Col :: Sing s -> TypeRep ty -> SColumn (Col s ty)
type instance Sing = SColumn

-- Extract the type of a column
type family ColType col where
  ColType (Col _ ty) = ty

-- A schema is an ordered list of named attributes
type TSchema = [Column]

-- predicate to check that a schema is free of a certain attribute
type family ColNotIn name s where
  ColNotIn _    '[]                    = 'True
  ColNotIn name ((Col name' _) ': t) =
    (name /= name') && (ColNotIn name t)

-- predicate to check that two schemas are disjoint
type family Disjoint s1 s2 where
  Disjoint '[]      _ = 'True
  Disjoint ((Col name ty) ': t) s = (ColNotIn name s) && (Disjoint t s)

-- A Row is one row of our database table, keyed by its schema.
data Row :: TSchema -> Type where
  EmptyRow :: Row '[]
  (::>) :: ColType col -> Row s -> Row (col ': s)
infixr 5 ::>

-- Map a predicate over all the types in a schema
type family AllSchemaTys c sch where
  AllSchemaTys _ '[] = (() :: Constraint)
  AllSchemaTys c (col ': cols) = (c (ColType col), AllSchemaTys c cols)

-- Convenient abbreviations for being to print and parse the types
-- in a schema
type ShowSchema s = AllSchemaTys Show s
type ReadSchema s = AllSchemaTys Read s

-- We can easily print out rows, as long as the data are printable
instance ShowSchema s => Show (Row s) where
  show EmptyRow  = ""
  show (h ::> t) = show h ++ " " ++ show t

-- In our simplistic case, we just store the list of rows. A more
-- sophisticated implementation could store some identifier to the connection
-- to an external database.
data Table :: TSchema -> Type where
  Table :: [Row s] -> Table s

instance ShowSchema s => Show (Table s) where
  show (Table rows) = unlines (map show rows)

-- The following functions parse our very simple flat file database format.

-- The file, with a name ending in ".table", consists of a sequence of lines,
-- where each line contains one entry in the table. There is no row separator;
-- if a row contains n pieces of data, that row is represented in n lines in
-- the file.

-- A schema is stored in a file ending in ".schema".
-- Each line is a column name followed by its type.

-- Read a row of a table
readRow :: ReadSchema s => Schema s -> [String] -> (Row s, [String])
readRow Nil             strs    = (EmptyRow, strs)
readRow (_ :>> _)       []      = error "Ran out of data while processing row"
readRow (_ :>> schTail) (sh:st) = (read sh ::> rowTail, strTail)
  where (rowTail, strTail) = readRow schTail st

-- Read in a table
readRows :: ReadSchema s => Schema s -> [String] -> [Row s]
readRows _   []  = []
readRows sch lst = (row : tail)
  where (row, strTail) = readRow  sch lst
        tail           = readRows sch strTail

-- Read in one line of a .schema file. Note that the type read must have kind *
readCol :: String -> (String, SomeTypeRep IsType)
readCol str = case break isSpace str of
  (name, ' ' : ty) -> (name, read ty)
  _                -> schemaError $ "Bad parse of " ++ str

-- Load in a schema.
withSchema :: String
           -> (forall (s :: TSchema). Schema s -> IO a)
           -> IO a
withSchema filename thing_inside = do
  schString <- readFile filename
  let schEntries = lines schString
      cols       = map readCol schEntries
  go cols thing_inside
  where
    go :: [(String, SomeTypeRep IsType)]
       -> (forall (s :: TSchema). Schema s -> IO a)
       -> IO a
    go []                           thing = thing Nil
    go ((name, SomeTypeRep tr) : cols) thing
      = go cols $ \schema ->
        case someSymbolVal name of
          SomeSymbol (_ :: Proxy name) ->
            thing (Col (SSym @name) tr :>> schema)

-- Load in a table of a given schema
loadTable :: ReadSchema s => String -> Schema s -> IO (Table s)
loadTable name schema = do
  dataString <- readFile name
  return (Table $ readRows schema (lines dataString))

-- In order to define strongly-typed projection from a row, we need to have a notion
-- that one schema is a subset of another. We permit the schemas to have their columns
-- in different orders. We define this subset relation via two inductively defined
-- propositions. In Haskell, these inductively defined propositions take the form of
-- GADTs. In their original form, they would look like this:
{-
data InProof :: Column -> Schema -> * where
  InHere  :: InProof col (col ': schTail)
  InThere :: InProof col cols -> InProof col (a ': cols)

data SubsetProof :: Schema -> Schema -> * where
  SubsetEmpty :: SubsetProof '[] s'
  SubsetCons  :: InProof col s' -> SubsetProof cols s'
              -> SubsetProof (col ': cols) s'
-}
-- However, it would be convenient to users of the database library not to require
-- building these proofs manually. So, we define type classes so that the compiler
-- builds the proofs automatically. To make everything work well together, we also
-- make the parameters to the proof GADT constructors implicit -- i.e. in the form
-- of type class constraints.

data InProof :: Column -> TSchema -> Type where
  InHere  :: InProof col (col ': schTail)
  InThere :: In name u cols => InProof (Col name u) (a ': cols)

class In (name :: Symbol) (u :: Type) (sch :: TSchema) where
  inProof :: InProof (Col name u) sch

-- These instances must be INCOHERENT because they overlap badly. The coherence
-- derives from the fact that one schema will mention a name only once, but this
-- is beyond our capabilities to easily encode, given the lack of a solver for
-- type-level finite maps.
instance {-# INCOHERENT #-} In name u ((Col name u) ': schTail) where
  inProof = InHere
instance {-# INCOHERENT #-} In name u cols => In name u (a ': cols) where
  inProof = InThere

data SubsetProof :: TSchema -> TSchema -> Type where
  SubsetEmpty :: SubsetProof '[] s'
  SubsetCons :: (In name u s', Subset cols s')
             => Proxy name -> Proxy u -> SubsetProof ((Col name u) ': cols) s'

class SubsetSupport s s' => Subset (s :: TSchema) (s' :: TSchema) where
  subset :: SubsetProof s s'

  -- The use of this constraint family allows us to assume a subset relationship
  -- when we recur on the structure of s.
  type SubsetSupport s s' :: Constraint

instance Subset '[] s' where
  subset = SubsetEmpty
  type SubsetSupport '[] s' = ()

instance (In name u s', Subset cols s') =>
           Subset ((Col name u) ': cols) s' where
  subset = SubsetCons Proxy Proxy
  type SubsetSupport ((Col name u) ': cols) s' = Subset cols s'

-- To access the data in a structured (and well-typed!) way, we use
-- an RA (short for Relational Algebra). An RA is indexed by the schema
-- of the data it produces.

data RA :: TSchema -> Type where
  -- The RA includes all data represented by the handle.
  Read :: Table s -> RA s

  -- The RA is a Cartesian product of the two RAs provided. Note that
  -- the schemas of the two provided RAs must be disjoint.
  Product :: (Disjoint s s' ~ 'True) => RA s -> RA s' -> RA (s ++ s')

  -- The RA is a projection conforming to the schema provided. The
  -- type-checker ensures that this schema is a subset of the data
  -- included in the provided RA.
  Project :: Subset s' s => RA s -> RA s'

  -- The RA contains only those rows of the provided RA for which
  -- the provided expression evaluates to True. Note that the
  -- schema of the provided RA and the resultant RA are the same
  -- because the columns of data are the same. Also note that
  -- the expression must return a Bool for this to type-check.
  Select :: Expr s Bool -> RA s -> RA s

-- Other constructors would be added in a more robust database
-- implementation.

-- An Expr is used with the Select constructor to choose some
-- subset of rows from a table. Expressions are indexed by the
-- schema over which they operate and the return value they
-- produce.
data Expr :: TSchema -> Type -> Type where
  (:+), (:-), (:*), (:/) :: Expr s Int -> Expr s Int -> Expr s Int

  (:<), (:<=), (:>), (:>=), (:==), (:/=)
    :: Ord a => Expr s a -> Expr s a -> Expr s Bool

  -- A literal
  Literal :: ty -> Expr s ty

  -- element of a list
  ElementOf :: Eq ty => Expr s ty -> Expr s [ty] -> Expr s Bool

  -- Projection in an expression -- evaluates to the value
  -- of the named column.
  Element :: In name ty s => Proxy name -> Expr s ty

-- Choose the elements of one list based on truth values in another
choose :: [Bool] -> [a] -> [a]
choose bs as = [ a | (a,True) <- zip as bs ]

-- Project a component of one row, assuming that the desired projection
-- is valid.
projectRow :: forall sub super.
              Subset sub super => Row super -> Row sub
projectRow r = case subset @sub @super of
  SubsetEmpty -> EmptyRow
  SubsetCons (_ :: Proxy name) (_ :: Proxy ty) ->
      find_datum inProof r ::> projectRow r
    where
      find_datum :: InProof (Col name ty) s -> Row s -> ty
      find_datum InHere  (h ::> _) = h
      find_datum InThere (_ ::> t) = find_datum inProof t

-- Evaluate an Expr
eval :: Expr s ty -> Row s -> ty
eval (a :+ b)  r = eval a r +  eval b r
eval (a :- b)  r = eval a r -  eval b r
eval (a :* b)  r = eval a r *  eval b r
eval (a :/ b)  r = eval a r `div` eval b r
eval (a :< b)  r = eval a r <  eval b r
eval (a :<= b) r = eval a r <= eval b r
eval (a :> b)  r = eval a r >  eval b r
eval (a :>= b) r = eval a r >= eval b r
eval (a :== b) r = eval a r == eval b r
eval (a :/= b) r = eval a r /= eval b r
eval (Literal n)                 _ = n
eval (ElementOf elt list)        r = eval elt r `elem` eval list r
eval (Element (_ :: Proxy name)) r = get_element inProof r
  where
    get_element :: InProof (Col name ty) s -> Row s -> ty
    get_element InHere (elt ::> _) = elt
    get_element InThere (_ ::> tail) = get_element inProof tail

-- Append two rows. Needed for Cartesian product.
rowAppend :: Row s -> Row s' -> Row (s ++ s')
rowAppend EmptyRow  r = r
rowAppend (h ::> t) r = h ::> rowAppend t r

-- The query function is the eliminator for an RA. It returns a list of
-- rows containing the data produced by the RA. In the IO monad only
-- because more sophisticated implementations would actually go out to
-- a DB server for this.
query :: RA s -> IO [Row s]
query (Read (Table rows)) = return rows
query (Product ra rb) = do
  rowsa <- query ra
  rowsb <- query rb
  return [ rowAppend rowa rowb | rowa <- rowsa, rowb <- rowsb ]
query (Project ra)     = map projectRow <$> query ra
query (Select expr ra) = filter (eval expr) <$> query ra

field :: forall name ty s. In name ty s => Expr s ty
field = Element (Proxy :: Proxy name)








-- A schema is a list of columns, with
-- data Column = Col String Type
-- NB: Dependent type support is still experimental
checkIn :: Π name -> Π ty -> Π schema
        -> (In name ty schema => r)
        -> r
checkIn name _  Nil                        _
  = schemaError ("Field " ++ show name ++ " not found.")
checkIn name ty ((Col name' ty') :>> rest) callback
  = case (name `eq` name', ty `eq` ty') of
      (Just Refl, Just Refl) -> callback
      (Just _,    _)         -> schemaError ("Found " ++ show name ++
                                             " but it maps to " ++ show ty')
      _                      -> checkIn name ty rest callback

-- example call:
-- checkIn "id" Int schema ({- access "id" and assume it has type Int -})
















-- Establish a Subset constraint
checkSubset :: Π sch1 -> Π sch2 -> (Subset sch1 sch2 => r) -> r
checkSubset Nil                    _     callback = callback
checkSubset (Col name ty :>> rest) super callback
  = checkSubset rest super $
    checkIn name ty super $
    callback

-- Check that two names are distinct
checkNotEqual :: forall (name1 :: Symbol) name2 r.
                 Π name1 -> Π name2
              -> (((name1 /= name2) ~ 'True) => r) -> r
checkNotEqual name1 name2 callback
  = case name1 `eq` name2 of
      Just Refl -> schemaError $ "Found " ++ show name1 ++
                          " in both supposedly disjoint schemas."
      Nothing   -> assumeEquality @(name1 /= name2) @'True $
                   callback

-- Establish a ColNotIn condition
checkColNotIn :: Π name -> Π sch2
              -> ((ColNotIn name sch2 ~ 'True) => r) -> r
checkColNotIn _    Nil                    callback = callback
checkColNotIn name (Col name' _ :>> rest) callback
  = checkNotEqual name name' $
    checkColNotIn name rest $
    callback

-- Establish a Disjointness constraint
checkDisjoint :: Π sch1 -> Π sch2
              -> ((Disjoint sch1 sch2 ~ 'True) => r) -> r
checkDisjoint Nil                   _    callback = callback
checkDisjoint (Col name _ :>> rest) sch2 callback
  = checkColNotIn name sch2 $
    checkDisjoint rest sch2 $
    callback

-- Establish a ShowSchema constraint
checkShowSchema :: Π sch -> (ShowSchema sch => r) -> r
checkShowSchema Nil                 callback = callback
checkShowSchema (Col _ ty :>> rest) callback
  = getReadShowInstances ty $
    checkShowSchema rest $
    callback

-- Establish a ReadSchema constraint
checkReadSchema :: Π sch -> (ReadSchema sch => r) -> r
checkReadSchema Nil                 callback = callback
checkReadSchema (Col _ ty :>> rest) callback
  = getReadShowInstances ty $
    checkReadSchema rest $
    callback

-- Terminate program with an easy-to-understand message
schemaError :: String -> a
schemaError str = errorWithoutStackTrace $ "Schema validation error: " ++ str

-------------------------
type NameSchema = [ Col "first" String, Col "last" String ]

printName :: Row NameSchema -> IO ()
printName (first ::> last ::> _) = putStrLn (first ++ " " ++ last)

readDB classes_sch students_sch = do
  classes_tab  <- loadTable "classes.table" classes_sch
  students_tab <- loadTable "students.table" students_sch

  putStr "Whose students do you want to see? "
  prof <- getLine

  let joined = Project (
                Select (field @"id" @Int `ElementOf` field @"students") (
                 Product (Select (field @"prof" :== Literal prof) (Read classes_tab))
                         (Read students_tab)))
  rows <- query joined
  mapM_ printName rows

------------------
main :: IO ()
main = withSchema "classes.schema" $ \classes_sch ->
       withSchema "students.schema" $ \students_sch ->
       checkDisjoint classes_sch students_sch $
       checkIn (SSym @"students") (typeRep @[Int]) (classes_sch %:++ students_sch) $
       checkIn (SSym @"prof") (typeRep @String) classes_sch $
       checkIn (SSym @"last") (typeRep @String) (classes_sch %:++ students_sch) $
       checkIn (SSym @"id") (typeRep @Int) (classes_sch %:++ students_sch) $
       checkIn (SSym @"first") (typeRep @String) (classes_sch %:++ students_sch) $
       checkReadSchema students_sch $
       checkReadSchema classes_sch $
       readDB classes_sch students_sch
