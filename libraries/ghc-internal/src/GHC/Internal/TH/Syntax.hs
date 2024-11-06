{-# OPTIONS_HADDOCK not-home #-} -- we want users to import Language.Haskell.TH.Syntax instead
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnboxedTuples #-}

-- | This module is used internally in GHC's integration with Template Haskell
-- and defines the abstract syntax of Template Haskell.
--
-- This is not a part of the public API, and as such, there are no API
-- guarantees for this module from version to version.
--
-- Import "Language.Haskell.TH" or "Language.Haskell.TH.Syntax" instead!
module GHC.Internal.TH.Syntax
    ( module GHC.Internal.TH.Syntax
      -- * Language extensions
    , module GHC.Internal.LanguageExtensions
    , ForeignSrcLang(..)
    ) where

#ifdef BOOTSTRAP_TH
import Prelude
import System.IO.Unsafe ( unsafePerformIO )
import Data.Char        ( isAlpha, isAlphaNum, isUpper )
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Word
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.C.Types
import GHC.Ptr          ( Ptr, plusPtr )
import GHC.Generics     ( Generic )
#else
import GHC.Internal.Base hiding (NonEmpty(..),Type, Module, sequence)
import GHC.Internal.Data.NonEmpty (NonEmpty(..))
import GHC.Internal.Data.Traversable
import GHC.Internal.Word
import GHC.Internal.Generics (Generic)
import GHC.Internal.Show
import GHC.Internal.Integer
import GHC.Internal.Real
import GHC.Internal.Data.Foldable
import GHC.Internal.Foreign.Ptr
import GHC.Internal.ForeignPtr
import GHC.Internal.Foreign.C.Types
import GHC.Internal.Foreign.C.String
import GHC.Internal.Num
import GHC.Internal.IO.Unsafe
import GHC.Internal.List (dropWhile, break, replicate, reverse, last)
import GHC.Internal.Unicode
#endif
import GHC.Internal.ForeignSrcLang
import GHC.Internal.LanguageExtensions

oneName, manyName :: Name
-- | Synonym for @''GHC.Internal.Types.One'@, from @ghc-internal@.
oneName  = mkNameG DataName "ghc-internal" "GHC.Internal.Types" "One"
-- | Synonym for @''GHC.Internal.Types.Many'@, from @ghc-internal@.
manyName = mkNameG DataName "ghc-internal" "GHC.Internal.Types" "Many"


-----------------------------------------------------
--              Names and uniques
-----------------------------------------------------

-- | The name of a module.
newtype ModName = ModName String        -- Module name
 deriving (Show,Eq,Ord,Generic)

-- | The name of a package.
newtype PkgName = PkgName String        -- package name
 deriving (Show,Eq,Ord,Generic)

-- | Obtained from 'reifyModule' and 'Language.Haskell.TH.Lib.thisModule'.
data Module = Module PkgName ModName -- package qualified module name
 deriving (Show,Eq,Ord,Generic)

-- | An "Occurence Name".
newtype OccName = OccName String
 deriving (Show,Eq,Ord,Generic)

-- | Smart constructor for 'ModName'
mkModName :: String -> ModName
mkModName s = ModName s

-- | Accessor for 'ModName'
modString :: ModName -> String
modString (ModName m) = m

-- | Smart constructor for 'PkgName'
mkPkgName :: String -> PkgName
mkPkgName s = PkgName s

-- | Accessor for 'PkgName'
pkgString :: PkgName -> String
pkgString (PkgName m) = m


-----------------------------------------------------
--              OccName
-----------------------------------------------------

-- | Smart constructor for 'OccName'
mkOccName :: String -> OccName
mkOccName s = OccName s

-- | Accessor for 'OccName'
occString :: OccName -> String
occString (OccName occ) = occ


-----------------------------------------------------
--               Names
-----------------------------------------------------
--
-- For "global" names ('NameG') we need a totally unique name,
-- so we must include the name-space of the thing
--
-- For unique-numbered things ('NameU'), we've got a unique reference
-- anyway, so no need for name space
--
-- For dynamically bound thing ('NameS') we probably want them to
-- in a context-dependent way, so again we don't want the name
-- space.  For example:
--
-- > let v = mkName "T" in [| data $v = $v |]
--
-- Here we use the same Name for both type constructor and data constructor
--
--
-- NameL and NameG are bound *outside* the TH syntax tree
-- either globally (NameG) or locally (NameL). Ex:
--
-- > f x = $(h [| (map, x) |])
--
-- The 'map' will be a NameG, and 'x' wil be a NameL
--
-- These Names should never appear in a binding position in a TH syntax tree

{- $namecapture #namecapture#
Much of 'Name' API is concerned with the problem of /name capture/, which
can be seen in the following example.

> f expr = [| let x = 0 in $expr |]
> ...
> g x = $( f [| x |] )
> h y = $( f [| y |] )

A naive desugaring of this would yield:

> g x = let x = 0 in x
> h y = let x = 0 in y

All of a sudden, @g@ and @h@ have different meanings! In this case,
we say that the @x@ in the RHS of @g@ has been /captured/
by the binding of @x@ in @f@.

What we actually want is for the @x@ in @f@ to be distinct from the
@x@ in @g@, so we get the following desugaring:

> g x = let x' = 0 in x
> h y = let x' = 0 in y

which avoids name capture as desired.

In the general case, we say that a @Name@ can be captured if
the thing it refers to can be changed by adding new declarations.
-}

{- |
An abstract type representing names in the syntax tree.

'Name's can be constructed in several ways, which come with different
name-capture guarantees (see "Language.Haskell.TH.Syntax#namecapture" for
an explanation of name capture):

  * the built-in syntax @'f@ and @''T@ can be used to construct names,
    The expression @'f@ gives a @Name@ which refers to the value @f@
    currently in scope, and @''T@ gives a @Name@ which refers to the
    type @T@ currently in scope. These names can never be captured.

  * 'lookupValueName' and 'lookupTypeName' are similar to @'f@ and
     @''T@ respectively, but the @Name@s are looked up at the point
     where the current splice is being run. These names can never be
     captured.

  * 'newName' monadically generates a new name, which can never
     be captured.

  * 'mkName' generates a capturable name.

Names constructed using @newName@ and @mkName@ may be used in bindings
(such as @let x = ...@ or @\x -> ...@), but names constructed using
@lookupValueName@, @lookupTypeName@, @'f@, @''T@ may not.
-}
data Name = Name OccName NameFlavour deriving (Eq, Generic)

instance Ord Name where
    -- check if unique is different before looking at strings
  (Name o1 f1) `compare` (Name o2 f2) = (f1 `compare` f2)   `thenCmp`
                                        (o1 `compare` o2)

data NameFlavour
  = NameS           -- ^ An unqualified name; dynamically bound
  | NameQ ModName   -- ^ A qualified name; dynamically bound
  | NameU !Uniq     -- ^ A unique local name
  | NameL !Uniq     -- ^ Local name bound outside of the TH AST
  | NameG NameSpace PkgName ModName -- ^ Global name bound outside of the TH AST:
                -- An original name (occurrences only, not binders)
                -- Need the namespace too to be sure which
                -- thing we are naming
  deriving ( Eq, Ord, Show, Generic )

data NameSpace = VarName        -- ^ Variables
               | DataName       -- ^ Data constructors
               | TcClsName      -- ^ Type constructors and classes; Haskell has them
                                -- in the same name space for now.
               | FldName
                 { fldParent :: !String
                   -- ^ The textual name of the parent of the field.
                   --
                   --   - For a field of a datatype, this is the name of the first constructor
                   --     of the datatype (regardless of whether this constructor has this field).
                   --   - For a field of a pattern synonym, this is the name of the pattern synonym.
                 }
               deriving( Eq, Ord, Show, Generic )

-- | @Uniq@ is used by GHC to distinguish names from each other.
type Uniq = Integer

-- | The name without its module prefix.
--
-- ==== __Examples__
--
-- >>> nameBase ''Data.Either.Either
-- "Either"
-- >>> nameBase (mkName "foo")
-- "foo"
-- >>> nameBase (mkName "Module.foo")
-- "foo"
nameBase :: Name -> String
nameBase (Name occ _) = occString occ

-- | Module prefix of a name, if it exists.
--
-- ==== __Examples__
--
-- >>> nameModule ''Data.Either.Either
-- Just "Data.Either"
-- >>> nameModule (mkName "foo")
-- Nothing
-- >>> nameModule (mkName "Module.foo")
-- Just "Module"
nameModule :: Name -> Maybe String
nameModule (Name _ (NameQ m))     = Just (modString m)
nameModule (Name _ (NameG _ _ m)) = Just (modString m)
nameModule _                      = Nothing

-- | A name's package, if it exists.
--
-- ==== __Examples__
--
-- >>> namePackage ''Data.Either.Either
-- Just "base"
-- >>> namePackage (mkName "foo")
-- Nothing
-- >>> namePackage (mkName "Module.foo")
-- Nothing
namePackage :: Name -> Maybe String
namePackage (Name _ (NameG _ p _)) = Just (pkgString p)
namePackage _                      = Nothing

-- | Returns whether a name represents an occurrence of a top-level variable
-- ('VarName'), data constructor ('DataName'), type constructor, or type class
-- ('TcClsName'). If we can't be sure, it returns 'Nothing'.
--
-- ==== __Examples__
--
-- >>> nameSpace 'Prelude.id
-- Just VarName
-- >>> nameSpace (mkName "id")
-- Nothing -- only works for top-level variable names
-- >>> nameSpace 'Data.Maybe.Just
-- Just DataName
-- >>> nameSpace ''Data.Maybe.Maybe
-- Just TcClsName
-- >>> nameSpace ''Data.Ord.Ord
-- Just TcClsName
nameSpace :: Name -> Maybe NameSpace
nameSpace (Name _ (NameG ns _ _)) = Just ns
nameSpace _                       = Nothing

{- |
Generate a capturable name. Occurrences of such names will be
resolved according to the Haskell scoping rules at the occurrence
site.

For example:

> f = [| pi + $(varE (mkName "pi")) |]
> ...
> g = let pi = 3 in $f

In this case, @g@ is desugared to

> g = Prelude.pi + 3

Note that @mkName@ may be used with qualified names:

> mkName "Prelude.pi"

See also 'Language.Haskell.TH.Lib.dyn' for a useful combinator. The above example could
be rewritten using 'Language.Haskell.TH.Lib.dyn' as

> f = [| pi + $(dyn "pi") |]
-}
mkName :: String -> Name
-- The string can have a '.', thus "Foo.baz",
-- giving a dynamically-bound qualified name,
-- in which case we want to generate a NameQ
--
-- Parse the string to see if it has a "." in it
-- so we know whether to generate a qualified or unqualified name
-- It's a bit tricky because we need to parse
--
-- > Foo.Baz.x   as    Qual Foo.Baz x
--
-- So we parse it from back to front
mkName str
  = split [] (reverse str)
  where
    split occ []        = Name (mkOccName occ) NameS
    split occ ('.':rev) | not (null occ)
                        , is_rev_mod_name rev
                        = Name (mkOccName occ) (NameQ (mkModName (reverse rev)))
        -- The 'not (null occ)' guard ensures that
        --      mkName "&." = Name "&." NameS
        -- The 'is_rev_mod' guards ensure that
        --      mkName ".&" = Name ".&" NameS
        --      mkName "^.." = Name "^.." NameS      -- #8633
        --      mkName "Data.Bits..&" = Name ".&" (NameQ "Data.Bits")
        -- This rather bizarre case actually happened; (.&.) is in Data.Bits
    split occ (c:rev)   = split (c:occ) rev

    -- Recognises a reversed module name xA.yB.C,
    -- with at least one component,
    -- and each component looks like a module name
    --   (i.e. non-empty, starts with capital, all alpha)
    is_rev_mod_name rev_mod_str
      | (compt, rest) <- break (== '.') rev_mod_str
      , not (null compt), isUpper (last compt), all is_mod_char compt
      = case rest of
          []             -> True
          (_dot : rest') -> is_rev_mod_name rest'
      | otherwise
      = False

    is_mod_char c = isAlphaNum c || c == '_' || c == '\''

-- | Only used internally
mkNameU :: String -> Uniq -> Name
mkNameU s u = Name (mkOccName s) (NameU u)

-- | Only used internally
mkNameL :: String -> Uniq -> Name
mkNameL s u = Name (mkOccName s) (NameL u)

-- | Only used internally
mkNameQ :: String -> String -> Name
mkNameQ mn occ = Name (mkOccName occ) (NameQ (mkModName mn))

-- | Used for 'x etc, but not available to the programmer
mkNameG :: NameSpace -> String -> String -> String -> Name
mkNameG ns pkg modu occ
  = Name (mkOccName occ) (NameG ns (mkPkgName pkg) (mkModName modu))

mkNameS :: String -> Name
mkNameS n = Name (mkOccName n) NameS

mkNameG_v, mkNameG_tc, mkNameG_d :: String -> String -> String -> Name
mkNameG_v  = mkNameG VarName
mkNameG_tc = mkNameG TcClsName
mkNameG_d  = mkNameG DataName

mkNameG_fld :: String -- ^ package
            -> String -- ^ module
            -> String -- ^ parent (first constructor of parent type)
            -> String -- ^ field name
            -> Name
mkNameG_fld pkg modu con occ = mkNameG (FldName con) pkg modu occ

data NameIs = Alone    -- ^ @name@
            | Applied  -- ^ @(name)@
            | Infix    -- ^ @\`name\`@

showName :: Name -> String
showName = showName' Alone

showName' :: NameIs -> Name -> String
showName' ni nm
 = case ni of
       Alone        -> nms
       Applied
        | pnam      -> nms
        | otherwise -> "(" ++ nms ++ ")"
       Infix
        | pnam      -> "`" ++ nms ++ "`"
        | otherwise -> nms
    where
        -- For now, we make the NameQ and NameG print the same, even though
        -- NameQ is a qualified name (so what it means depends on what the
        -- current scope is), and NameG is an original name (so its meaning
        -- should be independent of what's in scope.
        -- We may well want to distinguish them in the end.
        -- Ditto NameU and NameL
        nms = case nm of
          Name occ NameS          -> occString occ
          Name occ (NameQ m)      -> modString m ++ "." ++ occString occ
          Name occ (NameG _ _ m) -> modString m ++ "." ++ occString occ
          Name occ (NameU u)      -> occString occ ++ "_" ++ show u
          Name occ (NameL u)      -> occString occ ++ "_" ++ show u

        pnam = classify nms

        -- True if we are function style, e.g. f, [], (,)
        -- False if we are operator style, e.g. +, :+
        classify "" = False -- shouldn't happen; . operator is handled below
        classify (x:xs) | isAlpha x || (x `elem` "_[]()") =
                            case dropWhile (/='.') xs of
                                  (_:xs') -> classify xs'
                                  []      -> True
                        | otherwise = False

instance Show Name where
  show = showName

-- Tuple data and type constructors
-- | Tuple data constructor
tupleDataName :: Int -> Name
-- | Tuple type constructor
tupleTypeName :: Int -> Name

tupleDataName n = mk_tup_name n DataName  True
tupleTypeName n = mk_tup_name n TcClsName True

-- Unboxed tuple data and type constructors
-- | Unboxed tuple data constructor
unboxedTupleDataName :: Int -> Name
-- | Unboxed tuple type constructor
unboxedTupleTypeName :: Int -> Name

unboxedTupleDataName n = mk_tup_name n DataName  False
unboxedTupleTypeName n = mk_tup_name n TcClsName False

mk_tup_name :: Int -> NameSpace -> Bool -> Name
mk_tup_name n space boxed
  = Name (mkOccName tup_occ) (NameG space (mkPkgName "ghc-internal") tup_mod)
  where
    withParens thing
      | boxed     = "("  ++ thing ++ ")"
      | otherwise = "(#" ++ thing ++ "#)"
    tup_occ | n == 0, space == TcClsName = if boxed then "Unit" else "Unit#"
            | n == 1 = if boxed then solo else unboxed_solo
            | space == TcClsName = "Tuple" ++ show n ++ if boxed then "" else "#"
            | otherwise = withParens (replicate n_commas ',')
    n_commas = n - 1
    tup_mod  = mkModName (if boxed then "GHC.Internal.Tuple" else "GHC.Internal.Types")
    solo
      | space == DataName = "MkSolo"
      | otherwise = "Solo"

    unboxed_solo = solo ++ "#"

-- Unboxed sum data and type constructors
-- | Unboxed sum data constructor
unboxedSumDataName :: SumAlt -> SumArity -> Name
-- | Unboxed sum type constructor
unboxedSumTypeName :: SumArity -> Name

unboxedSumDataName alt arity
  | alt > arity
  = error $ prefix ++ "Index out of bounds." ++ debug_info

  | alt <= 0
  = error $ prefix ++ "Alt must be > 0." ++ debug_info

  | arity < 2
  = error $ prefix ++ "Arity must be >= 2." ++ debug_info

  | otherwise
  = Name (mkOccName sum_occ)
         (NameG DataName (mkPkgName "ghc-internal") (mkModName "GHC.Internal.Types"))

  where
    prefix     = "unboxedSumDataName: "
    debug_info = " (alt: " ++ show alt ++ ", arity: " ++ show arity ++ ")"

    -- Synced with the definition of mkSumDataConOcc in GHC.Builtin.Types
    sum_occ = '(' : '#' : bars nbars_before ++ '_' : bars nbars_after ++ "#)"
    bars i = replicate i '|'
    nbars_before = alt - 1
    nbars_after  = arity - alt

unboxedSumTypeName arity
  | arity < 2
  = error $ "unboxedSumTypeName: Arity must be >= 2."
         ++ " (arity: " ++ show arity ++ ")"

  | otherwise
  = Name (mkOccName sum_occ)
         (NameG TcClsName (mkPkgName "ghc-internal") (mkModName "GHC.Internal.Types"))

  where
    -- Synced with the definition of mkSumTyConOcc in GHC.Builtin.Types
    sum_occ = "Sum" ++ show arity ++ "#"

-----------------------------------------------------
--              Locations
-----------------------------------------------------

-- | A location within a source file.
data Loc
  = Loc { loc_filename :: String
        , loc_package  :: String
        , loc_module   :: String
        , loc_start    :: CharPos
        , loc_end      :: CharPos }
   deriving( Show, Eq, Ord, Generic )

type CharPos = (Int, Int)       -- ^ Line and character position


-----------------------------------------------------
--
--      The Info returned by reification
--
-----------------------------------------------------

-- | Obtained from 'reify' in the 'Q' Monad.
data Info
  =
  -- | A class, with a list of its visible instances
  ClassI
      Dec
      [InstanceDec]

  -- | A class method
  | ClassOpI
       Name
       Type
       ParentName

  -- | A \"plain\" type constructor. \"Fancier\" type constructors are returned
  -- using 'PrimTyConI' or 'FamilyI' as appropriate. At present, this reified
  -- declaration will never have derived instances attached to it (if you wish
  -- to check for an instance, see 'reifyInstances').
  | TyConI
        Dec

  -- | A type or data family, with a list of its visible instances. A closed
  -- type family is returned with 0 instances.
  | FamilyI
        Dec
        [InstanceDec]

  -- | A \"primitive\" type constructor, which can't be expressed with a 'Dec'.
  -- Examples: @(->)@, @Int#@.
  | PrimTyConI
       Name
       Arity
       Unlifted

  -- | A data constructor
  | DataConI
       Name
       Type
       ParentName

  -- | A pattern synonym
  | PatSynI
       Name
       PatSynType

  {- |
  A \"value\" variable (as opposed to a type variable, see 'TyVarI').

  The @Maybe Dec@ field contains @Just@ the declaration which
  defined the variable - including the RHS of the declaration -
  or else @Nothing@, in the case where the RHS is unavailable to
  the compiler.

  At present, this value is /always/ @Nothing@:
  returning the RHS has not yet been implemented and is tracked by
  [GHC #14474](https://gitlab.haskell.org/ghc/ghc/-/issues/14474).
  -}
  | VarI
       Name
       Type
       (Maybe Dec)

  {- |
  A type variable.

  The @Type@ field contains the type which underlies the variable.
  At present, this is always @'VarT' theName@, but future changes
  may permit refinement of this.
  -}
  | TyVarI      -- Scoped type variable
        Name
        Type    -- What it is bound to
  deriving( Show, Eq, Ord, Generic )

-- | Obtained from 'reifyModule' in the 'Q' Monad.
data ModuleInfo =
  -- | Contains the import list of the module.
  ModuleInfo [Module]
  deriving( Show, Eq, Ord, Generic )

{- |
In 'ClassOpI' and 'DataConI', name of the parent class or type
-}
type ParentName = Name

-- | In 'UnboxedSumE' and 'UnboxedSumP', the number associated with a
-- particular data constructor. 'SumAlt's are one-indexed and should never
-- exceed the value of its corresponding 'SumArity'. For example:
--
-- * @(\#_|\#)@ has 'SumAlt' 1 (out of a total 'SumArity' of 2)
--
-- * @(\#|_\#)@ has 'SumAlt' 2 (out of a total 'SumArity' of 2)
type SumAlt = Int

-- | In 'UnboxedSumE', 'UnboxedSumT', and 'UnboxedSumP', the total number of
-- 'SumAlt's. For example, @(\#|\#)@ has a 'SumArity' of 2.
type SumArity = Int

-- | In 'PrimTyConI', arity of the type constructor
type Arity = Int

-- | In 'PrimTyConI', is the type constructor unlifted?
type Unlifted = Bool

-- | 'InstanceDec' describes a single instance of a class or type function.
-- It is just a 'Dec', but guaranteed to be one of the following:
--
--   * 'InstanceD' (with empty @['Dec']@)
--
--   * 'DataInstD' or 'NewtypeInstD' (with empty derived @['Name']@)
--
--   * 'TySynInstD'
type InstanceDec = Dec

-- | Fixity, as specified in a @infix[lr] n@ declaration.
data Fixity          = Fixity Int FixityDirection
    deriving( Eq, Ord, Show, Generic )

-- | The associativity of an operator, as in an @infix@ declaration.
data FixityDirection = InfixL | InfixR | InfixN
    deriving( Eq, Ord, Show, Generic )

-- | Highest allowed operator precedence for 'Fixity' constructor (answer: 9)
maxPrecedence :: Int
maxPrecedence = (9::Int)

-- | Default fixity: @infixl 9@
defaultFixity :: Fixity
defaultFixity = Fixity maxPrecedence InfixL

-----------------------------------------------------
--
--      The main syntax data types
--
-----------------------------------------------------

-- | A Haskell literal. Note that the numeric types are all in terms of either
-- 'Integer' or 'Rational', regardless of the type they represent. The extra
-- precision reflects the textual representation in source code.
data Lit = CharL Char           -- ^ @\'c\'@
         | StringL String       -- ^ @"string"@
         | IntegerL Integer     -- ^ @123@. Used for overloaded and non-overloaded
                                -- literals. We don't have a good way to
                                -- represent non-overloaded literals at
                                -- the moment. Maybe that doesn't matter?
         | RationalL Rational   -- ^ @1.23@. See above comment on 'IntegerL'.
         | IntPrimL Integer     -- ^ @123#@
         | WordPrimL Integer    -- ^ @123##@
         | FloatPrimL Rational  -- ^ @1.23#@
         | DoublePrimL Rational -- ^ @1.23##@
         | StringPrimL [Word8]  -- ^ @"string"#@. A primitive C-style string, type 'Addr#'
         | BytesPrimL Bytes     -- ^ Some raw bytes, type 'Addr#':
         | CharPrimL Char       -- ^ @\'c\'#@
    deriving( Show, Eq, Ord, Generic )

    -- We could add Int, Float, Double etc, as we do in HsLit,
    -- but that could complicate the
    -- supposedly-simple TH.Syntax literal type

-- | Raw bytes embedded into the binary.
--
-- Avoid using Bytes constructor directly as it is likely to change in the
-- future. Use helpers such as `mkBytes` in Language.Haskell.TH.Lib instead.
data Bytes = Bytes
   { bytesPtr    :: ForeignPtr Word8 -- ^ Pointer to the data
   , bytesOffset :: Word             -- ^ Offset from the pointer
   , bytesSize   :: Word             -- ^ Number of bytes

   -- Maybe someday:
   -- , bytesAlignement  :: Word -- ^ Alignement constraint
   -- , bytesReadOnly    :: Bool -- ^ Shall we embed into a read-only
   --                            --   section or not
   -- , bytesInitialized :: Bool -- ^ False: only use `bytesSize` to allocate
   --                            --   an uninitialized region
   }
   deriving (Generic)

-- We can't derive Show instance for Bytes because we don't want to show the
-- pointer value but the actual bytes (similarly to what ByteString does). See
-- #16457.
instance Show Bytes where
   show b = unsafePerformIO $ withForeignPtr (bytesPtr b) $ \ptr ->
               peekCStringLen ( ptr `plusPtr` fromIntegral (bytesOffset b)
                              , fromIntegral (bytesSize b)
                              )

-- We can't derive Eq and Ord instances for Bytes because we don't want to
-- compare pointer values but the actual bytes (similarly to what ByteString
-- does).  See #16457
instance Eq Bytes where
   (==) = eqBytes

instance Ord Bytes where
   compare = compareBytes

eqBytes :: Bytes -> Bytes -> Bool
eqBytes a@(Bytes fp off len) b@(Bytes fp' off' len')
  | len /= len'              = False    -- short cut on length
  | fp == fp' && off == off' = True     -- short cut for the same bytes
  | otherwise                = compareBytes a b == EQ

compareBytes :: Bytes -> Bytes -> Ordering
compareBytes (Bytes _   _    0)    (Bytes _   _    0)    = EQ  -- short cut for empty Bytes
compareBytes (Bytes fp1 off1 len1) (Bytes fp2 off2 len2) =
    unsafePerformIO $
      withForeignPtr fp1 $ \p1 ->
      withForeignPtr fp2 $ \p2 -> do
        i <- memcmp (p1 `plusPtr` fromIntegral off1)
                    (p2 `plusPtr` fromIntegral off2)
                    (fromIntegral (min len1 len2))
        return $! (i `compare` 0) <> (len1 `compare` len2)

foreign import ccall unsafe "memcmp"
  memcmp :: Ptr a -> Ptr b -> CSize -> IO CInt


-- | Pattern in Haskell given in @{}@
data Pat
  = LitP Lit                        -- ^ @{ 5 or \'c\' }@
  | VarP Name                       -- ^ @{ x }@
  | TupP [Pat]                      -- ^ @{ (p1,p2) }@
  | UnboxedTupP [Pat]               -- ^ @{ (\# p1,p2 \#) }@
  | UnboxedSumP Pat SumAlt SumArity -- ^ @{ (\#|p|\#) }@
  | ConP Name [Type] [Pat]          -- ^ @data T1 = C1 t1 t2; {C1 \@ty1 p1 p2} = e@
  | InfixP Pat Name Pat             -- ^ @foo ({x :+ y}) = e@
  | UInfixP Pat Name Pat            -- ^ @foo ({x :+ y}) = e@
                                    --
                                    -- See "Language.Haskell.TH.Syntax#infix"
  | ParensP Pat                     -- ^ @{(p)}@
                                    --
                                    -- See "Language.Haskell.TH.Syntax#infix"
  | TildeP Pat                      -- ^ @{ ~p }@
  | BangP Pat                       -- ^ @{ !p }@
  | AsP Name Pat                    -- ^ @{ x \@ p }@
  | WildP                           -- ^ @{ _ }@
  | RecP Name [FieldPat]            -- ^ @f (Pt { pointx = x }) = g x@
  | ListP [ Pat ]                   -- ^ @{ [1,2,3] }@
  | SigP Pat Type                   -- ^ @{ p :: t }@
  | ViewP Exp Pat                   -- ^ @{ e -> p }@
  | TypeP Type                      -- ^ @{ type p }@
  | InvisP Type                     -- ^ @{ @p }@
  | OrP (NonEmpty Pat)              -- ^ @{ p1; p2 }@
  deriving( Show, Eq, Ord, Generic )

-- | A (field name, pattern) pair. See 'RecP'.
type FieldPat = (Name,Pat)

-- | A @case@-alternative
data Match = Match Pat Body [Dec] -- ^ @case e of { pat -> body where decs }@
    deriving( Show, Eq, Ord, Generic )

-- | A clause consists of patterns, guards, a body expression, and a list of
-- declarations under a @where@. Clauses are seen in equations for function
-- definitions, @case@-experssions, explicitly-bidirectional pattern synonyms,
-- etc.
data Clause = Clause [Pat] Body [Dec]
                                  -- ^ @f { p1 p2 = body where decs }@
    deriving( Show, Eq, Ord, Generic )

-- | A Haskell expression.
data Exp
  = VarE Name                          -- ^ @{ x }@
  | ConE Name                          -- ^ @data T1 = C1 t1 t2; p = {C1} e1 e2  @
  | LitE Lit                           -- ^ @{ 5 or \'c\'}@
  | AppE Exp Exp                       -- ^ @{ f x }@
  | AppTypeE Exp Type                  -- ^ @{ f \@Int }@

  | InfixE (Maybe Exp) Exp (Maybe Exp) -- ^ @{x + y} or {(x+)} or {(+ x)} or {(+)}@

    -- It's a bit gruesome to use an Exp as the operator when a Name
    -- would suffice. Historically, Exp was used to make it easier to
    -- distinguish between infix constructors and non-constructors.
    -- This is a bit overkill, since one could just as well call
    -- `startsConId` or `startsConSym` (from `GHC.Lexeme`) on a Name.
    -- Unfortunately, changing this design now would involve lots of
    -- code churn for consumers of the TH API, so we continue to use
    -- an Exp as the operator and perform an extra check during conversion
    -- to ensure that the Exp is a constructor or a variable (#16895).

  | UInfixE Exp Exp Exp                -- ^ @{x + y}@
                                       --
                                       -- See "Language.Haskell.TH.Syntax#infix"
  | ParensE Exp                        -- ^ @{ (e) }@
                                       --
                                       -- See "Language.Haskell.TH.Syntax#infix"
  | LamE [Pat] Exp                     -- ^ @{ \\ p1 p2 -> e }@
  | LamCaseE [Match]                   -- ^ @{ \\case m1; m2 }@
  | LamCasesE [Clause]                 -- ^ @{ \\cases m1; m2 }@
  | TupE [Maybe Exp]                   -- ^ @{ (e1,e2) }  @
                                       --
                                       -- The 'Maybe' is necessary for handling
                                       -- tuple sections.
                                       --
                                       -- > (1,)
                                       --
                                       -- translates to
                                       --
                                       -- > TupE [Just (LitE (IntegerL 1)),Nothing]

  | UnboxedTupE [Maybe Exp]            -- ^ @{ (\# e1,e2 \#) }  @
                                       --
                                       -- The 'Maybe' is necessary for handling
                                       -- tuple sections.
                                       --
                                       -- > (# 'c', #)
                                       --
                                       -- translates to
                                       --
                                       -- > UnboxedTupE [Just (LitE (CharL 'c')),Nothing]

  | UnboxedSumE Exp SumAlt SumArity    -- ^ @{ (\#|e|\#) }@
  | CondE Exp Exp Exp                  -- ^ @{ if e1 then e2 else e3 }@
  | MultiIfE [(Guard, Exp)]            -- ^ @{ if | g1 -> e1 | g2 -> e2 }@
  | LetE [Dec] Exp                     -- ^ @{ let { x=e1; y=e2 } in e3 }@
  | CaseE Exp [Match]                  -- ^ @{ case e of m1; m2 }@
  | DoE (Maybe ModName) [Stmt]         -- ^ @{ do { p <- e1; e2 }  }@ or a qualified do if
                                       -- the module name is present
  | MDoE (Maybe ModName) [Stmt]        -- ^ @{ mdo { x <- e1 y; y <- e2 x; } }@ or a qualified
                                       -- mdo if the module name is present
  | CompE [Stmt]                       -- ^ @{ [ (x,y) | x <- xs, y <- ys ] }@
      --
      -- The result expression of the comprehension is
      -- the /last/ of the @'Stmt'@s, and should be a 'NoBindS'.
      --
      -- E.g. translation:
      --
      -- > [ f x | x <- xs ]
      --
      -- > CompE [BindS (VarP x) (VarE xs), NoBindS (AppE (VarE f) (VarE x))]

  | ArithSeqE Range                    -- ^ @{ [ 1 ,2 .. 10 ] }@
  | ListE [ Exp ]                      -- ^ @{ [1,2,3] }@
  | SigE Exp Type                      -- ^ @{ e :: t }@
  | RecConE Name [FieldExp]            -- ^ @{ T { x = y, z = w } }@
  | RecUpdE Exp [FieldExp]             -- ^ @{ (f x) { z = w } }@
  | StaticE Exp                        -- ^ @{ static e }@
  | UnboundVarE Name                   -- ^ @{ _x }@
                                       --
                                       -- This is used for holes or unresolved
                                       -- identifiers in AST quotes. Note that
                                       -- it could either have a variable name
                                       -- or constructor name.
  | LabelE String                      -- ^ @{ #x }@ ( Overloaded label )
  | ImplicitParamVarE String           -- ^ @{ ?x }@ ( Implicit parameter )
  | GetFieldE Exp String               -- ^ @{ exp.field }@ ( Overloaded Record Dot )
  | ProjectionE (NonEmpty String)      -- ^ @(.x)@ or @(.x.y)@ (Record projections)
  | TypedBracketE Exp                  -- ^ @[|| e ||]@
  | TypedSpliceE Exp                   -- ^ @$$e@
  | TypeE Type                         -- ^ @{ type t }@
  | ForallE [TyVarBndr Specificity] Exp -- ^ @forall \<vars\>. \<expr\>@
  | ForallVisE [TyVarBndr ()] Exp      -- ^ @forall \<vars\> -> \<expr\>@
  | ConstrainedE [Exp] Exp             -- ^ @\<ctxt\> => \<expr\>@
  deriving( Show, Eq, Ord, Generic )

-- | A (field name, expression) pair. See 'RecConE' and 'RecUpdE'.
type FieldExp = (Name,Exp)

-- Omitted: implicit parameters

-- | A potentially guarded expression, as in function definitions or case
-- alternatives.
data Body
  = GuardedB [(Guard,Exp)]   -- ^ @f p { | e1 = e2
                                 --      | e3 = e4 }
                                 -- where ds@
  | NormalB Exp              -- ^ @f p { = e } where ds@
  deriving( Show, Eq, Ord, Generic )

-- | A single guard.
data Guard
  = NormalG Exp -- ^ @f x { | odd x } = x@
  | PatG [Stmt] -- ^ @f x { | Just y <- x, Just z <- y } = z@
  deriving( Show, Eq, Ord, Generic )

-- | A single statement, as in @do@-notation.
data Stmt
  = BindS Pat Exp -- ^ @p <- e@
  | LetS [ Dec ]  -- ^ @{ let { x=e1; y=e2 } }@
  | NoBindS Exp   -- ^ @e@
  | ParS [[Stmt]] -- ^ @x <- e1 | s2, s3 | s4@ (in 'CompE')
  | RecS [Stmt]   -- ^ @rec { s1; s2 }@
  deriving( Show, Eq, Ord, Generic )

-- | A list/enum range expression.
data Range = FromR Exp               -- ^ @[n ..]@
           | FromThenR Exp Exp       -- ^ @[n, m ..]@
           | FromToR Exp Exp         -- ^ @[n .. m]@
           | FromThenToR Exp Exp Exp -- ^ @[n, m .. k]@
           deriving( Show, Eq, Ord, Generic )

-- | A single declaration.
data Dec
  = FunD Name [Clause]            -- ^ @{ f p1 p2 = b where decs }@
  | ValD Pat Body [Dec]           -- ^ @{ p = b where decs }@
  | DataD Cxt Name [TyVarBndr BndrVis]
          (Maybe Kind)            -- Kind signature (allowed only for GADTs)
          [Con] [DerivClause]
                                  -- ^ @{ data Cxt x => T x = A x | B (T x)
                                  --       deriving (Z,W)
                                  --       deriving stock Eq }@
  | NewtypeD Cxt Name [TyVarBndr BndrVis]
             (Maybe Kind)         -- Kind signature
             Con [DerivClause]    -- ^ @{ newtype Cxt x => T x = A (B x)
                                  --       deriving (Z,W Q)
                                  --       deriving stock Eq }@
  | TypeDataD Name [TyVarBndr BndrVis]
          (Maybe Kind)            -- Kind signature (allowed only for GADTs)
          [Con]                   -- ^ @{ type data T x = A x | B (T x) }@
  | TySynD Name [TyVarBndr BndrVis] Type -- ^ @{ type T x = (x,x) }@
  | ClassD Cxt Name [TyVarBndr BndrVis]
         [FunDep] [Dec]           -- ^ @{ class Eq a => Ord a where ds }@
  | InstanceD (Maybe Overlap) Cxt Type [Dec]
                                  -- ^ @{ instance {\-\# OVERLAPS \#-\}
                                  --        Show w => Show [w] where ds }@
  | SigD Name Type                -- ^ @{ length :: [a] -> Int }@
  | KiSigD Name Kind              -- ^ @{ type TypeRep :: k -> Type }@
  | ForeignD Foreign              -- ^ @{ foreign import ... }
                                  --{ foreign export ... }@

  | InfixD Fixity NamespaceSpecifier Name
                                  -- ^ @{ infix 3 data foo }@
  | DefaultD [Type]               -- ^ @{ default (Integer, Double) }@

  -- | pragmas
  | PragmaD Pragma                -- ^ @{ {\-\# INLINE [1] foo \#-\} }@

  -- | data families (may also appear in [Dec] of 'ClassD' and 'InstanceD')
  | DataFamilyD Name [TyVarBndr BndrVis]
               (Maybe Kind)
         -- ^ @{ data family T a b c :: * }@

  | DataInstD Cxt (Maybe [TyVarBndr ()]) Type
             (Maybe Kind)         -- Kind signature
             [Con] [DerivClause]  -- ^ @{ data instance Cxt x => T [x]
                                  --       = A x | B (T x)
                                  --       deriving (Z,W)
                                  --       deriving stock Eq }@

  | NewtypeInstD Cxt (Maybe [TyVarBndr ()]) Type -- Quantified type vars
                 (Maybe Kind)      -- Kind signature
                 Con [DerivClause] -- ^ @{ newtype instance Cxt x => T [x]
                                   --        = A (B x)
                                   --        deriving (Z,W)
                                   --        deriving stock Eq }@
  | TySynInstD TySynEqn            -- ^ @{ type instance ... }@

  -- | open type families (may also appear in [Dec] of 'ClassD' and 'InstanceD')
  | OpenTypeFamilyD TypeFamilyHead
         -- ^ @{ type family T a b c = (r :: *) | r -> a b }@

  | ClosedTypeFamilyD TypeFamilyHead [TySynEqn]
       -- ^ @{ type family F a b = (r :: *) | r -> a where ... }@

  | RoleAnnotD Name [Role]     -- ^ @{ type role T nominal representational }@
  | StandaloneDerivD (Maybe DerivStrategy) Cxt Type
       -- ^ @{ deriving stock instance Ord a => Ord (Foo a) }@
  | DefaultSigD Name Type      -- ^ @{ default size :: Data a => a -> Int }@

  -- | Pattern Synonyms
  | PatSynD Name PatSynArgs PatSynDir Pat
      -- ^ @{ pattern P v1 v2 .. vn <- p }@  unidirectional           or
      --   @{ pattern P v1 v2 .. vn = p  }@  implicit bidirectional   or
      --   @{ pattern P v1 v2 .. vn <- p
      --        where P v1 v2 .. vn = e  }@  explicit bidirectional
      --
      -- also, besides prefix pattern synonyms, both infix and record
      -- pattern synonyms are supported. See 'PatSynArgs' for details

  | PatSynSigD Name PatSynType  -- ^ A pattern synonym's type signature.

  | ImplicitParamBindD String Exp
      -- ^ @{ ?x = expr }@
      --
      -- Implicit parameter binding declaration. Can only be used in let
      -- and where clauses which consist entirely of implicit bindings.
  deriving( Show, Eq, Ord, Generic )

-- | A way to specify a namespace to look in when GHC needs to find
--   a name's source
data NamespaceSpecifier
  = NoNamespaceSpecifier   -- ^ Name may be everything; If there are two
                           --   names in different namespaces, then consider both
  | TypeNamespaceSpecifier -- ^ Name should be a type-level entity, such as a
                           --   data type, type alias, type family, type class,
                           --   or type variable
  | DataNamespaceSpecifier -- ^ Name should be a term-level entity, such as a
                           --   function, data constructor, or pattern synonym
  deriving( Show, Eq, Ord, Generic )

-- | Varieties of allowed instance overlap.
data Overlap = Overlappable   -- ^ May be overlapped by more specific instances
             | Overlapping    -- ^ May overlap a more general instance
             | Overlaps       -- ^ Both 'Overlapping' and 'Overlappable'
             | Incoherent     -- ^ Both 'Overlapping' and 'Overlappable', and
                              -- pick an arbitrary one if multiple choices are
                              -- available.
  deriving( Show, Eq, Ord, Generic )

-- | A single @deriving@ clause at the end of a datatype declaration.
data DerivClause = DerivClause (Maybe DerivStrategy) Cxt
    -- ^ @{ deriving stock (Eq, Ord) }@
  deriving( Show, Eq, Ord, Generic )

-- | What the user explicitly requests when deriving an instance with
-- @-XDerivingStrategies@.
data DerivStrategy = StockStrategy    -- ^ @deriving {stock} C@
                   | AnyclassStrategy -- ^ @deriving {anyclass} C@, @-XDeriveAnyClass@
                   | NewtypeStrategy  -- ^ @deriving {newtype} C@, @-XGeneralizedNewtypeDeriving@
                   | ViaStrategy Type -- ^ @deriving C {via T}@, @-XDerivingVia@
  deriving( Show, Eq, Ord, Generic )

-- | A pattern synonym's type. Note that a pattern synonym's /fully/
-- specified type has a peculiar shape coming with two forall
-- quantifiers and two constraint contexts. For example, consider the
-- pattern synonym
--
-- > pattern P x1 x2 ... xn = <some-pattern>
--
-- P's complete type is of the following form
--
-- > pattern P :: forall universals.   required constraints
-- >           => forall existentials. provided constraints
-- >           => t1 -> t2 -> ... -> tn -> t
--
-- consisting of four parts:
--
--   1. the (possibly empty lists of) universally quantified type
--      variables and required constraints on them.
--   2. the (possibly empty lists of) existentially quantified
--      type variables and the provided constraints on them.
--   3. the types @t1@, @t2@, .., @tn@ of @x1@, @x2@, .., @xn@, respectively
--   4. the type @t@ of @\<some-pattern\>@, mentioning only universals.
--
-- Pattern synonym types interact with TH when (a) reifying a pattern
-- synonym, (b) pretty printing, or (c) specifying a pattern synonym's
-- type signature explicitly:
--
--   * Reification always returns a pattern synonym's /fully/ specified
--     type in abstract syntax.
--
--   * Pretty printing via 'Language.Haskell.TH.Ppr.pprPatSynType' abbreviates
--     a pattern synonym's type unambiguously in concrete syntax: The rule of
--     thumb is to print initial empty universals and the required
--     context as @() =>@, if existentials and a provided context
--     follow. If only universals and their required context, but no
--     existentials are specified, only the universals and their
--     required context are printed. If both or none are specified, so
--     both (or none) are printed.
--
--   * When specifying a pattern synonym's type explicitly with
--     'PatSynSigD' either one of the universals, the existentials, or
--     their contexts may be left empty.
--
-- See the GHC user's guide for more information on pattern synonyms
-- and their types:
-- <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#pattern-synonyms>.
type PatSynType = Type

-- | Common elements of 'OpenTypeFamilyD' and 'ClosedTypeFamilyD'. By
-- analogy with "head" for type classes and type class instances as
-- defined in /Type classes: an exploration of the design space/, the
-- @TypeFamilyHead@ is defined to be the elements of the declaration
-- between @type family@ and @where@.
data TypeFamilyHead =
  TypeFamilyHead Name [TyVarBndr BndrVis] FamilyResultSig (Maybe InjectivityAnn)
  deriving( Show, Eq, Ord, Generic )

-- | One equation of a type family instance or closed type family. The
-- arguments are the left-hand-side type and the right-hand-side result.
--
-- For instance, if you had the following type family:
--
-- @
-- type family Foo (a :: k) :: k where
--   forall k (a :: k). Foo \@k a = a
-- @
--
-- The @Foo \@k a = a@ equation would be represented as follows:
--
-- @
-- 'TySynEqn' ('Just' ['PlainTV' k, 'KindedTV' a ('VarT' k)])
--            ('AppT' ('AppKindT' ('ConT' ''Foo) ('VarT' k)) ('VarT' a))
--            ('VarT' a)
-- @
data TySynEqn = TySynEqn (Maybe [TyVarBndr ()]) Type Type
  deriving( Show, Eq, Ord, Generic )

-- | [Functional dependency](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/functional_dependencies.html)
-- syntax, as in a class declaration.
data FunDep = FunDep [Name] [Name] -- ^ @class C a b {| a -> b}@
  deriving( Show, Eq, Ord, Generic )

-- | A @foreign@ declaration.
data Foreign = ImportF Callconv Safety String Name Type
             -- ^ @foreign import callconv safety "foreign_name" haskellName :: type@
             | ExportF Callconv        String Name Type
             -- ^ @foreign export callconv "foreign_name" haskellName :: type@
         deriving( Show, Eq, Ord, Generic )

-- keep Callconv in sync with module ForeignCall in ghc/compiler/GHC/Types/ForeignCall.hs
-- | A calling convention identifier, as in a 'Foreign' declaration.
data Callconv = CCall | StdCall | CApi | Prim | JavaScript
          deriving( Show, Eq, Ord, Generic )

-- | A safety level, as in a 'Foreign' declaration.
data Safety = Unsafe | Safe | Interruptible
        deriving( Show, Eq, Ord, Generic )

data Pragma = InlineP         Name Inline RuleMatch Phases
            -- ^ @{ {\-\# [inline] [rule match] [phases] [phases] name #-} }@. See
            -- 'Inline' and 'RuleMatch'.
            | OpaqueP         Name
            -- ^ @{ {\-\# OPAQUE T #-} }@
            | SpecialiseEP    (Maybe [TyVarBndr ()]) [RuleBndr] Exp (Maybe Inline) Phases
            -- ^ @{ {\-\# SPECIALISE [forall t_1 ... t_i]. [forall b_1 ... b_j] [INLINE] [phases] exp #-} }@
            | SpecialiseInstP Type
            -- ^ @{ {\-\# SPECIALISE instance I #-} }@
            | RuleP           String (Maybe [TyVarBndr ()]) [RuleBndr] Exp Exp Phases
            -- ^ @{ {\-\# RULES "name" [phases] [forall t_1 ... t_i]. [forall b_1 ... b_j] rules... e_1 = e_2 #-} }@
            | AnnP            AnnTarget Exp
            -- ^ @{ {\-\# ANN target exp #-} }@
            | LineP           Int String
            -- ^ @{ {\-\# LINE n "file name" #-} }@
            | CompleteP       [Name] (Maybe Name)
                -- ^ @{ {\-\# COMPLETE C_1, ..., C_i [ :: T ] \#-} }@
            | SCCP            Name (Maybe String)
                -- ^ @{ {\-\# SCC fun "optional_name" \#-} }@
        deriving( Show, Eq, Ord, Generic )

-- | An inline pragma.
data Inline = NoInline
            -- ^ @{ {\-\# NOINLINE ... #-} }@
            | Inline
            -- ^ @{ {\-\# INLINE ... #-} }@
            | Inlinable
            -- ^ @{ {\-\# INLINABLE ... #-} }@
            deriving (Show, Eq, Ord, Generic)

-- | A @CONLIKE@ modifier, as in one of the various inline pragmas, or lack
-- thereof ('FunLike').
data RuleMatch = ConLike
               -- ^ @{ {\-\# CONLIKE [inline] ... #-} }@
               | FunLike
               -- ^ @{ {\-\# [inline] ... #-} }@
               deriving (Show, Eq, Ord, Generic)

-- | Phase control syntax.
data Phases = AllPhases
            -- ^ The default when unspecified
            | FromPhase Int
            -- ^ @[n]@
            | BeforePhase Int
            -- ^ @[~n]@
            deriving (Show, Eq, Ord, Generic)

-- | A binder found in the @forall@ of a @RULES@ pragma.
data RuleBndr = RuleVar Name
              -- ^ @forall {a} ... .@
              | TypedRuleVar Name Type
              -- ^ @forall {(a :: t)} ... .@
              deriving (Show, Eq, Ord, Generic)

-- | The target of an @ANN@ pragma
data AnnTarget = ModuleAnnotation
               -- ^ @{\-\# ANN {module} ... #-}@
               | TypeAnnotation Name
               -- ^ @{\-\# ANN type {name} ... #-}@
               | ValueAnnotation Name
               -- ^ @{\-\# ANN {name} ... #-}@
              deriving (Show, Eq, Ord, Generic)

-- | A context, as found on the left side of a @=>@ in a type.
type Cxt = [Pred]                 -- ^ @(Eq a, Ord b)@

-- | Since the advent of @ConstraintKinds@, constraints are really just types.
-- Equality constraints use the 'EqualityT' constructor. Constraints may also
-- be tuples of other constraints.
type Pred = Type

-- | 'SourceUnpackedness' corresponds to unpack annotations found in the source code.
--
-- This may not agree with the annotations returned by 'reifyConStrictness'.
-- See 'reifyConStrictness' for more information.
data SourceUnpackedness
  = NoSourceUnpackedness -- ^ @C a@
  | SourceNoUnpack       -- ^ @C { {\-\# NOUNPACK \#-\} } a@
  | SourceUnpack         -- ^ @C { {\-\# UNPACK \#-\} } a@
        deriving (Show, Eq, Ord, Generic)

-- | 'SourceStrictness' corresponds to strictness annotations found in the source code.
--
-- This may not agree with the annotations returned by 'reifyConStrictness'.
-- See 'reifyConStrictness' for more information.
data SourceStrictness = NoSourceStrictness    -- ^ @C a@
                      | SourceLazy            -- ^ @C {~}a@
                      | SourceStrict          -- ^ @C {!}a@
        deriving (Show, Eq, Ord, Generic)

-- | Unlike 'SourceStrictness' and 'SourceUnpackedness', 'DecidedStrictness'
-- refers to the strictness annotations that the compiler chooses for a data constructor
-- field, which may be different from what is written in source code.
--
-- Note that non-unpacked strict fields are assigned 'DecidedLazy' when a bang would be inappropriate,
-- such as the field of a newtype constructor and fields that have an unlifted type.
--
-- See 'reifyConStrictness' for more information.
data DecidedStrictness = DecidedLazy -- ^ Field inferred to not have a bang.
                       | DecidedStrict -- ^ Field inferred to have a bang.
                       | DecidedUnpack -- ^ Field inferred to be unpacked.
        deriving (Show, Eq, Ord, Generic)

-- | A data constructor.
--
-- The constructors for 'Con' can roughly be divided up into two categories:
-- those for constructors with \"vanilla\" syntax ('NormalC', 'RecC', and
-- 'InfixC'), and those for constructors with GADT syntax ('GadtC' and
-- 'RecGadtC'). The 'ForallC' constructor, which quantifies additional type
-- variables and class contexts, can surround either variety of constructor.
-- However, the type variables that it quantifies are different depending
-- on what constructor syntax is used:
--
-- * If a 'ForallC' surrounds a constructor with vanilla syntax, then the
--   'ForallC' will only quantify /existential/ type variables. For example:
--
--   @
--   data Foo a = forall b. MkFoo a b
--   @
--
--   In @MkFoo@, 'ForallC' will quantify @b@, but not @a@.
--
-- * If a 'ForallC' surrounds a constructor with GADT syntax, then the
--   'ForallC' will quantify /all/ type variables used in the constructor.
--   For example:
--
--   @
--   data Bar a b where
--     MkBar :: (a ~ b) => c -> MkBar a b
--   @
--
--   In @MkBar@, 'ForallC' will quantify @a@, @b@, and @c@.
--
-- Multiplicity annotations for data types are currently not supported
-- in Template Haskell (i.e. all fields represented by Template Haskell
-- will be linear).
data Con =
  -- | @C Int a@
    NormalC Name [BangType]

  -- | @C { v :: Int, w :: a }@
  | RecC Name [VarBangType]

  -- | @Int :+ a@
  | InfixC BangType Name BangType

  -- | @forall a. Eq a => C [a]@
  | ForallC [TyVarBndr Specificity] Cxt Con

  -- @C :: a -> b -> T b Int@
  | GadtC [Name]
            -- ^ The list of constructors, corresponding to the GADT constructor
            -- syntax @C1, C2 :: a -> T b@.
            --
            -- Invariant: the list must be non-empty.
          [BangType] -- ^ The constructor arguments
          Type -- ^ See Note [GADT return type]

  -- | @C :: { v :: Int } -> T b Int@
  | RecGadtC [Name]
             -- ^ The list of constructors, corresponding to the GADT record
             -- constructor syntax @C1, C2 :: { fld :: a } -> T b@.
             --
             -- Invariant: the list must be non-empty.
             [VarBangType] -- ^ The constructor arguments
             Type -- ^ See Note [GADT return type]
        deriving (Show, Eq, Ord, Generic)

-- Note [GADT return type]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- The return type of a GADT constructor does not necessarily match the name of
-- the data type:
--
-- type S = T
--
-- data T a where
--     MkT :: S Int
--
--
-- type S a = T
--
-- data T a where
--     MkT :: S Char Int
--
--
-- type Id a = a
-- type S a = T
--
-- data T a where
--     MkT :: Id (S Char Int)
--
--
-- That is why we allow the return type stored by a constructor to be an
-- arbitrary type. See also #11341

-- | Strictness information in a data constructor's argument.
data Bang = Bang SourceUnpackedness SourceStrictness
         -- ^ @C { {\-\# UNPACK \#-\} !}a@
        deriving (Show, Eq, Ord, Generic)

-- | A type with a strictness annotation, as in data constructors. See 'Con'.
type BangType    = (Bang, Type)

-- | 'BangType', but for record fields. See 'Con'.
type VarBangType = (Name, Bang, Type)

-- | As of @template-haskell-2.11.0.0@, 'Strict' has been replaced by 'Bang'.
type Strict      = Bang

-- | As of @template-haskell-2.11.0.0@, 'StrictType' has been replaced by
-- 'BangType'.
type StrictType    = BangType

-- | As of @template-haskell-2.11.0.0@, 'VarStrictType' has been replaced by
-- 'VarBangType'.
type VarStrictType = VarBangType

-- | A pattern synonym's directionality.
data PatSynDir
  = Unidir             -- ^ @pattern P x {<-} p@
  | ImplBidir          -- ^ @pattern P x {=} p@
  | ExplBidir [Clause] -- ^ @pattern P x {<-} p where P x = e@
  deriving( Show, Eq, Ord, Generic )

-- | A pattern synonym's argument type.
data PatSynArgs
  = PrefixPatSyn [Name]        -- ^ @pattern P {x y z} = p@
  | InfixPatSyn Name Name      -- ^ @pattern {x P y} = p@
  | RecordPatSyn [Name]        -- ^ @pattern P { {x,y,z} } = p@
  deriving( Show, Eq, Ord, Generic )

-- | A Haskell type.
data Type = ForallT [TyVarBndr Specificity] Cxt Type -- ^ @forall \<vars\>. \<ctxt\> => \<type\>@
          | ForallVisT [TyVarBndr ()] Type -- ^ @forall \<vars\> -> \<type\>@
          | AppT Type Type                 -- ^ @T a b@
          | AppKindT Type Kind             -- ^ @T \@k t@
          | SigT Type Kind                 -- ^ @t :: k@
          | VarT Name                      -- ^ @a@
          | ConT Name                      -- ^ @T@
          | PromotedT Name                 -- ^ @'T@
          | InfixT Type Name Type          -- ^ @T + T@
          | UInfixT Type Name Type         -- ^ @T + T@
                                           --
                                           -- See "Language.Haskell.TH.Syntax#infix"
          | PromotedInfixT Type Name Type  -- ^ @T :+: T@
          | PromotedUInfixT Type Name Type -- ^ @T :+: T@
                                           --
                                           -- See "Language.Haskell.TH.Syntax#infix"
          | ParensT Type                   -- ^ @(T)@

          -- See Note [Representing concrete syntax in types]
          | TupleT Int                     -- ^ @(,)@, @(,,)@, etc.
          | UnboxedTupleT Int              -- ^ @(\#,\#)@, @(\#,,\#)@, etc.
          | UnboxedSumT SumArity           -- ^ @(\#|\#)@, @(\#||\#)@, etc.
          | ArrowT                         -- ^ @->@
          | MulArrowT                      -- ^ @%n ->@
                                           --
                                           -- Generalised arrow type with multiplicity argument
          | EqualityT                      -- ^ @~@
          | ListT                          -- ^ @[]@
          | PromotedTupleT Int             -- ^ @'()@, @'(,)@, @'(,,)@, etc.
          | PromotedNilT                   -- ^ @'[]@
          | PromotedConsT                  -- ^ @'(:)@
          | StarT                          -- ^ @*@
          | ConstraintT                    -- ^ @Constraint@
          | LitT TyLit                     -- ^ @0@, @1@, @2@, etc.
          | WildCardT                      -- ^ @_@
          | ImplicitParamT String Type     -- ^ @?x :: t@
      deriving( Show, Eq, Ord, Generic )

-- | The specificity of a type variable in a @forall ...@.
data Specificity = SpecifiedSpec          -- ^ @a@
                 | InferredSpec           -- ^ @{a}@
      deriving( Show, Eq, Ord, Generic )

-- | The @flag@ type parameter is instantiated to one of the following types:
--
--   * 'Specificity' (examples: 'ForallC', 'ForallT')
--   * 'BndrVis' (examples: 'DataD', 'ClassD', etc.)
--   * '()', a catch-all type for other forms of binders, including 'ForallVisT', 'DataInstD', 'RuleP', and 'TyVarSig'
--
data TyVarBndr flag = PlainTV  Name flag      -- ^ @a@
                    | KindedTV Name flag Kind -- ^ @(a :: k)@
      deriving( Show, Eq, Ord, Generic, Functor, Foldable, Traversable )

-- | Visibility of a type variable. See [Inferred vs. specified type variables](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_applications.html#inferred-vs-specified-type-variables).
data BndrVis = BndrReq                    -- ^ @a@
             | BndrInvis                  -- ^ @\@a@
      deriving( Show, Eq, Ord, Generic )

-- | Type family result signature
data FamilyResultSig = NoSig              -- ^ no signature
                     | KindSig  Kind      -- ^ @k@
                     | TyVarSig (TyVarBndr ()) -- ^ @= r, = (r :: k)@
      deriving( Show, Eq, Ord, Generic )

-- | Injectivity annotation as in an [injective type family](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_families.html)
data InjectivityAnn = InjectivityAnn Name [Name]
  deriving ( Show, Eq, Ord, Generic )

-- | Type-level literals.
data TyLit = NumTyLit Integer             -- ^ @2@
           | StrTyLit String              -- ^ @\"Hello\"@
           | CharTyLit Char               -- ^ @\'C\'@, @since 4.16.0.0
  deriving ( Show, Eq, Ord, Generic )

-- | Role annotations
data Role = NominalR            -- ^ @nominal@
          | RepresentationalR   -- ^ @representational@
          | PhantomR            -- ^ @phantom@
          | InferR              -- ^ @_@
  deriving( Show, Eq, Ord, Generic )

-- | Annotation target for reifyAnnotations
data AnnLookup = AnnLookupModule Module
               | AnnLookupName Name
               deriving( Show, Eq, Ord, Generic )

-- | To avoid duplication between kinds and types, they
-- are defined to be the same. Naturally, you would never
-- have a type be 'StarT' and you would never have a kind
-- be 'SigT', but many of the other constructors are shared.
-- Note that the kind @Bool@ is denoted with 'ConT', not
-- 'PromotedT'. Similarly, tuple kinds are made with 'TupleT',
-- not 'PromotedTupleT'.

type Kind = Type

{- Note [Representing concrete syntax in types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Haskell has a rich concrete syntax for types, including
  t1 -> t2, (t1,t2), [t], and so on
In TH we represent all of this using AppT, with a distinguished
type constructor at the head.  So,
  Type              TH representation
  -----------------------------------------------
  t1 -> t2          ArrowT `AppT` t2 `AppT` t2
  [t]               ListT `AppT` t
  (t1,t2)           TupleT 2 `AppT` t1 `AppT` t2
  '(t1,t2)          PromotedTupleT 2 `AppT` t1 `AppT` t2

But if the original HsSyn used prefix application, we won't use
these special TH constructors.  For example
  [] t              ConT "[]" `AppT` t
  (->) t            ConT "->" `AppT` t
In this way we can faithfully represent in TH whether the original
HsType used concrete syntax or not.

The one case that doesn't fit this pattern is that of promoted lists
  '[ Maybe, IO ]    PromotedListT 2 `AppT` t1 `AppT` t2
but it's very smelly because there really is no type constructor
corresponding to PromotedListT. So we encode HsExplicitListTy with
PromotedConsT and PromotedNilT (which *do* have underlying type
constructors):
  '[ Maybe, IO ]    PromotedConsT `AppT` Maybe `AppT`
                    (PromotedConsT  `AppT` IO `AppT` PromotedNilT)
-}

-- | A location at which to attach Haddock documentation.
-- Note that adding documentation to a 'Name' defined oustide of the current
-- module will cause an error.
data DocLoc
  = ModuleDoc         -- ^ At the current module's header.
  | DeclDoc Name      -- ^ At a declaration, not necessarily top level.
  | ArgDoc Name Int   -- ^ At a specific argument of a function, indexed by its
                      -- position.
  | InstDoc Type      -- ^ At a class or family instance.
  deriving ( Show, Eq, Ord, Generic )

-----------------------------------------------------
--              Internal helper functions
-----------------------------------------------------

-- | Internal helper function.
cmpEq :: Ordering -> Bool
cmpEq EQ = True
cmpEq _  = False

-- | Internal helper function.
thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ o2 = o2
thenCmp o1 _  = o1

-- | Internal helper function.
get_cons_names :: Con -> [Name]
get_cons_names (NormalC n _)     = [n]
get_cons_names (RecC n _)        = [n]
get_cons_names (InfixC _ n _)    = [n]
get_cons_names (ForallC _ _ con) = get_cons_names con
-- GadtC can have multiple names, e.g
-- > data Bar a where
-- >   MkBar1, MkBar2 :: a -> Bar a
-- Will have one GadtC with [MkBar1, MkBar2] as names
get_cons_names (GadtC ns _ _)    = ns
get_cons_names (RecGadtC ns _ _) = ns
