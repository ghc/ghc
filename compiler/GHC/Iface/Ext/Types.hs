{-
Types for the .hie file format are defined here.

For more information see https://gitlab.haskell.org/ghc/ghc/wikis/hie-files
-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module GHC.Iface.Ext.Types where

import GhcPrelude

import Config
import Binary
import FastString                 ( FastString )
import GHC.Iface.Type
import GHC.Types.Module           ( ModuleName, Module )
import GHC.Types.Name             ( Name )
import Outputable hiding ( (<>) )
import GHC.Types.SrcLoc           ( RealSrcSpan )
import GHC.Types.Avail
import qualified Outputable as O ( (<>) )

import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.Set as S
import Data.ByteString            ( ByteString )
import Data.Data                  ( Typeable, Data )
import Data.Semigroup             ( Semigroup(..) )
import Data.Word                  ( Word8 )
import Control.Applicative        ( (<|>) )

type Span = RealSrcSpan

-- | Current version of @.hie@ files
hieVersion :: Integer
hieVersion = read (cProjectVersionInt ++ cProjectPatchLevel) :: Integer

{- |
GHC builds up a wealth of information about Haskell source as it compiles it.
@.hie@ files are a way of persisting some of this information to disk so that
external tools that need to work with haskell source don't need to parse,
typecheck, and rename all over again. These files contain:

  * a simplified AST

       * nodes are annotated with source positions and types
       * identifiers are annotated with scope information

  * the raw bytes of the initial Haskell source

Besides saving compilation cycles, @.hie@ files also offer a more stable
interface than the GHC API.
-}
data HieFile = HieFile
    { hie_hs_file :: FilePath
    -- ^ Initial Haskell source file path

    , hie_module :: Module
    -- ^ The module this HIE file is for

    , hie_types :: A.Array TypeIndex HieTypeFlat
    -- ^ Types referenced in the 'hie_asts'.
    --
    -- See Note [Efficient serialization of redundant type info]

    , hie_asts :: HieASTs TypeIndex
    -- ^ Type-annotated abstract syntax trees

    , hie_exports :: [AvailInfo]
    -- ^ The names that this module exports

    , hie_hs_src :: ByteString
    -- ^ Raw bytes of the initial Haskell source
    }
instance Binary HieFile where
  put_ bh hf = do
    put_ bh $ hie_hs_file hf
    put_ bh $ hie_module hf
    put_ bh $ hie_types hf
    put_ bh $ hie_asts hf
    put_ bh $ hie_exports hf
    put_ bh $ hie_hs_src hf

  get bh = HieFile
    <$> get bh
    <*> get bh
    <*> get bh
    <*> get bh
    <*> get bh
    <*> get bh


{-
Note [Efficient serialization of redundant type info]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The type information in .hie files is highly repetitive and redundant. For
example, consider the expression

    const True 'a'

There is a lot of shared structure between the types of subterms:

  * const True 'a' ::                 Bool
  * const True     ::         Char -> Bool
  * const          :: Bool -> Char -> Bool

Since all 3 of these types need to be stored in the .hie file, it is worth
making an effort to deduplicate this shared structure. The trick is to define
a new data type that is a flattened version of 'Type':

    data HieType a = HAppTy a a  -- data Type = AppTy Type Type
                   | HFunTy a a  --           | FunTy Type Type
                   | ...

    type TypeIndex = Int

Types in the final AST are stored in an 'A.Array TypeIndex (HieType TypeIndex)',
where the 'TypeIndex's in the 'HieType' are references to other elements of the
array. Types recovered from GHC are deduplicated and stored in this compressed
form with sharing of subtrees.
-}

type TypeIndex = Int

-- | A flattened version of 'Type'.
--
-- See Note [Efficient serialization of redundant type info]
data HieType a
  = HTyVarTy Name
  | HAppTy a (HieArgs a)
  | HTyConApp IfaceTyCon (HieArgs a)
  | HForAllTy ((Name, a),ArgFlag) a
  | HFunTy  a a
  | HQualTy a a           -- ^ type with constraint: @t1 => t2@ (see 'IfaceDFunTy')
  | HLitTy IfaceTyLit
  | HCastTy a
  | HCoercionTy
    deriving (Functor, Foldable, Traversable, Eq)

type HieTypeFlat = HieType TypeIndex

-- | Roughly isomorphic to the original core 'Type'.
newtype HieTypeFix = Roll (HieType (HieTypeFix))

instance Binary (HieType TypeIndex) where
  put_ bh (HTyVarTy n) = do
    putByte bh 0
    put_ bh n
  put_ bh (HAppTy a b) = do
    putByte bh 1
    put_ bh a
    put_ bh b
  put_ bh (HTyConApp n xs) = do
    putByte bh 2
    put_ bh n
    put_ bh xs
  put_ bh (HForAllTy bndr a) = do
    putByte bh 3
    put_ bh bndr
    put_ bh a
  put_ bh (HFunTy a b) = do
    putByte bh 4
    put_ bh a
    put_ bh b
  put_ bh (HQualTy a b) = do
    putByte bh 5
    put_ bh a
    put_ bh b
  put_ bh (HLitTy l) = do
    putByte bh 6
    put_ bh l
  put_ bh (HCastTy a) = do
    putByte bh 7
    put_ bh a
  put_ bh (HCoercionTy) = putByte bh 8

  get bh = do
    (t :: Word8) <- get bh
    case t of
      0 -> HTyVarTy <$> get bh
      1 -> HAppTy <$> get bh <*> get bh
      2 -> HTyConApp <$> get bh <*> get bh
      3 -> HForAllTy <$> get bh <*> get bh
      4 -> HFunTy <$> get bh <*> get bh
      5 -> HQualTy <$> get bh <*> get bh
      6 -> HLitTy <$> get bh
      7 -> HCastTy <$> get bh
      8 -> return HCoercionTy
      _ -> panic "Binary (HieArgs Int): invalid tag"


-- | A list of type arguments along with their respective visibilities (ie. is
-- this an argument that would return 'True' for 'isVisibleArgFlag'?).
newtype HieArgs a = HieArgs [(Bool,a)]
  deriving (Functor, Foldable, Traversable, Eq)

instance Binary (HieArgs TypeIndex) where
  put_ bh (HieArgs xs) = put_ bh xs
  get bh = HieArgs <$> get bh

-- | Mapping from filepaths (represented using 'FastString') to the
-- corresponding AST
newtype HieASTs a = HieASTs { getAsts :: (M.Map FastString (HieAST a)) }
  deriving (Functor, Foldable, Traversable)

instance Binary (HieASTs TypeIndex) where
  put_ bh asts = put_ bh $ M.toAscList $ getAsts asts
  get bh = HieASTs <$> fmap M.fromDistinctAscList (get bh)

instance Outputable a => Outputable (HieASTs a) where
  ppr (HieASTs asts) = M.foldrWithKey go "" asts
    where
      go k a rest = vcat $
        [ "File: " O.<> ppr k
        , ppr a
        , rest
        ]


data HieAST a =
  Node
    { nodeInfo :: NodeInfo a
    , nodeSpan :: Span
    , nodeChildren :: [HieAST a]
    } deriving (Functor, Foldable, Traversable)

instance Binary (HieAST TypeIndex) where
  put_ bh ast = do
    put_ bh $ nodeInfo ast
    put_ bh $ nodeSpan ast
    put_ bh $ nodeChildren ast

  get bh = Node
    <$> get bh
    <*> get bh
    <*> get bh

instance Outputable a => Outputable (HieAST a) where
  ppr (Node ni sp ch) = hang header 2 rest
    where
      header = text "Node@" O.<> ppr sp O.<> ":" <+> ppr ni
      rest = vcat (map ppr ch)

-- | The information stored in one AST node.
--
-- The type parameter exists to provide flexibility in representation of types
-- (see Note [Efficient serialization of redundant type info]).
data NodeInfo a = NodeInfo
    { nodeAnnotations :: S.Set (FastString,FastString)
    -- ^ (name of the AST node constructor, name of the AST node Type)

    , nodeType :: [a]
    -- ^ The Haskell types of this node, if any.

    , nodeIdentifiers :: NodeIdentifiers a
    -- ^ All the identifiers and their details
    } deriving (Functor, Foldable, Traversable)

instance Binary (NodeInfo TypeIndex) where
  put_ bh ni = do
    put_ bh $ S.toAscList $ nodeAnnotations ni
    put_ bh $ nodeType ni
    put_ bh $ M.toList $ nodeIdentifiers ni
  get bh = NodeInfo
    <$> fmap (S.fromDistinctAscList) (get bh)
    <*> get bh
    <*> fmap (M.fromList) (get bh)

instance Outputable a => Outputable (NodeInfo a) where
  ppr (NodeInfo anns typs idents) = braces $ fsep $ punctuate ", "
    [ parens (text "annotations:" <+> ppr anns)
    , parens (text "types:" <+> ppr typs)
    , parens (text "identifier info:" <+> pprNodeIdents idents)
    ]

pprNodeIdents :: Outputable a => NodeIdentifiers a -> SDoc
pprNodeIdents ni = braces $ fsep $ punctuate ", " $ map go $ M.toList ni
  where
    go (i,id) = parens $ hsep $ punctuate ", " [pprIdentifier i, ppr id]

pprIdentifier :: Identifier -> SDoc
pprIdentifier (Left mod) = text "module" <+> ppr mod
pprIdentifier (Right name) = text "name" <+> ppr name

type Identifier = Either ModuleName Name

type NodeIdentifiers a = M.Map Identifier (IdentifierDetails a)

-- | Information associated with every identifier
--
-- We need to include types with identifiers because sometimes multiple
-- identifiers occur in the same span(Overloaded Record Fields and so on)
data IdentifierDetails a = IdentifierDetails
  { identType :: Maybe a
  , identInfo :: S.Set ContextInfo
  } deriving (Eq, Functor, Foldable, Traversable)

instance Outputable a => Outputable (IdentifierDetails a) where
  ppr x = text "Details: " <+> ppr (identType x) <+> ppr (identInfo x)

instance Semigroup (IdentifierDetails a) where
  d1 <> d2 = IdentifierDetails (identType d1 <|> identType d2)
                               (S.union (identInfo d1) (identInfo d2))

instance Monoid (IdentifierDetails a) where
  mempty = IdentifierDetails Nothing S.empty

instance Binary (IdentifierDetails TypeIndex) where
  put_ bh dets = do
    put_ bh $ identType dets
    put_ bh $ S.toAscList $ identInfo dets
  get bh =  IdentifierDetails
    <$> get bh
    <*> fmap S.fromDistinctAscList (get bh)


-- | Different contexts under which identifiers exist
data ContextInfo
  = Use                -- ^ regular variable
  | MatchBind
  | IEThing IEType     -- ^ import/export
  | TyDecl

  -- | Value binding
  | ValBind
      BindType     -- ^ whether or not the binding is in an instance
      Scope        -- ^ scope over which the value is bound
      (Maybe Span) -- ^ span of entire binding

  -- | Pattern binding
  --
  -- This case is tricky because the bound identifier can be used in two
  -- distinct scopes. Consider the following example (with @-XViewPatterns@)
  --
  -- @
  -- do (b, a, (a -> True)) <- bar
  --    foo a
  -- @
  --
  -- The identifier @a@ has two scopes: in the view pattern @(a -> True)@ and
  -- in the rest of the @do@-block in @foo a@.
  | PatternBind
      Scope        -- ^ scope /in the pattern/ (the variable bound can be used
                   -- further in the pattern)
      Scope        -- ^ rest of the scope outside the pattern
      (Maybe Span) -- ^ span of entire binding

  | ClassTyDecl (Maybe Span)

  -- | Declaration
  | Decl
      DeclType     -- ^ type of declaration
      (Maybe Span) -- ^ span of entire binding

  -- | Type variable
  | TyVarBind Scope TyVarScope

  -- | Record field
  | RecField RecFieldContext (Maybe Span)
    deriving (Eq, Ord)

instance Outputable ContextInfo where
 ppr (Use) = text "usage"
 ppr (MatchBind) = text "LHS of a match group"
 ppr (IEThing x) = ppr x
 ppr (TyDecl) = text "bound in a type signature declaration"
 ppr (ValBind t sc sp) =
   ppr t <+> text "value bound with scope:" <+> ppr sc <+> pprBindSpan sp
 ppr (PatternBind sc1 sc2 sp) =
   text "bound in a pattern with scope:"
     <+> ppr sc1 <+> "," <+> ppr sc2
     <+> pprBindSpan sp
 ppr (ClassTyDecl sp) =
   text "bound in a class type declaration" <+> pprBindSpan sp
 ppr (Decl d sp) =
   text "declaration of" <+> ppr d <+> pprBindSpan sp
 ppr (TyVarBind sc1 sc2) =
   text "type variable binding with scope:"
     <+> ppr sc1 <+> "," <+> ppr sc2
 ppr (RecField ctx sp) =
   text "record field" <+> ppr ctx <+> pprBindSpan sp

pprBindSpan :: Maybe Span -> SDoc
pprBindSpan Nothing = text ""
pprBindSpan (Just sp) = text "at:" <+> ppr sp

instance Binary ContextInfo where
  put_ bh Use = putByte bh 0
  put_ bh (IEThing t) = do
    putByte bh 1
    put_ bh t
  put_ bh TyDecl = putByte bh 2
  put_ bh (ValBind bt sc msp) = do
    putByte bh 3
    put_ bh bt
    put_ bh sc
    put_ bh msp
  put_ bh (PatternBind a b c) = do
    putByte bh 4
    put_ bh a
    put_ bh b
    put_ bh c
  put_ bh (ClassTyDecl sp) = do
    putByte bh 5
    put_ bh sp
  put_ bh (Decl a b) = do
    putByte bh 6
    put_ bh a
    put_ bh b
  put_ bh (TyVarBind a b) = do
    putByte bh 7
    put_ bh a
    put_ bh b
  put_ bh (RecField a b) = do
    putByte bh 8
    put_ bh a
    put_ bh b
  put_ bh MatchBind = putByte bh 9

  get bh = do
    (t :: Word8) <- get bh
    case t of
      0 -> return Use
      1 -> IEThing <$> get bh
      2 -> return TyDecl
      3 -> ValBind <$> get bh <*> get bh <*> get bh
      4 -> PatternBind <$> get bh <*> get bh <*> get bh
      5 -> ClassTyDecl <$> get bh
      6 -> Decl <$> get bh <*> get bh
      7 -> TyVarBind <$> get bh <*> get bh
      8 -> RecField <$> get bh <*> get bh
      9 -> return MatchBind
      _ -> panic "Binary ContextInfo: invalid tag"

-- | Types of imports and exports
data IEType
  = Import
  | ImportAs
  | ImportHiding
  | Export
    deriving (Eq, Enum, Ord)

instance Outputable IEType where
  ppr Import = text "import"
  ppr ImportAs = text "import as"
  ppr ImportHiding = text "import hiding"
  ppr Export = text "export"

instance Binary IEType where
  put_ bh b = putByte bh (fromIntegral (fromEnum b))
  get bh = do x <- getByte bh; pure $! (toEnum (fromIntegral x))


data RecFieldContext
  = RecFieldDecl
  | RecFieldAssign
  | RecFieldMatch
  | RecFieldOcc
    deriving (Eq, Enum, Ord)

instance Outputable RecFieldContext where
  ppr RecFieldDecl = text "declaration"
  ppr RecFieldAssign = text "assignment"
  ppr RecFieldMatch = text "pattern match"
  ppr RecFieldOcc = text "occurence"

instance Binary RecFieldContext where
  put_ bh b = putByte bh (fromIntegral (fromEnum b))
  get bh = do x <- getByte bh; pure $! (toEnum (fromIntegral x))


data BindType
  = RegularBind
  | InstanceBind
    deriving (Eq, Ord, Enum)

instance Outputable BindType where
  ppr RegularBind = "regular"
  ppr InstanceBind = "instance"

instance Binary BindType where
  put_ bh b = putByte bh (fromIntegral (fromEnum b))
  get bh = do x <- getByte bh; pure $! (toEnum (fromIntegral x))

data DeclType
  = FamDec     -- ^ type or data family
  | SynDec     -- ^ type synonym
  | DataDec    -- ^ data declaration
  | ConDec     -- ^ constructor declaration
  | PatSynDec  -- ^ pattern synonym
  | ClassDec   -- ^ class declaration
  | InstDec    -- ^ instance declaration
    deriving (Eq, Ord, Enum)

instance Outputable DeclType where
  ppr FamDec = text "type or data family"
  ppr SynDec = text "type synonym"
  ppr DataDec = text "data"
  ppr ConDec = text "constructor"
  ppr PatSynDec = text "pattern synonym"
  ppr ClassDec = text "class"
  ppr InstDec = text "instance"

instance Binary DeclType where
  put_ bh b = putByte bh (fromIntegral (fromEnum b))
  get bh = do x <- getByte bh; pure $! (toEnum (fromIntegral x))

data Scope
  = NoScope
  | LocalScope Span
  | ModuleScope
    deriving (Eq, Ord, Typeable, Data)

instance Outputable Scope where
  ppr NoScope = text "NoScope"
  ppr (LocalScope sp) = text "LocalScope" <+> ppr sp
  ppr ModuleScope = text "ModuleScope"

instance Binary Scope where
  put_ bh NoScope = putByte bh 0
  put_ bh (LocalScope span) = do
    putByte bh 1
    put_ bh span
  put_ bh ModuleScope = putByte bh 2

  get bh = do
    (t :: Word8) <- get bh
    case t of
      0 -> return NoScope
      1 -> LocalScope <$> get bh
      2 -> return ModuleScope
      _ -> panic "Binary Scope: invalid tag"


-- | Scope of a type variable.
--
-- This warrants a data type apart from 'Scope' because of complexities
-- introduced by features like @-XScopedTypeVariables@ and @-XInstanceSigs@. For
-- example, consider:
--
-- @
-- foo, bar, baz :: forall a. a -> a
-- @
--
-- Here @a@ is in scope in all the definitions of @foo@, @bar@, and @baz@, so we
-- need a list of scopes to keep track of this. Furthermore, this list cannot be
-- computed until we resolve the binding sites of @foo@, @bar@, and @baz@.
--
-- Consequently, @a@ starts with an @'UnresolvedScope' [foo, bar, baz] Nothing@
-- which later gets resolved into a 'ResolvedScopes'.
data TyVarScope
  = ResolvedScopes [Scope]

  -- | Unresolved scopes should never show up in the final @.hie@ file
  | UnresolvedScope
        [Name]        -- ^ names of the definitions over which the scope spans
        (Maybe Span)  -- ^ the location of the instance/class declaration for
                      -- the case where the type variable is declared in a
                      -- method type signature
    deriving (Eq, Ord)

instance Outputable TyVarScope where
  ppr (ResolvedScopes xs) =
    text "type variable scopes:" <+> hsep (punctuate ", " $ map ppr xs)
  ppr (UnresolvedScope ns sp) =
    text "unresolved type variable scope for name" O.<> plural ns
      <+> pprBindSpan sp

instance Binary TyVarScope where
  put_ bh (ResolvedScopes xs) = do
    putByte bh 0
    put_ bh xs
  put_ bh (UnresolvedScope ns span) = do
    putByte bh 1
    put_ bh ns
    put_ bh span

  get bh = do
    (t :: Word8) <- get bh
    case t of
      0 -> ResolvedScopes <$> get bh
      1 -> UnresolvedScope <$> get bh <*> get bh
      _ -> panic "Binary TyVarScope: invalid tag"
