{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-
Types for the .hie file format are defined here.

For more information see https://gitlab.haskell.org/ghc/ghc/wikis/hie-files
-}

module GHC.Iface.Ext.Types where

import GHC.Prelude

import GHC.Settings.Config
import GHC.Utils.Binary
import GHC.Data.FastString
import GHC.Builtin.Utils
import GHC.Iface.Type
import GHC.Unit.Module            ( ModuleName, Module )
import GHC.Types.Name
import GHC.Utils.Outputable hiding ( (<>) )
import GHC.Types.SrcLoc
import GHC.Types.Avail
import GHC.Types.Unique
import qualified GHC.Utils.Outputable as O ( (<>) )
import GHC.Utils.Panic
import GHC.Core.ConLike           (ConLike(..))
import GHC.Core.TyCo.Rep          (Type(..))
import GHC.Core.Type              (coreFullView, isFunTy, Var (..))
import GHC.Core.TyCon             ( isTypeSynonymTyCon, isClassTyCon, isFamilyTyCon )
import GHC.Types.Id               ( Id, isRecordSelector, isClassOpId )
import GHC.Types.TyThing          (TyThing (..))
import GHC.Types.Var              (isTyVar, isFUNArg)

import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.Set as S
import Data.ByteString            ( ByteString )
import Data.Data                  ( Data )
import Data.Semigroup             ( Semigroup(..) )
import Data.Word                  ( Word8 )
import Control.Applicative        ( (<|>) )
import Data.Coerce                ( coerce  )
import Data.Function              ( on )
import qualified Data.Semigroup as S

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

    , hie_entity_infos :: M.Map Identifier (S.Set EntityInfo)
    -- ^ Entity information for each identifier in the `hie_asts`
    }

instance Binary (M.Map Identifier (S.Set EntityInfo)) where
  put_ bh m = put_ bh $ M.toAscList m
  get bh = fmap M.fromDistinctAscList (get bh)

instance Binary HieFile where
  put_ bh hf = do
    put_ bh $ hie_hs_file hf
    put_ bh $ hie_module hf
    put_ bh $ hie_types hf
    put_ bh $ hie_asts hf
    put_ bh $ hie_exports hf
    put_ bh $ hie_hs_src hf
    put_ bh $ hie_entity_infos hf

  get bh = HieFile
    <$> get bh
    <*> get bh
    <*> get bh
    <*> get bh
    <*> get bh
    <*> get bh
    <*> get bh

getIdentifierEntityInfo :: HieFile -> Identifier -> S.Set EntityInfo
getIdentifierEntityInfo hf ident = M.findWithDefault S.empty ident (hie_entity_infos hf)


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
  | HForAllTy ((Name, a),ForAllTyFlag) a
  | HFunTy a a a
  | HQualTy a a           -- ^ type with constraint: @t1 => t2@ (see 'IfaceDFunTy')
  | HLitTy IfaceTyLit
  | HCastTy a
  | HCoercionTy
    deriving (Functor, Foldable, Traversable, Eq)

type HieTypeFlat = HieType TypeIndex

-- | Roughly isomorphic to the original core 'Type'.
newtype HieTypeFix = Roll (HieType (HieTypeFix))
  deriving Eq

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
  put_ bh (HFunTy w a b) = do
    putByte bh 4
    put_ bh w
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
      4 -> HFunTy <$> get bh <*> get bh <*> get bh
      5 -> HQualTy <$> get bh <*> get bh
      6 -> HLitTy <$> get bh
      7 -> HCastTy <$> get bh
      8 -> return HCoercionTy
      _ -> panic "Binary (HieArgs Int): invalid tag"


-- | A list of type arguments along with their respective visibilities (ie. is
-- this an argument that would return 'True' for 'isVisibleForAllTyFlag'?).
newtype HieArgs a = HieArgs [(Bool,a)]
  deriving (Functor, Foldable, Traversable, Eq)

instance Binary (HieArgs TypeIndex) where
  put_ bh (HieArgs xs) = put_ bh xs
  get bh = HieArgs <$> get bh


-- A HiePath is just a lexical FastString. We use a lexical FastString to avoid
-- non-determinism when printing or storing HieASTs which are sorted by their
-- HiePath.
type HiePath = LexicalFastString

{-# COMPLETE HiePath #-}
pattern HiePath :: FastString -> HiePath
pattern HiePath fs = LexicalFastString fs

-- | Mapping from filepaths to the corresponding AST
newtype HieASTs a = HieASTs { getAsts :: M.Map HiePath (HieAST a) }
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
    { sourcedNodeInfo :: SourcedNodeInfo a
    , nodeSpan :: Span
    , nodeChildren :: [HieAST a]
    } deriving (Functor, Foldable, Traversable)

instance Binary (HieAST TypeIndex) where
  put_ bh ast = do
    put_ bh $ sourcedNodeInfo ast
    put_ bh $ BinSpan $ nodeSpan ast
    put_ bh $ nodeChildren ast

  get bh = Node
    <$> get bh
    <*> (unBinSpan <$> get bh)
    <*> get bh

instance Outputable a => Outputable (HieAST a) where
  ppr (Node ni sp ch) = hang header 2 rest
    where
      header = text "Node@" O.<> ppr sp O.<> ":" <+> ppr ni
      rest = vcat (map ppr ch)


-- | NodeInfos grouped by source
newtype SourcedNodeInfo a = SourcedNodeInfo { getSourcedNodeInfo :: (M.Map NodeOrigin (NodeInfo a)) }
  deriving (Functor, Foldable, Traversable)

instance Binary (SourcedNodeInfo TypeIndex) where
  put_ bh asts = put_ bh $ M.toAscList $ getSourcedNodeInfo asts
  get bh = SourcedNodeInfo <$> fmap M.fromDistinctAscList (get bh)

instance Outputable a => Outputable (SourcedNodeInfo a) where
  ppr (SourcedNodeInfo asts) = M.foldrWithKey go "" asts
    where
      go k a rest = vcat $
        [ "Source: " O.<> ppr k
        , ppr a
        , rest
        ]

-- | Source of node info
data NodeOrigin
  = SourceInfo
  | GeneratedInfo
    deriving (Eq, Enum, Ord)

instance Outputable NodeOrigin where
  ppr SourceInfo = text "From source"
  ppr GeneratedInfo = text "generated by ghc"

instance Binary NodeOrigin where
  put_ bh b = putByte bh (fromIntegral (fromEnum b))
  get bh = do x <- getByte bh; pure $! (toEnum (fromIntegral x))

-- | A node annotation
data NodeAnnotation = NodeAnnotation
   { nodeAnnotConstr :: !FastString -- ^ name of the AST node constructor
   , nodeAnnotType   :: !FastString -- ^ name of the AST node Type
   }
   deriving (Eq)

instance Ord NodeAnnotation where
   compare (NodeAnnotation c0 t0) (NodeAnnotation c1 t1)
      = mconcat [lexicalCompareFS c0 c1, lexicalCompareFS t0 t1]

instance Outputable NodeAnnotation where
   ppr (NodeAnnotation c t) = ppr (c,t)

instance Binary NodeAnnotation where
  put_ bh (NodeAnnotation c t) = do
    put_ bh c
    put_ bh t
  get bh = NodeAnnotation
    <$> get bh
    <*> get bh

-- | The information stored in one AST node.
--
-- The type parameter exists to provide flexibility in representation of types
-- (see Note [Efficient serialization of redundant type info]).
data NodeInfo a = NodeInfo
    { nodeAnnotations :: S.Set NodeAnnotation
    -- ^ Annotations

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
    put_ bh $ S.toList $ identInfo dets
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
  -- | Constraint/Dictionary evidence variable binding
  | EvidenceVarBind
      EvVarSource  -- ^ how did this bind come into being
      Scope        -- ^ scope over which the value is bound
      (Maybe Span) -- ^ span of the binding site

  -- | Usage of evidence variable
  | EvidenceVarUse
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
 ppr (EvidenceVarBind ctx sc sp) =
   text "evidence variable" <+> ppr ctx
     $$ "with scope:" <+> ppr sc
     $$ pprBindSpan sp
 ppr (EvidenceVarUse) =
   text "usage of evidence variable"

pprBindSpan :: Maybe Span -> SDoc
pprBindSpan Nothing = text ""
pprBindSpan (Just sp) = text "bound at:" <+> ppr sp

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
    put_ bh $ BinSpan <$> msp
  put_ bh (PatternBind a b c) = do
    putByte bh 4
    put_ bh a
    put_ bh b
    put_ bh $ BinSpan <$> c
  put_ bh (ClassTyDecl sp) = do
    putByte bh 5
    put_ bh $ BinSpan <$> sp
  put_ bh (Decl a b) = do
    putByte bh 6
    put_ bh a
    put_ bh $ BinSpan <$> b
  put_ bh (TyVarBind a b) = do
    putByte bh 7
    put_ bh a
    put_ bh b
  put_ bh (RecField a b) = do
    putByte bh 8
    put_ bh a
    put_ bh $ BinSpan <$> b
  put_ bh MatchBind = putByte bh 9
  put_ bh (EvidenceVarBind a b c) = do
    putByte bh 10
    put_ bh a
    put_ bh b
    put_ bh $ BinSpan <$> c
  put_ bh EvidenceVarUse = putByte bh 11

  get bh = do
    (t :: Word8) <- get bh
    case t of
      0 -> return Use
      1 -> IEThing <$> get bh
      2 -> return TyDecl
      3 -> ValBind <$> get bh <*> get bh <*> (fmap unBinSpan <$> get bh)
      4 -> PatternBind <$> get bh <*> get bh <*> (fmap unBinSpan <$> get bh)
      5 -> ClassTyDecl <$> (fmap unBinSpan <$> get bh)
      6 -> Decl <$> get bh <*> (fmap unBinSpan <$> get bh)
      7 -> TyVarBind <$> get bh <*> get bh
      8 -> RecField <$> get bh <*> (fmap unBinSpan <$> get bh)
      9 -> return MatchBind
      10 -> EvidenceVarBind <$> get bh <*> get bh <*> (fmap unBinSpan <$> get bh)
      11 -> return EvidenceVarUse
      _ -> panic "Binary ContextInfo: invalid tag"

data EvVarSource
  = EvPatternBind -- ^ bound by a pattern match
  | EvSigBind -- ^ bound by a type signature
  | EvWrapperBind -- ^ bound by a hswrapper
  | EvImplicitBind -- ^ bound by an implicit variable
  | EvInstBind { isSuperInst :: Bool, cls :: Name } -- ^ Bound by some instance of given class
  | EvLetBind EvBindDeps -- ^ A direct let binding
  deriving (Eq,Ord)

instance Binary EvVarSource where
  put_ bh EvPatternBind = putByte bh 0
  put_ bh EvSigBind = putByte bh 1
  put_ bh EvWrapperBind = putByte bh 2
  put_ bh EvImplicitBind = putByte bh 3
  put_ bh (EvInstBind b cls) = do
    putByte bh 4
    put_ bh b
    put_ bh cls
  put_ bh (EvLetBind deps) = do
    putByte bh 5
    put_ bh deps

  get bh = do
    (t :: Word8) <- get bh
    case t of
      0 -> pure EvPatternBind
      1 -> pure EvSigBind
      2 -> pure EvWrapperBind
      3 -> pure EvImplicitBind
      4 -> EvInstBind <$> get bh <*> get bh
      5 -> EvLetBind <$> get bh
      _ -> panic "Binary EvVarSource: invalid tag"

instance Outputable EvVarSource where
  ppr EvPatternBind = text "bound by a pattern"
  ppr EvSigBind = text "bound by a type signature"
  ppr EvWrapperBind = text "bound by a HsWrapper"
  ppr EvImplicitBind = text "bound by an implicit variable binding"
  ppr (EvInstBind False cls) = text "bound by an instance of class" <+> ppr cls
  ppr (EvInstBind True cls) = text "bound due to a superclass of " <+> ppr cls
  ppr (EvLetBind deps) = text "bound by a let, depending on:" <+> ppr deps

-- | Eq/Ord instances compare on the converted HieName,
-- as non-exported names may have different uniques after
-- a roundtrip
newtype EvBindDeps = EvBindDeps { getEvBindDeps :: [Name] }
  deriving Outputable

instance Eq EvBindDeps where
  (==) = coerce ((==) `on` map toHieName)

instance Ord EvBindDeps where
  compare = coerce (compare `on` map toHieName)

instance Binary EvBindDeps where
  put_ bh (EvBindDeps xs) = put_ bh xs
  get bh = EvBindDeps <$> get bh


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
  ppr RecFieldOcc = text "occurrence"

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
    deriving (Eq, Ord, Data)

instance Outputable Scope where
  ppr NoScope = text "NoScope"
  ppr (LocalScope sp) = text "LocalScope" <+> ppr sp
  ppr ModuleScope = text "ModuleScope"

instance Binary Scope where
  put_ bh NoScope = putByte bh 0
  put_ bh (LocalScope span) = do
    putByte bh 1
    put_ bh $ BinSpan span
  put_ bh ModuleScope = putByte bh 2

  get bh = do
    (t :: Word8) <- get bh
    case t of
      0 -> return NoScope
      1 -> LocalScope . unBinSpan <$> get bh
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
    put_ bh (BinSpan <$> span)

  get bh = do
    (t :: Word8) <- get bh
    case t of
      0 -> ResolvedScopes <$> get bh
      1 -> UnresolvedScope <$> get bh <*> (fmap unBinSpan <$> get bh)
      _ -> panic "Binary TyVarScope: invalid tag"

-- | `Name`'s get converted into `HieName`'s before being written into @.hie@
-- files. See 'toHieName' and 'fromHieName' for logic on how to convert between
-- these two types.
data HieName
  = ExternalName !Module !OccName !SrcSpan
  | LocalName !OccName !SrcSpan
  | KnownKeyName !Unique
  deriving (Eq)

instance Ord HieName where
  compare (ExternalName a b c) (ExternalName d e f) = compare (a,b) (d,e) S.<> leftmost_smallest c f
    -- TODO (int-index): Perhaps use RealSrcSpan in HieName?
  compare (LocalName a b) (LocalName c d) = compare a c S.<> leftmost_smallest b d
    -- TODO (int-index): Perhaps use RealSrcSpan in HieName?
  compare (KnownKeyName a) (KnownKeyName b) = nonDetCmpUnique a b
    -- Not actually non deterministic as it is a KnownKey
  compare ExternalName{} _ = LT
  compare LocalName{} ExternalName{} = GT
  compare LocalName{} _ = LT
  compare KnownKeyName{} _ = GT

instance Outputable HieName where
  ppr (ExternalName m n sp) = text "ExternalName" <+> ppr m <+> ppr n <+> ppr sp
  ppr (LocalName n sp) = text "LocalName" <+> ppr n <+> ppr sp
  ppr (KnownKeyName u) = text "KnownKeyName" <+> ppr u

hieNameOcc :: HieName -> OccName
hieNameOcc (ExternalName _ occ _) = occ
hieNameOcc (LocalName occ _) = occ
hieNameOcc (KnownKeyName u) =
  case lookupKnownKeyName u of
    Just n -> nameOccName n
    Nothing -> pprPanic "hieNameOcc:unknown known-key unique"
                        (ppr u)

toHieName :: Name -> HieName
toHieName name
  | isKnownKeyName name = KnownKeyName (nameUnique name)
  | isExternalName name = ExternalName (nameModule name)
                                       (nameOccName name)
                                       (removeBufSpan $ nameSrcSpan name)
  | otherwise = LocalName (nameOccName name) (removeBufSpan $ nameSrcSpan name)


{- Note [Capture Entity Information]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to capture the entity information for the identifier in HieAst, so that
language tools and protocols can take advantage making use of it.

Capture `EntityInfo` for a `Name` or `Id` in `renamedSource` or `typecheckedSource`
if it is a name, we ask the env for the `TyThing` then compute the `EntityInfo` from tyThing
if it is an Id, we compute the `EntityInfo` directly from Id

see issue #24544 for more details
-}


-- | Entity information
-- `EntityInfo` is a simplified version of `TyThing` and richer version than `Namespace` in `OccName`.
-- It state the kind of the entity, such as `Variable`, `TypeVariable`, `DataConstructor`, etc..
data EntityInfo
  = EntityVariable
  | EntityFunction
  | EntityDataConstructor
  | EntityTypeVariable
  | EntityClassMethod
  | EntityPatternSynonym
  | EntityTypeConstructor
  | EntityTypeClass
  | EntityTypeSynonym
  | EntityTypeFamily
  | EntityRecordField
  | EntityModuleName
  deriving (Eq, Ord, Enum, Show)


instance Outputable EntityInfo where
  ppr EntityVariable = text "variable"
  ppr EntityFunction = text "function"
  ppr EntityDataConstructor = text "data constructor"
  ppr EntityTypeVariable = text "type variable"
  ppr EntityClassMethod = text "class method"
  ppr EntityPatternSynonym = text "pattern synonym"
  ppr EntityTypeConstructor = text "type constructor"
  ppr EntityTypeClass = text "type class"
  ppr EntityTypeSynonym = text "type synonym"
  ppr EntityTypeFamily = text "type family"
  ppr EntityRecordField = text "record field"
  ppr EntityModuleName = text "module name"


instance Binary EntityInfo where
  put_ bh b = putByte bh (fromIntegral (fromEnum b))
  get bh = do x <- getByte bh; pure $! toEnum (fromIntegral x)


-- | Get the `EntityInfo` for an `Id`
idEntityInfo :: Id -> S.Set EntityInfo
idEntityInfo vid = S.fromList $ [EntityTypeVariable | isTyVar vid] <> [EntityFunction | isFunType $ varType vid]
  <> [EntityRecordField | isRecordSelector vid] <> [EntityClassMethod | isClassOpId vid] <> [EntityVariable]
  where
    isFunType a = case coreFullView a of
      ForAllTy _ t    -> isFunType t
      FunTy { ft_af = flg, ft_res = rhs } -> isFUNArg flg || isFunType rhs
      _x              -> isFunTy a

-- | Get the `EntityInfo` for a `TyThing`
tyThingEntityInfo :: TyThing -> S.Set EntityInfo
tyThingEntityInfo ty = case ty of
  AnId vid -> idEntityInfo vid
  AConLike con -> case con of
    RealDataCon _ -> S.singleton EntityDataConstructor
    PatSynCon _   -> S.singleton EntityPatternSynonym
  ATyCon tyCon -> S.fromList $ [EntityTypeSynonym | isTypeSynonymTyCon tyCon] <> [EntityTypeFamily | isFamilyTyCon tyCon]
                  <> [EntityTypeClass | isClassTyCon tyCon] <> [EntityTypeConstructor]
  ACoAxiom _ -> S.empty

nameEntityInfo :: Name -> S.Set EntityInfo
nameEntityInfo name
  | isTyVarName name = S.fromList [EntityVariable, EntityTypeVariable]
  | isDataConName name = S.singleton EntityDataConstructor
  | isTcClsNameSpace (occNameSpace $ occName name) = S.singleton EntityTypeConstructor
  | isFieldName name = S.fromList [EntityVariable, EntityRecordField]
  | isVarName name = S.fromList [EntityVariable]
  | otherwise = S.empty
