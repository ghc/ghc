{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable, StandaloneDeriving, TypeFamilies, RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Types
-- Copyright   :  (c) Simon Marlow      2003-2006,
--                    David Waern       2006-2009,
--                    Mateusz Kowalczyk 2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskellorg
-- Stability   :  experimental
-- Portability :  portable
--
-- Types that are commonly used through-out Haddock. Some of the most
-- important types are defined here, like 'Interface' and 'DocName'.
-----------------------------------------------------------------------------
module Haddock.Types (
  module Haddock.Types
  , HsDocString, LHsDocString
  , Fixity(..)
  , module Documentation.Haddock.Types

  -- $ Reexports
  , runWriter
  , tell
 ) where

import Control.DeepSeq
import Control.Exception (throw)
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Writer.Strict (Writer, WriterT, MonadWriter(..), lift, runWriter, runWriterT)
import Data.Typeable (Typeable)
import Data.Map (Map)
import Data.Data (Data)
import Data.Void (Void)
import Documentation.Haddock.Types
import GHC.Types.Basic (PromotionFlag(..))
import GHC.Types.Fixity (Fixity(..))
import GHC.Types.Var (Specificity)

import GHC
import GHC.Driver.Session (Language)
import qualified GHC.LanguageExtensions as LangExt
import GHC.Types.Name.Occurrence
import GHC.Utils.Outputable

-----------------------------------------------------------------------------
-- * Convenient synonyms
-----------------------------------------------------------------------------


type IfaceMap      = Map Module Interface
type InstIfaceMap  = Map Module InstalledInterface  -- TODO: rename
type DocMap a      = Map Name (MDoc a)
type ArgMap a      = Map Name (Map Int (MDoc a))
type SubMap        = Map Name [Name]
type DeclMap       = Map Name [LHsDecl GhcRn]
type InstMap       = Map RealSrcSpan Name
type FixMap        = Map Name Fixity
type DocPaths      = (FilePath, Maybe FilePath) -- paths to HTML and sources


-----------------------------------------------------------------------------
-- * Interface
-----------------------------------------------------------------------------


-- | 'Interface' holds all information used to render a single Haddock page.
-- It represents the /interface/ of a module. The core business of Haddock
-- lies in creating this structure. Note that the record contains some fields
-- that are only used to create the final record, and that are not used by the
-- backends.
data Interface = Interface
  {
    -- | The module behind this interface.
    ifaceMod             :: !Module

    -- | Is this a signature?
  , ifaceIsSig           :: !Bool

    -- | Original file name of the module.
  , ifaceOrigFilename    :: !FilePath

    -- | Textual information about the module.
  , ifaceInfo            :: !(HaddockModInfo Name)

    -- | Documentation header.
  , ifaceDoc             :: !(Documentation Name)

    -- | Documentation header with cross-reference information.
  , ifaceRnDoc           :: !(Documentation DocName)

    -- | Haddock options for this module (prune, ignore-exports, etc).
  , ifaceOptions         :: ![DocOption]

    -- | Declarations originating from the module. Excludes declarations without
    -- names (instances and stand-alone documentation comments). Includes
    -- names of subordinate declarations mapped to their parent declarations.
  , ifaceDeclMap         :: !(Map Name [LHsDecl GhcRn])

    -- | Documentation of declarations originating from the module (including
    -- subordinates).
  , ifaceDocMap          :: !(DocMap Name)
  , ifaceArgMap          :: !(ArgMap Name)

    -- | Documentation of declarations originating from the module (including
    -- subordinates).
  , ifaceRnDocMap        :: !(DocMap DocName)
  , ifaceRnArgMap        :: !(ArgMap DocName)

  , ifaceFixMap          :: !(Map Name Fixity)

  , ifaceExportItems     :: ![ExportItem GhcRn]
  , ifaceRnExportItems   :: ![ExportItem DocNameI]

    -- | All names exported by the module.
  , ifaceExports         :: ![Name]

    -- | All \"visible\" names exported by the module.
    -- A visible name is a name that will show up in the documentation of the
    -- module.
  , ifaceVisibleExports  :: ![Name]

    -- | Aliases of module imports as in @import A.B.C as C@.
  , ifaceModuleAliases   :: !AliasMap

    -- | Instances exported by the module.
  , ifaceInstances       :: ![ClsInst]
  , ifaceFamInstances    :: ![FamInst]

    -- | Orphan instances
  , ifaceOrphanInstances :: ![DocInstance GhcRn]
  , ifaceRnOrphanInstances :: ![DocInstance DocNameI]

    -- | The number of haddockable and haddocked items in the module, as a
    -- tuple. Haddockable items are the exports and the module itself.
  , ifaceHaddockCoverage :: !(Int, Int)

    -- | Warnings for things defined in this module.
  , ifaceWarningMap :: !WarningMap

    -- | Tokenized source code of module (avaliable if Haddock is invoked with
    -- source generation flag).
  , ifaceHieFile :: !(Maybe FilePath)
  , ifaceDynFlags :: !DynFlags
  }

type WarningMap = Map Name (Doc Name)


-- | A subset of the fields of 'Interface' that we store in the interface
-- files.
data InstalledInterface = InstalledInterface
  {
    -- | The module represented by this interface.
    instMod              :: Module

    -- | Is this a signature?
  , instIsSig            :: Bool

    -- | Textual information about the module.
  , instInfo             :: HaddockModInfo Name

    -- | Documentation of declarations originating from the module (including
    -- subordinates).
  , instDocMap           :: DocMap Name

  , instArgMap           :: ArgMap Name

    -- | All names exported by this module.
  , instExports          :: [Name]

    -- | All \"visible\" names exported by the module.
    -- A visible name is a name that will show up in the documentation of the
    -- module.
  , instVisibleExports   :: [Name]

    -- | Haddock options for this module (prune, ignore-exports, etc).
  , instOptions          :: [DocOption]

  , instFixMap           :: Map Name Fixity
  }


-- | Convert an 'Interface' to an 'InstalledInterface'
toInstalledIface :: Interface -> InstalledInterface
toInstalledIface interface = InstalledInterface
  { instMod              = ifaceMod              interface
  , instIsSig            = ifaceIsSig            interface
  , instInfo             = ifaceInfo             interface
  , instDocMap           = ifaceDocMap           interface
  , instArgMap           = ifaceArgMap           interface
  , instExports          = ifaceExports          interface
  , instVisibleExports   = ifaceVisibleExports   interface
  , instOptions          = ifaceOptions          interface
  , instFixMap           = ifaceFixMap           interface
  }


-----------------------------------------------------------------------------
-- * Export items & declarations
-----------------------------------------------------------------------------


data ExportItem name

  -- | An exported declaration.
  = ExportDecl
      {
        -- | A declaration.
        expItemDecl :: !(LHsDecl name)

        -- | Bundled patterns for a data type declaration
      , expItemPats :: ![(HsDecl name, DocForDecl (IdP name))]

        -- | Maybe a doc comment, and possibly docs for arguments (if this
        -- decl is a function or type-synonym).
      , expItemMbDoc :: !(DocForDecl (IdP name))

        -- | Subordinate names, possibly with documentation.
      , expItemSubDocs :: ![(IdP name, DocForDecl (IdP name))]

        -- | Instances relevant to this declaration, possibly with
        -- documentation.
      , expItemInstances :: ![DocInstance name]

        -- | Fixity decls relevant to this declaration (including subordinates).
      , expItemFixities :: ![(IdP name, Fixity)]

        -- | Whether the ExportItem is from a TH splice or not, for generating
        -- the appropriate type of Source link.
      , expItemSpliced :: !Bool
      }

  -- | An exported entity for which we have no documentation (perhaps because it
  -- resides in another package).
  | ExportNoDecl
      { expItemName :: !(IdP name)

        -- | Subordinate names.
      , expItemSubs :: ![IdP name]
      }

  -- | A section heading.
  | ExportGroup
      {
        -- | Section level (1, 2, 3, ...).
        expItemSectionLevel :: !Int

        -- | Section id (for hyperlinks).
      , expItemSectionId :: !String

        -- | Section heading text.
      , expItemSectionText :: !(Doc (IdP name))
      }

  -- | Some documentation.
  | ExportDoc !(MDoc (IdP name))

  -- | A cross-reference to another module.
  | ExportModule !Module

data Documentation name = Documentation
  { documentationDoc :: Maybe (MDoc name)
  , documentationWarning :: !(Maybe (Doc name))
  } deriving Functor


-- | Arguments and result are indexed by Int, zero-based from the left,
-- because that's the easiest to use when recursing over types.
type FnArgsDoc name = Map Int (MDoc name)
type DocForDecl name = (Documentation name, FnArgsDoc name)


noDocForDecl :: DocForDecl name
noDocForDecl = (Documentation Nothing Nothing, mempty)


-----------------------------------------------------------------------------
-- * Cross-referencing
-----------------------------------------------------------------------------


-- | Type of environment used to cross-reference identifiers in the syntax.
type LinkEnv = Map Name Module

-- | An 'RdrName' tagged with some type/value namespace information.
data NsRdrName = NsRdrName
  { namespace :: !Namespace
  , rdrName :: !RdrName
  }

-- | Extends 'Name' with cross-reference information.
data DocName
  = Documented Name Module
     -- ^ This thing is part of the (existing or resulting)
     -- documentation. The 'Module' is the preferred place
     -- in the documentation to refer to.
  | Undocumented Name
     -- ^ This thing is not part of the (existing or resulting)
     -- documentation, as far as Haddock knows.
  deriving (Eq, Data)

data DocNameI

type instance NoGhcTc DocNameI = DocNameI

type instance IdP DocNameI = DocName

instance CollectPass DocNameI where
  collectXXPat _ ext = dataConCantHappen ext
  collectXXHsBindsLR ext = dataConCantHappen ext

instance NamedThing DocName where
  getName (Documented name _) = name
  getName (Undocumented name) = name

-- | Useful for debugging
instance Outputable DocName where
  ppr = ppr . getName

instance OutputableBndr DocName where
  pprBndr _ = ppr . getName
  pprPrefixOcc = pprPrefixOcc . getName
  pprInfixOcc = pprInfixOcc . getName

class NamedThing name => SetName name where

    setName :: Name -> name -> name


instance SetName Name where

    setName name' _ = name'


instance SetName DocName where

    setName name' (Documented _ mdl) = Documented name' mdl
    setName name' (Undocumented _) = Undocumented name'

-- | Adds extra "wrapper" information to a name.
--
-- This is to work around the fact that most name types in GHC ('Name', 'RdrName',
-- 'OccName', ...) don't include backticks or parens.
data Wrap n
  = Unadorned { unwrap :: n  }     -- ^ don't do anything to the name
  | Parenthesized { unwrap :: n }  -- ^ add parentheses around the name
  | Backticked { unwrap :: n }     -- ^ add backticks around the name
  deriving (Show, Functor, Foldable, Traversable)

-- | Useful for debugging
instance Outputable n => Outputable (Wrap n) where
  ppr (Unadorned n)     = ppr n
  ppr (Parenthesized n) = hcat [ char '(', ppr n, char ')' ]
  ppr (Backticked n)    = hcat [ char '`', ppr n, char '`' ]

showWrapped :: (a -> String) -> Wrap a -> String
showWrapped f (Unadorned n) = f n
showWrapped f (Parenthesized n) = "(" ++ f n ++ ")"
showWrapped f (Backticked n) = "`" ++ f n ++ "`"

instance HasOccName DocName where

    occName = occName . getName

-----------------------------------------------------------------------------
-- * Instances
-----------------------------------------------------------------------------

-- | The three types of instances
data InstType name
  = ClassInst
      { clsiCtx :: [HsType name]
      , clsiTyVars :: LHsQTyVars name
      , clsiSigs :: [Sig name]
      , clsiAssocTys :: [PseudoFamilyDecl name]
      }
  | TypeInst  (Maybe (HsType name)) -- ^ Body (right-hand side)
  | DataInst (TyClDecl name)        -- ^ Data constructors

instance (OutputableBndrId p)
         => Outputable (InstType (GhcPass p)) where
  ppr (ClassInst { .. }) = text "ClassInst"
      <+> ppr clsiCtx
      <+> ppr clsiTyVars
      <+> ppr clsiSigs
  ppr (TypeInst  a) = text "TypeInst"  <+> ppr a
  ppr (DataInst  a) = text "DataInst"  <+> ppr a


-- | Almost the same as 'FamilyDecl' except for type binders.
--
-- In order to perform type specialization for class instances, we need to
-- substitute class variables to appropriate type. However, type variables in
-- associated type are specified using 'LHsTyVarBndrs' instead of 'HsType'.
-- This makes type substitution impossible and to overcome this issue,
-- 'PseudoFamilyDecl' type is introduced.
data PseudoFamilyDecl name = PseudoFamilyDecl
    { pfdInfo :: FamilyInfo name
    , pfdLName :: LocatedN (IdP name)
    , pfdTyVars :: [LHsType name]
    , pfdKindSig :: LFamilyResultSig name
    }


mkPseudoFamilyDecl :: FamilyDecl GhcRn -> PseudoFamilyDecl GhcRn
mkPseudoFamilyDecl (FamilyDecl { .. }) = PseudoFamilyDecl
    { pfdInfo = fdInfo
    , pfdLName = fdLName
    , pfdTyVars = [ L loc (mkType bndr) | L loc bndr <- hsq_explicit fdTyVars ]
    , pfdKindSig = fdResultSig
    }
  where
    mkType :: HsTyVarBndr flag GhcRn -> HsType GhcRn
    mkType (KindedTyVar _ _ (L loc name) lkind) =
        HsKindSig noAnn tvar lkind
      where
        tvar = L (na2la loc) (HsTyVar noAnn NotPromoted (L loc name))
    mkType (UserTyVar _ _ name) = HsTyVar noAnn NotPromoted name


-- | An instance head that may have documentation and a source location.
type DocInstance name = (InstHead name, Maybe (MDoc (IdP name)), Located (IdP name), Maybe Module)

-- | The head of an instance. Consists of a class name, a list of type
-- parameters (which may be annotated with kinds), and an instance type
data InstHead name = InstHead
    { ihdClsName :: IdP name
    , ihdTypes :: [HsType name]
    , ihdInstType :: InstType name
    }


-- | An instance origin information.
--
-- This is used primarily in HTML backend to generate unique instance
-- identifiers (for expandable sections).
data InstOrigin name
    = OriginClass name
    | OriginData name
    | OriginFamily name


instance NamedThing name => NamedThing (InstOrigin name) where

    getName (OriginClass name) = getName name
    getName (OriginData name) = getName name
    getName (OriginFamily name) = getName name


-----------------------------------------------------------------------------
-- * Documentation comments
-----------------------------------------------------------------------------


type LDoc id = Located (Doc id)

type Doc id = DocH (Wrap (ModuleName, OccName)) (Wrap id)
type MDoc id = MetaDoc (Wrap (ModuleName, OccName)) (Wrap id)

type DocMarkup id a = DocMarkupH (Wrap (ModuleName, OccName)) id a

instance (NFData a, NFData mod)
         => NFData (DocH mod a) where
  rnf doc = case doc of
    DocEmpty                  -> ()
    DocAppend a b             -> a `deepseq` b `deepseq` ()
    DocString a               -> a `deepseq` ()
    DocParagraph a            -> a `deepseq` ()
    DocIdentifier a           -> a `deepseq` ()
    DocIdentifierUnchecked a  -> a `deepseq` ()
    DocModule a               -> a `deepseq` ()
    DocWarning a              -> a `deepseq` ()
    DocEmphasis a             -> a `deepseq` ()
    DocBold a                 -> a `deepseq` ()
    DocMonospaced a           -> a `deepseq` ()
    DocUnorderedList a        -> a `deepseq` ()
    DocOrderedList a          -> a `deepseq` ()
    DocDefList a              -> a `deepseq` ()
    DocCodeBlock a            -> a `deepseq` ()
    DocHyperlink a            -> a `deepseq` ()
    DocPic a                  -> a `deepseq` ()
    DocMathInline a           -> a `deepseq` ()
    DocMathDisplay a          -> a `deepseq` ()
    DocAName a                -> a `deepseq` ()
    DocProperty a             -> a `deepseq` ()
    DocExamples a             -> a `deepseq` ()
    DocHeader a               -> a `deepseq` ()
    DocTable a                -> a `deepseq` ()

#if !MIN_VERSION_ghc(8,0,2)
-- These were added to GHC itself in 8.0.2
instance NFData Name where rnf x = seq x ()
instance NFData OccName where rnf x = seq x ()
instance NFData ModuleName where rnf x = seq x ()
#endif

instance NFData id => NFData (Header id) where
  rnf (Header a b) = a `deepseq` b `deepseq` ()

instance NFData id => NFData (Hyperlink id) where
  rnf (Hyperlink a b) = a `deepseq` b `deepseq` ()

instance NFData id => NFData (ModLink id) where
  rnf (ModLink a b) = a `deepseq` b `deepseq` ()

instance NFData Picture where
  rnf (Picture a b) = a `deepseq` b `deepseq` ()

instance NFData Example where
  rnf (Example a b) = a `deepseq` b `deepseq` ()

instance NFData id => NFData (Table id) where
    rnf (Table h b) = h `deepseq` b `deepseq` ()

instance NFData id => NFData (TableRow id) where
    rnf (TableRow cs) = cs `deepseq` ()

instance NFData id => NFData (TableCell id) where
    rnf (TableCell i j c) = i `deepseq` j `deepseq` c `deepseq` ()

exampleToString :: Example -> String
exampleToString (Example expression result) =
    ">>> " ++ expression ++ "\n" ++  unlines result

data HaddockModInfo name = HaddockModInfo
  { hmi_description :: Maybe (Doc name)
  , hmi_copyright   :: Maybe String
  , hmi_license     :: Maybe String
  , hmi_maintainer  :: Maybe String
  , hmi_stability   :: Maybe String
  , hmi_portability :: Maybe String
  , hmi_safety      :: Maybe String
  , hmi_language    :: Maybe Language
  , hmi_extensions  :: [LangExt.Extension]
  }


emptyHaddockModInfo :: HaddockModInfo a
emptyHaddockModInfo = HaddockModInfo
  { hmi_description = Nothing
  , hmi_copyright   = Nothing
  , hmi_license     = Nothing
  , hmi_maintainer  = Nothing
  , hmi_stability   = Nothing
  , hmi_portability = Nothing
  , hmi_safety      = Nothing
  , hmi_language    = Nothing
  , hmi_extensions  = []
  }


-----------------------------------------------------------------------------
-- * Options
-----------------------------------------------------------------------------


-- | Source-level options for controlling the documentation.
data DocOption
  = OptHide            -- ^ This module should not appear in the docs.
  | OptPrune
  | OptIgnoreExports   -- ^ Pretend everything is exported.
  | OptNotHome         -- ^ Not the best place to get docs for things
                       -- exported by this module.
  | OptShowExtensions  -- ^ Render enabled extensions for this module.
  deriving (Eq, Show)


-- | Option controlling how to qualify names
data QualOption
  = OptNoQual         -- ^ Never qualify any names.
  | OptFullQual       -- ^ Qualify all names fully.
  | OptLocalQual      -- ^ Qualify all imported names fully.
  | OptRelativeQual   -- ^ Like local, but strip module prefix
                      --   from modules in the same hierarchy.
  | OptAliasedQual    -- ^ Uses aliases of module names
                      --   as suggested by module import renamings.
                      --   However, we are unfortunately not able
                      --   to maintain the original qualifications.
                      --   Image a re-export of a whole module,
                      --   how could the re-exported identifiers be qualified?

type AliasMap = Map Module ModuleName

data Qualification
  = NoQual
  | FullQual
  | LocalQual Module
  | RelativeQual Module
  | AliasedQual AliasMap Module
       -- ^ @Module@ contains the current module.
       --   This way we can distinguish imported and local identifiers.

makeContentsQual :: QualOption -> Qualification
makeContentsQual qual =
  case qual of
    OptNoQual -> NoQual
    _         -> FullQual

makeModuleQual :: QualOption -> AliasMap -> Module -> Qualification
makeModuleQual qual aliases mdl =
  case qual of
    OptLocalQual      -> LocalQual mdl
    OptRelativeQual   -> RelativeQual mdl
    OptAliasedQual    -> AliasedQual aliases mdl
    OptFullQual       -> FullQual
    OptNoQual         -> NoQual

-- | Whether to hide empty contexts
-- Since pattern synonyms have two contexts with different semantics, it is
-- important to all of them, even if one of them is empty.
data HideEmptyContexts
  = HideEmptyContexts
  | ShowEmptyToplevelContexts

-- | When to qualify @since@ annotations with their package
data SinceQual
  = Always
  | External -- ^ only qualify when the thing being annotated is from
             -- an external package

-----------------------------------------------------------------------------
-- * Error handling
-----------------------------------------------------------------------------


-- A monad which collects error messages, locally defined to avoid a dep on mtl


type ErrMsg = String
type ErrMsgM = Writer [ErrMsg]


-- Exceptions


-- | Haddock's own exception type.
data HaddockException
  = HaddockException String
  | WithContext [String] SomeException
  deriving Typeable


instance Show HaddockException where
  show (HaddockException str) = str
  show (WithContext ctxts se)  = unlines $ ["While " ++ ctxt ++ ":\n" | ctxt <- reverse ctxts] ++ [show se]

throwE :: String -> a
instance Exception HaddockException
throwE str = throw (HaddockException str)

withExceptionContext :: MonadCatch m => String -> m a -> m a
withExceptionContext ctxt =
  handle (\ex ->
      case ex of
        HaddockException _ -> throwM $ WithContext [ctxt] (toException ex)
        WithContext ctxts se -> throwM $ WithContext (ctxt:ctxts) se
          ) .
  handle (throwM . WithContext [ctxt])

-- In "Haddock.Interface.Create", we need to gather
-- @Haddock.Types.ErrMsg@s a lot, like @ErrMsgM@ does,
-- but we can't just use @GhcT ErrMsgM@ because GhcT requires the
-- transformed monad to be MonadIO.
newtype ErrMsgGhc a = ErrMsgGhc { unErrMsgGhc :: WriterT [ErrMsg] Ghc a }


deriving newtype instance Functor ErrMsgGhc
deriving newtype instance Applicative ErrMsgGhc
deriving newtype instance Monad ErrMsgGhc
deriving newtype instance (MonadWriter [ErrMsg]) ErrMsgGhc
deriving newtype instance MonadIO ErrMsgGhc


runWriterGhc :: ErrMsgGhc a -> Ghc (a, [ErrMsg])
runWriterGhc = runWriterT . unErrMsgGhc

liftGhcToErrMsgGhc :: Ghc a -> ErrMsgGhc a
liftGhcToErrMsgGhc = ErrMsgGhc . lift

liftErrMsg :: ErrMsgM a -> ErrMsgGhc a
liftErrMsg = writer . runWriter

-----------------------------------------------------------------------------
-- * Pass sensitive types
-----------------------------------------------------------------------------

type instance XRec DocNameI a = GenLocated (Anno a) a
instance UnXRec DocNameI where
  unXRec = unLoc
instance MapXRec DocNameI where
  mapXRec = fmap
instance WrapXRec DocNameI (HsType DocNameI) where
  wrapXRec = noLocA

type instance Anno DocName                           = SrcSpanAnnN
type instance Anno (HsTyVarBndr flag DocNameI)       = SrcSpanAnnA
type instance Anno [LocatedA (HsType DocNameI)]      = SrcSpanAnnC
type instance Anno (HsType DocNameI)                 = SrcSpanAnnA
type instance Anno (DataFamInstDecl DocNameI)        = SrcSpanAnnA
type instance Anno (DerivStrategy DocNameI)          = SrcAnn NoEpAnns
type instance Anno (FieldOcc DocNameI)               = SrcAnn NoEpAnns
type instance Anno (ConDeclField DocNameI)           = SrcSpan
type instance Anno (Located (ConDeclField DocNameI)) = SrcSpan
type instance Anno [Located (ConDeclField DocNameI)] = SrcSpan
type instance Anno (ConDecl DocNameI)                = SrcSpan
type instance Anno (FunDep DocNameI)                 = SrcSpan
type instance Anno (TyFamInstDecl DocNameI)          = SrcSpanAnnA
type instance Anno [LocatedA (TyFamInstDecl DocNameI)] = SrcSpanAnnL
type instance Anno (FamilyDecl DocNameI)               = SrcSpan
type instance Anno (Sig DocNameI)                      = SrcSpan
type instance Anno (InjectivityAnn DocNameI)           = SrcAnn NoEpAnns
type instance Anno (HsDecl DocNameI)                   = SrcSpanAnnA
type instance Anno (FamilyResultSig DocNameI)          = SrcAnn NoEpAnns
type instance Anno (HsOuterTyVarBndrs Specificity DocNameI) = SrcSpanAnnA
type instance Anno (HsSigType DocNameI)                     = SrcSpanAnnA

type XRecCond a
  = ( XParTy a           ~ EpAnn AnnParen
    , NoGhcTc a ~ a
    , MapXRec a
    , UnXRec a
    , WrapXRec a (HsType a)
    )

type instance XForAllTy        DocNameI = EpAnn [AddEpAnn]
type instance XQualTy          DocNameI = EpAnn [AddEpAnn]
type instance XTyVar           DocNameI = EpAnn [AddEpAnn]
type instance XStarTy          DocNameI = EpAnn [AddEpAnn]
type instance XAppTy           DocNameI = EpAnn [AddEpAnn]
type instance XAppKindTy       DocNameI = EpAnn [AddEpAnn]
type instance XFunTy           DocNameI = EpAnn [AddEpAnn]
type instance XListTy          DocNameI = EpAnn AnnParen
type instance XTupleTy         DocNameI = EpAnn AnnParen
type instance XSumTy           DocNameI = EpAnn AnnParen
type instance XOpTy            DocNameI = EpAnn [AddEpAnn]
type instance XParTy           DocNameI = EpAnn AnnParen
type instance XIParamTy        DocNameI = EpAnn [AddEpAnn]
type instance XKindSig         DocNameI = EpAnn [AddEpAnn]
type instance XSpliceTy        DocNameI = Void       -- see `renameHsSpliceTy`
type instance XDocTy           DocNameI = EpAnn [AddEpAnn]
type instance XBangTy          DocNameI = EpAnn [AddEpAnn]
type instance XRecTy           DocNameI = EpAnn [AddEpAnn]
type instance XExplicitListTy  DocNameI = EpAnn [AddEpAnn]
type instance XExplicitTupleTy DocNameI = EpAnn [AddEpAnn]
type instance XTyLit           DocNameI = EpAnn [AddEpAnn]
type instance XWildCardTy      DocNameI = EpAnn [AddEpAnn]
type instance XXType           DocNameI = HsCoreTy

type instance XHsForAllVis        DocNameI = NoExtField
type instance XHsForAllInvis      DocNameI = NoExtField
type instance XXHsForAllTelescope DocNameI = DataConCantHappen

type instance XUserTyVar    DocNameI = NoExtField
type instance XKindedTyVar  DocNameI = NoExtField
type instance XXTyVarBndr   DocNameI = DataConCantHappen

type instance XCFieldOcc   DocNameI = DocName
type instance XXFieldOcc   DocNameI = NoExtField

type instance XFixitySig   DocNameI = NoExtField
type instance XFixSig      DocNameI = NoExtField
type instance XPatSynSig   DocNameI = NoExtField
type instance XClassOpSig  DocNameI = NoExtField
type instance XTypeSig     DocNameI = NoExtField
type instance XMinimalSig  DocNameI = NoExtField

type instance XForeignExport  DocNameI = NoExtField
type instance XForeignImport  DocNameI = NoExtField
type instance XConDeclGADT    DocNameI = NoExtField
type instance XConDeclH98     DocNameI = NoExtField
type instance XXConDecl       DocNameI = DataConCantHappen

type instance XDerivD     DocNameI = NoExtField
type instance XInstD      DocNameI = NoExtField
type instance XForD       DocNameI = NoExtField
type instance XSigD       DocNameI = NoExtField
type instance XTyClD      DocNameI = NoExtField

type instance XNoSig            DocNameI = NoExtField
type instance XCKindSig         DocNameI = NoExtField
type instance XTyVarSig         DocNameI = NoExtField
type instance XXFamilyResultSig DocNameI = DataConCantHappen

type instance XCFamEqn       DocNameI _ = NoExtField
type instance XXFamEqn       DocNameI _ = DataConCantHappen

type instance XCClsInstDecl DocNameI = NoExtField
type instance XCDerivDecl   DocNameI = NoExtField
type instance XStockStrategy    DocNameI = NoExtField
type instance XAnyClassStrategy DocNameI = NoExtField
type instance XNewtypeStrategy  DocNameI = NoExtField
type instance XViaStrategy  DocNameI = LHsSigType DocNameI
type instance XDataFamInstD DocNameI = NoExtField
type instance XTyFamInstD   DocNameI = NoExtField
type instance XClsInstD     DocNameI = NoExtField
type instance XCHsDataDefn  DocNameI = NoExtField
type instance XCFamilyDecl  DocNameI = NoExtField
type instance XClassDecl    DocNameI = NoExtField
type instance XDataDecl     DocNameI = NoExtField
type instance XSynDecl      DocNameI = NoExtField
type instance XFamDecl      DocNameI = NoExtField
type instance XXFamilyDecl  DocNameI = DataConCantHappen
type instance XXTyClDecl    DocNameI = DataConCantHappen

type instance XHsWC DocNameI _ = NoExtField

type instance XHsOuterExplicit    DocNameI _ = NoExtField
type instance XHsOuterImplicit    DocNameI   = NoExtField
type instance XXHsOuterTyVarBndrs DocNameI   = DataConCantHappen

type instance XHsSig      DocNameI = NoExtField
type instance XXHsSigType DocNameI = DataConCantHappen

type instance XHsQTvs        DocNameI = NoExtField
type instance XConDeclField  DocNameI = NoExtField
type instance XXConDeclField DocNameI = DataConCantHappen

type instance XXPat DocNameI = DataConCantHappen
type instance XXHsBindsLR DocNameI a = DataConCantHappen

type instance XCInjectivityAnn DocNameI = NoExtField

type instance XCFunDep DocNameI = NoExtField

type instance XCTyFamInstDecl DocNameI = NoExtField
