{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable, StandaloneDeriving, TypeFamilies, RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
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
 ) where

import Control.Exception
import Control.Arrow hiding ((<+>))
import Control.DeepSeq
import Control.Monad.IO.Class (MonadIO(..))
import Data.Typeable
import Data.Map (Map)
import Data.Data (Data)
import qualified Data.Map as Map
import Documentation.Haddock.Types
import BasicTypes (Fixity(..))

import GHC hiding (NoLink)
import DynFlags (Language)
import qualified GHC.LanguageExtensions as LangExt
import OccName
import Outputable
import Control.Applicative (Applicative(..))
import Control.Monad (ap)

import Haddock.Backends.Hyperlinker.Types

-----------------------------------------------------------------------------
-- * Convenient synonyms
-----------------------------------------------------------------------------


type IfaceMap      = Map Module Interface
type InstIfaceMap  = Map Module InstalledInterface  -- TODO: rename
type DocMap a      = Map Name (MDoc a)
type ArgMap a      = Map Name (Map Int (MDoc a))
type SubMap        = Map Name [Name]
type DeclMap       = Map Name [LHsDecl GhcRn]
type InstMap       = Map SrcSpan Name
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
  , ifaceTokenizedSrc :: !(Maybe [RichToken])
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
noDocForDecl = (Documentation Nothing Nothing, Map.empty)


-----------------------------------------------------------------------------
-- * Cross-referencing
-----------------------------------------------------------------------------


-- | Type of environment used to cross-reference identifiers in the syntax.
type LinkEnv = Map Name Module

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

type instance IdP DocNameI = DocName


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

instance (a ~ GhcPass p,OutputableBndrId a)
         => Outputable (InstType a) where
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
    , pfdLName :: Located (IdP name)
    , pfdTyVars :: [LHsType name]
    , pfdKindSig :: LFamilyResultSig name
    }


mkPseudoFamilyDecl :: FamilyDecl (GhcPass p) -> PseudoFamilyDecl (GhcPass p)
mkPseudoFamilyDecl (FamilyDecl { .. }) = PseudoFamilyDecl
    { pfdInfo = fdInfo
    , pfdLName = fdLName
    , pfdTyVars = [ L loc (mkType bndr) | L loc bndr <- hsq_explicit fdTyVars ]
    , pfdKindSig = fdResultSig
    }
  where
    mkType (KindedTyVar _ (L loc name) lkind) =
        HsKindSig NoExt tvar lkind
      where
        tvar = L loc (HsTyVar NoExt NotPromoted (L loc name))
    mkType (UserTyVar _ name) = HsTyVar NoExt NotPromoted name
    mkType (XTyVarBndr _ ) = panic "haddock:mkPseudoFamilyDecl"
mkPseudoFamilyDecl (XFamilyDecl {}) = panic "haddock:mkPseudoFamilyDecl"


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

type Doc id = DocH (ModuleName, OccName) id
type MDoc id = MetaDoc (ModuleName, OccName) id

type DocMarkup id a = DocMarkupH (ModuleName, OccName) id a

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

instance NFData Hyperlink where
  rnf (Hyperlink a b) = a `deepseq` b `deepseq` ()

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
newtype ErrMsgM a = Writer { runWriter :: (a, [ErrMsg]) }


instance Functor ErrMsgM where
        fmap f (Writer (a, msgs)) = Writer (f a, msgs)

instance Applicative ErrMsgM where
    pure a = Writer (a, [])
    (<*>)  = ap

instance Monad ErrMsgM where
        return   = pure
        m >>= k  = Writer $ let
                (a, w)  = runWriter m
                (b, w') = runWriter (k a)
                in (b, w ++ w')


tell :: [ErrMsg] -> ErrMsgM ()
tell w = Writer ((), w)


-- Exceptions


-- | Haddock's own exception type.
data HaddockException = HaddockException String deriving Typeable


instance Show HaddockException where
  show (HaddockException str) = str


throwE :: String -> a
instance Exception HaddockException
throwE str = throw (HaddockException str)


-- In "Haddock.Interface.Create", we need to gather
-- @Haddock.Types.ErrMsg@s a lot, like @ErrMsgM@ does,
-- but we can't just use @GhcT ErrMsgM@ because GhcT requires the
-- transformed monad to be MonadIO.
newtype ErrMsgGhc a = WriterGhc { runWriterGhc :: Ghc (a, [ErrMsg]) }
--instance MonadIO ErrMsgGhc where
--  liftIO = WriterGhc . fmap (\a->(a,[])) liftIO
--er, implementing GhcMonad involves annoying ExceptionMonad and
--WarnLogMonad classes, so don't bother.
liftGhcToErrMsgGhc :: Ghc a -> ErrMsgGhc a
liftGhcToErrMsgGhc = WriterGhc . fmap (\a->(a,[]))
liftErrMsg :: ErrMsgM a -> ErrMsgGhc a
liftErrMsg = WriterGhc . return . runWriter
--  for now, use (liftErrMsg . tell) for this
--tell :: [ErrMsg] -> ErrMsgGhc ()
--tell msgs = WriterGhc $ return ( (), msgs )


instance Functor ErrMsgGhc where
  fmap f (WriterGhc x) = WriterGhc (fmap (first f) x)

instance Applicative ErrMsgGhc where
    pure a = WriterGhc (return (a, []))
    (<*>) = ap

instance Monad ErrMsgGhc where
  return = pure
  m >>= k = WriterGhc $ runWriterGhc m >>= \ (a, msgs1) ->
               fmap (second (msgs1 ++)) (runWriterGhc (k a))

instance MonadIO ErrMsgGhc where
  liftIO m = WriterGhc (fmap (\x -> (x, [])) (liftIO m))

-----------------------------------------------------------------------------
-- * Pass sensitive types
-----------------------------------------------------------------------------

type instance XForAllTy        DocNameI = NoExt
type instance XQualTy          DocNameI = NoExt
type instance XTyVar           DocNameI = NoExt
type instance XStarTy          DocNameI = NoExt
type instance XAppTy           DocNameI = NoExt
type instance XFunTy           DocNameI = NoExt
type instance XListTy          DocNameI = NoExt
type instance XTupleTy         DocNameI = NoExt
type instance XSumTy           DocNameI = NoExt
type instance XOpTy            DocNameI = NoExt
type instance XParTy           DocNameI = NoExt
type instance XIParamTy        DocNameI = NoExt
type instance XKindSig         DocNameI = NoExt
type instance XSpliceTy        DocNameI = NoExt
type instance XDocTy           DocNameI = NoExt
type instance XBangTy          DocNameI = NoExt
type instance XRecTy           DocNameI = NoExt
type instance XExplicitListTy  DocNameI = NoExt
type instance XExplicitTupleTy DocNameI = NoExt
type instance XTyLit           DocNameI = NoExt
type instance XWildCardTy      DocNameI = HsWildCardInfo
type instance XXType           DocNameI = NewHsTypeX

type instance XUserTyVar    DocNameI = NoExt
type instance XKindedTyVar  DocNameI = NoExt
type instance XXTyVarBndr   DocNameI = NoExt

type instance XCFieldOcc   DocNameI = DocName
type instance XXFieldOcc   DocNameI = NoExt

type instance XFixitySig   DocNameI = NoExt
type instance XFixSig      DocNameI = NoExt
type instance XPatSynSig   DocNameI = NoExt
type instance XClassOpSig  DocNameI = NoExt
type instance XTypeSig     DocNameI = NoExt
type instance XMinimalSig  DocNameI = NoExt

type instance XForeignExport  DocNameI = NoExt
type instance XForeignImport  DocNameI = NoExt
type instance XConDeclGADT    DocNameI = NoExt
type instance XConDeclH98     DocNameI = NoExt

type instance XDerivD     DocNameI = NoExt
type instance XInstD      DocNameI = NoExt
type instance XForD       DocNameI = NoExt
type instance XSigD       DocNameI = NoExt
type instance XTyClD      DocNameI = NoExt

type instance XNoSig      DocNameI = NoExt
type instance XCKindSig   DocNameI = NoExt
type instance XTyVarSig   DocNameI = NoExt

type instance XCFamEqn       DocNameI _ _ = NoExt

type instance XCClsInstDecl DocNameI = NoExt
type instance XCDerivDecl   DocNameI = NoExt
type instance XViaStrategy  DocNameI = LHsSigType DocNameI
type instance XDataFamInstD DocNameI = NoExt
type instance XTyFamInstD   DocNameI = NoExt
type instance XClsInstD     DocNameI = NoExt
type instance XCHsDataDefn  DocNameI = NoExt
type instance XCFamilyDecl  DocNameI = NoExt
type instance XClassDecl    DocNameI = NoExt
type instance XDataDecl     DocNameI = NoExt
type instance XSynDecl      DocNameI = NoExt
type instance XFamDecl      DocNameI = NoExt

type instance XHsIB      DocNameI _ = NoExt
type instance XHsWC      DocNameI _ = NoExt

type instance XHsQTvs        DocNameI = NoExt
type instance XConDeclField  DocNameI = NoExt

