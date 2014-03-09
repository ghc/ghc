{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable, StandaloneDeriving #-}
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
 ) where

import Data.Foldable
import Data.Traversable
import Control.Exception
import Control.Arrow hiding ((<+>))
import Control.DeepSeq
import Data.Typeable
import Data.Map (Map)
import qualified Data.Map as Map
import BasicTypes (Fixity(..))
import GHC hiding (NoLink)
import DynFlags (ExtensionFlag, Language)
import OccName
import Outputable
import Control.Applicative (Applicative(..))
import Control.Monad (ap)

-----------------------------------------------------------------------------
-- * Convenient synonyms
-----------------------------------------------------------------------------


type IfaceMap      = Map Module Interface
type InstIfaceMap  = Map Module InstalledInterface  -- TODO: rename
type DocMap a      = Map Name (Doc a)
type ArgMap a      = Map Name (Map Int (Doc a))
type SubMap        = Map Name [Name]
type DeclMap       = Map Name [LHsDecl Name]
type InstMap       = Map SrcSpan Name
type FixMap        = Map Name Fixity
type SrcMap        = Map PackageId FilePath
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
  , ifaceDeclMap         :: !(Map Name [LHsDecl Name])

    -- | Documentation of declarations originating from the module (including
    -- subordinates).
  , ifaceDocMap          :: !(DocMap Name)
  , ifaceArgMap          :: !(ArgMap Name)

    -- | Documentation of declarations originating from the module (including
    -- subordinates).
  , ifaceRnDocMap        :: !(DocMap DocName)
  , ifaceRnArgMap        :: !(ArgMap DocName)

  , ifaceSubMap          :: !(Map Name [Name])

  , ifaceExportItems     :: ![ExportItem Name]
  , ifaceRnExportItems   :: ![ExportItem DocName]

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

    -- | The number of haddockable and haddocked items in the module, as a
    -- tuple. Haddockable items are the exports and the module itself.
  , ifaceHaddockCoverage :: !(Int, Int)

    -- | Warnings for things defined in this module.
  , ifaceWarningMap :: !WarningMap
  }

type WarningMap = DocMap Name


-- | A subset of the fields of 'Interface' that we store in the interface
-- files.
data InstalledInterface = InstalledInterface
  {
    -- | The module represented by this interface.
    instMod            :: Module

    -- | Textual information about the module.
  , instInfo           :: HaddockModInfo Name

    -- | Documentation of declarations originating from the module (including
    -- subordinates).
  , instDocMap         :: DocMap Name

  , instArgMap         :: ArgMap Name

    -- | All names exported by this module.
  , instExports        :: [Name]

    -- | All \"visible\" names exported by the module.
    -- A visible name is a name that will show up in the documentation of the
    -- module.
  , instVisibleExports :: [Name]

    -- | Haddock options for this module (prune, ignore-exports, etc).
  , instOptions        :: [DocOption]

  , instSubMap         :: Map Name [Name]
  }


-- | Convert an 'Interface' to an 'InstalledInterface'
toInstalledIface :: Interface -> InstalledInterface
toInstalledIface interface = InstalledInterface
  { instMod            = ifaceMod            interface
  , instInfo           = ifaceInfo           interface
  , instDocMap         = ifaceDocMap         interface
  , instArgMap         = ifaceArgMap         interface
  , instExports        = ifaceExports        interface
  , instVisibleExports = ifaceVisibleExports interface
  , instOptions        = ifaceOptions        interface
  , instSubMap         = ifaceSubMap         interface
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

        -- | Maybe a doc comment, and possibly docs for arguments (if this
        -- decl is a function or type-synonym).
      , expItemMbDoc :: !(DocForDecl name)

        -- | Subordinate names, possibly with documentation.
      , expItemSubDocs :: ![(name, DocForDecl name)]

        -- | Instances relevant to this declaration, possibly with
        -- documentation.
      , expItemInstances :: ![DocInstance name]

        -- | Fixity decls relevant to this declaration (including subordinates).
      , expItemFixities :: ![(name, Fixity)]

        -- | Whether the ExportItem is from a TH splice or not, for generating
        -- the appropriate type of Source link.
      , expItemSpliced :: !Bool
      }

  -- | An exported entity for which we have no documentation (perhaps because it
  -- resides in another package).
  | ExportNoDecl
      { expItemName :: !name

        -- | Subordinate names.
      , expItemSubs :: ![name]
      }

  -- | A section heading.
  | ExportGroup
      {
        -- | Section level (1, 2, 3, ...).
        expItemSectionLevel :: !Int

        -- | Section id (for hyperlinks).
      , expItemSectionId :: !String

        -- | Section heading text.
      , expItemSectionText :: !(Doc name)
      }

  -- | Some documentation.
  | ExportDoc !(Doc name)

  -- | A cross-reference to another module.
  | ExportModule !Module

data Documentation name = Documentation
  { documentationDoc :: Maybe (Doc name)
  , documentationWarning :: !(Maybe (Doc name))
  } deriving Functor


-- | Arguments and result are indexed by Int, zero-based from the left,
-- because that's the easiest to use when recursing over types.
type FnArgsDoc name = Map Int (Doc name)
type DocForDecl name = (Documentation name, FnArgsDoc name)


noDocForDecl :: DocForDecl name
noDocForDecl = (Documentation Nothing Nothing, Map.empty)


unrenameDocForDecl :: DocForDecl DocName -> DocForDecl Name
unrenameDocForDecl (doc, fnArgsDoc) =
    (fmap getName doc, (fmap . fmap) getName fnArgsDoc)


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
  deriving Eq


instance NamedThing DocName where
  getName (Documented name _) = name
  getName (Undocumented name) = name


-----------------------------------------------------------------------------
-- * Instances
-----------------------------------------------------------------------------

-- | The three types of instances
data InstType name
  = ClassInst [HsType name]  -- ^ Context
  | TypeInst  (HsType name)  -- ^ Body (right-hand side)
  | DataInst (TyClDecl name) -- ^ Data constructors

instance OutputableBndr a => Outputable (InstType a) where
  ppr (ClassInst a) = text "ClassInst" <+> ppr a
  ppr (TypeInst  a) = text "TypeInst"  <+> ppr a
  ppr (DataInst  a) = text "DataInst"  <+> ppr a

-- | An instance head that may have documentation.
type DocInstance name = (InstHead name, Maybe (Doc name))

-- | The head of an instance. Consists of a class name, a list of kind
-- parameters, a list of type parameters and an instance type
type InstHead name = (name, [HsType name], [HsType name], InstType name)

-----------------------------------------------------------------------------
-- * Documentation comments
-----------------------------------------------------------------------------


type LDoc id = Located (Doc id)


data Doc id
  = DocEmpty
  | DocAppend (Doc id) (Doc id)
  | DocString String
  | DocParagraph (Doc id)
  | DocIdentifier id
  | DocIdentifierUnchecked (ModuleName, OccName)
  | DocModule String
  | DocWarning (Doc id)
  | DocEmphasis (Doc id)
  | DocMonospaced (Doc id)
  | DocBold (Doc id)
  | DocUnorderedList [Doc id]
  | DocOrderedList [Doc id]
  | DocDefList [(Doc id, Doc id)]
  | DocCodeBlock (Doc id)
  | DocHyperlink Hyperlink
  | DocPic Picture
  | DocAName String
  | DocProperty String
  | DocExamples [Example]
  | DocHeader (Header (Doc id))
  deriving (Functor, Foldable, Traversable)

instance Foldable Header where
  foldMap f (Header _ a) = f a

instance Traversable Header where
  traverse f (Header l a) = Header l `fmap` f a

instance NFData a => NFData (Doc a) where
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
    DocAName a                -> a `deepseq` ()
    DocProperty a             -> a `deepseq` ()
    DocExamples a             -> a `deepseq` ()
    DocHeader a               -> a `deepseq` ()


instance NFData Name
instance NFData OccName
instance NFData ModuleName


data Hyperlink = Hyperlink
  { hyperlinkUrl   :: String
  , hyperlinkLabel :: Maybe String
  } deriving (Eq, Show)


data Picture = Picture
  { pictureUri   :: String
  , pictureTitle :: Maybe String
  } deriving (Eq, Show)

data Header id = Header
  { headerLevel :: Int
  , headerTitle :: id
  } deriving Functor

instance NFData id => NFData (Header id) where
  rnf (Header a b) = a `deepseq` b `deepseq` ()

instance NFData Hyperlink where
  rnf (Hyperlink a b) = a `deepseq` b `deepseq` ()

instance NFData Picture where
  rnf (Picture a b) = a `deepseq` b `deepseq` ()


data Example = Example
  { exampleExpression :: String
  , exampleResult     :: [String]
  } deriving (Eq, Show)


instance NFData Example where
  rnf (Example a b) = a `deepseq` b `deepseq` ()


exampleToString :: Example -> String
exampleToString (Example expression result) =
    ">>> " ++ expression ++ "\n" ++  unlines result


data DocMarkup id a = Markup
  { markupEmpty                :: a
  , markupString               :: String -> a
  , markupParagraph            :: a -> a
  , markupAppend               :: a -> a -> a
  , markupIdentifier           :: id -> a
  , markupIdentifierUnchecked  :: (ModuleName, OccName) -> a
  , markupModule               :: String -> a
  , markupWarning              :: a -> a
  , markupEmphasis             :: a -> a
  , markupBold                 :: a -> a
  , markupMonospaced           :: a -> a
  , markupUnorderedList        :: [a] -> a
  , markupOrderedList          :: [a] -> a
  , markupDefList              :: [(a,a)] -> a
  , markupCodeBlock            :: a -> a
  , markupHyperlink            :: Hyperlink -> a
  , markupAName                :: String -> a
  , markupPic                  :: Picture -> a
  , markupProperty             :: String -> a
  , markupExample              :: [Example] -> a
  , markupHeader               :: Header a -> a
  }


data HaddockModInfo name = HaddockModInfo
  { hmi_description :: Maybe (Doc name)
  , hmi_copyright   :: Maybe String
  , hmi_license     :: Maybe String
  , hmi_maintainer  :: Maybe String
  , hmi_stability   :: Maybe String
  , hmi_portability :: Maybe String
  , hmi_safety      :: Maybe String
  , hmi_language    :: Maybe Language
  , hmi_extensions  :: [ExtensionFlag]
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


{-! for DocOption derive: Binary !-}
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


-----------------------------------------------------------------------------
-- * Error handling
-----------------------------------------------------------------------------


-- A monad which collects error messages, locally defined to avoid a dep on mtl


type ErrMsg = String
newtype ErrMsgM a = Writer { runWriter :: (a, [ErrMsg]) }


instance Functor ErrMsgM where
        fmap f (Writer (a, msgs)) = Writer (f a, msgs)

instance Applicative ErrMsgM where
    pure = return
    (<*>) = ap

instance Monad ErrMsgM where
        return a = Writer (a, [])
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
    pure = return
    (<*>) = ap

instance Monad ErrMsgGhc where
  return a = WriterGhc (return (a, []))
  m >>= k = WriterGhc $ runWriterGhc m >>= \ (a, msgs1) ->
               fmap (second (msgs1 ++)) (runWriterGhc (k a))
