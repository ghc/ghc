{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Types
-- Copyright   :  (c) Simon Marlow 2003-2006,
--                    David Waern  2006-2009
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
-- avoid duplicate-export warnings, use the conditional to only
-- mention things not defined in this module:
#if __GLASGOW_HASKELL__ >= 611
  , HsDocString, LHsDocString
#else
  , HsDoc(..), LHsDoc, HaddockModInfo(..), emptyHaddockModInfo
#endif
 ) where


import Control.Exception
import Data.Typeable
import Data.Map (Map)
import qualified Data.Map as Map
import GHC hiding (NoLink)
import Name


-- convenient short-hands
type Decl = LHsDecl Name
type Doc  = HsDoc Name

#if __GLASGOW_HASKELL__ <= 610
type HsDocString = HsDoc Name
type LHsDocString = Located HsDocString
#endif

type FnArgsDoc name = Map Int (HsDoc name)
type DocForDecl name = (Maybe (HsDoc name), FnArgsDoc name)

noDocForDecl :: DocForDecl name
noDocForDecl = (Nothing, Map.empty)

-- | A declaration that may have documentation, including its subordinates,
-- which may also have documentation
type DeclInfo = (Decl, DocForDecl Name, [(Name, DocForDecl Name)])


-- | A 'DocName' is an identifier that may be documented. The 'Module'
-- component specifies the place which we want to link to in the documentation.
data DocName = Documented Name Module | Undocumented Name
  deriving Eq


-- | The 'OccName' belonging to this name
docNameOcc :: DocName -> OccName
docNameOcc = nameOccName . getName


instance NamedThing DocName where
  getName (Documented name _) = name
  getName (Undocumented name) = name


{-! for DocOption derive: Binary !-}
data DocOption
  = OptHide           -- ^ This module should not appear in the docs
  | OptPrune
  | OptIgnoreExports  -- ^ Pretend everything is exported
  | OptNotHome        -- ^ Not the best place to get docs for things
                      -- exported by this module.
  deriving (Eq, Show)


data ExportItem name

  = ExportDecl {		  		
	
      -- | A declaration
      expItemDecl :: LHsDecl name, 
			       
      -- | Maybe a doc comment, and possibly docs for arguments (if this
      -- decl is a function or type-synonym)
      expItemMbDoc :: DocForDecl name,

      -- | Subordinate names, possibly with documentation
      expItemSubDocs :: [(name, DocForDecl name)],

      -- | Instances relevant to this declaration
      expItemInstances :: [InstHead name]
	
	  }	-- ^ An exported declaration 
		    
  | ExportNoDecl {
      expItemName :: name,

      -- | Subordinate names
      expItemSubs :: [name]

		} -- ^ An exported entity for which we have no 
          -- documentation (perhaps because it resides in
          -- another package)

  | ExportGroup { 

      -- | Section level (1, 2, 3, ... )
      expItemSectionLevel :: Int,

      -- | Section id (for hyperlinks)
      expItemSectionId :: String,     
			
      -- | Section heading text
      expItemSectionText :: HsDoc name

    } -- ^ A section heading

  | ExportDoc (HsDoc name) -- ^ Some documentation

  | ExportModule Module    -- ^ A cross-reference to another module


type InstHead name = ([HsPred name], name, [HsType name])
type ModuleMap     = Map Module Interface
type InstIfaceMap  = Map Module InstalledInterface
type DocMap        = Map Name (HsDoc DocName)
type LinkEnv       = Map Name Module

#if __GLASGOW_HASKELL__ >= 611
type GhcDocHdr = Maybe LHsDocString
#else
type GhcDocHdr = (HaddockModInfo Name, Maybe (HsDoc Name))
#endif

-- | This structure holds the module information we get from GHC's 
-- type checking phase
data GhcModule = GhcModule {
   ghcModule         :: Module,
   ghcFilename       :: FilePath,
   ghcMbDocOpts      :: Maybe String,
   ghcMbDocHdr       :: GhcDocHdr,
   ghcGroup          :: HsGroup Name,
   ghcMbExports      :: Maybe [LIE Name],
   ghcExportedNames  :: [Name],
   ghcDefinedNames   :: [Name],
   ghcNamesInScope   :: [Name],
   ghcInstances      :: [Instance]
}


-- | This is the data structure used to render a Haddock page for a module - it
-- is the "interface" of the module. The core of Haddock lies in creating this 
-- structure (see Haddock.Interface). The structure also holds intermediate
-- data needed during its creation.
data Interface = Interface {

  -- | The module represented by this interface
  ifaceMod             :: Module,

  -- | The original filename for this module
  ifaceOrigFilename    :: FilePath,

  -- | Textual information about the module 
  ifaceInfo            :: !(HaddockModInfo Name),

  -- | The documentation header for this module
  ifaceDoc             :: !(Maybe (HsDoc Name)),

  -- | The renamed documentation header for this module
  ifaceRnDoc           :: Maybe (HsDoc DocName),

  -- | The Haddock options for this module (prune, ignore-exports, etc)
  ifaceOptions         :: ![DocOption],

  -- | The declarations of the module.  Excludes declarations that don't
  -- have names (instances and stand-alone documentation comments). Includes
  -- subordinate names, but they are mapped to their parent declarations.
  ifaceDeclMap         :: Map Name DeclInfo,

  -- | Everything declared in the module (including subordinates) that has docs
  ifaceRnDocMap        :: Map Name (DocForDecl DocName),

  ifaceSubMap          :: Map Name [Name],

  ifaceExportItems     :: ![ExportItem Name],
  ifaceRnExportItems   :: [ExportItem DocName],

  -- | All names defined in this module
  ifaceLocals          :: ![Name],

  -- | All names exported by this module
  ifaceExports         :: ![Name],

  -- | All the visible names exported by this module
  -- For a name to be visible, it has to:
  --
  --  * be exported normally, and not via a full module re-exportation.
  --
  --  * have a declaration in this module or any of it's imports, with the
  --    exception that it can't be from another package.
  --
  -- Basically, a visible name is a name that will show up in the documentation
  -- for this module.
  ifaceVisibleExports  :: ![Name],

  -- | The instances exported by this module
  ifaceInstances       :: ![Instance]
}


-- | A smaller version of 'Interface' that we can get from the Haddock
-- interface files.
data InstalledInterface = InstalledInterface {

  -- | The module represented by this interface
  instMod            :: Module,

  -- | Textual information about the module 
  instInfo           :: HaddockModInfo Name,

  -- | Everything declared in the module (including subordinates) that has docs
  instDocMap         :: Map Name (DocForDecl Name),

  -- | All names exported by this module
  instExports        :: [Name],

  -- | All the visible names exported by this module
  -- For a name to be visible, it has to:
  --
  --  * be exported normally, and not via a full module re-exportation.
  --
  --  * have a declaration in this module or any of it's imports, with the
  --    exception that it can't be from another package.
  --
  -- Basically, a visible name is a name that will show up in the documentation
  -- for this module.
  instVisibleExports :: [Name],

  -- | The Haddock options for this module (prune, ignore-exports, etc)
  instOptions        :: [DocOption],

  instSubMap         :: Map Name [Name]
}


-- | Convert an 'Interface' to an 'InstalledInterface'
toInstalledIface :: Interface -> InstalledInterface
toInstalledIface interface = InstalledInterface {
  instMod            = ifaceMod            interface,
  instInfo           = ifaceInfo           interface,
  instDocMap         = fmap unrenameDocForDecl $ ifaceRnDocMap interface,
  instExports        = ifaceExports        interface,
  instVisibleExports = ifaceVisibleExports interface,
  instOptions        = ifaceOptions        interface,
  instSubMap         = ifaceSubMap         interface
}

unrenameHsDoc :: HsDoc DocName -> HsDoc Name
unrenameHsDoc = fmapHsDoc getName
unrenameDocForDecl :: DocForDecl DocName -> DocForDecl Name
unrenameDocForDecl (mbDoc, fnArgsDoc) =
    (fmap unrenameHsDoc mbDoc, fmap unrenameHsDoc fnArgsDoc)

#if __GLASGOW_HASKELL__ >= 611
data HsDoc id
  = DocEmpty
  | DocAppend (HsDoc id) (HsDoc id)
  | DocString String
  | DocParagraph (HsDoc id)
  | DocIdentifier [id]
  | DocModule String
  | DocEmphasis (HsDoc id)
  | DocMonospaced (HsDoc id)
  | DocUnorderedList [HsDoc id]
  | DocOrderedList [HsDoc id]
  | DocDefList [(HsDoc id, HsDoc id)]
  | DocCodeBlock (HsDoc id)
  | DocURL String
  | DocPic String
  | DocAName String
  deriving (Eq, Show)

type LHsDoc id = Located (HsDoc id)
#endif

data DocMarkup id a = Markup {
  markupEmpty         :: a,
  markupString        :: String -> a,
  markupParagraph     :: a -> a,
  markupAppend        :: a -> a -> a,
  markupIdentifier    :: [id] -> a,
  markupModule        :: String -> a,
  markupEmphasis      :: a -> a,
  markupMonospaced    :: a -> a,
  markupUnorderedList :: [a] -> a,
  markupOrderedList   :: [a] -> a,
  markupDefList       :: [(a,a)] -> a,
  markupCodeBlock     :: a -> a,
  markupURL           :: String -> a,
  markupAName         :: String -> a,
  markupPic           :: String -> a
}

#if __GLASGOW_HASKELL__ >= 611
data HaddockModInfo name = HaddockModInfo {
        hmi_description :: Maybe (HsDoc name),
        hmi_portability :: Maybe String,
        hmi_stability   :: Maybe String,
        hmi_maintainer  :: Maybe String
}

emptyHaddockModInfo :: HaddockModInfo a
emptyHaddockModInfo = HaddockModInfo {
        hmi_description = Nothing,
        hmi_portability = Nothing,
        hmi_stability   = Nothing,
        hmi_maintainer  = Nothing
}
#endif


-- A monad which collects error messages, locally defined to avoid a dep on mtl

type ErrMsg = String

newtype ErrMsgM a = Writer { runWriter :: (a, [ErrMsg]) }

instance Functor ErrMsgM where
        fmap f (Writer (a, msgs)) = Writer (f a, msgs)

instance Monad ErrMsgM where
        return a = Writer (a, [])
        m >>= k  = Writer $ let
                (a, w)  = runWriter m
                (b, w') = runWriter (k a)
                in (b, w ++ w')

tell :: [ErrMsg] -> ErrMsgM ()
tell w = Writer ((), w)


-- Exceptions

-- | Haddock's own exception type
data HaddockException = HaddockException String deriving Typeable


instance Show HaddockException where
  show (HaddockException str) = str


throwE :: String -> a
#if __GLASGOW_HASKELL__ >= 609
instance Exception HaddockException
throwE str = throw (HaddockException str)
#else
throwE str = throwDyn (HaddockException str)
#endif

-- In "Haddock.Interface.Create", we need to gather
-- @Haddock.Types.ErrMsg@s a lot, like @ErrMsgM@ does,
-- but we can't just use @GhcT ErrMsgM@ because GhcT requires the
-- transformed monad to be MonadIO.
newtype ErrMsgGhc a = WriterGhc { runWriterGhc :: (Ghc (a, [ErrMsg])) }
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
  fmap f (WriterGhc x) = WriterGhc (fmap (\(a,msgs)->(f a,msgs)) x)
instance Monad ErrMsgGhc where
  return a = WriterGhc (return (a, []))
  m >>= k = WriterGhc $ runWriterGhc m >>= \ (a, msgs1) ->
               fmap (\ (b, msgs2) -> (b, msgs1 ++ msgs2)) (runWriterGhc (k a))

-- When HsDoc syntax is part of the Haddock codebase, we'll just
-- declare a Functor instance.
fmapHsDoc :: (a->b) -> HsDoc a -> HsDoc b
fmapHsDoc _ DocEmpty = DocEmpty
fmapHsDoc f (DocAppend a b) = DocAppend (fmapHsDoc f a) (fmapHsDoc f b)
fmapHsDoc _ (DocString s) = DocString s
fmapHsDoc _ (DocModule s) = DocModule s
fmapHsDoc _ (DocURL s) = DocURL s
fmapHsDoc _ (DocPic s) = DocPic s
fmapHsDoc _ (DocAName s) = DocAName s
fmapHsDoc f (DocParagraph a) = DocParagraph (fmapHsDoc f a)
fmapHsDoc f (DocEmphasis a) = DocEmphasis (fmapHsDoc f a)
fmapHsDoc f (DocMonospaced a) = DocMonospaced (fmapHsDoc f a)
fmapHsDoc f (DocCodeBlock a) = DocMonospaced (fmapHsDoc f a)
fmapHsDoc f (DocIdentifier a) = DocIdentifier (map f a)
fmapHsDoc f (DocOrderedList a) = DocOrderedList (map (fmapHsDoc f) a)
fmapHsDoc f (DocUnorderedList a) = DocUnorderedList (map (fmapHsDoc f) a)
fmapHsDoc f (DocDefList a) = DocDefList (map (\(b,c)->(fmapHsDoc f b, fmapHsDoc f c)) a)

