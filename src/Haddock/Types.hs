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

module Haddock.Types where


import Control.Exception
import Data.Typeable
import Data.Map (Map)
import qualified Data.Map as Map
import GHC hiding (NoLink)
import Name


-- convenient short-hands
type Decl = LHsDecl Name
type Doc  = HsDoc Name


-- | A declaration that may have documentation, including its subordinates,
-- which may also have documentation
type DeclInfo = (Decl, Maybe Doc, [(Name, Maybe Doc)])


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
			       
      -- | Maybe a doc comment
      expItemMbDoc :: Maybe (HsDoc name),

      -- | Subordinate names, possibly with documentation
      expItemSubDocs :: [(name, Maybe (HsDoc name))],

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


-- | This structure holds the module information we get from GHC's 
-- type checking phase
data GhcModule = GhcModule {
   ghcModule         :: Module,
   ghcFilename       :: FilePath,
   ghcMbDocOpts      :: Maybe String,
   ghcHaddockModInfo :: HaddockModInfo Name,
   ghcMbDoc          :: Maybe (HsDoc Name),
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

  -- | The documented module
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

  ifaceDeclMap         :: Map Name DeclInfo,
  ifaceRnDocMap        :: Map Name (HsDoc DocName),
  ifaceSubMap          :: Map Name [Name],

  ifaceExportItems     :: ![ExportItem Name],
  ifaceRnExportItems   :: [ExportItem DocName],

  -- | All the names that are defined in this module
  ifaceLocals          :: ![Name],

  -- | All the names that are exported by this module
  ifaceExports         :: ![Name],

  -- | All the visible names exported by this module
  -- For a name to be visible, it has to:
  -- - be exported normally, and not via a full module re-exportation.
  -- - have a declaration in this module or any of it's imports, with the    
  --   exception that it can't be from another package.
  -- Basically, a visible name is a name that will show up in the documentation
  -- for this module.
  ifaceVisibleExports  :: ![Name],

  -- | The instances exported by this module
  ifaceInstances       :: ![Instance]
}


-- | A smaller version of 'Interface' that we can get from the Haddock
-- interface files.
data InstalledInterface = InstalledInterface {
  instMod            :: Module,
  instInfo           :: HaddockModInfo Name,
  instDocMap         :: Map Name (HsDoc DocName),
  instExports        :: [Name],
  instVisibleExports :: [Name],
  instOptions        :: [DocOption],
  instSubMap         :: Map Name [Name]
}


-- | Convert an 'Interface' to an 'InstalledInterface'
toInstalledIface :: Interface -> InstalledInterface
toInstalledIface interface = InstalledInterface {
  instMod            = ifaceMod            interface,
  instInfo           = ifaceInfo           interface,
  instDocMap         = ifaceRnDocMap       interface,
  instExports        = ifaceExports        interface,
  instVisibleExports = ifaceVisibleExports interface,
  instOptions        = ifaceOptions        interface,
  instSubMap         = ifaceSubMap         interface
}


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


-- A monad which collects error messages, locally defined to avoid a dep on mtl

type ErrMsg = String

newtype ErrMsgM a = Writer { runWriter :: (a, [ErrMsg]) }

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
