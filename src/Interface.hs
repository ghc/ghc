module Interface (
  Interface,
  dumpInterface,
  readInterface  
) where

import HaddockUtil   ( noDieMsg, moduleString )
import Binary2       ( BinHandle, Binary(..), FormatVersion, mkFormatVersion, 
                       openBinMem, writeBinMem, readBinMem, putByte, getByte,
                       getString, putString )

import Name          ( Name, nameOccName, nameModule, mkExternalName )
import OccName       ( OccName, isVarOcc, isDataOcc, isTvOcc, isTcOcc, varName, 
                       dataName, tvName, tcClsName, occNameString, mkOccName )
import Unique        ( mkUnique )
import Module        ( Module, mkModule, mkModuleName, modulePackageId ) 
import PackageConfig ( packageIdString, stringToPackageId )
import SrcLoc        ( noSrcLoc ) 

import qualified Data.Map as Map
import Data.Map (Map)

------------------------------------------------------------------------------
-- Reading the current interface format
------------------------------------------------------------------------------

thisFormatVersion :: FormatVersion
thisFormatVersion = mkFormatVersion 3

dumpInterface :: Map Name Name -> [Module] -> FilePath -> IO ()
dumpInterface globalDocEnv modules fileName = do
  bh <- openBinMem 100000
  put_ bh thisFormatVersion
  mapM (put_ bh) modules
  putDocEnv bh globalDocEnv
  writeBinMem bh fileName

putDocEnv :: BinHandle -> Map Name Name -> IO ()
putDocEnv bh env = put_ bh list 
  where 
    list = [ (nameModule o, nameOccName o, nameModule e)  | 
             (o, e) <- Map.toList env ]

getDocEnv :: BinHandle -> IO (Map Name Name)
getDocEnv bh = do
  list <- get bh
  return (Map.fromList [(mkName mdl1 occ, mkName mdl2 occ) | 
                        (mdl1,occ,mdl2) <- list])

mkName mdl occ = mkExternalName (mkUnique 'X' 0) mdl occ Nothing noSrcLoc

type Interface = ([Module], Map Name Name)

readInterface :: FilePath -> IO Interface
readInterface fileName = do
  bh <- readBinMem fileName
  formatVersion <- get bh
  case formatVersion of
    v | v == thisFormatVersion -> do
      modules::[Module] <- get bh 
      env <- getDocEnv bh
      return (modules, env)
--    v | v == mkFormatVersion 2 -> do
--      (stuff :: [StoredInterface2]) <- get bh
--      return ([], Map.empty)
--      doc_env <- getDocEnv bh
--      return (map toInterface2 stuff, doc_env)
    otherwise -> do
            noDieMsg (
               "Warning: The interface file " ++ show fileName 
                  ++ " could not be read.\n"
                  ++ "Interface files from earlier version of Haddock are not "
                  ++ "supported yet.\n")
            return ([],Map.empty)

encodeNS n 
  | isVarOcc  n = 0
  | isDataOcc n = 1
  | isTvOcc   n = 2         
  | isTcOcc   n = 3

decodeNS n = case n of 
  0 -> varName
  1 -> dataName
  2 -> tvName
  _ -> tcClsName

instance Binary OccName where
  put_ bh n = do
    put_ bh (occNameString n)
    putByte bh (encodeNS n)
  get bh = do
    string <- get bh
    ns <- getByte bh
    return (mkOccName (decodeNS ns) string)

instance Binary Module where
  put_ bh m = do
    put_ bh (moduleString m)
    put_ bh ((packageIdString . modulePackageId) m)
  get bh = do 
    m <- get bh
    p <- get bh
    return (mkModule (stringToPackageId p) (mkModuleName m))

------------------------------------------------------------------------------
-- Reading old interface formats
------------------------------------------------------------------------------

type StoredInterface2 =
   (Module2, Maybe Doc2, Maybe String, Bool, 
    [(HsName, Module2)], [(HsName, [HsName])])

newtype Module2 = Module2 String
  deriving (Eq,Ord)

data HsQName
	= Qual Module HsName
	| UnQual HsName
  deriving (Eq,Ord)

data HsName 
	= HsTyClsName HsIdentifier
	| HsVarName HsIdentifier
  deriving (Eq,Ord)

data HsIdentifier
	= HsIdent   String
	| HsSymbol  String
	| HsSpecial String
  deriving (Eq,Ord)

data GenDoc id
  = DocEmpty 
  | DocAppend (GenDoc id) (GenDoc id)
  | DocString String
  | DocParagraph (GenDoc id)
  | DocIdentifier id
  | DocModule String
  | DocEmphasis (GenDoc id)
  | DocMonospaced (GenDoc id)
  | DocUnorderedList [GenDoc id]
  | DocOrderedList [GenDoc id]
  | DocDefList [(GenDoc id, GenDoc id)]
  | DocCodeBlock (GenDoc id)
  | DocURL String
  | DocAName String
  deriving (Eq, Show)

type Doc2 = GenDoc [HsQName]

------------------------------------------------------------------------------
-- Binary instances for stuff
------------------------------------------------------------------------------

instance Binary Module2 where
  put_ bh (Module2 m) = putString bh m
  get bh = do m <- getString bh; return $! (Module2 m)

instance Binary HsQName where
  put_ bh (Qual m s) = do putByte bh 0; put_ bh m; put_ bh s
  put_ bh (UnQual s) = do putByte bh 1; put_ bh s
  get bh = do b <- getByte bh
	      case b of
		0 -> do m <- get bh; s <- get bh; return (Qual m s)
		_ -> do s <- get bh; return (UnQual s)

instance Binary HsName where
  put_ bh (HsTyClsName s) = do putByte bh 0; put_ bh s
  put_ bh (HsVarName s)   = do putByte bh 1; put_ bh s
  get bh = do b <- getByte bh
	      case b of
		0 -> do s <- get bh; return (HsTyClsName s)
		_ -> do s <- get bh; return (HsVarName s)

instance Binary HsIdentifier where
  put_ bh (HsIdent s)   = do putByte bh 0; putString bh s
  put_ bh (HsSymbol s)  = do putByte bh 1; putString bh s
  put_ bh (HsSpecial s) = do putByte bh 2; putString bh s
  get bh = do b <- getByte bh
	      case b of
		0 -> do s <- getString bh; return (HsIdent s)
		1 -> do s <- getString bh; return (HsSymbol s)
		_ -> do s <- getString bh; return (HsSpecial s)

instance Binary id => Binary (GenDoc id) where
   put_ bh DocEmpty = putByte bh 0
   put_ bh (DocAppend gd1 gd2) = do putByte bh 1;put_ bh gd1;put_ bh gd2
   put_ bh (DocString s) = do putByte bh 2;putString bh s
   put_ bh (DocParagraph gd) = do putByte bh 3;put_ bh gd
   put_ bh (DocIdentifier id) = do putByte bh 4;put_ bh id
   put_ bh (DocModule s) = do putByte bh 5;putString bh s
   put_ bh (DocEmphasis gd) = do putByte bh 6;put_ bh gd
   put_ bh (DocMonospaced gd) = do putByte bh 7;put_ bh gd
   put_ bh (DocUnorderedList gd) = do putByte bh 8;put_ bh gd
   put_ bh (DocOrderedList gd) = do putByte bh 9;put_ bh gd
   put_ bh (DocDefList gd) = do putByte bh 10;put_ bh gd
   put_ bh (DocCodeBlock gd) = do putByte bh 11;put_ bh gd
   put_ bh (DocURL s) = do putByte bh 12;putString bh s
   put_ bh (DocAName s) = do putByte bh 13;putString bh s
   get bh = do b <- getByte bh
               case b of
                  0 -> return DocEmpty
                  1 -> do gd1 <- get bh;gd2 <- get bh;return (DocAppend gd1 gd2)
                  2 -> do s <- getString bh;return (DocString s)
                  3 -> do gd <- get bh;return (DocParagraph gd)
                  4 -> do id <- get bh;return (DocIdentifier id)
                  5 -> do s <- getString bh;return (DocModule s)
                  6 -> do gd <- get bh;return (DocEmphasis gd)
                  7 -> do gd <- get bh;return (DocMonospaced gd)
                  8 -> do gd <- get bh;return (DocUnorderedList gd)
                  9 -> do gd <- get bh;return (DocOrderedList gd)
                  10 -> do gd <- get bh;return (DocDefList gd)
                  11 -> do gd <- get bh;return (DocCodeBlock gd)
                  12 -> do s <- getString bh;return (DocURL s)
                  13 -> do s <- getString bh;return (DocAName s) 
                  _ -> error ("Mysterious byte in document in interface" 
                     ++ show b)

{-
-- | How we store interfaces.  Not everything is stored.
type StoredInterface1 =
   (Module,Maybe Doc,Maybe String,Bool,[(HsName,HsQName)],[(HsName,HsQName)],
      [(HsName,[HsName])])

-- | How we used to store interfaces.
type NullVersionStoredInterface = 
   (Module,Maybe String,Bool,[(HsName,HsQName)],[(HsName,HsQName)],
      [(HsName,[HsName])])

dumpInterfaces :: [Interface] -> Map HsQName HsQName -> FilePath -> IO ()
dumpInterfaces interfaces global_doc_env fileName =
   do
      let
         preparedInterfaces :: [StoredInterface2]
         preparedInterfaces = map from_interface interfaces

      bh <- openBinMem 100000
      put_ bh thisFormatVersion
      put_ bh preparedInterfaces
      putDocEnv bh global_doc_env
      writeBinMem bh fileName


readIface :: FilePath -> IO ([Interface], Map HsQName HsQName)
readIface fileName = do
   bh <- readBinMem fileName
   formatVersion <- get bh
   case formatVersion of
     v | v == thisFormatVersion -> do
            (stuff :: [StoredInterface2]) <- get bh
	    doc_env <- getDocEnv bh
            return (map to_interface2 stuff, doc_env)
     v | v == mkFormatVersion 1 -> do
            (stuff :: [StoredInterface1]) <- get bh
            return (map to_interface1 stuff, Map.empty)
     v | v == nullFormatVersion -> do
            (stuff :: [NullVersionStoredInterface]) <- get bh
            return (map nullVersion_to_interface stuff, Map.empty)
     otherwise -> do
            noDieMsg (
               "Warning: The interface file " ++ show fileName 
                  ++ " could not be read.\n"
                  ++ "Maybe it's from a later version of Haddock?\n")
            return ([], Map.empty)

from_interface :: Interface -> StoredInterface2
from_interface iface =
   (  iface_module iface,
      toDescription iface,iface_package iface,
      OptHide `elem` iface_options iface,
      [(n,mdl) | (n,Qual mdl n') <- Map.toAscList (iface_env iface),
		 if n /= n' then error "help!" else True], 
      Map.toAscList (iface_sub iface)
      )

getDocEnv :: BinHandle -> IO (Map HsQName HsQName)
getDocEnv bh = do
   doc_env_list <- get bh
   return (Map.fromList [(Qual mdl1 nm,Qual mdl2 nm) | 
			 (mdl1,nm,mdl2) <- doc_env_list])

to_interface1 :: StoredInterface1 -> Interface
to_interface1 (mdl,descriptionOpt,package, hide, env, _, sub) = 
   Interface { 
      iface_module	 = mdl,
      iface_filename     = "",
      iface_orig_filename= "",
      iface_package      = package,
      iface_env          = Map.fromList env,
      iface_sub          = Map.fromList sub,
      iface_reexported   = [],
      iface_exports      = [],
      iface_orig_exports = [],
      iface_insts        = [],
      iface_decls        = Map.empty,
      iface_info         = toModuleInfo descriptionOpt,
      iface_doc          = Nothing,
      iface_options      = if hide then [OptHide] else []
      }

to_interface2 :: StoredInterface2 -> Interface
to_interface2 (mdl,descriptionOpt,package, hide, env, sub) =
   Interface { 
      iface_module	 = mdl,
      iface_filename     = "",
      iface_orig_filename= "",
      iface_package      = package,
      iface_env          = 
	Map.fromList [(n,Qual mdl n) | (n,mdl) <- env],
      iface_sub          = Map.fromList sub,
      iface_reexported   = [],
      iface_exports      = [],
      iface_orig_exports = [],
      iface_insts        = [],
      iface_decls        = Map.empty,
      iface_info         = toModuleInfo descriptionOpt,
      iface_doc          = Nothing,
      iface_options      = if hide then [OptHide] else []
      }

nullVersion_to_interface :: NullVersionStoredInterface -> Interface
nullVersion_to_interface (mdl, package, hide, env, reexported, sub) = 
   Interface { 
      iface_module	 = mdl,
      iface_filename     = "",
      iface_orig_filename= "",
      iface_package      = package,
      iface_env          = Map.fromList env,
      iface_sub          = Map.fromList sub,
      iface_reexported   = [],
      iface_exports      = [],
      iface_orig_exports = [],
      iface_insts        = [],
      iface_decls        = Map.empty,
      iface_info         = emptyModuleInfo,
      iface_doc          = Nothing,
      iface_options      = if hide then [OptHide] else []
      }

toModuleInfo :: Maybe Doc -> ModuleInfo
toModuleInfo descriptionOpt = 
   emptyModuleInfo {description = descriptionOpt}

-}
