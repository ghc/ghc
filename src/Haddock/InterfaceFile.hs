{-# OPTIONS_GHC -fno-warn-orphans #-}
--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.InterfaceFile (
  InterfaceFile(..),
  readInterfaceFile,
  writeInterfaceFile
) where


import Haddock.DocName ()
import Haddock.Types

import Data.List
import Data.Word
import Data.Array
import Data.IORef
import qualified Data.Map as Map
import System.IO
import Control.Monad

import GHC hiding (NoLink)
import SrcLoc   (noSrcSpan) -- tmp, GHC now exports this
import Binary
import Name
import UniqSupply
import UniqFM
import IfaceEnv
import Module
import HscTypes
import FastMutInt
import HsDoc
import FastString
import Unique


data InterfaceFile = InterfaceFile {
  ifLinkEnv         :: LinkEnv,
  ifInstalledIfaces :: [InstalledInterface]
} 


binaryInterfaceMagic :: Word32
binaryInterfaceMagic = 0xD0Cface


-- Since datatypes in GHC might change between patchlevel versions,
-- and because we store GHC datatypes in our interface files,
-- we need to make sure we version our interface files accordingly.
--
-- Instead of adding one, we add three to all version numbers
-- when one of our own (stored) datatypes is changed. 
binaryInterfaceVersion :: Word16
#if __GLASGOW_HASKELL__ == 608 && __GHC_PATCHLEVEL__ == 2
binaryInterfaceVersion = 2
#endif
#if __GLASGOW_HASKELL__ == 608 && __GHC_PATCHLEVEL__ == 3
binaryInterfaceVersion = 3
#endif
#if __GLASGOW_HASKELL__ >= 609
binaryInterfaceVersion = 4
#endif


initBinMemSize :: Int
initBinMemSize = 1024*1024


writeInterfaceFile :: FilePath -> InterfaceFile -> IO ()
writeInterfaceFile filename iface = do 
  bh0 <- openBinMem initBinMemSize
  put_ bh0 binaryInterfaceMagic
  put_ bh0 binaryInterfaceVersion

  -- remember where the dictionary pointer will go
  dict_p_p <- tellBin bh0
  put_ bh0 dict_p_p

  -- remember where the symbol table pointer will go
  symtab_p_p <- tellBin bh0
  put_ bh0 symtab_p_p

  -- Make some intial state
#if __GLASGOW_HASKELL__ >= 609
  symtab_next <- newFastMutInt
  writeFastMutInt symtab_next 0
  symtab_map <- newIORef emptyUFM
  let bin_symtab = BinSymbolTable {
                      bin_symtab_next = symtab_next,
                      bin_symtab_map  = symtab_map }
  dict_next_ref <- newFastMutInt
  writeFastMutInt dict_next_ref 0
  dict_map_ref <- newIORef emptyUFM
  let bin_dict = BinDictionary {
                      bin_dict_next = dict_next_ref,
                      bin_dict_map  = dict_map_ref }
  ud <- newWriteState (putName bin_symtab) (putFastString bin_dict)
#else
  ud <- newWriteState
#endif

  -- put the main thing
  bh <- return $ setUserData bh0 ud
  put_ bh iface

  -- write the symtab pointer at the fornt of the file
  symtab_p <- tellBin bh
  putAt bh symtab_p_p symtab_p
  seekBin bh symtab_p		

  -- write the symbol table itself
#if __GLASGOW_HASKELL__ >= 609
  symtab_next' <- readFastMutInt symtab_next
  symtab_map'  <- readIORef symtab_map
#else
  symtab_next' <- readFastMutInt (ud_symtab_next ud)
  symtab_map'  <- readIORef (ud_symtab_map ud)
#endif
  putSymbolTable bh symtab_next' symtab_map'

  -- write the dictionary pointer at the fornt of the file
  dict_p <- tellBin bh
  putAt bh dict_p_p dict_p
  seekBin bh dict_p

  -- write the dictionary itself
#if __GLASGOW_HASKELL__ >= 609
  dict_next <- readFastMutInt dict_next_ref
  dict_map  <- readIORef dict_map_ref
#else
  dict_next <- readFastMutInt (ud_dict_next ud)
  dict_map  <- readIORef (ud_dict_map ud)
#endif
  putDictionary bh dict_next dict_map

  -- and send the result to the file
  writeBinMem bh filename
  return ()


-- | Read a Haddock (@.haddock@) interface file. Return either an 
-- 'InterfaceFile' or an error message. If given a GHC 'Session', the function
-- registers all read names in the name cache of the session.
readInterfaceFile :: Maybe Session -> FilePath -> IO (Either String InterfaceFile)
readInterfaceFile mbSession filename = do
  bh0 <- readBinMem filename

  magic   <- get bh0
  version <- get bh0

  case () of
    _ | magic /= binaryInterfaceMagic -> return . Left $
      "Magic number mismatch: couldn't load interface file: " ++ filename
      | version /= binaryInterfaceVersion -> return . Left $
      "Interface file is of wrong version: " ++ filename
      | otherwise -> do

      -- get the dictionary
      dict_p <- get bh0
      data_p <- tellBin bh0
      seekBin bh0 dict_p
      dict <- getDictionary bh0
      seekBin bh0 data_p

      -- initialise the user-data field of bh0
      ud <- newReadState dict
      bh1 <- return (setUserData bh0 ud)

      -- get the name cache from ghc if we have a ghc session,
      -- otherwise create a new one
      (theNC, mbRef) <- case mbSession of
        Just session -> do
          ref <- withSession session (return . hsc_NC)
          nc <- readIORef ref
          return (nc, Just ref)
        Nothing -> do
          -- construct an empty name cache
          u  <- mkSplitUniqSupply 'a' -- ??
          return (initNameCache u [], Nothing)

      -- get the symbol table
      symtab_p <- get bh1
      data_p'  <- tellBin bh1
      seekBin bh1 symtab_p
      (nc', symtab) <- getSymbolTable bh1 theNC
      seekBin bh1 data_p'

      -- write back the new name cache if we have a ghc session
      case mbRef of
        Just ref -> writeIORef ref nc'
        Nothing  -> return ()

      -- set the symbol table
      let ud' = getUserData bh1
      bh2 <- return $! setUserData bh1 ud'{ud_symtab = symtab}

      -- load the actual data
      iface <- get bh2
      return (Right iface)


-------------------------------------------------------------------------------
-- Symbol table
-------------------------------------------------------------------------------


#if __GLASGOW_HASKELL__ >= 609
putName :: BinSymbolTable -> BinHandle -> Name -> IO ()
putName BinSymbolTable{
            bin_symtab_map = symtab_map_ref,
            bin_symtab_next = symtab_next }    bh name
  = do
    symtab_map <- readIORef symtab_map_ref
    case lookupUFM symtab_map name of
      Just (off,_) -> put_ bh off
      Nothing -> do
         off <- readFastMutInt symtab_next
         writeFastMutInt symtab_next (off+1)
         writeIORef symtab_map_ref
             $! addToUFM symtab_map name (off,name)
         put_ bh off


data BinSymbolTable = BinSymbolTable {
        bin_symtab_next :: !FastMutInt, -- The next index to use
        bin_symtab_map  :: !(IORef (UniqFM (Int,Name)))
                                -- indexed by Name
  }


putFastString :: BinDictionary -> BinHandle -> FastString -> IO ()
putFastString BinDictionary { bin_dict_next = j_r,
                              bin_dict_map  = out_r}  bh f
  = do
    out <- readIORef out_r
    let unique = getUnique f
    case lookupUFM out unique of
        Just (j, _)  -> put_ bh j
        Nothing -> do
           j <- readFastMutInt j_r
           put_ bh j
           writeFastMutInt j_r (j + 1)
           writeIORef out_r $! addToUFM out unique (j, f)


data BinDictionary = BinDictionary {
        bin_dict_next :: !FastMutInt, -- The next index to use
        bin_dict_map  :: !(IORef (UniqFM (Int,FastString)))
                                -- indexed by FastString
  }
#endif


putSymbolTable :: BinHandle -> Int -> UniqFM (Int,Name) -> IO ()
putSymbolTable bh next_off symtab = do
  put_ bh next_off
  let names = elems (array (0,next_off-1) (eltsUFM symtab))
  mapM_ (\n -> serialiseName bh n symtab) names

getSymbolTable :: BinHandle -> NameCache -> IO (NameCache, Array Int Name)
getSymbolTable bh namecache = do
  sz <- get bh
  od_names <- sequence (replicate sz (get bh))
  let 
        arr = listArray (0,sz-1) names
        (namecache', names) =    
                mapAccumR (fromOnDiskName arr) namecache od_names
  --
  return (namecache', arr)

type OnDiskName = (PackageId, ModuleName, OccName)

fromOnDiskName
   :: Array Int Name
   -> NameCache
   -> OnDiskName
   -> (NameCache, Name)
fromOnDiskName _ nc (pid, mod_name, occ) =
  let 
        modu  = mkModule pid mod_name
        cache = nsNames nc
  in
  case lookupOrigNameCache cache modu occ of
     Just name -> (nc, name)
     Nothing   -> 
        let 
                us        = nsUniqs nc
                u         = uniqFromSupply us
                name      = mkExternalName u modu occ noSrcSpan
                new_cache = extendNameCache cache modu occ name
        in        
        case splitUniqSupply us of { (us',_) -> 
        ( nc{ nsUniqs = us', nsNames = new_cache }, name )
        }

serialiseName :: BinHandle -> Name -> UniqFM (Int,Name) -> IO ()
serialiseName bh name _ = do
  let modu = nameModule name
  put_ bh (modulePackageId modu, moduleName modu, nameOccName name)


-------------------------------------------------------------------------------
-- GhcBinary instances
-------------------------------------------------------------------------------


instance Binary InterfaceFile where
  put_ bh (InterfaceFile env ifaces) = do
    put_ bh (Map.toList env)
    put_ bh ifaces

  get bh = do
    env    <- get bh
    ifaces <- get bh
    return (InterfaceFile (Map.fromList env) ifaces)


instance Binary InstalledInterface where
  put_ bh (InstalledInterface modu info docMap exps visExps) = do
    put_ bh modu
    put_ bh info
    put_ bh (Map.toList docMap)
    put_ bh exps
    put_ bh visExps

  get bh = do
    modu    <- get bh
    info    <- get bh
    docMap  <- get bh
    exps    <- get bh
    visExps <- get bh
    return (InstalledInterface modu info (Map.fromList docMap) exps visExps)


instance Binary DocOption where
    put_ bh OptHide = do
            putByte bh 0
    put_ bh OptPrune = do
            putByte bh 1
    put_ bh OptIgnoreExports = do
            putByte bh 2
    put_ bh OptNotHome = do
            putByte bh 3
    get bh = do
            h <- getByte bh
            case h of
              0 -> do
                    return OptHide
              1 -> do
                    return OptPrune
              2 -> do
                    return OptIgnoreExports
              3 -> do
                    return OptNotHome
              _ -> fail "invalid binary data found"


{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance (Binary id) => Binary (HsDoc id) where
    put_ bh DocEmpty = do
            putByte bh 0
    put_ bh (DocAppend aa ab) = do
            putByte bh 1
            put_ bh aa
            put_ bh ab
    put_ bh (DocString ac) = do
            putByte bh 2
            put_ bh ac
    put_ bh (DocParagraph ad) = do
            putByte bh 3
            put_ bh ad
    put_ bh (DocIdentifier ae) = do
            putByte bh 4
            put_ bh ae
    put_ bh (DocModule af) = do
            putByte bh 5
            put_ bh af
    put_ bh (DocEmphasis ag) = do
            putByte bh 6
            put_ bh ag
    put_ bh (DocMonospaced ah) = do
            putByte bh 7
            put_ bh ah
    put_ bh (DocUnorderedList ai) = do
            putByte bh 8
            put_ bh ai
    put_ bh (DocOrderedList aj) = do
            putByte bh 9
            put_ bh aj
    put_ bh (DocDefList ak) = do
            putByte bh 10
            put_ bh ak
    put_ bh (DocCodeBlock al) = do
            putByte bh 11
            put_ bh al
    put_ bh (DocURL am) = do
            putByte bh 12
            put_ bh am
    put_ bh (DocPic x) = do
            putByte bh 13
            put_ bh x
    put_ bh (DocAName an) = do
            putByte bh 14
            put_ bh an
    get bh = do
            h <- getByte bh
            case h of
              0 -> do
                    return DocEmpty
              1 -> do
                    aa <- get bh
                    ab <- get bh
                    return (DocAppend aa ab)
              2 -> do
                    ac <- get bh
                    return (DocString ac)
              3 -> do
                    ad <- get bh
                    return (DocParagraph ad)
              4 -> do
                    ae <- get bh
                    return (DocIdentifier ae)
              5 -> do
                    af <- get bh
                    return (DocModule af)
              6 -> do
                    ag <- get bh
                    return (DocEmphasis ag)
              7 -> do
                    ah <- get bh
                    return (DocMonospaced ah)
              8 -> do
                    ai <- get bh
                    return (DocUnorderedList ai)
              9 -> do
                    aj <- get bh
                    return (DocOrderedList aj)
              10 -> do
                    ak <- get bh
                    return (DocDefList ak)
              11 -> do
                    al <- get bh
                    return (DocCodeBlock al)
              12 -> do
                    am <- get bh
                    return (DocURL am)
              13 -> do
                    x <- get bh
                    return (DocPic x)
              14 -> do
                    an <- get bh
                    return (DocAName an)
              _ -> fail "invalid binary data found"


instance Binary name => Binary (HaddockModInfo name) where
  put_ bh hmi = do
    put_ bh (hmi_description hmi)
    put_ bh (hmi_portability hmi)
    put_ bh (hmi_stability   hmi)
    put_ bh (hmi_maintainer  hmi)
  
  get bh = do
    descr <- get bh
    porta <- get bh
    stabi <- get bh
    maint <- get bh
    return (HaddockModInfo descr porta stabi maint)
