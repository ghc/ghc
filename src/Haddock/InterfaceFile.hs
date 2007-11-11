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


import Haddock.Types
import Haddock.Exception

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
import Packages
import HscTypes
import FastMutInt
import InstEnv
import HsDoc


data InterfaceFile = InterfaceFile {
  ifLinkEnv         :: LinkEnv,
  ifInstalledIfaces :: [InstalledInterface]
} 


binaryInterfaceMagic = 0xD0Cface :: Word32
binaryInterfaceVersion = 0 :: Word16


initBinMemSize = (1024*1024) :: Int


writeInterfaceFile :: FilePath -> InterfaceFile -> IO ()
writeInterfaceFile filename iface = do 
  bh <- openBinMem initBinMemSize
  put_ bh binaryInterfaceMagic
  put_ bh binaryInterfaceVersion

  -- remember where the dictionary pointer will go
  dict_p_p <- tellBin bh
  put_ bh dict_p_p	

  -- remember where the symbol table pointer will go
  symtab_p_p <- tellBin bh
  put_ bh symtab_p_p

  -- Make some intial state
  ud <- newWriteState

  -- put the main thing
  bh <- return $ setUserData bh ud
  put_ bh iface

  -- write the symtab pointer at the fornt of the file
  symtab_p <- tellBin bh
  putAt bh symtab_p_p symtab_p
  seekBin bh symtab_p		

  -- write the symbol table itself
  symtab_next <- readFastMutInt (ud_symtab_next ud)
  symtab_map  <- readIORef (ud_symtab_map ud)
  putSymbolTable bh symtab_next symtab_map

  -- write the dictionary pointer at the fornt of the file
  dict_p <- tellBin bh
  putAt bh dict_p_p dict_p
  seekBin bh dict_p

  -- write the dictionary itself
  dict_next <- readFastMutInt (ud_dict_next ud)
  dict_map  <- readIORef (ud_dict_map ud)
  putDictionary bh dict_next dict_map

	-- snd send the result to the file
  writeBinMem bh filename
  return ()


readInterfaceFile :: Maybe Session -> FilePath -> IO (Either String InterfaceFile)
readInterfaceFile mbSession filename = do
  bh <- readBinMem filename

  magic   <- get bh
  version <- get bh

  case () of
    _ | magic /= binaryInterfaceMagic -> return . Left $
      "Magic number mismatch: couldn't load interface file: " ++ filename
      | version /= binaryInterfaceVersion -> return . Left $
      "Interface file is of wrong version: " ++ filename
      | otherwise -> do

      -- get the dictionary
      dict_p <- get bh
      data_p <- tellBin bh		
      seekBin bh dict_p
      dict <- getDictionary bh
      seekBin bh data_p		

      -- initialise the user-data field of bh
      ud <- newReadState dict
      bh <- return (setUserData bh ud)

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
      symtab_p <- get bh
      data_p   <- tellBin bh
      seekBin bh symtab_p
      (nc', symtab) <- getSymbolTable bh theNC
      seekBin bh data_p

      -- write back the new name cache if we have a ghc session
      case mbRef of
        Just ref -> writeIORef ref nc'
        Nothing  -> return ()

      -- set the symbol table
      let ud = getUserData bh
      bh <- return $! setUserData bh ud{ud_symtab = symtab}

      -- load the actual data
      iface <- get bh
      return (Right iface)


-------------------------------------------------------------------------------
-- Symbol table
-------------------------------------------------------------------------------

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
fromOnDiskName arr nc (pid, mod_name, occ) =
  let 
        mod   = mkModule pid mod_name
        cache = nsNames nc
  in
  case lookupOrigNameCache cache  mod occ of
     Just name -> (nc, name)
     Nothing   -> 
        let 
                us        = nsUniqs nc
                uniq      = uniqFromSupply us
                name      = mkExternalName uniq mod occ noSrcSpan
                new_cache = extendNameCache cache mod occ name
        in        
        case splitUniqSupply us of { (us',_) -> 
        ( nc{ nsUniqs = us', nsNames = new_cache }, name )
        }

serialiseName :: BinHandle -> Name -> UniqFM (Int,Name) -> IO ()
serialiseName bh name symtab = do
  let mod = nameModule name
  put_ bh (modulePackageId mod, moduleName mod, nameOccName name)


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
  put_ bh (InstalledInterface mod info docMap exps visExps) = do
    put_ bh mod
    put_ bh info
    put_ bh (Map.toList docMap)
    put_ bh exps
    put_ bh visExps

  get bh = do
    mod     <- get bh
    info    <- get bh
    docMap  <- get bh
    exps    <- get bh
    visExps <- get bh
    return (InstalledInterface mod info (Map.fromList docMap) exps visExps)


{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance Binary DocName where
    put_ bh (Link aa) = do
            putByte bh 0
            put_ bh aa
    put_ bh (NoLink ab) = do
            putByte bh 1
            put_ bh ab
    get bh = do
            h <- getByte bh
            case h of
              0 -> do
                    aa <- get bh
                    return (Link aa)
              1 -> do
                    ab <- get bh
                    return (NoLink ab)
              _ -> fail "invalid binary data found"


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
    put_ bh (DocAName an) = do
            putByte bh 13
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
