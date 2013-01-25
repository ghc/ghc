--
--  (c) The University of Glasgow 2002-2006
--

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -O #-}
-- We always optimise this, otherwise performance of a non-optimised
-- compiler is severely affected

-- | Binary interface file support.
module BinIface (
        writeBinIface,
        readBinIface,
        getSymtabName,
        getDictFastString,
        CheckHiWay(..),
        TraceBinIFaceReading(..)
    ) where

#include "HsVersions.h"

import TcRnMonad
import TyCon
import DataCon    (dataConName, dataConWorkId, dataConTyCon)
import PrelInfo   (wiredInThings, basicKnownKeyNames)
import Id         (idName, isDataConWorkId_maybe)
import CoreSyn    (DFunArg(..))
import Coercion   (LeftOrRight(..))
import TysWiredIn
import IfaceEnv
import HscTypes
import BasicTypes
import Demand
import Annotations
import IfaceSyn
import Module
import Name
import Avail
import VarEnv
import DynFlags
import UniqFM
import UniqSupply
import CostCentre
import Panic
import Binary
import SrcLoc
import ErrUtils
import FastMutInt
import Unique
import Outputable
import Platform
import FastString
import Constants
import Util

import Data.Bits
import Data.Char
import Data.List
import Data.Word
import Data.Array
import Data.IORef
import Control.Monad


-- ---------------------------------------------------------------------------
-- Reading and writing binary interface files
--

data CheckHiWay = CheckHiWay | IgnoreHiWay
    deriving Eq

data TraceBinIFaceReading = TraceBinIFaceReading | QuietBinIFaceReading
    deriving Eq

-- | Read an interface file
readBinIface :: CheckHiWay -> TraceBinIFaceReading -> FilePath
             -> TcRnIf a b ModIface
readBinIface checkHiWay traceBinIFaceReading hi_path = do
    ncu <- mkNameCacheUpdater
    dflags <- getDynFlags
    liftIO $ readBinIface_ dflags checkHiWay traceBinIFaceReading hi_path ncu

readBinIface_ :: DynFlags -> CheckHiWay -> TraceBinIFaceReading -> FilePath
              -> NameCacheUpdater
              -> IO ModIface
readBinIface_ dflags checkHiWay traceBinIFaceReading hi_path ncu = do
    let printer :: SDoc -> IO ()
        printer = case traceBinIFaceReading of
                      TraceBinIFaceReading -> \sd -> log_action dflags dflags SevOutput noSrcSpan defaultDumpStyle sd
                      QuietBinIFaceReading -> \_ -> return ()
        wantedGot :: Outputable a => String -> a -> a -> IO ()
        wantedGot what wanted got =
            printer (text what <> text ": " <>
                     vcat [text "Wanted " <> ppr wanted <> text ",",
                           text "got    " <> ppr got])

        errorOnMismatch :: (Eq a, Show a) => String -> a -> a -> IO ()
        errorOnMismatch what wanted got =
            -- This will be caught by readIface which will emit an error
            -- msg containing the iface module name.
            when (wanted /= got) $ throwGhcException $ ProgramError
                         (what ++ " (wanted " ++ show wanted
                               ++ ", got "    ++ show got ++ ")")
    bh <- Binary.readBinMem hi_path

    -- Read the magic number to check that this really is a GHC .hi file
    -- (This magic number does not change when we change
    --  GHC interface file format)
    magic <- get bh
    wantedGot "Magic" (binaryInterfaceMagic dflags) magic
    errorOnMismatch "magic number mismatch: old/corrupt interface file?"
        (binaryInterfaceMagic dflags) magic

    -- Note [dummy iface field]
    -- read a dummy 32/64 bit value.  This field used to hold the
    -- dictionary pointer in old interface file formats, but now
    -- the dictionary pointer is after the version (where it
    -- should be).  Also, the serialisation of value of type "Bin
    -- a" used to depend on the word size of the machine, now they
    -- are always 32 bits.
    if wORD_SIZE dflags == 4
        then do _ <- Binary.get bh :: IO Word32; return ()
        else do _ <- Binary.get bh :: IO Word64; return ()

    -- Check the interface file version and ways.
    check_ver  <- get bh
    let our_ver = show hiVersion
    wantedGot "Version" our_ver check_ver
    errorOnMismatch "mismatched interface file versions" our_ver check_ver

    check_way <- get bh
    let way_descr = getWayDescr dflags
    wantedGot "Way" way_descr check_way
    when (checkHiWay == CheckHiWay) $
        errorOnMismatch "mismatched interface file ways" way_descr check_way

    -- Read the dictionary
    -- The next word in the file is a pointer to where the dictionary is
    -- (probably at the end of the file)
    dict_p <- Binary.get bh
    data_p <- tellBin bh          -- Remember where we are now
    seekBin bh dict_p
    dict   <- getDictionary bh
    seekBin bh data_p             -- Back to where we were before

    -- Initialise the user-data field of bh
    bh <- do
        bh <- return $ setUserData bh $ newReadState (error "getSymtabName")
                                                     (getDictFastString dict)
        symtab_p <- Binary.get bh     -- Get the symtab ptr
        data_p <- tellBin bh          -- Remember where we are now
        seekBin bh symtab_p
        symtab <- getSymbolTable bh ncu
        seekBin bh data_p             -- Back to where we were before
    
        -- It is only now that we know how to get a Name
        return $ setUserData bh $ newReadState (getSymtabName ncu dict symtab)
                                               (getDictFastString dict)

    -- Read the interface file
    get bh

-- | Write an interface file
writeBinIface :: DynFlags -> FilePath -> ModIface -> IO ()
writeBinIface dflags hi_path mod_iface = do
    bh <- openBinMem initBinMemSize
    put_ bh (binaryInterfaceMagic dflags)

   -- dummy 32/64-bit field before the version/way for
   -- compatibility with older interface file formats.
   -- See Note [dummy iface field] above.
    if wORD_SIZE dflags == 4
        then Binary.put_ bh (0 :: Word32)
        else Binary.put_ bh (0 :: Word64)

    -- The version and way descriptor go next
    put_ bh (show hiVersion)
    let way_descr = getWayDescr dflags
    put_  bh way_descr

    -- Remember where the dictionary pointer will go
    dict_p_p <- tellBin bh
    -- Placeholder for ptr to dictionary
    put_ bh dict_p_p

    -- Remember where the symbol table pointer will go
    symtab_p_p <- tellBin bh
    put_ bh symtab_p_p

    -- Make some intial state
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
  
    -- Put the main thing, 
    bh <- return $ setUserData bh $ newWriteState (putName bin_dict bin_symtab)
                                                  (putFastString bin_dict)
    put_ bh mod_iface

    -- Write the symtab pointer at the fornt of the file
    symtab_p <- tellBin bh        -- This is where the symtab will start
    putAt bh symtab_p_p symtab_p  -- Fill in the placeholder
    seekBin bh symtab_p           -- Seek back to the end of the file

    -- Write the symbol table itself
    symtab_next <- readFastMutInt symtab_next
    symtab_map  <- readIORef symtab_map
    putSymbolTable bh symtab_next symtab_map
    debugTraceMsg dflags 3 (text "writeBinIface:" <+> int symtab_next 
                                <+> text "Names")

    -- NB. write the dictionary after the symbol table, because
    -- writing the symbol table may create more dictionary entries.

    -- Write the dictionary pointer at the fornt of the file
    dict_p <- tellBin bh          -- This is where the dictionary will start
    putAt bh dict_p_p dict_p      -- Fill in the placeholder
    seekBin bh dict_p             -- Seek back to the end of the file

    -- Write the dictionary itself
    dict_next <- readFastMutInt dict_next_ref
    dict_map  <- readIORef dict_map_ref
    putDictionary bh dict_next dict_map
    debugTraceMsg dflags 3 (text "writeBinIface:" <+> int dict_next
                                <+> text "dict entries")

    -- And send the result to the file
    writeBinMem bh hi_path

-- | Initial ram buffer to allocate for writing interface files
initBinMemSize :: Int
initBinMemSize = 1024 * 1024

binaryInterfaceMagic :: DynFlags -> Word32
binaryInterfaceMagic dflags
 | target32Bit (targetPlatform dflags) = 0x1face
 | otherwise                           = 0x1face64


-- -----------------------------------------------------------------------------
-- The symbol table
--

putSymbolTable :: BinHandle -> Int -> UniqFM (Int,Name) -> IO ()
putSymbolTable bh next_off symtab = do
    put_ bh next_off
    let names = elems (array (0,next_off-1) (eltsUFM symtab))
    mapM_ (\n -> serialiseName bh n symtab) names

getSymbolTable :: BinHandle -> NameCacheUpdater -> IO SymbolTable
getSymbolTable bh ncu = do
    sz <- get bh
    od_names <- sequence (replicate sz (get bh))
    updateNameCache ncu $ \namecache ->
        let arr = listArray (0,sz-1) names
            (namecache', names) =    
                mapAccumR (fromOnDiskName arr) namecache od_names
        in (namecache', arr)

type OnDiskName = (PackageId, ModuleName, OccName)

fromOnDiskName :: Array Int Name -> NameCache -> OnDiskName -> (NameCache, Name)
fromOnDiskName _ nc (pid, mod_name, occ) =
    let mod   = mkModule pid mod_name
        cache = nsNames nc
    in case lookupOrigNameCache cache  mod occ of
           Just name -> (nc, name)
           Nothing   ->
               let (uniq, us) = takeUniqFromSupply (nsUniqs nc)
                   name       = mkExternalName uniq mod occ noSrcSpan
                   new_cache  = extendNameCache cache mod occ name
               in ( nc{ nsUniqs = us, nsNames = new_cache }, name )

serialiseName :: BinHandle -> Name -> UniqFM (Int,Name) -> IO ()
serialiseName bh name _ = do
    let mod = ASSERT2( isExternalName name, ppr name ) nameModule name
    put_ bh (modulePackageId mod, moduleName mod, nameOccName name)


-- Note [Symbol table representation of names]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- An occurrence of a name in an interface file is serialized as a single 32-bit word.
-- The format of this word is:
--  00xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
--   A normal name. x is an index into the symbol table
--  01xxxxxxxxyyyyyyyyyyyyyyyyyyyyyyyy
--   A known-key name. x is the Unique's Char, y is the int part
--  10xxyyzzzzzzzzzzzzzzzzzzzzzzzzzzzz
--   A tuple name:
--    x is the tuple sort (00b ==> boxed, 01b ==> unboxed, 10b ==> constraint)
--    y is the thing (00b ==> tycon, 01b ==> datacon, 10b ==> datacon worker)
--    z is the arity
--  11xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
--   An implicit parameter TyCon name. x is an index into the FastString *dictionary*
--
-- Note that we have to have special representation for tuples and IP TyCons because they
-- form an "infinite" family and hence are not recorded explicitly in wiredInTyThings or
-- basicKnownKeyNames.

knownKeyNamesMap :: UniqFM Name
knownKeyNamesMap = listToUFM_Directly [(nameUnique n, n) | n <- knownKeyNames]
  where
    knownKeyNames :: [Name]
    knownKeyNames = map getName wiredInThings ++ basicKnownKeyNames


-- See Note [Symbol table representation of names]
putName :: BinDictionary -> BinSymbolTable -> BinHandle -> Name -> IO ()
putName _dict BinSymbolTable{ 
               bin_symtab_map = symtab_map_ref,
               bin_symtab_next = symtab_next }    bh name
  | name `elemUFM` knownKeyNamesMap
  , let (c, u) = unpkUnique (nameUnique name) -- INVARIANT: (ord c) fits in 8 bits
  = -- ASSERT(u < 2^(22 :: Int))
    put_ bh (0x40000000 .|. (fromIntegral (ord c) `shiftL` 22) .|. (fromIntegral u :: Word32))
  | otherwise
  = case wiredInNameTyThing_maybe name of
     Just (ATyCon tc)
       | isTupleTyCon tc             -> putTupleName_ bh tc 0
     Just (ADataCon dc)
       | let tc = dataConTyCon dc, isTupleTyCon tc -> putTupleName_ bh tc 1
     Just (AnId x)
       | Just dc <- isDataConWorkId_maybe x, let tc = dataConTyCon dc, isTupleTyCon tc -> putTupleName_ bh tc 2
     _ -> do
       symtab_map <- readIORef symtab_map_ref
       case lookupUFM symtab_map name of
         Just (off,_) -> put_ bh (fromIntegral off :: Word32)
         Nothing -> do
            off <- readFastMutInt symtab_next
            -- MASSERT(off < 2^(30 :: Int))
            writeFastMutInt symtab_next (off+1)
            writeIORef symtab_map_ref
                $! addToUFM symtab_map name (off,name)
            put_ bh (fromIntegral off :: Word32)

putTupleName_ :: BinHandle -> TyCon -> Word32 -> IO ()
putTupleName_ bh tc thing_tag
  = -- ASSERT(arity < 2^(30 :: Int))
    put_ bh (0x80000000 .|. (sort_tag `shiftL` 28) .|. (thing_tag `shiftL` 26) .|. arity)
  where
    arity = fromIntegral (tupleTyConArity tc)
    sort_tag = case tupleTyConSort tc of
        BoxedTuple      -> 0
        UnboxedTuple    -> 1
        ConstraintTuple -> 2

-- See Note [Symbol table representation of names]
getSymtabName :: NameCacheUpdater
              -> Dictionary -> SymbolTable
              -> BinHandle -> IO Name
getSymtabName _ncu _dict symtab bh = do
    i <- get bh
    case i .&. 0xC0000000 of
        0x00000000 -> return $! symtab ! fromIntegral (i ::  Word32)
        0x40000000 -> return $! case lookupUFM_Directly knownKeyNamesMap (mkUnique tag ix) of
                        Nothing -> pprPanic "getSymtabName:unknown known-key unique" (ppr i)
                        Just n  -> n
          where tag = chr (fromIntegral ((i .&. 0x3FC00000) `shiftR` 22))
                ix = fromIntegral i .&. 0x003FFFFF
        0x80000000 -> return $! case thing_tag of
                        0 -> tyConName (tupleTyCon sort arity)
                        1 -> dataConName dc
                        2 -> idName (dataConWorkId dc)
                        _ -> pprPanic "getSymtabName:unknown tuple thing" (ppr i)
          where
            dc = tupleCon sort arity
            sort = case (i .&. 0x30000000) `shiftR` 28 of
                     0 -> BoxedTuple
                     1 -> UnboxedTuple
                     2 -> ConstraintTuple
                     _ -> pprPanic "getSymtabName:unknown tuple sort" (ppr i)
            thing_tag = (i .&. 0x0CFFFFFF) `shiftR` 26
            arity = fromIntegral (i .&. 0x03FFFFFF)
        _          -> pprPanic "getSymtabName:unknown name tag" (ppr i)

data BinSymbolTable = BinSymbolTable {
        bin_symtab_next :: !FastMutInt, -- The next index to use
        bin_symtab_map  :: !(IORef (UniqFM (Int,Name)))
                                -- indexed by Name
  }


putFastString :: BinDictionary -> BinHandle -> FastString -> IO ()
putFastString dict bh fs = allocateFastString dict fs >>= put_ bh

allocateFastString :: BinDictionary -> FastString -> IO Word32
allocateFastString BinDictionary { bin_dict_next = j_r,
                                   bin_dict_map  = out_r} f = do
    out <- readIORef out_r
    let uniq = getUnique f
    case lookupUFM out uniq of
        Just (j, _)  -> return (fromIntegral j :: Word32)
        Nothing -> do
           j <- readFastMutInt j_r
           writeFastMutInt j_r (j + 1)
           writeIORef out_r $! addToUFM out uniq (j, f)
           return (fromIntegral j :: Word32)

getDictFastString :: Dictionary -> BinHandle -> IO FastString
getDictFastString dict bh = do
    j <- get bh
    return $! (dict ! fromIntegral (j :: Word32))

data BinDictionary = BinDictionary {
        bin_dict_next :: !FastMutInt, -- The next index to use
        bin_dict_map  :: !(IORef (UniqFM (Int,FastString)))
                                -- indexed by FastString
  }

-- -----------------------------------------------------------------------------
-- All the binary instances

-- BasicTypes
{-! for Fixity derive: Binary !-}
{-! for FixityDirection derive: Binary !-}
{-! for Boxity derive: Binary !-}
{-! for StrictnessMark derive: Binary !-}
{-! for Activation derive: Binary !-}

-- Demand
{-! for Demand derive: Binary !-}
{-! for Demands derive: Binary !-}
{-! for DmdResult derive: Binary !-}
{-! for StrictSig derive: Binary !-}

-- Class
{-! for DefMeth derive: Binary !-}

-- HsTypes
{-! for HsPred derive: Binary !-}
{-! for HsType derive: Binary !-}
{-! for TupCon derive: Binary !-}
{-! for HsTyVarBndr derive: Binary !-}

-- HsCore
{-! for UfExpr derive: Binary !-}
{-! for UfConAlt derive: Binary !-}
{-! for UfBinding derive: Binary !-}
{-! for UfBinder derive: Binary !-}
{-! for HsIdInfo derive: Binary !-}
{-! for UfNote derive: Binary !-}

-- HsDecls
{-! for ConDetails derive: Binary !-}
{-! for BangType derive: Binary !-}

-- CostCentre
{-! for IsCafCC derive: Binary !-}
{-! for CostCentre derive: Binary !-}



-- ---------------------------------------------------------------------------
-- Reading a binary interface into ParsedIface

instance Binary ModIface where
   put_ bh (ModIface {
                 mi_module    = mod,
                 mi_boot      = is_boot,
                 mi_iface_hash= iface_hash,
                 mi_mod_hash  = mod_hash,
                 mi_flag_hash = flag_hash,
                 mi_orphan    = orphan,
                 mi_finsts    = hasFamInsts,
                 mi_deps      = deps,
                 mi_usages    = usages,
                 mi_exports   = exports,
                 mi_exp_hash  = exp_hash,
                 mi_used_th   = used_th,
                 mi_fixities  = fixities,
                 mi_warns     = warns,
                 mi_anns      = anns,
                 mi_decls     = decls,
                 mi_insts     = insts,
                 mi_fam_insts = fam_insts,
                 mi_rules     = rules,
                 mi_orphan_hash = orphan_hash,
                 mi_vect_info = vect_info,
                 mi_hpc       = hpc_info,
                 mi_trust     = trust,
                 mi_trust_pkg = trust_pkg }) = do
        put_ bh mod
        put_ bh is_boot
        put_ bh iface_hash
        put_ bh mod_hash
        put_ bh flag_hash
        put_ bh orphan
        put_ bh hasFamInsts
        lazyPut bh deps
        lazyPut bh usages
        put_ bh exports
        put_ bh exp_hash
        put_ bh used_th
        put_ bh fixities
        lazyPut bh warns
        lazyPut bh anns
        put_ bh decls
        put_ bh insts
        put_ bh fam_insts
        lazyPut bh rules
        put_ bh orphan_hash
        put_ bh vect_info
        put_ bh hpc_info
        put_ bh trust
        put_ bh trust_pkg

   get bh = do
        mod_name    <- get bh
        is_boot     <- get bh
        iface_hash  <- get bh
        mod_hash    <- get bh
        flag_hash   <- get bh
        orphan      <- get bh
        hasFamInsts <- get bh
        deps        <- lazyGet bh
        usages      <- {-# SCC "bin_usages" #-} lazyGet bh
        exports     <- {-# SCC "bin_exports" #-} get bh
        exp_hash    <- get bh
        used_th     <- get bh
        fixities    <- {-# SCC "bin_fixities" #-} get bh
        warns       <- {-# SCC "bin_warns" #-} lazyGet bh
        anns        <- {-# SCC "bin_anns" #-} lazyGet bh
        decls       <- {-# SCC "bin_tycldecls" #-} get bh
        insts       <- {-# SCC "bin_insts" #-} get bh
        fam_insts   <- {-# SCC "bin_fam_insts" #-} get bh
        rules       <- {-# SCC "bin_rules" #-} lazyGet bh
        orphan_hash <- get bh
        vect_info   <- get bh
        hpc_info    <- get bh
        trust       <- get bh
        trust_pkg   <- get bh
        return (ModIface {
                 mi_module      = mod_name,
                 mi_boot        = is_boot,
                 mi_iface_hash  = iface_hash,
                 mi_mod_hash    = mod_hash,
                 mi_flag_hash   = flag_hash,
                 mi_orphan      = orphan,
                 mi_finsts      = hasFamInsts,
                 mi_deps        = deps,
                 mi_usages      = usages,
                 mi_exports     = exports,
                 mi_exp_hash    = exp_hash,
                 mi_used_th     = used_th,
                 mi_anns        = anns,
                 mi_fixities    = fixities,
                 mi_warns       = warns,
                 mi_decls       = decls,
                 mi_globals     = Nothing,
                 mi_insts       = insts,
                 mi_fam_insts   = fam_insts,
                 mi_rules       = rules,
                 mi_orphan_hash = orphan_hash,
                 mi_vect_info   = vect_info,
                 mi_hpc         = hpc_info,
                 mi_trust       = trust,
                 mi_trust_pkg   = trust_pkg,
                        -- And build the cached values
                 mi_warn_fn     = mkIfaceWarnCache warns,
                 mi_fix_fn      = mkIfaceFixCache fixities,
                 mi_hash_fn     = mkIfaceHashCache decls })

getWayDescr :: DynFlags -> String
getWayDescr dflags
  | platformUnregisterised (targetPlatform dflags) = 'u':tag
  | otherwise                                      =     tag
  where tag = buildTag dflags
        -- if this is an unregisterised build, make sure our interfaces
        -- can't be used by a registerised build.

-------------------------------------------------------------------------
--              Types from: HscTypes
-------------------------------------------------------------------------

instance Binary Dependencies where
    put_ bh deps = do put_ bh (dep_mods deps)
                      put_ bh (dep_pkgs deps)
                      put_ bh (dep_orphs deps)
                      put_ bh (dep_finsts deps)

    get bh = do ms <- get bh 
                ps <- get bh
                os <- get bh
                fis <- get bh
                return (Deps { dep_mods = ms, dep_pkgs = ps, dep_orphs = os,
                               dep_finsts = fis })

instance Binary AvailInfo where
    put_ bh (Avail aa) = do
            putByte bh 0
            put_ bh aa
    put_ bh (AvailTC ab ac) = do
            putByte bh 1
            put_ bh ab
            put_ bh ac
    get bh = do
            h <- getByte bh
            case h of
              0 -> do aa <- get bh
                      return (Avail aa)
              _ -> do ab <- get bh
                      ac <- get bh
                      return (AvailTC ab ac)

instance Binary Usage where
    put_ bh usg@UsagePackageModule{} = do 
        putByte bh 0
        put_ bh (usg_mod usg)
        put_ bh (usg_mod_hash usg)
        put_ bh (usg_safe     usg)

    put_ bh usg@UsageHomeModule{} = do 
        putByte bh 1
        put_ bh (usg_mod_name usg)
        put_ bh (usg_mod_hash usg)
        put_ bh (usg_exports  usg)
        put_ bh (usg_entities usg)
        put_ bh (usg_safe     usg)

    put_ bh usg@UsageFile{} = do 
        putByte bh 2
        put_ bh (usg_file_path usg)
        put_ bh (usg_mtime     usg)

    get bh = do
        h <- getByte bh
        case h of
          0 -> do
            nm    <- get bh
            mod   <- get bh
            safe  <- get bh
            return UsagePackageModule { usg_mod = nm, usg_mod_hash = mod, usg_safe = safe }
          1 -> do
            nm    <- get bh
            mod   <- get bh
            exps  <- get bh
            ents  <- get bh
            safe  <- get bh
            return UsageHomeModule { usg_mod_name = nm, usg_mod_hash = mod,
                     usg_exports = exps, usg_entities = ents, usg_safe = safe }
          2 -> do
            fp    <- get bh
            mtime <- get bh
            return UsageFile { usg_file_path = fp, usg_mtime = mtime }
          i -> error ("Binary.get(Usage): " ++ show i)

instance Binary Warnings where
    put_ bh NoWarnings     = putByte bh 0
    put_ bh (WarnAll t) = do
            putByte bh 1
            put_ bh t
    put_ bh (WarnSome ts) = do
            putByte bh 2
            put_ bh ts

    get bh = do
            h <- getByte bh
            case h of
              0 -> return NoWarnings
              1 -> do aa <- get bh
                      return (WarnAll aa)
              _ -> do aa <- get bh
                      return (WarnSome aa)

instance Binary WarningTxt where
    put_ bh (WarningTxt w) = do
            putByte bh 0
            put_ bh w
    put_ bh (DeprecatedTxt d) = do
            putByte bh 1
            put_ bh d

    get bh = do
            h <- getByte bh
            case h of
              0 -> do w <- get bh
                      return (WarningTxt w)
              _ -> do d <- get bh
                      return (DeprecatedTxt d)

-------------------------------------------------------------------------
--              Types from: BasicTypes
-------------------------------------------------------------------------

instance Binary Activation where
    put_ bh NeverActive = do
            putByte bh 0
    put_ bh AlwaysActive = do
            putByte bh 1
    put_ bh (ActiveBefore aa) = do
            putByte bh 2
            put_ bh aa
    put_ bh (ActiveAfter ab) = do
            putByte bh 3
            put_ bh ab
    get bh = do
            h <- getByte bh
            case h of
              0 -> do return NeverActive
              1 -> do return AlwaysActive
              2 -> do aa <- get bh
                      return (ActiveBefore aa)
              _ -> do ab <- get bh
                      return (ActiveAfter ab)

instance Binary RuleMatchInfo where
    put_ bh FunLike = putByte bh 0
    put_ bh ConLike = putByte bh 1
    get bh = do
            h <- getByte bh
            if h == 1 then return ConLike
                      else return FunLike

instance Binary InlinePragma where
    put_ bh (InlinePragma a b c d) = do
            put_ bh a
            put_ bh b
            put_ bh c
            put_ bh d

    get bh = do
           a <- get bh
           b <- get bh
           c <- get bh
           d <- get bh
           return (InlinePragma a b c d)

instance Binary InlineSpec where
    put_ bh EmptyInlineSpec = putByte bh 0
    put_ bh Inline          = putByte bh 1
    put_ bh Inlinable       = putByte bh 2
    put_ bh NoInline        = putByte bh 3

    get bh = do h <- getByte bh
                case h of
                  0 -> return EmptyInlineSpec
                  1 -> return Inline
                  2 -> return Inlinable
                  _ -> return NoInline

instance Binary IfaceBang where
    put_ bh IfNoBang        = putByte bh 0
    put_ bh IfStrict        = putByte bh 1
    put_ bh IfUnpack        = putByte bh 2
    put_ bh (IfUnpackCo co) = putByte bh 3 >> put_ bh co

    get bh = do
            h <- getByte bh
            case h of
              0 -> do return IfNoBang
              1 -> do return IfStrict
              2 -> do return IfUnpack
              _ -> do { a <- get bh; return (IfUnpackCo a) }

instance Binary TupleSort where
    put_ bh BoxedTuple      = putByte bh 0
    put_ bh UnboxedTuple    = putByte bh 1
    put_ bh ConstraintTuple = putByte bh 2
    get bh = do
      h <- getByte bh
      case h of
        0 -> do return BoxedTuple
        1 -> do return UnboxedTuple
        _ -> do return ConstraintTuple

instance Binary RecFlag where
    put_ bh Recursive = do
            putByte bh 0
    put_ bh NonRecursive = do
            putByte bh 1
    get bh = do
            h <- getByte bh
            case h of
              0 -> do return Recursive
              _ -> do return NonRecursive

instance Binary DefMethSpec where
    put_ bh NoDM      = putByte bh 0
    put_ bh VanillaDM = putByte bh 1
    put_ bh GenericDM = putByte bh 2
    get bh = do
            h <- getByte bh
            case h of
              0 -> return NoDM
              1 -> return VanillaDM
              _ -> return GenericDM

instance Binary FixityDirection where
    put_ bh InfixL = do
            putByte bh 0
    put_ bh InfixR = do
            putByte bh 1
    put_ bh InfixN = do
            putByte bh 2
    get bh = do
            h <- getByte bh
            case h of
              0 -> do return InfixL
              1 -> do return InfixR
              _ -> do return InfixN

instance Binary Fixity where
    put_ bh (Fixity aa ab) = do
            put_ bh aa
            put_ bh ab
    get bh = do
          aa <- get bh
          ab <- get bh
          return (Fixity aa ab)

-------------------------------------------------------------------------
--              Types from: Demand
-------------------------------------------------------------------------

instance Binary DmdType where
        -- Ignore DmdEnv when spitting out the DmdType
  put bh (DmdType _ ds dr) = do p <- put bh ds; put_ bh dr; return (castBin p)
  get bh = do ds <- get bh; dr <- get bh; return (DmdType emptyVarEnv ds dr)

instance Binary Demand where
    put_ bh Top = do
            putByte bh 0
    put_ bh Abs = do
            putByte bh 1
    put_ bh (Call aa) = do
            putByte bh 2
            put_ bh aa
    put_ bh (Eval ab) = do
            putByte bh 3
            put_ bh ab
    put_ bh (Defer ac) = do
            putByte bh 4
            put_ bh ac
    put_ bh (Box ad) = do
            putByte bh 5
            put_ bh ad
    put_ bh Bot = do
            putByte bh 6
    get bh = do
            h <- getByte bh
            case h of
              0 -> do return Top
              1 -> do return Abs
              2 -> do aa <- get bh
                      return (Call aa)
              3 -> do ab <- get bh
                      return (Eval ab)
              4 -> do ac <- get bh
                      return (Defer ac)
              5 -> do ad <- get bh
                      return (Box ad)
              _ -> do return Bot

instance Binary Demands where
    put_ bh (Poly aa) = do
            putByte bh 0
            put_ bh aa
    put_ bh (Prod ab) = do
            putByte bh 1
            put_ bh ab
    get bh = do
            h <- getByte bh
            case h of
              0 -> do aa <- get bh
                      return (Poly aa)
              _ -> do ab <- get bh
                      return (Prod ab)

instance Binary DmdResult where
    put_ bh TopRes = do
            putByte bh 0
    put_ bh RetCPR = do
            putByte bh 1
    put_ bh BotRes = do
            putByte bh 2
    get bh = do
            h <- getByte bh
            case h of
              0 -> do return TopRes
              1 -> do return RetCPR     -- Really use RetCPR even if -fcpr-off
                                        -- The wrapper was generated for CPR in 
                                        -- the imported module!
              _ -> do return BotRes

instance Binary StrictSig where
    put_ bh (StrictSig aa) = do
            put_ bh aa
    get bh = do
          aa <- get bh
          return (StrictSig aa)


-------------------------------------------------------------------------
--              Types from: CostCentre
-------------------------------------------------------------------------

instance Binary IsCafCC where
    put_ bh CafCC = do
            putByte bh 0
    put_ bh NotCafCC = do
            putByte bh 1
    get bh = do
            h <- getByte bh
            case h of
              0 -> do return CafCC
              _ -> do return NotCafCC

instance Binary CostCentre where
    put_ bh (NormalCC aa ab ac _ad ae) = do
            putByte bh 0
            put_ bh aa
            put_ bh ab
            put_ bh ac
            put_ bh ae
    put_ bh (AllCafsCC ae _af) = do
            putByte bh 1
            put_ bh ae
    get bh = do
            h <- getByte bh
            case h of
              0 -> do aa <- get bh
                      ab <- get bh
                      ac <- get bh
                      ae <- get bh
                      return (NormalCC aa ab ac noSrcSpan ae)
              _ -> do ae <- get bh
                      return (AllCafsCC ae noSrcSpan)

    -- We ignore the SrcSpans in CostCentres when we serialise them,
    -- and set the SrcSpans to noSrcSpan when deserialising.  This is
    -- ok, because we only need the SrcSpan when declaring the
    -- CostCentre in the original module, it is not used by importing
    -- modules.

-------------------------------------------------------------------------
--              IfaceTypes and friends
-------------------------------------------------------------------------

instance Binary IfaceBndr where
    put_ bh (IfaceIdBndr aa) = do
            putByte bh 0
            put_ bh aa
    put_ bh (IfaceTvBndr ab) = do
            putByte bh 1
            put_ bh ab
    get bh = do
            h <- getByte bh
            case h of
              0 -> do aa <- get bh
                      return (IfaceIdBndr aa)
              _ -> do ab <- get bh
                      return (IfaceTvBndr ab)

instance Binary IfaceLetBndr where
    put_ bh (IfLetBndr a b c) = do
            put_ bh a
            put_ bh b
            put_ bh c
    get bh = do a <- get bh
                b <- get bh
                c <- get bh
                return (IfLetBndr a b c)           

instance Binary IfaceType where
    put_ bh (IfaceForAllTy aa ab) = do
            putByte bh 0
            put_ bh aa
            put_ bh ab
    put_ bh (IfaceTyVar ad) = do
            putByte bh 1
            put_ bh ad
    put_ bh (IfaceAppTy ae af) = do
            putByte bh 2
            put_ bh ae
            put_ bh af
    put_ bh (IfaceFunTy ag ah) = do
            putByte bh 3
            put_ bh ag
            put_ bh ah
    put_ bh (IfaceCoConApp cc tys)
      = do { putByte bh 4; put_ bh cc; put_ bh tys }
    put_ bh (IfaceTyConApp tc tys)
      = do { putByte bh 5; put_ bh tc; put_ bh tys }

    put_ bh (IfaceLitTy n)
      = do { putByte bh 30; put_ bh n }


    get bh = do
            h <- getByte bh
            case h of
              0 -> do aa <- get bh
                      ab <- get bh
                      return (IfaceForAllTy aa ab)
              1 -> do ad <- get bh
                      return (IfaceTyVar ad)
              2 -> do ae <- get bh
                      af <- get bh
                      return (IfaceAppTy ae af)
              3 -> do ag <- get bh
                      ah <- get bh
                      return (IfaceFunTy ag ah)
              4 -> do { cc <- get bh; tys <- get bh
                      ; return (IfaceCoConApp cc tys) }
              5 -> do { tc <- get bh; tys <- get bh
                      ; return (IfaceTyConApp tc tys) }

              30 -> do n <- get bh
                       return (IfaceLitTy n)

              _  -> panic ("get IfaceType " ++ show h)

instance Binary IfaceTyLit where
  put_ bh (IfaceNumTyLit n)  = putByte bh 1 >> put_ bh n
  put_ bh (IfaceStrTyLit n)  = putByte bh 2 >> put_ bh n

  get bh =
    do tag <- getByte bh
       case tag of
         1 -> do { n <- get bh
                 ; return (IfaceNumTyLit n) }
         2 -> do { n <- get bh
                 ; return (IfaceStrTyLit n) }
         _ -> panic ("get IfaceTyLit " ++ show tag)

instance Binary IfaceTyCon where
   put_ bh (IfaceTc ext) = put_ bh ext
   get bh = liftM IfaceTc (get bh)

instance Binary LeftOrRight where
   put_ bh CLeft  = putByte bh 0
   put_ bh CRight = putByte bh 1

   get bh = do { h <- getByte bh
               ; case h of
                   0 -> return CLeft
                   _ -> return CRight }

instance Binary IfaceCoCon where
   put_ bh (IfaceCoAx n ind)   = do { putByte bh 0; put_ bh n; put_ bh ind }
   put_ bh IfaceReflCo         = putByte bh 1
   put_ bh IfaceUnsafeCo       = putByte bh 2
   put_ bh IfaceSymCo          = putByte bh 3
   put_ bh IfaceTransCo        = putByte bh 4
   put_ bh IfaceInstCo         = putByte bh 5
   put_ bh (IfaceNthCo d)      = do { putByte bh 6; put_ bh d }
   put_ bh (IfaceLRCo lr)      = do { putByte bh 7; put_ bh lr }

   get bh = do
        h <- getByte bh
        case h of
          0 -> do { n <- get bh; ind <- get bh; return (IfaceCoAx n ind) }
          1 -> return IfaceReflCo 
          2 -> return IfaceUnsafeCo
          3 -> return IfaceSymCo
          4 -> return IfaceTransCo
          5 -> return IfaceInstCo
          6 -> do { d <- get bh; return (IfaceNthCo d) }
          7 -> do { lr <- get bh; return (IfaceLRCo lr) }
          _ -> panic ("get IfaceCoCon " ++ show h)

-------------------------------------------------------------------------
--              IfaceExpr and friends
-------------------------------------------------------------------------

instance Binary IfaceExpr where
    put_ bh (IfaceLcl aa) = do
        putByte bh 0
        put_ bh aa
    put_ bh (IfaceType ab) = do
        putByte bh 1
        put_ bh ab
    put_ bh (IfaceCo ab) = do
        putByte bh 2
        put_ bh ab
    put_ bh (IfaceTuple ac ad) = do
        putByte bh 3
        put_ bh ac
        put_ bh ad
    put_ bh (IfaceLam ae af) = do
        putByte bh 4
        put_ bh ae
        put_ bh af
    put_ bh (IfaceApp ag ah) = do
        putByte bh 5
        put_ bh ag
        put_ bh ah
    put_ bh (IfaceCase ai aj ak) = do
        putByte bh 6
        put_ bh ai
        put_ bh aj
        put_ bh ak
    put_ bh (IfaceLet al am) = do
        putByte bh 7
        put_ bh al
        put_ bh am
    put_ bh (IfaceTick an ao) = do
        putByte bh 8
        put_ bh an
        put_ bh ao
    put_ bh (IfaceLit ap) = do
        putByte bh 9
        put_ bh ap
    put_ bh (IfaceFCall as at) = do
        putByte bh 10
        put_ bh as
        put_ bh at
    put_ bh (IfaceExt aa) = do
        putByte bh 11
        put_ bh aa
    put_ bh (IfaceCast ie ico) = do
        putByte bh 12
        put_ bh ie
        put_ bh ico
    put_ bh (IfaceECase a b) = do
        putByte bh 13
        put_ bh a
        put_ bh b
    get bh = do
        h <- getByte bh
        case h of
            0 -> do aa <- get bh
                    return (IfaceLcl aa)
            1 -> do ab <- get bh
                    return (IfaceType ab)
            2 -> do ab <- get bh
                    return (IfaceCo ab)
            3 -> do ac <- get bh
                    ad <- get bh
                    return (IfaceTuple ac ad)
            4 -> do ae <- get bh
                    af <- get bh
                    return (IfaceLam ae af)
            5 -> do ag <- get bh
                    ah <- get bh
                    return (IfaceApp ag ah)
            6 -> do ai <- get bh
                    aj <- get bh
                    ak <- get bh
                    return (IfaceCase ai aj ak)
            7 -> do al <- get bh
                    am <- get bh
                    return (IfaceLet al am)
            8 -> do an <- get bh
                    ao <- get bh
                    return (IfaceTick an ao)
            9 -> do ap <- get bh
                    return (IfaceLit ap)
            10 -> do as <- get bh
                     at <- get bh
                     return (IfaceFCall as at)
            11 -> do aa <- get bh
                     return (IfaceExt aa)
            12 -> do ie <- get bh
                     ico <- get bh
                     return (IfaceCast ie ico)
            13 -> do a <- get bh
                     b <- get bh
                     return (IfaceECase a b)
            _ -> panic ("get IfaceExpr " ++ show h)

instance Binary IfaceConAlt where
    put_ bh IfaceDefault      = putByte bh 0
    put_ bh (IfaceDataAlt aa) = putByte bh 1 >> put_ bh aa
    put_ bh (IfaceLitAlt ac)  = putByte bh 2 >> put_ bh ac
    get bh = do
        h <- getByte bh
        case h of
            0 -> return IfaceDefault
            1 -> get bh >>= (return . IfaceDataAlt)
            _ -> get bh >>= (return . IfaceLitAlt)

instance Binary IfaceBinding where
    put_ bh (IfaceNonRec aa ab) = putByte bh 0 >> put_ bh aa >> put_ bh ab
    put_ bh (IfaceRec ac)       = putByte bh 1 >> put_ bh ac
    get bh = do
        h <- getByte bh
        case h of
            0 -> do { aa <- get bh; ab <- get bh; return (IfaceNonRec aa ab) }
            _ -> do { ac <- get bh; return (IfaceRec ac) }

instance Binary IfaceIdDetails where
    put_ bh IfVanillaId      = putByte bh 0
    put_ bh (IfRecSelId a b) = putByte bh 1 >> put_ bh a >> put_ bh b
    put_ bh (IfDFunId n)     = do { putByte bh 2; put_ bh n }
    get bh = do
        h <- getByte bh
        case h of
            0 -> return IfVanillaId
            1 -> do { a <- get bh; b <- get bh; return (IfRecSelId a b) }
            _ -> do { n <- get bh; return (IfDFunId n) }

instance Binary (DFunArg IfaceExpr) where
    put_ bh (DFunPolyArg  e) = putByte bh 0 >> put_ bh e
    put_ bh (DFunLamArg i)   = putByte bh 1 >> put_ bh i
    get bh = do { h <- getByte bh
                ; case h of
                    0 -> do { a <- get bh; return (DFunPolyArg a) }
                    _ -> do { a <- get bh; return (DFunLamArg a) } }

instance Binary IfaceIdInfo where
    put_ bh NoInfo      = putByte bh 0
    put_ bh (HasInfo i) = putByte bh 1 >> lazyPut bh i -- NB lazyPut

    get bh = do
        h <- getByte bh
        case h of
            0 -> return NoInfo
            _ -> lazyGet bh >>= (return . HasInfo)     -- NB lazyGet

instance Binary IfaceInfoItem where
    put_ bh (HsArity aa)      = putByte bh 0 >> put_ bh aa
    put_ bh (HsStrictness ab) = putByte bh 1 >> put_ bh ab
    put_ bh (HsUnfold lb ad)  = putByte bh 2 >> put_ bh lb >> put_ bh ad
    put_ bh (HsInline ad)     = putByte bh 3 >> put_ bh ad
    put_ bh HsNoCafRefs       = putByte bh 4
    get bh = do
        h <- getByte bh
        case h of
            0 -> get bh >>= (return . HsArity)
            1 -> get bh >>= (return . HsStrictness)
            2 -> do lb <- get bh
                    ad <- get bh
                    return (HsUnfold lb ad)
            3 -> get bh >>= (return . HsInline)
            _ -> return HsNoCafRefs

instance Binary IfaceUnfolding where
    put_ bh (IfCoreUnfold s e) = do
        putByte bh 0
        put_ bh s
        put_ bh e
    put_ bh (IfInlineRule a b c d) = do
        putByte bh 1
        put_ bh a
        put_ bh b
        put_ bh c
        put_ bh d
    put_ bh (IfLclWrapper a n) = do
        putByte bh 2
        put_ bh a
        put_ bh n
    put_ bh (IfExtWrapper a n) = do
        putByte bh 3
        put_ bh a
        put_ bh n
    put_ bh (IfDFunUnfold as) = do
        putByte bh 4
        put_ bh as
    put_ bh (IfCompulsory e) = do
        putByte bh 5
        put_ bh e
    get bh = do
        h <- getByte bh
        case h of
            0 -> do s <- get bh
                    e <- get bh
                    return (IfCoreUnfold s e)
            1 -> do a <- get bh
                    b <- get bh
                    c <- get bh
                    d <- get bh
                    return (IfInlineRule a b c d)
            2 -> do a <- get bh
                    n <- get bh
                    return (IfLclWrapper a n)
            3 -> do a <- get bh
                    n <- get bh
                    return (IfExtWrapper a n)
            4 -> do as <- get bh
                    return (IfDFunUnfold as)
            _ -> do e <- get bh
                    return (IfCompulsory e)

instance Binary IfaceTickish where
    put_ bh (IfaceHpcTick m ix) = do
        putByte bh 0
        put_ bh m
        put_ bh ix
    put_ bh (IfaceSCC cc tick push) = do
        putByte bh 1
        put_ bh cc
        put_ bh tick
        put_ bh push

    get bh = do
        h <- getByte bh
        case h of
            0 -> do m <- get bh
                    ix <- get bh
                    return (IfaceHpcTick m ix)
            1 -> do cc <- get bh
                    tick <- get bh
                    push <- get bh
                    return (IfaceSCC cc tick push)
            _ -> panic ("get IfaceTickish " ++ show h)

-------------------------------------------------------------------------
--              IfaceDecl and friends
-------------------------------------------------------------------------

-- A bit of magic going on here: there's no need to store the OccName
-- for a decl on the disk, since we can infer the namespace from the
-- context; however it is useful to have the OccName in the IfaceDecl
-- to avoid re-building it in various places.  So we build the OccName
-- when de-serialising.

instance Binary IfaceDecl where
    put_ bh (IfaceId name ty details idinfo) = do
        putByte bh 0
        put_ bh (occNameFS name)
        put_ bh ty
        put_ bh details
        put_ bh idinfo

    put_ _ (IfaceForeign _ _) = 
        error "Binary.put_(IfaceDecl): IfaceForeign"

    put_ bh (IfaceData a1 a2 a3 a4 a5 a6 a7 a8 a9) = do
        putByte bh 2
        put_ bh (occNameFS a1)
        put_ bh a2
        put_ bh a3
        put_ bh a4
        put_ bh a5
        put_ bh a6
        put_ bh a7
        put_ bh a8
        put_ bh a9

    put_ bh (IfaceSyn a1 a2 a3 a4) = do
        putByte bh 3
        put_ bh (occNameFS a1)
        put_ bh a2
        put_ bh a3
        put_ bh a4

    put_ bh (IfaceClass a1 a2 a3 a4 a5 a6 a7) = do
        putByte bh 4
        put_ bh a1
        put_ bh (occNameFS a2)
        put_ bh a3
        put_ bh a4
        put_ bh a5
        put_ bh a6
        put_ bh a7
        
    put_ bh (IfaceAxiom a1 a2 a3) = do
        putByte bh 5
        put_ bh (occNameFS a1)
        put_ bh a2
        put_ bh a3

    get bh = do
        h <- getByte bh
        case h of
            0 -> do name    <- get bh
                    ty      <- get bh
                    details <- get bh
                    idinfo  <- get bh
                    occ <- return $! mkOccNameFS varName name
                    return (IfaceId occ ty details idinfo)
            1 -> error "Binary.get(TyClDecl): ForeignType"
            2 -> do a1 <- get bh
                    a2 <- get bh
                    a3 <- get bh
                    a4 <- get bh
                    a5 <- get bh
                    a6 <- get bh
                    a7 <- get bh
                    a8 <- get bh
                    a9 <- get bh
                    occ <- return $! mkOccNameFS tcName a1
                    return (IfaceData occ a2 a3 a4 a5 a6 a7 a8 a9)
            3 -> do a1 <- get bh
                    a2 <- get bh
                    a3 <- get bh
                    a4 <- get bh
                    occ <- return $! mkOccNameFS tcName a1
                    return (IfaceSyn occ a2 a3 a4)
            4 -> do a1 <- get bh
                    a2 <- get bh
                    a3 <- get bh
                    a4 <- get bh
                    a5 <- get bh
                    a6 <- get bh
                    a7 <- get bh
                    occ <- return $! mkOccNameFS clsName a2
                    return (IfaceClass a1 occ a3 a4 a5 a6 a7)
            _ -> do a1 <- get bh
                    a2 <- get bh
                    a3 <- get bh
                    occ <- return $! mkOccNameFS tcName a1
                    return (IfaceAxiom occ a2 a3)

instance Binary IfaceAxBranch where
    put_ bh (IfaceAxBranch a1 a2 a3) = do
        put_ bh a1
        put_ bh a2
        put_ bh a3
    get bh = do
        a1 <- get bh
        a2 <- get bh
        a3 <- get bh
        return (IfaceAxBranch a1 a2 a3)

instance Binary ty => Binary (SynTyConRhs ty) where
    put_ bh (SynFamilyTyCon a b) = putByte bh 0 >> put_ bh a >> put_ bh b
    put_ bh (SynonymTyCon ty)    = putByte bh 1 >> put_ bh ty

    get bh = do { h <- getByte bh
                ; case h of
                    0 -> do { a <- get bh
                            ; b <- get bh
                            ; return (SynFamilyTyCon a b) }
                    _ -> do { ty <- get bh
                            ; return (SynonymTyCon ty) } }

instance Binary IfaceClsInst where
    put_ bh (IfaceClsInst cls tys dfun flag orph) = do
        put_ bh cls
        put_ bh tys
        put_ bh dfun
        put_ bh flag
        put_ bh orph
    get bh = do
        cls  <- get bh
        tys  <- get bh
        dfun <- get bh
        flag <- get bh
        orph <- get bh
        return (IfaceClsInst cls tys dfun flag orph)

instance Binary IfaceFamInst where
    put_ bh (IfaceFamInst fam group tys name orph) = do
        put_ bh fam
        put_ bh group
        put_ bh tys
        put_ bh name
        put_ bh orph
    get bh = do
        fam      <- get bh
        group    <- get bh
        tys      <- get bh
        name     <- get bh
        orph     <- get bh
        return (IfaceFamInst fam group tys name orph)

instance Binary OverlapFlag where
    put_ bh (NoOverlap  b) = putByte bh 0 >> put_ bh b
    put_ bh (OverlapOk  b) = putByte bh 1 >> put_ bh b
    put_ bh (Incoherent b) = putByte bh 2 >> put_ bh b
    get bh = do
        h <- getByte bh
        b <- get bh
        case h of
            0 -> return $ NoOverlap b
            1 -> return $ OverlapOk b
            2 -> return $ Incoherent b
            _ -> panic ("get OverlapFlag " ++ show h)

instance Binary IfaceConDecls where
    put_ bh (IfAbstractTyCon d) = putByte bh 0 >> put_ bh d
    put_ bh IfDataFamTyCon     = putByte bh 1
    put_ bh (IfDataTyCon cs)    = putByte bh 2 >> put_ bh cs
    put_ bh (IfNewTyCon c)      = putByte bh 3 >> put_ bh c
    get bh = do
        h <- getByte bh
        case h of
            0 -> get bh >>= (return . IfAbstractTyCon)
            1 -> return IfDataFamTyCon
            2 -> get bh >>= (return . IfDataTyCon)
            _ -> get bh >>= (return . IfNewTyCon)

instance Binary IfaceConDecl where
    put_ bh (IfCon a1 a2 a3 a4 a5 a6 a7 a8 a9 a10) = do
        put_ bh a1
        put_ bh a2
        put_ bh a3
        put_ bh a4
        put_ bh a5
        put_ bh a6
        put_ bh a7
        put_ bh a8
        put_ bh a9
        put_ bh a10
    get bh = do
        a1 <- get bh
        a2 <- get bh
        a3 <- get bh          
        a4 <- get bh
        a5 <- get bh
        a6 <- get bh
        a7 <- get bh
        a8 <- get bh
        a9 <- get bh
        a10 <- get bh
        return (IfCon a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)

instance Binary IfaceAT where
    put_ bh (IfaceAT dec defs) = do
        put_ bh dec
        put_ bh defs
    get bh = do
        dec  <- get bh
        defs <- get bh
        return (IfaceAT dec defs)

instance Binary IfaceATDefault where
    put_ bh (IfaceATD tvs pat_tys ty) = do
        put_ bh tvs
        put_ bh pat_tys
        put_ bh ty
    get bh = liftM3 IfaceATD (get bh) (get bh) (get bh)

instance Binary IfaceClassOp where
    put_ bh (IfaceClassOp n def ty) = do 
        put_ bh (occNameFS n)
        put_ bh def     
        put_ bh ty
    get bh = do
        n   <- get bh
        def <- get bh
        ty  <- get bh
        occ <- return $! mkOccNameFS varName n
        return (IfaceClassOp occ def ty)

instance Binary IfaceRule where
    put_ bh (IfaceRule a1 a2 a3 a4 a5 a6 a7 a8) = do
        put_ bh a1
        put_ bh a2
        put_ bh a3
        put_ bh a4
        put_ bh a5
        put_ bh a6
        put_ bh a7
        put_ bh a8
    get bh = do
        a1 <- get bh
        a2 <- get bh
        a3 <- get bh
        a4 <- get bh
        a5 <- get bh
        a6 <- get bh
        a7 <- get bh
        a8 <- get bh
        return (IfaceRule a1 a2 a3 a4 a5 a6 a7 a8)

instance Binary IfaceAnnotation where
    put_ bh (IfaceAnnotation a1 a2) = do
        put_ bh a1
        put_ bh a2
    get bh = do
        a1 <- get bh
        a2 <- get bh
        return (IfaceAnnotation a1 a2)

instance Binary name => Binary (AnnTarget name) where
    put_ bh (NamedTarget a) = do
        putByte bh 0
        put_ bh a
    put_ bh (ModuleTarget a) = do
        putByte bh 1
        put_ bh a
    get bh = do
        h <- getByte bh
        case h of
            0 -> get bh >>= (return . NamedTarget)
            _ -> get bh >>= (return . ModuleTarget)

instance Binary IfaceVectInfo where
    put_ bh (IfaceVectInfo a1 a2 a3 a4 a5) = do
        put_ bh a1
        put_ bh a2
        put_ bh a3
        put_ bh a4
        put_ bh a5
    get bh = do
        a1 <- get bh
        a2 <- get bh
        a3 <- get bh
        a4 <- get bh
        a5 <- get bh
        return (IfaceVectInfo a1 a2 a3 a4 a5)

instance Binary IfaceTrustInfo where
    put_ bh iftrust = putByte bh $ trustInfoToNum iftrust
    get bh = getByte bh >>= (return . numToTrustInfo)

