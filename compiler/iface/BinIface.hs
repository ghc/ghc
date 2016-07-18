{-# LANGUAGE BinaryLiterals, CPP, ScopedTypeVariables #-}

--
--  (c) The University of Glasgow 2002-2006
--

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
import ConLike
import PrelInfo   ( knownKeyNames )
import Id         ( idName, isDataConWorkId_maybe )
import TysWiredIn
import IfaceEnv
import HscTypes
import BasicTypes
import Module
import Name
import DynFlags
import UniqFM
import UniqSupply
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
import DataCon

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
                      TraceBinIFaceReading -> \sd ->
                          log_action dflags
                                     dflags
                                     NoReason
                                     SevOutput
                                     noSrcSpan
                                     defaultDumpStyle
                                     sd
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
            when (wanted /= got) $ throwGhcExceptionIO $ ProgramError
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
    let names = elems (array (0,next_off-1) (nonDetEltsUFM symtab))
      -- It's OK to use nonDetEltsUFM here because the elements have
      -- indices that array uses to create order
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

type OnDiskName = (UnitId, ModuleName, OccName)

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
    put_ bh (moduleUnitId mod, moduleName mod, nameOccName name)


-- Note [Symbol table representation of names]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- An occurrence of a name in an interface file is serialized as a single 32-bit word.
-- The format of this word is:
--  00xxxxxx xxxxxxxx xxxxxxxx xxxxxxxx
--   A normal name. x is an index into the symbol table
--  01xxxxxx xxyyyyyy yyyyyyyy yyyyyyyyyy
--   A known-key name. x is the Unique's Char, y is the int part
--  100xxyyz zzzzzzzz zzzzzzzz zzzzzzzz
--   A tuple name:
--    x is the tuple sort (00b ==> boxed, 01b ==> unboxed, 10b ==> constraint)
--    y is the thing (00b ==> tycon, 01b ==> datacon, 10b ==> datacon worker)
--    z is the arity
--
--  10100xxx xxxxxxxx xxxxxxxx xxxxxxxx
--   A sum tycon name:
--    x is the arity
--  10101xxx xxxxxxxx xxyyyyyy yyyyyyyy
--   A sum datacon name:
--    x is the arity
--    y is the alternative
--  10110xxx xxxxxxxx xxyyyyyy yyyyyyyy
--    worker
--  11xxxxxx xxxxxxxx xxxxxxxx xxxxxxxx
--   An implicit parameter TyCon name. x is an index into the FastString *dictionary*
--
-- Note that we have to have special representation for tuples, sums, and IP
-- TyCons because they form an "infinite" family and hence are not recorded
-- explicitly in wiredInTyThings or basicKnownKeyNames.

knownKeyNamesMap :: UniqFM Name
knownKeyNamesMap = listToUFM_Directly [(nameUnique n, n) | n <- knownKeyNames]

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
       | Just sort <- tyConTuple_maybe tc -> putTupleName_ bh tc sort 0
       | isUnboxedSumTyCon tc -> putSumTyConName_ bh tc
     Just (AConLike (RealDataCon dc))
       | let tc = dataConTyCon dc
       , Just sort <- tyConTuple_maybe tc -> putTupleName_ bh tc sort 1
       | isUnboxedSumCon dc -> putSumDataConName_ bh dc
     Just (AnId x)
       | Just dc <- isDataConWorkId_maybe x
       , let tc = dataConTyCon dc
       , Just sort <- tyConTuple_maybe tc -> putTupleName_ bh tc sort 2
     Just (AnId x)
       | Just dc <- isDataConWorkId_maybe x
       , isUnboxedSumCon dc
       -> putSumWorkerId_ bh dc
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

putTupleName_ :: BinHandle -> TyCon -> TupleSort -> Word32 -> IO ()
putTupleName_ bh tc tup_sort thing_tag
  = ASSERT(arity < 2^(25 :: Int))
    put_ bh (0x80000000 .|. (sort_tag `shiftL` 27) .|. (thing_tag `shiftL` 25) .|. arity)
  where
    (sort_tag, arity) = case tup_sort of
      BoxedTuple      -> (0, fromIntegral (tyConArity tc))
      UnboxedTuple    -> (1, fromIntegral (tyConArity tc `div` 2))
        -- See Note [Unboxed tuple RuntimeRep vars] in TyCon
      ConstraintTuple -> pprPanic "putTupleName:ConstraintTuple" (ppr tc)

putSumTyConName_ :: BinHandle -> TyCon -> IO ()
putSumTyConName_ bh tc
  = ASSERT(arity < 2^(27 :: Int))
    put_ bh (0xA0000000 .|. arity)
  where
    arity = (fromIntegral (tyConArity tc) `div` 2) :: Word32

putSumDataConName_ :: BinHandle -> DataCon -> IO ()
putSumDataConName_ bh dc
  = ASSERT(arity < 2^(13 :: Int) && alt < 2^(14 :: Int))
    put_ bh (0xA8000000 .|. (arity `shiftL` 14) .|. alt)
  where
    tc       = dataConTyCon dc
    alt      = fromIntegral (dataConTag dc)
    arity    = (fromIntegral (tyConArity tc) `div` 2) :: Word32

putSumWorkerId_ :: BinHandle -> DataCon -> IO ()
putSumWorkerId_ bh dc
  = put_ bh (0xB0000000 .|. (arity `shiftL` 14) .|. alt)
  where
    tc       = dataConTyCon dc
    alt      = fromIntegral (dataConTag dc)
    arity    = (fromIntegral (tyConArity tc) `div` 2) :: Word32

-- See Note [Symbol table representation of names]
getSymtabName :: NameCacheUpdater
              -> Dictionary -> SymbolTable
              -> BinHandle -> IO Name
getSymtabName _ncu _dict symtab bh = do
    i :: Word32 <- get bh
    case i .&. 0xC0000000 of
      0x00000000 -> return $! symtab ! fromIntegral i

      0x40000000 ->
        let
          tag = chr (fromIntegral ((i .&. 0x3FC00000) `shiftR` 22))
          ix  = fromIntegral i .&. 0x003FFFFF
        in
          return $! case lookupUFM_Directly knownKeyNamesMap (mkUnique tag ix) of
                      Nothing -> pprPanic "getSymtabName:unknown known-key unique" (ppr i)
                      Just n  -> n

      0x80000000 ->
        case i .&. 0x20000000 of
          0x00000000 ->
            let
              dc = tupleDataCon sort arity
              sort = case (i .&. 0x18000000) `shiftR` 27 of
                       0 -> Boxed
                       1 -> Unboxed
                       _ -> pprPanic "getSymtabName:unknown tuple sort" (ppr i)
              arity = fromIntegral (i .&. 0x01FFFFFF)
            in
              return $! case ( (i .&. 0x06FFFFFF) `shiftR` 25 ) of
                0 -> tyConName (tupleTyCon sort arity)
                1 -> dataConName dc
                2 -> idName (dataConWorkId dc)
                _ -> pprPanic "getSymtabName:unknown tuple thing" (ppr i)

          0x20000000 ->
            return $! case ((i .&. 0x18000000) `shiftR` 27) of
              0 -> tyConName $ sumTyCon ( fromIntegral (i .&. 0x7ffffff) )
              1 -> let
                     alt =
                       -- first (least significant) 14 bits
                       fromIntegral (i .&. 0b11111111111111)
                     arity =
                       -- next 13 bits
                       fromIntegral ((i `shiftR` 14) .&. 0b1111111111111)
                   in
                     ASSERT( arity >= alt )
                     dataConName (sumDataCon alt arity)
              2 -> let
                     alt =
                       -- first (least significant) 14 bits
                       fromIntegral (i .&. 0b11111111111111)
                     arity =
                       -- next 13 bits
                       fromIntegral ((i `shiftR` 14) .&. 0b1111111111111)
                   in
                     ASSERT( arity >= alt )
                     idName (dataConWorkId (sumDataCon alt arity))

              _ -> pprPanic "getSymtabName:unknown sum sort" (ppr i)
          _ -> pprPanic "getSyntabName:unknown `tuple or sum` tag" (ppr i)
      _ -> pprPanic "getSymtabName:unknown name tag" (ppr i)

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

getWayDescr :: DynFlags -> String
getWayDescr dflags
  | platformUnregisterised (targetPlatform dflags) = 'u':tag
  | otherwise                                      =     tag
  where tag = buildTag dflags
        -- if this is an unregisterised build, make sure our interfaces
        -- can't be used by a registerised build.
