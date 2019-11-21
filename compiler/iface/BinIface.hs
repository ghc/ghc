{-# LANGUAGE BinaryLiterals, CPP, ScopedTypeVariables, BangPatterns #-}

--
--  (c) The University of Glasgow 2002-2006
--

{-# OPTIONS_GHC -O2 #-}
-- We always optimise this, otherwise performance of a non-optimised
-- compiler is severely affected

-- | Binary interface file support.
module BinIface (
        writeBinIface,
        readBinIface,
        getSymtabName,
        getDictFastString,
        CheckHiWay(..),
        TraceBinIFaceReading(..),
        getWithUserData,
        putWithUserData

    ) where

#include "HsVersions.h"

import GhcPrelude

import TcRnMonad
import PrelInfo   ( isKnownKeyName, lookupKnownKeyName )
import IfaceEnv
import HscTypes
import Module
import Name
import DynFlags
import UniqFM
import UniqSupply
import Panic
import Binary
import Binary.Unsafe (ioP, ioG)
import SrcLoc
import ErrUtils
import FastMutInt
import Unique
import Outputable
import NameCache
import GHC.Platform
import FastString
import Constants
import Util

import Data.Array
import Data.Array.ST
import Data.Array.Unsafe
import Data.Bits
import Data.Char
import Data.Word
import Data.IORef
import Data.Foldable
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.State.Strict as State

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
                          putLogMsg dflags
                                    NoReason
                                    SevOutput
                                    noSrcSpan
                                    (defaultDumpStyle dflags)
                                    sd
                      QuietBinIFaceReading -> \_ -> return ()

        wantedGot :: String -> a -> a -> (a -> SDoc) -> Get ()
        wantedGot what wanted got ppr' = ioG $
            printer (text what <> text ": " <>
                     vcat [text "Wanted " <> ppr' wanted <> text ",",
                           text "got    " <> ppr' got])

        errorOnMismatch :: (Eq a, Show a) => String -> a -> a -> Get ()
        errorOnMismatch what wanted got = ioG $
            -- This will be caught by readIface which will emit an error
            -- msg containing the iface module name.
            when (wanted /= got) $ throwGhcExceptionIO $ ProgramError
                         (what ++ " (wanted " ++ show wanted
                               ++ ", got "    ++ show got ++ ")")

    bd <- Binary.readBinData hi_path

    runGetIO bd $ do
      -- Read the magic number to check that this really is a GHC .hi file
      -- (This magic number does not change when we change
      --  GHC interface file format)
      magic <- get
      wantedGot "Magic" (binaryInterfaceMagic dflags) magic ppr
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
          then do _ <- Binary.get :: Get Word32; return ()
          else do _ <- Binary.get :: Get Word64; return ()

      -- Check the interface file version and ways.
      check_ver <- get
      let our_ver = show hiVersion
      wantedGot "Version" our_ver check_ver text
      errorOnMismatch "mismatched interface file versions" our_ver check_ver

      check_way <- get
      let way_descr = getWayDescr dflags
      wantedGot "Way" way_descr check_way ppr
      when (checkHiWay == CheckHiWay) $
          errorOnMismatch "mismatched interface file ways" way_descr check_way
      getWithUserData ncu


-- | This performs a get action after reading the dictionary and symbol
-- table. It is necessary to run this before trying to deserialise any
-- Names or FastStrings.
getWithUserData :: Binary a => NameCacheUpdater -> Get a
getWithUserData ncu = do
    -- Read the dictionary
    -- The next word in the file is a pointer to where the dictionary is
    -- (probably at the end of the file)
    dict_p <- Binary.get
    data_p <- tellG               -- Remember where we are now
    seekG dict_p
    dict   <- getDictionary
    seekG data_p                -- Back to where we were before

    readState (error "getSymtabName") (getDictFastString dict) $ do

      symtab_p <- Binary.get       -- Get the symtab ptr
      data_p <- tellG              -- Remember where we are now
      seekG symtab_p
      symtab <- getSymbolTable ncu
      seekG data_p                 -- Back to where we were before

      -- It is only now that we know how to get a `Name`
      readState (getSymtabName ncu dict symtab) (getDictFastString dict) get

-- | Write an interface file
writeBinIface :: DynFlags -> FilePath -> ModIface -> IO ()
writeBinIface dflags hi_path mod_iface = do

    bd <- runPutIO $ do
      put (binaryInterfaceMagic dflags)

      -- dummy 32/64-bit field before the version/way for
      -- compatibility with older interface file formats.
      -- See Note [dummy iface field] above.
      if wORD_SIZE dflags == 4
        then Binary.put (0 :: Word32)
        else Binary.put (0 :: Word64)

      -- The version and way descriptor go next
      put (show hiVersion)
      let way_descr = getWayDescr dflags
      put way_descr

      putWithUserData (ioP . debugTraceMsg dflags 3) mod_iface

    -- And send the result to the file
    writeBinData bd hi_path

-- | Put a piece of data with an initialised `UserData` field. This
-- is necessary if you want to serialise Names or FastStrings.
-- It also writes a symbol table and the dictionary.
-- This segment should be read using `getWithUserData`.
putWithUserData :: Binary a => (SDoc -> Put ()) -> a -> Put ()
putWithUserData log_action payload = do
    -- Remember where the dictionary pointer will go
    dict_p_p <- tellP
    -- Placeholder for ptr to dictionary
    put dict_p_p

    -- Remember where the symbol table pointer will go
    symtab_p_p <- tellP
    put symtab_p_p
    -- Make some initial state
    symtab_next <- ioP $ newFastMutInt
    ioP $ writeFastMutInt symtab_next 0
    symtab_map <- ioP $ newIORef emptyUFM
    let bin_symtab = BinSymbolTable {
                         bin_symtab_next = symtab_next,
                         bin_symtab_map  = symtab_map }
    dict_next_ref <- ioP $ newFastMutInt
    ioP $ writeFastMutInt dict_next_ref 0
    dict_map_ref <- ioP $ newIORef emptyUFM
    let bin_dict = BinDictionary {
                       bin_dict_next = dict_next_ref,
                       bin_dict_map  = dict_map_ref }

    -- Put the main thing,
    writeState (putName bin_dict bin_symtab)
               (putName bin_dict bin_symtab)
               (putFastString bin_dict) $ do
      put payload

      -- Write the symtab pointer at the front of the file
      symtab_p <- tellP             -- This is where the symtab will start
      putAt symtab_p_p symtab_p     -- Fill in the placeholder
      seekP symtab_p           -- Seek back to the end of the file

      -- Write the symbol table itself
      symtab_next <- ioP $ readFastMutInt symtab_next
      symtab_map  <- ioP $ readIORef symtab_map
      putSymbolTable symtab_next symtab_map
      log_action (text "writeBinIface:" <+> int symtab_next
                                        <+> text "Names")

      -- NB. write the dictionary after the symbol table, because
      -- writing the symbol table may create more dictionary entries.

      -- Write the dictionary pointer at the front of the file
      dict_p <- tellP               -- This is where the dictionary will start
      putAt dict_p_p dict_p         -- Fill in the placeholder
      seekP dict_p                  -- Seek back to the end of the file

      -- Write the dictionary itself
      dict_next <- ioP $ readFastMutInt dict_next_ref
      dict_map  <- ioP $ readIORef dict_map_ref
      putDictionary dict_next dict_map
      log_action (text "writeBinIface:" <+> int dict_next
                                        <+> text "dict entries")


binaryInterfaceMagic :: DynFlags -> Word32
binaryInterfaceMagic dflags
 | target32Bit (targetPlatform dflags) = 0x1face
 | otherwise                           = 0x1face64


-- -----------------------------------------------------------------------------
-- The symbol table
--

putSymbolTable :: Int -> UniqFM (Int,Name) -> Put ()
putSymbolTable next_off symtab = do
    put next_off
    let names = elems (array (0,next_off-1) (nonDetEltsUFM symtab))
      -- It's OK to use nonDetEltsUFM here because the elements have
      -- indices that array uses to create order
    mapM_ (\n -> serialiseName n symtab) names

getSymbolTable :: NameCacheUpdater -> Get SymbolTable
getSymbolTable ncu = do
    sz <- get
    od_names <- sequence (replicate sz get)
    ioG $ updateNameCache ncu $ \namecache ->
        runST $ flip State.evalStateT namecache $ do
            mut_arr <- lift $ newSTArray_ (0, sz-1)
            for_ (zip [0..] od_names) $ \(i, odn) -> do
                (nc, !n) <- State.gets $ \nc -> fromOnDiskName nc odn
                lift $ writeArray mut_arr i n
                State.put nc
            arr <- lift $ unsafeFreeze mut_arr
            namecache' <- State.get
            return (namecache', arr)
  where
    -- This binding is required because the type of newArray_ cannot be inferred
    newSTArray_ :: forall s. (Int, Int) -> ST s (STArray s Int Name)
    newSTArray_ = newArray_

type OnDiskName = (UnitId, ModuleName, OccName)

fromOnDiskName :: NameCache -> OnDiskName -> (NameCache, Name)
fromOnDiskName nc (pid, mod_name, occ) =
    let mod   = mkModule pid mod_name
        cache = nsNames nc
    in case lookupOrigNameCache cache  mod occ of
           Just name -> (nc, name)
           Nothing   ->
               let (uniq, us) = takeUniqFromSupply (nsUniqs nc)
                   name       = mkExternalName uniq mod occ noSrcSpan
                   new_cache  = extendNameCache cache mod occ name
               in ( nc{ nsUniqs = us, nsNames = new_cache }, name )

serialiseName :: Name -> UniqFM (Int,Name) -> Put ()
serialiseName name _ = do
    let mod = ASSERT2( isExternalName name, ppr name ) nameModule name
    put (moduleUnitId mod, moduleName mod, nameOccName name)


-- Note [Symbol table representation of names]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- An occurrence of a name in an interface file is serialized as a single 32-bit
-- word. The format of this word is:
--  00xxxxxx xxxxxxxx xxxxxxxx xxxxxxxx
--   A normal name. x is an index into the symbol table
--  10xxxxxx xxyyyyyy yyyyyyyy yyyyyyyy
--   A known-key name. x is the Unique's Char, y is the int part. We assume that
--   all known-key uniques fit in this space. This is asserted by
--   PrelInfo.knownKeyNamesOkay.
--
-- During serialization we check for known-key things using isKnownKeyName.
-- During deserialization we use lookupKnownKeyName to get from the unique back
-- to its corresponding Name.


-- See Note [Symbol table representation of names]
putName :: BinDictionary -> BinSymbolTable -> Name -> Put ()
putName _dict BinSymbolTable{
               bin_symtab_map = symtab_map_ref,
               bin_symtab_next = symtab_next }
        name
  | isKnownKeyName name
  , let (c, u) = unpkUnique (nameUnique name) -- INVARIANT: (ord c) fits in 8 bits
  = -- ASSERT(u < 2^(22 :: Int))
    put (0x80000000
         .|. (fromIntegral (ord c) `shiftL` 22)
         .|. (fromIntegral u :: Word32))

  | otherwise
  = do symtab_map <- ioP $ readIORef symtab_map_ref
       case lookupUFM symtab_map name of
         Just (off,_) -> put (fromIntegral off :: Word32)
         Nothing -> do
            off <- ioP $ readFastMutInt symtab_next
            -- MASSERT(off < 2^(30 :: Int))
            ioP $ writeFastMutInt symtab_next (off+1)
            ioP $ writeIORef symtab_map_ref
                $! addToUFM symtab_map name (off,name)
            put (fromIntegral off :: Word32)

-- See Note [Symbol table representation of names]
getSymtabName :: NameCacheUpdater
              -> Dictionary -> SymbolTable
              -> Get Name
getSymtabName _ncu _dict symtab = do
    i :: Word32 <- get
    case i .&. 0xC0000000 of
      0x00000000 -> return $! symtab ! fromIntegral i

      0x80000000 ->
        let
          tag = chr (fromIntegral ((i .&. 0x3FC00000) `shiftR` 22))
          ix  = fromIntegral i .&. 0x003FFFFF
          u   = mkUnique tag ix
        in
          return $! case lookupKnownKeyName u of
                      Nothing -> pprPanic "getSymtabName:unknown known-key unique"
                                          (ppr i $$ ppr (unpkUnique u))
                      Just n  -> n

      _ -> pprPanic "getSymtabName:unknown name tag" (ppr i)

data BinSymbolTable = BinSymbolTable {
        bin_symtab_next :: !FastMutInt, -- The next index to use
        bin_symtab_map  :: !(IORef (UniqFM (Int,Name)))
                                -- indexed by Name
  }

putFastString :: BinDictionary -> FastString -> Put ()
putFastString dict fs = ioP (allocateFastString dict fs) >>= put

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

getDictFastString :: Dictionary -> Get FastString
getDictFastString dict = do
    j <- get
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
