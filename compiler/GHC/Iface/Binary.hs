{-# LANGUAGE BinaryLiterals, ScopedTypeVariables #-}

--
--  (c) The University of Glasgow 2002-2006
--

{-# OPTIONS_GHC -O2 #-}
-- We always optimise this, otherwise performance of a non-optimised
-- compiler is severely affected

-- | Binary interface file support.
module GHC.Iface.Binary (
        -- * Public API for interface file serialisation
        writeBinIface,
        readBinIface,
        readBinIfaceHeader,
        getSymtabName,
        CheckHiWay(..),
        TraceBinIFace(..),
        getWithUserData,
        putWithUserData,

        -- * Internal serialisation functions
        getSymbolTable,
        putName,
        putSymbolTable,
        BinSymbolTable(..),
    ) where

import GHC.Prelude

import GHC.Tc.Utils.Monad
import GHC.Builtin.Utils   ( isKnownKeyName, lookupKnownKeyName )
import GHC.Unit
import GHC.Unit.Module.ModIface
import GHC.Types.Name
import GHC.Platform.Profile
import GHC.Types.Unique.FM
import GHC.Utils.Panic
import GHC.Utils.Binary as Binary
import GHC.Data.FastMutInt
import GHC.Types.Unique
import GHC.Utils.Outputable
import GHC.Types.Name.Cache
import GHC.Types.SrcLoc
import GHC.Platform
import GHC.Settings.Constants
import GHC.Utils.Fingerprint

import Data.Array
import Data.Array.IO
import Data.Array.Unsafe
import Data.Char
import Data.Word
import Data.IORef
import Control.Monad

-- ---------------------------------------------------------------------------
-- Reading and writing binary interface files
--

data CheckHiWay = CheckHiWay | IgnoreHiWay
    deriving Eq

data TraceBinIFace
   = TraceBinIFace (SDoc -> IO ())
   | QuietBinIFace

-- | Read an interface file header, checking the magic number, version, and
-- way. Returns the hash of the source file and a BinHandle which points at the
-- start of the rest of the interface file data.
readBinIfaceHeader
  :: Profile
  -> NameCache
  -> CheckHiWay
  -> TraceBinIFace
  -> FilePath
  -> IO (Fingerprint, BinHandle)
readBinIfaceHeader profile _name_cache checkHiWay traceBinIFace hi_path = do
    let platform = profilePlatform profile

        wantedGot :: String -> a -> a -> (a -> SDoc) -> IO ()
        wantedGot what wanted got ppr' =
            case traceBinIFace of
               QuietBinIFace         -> return ()
               TraceBinIFace printer -> printer $
                     text what <> text ": " <>
                     vcat [text "Wanted " <> ppr' wanted <> text ",",
                           text "got    " <> ppr' got]

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
    wantedGot "Magic" (binaryInterfaceMagic platform) magic (ppr . unFixedLength)
    errorOnMismatch "magic number mismatch: old/corrupt interface file?"
        (unFixedLength $ binaryInterfaceMagic platform) (unFixedLength magic)

    -- Check the interface file version and profile tag.
    check_ver  <- get bh
    let our_ver = show hiVersion
    wantedGot "Version" our_ver check_ver text
    errorOnMismatch "mismatched interface file versions" our_ver check_ver

    check_tag <- get bh
    let tag = profileBuildTag profile
    wantedGot "Way" tag check_tag text
    when (checkHiWay == CheckHiWay) $
        errorOnMismatch "mismatched interface file profile tag" tag check_tag

    src_hash <- get bh
    pure (src_hash, bh)

-- | Read an interface file.
readBinIface
  :: Profile
  -> NameCache
  -> CheckHiWay
  -> TraceBinIFace
  -> FilePath
  -> IO ModIface
readBinIface profile name_cache checkHiWay traceBinIface hi_path = do
    (src_hash, bh) <- readBinIfaceHeader profile name_cache checkHiWay traceBinIface hi_path

    extFields_p <- get bh

    mod_iface <- getWithUserData name_cache bh

    seekBin bh extFields_p
    extFields <- get bh

    return mod_iface
      { mi_ext_fields = extFields
      , mi_src_hash = src_hash
      }

-- | This performs a get action after reading the dictionary and symbol
-- table. It is necessary to run this before trying to deserialise any
-- Names or FastStrings.
getWithUserData :: Binary a => NameCache -> BinHandle -> IO a
getWithUserData name_cache bh = do
  bh <- getTables name_cache bh
  get bh

-- | Setup a BinHandle to read something written using putWithTables
--
-- Reading names has the side effect of adding them into the given NameCache.
getTables :: NameCache -> BinHandle -> IO BinHandle
getTables name_cache bh = do
    -- Read the dictionary
    -- The next word in the file is a pointer to where the dictionary is
    -- (probably at the end of the file)
    dict <- Binary.forwardGet bh (getDictionary bh)

    -- Initialise the user-data field of bh
    let bh_fs = setUserData bh $ newReadState (error "getSymtabName")
                                              (getDictFastString dict)

    symtab <- Binary.forwardGet bh_fs (getSymbolTable bh_fs name_cache)

    -- It is only now that we know how to get a Name
    return $ setUserData bh $ newReadState (getSymtabName name_cache dict symtab)
                                           (getDictFastString dict)

-- | Write an interface file
writeBinIface :: Profile -> TraceBinIFace -> FilePath -> ModIface -> IO ()
writeBinIface profile traceBinIface hi_path mod_iface = do
    bh <- openBinMem initBinMemSize
    let platform = profilePlatform profile
    put_ bh (binaryInterfaceMagic platform)

    -- The version, profile tag, and source hash go next
    put_ bh (show hiVersion)
    let tag = profileBuildTag profile
    put_  bh tag
    put_  bh (mi_src_hash mod_iface)

    extFields_p_p <- tellBin bh
    put_ bh extFields_p_p

    putWithUserData traceBinIface bh mod_iface

    extFields_p <- tellBin bh
    putAt bh extFields_p_p extFields_p
    seekBin bh extFields_p
    put_ bh (mi_ext_fields mod_iface)

    -- And send the result to the file
    writeBinMem bh hi_path

-- | Put a piece of data with an initialised `UserData` field. This
-- is necessary if you want to serialise Names or FastStrings.
-- It also writes a symbol table and the dictionary.
-- This segment should be read using `getWithUserData`.
putWithUserData :: Binary a => TraceBinIFace -> BinHandle -> a -> IO ()
putWithUserData traceBinIface bh payload = do
  (name_count, fs_count, _b) <- putWithTables bh (\bh' -> put bh' payload)

  case traceBinIface of
    QuietBinIFace         -> return ()
    TraceBinIFace printer -> do
       printer (text "writeBinIface:" <+> int name_count
                                      <+> text "Names")
       printer (text "writeBinIface:" <+> int fs_count
                                      <+> text "dict entries")

-- | Write name/symbol tables
--
-- 1. setup the given BinHandle with Name/FastString table handling
-- 2. write the following
--    - FastString table pointer
--    - Name table pointer
--    - payload
--    - Name table
--    - FastString table
--
-- It returns (number of names, number of FastStrings, payload write result)
--
putWithTables :: BinHandle -> (BinHandle -> IO b) -> IO (Int,Int,b)
putWithTables bh put_payload = do
    -- initialize state for the name table and the FastString table.
    symtab_next <- newFastMutInt 0
    symtab_map <- newIORef emptyUFM
    let bin_symtab = BinSymbolTable
                      { bin_symtab_next = symtab_next
                      , bin_symtab_map  = symtab_map
                      }

    (bh_fs, bin_dict, put_dict) <- initFSTable bh

    (fs_count,(name_count,r)) <- forwardPut bh (const put_dict) $ do

      -- NB. write the dictionary after the symbol table, because
      -- writing the symbol table may create more dictionary entries.
      let put_symtab = do
            name_count <- readFastMutInt symtab_next
            symtab_map  <- readIORef symtab_map
            putSymbolTable bh_fs name_count symtab_map
            pure name_count

      forwardPut bh_fs (const put_symtab) $ do

        -- BinHandle with FastString and Name writing support
        let ud_fs = getUserData bh_fs
        let ud_name = ud_fs
                        { ud_put_nonbinding_name = putName bin_dict bin_symtab
                        , ud_put_binding_name    = putName bin_dict bin_symtab
                        }
        let bh_name = setUserData bh ud_name

        put_payload bh_name

    return (name_count, fs_count, r)



-- | Initial ram buffer to allocate for writing interface files
initBinMemSize :: Int
initBinMemSize = 1024 * 1024

binaryInterfaceMagic :: Platform -> FixedLengthEncoding Word32
binaryInterfaceMagic platform
 | target32Bit platform = FixedLengthEncoding 0x1face
 | otherwise            = FixedLengthEncoding 0x1face64


-- -----------------------------------------------------------------------------
-- The symbol table
--

putSymbolTable :: BinHandle -> Int -> UniqFM Name (Int,Name) -> IO ()
putSymbolTable bh name_count symtab = do
    put_ bh name_count
    let names = elems (array (0,name_count-1) (nonDetEltsUFM symtab))
      -- It's OK to use nonDetEltsUFM here because the elements have
      -- indices that array uses to create order
    mapM_ (\n -> serialiseName bh n symtab) names


getSymbolTable :: BinHandle -> NameCache -> IO SymbolTable
getSymbolTable bh name_cache = do
    sz <- get bh :: IO Int
    -- create an array of Names for the symbols and add them to the NameCache
    updateNameCache' name_cache $ \cache0 -> do
        mut_arr <- newArray_ (0, sz-1) :: IO (IOArray Int Name)
        cache <- foldGet' (fromIntegral sz) bh cache0 $ \i (uid, mod_name, occ) cache -> do
          let mod = mkModule uid mod_name
          case lookupOrigNameCache cache mod occ of
            Just name -> do
              writeArray mut_arr (fromIntegral i) name
              return cache
            Nothing   -> do
              uniq <- takeUniqFromNameCache name_cache
              let name      = mkExternalName uniq mod occ noSrcSpan
                  new_cache = extendOrigNameCache cache mod occ name
              writeArray mut_arr (fromIntegral i) name
              return new_cache
        arr <- unsafeFreeze mut_arr
        return (cache, arr)

serialiseName :: BinHandle -> Name -> UniqFM key (Int,Name) -> IO ()
serialiseName bh name _ = do
    let mod = assertPpr (isExternalName name) (ppr name) (nameModule name)
    put_ bh (moduleUnit mod, moduleName mod, nameOccName name)


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
--   GHC.Builtin.Utils.knownKeyNamesOkay.
--
-- During serialization we check for known-key things using isKnownKeyName.
-- During deserialization we use lookupKnownKeyName to get from the unique back
-- to its corresponding Name.


-- See Note [Symbol table representation of names]
putName :: FSTable -> BinSymbolTable -> BinHandle -> Name -> IO ()
putName _dict BinSymbolTable{
               bin_symtab_map = symtab_map_ref,
               bin_symtab_next = symtab_next }
        bh name
  | isKnownKeyName name
  , let (c, u) = unpkUnique (nameUnique name) -- INVARIANT: (ord c) fits in 8 bits
  = -- assert (u < 2^(22 :: Int))
    put_ bh (0x80000000
             .|. (fromIntegral (ord c) `shiftL` 22)
             .|. (fromIntegral u :: Word32))

  | otherwise
  = do symtab_map <- readIORef symtab_map_ref
       case lookupUFM symtab_map name of
         Just (off,_) -> put_ bh (fromIntegral off :: Word32)
         Nothing -> do
            off <- readFastMutInt symtab_next
            -- massert (off < 2^(30 :: Int))
            writeFastMutInt symtab_next (off+1)
            writeIORef symtab_map_ref
                $! addToUFM symtab_map name (off,name)
            put_ bh (fromIntegral off :: Word32)

-- See Note [Symbol table representation of names]
getSymtabName :: NameCache
              -> Dictionary -> SymbolTable
              -> BinHandle -> IO Name
getSymtabName _name_cache _dict symtab bh = do
    i :: Word32 <- get bh
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
                                          (ppr i $$ ppr u $$ char tag $$ ppr ix)
                      Just n  -> n

      _ -> pprPanic "getSymtabName:unknown name tag" (ppr i)

data BinSymbolTable = BinSymbolTable {
        bin_symtab_next :: !FastMutInt, -- The next index to use
        bin_symtab_map  :: !(IORef (UniqFM Name (Int,Name)))
                                -- indexed by Name
  }

