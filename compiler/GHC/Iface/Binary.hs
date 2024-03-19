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

import GHC.Builtin.Utils   ( isKnownKeyName, lookupKnownKeyName )
import GHC.Unit
import GHC.Unit.Module.ModIface
import GHC.Types.Name
import GHC.Platform.Profile
import GHC.Types.Unique.FM
import GHC.Utils.Panic
import GHC.Utils.Binary as Binary
import GHC.Data.FastMutInt
import GHC.Data.FastString (FastString)
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
import GHC.Iface.Type (IfaceType, getIfaceType, putIfaceType)
import System.IO.Unsafe


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
  -> IO (Fingerprint, ReadBinHandle)
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
--
-- See Note [Iface Binary Serialisation] for details.
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

    seekBinReader bh extFields_p
    extFields <- get bh

    return mod_iface
      { mi_ext_fields = extFields
      , mi_src_hash = src_hash
      }

-- | This performs a get action after reading the dictionary and symbol
-- table. It is necessary to run this before trying to deserialise any
-- Names or FastStrings.
getWithUserData :: Binary a => NameCache -> ReadBinHandle -> IO a
getWithUserData name_cache bh = do
  bh <- getTables name_cache bh
  get bh

-- | Setup a BinHandle to read something written using putWithTables
--
-- Reading names has the side effect of adding them into the given NameCache.
getTables :: NameCache -> ReadBinHandle -> IO ReadBinHandle
getTables name_cache bh = do
    bhRef <- newIORef (error "used too soon")
    -- It is important this is passed to 'getTable'
    ud <- unsafeInterleaveIO (readIORef bhRef)

    fsReaderTable <- initFastStringReaderTable
    nameReaderTable <- initReadNameCachedBinary name_cache
    ifaceTypeReaderTable <- initReadIfaceTypeTable ud

    -- The order of these deserialisation matters!
    --
    -- See Note [Iface Binary Serialiser Order] for details.
    fsTable <- Binary.forwardGet bh (getTable fsReaderTable bh)
    let
      fsReader = mkReaderFromTable fsReaderTable fsTable
      bhFs = addReaderToUserData fsReader bh


    nameTable <- Binary.forwardGet bh (getTable nameReaderTable bhFs)
    let
      nameReader = mkReaderFromTable nameReaderTable nameTable
      bhName = addReaderToUserData nameReader bhFs

    ifaceTypeTable <- Binary.forwardGet bh (getTable ifaceTypeReaderTable bhName)
    let
      ifaceTypeReader = mkReaderFromTable ifaceTypeReaderTable ifaceTypeTable
      bhIfaceType = addReaderToUserData ifaceTypeReader bhName

    writeIORef bhRef (getReaderUserData bhIfaceType)
    pure bhIfaceType

-- | Write an interface file.
--
-- See Note [Iface Binary Serialisation] for details.
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

    extFields_p_p <- tellBinWriter bh
    put_ bh extFields_p_p

    putWithUserData traceBinIface bh mod_iface

    extFields_p <- tellBinWriter bh
    putAt bh extFields_p_p extFields_p
    seekBinWriter bh extFields_p
    put_ bh (mi_ext_fields mod_iface)

    -- And send the result to the file
    writeBinMem bh hi_path

-- | Put a piece of data with an initialised `UserData` field. This
-- is necessary if you want to serialise Names or FastStrings.
-- It also writes a symbol table and the dictionary.
-- This segment should be read using `getWithUserData`.
putWithUserData :: Binary a => TraceBinIFace -> WriteBinHandle -> a -> IO ()
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
-- See Note [Iface Binary Serialiser Order]
putWithTables :: WriteBinHandle -> (WriteBinHandle -> IO b) -> IO (Int, Int, b)
putWithTables bh' put_payload = do
  (fast_wt, fsWriter) <- initFastStringWriterTable
  (name_wt, nameWriter) <- initWriteNameTable
  (ifaceType_wt, ifaceTypeWriter) <- initWriteIfaceType

  let writerUserData = mkWriterUserData
        [ mkSomeBinaryWriter @FastString fsWriter
        , mkSomeBinaryWriter @Name nameWriter
        , mkSomeBinaryWriter @BindingName  $ mkWriter (\bh name -> putEntry nameWriter bh (getBindingName name))
        , mkSomeBinaryWriter @IfaceType ifaceTypeWriter
        ]
  let bh = setWriterUserData bh' writerUserData

  (fs_count : name_count : _, r) <-
    -- The order of these entries matters!
    --
    -- See Note [Iface Binary Serialiser Order] for details.
    putAllTables bh [fast_wt, name_wt, ifaceType_wt] $ do
      put_payload bh

  return (name_count, fs_count, r)
 where
  putAllTables _ [] act = do
    a <- act
    pure ([], a)
  putAllTables bh (x : xs) act = do
    (r, (res, a)) <- forwardPut bh (const $ putTable x bh) $ do
      putAllTables bh xs act
    pure (r : res, a)

-- | Initial ram buffer to allocate for writing interface files
initBinMemSize :: Int
initBinMemSize = 1024 * 1024

binaryInterfaceMagic :: Platform -> FixedLengthEncoding Word32
binaryInterfaceMagic platform
 | target32Bit platform = FixedLengthEncoding 0x1face
 | otherwise            = FixedLengthEncoding 0x1face64


{-
Note [Iface Binary Serialisation]
~~~~~~~~~~~~~~~~~~~
When we serialise a 'ModIface', many symbols are redundant.
For example, there can be duplicated 'FastString's and 'Name's.
To save space, we deduplicate some symbols, such as 'FastString' and 'Name',
by maintaining a table of already seen symbols.
When serialising a symbol, we lookup whether we have encountered the symbol before.
If yes, we write the index of the symbol, otherwise we generate a new index and store it in the table.

Besides saving a lot of disk space, this additionally enables us to automatically share
these symbols when we read the 'ModIface' from disk, without additional mechanisms such as 'FastStringTable'.

Note [Iface Binary Serialiser Order]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Serialisation of 'ModIface' uses tables to deduplicate symbols that occur often.

After 'ModIface' has been written to disk, we write the deduplication tables.
Writing a table may add additional entries to *other* deduplication tables, thus
we need to make sure that the symbol table we serialise only depends on
deduplication tables that haven't been written to disk yet.

For example, assume we maintain deduplication tables for 'FastString' and 'Name'.
The symbol 'Name' depends on 'FastString', so serialising a 'Name' may add a 'FastString'
to the 'FastString' deduplication table.
Thus, 'Name' table needs to be serialised to disk before the 'FastString' table.

When we read the 'ModIface' from disk, we consequentially need to read the 'FastString'
deduplication table from disk, before we can deserialise the 'Name' deduplication table.
Therefore, before we serialise the tables, we write forward pointers that allow us to jump ahead
to the table we need to deserialise first.

Here, a visualisation of the table structure we currently have:

┌──────────────┐
│   Headers    │
├──────────────┤
│   Ptr FS     ├────────┐
├──────────────┤        │
│   Ptr Name   ├─────┐  │
├──────────────┤     │  │
│              │     │  │
│   ModIface   │     │  │
│   Payload    │     │  │
│              │     │  │
├──────────────┤     │  │
│              │     │  │
│  Name Table  │◄────┘  │
│              │        │
├──────────────┤        │
│              │        │
│   FS Table   │◄───────┘
│              │
└──────────────┘

-}


-- -----------------------------------------------------------------------------
-- The symbol table
--

initReadIfaceTypeTable :: ReaderUserData -> IO (ReaderTable IfaceType)
initReadIfaceTypeTable ud = do
  pure $
    ReaderTable
      { getTable = getGenericSymbolTable (\bh -> getIfaceType (setReaderUserData bh ud))
      , mkReaderFromTable = \tbl -> mkReader (getGenericSymtab tbl)
      }

initWriteIfaceType :: IO (WriterTable, BinaryWriter IfaceType)
initWriteIfaceType = do
  sym_tab <- initGenericSymbolTable
  pure
    ( WriterTable
        { putTable = putGenericSymbolTable sym_tab (lazyPut' putIfaceType)
        }
    , mkWriter $ putGenericSymTab sym_tab
    )


initReadNameCachedBinary :: NameCache -> IO (ReaderTable Name)
initReadNameCachedBinary cache = do
  return $
    ReaderTable
      { getTable = \bh -> getSymbolTable bh cache
      , mkReaderFromTable = \tbl -> mkReader (getSymtabName tbl)
      }

data BinSymbolTable = BinSymbolTable {
        bin_symtab_next :: !FastMutInt, -- The next index to use
        bin_symtab_map  :: !(IORef (UniqFM Name (Int,Name)))
                                -- indexed by Name
  }

initWriteNameTable :: IO (WriterTable, BinaryWriter Name)
initWriteNameTable = do
  symtab_next <- newFastMutInt 0
  symtab_map <- newIORef emptyUFM
  let bin_symtab =
        BinSymbolTable
          { bin_symtab_next = symtab_next
          , bin_symtab_map = symtab_map
          }

  let put_symtab bh = do
        name_count <- readFastMutInt symtab_next
        symtab_map <- readIORef symtab_map
        putSymbolTable bh name_count symtab_map
        pure name_count

  return
    ( WriterTable
        { putTable = put_symtab
        }
    , mkWriter $ putName bin_symtab
    )


putSymbolTable :: WriteBinHandle -> Int -> UniqFM Name (Int,Name) -> IO ()
putSymbolTable bh name_count symtab = do
    put_ bh name_count
    let names = elems (array (0,name_count-1) (nonDetEltsUFM symtab))
      -- It's OK to use nonDetEltsUFM here because the elements have
      -- indices that array uses to create order
    mapM_ (\n -> serialiseName bh n symtab) names


getSymbolTable :: ReadBinHandle -> NameCache -> IO (SymbolTable Name)
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

serialiseName :: WriteBinHandle -> Name -> UniqFM key (Int,Name) -> IO ()
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
putName :: BinSymbolTable -> WriteBinHandle -> Name -> IO ()
putName BinSymbolTable{
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
getSymtabName :: SymbolTable Name
              -> ReadBinHandle -> IO Name
getSymtabName symtab bh = do
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
