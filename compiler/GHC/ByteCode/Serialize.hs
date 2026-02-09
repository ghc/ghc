{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Orphans are here since the Binary instances use an ad-hoc means of serialising
-- names which we don't want to pollute the rest of the codebase with.
{-# OPTIONS_GHC -Wno-orphans #-}
{- | This module implements the serialization of bytecode objects to and from disk.
-}
module GHC.ByteCode.Serialize
  ( writeBinByteCode, readBinByteCode, ModuleByteCode(..)
  , BytecodeLibX(..)
  , BytecodeLib
  , OnDiskBytecodeLib
  , InterpreterLibrary(..)
  , InterpreterLibraryContents(..)
  , writeBytecodeLib
  , readBytecodeLib
  , decodeOnDiskModuleByteCode
  , decodeOnDiskBytecodeLib
  , showBytecode
  )
where

import Control.Monad
import Data.Array (Array, assocs)
import Data.Array.Base (UArray(..))
import Data.Array.IArray (IArray, elems)
import Data.Binary qualified as Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable
import Data.IORef
import Data.Proxy
import Data.Traversable
import Data.Word
import qualified Data.IntMap as IM
import Foreign.Storable (Storable, sizeOf)
import GHC.ByteCode.Types
import GHC.Data.FastString
import GHC.Driver.DynFlags
import GHC.Driver.Env
import GHC.Exts (ByteArray#, Int(I#), sizeofByteArray#)
import GHC.HsToCore.Breakpoints (ModBreaks(..))
import GHC.Iface.Binary
import GHC.Linker.Types
import GHC.Prelude
import GHCi.Message (ConInfoTable(..))
import GHC.Types.Name
import GHC.Types.Name.Cache
import GHC.Types.Name.Env
import GHC.Types.SrcLoc
import GHC.Types.Error (MessageClass(MCDump))
import GHC.Unit.Types
import GHC.Utils.Binary
import GHC.Utils.Exception
import GHC.Utils.Logger
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.TmpFs
import Numeric (showHex)
import System.Directory
import System.FilePath
import System.IO.Unsafe (unsafeInterleaveIO)

{- Note [Overview of persistent bytecode]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By default, when using the interpreter, a Haskell module is first compiled to
bytecode (which lives in memory) and then executed by the RTS interpreter.
However, when dealing with many modules compiling to bytecode from scratch every
time is expensive. This is especially relevant for interpreter-heavy workflows
on large projects where changes are incremental or non-existent (e.g. running
the project in the debugger).

In light of this, GHC can produce `.gbc` files, which contain a
serialized representation of the bytecode for a Haskell module. These
files are written by enabling the flag `-fwrite-byte-code` when using the
interpreter.

The driver will always look for both the interface and the `.gbc` file and load
those to avoid unnecessary recompilation. This can save a lot of time if you
have many modules. Even compared to `-fwrite-if-simplified-core`.

.gbc files are standalone, in the sense that they can be loaded into the interpreter
without having the interface file or source files available. In the future you could
create a "bytecode executable", which just contained bytecode objects, a simple wrapper
and the runtime, which would load the bytecode objects and execute main.

.gbc files also contain the contents of object files which arise from foreign files
and other stubs (such as info table map, foreign files added by TH, CApiFFI
etc). In the normal compilation pipeline, these are merged into the final object
by object merging to produce a single .o file. Bytecode objects are not "normal
objects", so they are stored alongside the 'CompiledByteCode' and written to
temporary files when needed.

The ticket where bytecode objects were dicussed is #26298

See Note [-fwrite-byte-code is not the default]
See Note [Recompilation avoidance with bytecode objects]

-}

-- | The on-disk representation of a bytecode object for a specific module.
--
-- This is the representation which we serialise and write to disk.
-- The difference from 'ModuleByteCode' is that the contents of the object files
-- contained by 'ModuleByteCode' are stored in-memory rather than as file paths to
-- temporary files.
data OnDiskModuleByteCode = OnDiskModuleByteCode { odgbc_module :: Module
                                                 , odgbc_compiled_byte_code :: CompiledByteCode
                                                 , odgbc_foreign :: [ByteString]  -- ^ Contents of object files
                                                 }


type OnDiskBytecodeLib = BytecodeLibX (Maybe InterpreterLibraryContents)

instance Outputable a => Outputable (BytecodeLibX a) where
  ppr (BytecodeLib {..}) = vcat [
    (text "BytecodeLib" <+> ppr bytecodeLibUnitId),
    (text "Files" <+> ppr bytecodeLibFiles),
    (text "Foreign" <+> ppr bytecodeLibForeign) ]

type BytecodeLib = BytecodeLibX (Maybe InterpreterLibrary)

-- | A bytecode library is a collection of CompiledByteCode objects and a .so file containing the combination of foreign stubs
data BytecodeLibX a = BytecodeLib {
    bytecodeLibUnitId :: UnitId,
    bytecodeLibFiles :: [CompiledByteCode],
    bytecodeLibForeign :: a -- A library file containing the combination of foreign stubs. (Ie arising from CApiFFI)
}

data InterpreterLibrary = InterpreterSharedObject { getSharedObjectFilePath :: FilePath, getSharedObjectDir :: FilePath, getSharedObjectLibName :: String }
                         | InterpreterStaticObjects { getStaticObjects :: [FilePath] }


instance Outputable InterpreterLibrary where
  ppr (InterpreterSharedObject path dir name) = text "SharedObject" <+> text path <+> text dir <+> text name
  ppr (InterpreterStaticObjects paths) = text "StaticObjects" <+> text (show paths)


data InterpreterLibraryContents = InterpreterLibrarySharedContents { interpreterLibraryContents :: ByteString }
                                | InterpreterLibraryStaticContents { interpreterLibraryStaticContents :: [ByteString] }

instance Binary InterpreterLibraryContents where
  get bh = do
    t <- getByte bh
    case t of
      0 -> InterpreterLibrarySharedContents <$> get bh
      1 -> InterpreterLibraryStaticContents <$> get bh
      _ -> panic "Binary InterpreterLibraryContents: invalid byte"
  put_ bh (InterpreterLibrarySharedContents contents) = do
    putByte bh 0
    put_ bh contents
  put_ bh (InterpreterLibraryStaticContents contents) = do
    putByte bh 1
    put_ bh contents

instance Binary OnDiskBytecodeLib where
  get bh = do
    bytecodeLibUnitId <- get bh
    bytecodeLibFiles <- get bh
    bytecodeLibForeign <- get bh
    pure BytecodeLib {..}

  put_ bh BytecodeLib {..} = do
    put_ bh bytecodeLibUnitId
    put_ bh bytecodeLibFiles
    put_ bh bytecodeLibForeign



writeBytecodeLib :: BytecodeLib -> FilePath -> IO ()
writeBytecodeLib lib path = do
  odbco <- encodeBytecodeLib lib
  createDirectoryIfMissing True (takeDirectory path)
  bh' <- openBinMem (1024 * 1024)
  bh <- addBinNameWriter bh'
  putWithUserData QuietBinIFace NormalCompression bh odbco
  writeBinMem bh path

readBytecodeLib :: HscEnv -> FilePath -> IO OnDiskBytecodeLib
readBytecodeLib hsc_env path = do
  bh' <- readBinMem path
  bh <- addBinNameReader hsc_env bh'
  res <- getWithUserData (hsc_NC hsc_env) bh
  pure res

instance Binary OnDiskModuleByteCode where
  get bh = do
    odgbc_module <- get bh
    odgbc_compiled_byte_code <- get bh
    odgbc_foreign <- get bh
    pure OnDiskModuleByteCode {..}

  put_ bh OnDiskModuleByteCode {..} = do
    put_ bh odgbc_module
    put_ bh odgbc_compiled_byte_code
    put_ bh odgbc_foreign

-- | Convert an 'OnDiskModuleByteCode' to an 'ModuleByteCode'.
-- 'OnDiskModuleByteCode' is the representation which we read from a file,
-- the 'ModuleByteCode' is the representation which is manipulated by program logic.
--
-- This notably writes the object files to temporary files.
-- They are written to temporary files so that the normal object file loading
-- code paths (which expect object files to exist as on-disk files) can be used
-- in the loader.
decodeOnDiskModuleByteCode :: HscEnv -> OnDiskModuleByteCode -> IO ModuleByteCode
decodeOnDiskModuleByteCode hsc_env odbco = do
  foreign_files <- writeObjectFiles (hsc_logger hsc_env) (hsc_tmpfs hsc_env) (tmpDir (hsc_dflags hsc_env)) (odgbc_foreign odbco)
  pure $ ModuleByteCode {
    gbc_module = odgbc_module odbco,
    gbc_compiled_byte_code = odgbc_compiled_byte_code odbco,
    gbc_foreign_files = foreign_files
   }

decodeOnDiskBytecodeLib :: HscEnv -> OnDiskBytecodeLib -> IO BytecodeLib
decodeOnDiskBytecodeLib hsc_env odbco = do
  foreign_contents <- traverse (writeInterpreterLibraryFile (hsc_logger hsc_env) (hsc_tmpfs hsc_env) (tmpDir (hsc_dflags hsc_env)))  (bytecodeLibForeign odbco)
  pure $ BytecodeLib {
    bytecodeLibUnitId = bytecodeLibUnitId odbco,
    bytecodeLibFiles = bytecodeLibFiles odbco,
    bytecodeLibForeign = foreign_contents
   }

encodeBytecodeLib :: BytecodeLib -> IO OnDiskBytecodeLib
encodeBytecodeLib (BytecodeLib {..}) = do
  foreign_contents <- traverse readInterpreterLibraryFile bytecodeLibForeign
  pure $ BytecodeLib {
    bytecodeLibUnitId = bytecodeLibUnitId,
    bytecodeLibFiles = bytecodeLibFiles,
    bytecodeLibForeign = foreign_contents
   }

readObjectFile :: FilePath -> IO ByteString
readObjectFile f = BS.readFile f

readObjectFiles :: [FilePath] -> IO [ByteString]
readObjectFiles fs = mapM readObjectFile fs

-- | Write a list of bytestrings, representing object files, to a temporary files.
writeObjectFiles ::  Logger -> TmpFs -> TempDir -> [ByteString] -> IO [FilePath]
writeObjectFiles logger tmpfs tmp_dir files =
  for files $ \file -> do
    f <- newTempName logger tmpfs tmp_dir TFL_GhcSession "o"
    BS.writeFile f file
    pure f

writeInterpreterLibraryFile :: Logger -> TmpFs -> TempDir -> InterpreterLibraryContents -> IO InterpreterLibrary
writeInterpreterLibraryFile logger tmpfs tmp_dir (InterpreterLibrarySharedContents contents) = do
  (soFile, libdir, libname)  <- newTempLibName logger tmpfs tmp_dir TFL_GhcSession "so"
  BS.writeFile soFile contents
  pure (InterpreterSharedObject soFile libdir libname)
writeInterpreterLibraryFile logger tmpfs tmp_dir (InterpreterLibraryStaticContents contents) = do
  object_files <- writeObjectFiles logger tmpfs tmp_dir contents
  pure (InterpreterStaticObjects object_files)

readInterpreterLibraryFile :: InterpreterLibrary -> IO InterpreterLibraryContents
readInterpreterLibraryFile (InterpreterSharedObject path _ _) = do
  contents <- BS.readFile path
  pure (InterpreterLibrarySharedContents contents)
readInterpreterLibraryFile (InterpreterStaticObjects paths) = do
  contents <- readObjectFiles paths
  pure (InterpreterLibraryStaticContents contents)

-- | Prepare an in-memory 'ModuleByteCode' for writing to disk.
encodeOnDiskModuleByteCode :: ModuleByteCode -> IO OnDiskModuleByteCode
encodeOnDiskModuleByteCode bco = do
  foreign_contents <- readObjectFiles (gbc_foreign_files bco)
  pure $ OnDiskModuleByteCode {
    odgbc_module = gbc_module bco,
    odgbc_compiled_byte_code = gbc_compiled_byte_code bco,
    odgbc_foreign = foreign_contents
   }

-- | Read a 'ModuleByteCode' from a file.
readBinByteCode :: HscEnv -> FilePath -> IO ModuleByteCode
readBinByteCode hsc_env f = do
  odbco <- readOnDiskModuleByteCode hsc_env f
  decodeOnDiskModuleByteCode hsc_env odbco

readOnDiskModuleByteCode :: HscEnv -> FilePath -> IO OnDiskModuleByteCode
readOnDiskModuleByteCode hsc_env f = do
  bh' <- readBinMem f
  bh <- addBinNameReader hsc_env bh'
  getWithUserData (hsc_NC hsc_env) bh

-- | Write a 'ModuleByteCode' to a file.
writeBinByteCode :: FilePath -> ModuleByteCode -> IO ()
writeBinByteCode f cbc = do
  createDirectoryIfMissing True (takeDirectory f)
  bh' <- openBinMem (1024 * 1024)
  bh <- addBinNameWriter bh'
  odbco <- encodeOnDiskModuleByteCode cbc
  putWithUserData QuietBinIFace NormalCompression bh odbco
  writeBinMem bh f

instance Binary CompiledByteCode where
  get bh = do
    bc_bcos <- get bh
    bc_itbls_len <- get bh
    bc_itbls <- replicateM bc_itbls_len $ do
      nm <- getViaBinName bh
      itbl <- get bh
      pure (nm, itbl)
    bc_strs_len <- get bh
    bc_strs <-
      replicateM bc_strs_len $ (,) <$> getViaBinName bh <*> get bh
    bc_breaks <- get bh
    bc_spt_entries <- get bh
    return $
      CompiledByteCode
        { bc_bcos,
          bc_itbls,
          bc_strs,
          bc_breaks,
          bc_spt_entries
        }

  put_ bh CompiledByteCode {..} = do
    put_ bh bc_bcos
    put_ bh $ length bc_itbls
    for_ bc_itbls $ \(nm, itbl) -> do
      putViaBinName bh nm
      put_ bh itbl
    put_ bh $ length bc_strs
    for_ bc_strs $ \(nm, str) -> putViaBinName bh nm *> put_ bh str
    put_ bh bc_breaks
    put_ bh bc_spt_entries

instance Binary UnlinkedBCO where
  get bh =
    UnlinkedBCO
      <$> getViaBinName bh
      <*> get bh
      <*> (Binary.decode <$> get bh)
      <*> (Binary.decode <$> get bh)
      <*> get bh
      <*> get bh

  put_ bh UnlinkedBCO {..} = do
    putViaBinName bh unlinkedBCOName
    put_ bh unlinkedBCOArity
    put_ bh $ Binary.encode unlinkedBCOInstrs
    put_ bh $ Binary.encode unlinkedBCOBitmap
    put_ bh unlinkedBCOLits
    put_ bh unlinkedBCOPtrs

instance Binary BCOPtr where
  get bh = do
    t <- getByte bh
    case t of
      0 -> BCOPtrName <$> getViaBinName bh
      1 -> BCOPtrPrimOp <$> get bh
      2 -> BCOPtrBCO <$> get bh
      3 -> BCOPtrBreakArray <$> get bh
      _ -> panic "Binary BCOPtr: invalid byte"

  put_ bh ptr = case ptr of
    BCOPtrName nm -> putByte bh 0 *> putViaBinName bh nm
    BCOPtrPrimOp op -> putByte bh 1 *> put_ bh op
    BCOPtrBCO bco -> putByte bh 2 *> put_ bh bco
    BCOPtrBreakArray info_mod -> putByte bh 3 *> put_ bh info_mod

instance Binary BCONPtr where
  get bh = do
    t <- getByte bh
    case t of
      0 -> BCONPtrWord . fromIntegral <$> (get bh :: IO Word64)
      1 -> BCONPtrLbl <$> get bh
      2 -> BCONPtrItbl <$> getViaBinName bh
      3 -> BCONPtrAddr <$> getViaBinName bh
      4 -> BCONPtrStr <$> get bh
      5 -> BCONPtrFS <$> get bh
      6 -> BCONPtrFFIInfo <$> get bh
      7 -> BCONPtrCostCentre <$> get bh
      _ -> panic "Binary BCONPtr: invalid byte"

  put_ bh ptr = case ptr of
    BCONPtrWord lit -> putByte bh 0 *> put_ bh (fromIntegral lit :: Word64)
    BCONPtrLbl sym -> putByte bh 1 *> put_ bh sym
    BCONPtrItbl nm -> putByte bh 2 *> putViaBinName bh nm
    BCONPtrAddr nm -> putByte bh 3 *> putViaBinName bh nm
    BCONPtrStr str -> putByte bh 4 *> put_ bh str
    BCONPtrFS fs -> putByte bh 5 *> put_ bh fs
    BCONPtrFFIInfo ffi -> putByte bh 6 *> put_ bh ffi
    BCONPtrCostCentre ibi -> putByte bh 7 *> put_ bh ibi

newtype BinName = BinName {unBinName :: Name}

getViaBinName :: ReadBinHandle -> IO Name
getViaBinName bh = case findUserDataReader Proxy bh of
  BinaryReader f -> unBinName <$> f bh

putViaBinName :: WriteBinHandle -> Name -> IO ()
putViaBinName bh nm = case findUserDataWriter Proxy bh of
  BinaryWriter f -> f bh $ BinName nm

data BytecodeNameEnv = ByteCodeNameEnv { _bytecode_next_id :: !Word64
                                       , _bytecode_name_subst :: NameEnv Word64
                                       }

addBinNameWriter :: WriteBinHandle -> IO WriteBinHandle
addBinNameWriter bh' = do
  env_ref <- newIORef (ByteCodeNameEnv 0 emptyNameEnv)
  evaluate
    $ flip addWriterToUserData bh'
    $ BinaryWriter
    $ \bh (BinName nm) ->
      if
        | isExternalName nm -> do
            putByte bh 0
            put_ bh nm
        | otherwise -> do
            putByte bh 1
            key <- getBinNameKey env_ref nm
            -- Delimit the OccName from the deterministic counter to keep the
            -- encoding injective, avoiding collisions like "foo1" vs "foo#1".
            put_ bh (occNameFS (occName nm) `appendFS` mkFastString ('#' : show key))
  where
    -- Find a deterministic key for local names. This
    getBinNameKey ref name = do
      atomicModifyIORef ref (\b@(ByteCodeNameEnv next subst) ->
        case lookupNameEnv subst name of
          Just idx -> (b, idx)
          Nothing  -> (ByteCodeNameEnv (next + 1) (extendNameEnv subst name next), next))

addBinNameReader :: HscEnv -> ReadBinHandle -> IO ReadBinHandle
addBinNameReader HscEnv {..} bh' = do
  env_ref <- newIORef emptyOccEnv
  pure $ flip addReaderToUserData bh' $ BinaryReader $ \bh -> do
    t <- getByte bh
    case t of
      0 -> do
        nm <- get bh
        pure $ BinName nm
      1 -> do
        occ <- mkVarOccFS <$> get bh
        -- We don't want to get a new unique from the NameCache each time we
        -- see a name.
        nm' <- unsafeInterleaveIO $ do
          u <- takeUniqFromNameCache hsc_NC
          evaluate $ mkInternalName u occ noSrcSpan
        fmap BinName $ atomicModifyIORef' env_ref $ \env ->
          case lookupOccEnv env occ of
            Just nm -> (env, nm)
            _ -> nm' `seq` (extendOccEnv env occ nm', nm')
      _ -> panic "Binary BinName: invalid byte"

-- Note [Serializing Names in bytecode]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The bytecode related types contain various Names which we need to
-- serialize. Unfortunately, we can't directly use the Binary instance
-- of Name: it is only meant to be used for serializing external Names
-- in BinIface logic, but bytecode does contain internal Names.
--
-- We also need to maintain the invariant that: any pair of internal
-- Names with equal/different uniques must also be deserialized to
-- have the same equality. Therefore when we write the names to the interface, we
-- use an incrementing counter to give each local name it's own unique number. A substitution
-- is maintained to give each occurence of the Name the same unique key. When the interface
-- is read, a reverse mapping is used from these unique keys to a Name.


-- | Debugging function for displaying a .gbc file.
showBytecode :: Logger -> HscEnv -> FilePath -> IO ()
showBytecode logger hsc_env filename = do
  odbco <- readOnDiskModuleByteCode hsc_env filename
  logMsg logger MCDump noSrcSpan
    $ withPprStyle defaultDumpStyle
    $ pprOnDiskModuleByteCode odbco

pprOnDiskModuleByteCode :: OnDiskModuleByteCode -> SDoc
pprOnDiskModuleByteCode OnDiskModuleByteCode{..} =
  vcat
    [ text "module:" <+> ppr odgbc_module
    , text "compiled bytecode:" $$ nest 2 (pprCompiledByteCodeDetailed odgbc_compiled_byte_code)
    , text "foreign blobs:" $$ nest 2 (pprForeignBlobs odgbc_foreign)
    ]

pprForeignBlobs :: [ByteString] -> SDoc
pprForeignBlobs blobs =
  case zip [(0 :: Int)..] blobs of
    [] -> text "<none>"
    entries -> vcat $ map (\(i, blob) ->
                 vcat [ text ("blob[" ++ show i ++ "] length " ++ show (BS.length blob))
                      , nest 2 (pprByteString blob)
                      ]) entries

pprCompiledByteCodeDetailed :: CompiledByteCode -> SDoc
pprCompiledByteCodeDetailed CompiledByteCode{..} =
  vcat
    [ text "BCOs:" $$ nest 2 (pprBCOList (elemsFlatBag bc_bcos))
    , text "info tables:" $$ nest 2 (pprInfoTables bc_itbls)
    , text "strings:" $$ nest 2 (pprStringLiterals bc_strs)
    , text "breakpoints:" $$ nest 2 (maybe (text "<none>") pprInternalModBreaksDetailed bc_breaks)
    , text "spt entries:" $$ nest 2 (pprListOrNone (map ppr bc_spt_entries))
    ]

pprBCOList :: [UnlinkedBCO] -> SDoc
pprBCOList [] = text "<none>"
pprBCOList bcos = vcat (map pprUnlinkedBCODetailed bcos)

pprUnlinkedBCODetailed :: UnlinkedBCO -> SDoc
pprUnlinkedBCODetailed UnlinkedBCO{..} =
  vcat
    [ text "BCO" <+> ppr unlinkedBCOName
    , nest 2 $ vcat
        [ text "arity:" <+> int unlinkedBCOArity
        , text "instrs:" <+> pprIntegerList (wordListFromBCO unlinkedBCOInstrs)
        , text "bitmap:" <+> pprIntegerList (wordListFromBCO unlinkedBCOBitmap)
        , text "lits:" $$ nest 2 (pprFlatBag pprBCONPtr unlinkedBCOLits)
        , text "ptrs:" $$ nest 2 (pprFlatBag pprBCOPtr unlinkedBCOPtrs)
        ]
    ]

pprBCONPtr :: BCONPtr -> SDoc
pprBCONPtr ptr = case ptr of
  BCONPtrWord lit -> tag "word" <+> integer (toInteger lit)
  BCONPtrLbl sym -> tag "label" <+> ppr sym
  BCONPtrItbl nm -> tag "itbl" <+> ppr nm
  BCONPtrAddr nm -> tag "addr" <+> ppr nm
  BCONPtrStr bs -> tag "string" $$ nest 2 (pprByteString bs)
  BCONPtrFS fs -> tag "fs" <+> ppr fs
  BCONPtrFFIInfo ffi -> tag "ffi" <+> text (show ffi)
  BCONPtrCostCentre ibi -> tag "cost-centre" <+> ppr ibi
  where
    tag t = text t <> colon

pprBCOPtr :: BCOPtr -> SDoc
pprBCOPtr ptr = case ptr of
  BCOPtrName nm -> tag "name" <+> ppr nm
  BCOPtrPrimOp op -> tag "primop" <+> ppr op
  BCOPtrBCO bco -> tag "bco" $$ nest 2 (pprUnlinkedBCODetailed bco)
  BCOPtrBreakArray m -> tag "break-array" <+> ppr m
  where
    tag t = text t <> colon

pprInfoTables :: [(Name, ConInfoTable)] -> SDoc
pprInfoTables [] = text "<none>"
pprInfoTables entries =
  vcat [ ppr nm <> colon <+> text (show info)
       | (nm, info) <- entries
       ]

pprStringLiterals :: [(Name, ByteString)] -> SDoc
pprStringLiterals [] = text "<none>"
pprStringLiterals entries =
  vcat [ ppr nm <> colon $$ nest 2 (pprByteString bytes)
       | (nm, bytes) <- entries
       ]

pprFlatBag :: (a -> SDoc) -> FlatBag a -> SDoc
pprFlatBag printer bag =
  case elemsFlatBag bag of
    [] -> text "<empty>"
    xs -> vcat (map printer xs)

pprInternalModBreaksDetailed :: InternalModBreaks -> SDoc
pprInternalModBreaksDetailed InternalModBreaks{..} =
  vcat
    [ text "module:" <+> ppr (modBreaks_module imodBreaks_modBreaks)
    , text "break info:" $$ nest 2 (pprBreakInfoMap imodBreaks_breakInfo)
    , text "mod breaks:" $$ nest 2 (pprModBreaksDetailed imodBreaks_modBreaks)
    ]

pprBreakInfoMap :: IM.IntMap CgBreakInfo -> SDoc
pprBreakInfoMap infos
  | IM.null infos = text "<empty>"
  | otherwise = vcat [ int ix <> colon <+> ppr info
                     | (ix, info) <- IM.toList infos
                     ]

pprModBreaksDetailed :: ModBreaks -> SDoc
pprModBreaksDetailed ModBreaks{..} =
  vcat
    [ text "locations:" $$ nest 2 (pprArrayWith (ppr . unBinSrcSpan) modBreaks_locs_)
    , text "vars:" $$ nest 2 (pprArrayWith ppr modBreaks_vars)
    , text "decls:" $$ nest 2 (pprArrayWith pprStringsList modBreaks_decls)
    , text "ccs:" $$ nest 2 (pprArrayWith pprCC modBreaks_ccs)
    ]

pprCC :: (String, String) -> SDoc
pprCC (occ, spanDesc) =
  parens (text (show occ) <> comma <+> text (show spanDesc))

pprStringsList :: [String] -> SDoc
pprStringsList strs =
  brackets $ fsep $ punctuate comma (map (text . show) strs)

pprArrayWith :: (a -> SDoc) -> Array Int a -> SDoc
pprArrayWith printer arr =
  case assocs arr of
    [] -> text "<empty>"
    xs -> vcat [ int idx <> colon <+> printer val
               | (idx, val) <- xs
               ]

pprByteString :: ByteString -> SDoc
pprByteString bs
  | BS.null bs = text "<empty>"
  | otherwise =
      let bytes = BS.unpack bs
          chunks = chunked 16 (map byteHex bytes)
      in vcat (map text chunks)

chunked :: Int -> [String] -> [String]
chunked _ [] = []
chunked n xs =
  unwords (take n xs) : chunked n (drop n xs)

byteHex :: Word8 -> String
byteHex w =
  let s = showHex w ""
  in replicate (2 - length s) '0' ++ s

pprListOrNone :: [SDoc] -> SDoc
pprListOrNone [] = text "<none>"
pprListOrNone docs = vcat docs

pprIntegerList :: [Integer] -> SDoc
pprIntegerList ints =
  brackets $ fsep $ punctuate comma (map integer ints)

wordListFromBCO :: forall a. (Integral a, Storable a, IArray UArray a)
                => BCOByteArray a -> [Integer]
wordListFromBCO arr = map (toInteger . fromIntegral) (bcoByteArrayElems arr)

bcoByteArrayElems :: forall a. (Storable a, IArray UArray a) => BCOByteArray a -> [a]
bcoByteArrayElems (BCOByteArray ba#) =
  let elemSize = sizeOf (undefined :: a)
      lenBytes = I# (sizeofByteArray# ba#)
      count
        | elemSize <= 0 = 0
        | otherwise = lenBytes `div` elemSize
      hi = count - 1
      ua = UArray 0 hi count ba# :: UArray Int a
  in elems ua
