{-# LANGUAGE CPP #-}

module GHC.StgToCmm.InfoTableProv (emitIpeBufferListNode) where

import Foreign

#if defined(HAVE_LIBZSTD)
import Foreign.C.Types
import qualified Data.ByteString.Internal as BSI
import GHC.IO (unsafePerformIO)
#endif

import GHC.Prelude
import GHC.Platform
import GHC.Types.SrcLoc (pprUserRealSpan, srcSpanFile)
import GHC.Unit.Module
import GHC.Utils.Outputable
import GHC.Data.FastString (fastStringToShortText, unpackFS, LexicalFastString(..))

import GHC.Cmm
import GHC.Cmm.CLabel
import GHC.Cmm.Utils

import GHC.StgToCmm.Config
import GHC.StgToCmm.Monad

import GHC.Data.ShortText (ShortText)
import qualified GHC.Data.ShortText as ST

import Control.Monad.Trans.State.Strict

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M

{-
Note [Compression and Decompression of IPE data]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Compiling with `-finfo-table-map` causes build results to include a map from
info tables to source positions called the info table provenance entry (IPE)
map. See Note [Mapping Info Tables to Source Positions]. The IPE information
can grow the size of build results significantly. At the time of writing, a
default build of GHC results in a total of 109M of libHSghc-*.so build results.
A default+ipe build of GHC (see ./hadrian/doc/flavours.md) results in 262M of
libHSghc-*.so build results without compression.

We reduce the impact of IPE data on the size of build results by compressing
the data before it is emitted using the zstd compression library. See
Note [The Info Table Provenance Entry (IPE) Map] for information on the layout
of IPE data on disk and in the RTS. We cannot simply compress all data held in
the IPE entry buffer, as the pointers to info tables must be converted to
memory addresses during linking. Therefore, we can only compress the strings
table and the IPE entries themselves (which essentially only consist of indices
into the strings table).

With compression, a default+ipe build of GHC results in a total of 205M of
libHSghc-*.so build results. This is over a 20% reduction from the uncompressed
case.

Decompression happens lazily, as it only occurs when the IPE map is
constructed (which is also done lazily on first lookup or traversal). During
construction, the 'compressed' field of each IPE buffer list node is examined.
If the field indicates that the data has been compressed, the entry data and
strings table are decompressed before continuing with the normal IPE map
construction.
-}

emitIpeBufferListNode ::
     Module
  -> [InfoProvEnt]
  -> FCode ()
emitIpeBufferListNode _ [] = return ()
emitIpeBufferListNode this_mod ents = do
    cfg <- getStgToCmmConfig

    tables_lbl  <- mkStringLitLabel <$> newUnique
    strings_lbl <- mkStringLitLabel <$> newUnique
    entries_lbl <- mkStringLitLabel <$> newUnique

    let ctx      = stgToCmmContext cfg
        platform = stgToCmmPlatform cfg
        int n    = mkIntCLit platform n

        ((cg_ipes, unit_id, module_name), strtab) = flip runState emptyStringTable $ do
          unit_id <- lookupStringTable $ ST.pack $ renderWithContext ctx (ppr $ moduleName this_mod)
          module_name <- lookupStringTable $ ST.pack $ renderWithContext ctx (ppr $ moduleUnit this_mod)
          cg_ipes <- mapM (toCgIPE platform ctx) ents
          return (cg_ipes, unit_id, module_name)

        tables :: [CmmStatic]
        tables = map (CmmStaticLit . CmmLabel . ipeInfoTablePtr) cg_ipes

        uncompressed_strings :: BS.ByteString
        uncompressed_strings = getStringTableStrings strtab

        strings_bytes :: BS.ByteString
        strings_bytes = compress defaultCompressionLevel uncompressed_strings

        strings :: [CmmStatic]
        strings = [CmmString strings_bytes]

        uncompressed_entries :: BS.ByteString
        uncompressed_entries = toIpeBufferEntries (platformByteOrder platform) cg_ipes

        entries_bytes :: BS.ByteString
        entries_bytes = compress defaultCompressionLevel uncompressed_entries

        entries :: [CmmStatic]
        entries = [CmmString entries_bytes]

        ipe_buffer_lbl :: CLabel
        ipe_buffer_lbl = mkIPELabel this_mod

        ipe_buffer_node :: [CmmStatic]
        ipe_buffer_node = map CmmStaticLit
          [ -- 'next' field
            zeroCLit platform

            -- 'compressed' field
          , int do_compress

            -- 'count' field
          , int $ length cg_ipes

            -- 'tables' field
          , CmmLabel tables_lbl

            -- 'entries' field
          , CmmLabel entries_lbl

            -- 'entries_size' field (decompressed size)
          , int $ BS.length uncompressed_entries

            -- 'string_table' field
          , CmmLabel strings_lbl

            -- 'string_table_size' field (decompressed size)
          , int $ BS.length uncompressed_strings

            -- 'module_name' field
          , CmmInt (fromIntegral module_name) W32

            -- 'unit_id' field
          , CmmInt (fromIntegral unit_id) W32
          ]

    -- Emit the list of info table pointers
    emitDecl $ CmmData
      (Section Data tables_lbl)
      (CmmStaticsRaw tables_lbl tables)

    -- Emit the strings table
    emitDecl $ CmmData
      (Section Data strings_lbl)
      (CmmStaticsRaw strings_lbl strings)

    -- Emit the list of IPE buffer entries
    emitDecl $ CmmData
      (Section Data entries_lbl)
      (CmmStaticsRaw entries_lbl entries)

    -- Emit the IPE buffer list node
    emitDecl $ CmmData
      (Section Data ipe_buffer_lbl)
      (CmmStaticsRaw ipe_buffer_lbl ipe_buffer_node)

-- | Emit the fields of an IpeBufferEntry struct for each entry in a given list.
toIpeBufferEntries ::
     ByteOrder       -- ^ Byte order to write the data in
  -> [CgInfoProvEnt] -- ^ List of IPE buffer entries
  -> BS.ByteString
toIpeBufferEntries byte_order cg_ipes =
      BSL.toStrict . BSB.toLazyByteString . mconcat
    $ map (mconcat . map word32Builder . to_ipe_buf_ent) cg_ipes
  where
    to_ipe_buf_ent :: CgInfoProvEnt -> [Word32]
    to_ipe_buf_ent cg_ipe =
      [ ipeTableName cg_ipe
      , fromIntegral $ ipeClosureDesc cg_ipe
      , ipeTypeDesc cg_ipe
      , ipeLabel cg_ipe
      , ipeSrcFile cg_ipe
      , ipeSrcSpan cg_ipe
      ]

    word32Builder :: Word32 -> BSB.Builder
    word32Builder = case byte_order of
      BigEndian    -> BSB.word32BE
      LittleEndian -> BSB.word32LE

toCgIPE :: Platform -> SDocContext -> InfoProvEnt -> State StringTable CgInfoProvEnt
toCgIPE platform ctx ipe = do
    table_name <- lookupStringTable $ ST.pack $ renderWithContext ctx (pprCLabel platform (infoTablePtr ipe))
    type_desc <- lookupStringTable $ ST.pack $ infoTableType ipe
    let label_str = maybe "" ((\(LexicalFastString s) -> unpackFS s) . snd) (infoTableProv ipe)
    let (src_loc_file, src_loc_span) =
            case infoTableProv ipe of
              Nothing -> (mempty, "")
              Just (span, _) ->
                  let file = fastStringToShortText $ srcSpanFile span
                      coords = renderWithContext ctx (pprUserRealSpan False span)
                  in (file, coords)
    label    <- lookupStringTable $ ST.pack label_str
    src_file <- lookupStringTable src_loc_file
    src_span <- lookupStringTable $ ST.pack src_loc_span
    return $ CgInfoProvEnt { ipeInfoTablePtr = infoTablePtr ipe
                           , ipeTableName = table_name
                           , ipeClosureDesc = fromIntegral (infoProvEntClosureType ipe)
                           , ipeTypeDesc = type_desc
                           , ipeLabel = label
                           , ipeSrcFile = src_file
                           , ipeSrcSpan = src_span
                           }

data CgInfoProvEnt = CgInfoProvEnt
                               { ipeInfoTablePtr :: !CLabel
                               , ipeTableName :: !StrTabOffset
                               , ipeClosureDesc :: !Word32
                               , ipeTypeDesc :: !StrTabOffset
                               , ipeLabel :: !StrTabOffset
                               , ipeSrcFile :: !StrTabOffset
                               , ipeSrcSpan :: !StrTabOffset
                               }

data StringTable = StringTable { stStrings :: DList ShortText
                               , stLength :: !Int
                               , stLookup :: !(M.Map ShortText StrTabOffset)
                               }

type StrTabOffset = Word32

emptyStringTable :: StringTable
emptyStringTable =
    StringTable { stStrings = emptyDList
                , stLength = 0
                , stLookup = M.empty
                }

getStringTableStrings :: StringTable -> BS.ByteString
getStringTableStrings st =
    BSL.toStrict $ BSB.toLazyByteString
    $ foldMap f $ dlistToList (stStrings st)
  where
    f x = BSB.shortByteString (ST.contents x) `mappend` BSB.word8 0

lookupStringTable :: ShortText -> State StringTable StrTabOffset
lookupStringTable str = state $ \st ->
    case M.lookup str (stLookup st) of
      Just off -> (off, st)
      Nothing ->
          let !st' = st { stStrings = stStrings st `snoc` str
                        , stLength  = stLength st + ST.byteLength str + 1
                        , stLookup  = M.insert str res (stLookup st)
                        }
              res = fromIntegral (stLength st)
          in (res, st')

do_compress :: Int
compress    :: Int -> BS.ByteString -> BS.ByteString
#if !defined(HAVE_LIBZSTD)
do_compress   = 0
compress _ bs = bs
#else
do_compress = 1

compress clvl (BSI.PS srcForeignPtr off len) = unsafePerformIO $
    withForeignPtr srcForeignPtr $ \srcPtr -> do
      maxCompressedSize <- zstd_compress_bound $ fromIntegral len
      dstForeignPtr <- BSI.mallocByteString (fromIntegral maxCompressedSize)
      withForeignPtr dstForeignPtr $ \dstPtr -> do
        compressedSize <- fromIntegral <$>
          zstd_compress
            dstPtr
            maxCompressedSize
            (srcPtr `plusPtr` off)
            (fromIntegral len)
            (fromIntegral clvl)
        BSI.create compressedSize $ \p -> copyBytes p dstPtr compressedSize

foreign import ccall unsafe "ZSTD_compress"
    zstd_compress ::
         Ptr dst -- ^ Destination buffer
      -> CSize   -- ^ Capacity of destination buffer
      -> Ptr src -- ^ Source buffer
      -> CSize   -- ^ Size of source buffer
      -> CInt    -- ^ Compression level
      -> IO CSize

-- | Compute the maximum compressed size for a given source buffer size
foreign import ccall unsafe "ZSTD_compressBound"
    zstd_compress_bound ::
         CSize -- ^ Size of source buffer
      -> IO CSize
#endif

defaultCompressionLevel :: Int
defaultCompressionLevel = 3

newtype DList a = DList ([a] -> [a])

emptyDList :: DList a
emptyDList = DList id

snoc :: DList a -> a -> DList a
snoc (DList f) x = DList (f . (x:))

dlistToList :: DList a -> [a]
dlistToList (DList f) = f []
