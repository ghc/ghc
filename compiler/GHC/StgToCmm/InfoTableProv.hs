{-# LANGUAGE CPP #-}

module GHC.StgToCmm.InfoTableProv (emitIpeBufferListNode) where

import Foreign

#if defined(HAVE_LIBZSTD)
import Foreign.C.Types
import qualified Data.ByteString.Internal as BSI
import GHC.IO (unsafePerformIO)
#endif

import Data.Char
import Data.Foldable
import GHC.Prelude
import GHC.Platform
import GHC.Types.SrcLoc (pprUserRealSpan, srcSpanFile)
import GHC.Types.Unique.DSM
import GHC.Unit.Module
import GHC.Utils.Outputable
import GHC.Data.FastString (fastStringToShortText, unpackFS, LexicalFastString(..))
import GHC.Data.OrdList (OrdList, nilOL, snocOL)

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

Note [IPE Stripping and magic words]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For systems which support ELF executables:

The metadata part of IPE info is placed into a separate ELF section (.ipe).
This can then be stripped afterwards if you don't require the metadata

```
-- Remove the section
objcopy --remove-section .ipe <your-exe>
-- Repack and compress the executable
upx <your-exe>
```

The .ipe section starts with a magic 64-bit word "IPE\0IPE\0`, encoded as ascii.

The RTS checks to see if the .ipe section starts with the magic word. If the
section has been stripped then it won't start with the magic word and the
metadata won't be accessible for the info tables.

-}

emitIpeBufferListNode ::
     Module
  -> [InfoProvEnt]
  -> DUniqSupply -- ^ Symbols created source uniques deterministically
                 -- All uniques must be created from this supply.
                 -- NB: If you are creating a new symbol within this function,
                 -- make sure it is local only (as in not `externallyVisibleCLabel`).
                 -- If you need it to be global, reconsider the comment on the
                 -- call of emitIpeBufferListNode in Cmm.Parser.
  -> FCode DUniqSupply
emitIpeBufferListNode _ [] dus = return dus
emitIpeBufferListNode this_mod ents dus0 = do
    cfg <- getStgToCmmConfig

    let (u1, dus1) = takeUniqueFromDSupply dus0
        (u2, dus2) = takeUniqueFromDSupply dus1
        (u3, dus3) = takeUniqueFromDSupply dus2

        tables_lbl  = mkStringLitLabel u1
        strings_lbl = mkStringLitLabel u2
        entries_lbl = mkStringLitLabel u3

        ctx      = stgToCmmContext cfg
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
        strings = [CmmString (ipe_header `mappend` strings_bytes)]

        uncompressed_entries :: BS.ByteString
        uncompressed_entries = toIpeBufferEntries (platformByteOrder platform) cg_ipes

        entries_bytes :: BS.ByteString
        entries_bytes = compress defaultCompressionLevel uncompressed_entries

        entries :: [CmmStatic]
        entries = [CmmString (ipe_header `mappend` entries_bytes)]

        ipe_buffer_lbl :: CLabel
        ipe_buffer_lbl = mkIPELabel this_mod

        -- A string which fits into one 64-bit word.
        ipe_header_word :: Word64
        ipe_header_word = stringToWord64BE "IPE\0IPE\0"

        -- Convert 8 bytes to Word64 using big-endian interpretation
        stringToWord64BE :: String -> Word64
        stringToWord64BE = foldl' (\acc b -> GHC.Prelude.shiftL acc 8 .|. fromIntegral (ord b)) 0

        -- A magic word we can use to see if the IPE information has been stripped
        -- or not
        -- See Note [IPE Stripping and magic words]
        -- When read then literally the string should read IPE\0IPE\0 in hex dumps.
        --
        -- There is some complexity here to turn this into a ByteString rather than
        -- a simpler CmmStaticLit, since the unregistered backend does not cope well
        -- with CmmStaticsRaw being a mixure of CmmStaticLit and CmmString.
        ipe_header :: BS.ByteString
        ipe_header = BSL.toStrict . BSB.toLazyByteString $
                       case platformByteOrder platform of
                         LittleEndian -> BSB.word64LE ipe_header_word
                         BigEndian -> BSB.word64BE ipe_header_word


        ipe_buffer_node :: [CmmStatic]
        ipe_buffer_node = map CmmStaticLit
          [ -- 'next' field
            zeroCLit platform

            -- 'node_id' field
          , zeroCLit platform

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
      (Section IPE strings_lbl)
      (CmmStaticsRaw strings_lbl strings)

    -- Emit the list of IPE buffer entries
    emitDecl $ CmmData
      (Section IPE entries_lbl)
      (CmmStaticsRaw entries_lbl entries)

    -- Emit the IPE buffer list node
    emitDecl $ CmmData
      (Section Data ipe_buffer_lbl)
      (CmmStaticsRaw ipe_buffer_lbl ipe_buffer_node)

    return dus3

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

data StringTable = StringTable { stStrings :: !(OrdList ShortText)
                               , stLength :: !Int
                               , stLookup :: !(M.Map ShortText StrTabOffset)
                               }

type StrTabOffset = Word32

emptyStringTable :: StringTable
emptyStringTable =
    StringTable { stStrings = nilOL
                , stLength = 0
                , stLookup = M.empty
                }

getStringTableStrings :: StringTable -> BS.ByteString
getStringTableStrings st =
    BSL.toStrict $ BSB.toLazyByteString
    $ foldMap' f $ stStrings st
  where
    f x = BSB.shortByteString (ST.contents x) `mappend` BSB.word8 0

lookupStringTable :: ShortText -> State StringTable StrTabOffset
lookupStringTable str = state $ \st ->
    case M.lookup str (stLookup st) of
      Just off -> (off, st)
      Nothing ->
          let !st' = st { stStrings = stStrings st `snocOL` str
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
