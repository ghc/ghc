module GHC.StgToCmm.InfoTableProv (emitIpeBufferListNode) where

import GHC.Prelude
import GHC.Platform
import GHC.Unit.Module
import GHC.Utils.Outputable
import GHC.Types.SrcLoc (pprUserRealSpan, srcSpanFile)
import GHC.Data.FastString (fastStringToShortText, unpackFS, LexicalFastString(..))

import GHC.Cmm
import GHC.Cmm.CLabel
import GHC.Cmm.Utils

import GHC.StgToCmm.Config
import GHC.StgToCmm.Monad

import GHC.Data.ShortText (ShortText)
import qualified GHC.Data.ShortText as ST

import Data.Word (Word32)

import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Strict
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL

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

        (cg_ipes, strtab) = flip runState emptyStringTable $ do
          module_name <- lookupStringTable $ ST.pack $ renderWithContext ctx (ppr this_mod)
          mapM (toCgIPE platform ctx module_name) ents

        tables :: [CmmStatic]
        tables = map (CmmStaticLit . CmmLabel . ipeInfoTablePtr) cg_ipes

        strings_bytes :: BS.ByteString
        strings_bytes = getStringTableStrings strtab

        strings :: [CmmStatic]
        strings = [CmmString strings_bytes]

        entries :: [CmmStatic]
        entries = toIpeBufferEntries cg_ipes

        ipe_buffer_lbl :: CLabel
        ipe_buffer_lbl = mkIPELabel this_mod

        ipe_buffer_node :: [CmmStatic]
        ipe_buffer_node = map CmmStaticLit
          [ -- 'next' field
            zeroCLit platform

            -- 'compressed' field
          , int $ 0 -- hardcoded to 0 for now

            -- 'count' field
          , int $ length cg_ipes

            -- 'tables' field
          , CmmLabel tables_lbl

            -- 'entries' field
          , CmmLabel entries_lbl

            -- 'entries_size' field
          , int (length cg_ipes * 8 * 32)

            -- 'string_table' field
          , CmmLabel strings_lbl

            -- 'string_table_size' field
          , int (BS.length strings_bytes)
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

-- | Emit the fields of an @IpeBufferEntry@ as a list of 'Word32's for each
-- 'CgInfoProvEnt' in the given list
toIpeBufferEntries ::
     [CgInfoProvEnt] -- ^ List of buffer entries to emit
  -> [CmmStatic]
toIpeBufferEntries cg_ipes =
    concatMap (map int32 . to_ipe_buf_ent) cg_ipes
  where
    int32 n = CmmStaticLit $ CmmInt (fromIntegral n) W32

    to_ipe_buf_ent :: CgInfoProvEnt -> [Word32]
    to_ipe_buf_ent cg_ipe =
      [ ipeTableName cg_ipe
      , ipeClosureDesc cg_ipe
      , ipeTypeDesc cg_ipe
      , ipeLabel cg_ipe
      , ipeModuleName cg_ipe
      , ipeSrcFile cg_ipe
      , ipeSrcSpan cg_ipe
      , 0 -- padding
      ]

toCgIPE :: Platform -> SDocContext -> StrTabOffset -> InfoProvEnt -> State StringTable CgInfoProvEnt
toCgIPE platform ctx module_name ipe = do
    table_name <- lookupStringTable $ ST.pack $ renderWithContext ctx (pprCLabel platform (infoTablePtr ipe))
    closure_desc <- lookupStringTable $ ST.pack $ show (infoProvEntClosureType ipe)
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
    src_file <- lookupStringTable $ src_loc_file
    src_span <- lookupStringTable $ ST.pack src_loc_span
    return $ CgInfoProvEnt { ipeInfoTablePtr = infoTablePtr ipe
                           , ipeTableName = table_name
                           , ipeClosureDesc = closure_desc
                           , ipeTypeDesc = type_desc
                           , ipeLabel = label
                           , ipeModuleName = module_name
                           , ipeSrcFile = src_file
                           , ipeSrcSpan = src_span
                           }

data CgInfoProvEnt = CgInfoProvEnt
                               { ipeInfoTablePtr :: !CLabel
                               , ipeTableName :: !StrTabOffset
                               , ipeClosureDesc :: !StrTabOffset
                               , ipeTypeDesc :: !StrTabOffset
                               , ipeLabel :: !StrTabOffset
                               , ipeModuleName :: !StrTabOffset
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

newtype DList a = DList ([a] -> [a])

emptyDList :: DList a
emptyDList = DList id

snoc :: DList a -> a -> DList a
snoc (DList f) x = DList (f . (x:))

dlistToList :: DList a -> [a]
dlistToList (DList f) = f []
