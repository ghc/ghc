module GHC.StgToCmm.InfoTableProv (emitIpeBufferListNode) where

import GHC.Prelude
import GHC.Platform
import GHC.Unit.Module
import GHC.Utils.Outputable
import GHC.Types.SrcLoc (pprUserRealSpan, srcSpanFile)
import GHC.Data.FastString (fastStringToShortText)

import GHC.Cmm.CLabel
import GHC.Cmm.Expr
import GHC.Cmm.Utils
import GHC.StgToCmm.Config
import GHC.StgToCmm.Lit (newByteStringCLit)
import GHC.StgToCmm.Monad
import GHC.StgToCmm.Utils

import GHC.Data.ShortText (ShortText)
import qualified GHC.Data.ShortText as ST

import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Strict
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL

emitIpeBufferListNode :: Module
                      -> [InfoProvEnt]
                      -> FCode ()
emitIpeBufferListNode _ [] = return ()
emitIpeBufferListNode this_mod ents = do
    cfg <- getStgToCmmConfig
    let ctx      = stgToCmmContext  cfg
        platform = stgToCmmPlatform cfg

    let (cg_ipes, strtab) = flip runState emptyStringTable $ do
            module_name <- lookupStringTable $ ST.pack $ renderWithContext ctx (ppr this_mod)
            mapM (toCgIPE platform ctx module_name) ents

    let -- Emit the fields of an IpeBufferEntry struct.
        toIpeBufferEntry :: CgInfoProvEnt -> [CmmLit]
        toIpeBufferEntry cg_ipe =
            [ CmmLabel (ipeInfoTablePtr cg_ipe)
            , strtab_offset (ipeTableName cg_ipe)
            , strtab_offset (ipeClosureDesc cg_ipe)
            , strtab_offset (ipeTypeDesc cg_ipe)
            , strtab_offset (ipeLabel cg_ipe)
            , strtab_offset (ipeModuleName cg_ipe)
            , strtab_offset (ipeSrcFile cg_ipe)
            , strtab_offset (ipeSrcSpan cg_ipe)
            , int32 0
            ]

        int n = mkIntCLit platform n
        int32 n = CmmInt n W32
        strtab_offset (StrTabOffset n) = int32 (fromIntegral n)

    strings <- newByteStringCLit (getStringTableStrings strtab)
    let lits = [ zeroCLit platform     -- 'next' field
               , strings               -- 'strings' field
               , int $ length cg_ipes  -- 'count' field
               ] ++ concatMap toIpeBufferEntry cg_ipes
    emitDataLits (mkIPELabel this_mod) lits

toCgIPE :: Platform -> SDocContext -> StrTabOffset -> InfoProvEnt -> State StringTable CgInfoProvEnt
toCgIPE platform ctx module_name ipe = do
    table_name <- lookupStringTable $ ST.pack $ renderWithContext ctx (pprCLabel platform (infoTablePtr ipe))
    closure_desc <- lookupStringTable $ ST.pack $ show (infoProvEntClosureType ipe)
    type_desc <- lookupStringTable $ ST.pack $ infoTableType ipe
    let label_str = maybe "" snd (infoTableProv ipe)
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

newtype StrTabOffset = StrTabOffset Int

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
              res = StrTabOffset (stLength st)
          in (res, st')

newtype DList a = DList ([a] -> [a])

emptyDList :: DList a
emptyDList = DList id

snoc :: DList a -> a -> DList a
snoc (DList f) x = DList (f . (x:))

dlistToList :: DList a -> [a]
dlistToList (DList f) = f []
