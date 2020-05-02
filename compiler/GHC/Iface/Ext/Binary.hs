{-
Binary serialization for .hie files.
-}
{-# LANGUAGE ScopedTypeVariables #-}
module GHC.Iface.Ext.Binary
   ( readHieFile
   , readHieFileWithVersion
   , HieHeader
   , writeHieFile
   , HieName(..)
   , toHieName
   , HieFileResult(..)
   , hieMagic
   , hieNameOcc
   , NameCacheUpdater(..)
   )
where

import GHC.Settings.Utils         ( maybeRead )

import Config                     ( cProjectVersion )
import GHC.Prelude
import GHC.Utils.Binary
import GHC.Iface.Binary           ( getDictFastString )
import GHC.Data.FastMutInt
import GHC.Data.FastString        ( FastString )
import GHC.Unit.Module            ( Module )
import GHC.Types.Name
import GHC.Types.Name.Cache
import GHC.Utils.Outputable
import GHC.Builtin.Utils
import GHC.Types.SrcLoc as SrcLoc
import GHC.Types.Unique.Supply    ( takeUniqFromSupply )
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Utils.Misc
import GHC.Iface.Env (NameCacheUpdater(..))

import qualified Data.Array as A
import Data.IORef
import Data.ByteString            ( ByteString )
import qualified Data.ByteString  as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List                  ( mapAccumR )
import Data.Word                  ( Word8, Word32 )
import Control.Monad              ( replicateM, when )
import System.Directory           ( createDirectoryIfMissing )
import System.FilePath            ( takeDirectory )

import GHC.Iface.Ext.Types

-- | `Name`'s get converted into `HieName`'s before being written into @.hie@
-- files. See 'toHieName' and 'fromHieName' for logic on how to convert between
-- these two types.
data HieName
  = ExternalName !Module !OccName !SrcSpan
  | LocalName !OccName !SrcSpan
  | KnownKeyName !Unique
  deriving (Eq)

instance Ord HieName where
  compare (ExternalName a b c) (ExternalName d e f) = compare (a,b) (d,e) `thenCmp` SrcLoc.leftmost_smallest c f
    -- TODO (int-index): Perhaps use RealSrcSpan in HieName?
  compare (LocalName a b) (LocalName c d) = compare a c `thenCmp` SrcLoc.leftmost_smallest b d
    -- TODO (int-index): Perhaps use RealSrcSpan in HieName?
  compare (KnownKeyName a) (KnownKeyName b) = nonDetCmpUnique a b
    -- Not actually non deterministic as it is a KnownKey
  compare ExternalName{} _ = LT
  compare LocalName{} ExternalName{} = GT
  compare LocalName{} _ = LT
  compare KnownKeyName{} _ = GT

instance Outputable HieName where
  ppr (ExternalName m n sp) = text "ExternalName" <+> ppr m <+> ppr n <+> ppr sp
  ppr (LocalName n sp) = text "LocalName" <+> ppr n <+> ppr sp
  ppr (KnownKeyName u) = text "KnownKeyName" <+> ppr u

hieNameOcc :: HieName -> OccName
hieNameOcc (ExternalName _ occ _) = occ
hieNameOcc (LocalName occ _) = occ
hieNameOcc (KnownKeyName u) =
  case lookupKnownKeyName u of
    Just n -> nameOccName n
    Nothing -> pprPanic "hieNameOcc:unknown known-key unique"
                        (ppr (unpkUnique u))


data HieSymbolTable = HieSymbolTable
  { hie_symtab_next :: !FastMutInt
  , hie_symtab_map  :: !(IORef (UniqFM (Int, HieName)))
  }

data HieDictionary = HieDictionary
  { hie_dict_next :: !FastMutInt -- The next index to use
  , hie_dict_map  :: !(IORef (UniqFM (Int,FastString))) -- indexed by FastString
  }

initBinMemSize :: Int
initBinMemSize = 1024*1024

-- | The header for HIE files - Capital ASCII letters "HIE".
hieMagic :: [Word8]
hieMagic = [72,73,69]

hieMagicLen :: Int
hieMagicLen = length hieMagic

ghcVersion :: ByteString
ghcVersion = BSC.pack cProjectVersion

putBinLine :: BinHandle -> ByteString -> IO ()
putBinLine bh xs = do
  mapM_ (putByte bh) $ BS.unpack xs
  putByte bh 10 -- newline char

-- | Write a `HieFile` to the given `FilePath`, with a proper header and
-- symbol tables for `Name`s and `FastString`s
writeHieFile :: FilePath -> HieFile -> IO ()
writeHieFile hie_file_path hiefile = do
  bh0 <- openBinMem initBinMemSize

  -- Write the header: hieHeader followed by the
  -- hieVersion and the GHC version used to generate this file
  mapM_ (putByte bh0) hieMagic
  putBinLine bh0 $ BSC.pack $ show hieVersion
  putBinLine bh0 $ ghcVersion

  -- remember where the dictionary pointer will go
  dict_p_p <- tellBin bh0
  put_ bh0 dict_p_p

  -- remember where the symbol table pointer will go
  symtab_p_p <- tellBin bh0
  put_ bh0 symtab_p_p

  -- Make some initial state
  symtab_next <- newFastMutInt
  writeFastMutInt symtab_next 0
  symtab_map <- newIORef emptyUFM
  let hie_symtab = HieSymbolTable {
                      hie_symtab_next = symtab_next,
                      hie_symtab_map  = symtab_map }
  dict_next_ref <- newFastMutInt
  writeFastMutInt dict_next_ref 0
  dict_map_ref <- newIORef emptyUFM
  let hie_dict = HieDictionary {
                      hie_dict_next = dict_next_ref,
                      hie_dict_map  = dict_map_ref }

  -- put the main thing
  let bh = setUserData bh0 $ newWriteState (putName hie_symtab)
                                           (putName hie_symtab)
                                           (putFastString hie_dict)
  put_ bh hiefile

  -- write the symtab pointer at the front of the file
  symtab_p <- tellBin bh
  putAt bh symtab_p_p symtab_p
  seekBin bh symtab_p

  -- write the symbol table itself
  symtab_next' <- readFastMutInt symtab_next
  symtab_map'  <- readIORef symtab_map
  putSymbolTable bh symtab_next' symtab_map'

  -- write the dictionary pointer at the front of the file
  dict_p <- tellBin bh
  putAt bh dict_p_p dict_p
  seekBin bh dict_p

  -- write the dictionary itself
  dict_next <- readFastMutInt dict_next_ref
  dict_map  <- readIORef dict_map_ref
  putDictionary bh dict_next dict_map

  -- and send the result to the file
  createDirectoryIfMissing True (takeDirectory hie_file_path)
  writeBinMem bh hie_file_path
  return ()

data HieFileResult
  = HieFileResult
  { hie_file_result_version :: Integer
  , hie_file_result_ghc_version :: ByteString
  , hie_file_result :: HieFile
  }

type HieHeader = (Integer, ByteString)

-- | Read a `HieFile` from a `FilePath`. Can use
-- an existing `NameCache`. Allows you to specify
-- which versions of hieFile to attempt to read.
-- `Left` case returns the failing header versions.
readHieFileWithVersion :: (HieHeader -> Bool) -> NameCacheUpdater -> FilePath -> IO (Either HieHeader HieFileResult)
readHieFileWithVersion readVersion ncu file = do
  bh0 <- readBinMem file

  (hieVersion, ghcVersion) <- readHieFileHeader file bh0

  if readVersion (hieVersion, ghcVersion)
  then do
    hieFile <- readHieFileContents bh0 ncu
    return $ Right (HieFileResult hieVersion ghcVersion hieFile)
  else return $ Left (hieVersion, ghcVersion)


-- | Read a `HieFile` from a `FilePath`. Can use
-- an existing `NameCache`.
readHieFile :: NameCacheUpdater -> FilePath -> IO HieFileResult
readHieFile ncu file = do

  bh0 <- readBinMem file

  (readHieVersion, ghcVersion) <- readHieFileHeader file bh0

  -- Check if the versions match
  when (readHieVersion /= hieVersion) $
    panic $ unwords ["readHieFile: hie file versions don't match for file:"
                    , file
                    , "Expected"
                    , show hieVersion
                    , "but got", show readHieVersion
                    ]
  hieFile <- readHieFileContents bh0 ncu
  return $ HieFileResult hieVersion ghcVersion hieFile

readBinLine :: BinHandle -> IO ByteString
readBinLine bh = BS.pack . reverse <$> loop []
  where
    loop acc = do
      char <- get bh :: IO Word8
      if char == 10 -- ASCII newline '\n'
      then return acc
      else loop (char : acc)

readHieFileHeader :: FilePath -> BinHandle -> IO HieHeader
readHieFileHeader file bh0 = do
  -- Read the header
  magic <- replicateM hieMagicLen (get bh0)
  version <- BSC.unpack <$> readBinLine bh0
  case maybeRead version of
    Nothing ->
      panic $ unwords ["readHieFileHeader: hieVersion isn't an Integer:"
                      , show version
                      ]
    Just readHieVersion -> do
      ghcVersion <- readBinLine bh0

      -- Check if the header is valid
      when (magic /= hieMagic) $
        panic $ unwords ["readHieFileHeader: headers don't match for file:"
                        , file
                        , "Expected"
                        , show hieMagic
                        , "but got", show magic
                        ]
      return (readHieVersion, ghcVersion)

readHieFileContents :: BinHandle -> NameCacheUpdater -> IO HieFile
readHieFileContents bh0 ncu = do

  dict  <- get_dictionary bh0

  -- read the symbol table so we are capable of reading the actual data
  bh1 <- do
      let bh1 = setUserData bh0 $ newReadState (error "getSymtabName")
                                               (getDictFastString dict)
      symtab <- get_symbol_table bh1
      let bh1' = setUserData bh1
               $ newReadState (getSymTabName symtab)
                              (getDictFastString dict)
      return bh1'

  -- load the actual data
  hiefile <- get bh1
  return hiefile
  where
    get_dictionary bin_handle = do
      dict_p <- get bin_handle
      data_p <- tellBin bin_handle
      seekBin bin_handle dict_p
      dict <- getDictionary bin_handle
      seekBin bin_handle data_p
      return dict

    get_symbol_table bh1 = do
      symtab_p <- get bh1
      data_p'  <- tellBin bh1
      seekBin bh1 symtab_p
      symtab <- getSymbolTable bh1 ncu
      seekBin bh1 data_p'
      return symtab

putFastString :: HieDictionary -> BinHandle -> FastString -> IO ()
putFastString HieDictionary { hie_dict_next = j_r,
                              hie_dict_map  = out_r}  bh f
  = do
    out <- readIORef out_r
    let unique = getUnique f
    case lookupUFM out unique of
        Just (j, _)  -> put_ bh (fromIntegral j :: Word32)
        Nothing -> do
           j <- readFastMutInt j_r
           put_ bh (fromIntegral j :: Word32)
           writeFastMutInt j_r (j + 1)
           writeIORef out_r $! addToUFM out unique (j, f)

putSymbolTable :: BinHandle -> Int -> UniqFM (Int,HieName) -> IO ()
putSymbolTable bh next_off symtab = do
  put_ bh next_off
  let names = A.elems (A.array (0,next_off-1) (nonDetEltsUFM symtab))
  mapM_ (putHieName bh) names

getSymbolTable :: BinHandle -> NameCacheUpdater -> IO SymbolTable
getSymbolTable bh ncu = do
  sz <- get bh
  od_names <- replicateM sz (getHieName bh)
  updateNameCache ncu $ \nc ->
    let arr = A.listArray (0,sz-1) names
        (nc', names) = mapAccumR fromHieName nc od_names
        in (nc',arr)

getSymTabName :: SymbolTable -> BinHandle -> IO Name
getSymTabName st bh = do
  i :: Word32 <- get bh
  return $ st A.! (fromIntegral i)

putName :: HieSymbolTable -> BinHandle -> Name -> IO ()
putName (HieSymbolTable next ref) bh name = do
  symmap <- readIORef ref
  case lookupUFM symmap name of
    Just (off, ExternalName mod occ (UnhelpfulSpan _))
      | isGoodSrcSpan (nameSrcSpan name) -> do
      let hieName = ExternalName mod occ (nameSrcSpan name)
      writeIORef ref $! addToUFM symmap name (off, hieName)
      put_ bh (fromIntegral off :: Word32)
    Just (off, LocalName _occ span)
      | notLocal (toHieName name) || nameSrcSpan name /= span -> do
      writeIORef ref $! addToUFM symmap name (off, toHieName name)
      put_ bh (fromIntegral off :: Word32)
    Just (off, _) -> put_ bh (fromIntegral off :: Word32)
    Nothing -> do
        off <- readFastMutInt next
        writeFastMutInt next (off+1)
        writeIORef ref $! addToUFM symmap name (off, toHieName name)
        put_ bh (fromIntegral off :: Word32)

  where
    notLocal :: HieName -> Bool
    notLocal LocalName{} = False
    notLocal _ = True


-- ** Converting to and from `HieName`'s

toHieName :: Name -> HieName
toHieName name
  | isKnownKeyName name = KnownKeyName (nameUnique name)
  | isExternalName name = ExternalName (nameModule name)
                                       (nameOccName name)
                                       (nameSrcSpan name)
  | otherwise = LocalName (nameOccName name) (nameSrcSpan name)

fromHieName :: NameCache -> HieName -> (NameCache, Name)
fromHieName nc (ExternalName mod occ span) =
    let cache = nsNames nc
    in case lookupOrigNameCache cache mod occ of
         Just name -> (nc, name)
         Nothing ->
           let (uniq, us) = takeUniqFromSupply (nsUniqs nc)
               name       = mkExternalName uniq mod occ span
               new_cache  = extendNameCache cache mod occ name
           in ( nc{ nsUniqs = us, nsNames = new_cache }, name )
fromHieName nc (LocalName occ span) =
    let (uniq, us) = takeUniqFromSupply (nsUniqs nc)
        name       = mkInternalName uniq occ span
    in ( nc{ nsUniqs = us }, name )
fromHieName nc (KnownKeyName u) = case lookupKnownKeyName u of
    Nothing -> pprPanic "fromHieName:unknown known-key unique"
                        (ppr (unpkUnique u))
    Just n -> (nc, n)

-- ** Reading and writing `HieName`'s

putHieName :: BinHandle -> HieName -> IO ()
putHieName bh (ExternalName mod occ span) = do
  putByte bh 0
  put_ bh (mod, occ, span)
putHieName bh (LocalName occName span) = do
  putByte bh 1
  put_ bh (occName, span)
putHieName bh (KnownKeyName uniq) = do
  putByte bh 2
  put_ bh $ unpkUnique uniq

getHieName :: BinHandle -> IO HieName
getHieName bh = do
  t <- getByte bh
  case t of
    0 -> do
      (modu, occ, span) <- get bh
      return $ ExternalName modu occ span
    1 -> do
      (occ, span) <- get bh
      return $ LocalName occ span
    2 -> do
      (c,i) <- get bh
      return $ KnownKeyName $ mkUnique c i
    _ -> panic "GHC.Iface.Ext.Binary.getHieName: invalid tag"
