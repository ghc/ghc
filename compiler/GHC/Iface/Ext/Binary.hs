{-
Binary serialization for .hie files.
-}

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
   )
where

import GHC.Settings.Utils         ( maybeRead )
import GHC.Settings.Config        ( cProjectVersion )
import GHC.Prelude
import GHC.Utils.Binary
import GHC.Data.FastMutInt
import GHC.Data.FastString        ( FastString )
import GHC.Types.Name
import GHC.Types.Name.Cache
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Builtin.Utils
import GHC.Types.SrcLoc as SrcLoc
import GHC.Types.Unique
import GHC.Types.Unique.FM

import qualified Data.Array        as A
import qualified Data.Array.IO     as A
import qualified Data.Array.Unsafe as A
import Data.IORef
import Data.ByteString            ( ByteString )
import qualified Data.ByteString  as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Word                  ( Word8, Word32 )
import Control.Monad              ( replicateM, when, forM_, foldM )
import System.Directory           ( createDirectoryIfMissing )
import System.FilePath            ( takeDirectory )

import GHC.Iface.Ext.Types
import GHC.Iface.Binary (initWriteIfaceType, putAllTables, initReadIfaceTypeTable, CompressionIFace)
import GHC.Iface.Type (IfaceType)
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified GHC.Utils.Binary as Binary

data HieSymbolTable = HieSymbolTable
  { hie_symtab_next :: !FastMutInt
  , hie_symtab_map  :: !(IORef (UniqFM Name (Int, HieName)))
  }

initBinMemSize :: Int
initBinMemSize = 1024*1024

-- | The header for HIE files - Capital ASCII letters \"HIE\".
hieMagic :: [Word8]
hieMagic = [72,73,69]

hieMagicLen :: Int
hieMagicLen = length hieMagic

ghcVersion :: ByteString
ghcVersion = BSC.pack cProjectVersion

putBinLine :: WriteBinHandle -> ByteString -> IO ()
putBinLine bh xs = do
  mapM_ (putByte bh) $ BS.unpack xs
  putByte bh 10 -- newline char

-- | Write a `HieFile` to the given `FilePath`, with a proper header and
-- symbol tables for `Name`s and `FastString`s
writeHieFile :: CompressionIFace -> FilePath -> HieFile -> IO ()
writeHieFile compression hie_file_path hiefile = do
  bh0 <- openBinMem initBinMemSize

  -- Write the header: hieHeader followed by the
  -- hieVersion and the GHC version used to generate this file
  mapM_ (putByte bh0) hieMagic
  putBinLine bh0 $ BSC.pack $ show hieVersion
  putBinLine bh0 $ ghcVersion

  (fs_tbl, fs_w) <- initFastStringWriterTable
  (name_tbl, name_w) <- initWriteNameTable
  (iface_tbl, iface_w) <- initWriteIfaceType compression

  let bh = setWriterUserData bh0 $ mkWriterUserData
        [ mkSomeBinaryWriter @IfaceType iface_w
        , mkSomeBinaryWriter @Name name_w
        , mkSomeBinaryWriter @BindingName (simpleBindingNameWriter name_w)
        , mkSomeBinaryWriter @FastString fs_w
        ]

  -- Discard number of written elements
  -- Order matters! See Note [Order of deduplication tables during iface binary serialisation]
  _ <- putAllTables bh [fs_tbl, name_tbl, iface_tbl] $ do
    put_ bh hiefile

  -- and send the result to the file
  createDirectoryIfMissing True (takeDirectory hie_file_path)
  writeBinMem bh hie_file_path
  return ()

initWriteNameTable :: IO (WriterTable, BinaryWriter Name)
initWriteNameTable = do
  symtab_next <- newFastMutInt 0
  symtab_map <- newIORef emptyUFM
  let bin_symtab =
        HieSymbolTable
          { hie_symtab_next = symtab_next
          , hie_symtab_map = symtab_map
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

initReadNameTable :: NameCache -> IO (ReaderTable Name)
initReadNameTable cache = do
  return $
    ReaderTable
      { getTable = \bh -> getSymbolTable bh cache
      , mkReaderFromTable = \tbl -> mkReader (getSymTabName tbl)
      }

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
readHieFileWithVersion :: (HieHeader -> Bool) -> NameCache -> FilePath -> IO (Either HieHeader HieFileResult)
readHieFileWithVersion readVersion name_cache file = do
  bh0 <- readBinMem file

  (hieVersion, ghcVersion) <- readHieFileHeader file bh0

  if readVersion (hieVersion, ghcVersion)
  then do
    hieFile <- readHieFileContents bh0 name_cache
    return $ Right (HieFileResult hieVersion ghcVersion hieFile)
  else return $ Left (hieVersion, ghcVersion)


-- | Read a `HieFile` from a `FilePath`. Can use
-- an existing `NameCache`.
readHieFile :: NameCache -> FilePath -> IO HieFileResult
readHieFile name_cache file = do

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
  hieFile <- readHieFileContents bh0 name_cache
  return $ HieFileResult hieVersion ghcVersion hieFile

readBinLine :: ReadBinHandle -> IO ByteString
readBinLine bh = BS.pack . reverse <$> loop []
  where
    loop acc = do
      char <- get bh :: IO Word8
      if char == 10 -- ASCII newline '\n'
      then return acc
      else loop (char : acc)

readHieFileHeader :: FilePath -> ReadBinHandle -> IO HieHeader
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

readHieFileContents :: ReadBinHandle -> NameCache -> IO HieFile
readHieFileContents bh0 name_cache = do
  bhRef <- newIORef (error "used too soon")
  -- It is important this is passed to 'getTable'
  ud <- unsafeInterleaveIO (readIORef bhRef)

  fsReaderTable <- initFastStringReaderTable
  nameReaderTable <- initReadNameTable name_cache
  ifaceTypeReaderTable <- initReadIfaceTypeTable ud

  -- read the symbol table so we are capable of reading the actual data
  bh1 <-
    foldM (\bh tblReader -> tblReader bh) bh0
      [ get_dictionary fsReaderTable
      , get_dictionary nameReaderTable
      , get_dictionary ifaceTypeReaderTable
      ]

  -- load the actual data
  get bh1
  where
    get_dictionary tbl bin_handle = do
      fsTable <- Binary.forwardGetRel bin_handle (getTable tbl bin_handle)
      let
        fsReader = mkReaderFromTable tbl fsTable
        bhFs = addReaderToUserData fsReader bin_handle
      pure bhFs


putSymbolTable :: WriteBinHandle -> Int -> UniqFM Name (Int,HieName) -> IO ()
putSymbolTable bh next_off symtab = do
  put_ bh next_off
  let names = A.elems (A.array (0,next_off-1) (nonDetEltsUFM symtab))
  mapM_ (putHieName bh) names

getSymbolTable :: ReadBinHandle -> NameCache -> IO (SymbolTable Name)
getSymbolTable bh name_cache = do
  sz <- get bh
  mut_arr <- A.newArray_ (0, sz-1) :: IO (A.IOArray Int Name)
  forM_ [0..(sz-1)] $ \i -> do
    od_name <- getHieName bh
    name <- fromHieName name_cache od_name
    A.writeArray mut_arr i name
  A.unsafeFreeze mut_arr

getSymTabName :: SymbolTable Name -> ReadBinHandle -> IO Name
getSymTabName st bh = do
  i :: Word32 <- get bh
  return $ st A.! (fromIntegral i)

putName :: HieSymbolTable -> WriteBinHandle -> Name -> IO ()
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

fromHieName :: NameCache -> HieName -> IO Name
fromHieName nc hie_name = do

  case hie_name of
    ExternalName mod occ span -> updateNameCache nc mod occ $ \cache -> do
      case lookupOrigNameCache cache mod occ of
        Just name -> pure (cache, name)
        Nothing   -> do
          uniq <- takeUniqFromNameCache nc
          let name       = mkExternalName uniq mod occ span
              new_cache  = extendOrigNameCache cache mod occ name
          pure (new_cache, name)

    LocalName occ span -> do
      uniq <- takeUniqFromNameCache nc
      -- don't update the NameCache for local names
      pure $ mkInternalName uniq occ span

    KnownKeyName u -> case lookupKnownKeyName u of
      Nothing -> pprPanic "fromHieName:unknown known-key unique"
                          (ppr u)
      Just n -> pure n

-- ** Reading and writing `HieName`'s

putHieName :: WriteBinHandle -> HieName -> IO ()
putHieName bh (ExternalName mod occ span) = do
  putByte bh 0
  put_ bh (mod, occ, BinSrcSpan span)
putHieName bh (LocalName occName span) = do
  putByte bh 1
  put_ bh (occName, BinSrcSpan span)
putHieName bh (KnownKeyName uniq) = do
  putByte bh 2
  put_ bh $ unpkUnique uniq

getHieName :: ReadBinHandle -> IO HieName
getHieName bh = do
  t <- getByte bh
  case t of
    0 -> do
      (modu, occ, span) <- get bh
      return $ ExternalName modu occ $ unBinSrcSpan span
    1 -> do
      (occ, span) <- get bh
      return $ LocalName occ $ unBinSrcSpan span
    2 -> do
      (c,i) <- get bh
      return $ KnownKeyName $ mkUnique c i
    _ -> panic "GHC.Iface.Ext.Binary.getHieName: invalid tag"
