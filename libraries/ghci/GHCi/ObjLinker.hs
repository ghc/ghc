{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHCi.ObjLinker
  ( initObjLinker
  , loadDLLs
  , loadArchive
  , loadObj
  , unloadObj
  , purgeObj
  , lookupSymbol
  , lookupSymbolInDLL
  , resolveObjs
  , addLibrarySearchPath
  , removeLibrarySearchPath
  , findSystemLibrary
  ) where

import Prelude

import Control.Exception
import Control.Concurrent.MVar
import Control.Monad
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Int
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Word
import Data.Array
import Data.Char (isDigit, isSpace)
import Data.List (sortBy, findIndex, isPrefixOf, isInfixOf, isSuffixOf, nub)
import Foreign
import Foreign.C
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>), splitSearchPath, isAbsolute, normalise)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Directory (getWorkingDirectory)
import System.Posix.Files
  ( getFileStatus
  , isDirectory
  , isRegularFile
  )

import GHCi.Message (LoadedDLL)

#if defined(x86_64_HOST_ARCH) && defined(linux_HOST_OS) && !defined(wasm32_HOST_ARCH)

--------------------------------------------------------------------------------
-- Public API (x86_64-linux)
--------------------------------------------------------------------------------

initObjLinker :: Bool -> IO ()
initObjLinker retainCAFs = modifyMVar_ linkerState $ \st -> do
  if lsInitDone st
    then pure st
    else do
      c_initUnloadCheck
      baseSyms <- loadRtsSymbols
      let syms' = updateNewCAF retainCAFs baseSyms
      pure st
        { lsInitDone = True
        , lsRetainCAFs = retainCAFs
        , lsSymbols = syms'
        }

ensureInit :: IO ()
ensureInit = modifyMVar_ linkerState $ \st -> do
  if lsInitDone st
    then pure st
    else do
      c_initUnloadCheck
      baseSyms <- loadRtsSymbols
      let syms' = updateNewCAF False baseSyms
      pure st
        { lsInitDone = True
        , lsRetainCAFs = False
        , lsSymbols = syms'
        }

normalizePath :: FilePath -> IO FilePath
normalizePath path =
  (do
      cwd <- getWorkingDirectory
      let absPath = if isAbsolute path then path else cwd </> path
      pure (normalise absPath)
  ) `catch` \(_ :: SomeException) -> pure path

loadDLLs :: [String] -> IO (Either String [Ptr LoadedDLL])
loadDLLs ps = ensureInit >> do
  r <- foldM step (Right []) ps
  case r of
    Right hs -> do
      modifyMVar_ linkerState $ \st ->
        pure st { lsDlls = hs ++ lsDlls st }
      pure r
    Left _ -> pure r
  where
    step (Left err) _ = pure (Left err)
    step (Right hs) p = do
      r <- loadDLL p
      pure $ case r of
        Left e -> Left e
        Right h -> Right (h:hs)

loadArchive :: String -> IO ()
loadArchive path = do
  ensureInit
  npath <- normalizePath path
  bs <- BS.readFile npath
  members <- parseArchive npath bs
  -- Load all members for now
  objs <- forM (zip [0 :: Int ..] members) $ \(idx, (memberName, memberBytes)) -> do
    let objName = npath <> "(" <> memberName <> "#" <> show idx <> ")"
    loadObjFromBytes objName memberBytes
    pure objName
  modifyMVar_ linkerState $ \st ->
    pure st { lsArchives = Map.insert npath objs (lsArchives st) }

loadObj :: String -> IO ()
loadObj path = do
  ensureInit
  npath <- normalizePath path
  parsed <- getParsedObject npath
  loadParsedObject parsed

unloadObj :: String -> IO ()
unloadObj path = do
  npath <- normalizePath path
  when linkerDebug $
    hPutStrLn stderr ("GHCi.ObjLinker: unloadObj " <> npath)
  modifyMVar_ linkerState $ \st -> do
    case Map.lookup npath (lsArchives st) of
      Just members -> do
        st' <- foldM unloadOne st members
        pure st' { lsArchives = Map.delete npath (lsArchives st') }
      Nothing -> unloadOne st npath
  where
    unloadOne st p =
      case Map.lookup p (lsObjects st) of
        Nothing -> pure st
        Just obj -> do
          runFiniSections obj
          -- remove symbols for this object
          let symMap' = removeSymbolsFor p (lsSymbols st)
          -- free stable pointers associated with foreign exports
          when (loOcPtr obj /= nullPtr) $ do
            c_freeObjectCodeExports (loOcPtr obj)
            c_unloadObject (loOcPtr obj) 0
          let objs' = Map.delete p (lsObjects st)
          pure st { lsObjects = objs', lsSymbols = symMap' }

purgeObj :: String -> IO ()
purgeObj path = modifyMVar_ linkerState $ \st -> do
  npath <- normalizePath path
  case Map.lookup npath (lsArchives st) of
    Just members -> do
      let symMap' = foldl' (\m p -> removeSymbolsFor p m) (lsSymbols st) members
      forM_ members $ \p ->
        case Map.lookup p (lsObjects st) of
          Nothing -> pure ()
          Just obj -> when (loOcPtr obj /= nullPtr) $ do
            c_freeObjectCodeExports (loOcPtr obj)
            c_unloadObject (loOcPtr obj) 1
      pure st { lsSymbols = symMap' }
    Nothing -> do
      let symMap' = removeSymbolsFor npath (lsSymbols st)
      case Map.lookup npath (lsObjects st) of
        Nothing -> pure ()
        Just obj -> when (loOcPtr obj /= nullPtr) $ do
          c_freeObjectCodeExports (loOcPtr obj)
          c_unloadObject (loOcPtr obj) 1
      pure st { lsSymbols = symMap' }

lookupSymbol :: String -> IO (Maybe (Ptr a))
lookupSymbol name = ensureInit >> withMVar linkerState (\st -> do
  case lookupSymbolMap name (lsSymbols st) of
    Just addr -> pure (Just (castPtr addr))
    Nothing -> withCString name $ \cname -> do
      maddr <- lookupInDlls cname (lsDlls st)
      case maddr of
        Just addr -> pure (Just (castPtr addr))
        Nothing -> do
          addr <- c_dlsym_default cname
          if addr == nullPtr
            then do
              optimistic <- linkerOptimistic
              reportMissingSymbol name optimistic
              if optimistic
                then pure (Just (castPtr optimisticSymbolPtr))
                else pure Nothing
            else pure (Just (castPtr addr)))
  where
    lookupInDlls _ [] = pure Nothing
    lookupInDlls cname (h:hs) = do
      addr <- c_dlsym h cname
      if addr == nullPtr
        then lookupInDlls cname hs
        else pure (Just addr)

lookupSymbolInDLL :: Ptr LoadedDLL -> String -> IO (Maybe (Ptr a))
lookupSymbolInDLL h name = withCString name $ \cname -> do
  addr <- c_dlsym h cname
  if addr == nullPtr then pure Nothing else pure (Just (castPtr addr))

resolveObjs :: IO Bool
resolveObjs = modifyMVar linkerState $ \st -> do
  let objs = Map.elems (lsObjects st)
      pending = filter (not . loResolved) objs
  if null pending
    then pure (st, True)
    else do
      result <- resolveAll st pending
      case result of
        Left err -> do
          when linkerDebug $
            hPutStrLn stderr ("GHCi.ObjLinker: " <> err)
          pure (st, False)
        Right resolved -> do
          let objs' = foldl' (\m o -> Map.insert (loPath o) o m) (lsObjects st) resolved
          pure (st { lsObjects = objs' }, True)

addLibrarySearchPath :: String -> IO (Ptr ())
addLibrarySearchPath path = do
  sp <- newStablePtr path
  let key = castStablePtrToPtr sp
  modifyMVar_ librarySearchPaths $ \m ->
    pure (Map.insert key path m)
  pure key

removeLibrarySearchPath :: Ptr () -> IO Bool
removeLibrarySearchPath key =
  modifyMVar librarySearchPaths $ \m ->
    case Map.lookup key m of
      Nothing -> pure (m, False)
      Just _ -> do
        freeStablePtr (castPtrToStablePtr key)
        pure (Map.delete key m, True)

findSystemLibrary :: String -> IO (Maybe String)
findSystemLibrary _ = pure Nothing

--------------------------------------------------------------------------------
-- Linker state
--------------------------------------------------------------------------------

data LinkerState = LinkerState
  { lsInitDone :: !Bool
  , lsRetainCAFs :: !Bool
  , lsObjects :: !(Map FilePath LoadedObject)
  , lsArchives :: !(Map FilePath [FilePath])
  , lsSymbols :: !(Map String [SymbolInfo])
  , lsDlls :: ![Ptr LoadedDLL]
  }

linkerState :: MVar LinkerState
linkerState = unsafePerformIO $ newMVar LinkerState
  { lsInitDone = False
  , lsRetainCAFs = False
  , lsObjects = Map.empty
  , lsArchives = Map.empty
  , lsSymbols = Map.empty
  , lsDlls = []
  }
{-# NOINLINE linkerState #-}

linkerDebug :: Bool
linkerDebug = unsafePerformIO $ do
  env <- lookupEnv "GHCI_LINKER_DEBUG"
  pure $ case env of
    Nothing -> False
    Just "0" -> False
    Just _ -> True
{-# NOINLINE linkerDebug #-}

librarySearchPaths :: MVar (Map (Ptr ()) FilePath)
librarySearchPaths = unsafePerformIO (newMVar Map.empty)
{-# NOINLINE librarySearchPaths #-}

parsedObjectCache :: MVar (Map FilePath ParsedObject)
parsedObjectCache = unsafePerformIO (newMVar Map.empty)
{-# NOINLINE parsedObjectCache #-}

getLibrarySearchPaths :: IO [FilePath]
getLibrarySearchPaths = do
  added <- Map.elems <$> readMVar librarySearchPaths
  env <- lookupEnv "LD_LIBRARY_PATH"
  let envPaths = maybe [] splitSearchPath env
  defaults <- defaultLibraryDirs
  pure (nub (added ++ envPaths ++ defaults))

defaultLibraryDirs :: IO [FilePath]
defaultLibraryDirs = do
  let dirs =
        [ "/lib"
        , "/lib64"
        , "/usr/lib"
        , "/usr/lib64"
        , "/usr/local/lib"
        , "/usr/local/lib64"
        , "/lib/x86_64-linux-gnu"
        , "/usr/lib/x86_64-linux-gnu"
        ]
  filterM doesDirectoryExist dirs

doesFileExist :: FilePath -> IO Bool
doesFileExist path =
  (isRegularFile <$> getFileStatus path) `catch` \(_ :: SomeException) -> pure False

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist path =
  (isDirectory <$> getFileStatus path) `catch` \(_ :: SomeException) -> pure False

--------------------------------------------------------------------------------
-- Symbol table
--------------------------------------------------------------------------------

data SymStrength = SymStrong | SymWeak deriving (Eq, Show)

data SymKind = SymCode | SymData deriving (Eq, Show)

data SymOrigin = OriginRTS | OriginObj FilePath deriving (Eq, Show)

data SymbolInfo = SymbolInfo
  { symAddr :: !(Ptr ())
  , symStrength :: !SymStrength
  , symKind :: !SymKind
  , symHidden :: !Bool
  , symOrigin :: !SymOrigin
  }

lookupSymbolMap :: String -> Map String [SymbolInfo] -> Maybe (Ptr ())
lookupSymbolMap name syms = do
  infos <- Map.lookup name syms
  case infos of
    [] -> Nothing
    (i:_) -> Just (symAddr i)

insertSymbol :: String -> SymbolInfo -> Map String [SymbolInfo] -> Either String (Map String [SymbolInfo])
insertSymbol name info syms =
  Right $ case Map.lookup name syms of
    Nothing -> Map.insert name [info] syms
    Just infos -> Map.insert name (insertByPrecedence info infos) syms
  where
    rank SymbolInfo{symHidden = hidden, symStrength = strength} =
      (if hidden then (1 :: Int) else 0, if strength == SymWeak then (1 :: Int) else 0)

    insertByPrecedence newInfo infos =
      let (higher, lower) = span (\i -> rank i <= rank newInfo) infos
      in higher ++ [newInfo] ++ lower

removeSymbolsFor :: FilePath -> Map String [SymbolInfo] -> Map String [SymbolInfo]
removeSymbolsFor path = Map.mapMaybe dropOrigin
  where
    dropOrigin infos =
      let infos' = filter (\i -> symOrigin i /= OriginObj path) infos
      in if null infos' then Nothing else Just infos'

--------------------------------------------------------------------------------
-- ELF parsing and loading
--------------------------------------------------------------------------------

data ElfHeader = ElfHeader
  { ehMachine :: !Word16
  , ehShoff :: !Word64
  , ehShentsize :: !Word16
  , ehShnum :: !Word16
  , ehShstrndx :: !Word16
  }


data SectionHeader = SectionHeader
  { shName :: !Word32
  , shType :: !Word32
  , shFlags :: !Word64
  , shAddr :: !Word64
  , shOffset :: !Word64
  , shSize :: !Word64
  , shLink :: !Word32
  , shInfo :: !Word32
  , shAddralign :: !Word64
  , shEntsize :: !Word64
  }


data SectionInfo = SectionInfo !String !(Ptr Word8) !Int


data ElfSymbol = ElfSymbol
  { esName :: !String
  , esBind :: !Word8
  , esType :: !Word8
  , esVis :: !Word8
  , esShndx :: !Word16
  , esValue :: !Word64
  , esSize :: !Word64
  }


data RelocEntry = RelocEntry !Word64 !Word64 !Int64


data RelocSection = RelocSection
  { rsTarget :: !Int
  , rsEntries :: ![RelocEntry]
  }


data InitKind
  = InitInit
  | InitFini
  | InitCtors
  | InitDtors
  | InitArray
  | FiniArray
  deriving (Eq, Show)


data InitSection = InitSection !InitKind !Word32 !(Ptr Word8) !Int

type InitFun = CInt -> Ptr CString -> Ptr CString -> IO ()
type FiniFun = IO ()


data LoadedObject = LoadedObject
  { loPath :: !FilePath
  , loMemBase :: !(Ptr Word8)
  , loMemSize :: !Int
  , loSections :: !(Array Int SectionInfo)
  , loSectionKinds :: !(Array Int CInt)
  , loSymbols :: !(Array Int ElfSymbol)
  , loSymAddrs :: !(Array Int (Maybe (Ptr Word8)))
  , loRelocs :: ![RelocSection]
  , loResolved :: !Bool
  , loExtras :: !(Ptr Word8)
  , loFingerprint :: !Word64
  , loOcPtr :: !(Ptr ())
  }

data ParsedObject = ParsedObject
  { poPath :: !FilePath
  , poBytes :: !ByteString
  , poShdrs :: !(Array Int SectionHeader)
  , poSecNames :: !(Array Int String)
  , poSecKinds :: !(Array Int CInt)
  , poSymbols :: !(Array Int ElfSymbol)
  , poRelocs :: ![RelocSection]
  }

loadObjFromBytes :: FilePath -> ByteString -> IO ()
loadObjFromBytes path bs = do
  parsed <- parseElfObject path bs
  loadParsedObject parsed

getParsedObject :: FilePath -> IO ParsedObject
getParsedObject path = do
  bs <- BS.readFile path
  mCached <- withMVar parsedObjectCache (pure . Map.lookup path)
  case mCached of
    Just cached | poBytes cached == bs -> pure cached
    _ -> do
      parsed <- parseElfObject path bs
      modifyMVar_ parsedObjectCache $ \m ->
        pure (Map.insert path parsed m)
      pure parsed

loadParsedObject :: ParsedObject -> IO ()
loadParsedObject parsed = do
  obj <- instantiateObject parsed
  let path = loPath obj
      insertNew st = do
        let (symInsertResult, _exportedNames) = insertObjectSymbols (lsSymbols st) obj
        case symInsertResult of
          Left err -> do
            freeLoadedObject obj
            throwIO (ErrorCall ("loadObj " <> show path <> ": " <> err))
          Right symMap' -> do
            obj' <- registerLoadedObject obj `onException` freeLoadedObject obj
            let objs' = Map.insert path obj' (lsObjects st)
            pure st { lsSymbols = symMap', lsObjects = objs' }
  when linkerDebug $
    hPutStrLn stderr ("GHCi.ObjLinker: loadObj " <> path)
  modifyMVar_ linkerState $ \st -> do
    case Map.lookup path (lsObjects st) of
      Just old
        | loFingerprint old == loFingerprint obj -> do
            when linkerDebug $
              hPutStrLn stderr ("GHCi.ObjLinker: loadObj " <> path <> " already loaded")
            freeLoadedObject obj
            pure st
        | otherwise -> do
            when linkerDebug $
              hPutStrLn stderr ("GHCi.ObjLinker: loadObj " <> path <> " reload")
            -- reload: unload old object and replace it
            runFiniSections old
            let symMap0 = removeSymbolsFor path (lsSymbols st)
            when (loOcPtr old /= nullPtr) $ do
              c_freeObjectCodeExports (loOcPtr old)
              c_unloadObject (loOcPtr old) 0
            let st' = st { lsObjects = Map.delete path (lsObjects st)
                         , lsSymbols = symMap0
                         }
            insertNew st'
      Nothing -> insertNew st

freeLoadedObject :: LoadedObject -> IO ()
freeLoadedObject obj = do
  when (loOcPtr obj /= nullPtr) $
    c_freeObjectCodeExports (loOcPtr obj)
  when (loOcPtr obj == nullPtr && loMemSize obj > 0 && loMemBase obj /= nullPtr) $
    c_munmap (castPtr (loMemBase obj)) (fromIntegral (loMemSize obj))

insertObjectSymbols :: Map String [SymbolInfo] -> LoadedObject -> (Either String (Map String [SymbolInfo]), [String])
insertObjectSymbols symMap obj =
  let (symMap', names) = foldl' step (Right symMap, []) (assocs (loSymbols obj))
  in (symMap', names)
  where
    step (Left e, names) _ = (Left e, names)
    step (Right m, names) (idx, sym) =
      if esBind sym == stBindLocal || esShndx sym == shnUndef
        then (Right m, names)
        else case loSymAddrs obj ! idx of
          Nothing -> (Right m, names)
          Just addr ->
            let name = esName sym
                strength = if esBind sym == stBindWeak then SymWeak else SymStrong
                kind = if esType sym == stTypeFunc then SymCode else SymData
                info = SymbolInfo
                  { symAddr = castPtr addr
                  , symStrength = strength
                  , symKind = kind
                  , symHidden = isHiddenSym sym
                  , symOrigin = OriginObj (loPath obj)
                  }
            in case insertSymbol name info m of
              Left e -> (Left e, names)
              Right m' -> (Right m', name:names)

registerLoadedObject :: LoadedObject -> IO LoadedObject
registerLoadedObject obj = do
  let sections = gcSections obj
      starts = map (\(a, _, _) -> a) sections
      sizes = map (\(_, s, _) -> fromIntegral s :: CSize) sections
      kinds = map (\(_, _, k) -> k) sections
      n = length sections
  oc <- withCString (loPath obj) $ \cpath ->
        withArray starts $ \pStarts ->
        withArray sizes $ \pSizes ->
        withArray kinds $ \pKinds ->
          c_registerObject cpath (fromIntegral n) pStarts pSizes pKinds
                          (loMemBase obj) (fromIntegral (loMemSize obj))
  when (oc == nullPtr) $
    throwIO (ErrorCall ("loadObj " <> show (loPath obj) <> ": failed to register object"))
  pure obj { loOcPtr = oc }

gcSections :: LoadedObject -> [(Ptr Word8, Int, CInt)]
gcSections obj =
  let idxs = indices (loSections obj)
      sections = [ (addr, sz, kind)
                 | i <- idxs
                 , let SectionInfo _ addr sz = loSections obj ! i
                 , let kind = loSectionKinds obj ! i
                 , addr /= nullPtr
                 , sz > 0
                 , kind /= sectionKindOther
                 ]
  in if null sections && loMemSize obj > 0
       then [(loMemBase obj, loMemSize obj, sectionKindCodeOrROData)]
       else sections

resolveAll :: LinkerState -> [LoadedObject] -> IO (Either String [LoadedObject])
resolveAll st objs = go [] objs
  where
    go acc [] = pure (Right (reverse acc))
    go acc (o:os) = do
      r <- resolveObject st o
      case r of
        Left err -> pure (Left err)
        Right o' -> go (o':acc) os

resolveObject :: LinkerState -> LoadedObject -> IO (Either String LoadedObject)
resolveObject st obj = do
  e <- applyRelocations st obj
  case e of
    Left err -> pure (Left err)
    Right () -> do
      runInitSections st obj
      when (loOcPtr obj /= nullPtr) $
        c_setObjectStatus (loOcPtr obj) objectReady
      pure (Right obj { loResolved = True })

applyRelocations :: LinkerState -> LoadedObject -> IO (Either String ())
applyRelocations st obj = do
  let relocs = loRelocs obj
  runExcept relocs
  where
    runExcept [] = pure (Right ())
    runExcept (rs:rss) = do
      r <- applyRelocSection rs
      case r of
        Left err -> pure (Left err)
        Right () -> runExcept rss

    applyRelocSection (RelocSection target entries) = do
      let SectionInfo _ secAddr _ = loSections obj ! target
      foldM (\acc re -> case acc of
              Left e -> pure (Left e)
              Right () -> applyRelocEntry secAddr re) (Right ()) entries

    applyRelocEntry secAddr (RelocEntry off info addend) = do
      let symIndex = fromIntegral (info `shiftR` 32)
          relType = fromIntegral (info .&. 0xffffffff) :: Word32
          p = secAddr `plusPtr` fromIntegral off
      symRes <- resolveSymbol st obj symIndex
      case symRes of
        Left err -> pure (Left err)
        Right sAddr -> do
          let s = ptrToWord64 sAddr
              a = fromIntegral addend :: Int64
              p64 = ptrToWord64 p
          case relType of
            0 -> pure (Right ()) -- R_X86_64_NONE
            1 -> do -- R_X86_64_64
              pokeWord64LE p (fromIntegral (s + fromIntegral a))
              pure (Right ())
            2 -> do -- R_X86_64_PC32
              let off64 = (fromIntegral s :: Int64) + a - (fromIntegral p64 :: Int64)
              if fitsInt32 off64
                then pokeWord32LE p (fromIntegral (fromIntegral off64 :: Int32)) >> pure (Right ())
                else do
                  jmp <- ensureSymbolExtra obj symIndex s
                  let off' = (fromIntegral (ptrToWord64 jmp) :: Int64) + a - (fromIntegral p64 :: Int64)
                  if fitsInt32 off'
                    then pokeWord32LE p (fromIntegral (fromIntegral off' :: Int32)) >> pure (Right ())
                    else pure (Left "R_X86_64_PC32 relocation out of range")
            24 -> do -- R_X86_64_PC64
              let off64 = (fromIntegral s :: Int64) + a - (fromIntegral p64 :: Int64)
              pokeWord64LE p (fromIntegral off64)
              pure (Right ())
            10 -> do -- R_X86_64_32
              let val64 = (fromIntegral s :: Int64) + a
              if fitsWord32 val64
                then pokeWord32LE p (fromIntegral (fromIntegral val64 :: Word32)) >> pure (Right ())
                else do
                  jmp <- ensureSymbolExtra obj symIndex s
                  let val' = (fromIntegral (ptrToWord64 jmp) :: Int64) + a
                  if fitsWord32 val'
                    then pokeWord32LE p (fromIntegral (fromIntegral val' :: Word32)) >> pure (Right ())
                    else pure (Left "R_X86_64_32 relocation out of range")
            11 -> do -- R_X86_64_32S
              let val64 = (fromIntegral s :: Int64) + a
              if fitsInt32 val64
                then pokeWord32LE p (fromIntegral (fromIntegral val64 :: Int32)) >> pure (Right ())
                else do
                  jmp <- ensureSymbolExtra obj symIndex s
                  let val' = (fromIntegral (ptrToWord64 jmp) :: Int64) + a
                  if fitsInt32 val'
                    then pokeWord32LE p (fromIntegral (fromIntegral val' :: Int32)) >> pure (Right ())
                    else pure (Left "R_X86_64_32S relocation out of range")
            9 -> do -- R_X86_64_GOTPCREL
              got <- ensureGotEntry obj symIndex s
              let off64 = (fromIntegral (ptrToWord64 got) :: Int64) + a - (fromIntegral p64 :: Int64)
              if fitsInt32 off64
                then pokeWord32LE p (fromIntegral (fromIntegral off64 :: Int32)) >> pure (Right ())
                else pure (Left "R_X86_64_GOTPCREL relocation out of range")
            41 -> do -- R_X86_64_GOTPCRELX
              got <- ensureGotEntry obj symIndex s
              let off64 = (fromIntegral (ptrToWord64 got) :: Int64) + a - (fromIntegral p64 :: Int64)
              if fitsInt32 off64
                then pokeWord32LE p (fromIntegral (fromIntegral off64 :: Int32)) >> pure (Right ())
                else pure (Left "R_X86_64_GOTPCRELX relocation out of range")
            42 -> do -- R_X86_64_REX_GOTPCRELX
              got <- ensureGotEntry obj symIndex s
              let off64 = (fromIntegral (ptrToWord64 got) :: Int64) + a - (fromIntegral p64 :: Int64)
              if fitsInt32 off64
                then pokeWord32LE p (fromIntegral (fromIntegral off64 :: Int32)) >> pure (Right ())
                else pure (Left "R_X86_64_REX_GOTPCRELX relocation out of range")
            19 -> pure (Left "R_X86_64_TLSGD relocation not supported")
            4 -> do -- R_X86_64_PLT32
              let off64 = (fromIntegral s :: Int64) + a - (fromIntegral p64 :: Int64)
              if fitsInt32 off64
                then pokeWord32LE p (fromIntegral (fromIntegral off64 :: Int32)) >> pure (Right ())
                else do
                  jmp <- ensureSymbolExtra obj symIndex s
                  let off' = (fromIntegral (ptrToWord64 jmp) :: Int64) + a - (fromIntegral p64 :: Int64)
                  if fitsInt32 off'
                    then pokeWord32LE p (fromIntegral (fromIntegral off' :: Int32)) >> pure (Right ())
                    else pure (Left "R_X86_64_PLT32 relocation out of range")
            _ -> pure (Left ("unhandled relocation type: " <> show relType))

resolveSymbol :: LinkerState -> LoadedObject -> Int -> IO (Either String (Ptr Word8))
resolveSymbol st obj symIndex = do
  let sym = loSymbols obj ! symIndex
      name = esName sym
      bind = esBind sym
      shndx = esShndx sym
  if bind == stBindLocal
    then pure $ case loSymAddrs obj ! symIndex of
      Just addr -> Right addr
      Nothing -> Left "local symbol has no address"
    else if shndx /= shnUndef
      then
        if isHiddenSym sym
          then pure $ case loSymAddrs obj ! symIndex of
            Just addr -> Right addr
            Nothing -> Left "hidden symbol has no address"
          else case lookupSymbolMap name (lsSymbols st) of
            Just addr -> pure (Right (castPtr addr))
            Nothing -> resolveMissingSymbol name
      else do
        case lookupSymbolMap name (lsSymbols st) of
          Just addr -> pure (Right (castPtr addr))
          Nothing -> do
            addr <- withCString name c_dlsym_default
            if addr /= nullPtr
              then pure (Right (castPtr addr))
              else if bind == stBindWeak
                then pure (Right nullPtr)
                else resolveMissingSymbol name

optimisticSymbolPtr :: Ptr a
optimisticSymbolPtr = wordPtrToPtr (fromIntegral (0xDEADBEEF :: Word64))

linkerOptimistic :: IO Bool
linkerOptimistic = (/= 0) <$> c_linkerOptimistic

reportMissingSymbol :: String -> Bool -> IO ()
reportMissingSymbol name optimistic =
  withCString name $ \cname ->
    c_reportMissingSymbol cname (if optimistic then 1 else 0)

resolveMissingSymbol :: String -> IO (Either String (Ptr Word8))
resolveMissingSymbol name = do
  optimistic <- linkerOptimistic
  reportMissingSymbol name optimistic
  if optimistic
    then pure (Right (castPtr optimisticSymbolPtr))
    else pure (Left ("unknown symbol: " <> name))

--------------------------------------------------------------------------------
-- Init/fini handling
--------------------------------------------------------------------------------

runInitSections :: LinkerState -> LoadedObject -> IO ()
runInitSections _st obj = do
  let (inits, _) = collectInitFini obj
  -- Associate foreign exports with this object while running initializers
  if loOcPtr obj /= nullPtr
    then bracket_ (c_foreignExportsLoadingObject (loOcPtr obj))
                  c_foreignExportsFinishedLoadingObject
                  (runInitList inits)
    else runInitList inits
  -- We intentionally do not run finalizers here
  pure ()

runFiniSections :: LoadedObject -> IO ()
runFiniSections obj = do
  let (_, finis) = collectInitFini obj
  runFiniList finis

runFiniList :: [InitSection] -> IO ()
runFiniList sections = do
  let sorted = sortByPriorityDesc sections
  forM_ sorted runSection

collectInitFini :: LoadedObject -> ([InitSection], [InitSection])
collectInitFini obj =
  let secs = elems (loSections obj)
      inits = mapMaybe classify secs
  in (filter isInit inits, filter isFini inits)
  where
    classify (SectionInfo name addr size) =
      parseInitSection name addr size

    isInit (InitSection kind _ _ _) = case kind of
      InitInit -> True
      InitCtors -> True
      InitArray -> True
      _ -> False

    isFini (InitSection kind _ _ _) = case kind of
      InitFini -> True
      InitDtors -> True
      FiniArray -> True
      _ -> False

runInitList :: [InitSection] -> IO ()
runInitList sections = do
  let sorted = sortByPriority sections
  forM_ sorted runSection

sortByPriority :: [InitSection] -> [InitSection]
sortByPriority = sortBy cmp
  where
    cmp (InitSection _ pa _ _) (InitSection _ pb _ _) = compare pa pb

sortByPriorityDesc :: [InitSection] -> [InitSection]
sortByPriorityDesc = sortBy cmp
  where
    cmp (InitSection _ pa _ _) (InitSection _ pb _ _) = compare pb pa

runSection :: InitSection -> IO ()
runSection (InitSection kind _ addr size) = do
  case kind of
    InitInit -> callInit addr
    InitFini -> callFini addr
    InitCtors -> callCtors addr size
    InitDtors -> callDtors addr size
    InitArray -> callInitArray addr size
    FiniArray -> callFiniArray addr size

callInit :: Ptr Word8 -> IO ()
callInit addr = withProgEnv $ \argc argv envv -> do
  let f = mkInitFun (castPtrToFunPtr addr)
  f argc argv envv

callFini :: Ptr Word8 -> IO ()
callFini addr = do
  let f = mkFiniFun (castPtrToFunPtr addr)
  f

callCtors :: Ptr Word8 -> Int -> IO ()
callCtors addr size = withProgEnv $ \argc argv envv -> do
  let count = size `div` ptrSize
      base = castPtr addr :: Ptr (FunPtr InitFun)
      loop i
        | i < 0 = pure ()
        | otherwise = do
            fun <- peekElemOff base i
            when (fun /= nullFunPtr && fun /= minusOneFunPtr) $ do
              let f = mkInitFun fun
              f argc argv envv
            loop (i - 1)
  loop (count - 1)

callDtors :: Ptr Word8 -> Int -> IO ()
callDtors addr size = do
  let count = size `div` ptrSize
      base = castPtr addr :: Ptr (FunPtr FiniFun)
      loop i
        | i >= count = pure ()
        | otherwise = do
            fun <- peekElemOff base i
            when (fun /= nullFunPtr && fun /= minusOneFunPtr) $ do
              let f = mkFiniFun fun
              f
            loop (i + 1)
  loop 0

callInitArray :: Ptr Word8 -> Int -> IO ()
callInitArray addr size = withProgEnv $ \argc argv envv -> do
  let count = size `div` ptrSize
      base = castPtr addr :: Ptr (FunPtr InitFun)
  forM_ [0..count-1] $ \i -> do
    fun <- peekElemOff base i
    when (fun /= nullFunPtr) $ do
      let f = mkInitFun fun
      f argc argv envv

callFiniArray :: Ptr Word8 -> Int -> IO ()
callFiniArray addr size = do
  let count = size `div` ptrSize
      base = castPtr addr :: Ptr (FunPtr FiniFun)
  when (count > 0) $
    forM_ [count-1, count-2 .. 0] $ \i -> do
      fun <- peekElemOff base i
      when (fun /= nullFunPtr) $ do
        let f = mkFiniFun fun
        f

minusOneFunPtr :: FunPtr a
minusOneFunPtr = castPtrToFunPtr (intPtrToPtr (IntPtr (-1)))

ptrSize :: Int
ptrSize = sizeOf (nullPtr :: Ptr ())

--------------------------------------------------------------------------------
-- Archive parsing
--------------------------------------------------------------------------------

type ArchiveMember = (String, ByteString)

parseArchive :: FilePath -> ByteString -> IO [ArchiveMember]
parseArchive path bs =
  if BS.take 8 bs /= BSC.pack "!<arch>\n"
    then throwIO (ErrorCall ("loadArchive " <> show path <> ": not an archive"))
    else pure (parseMembers (BS.drop 8 bs) Nothing)
  where
    parseMembers input extTable
      | BS.null input = []
      | BS.length input < 60 = []
      | otherwise =
          let (hdr, rest0) = BS.splitAt 60 input
              nameField = BS.take 16 hdr
              sizeField = BS.take 10 (BS.drop 48 hdr)
              endField = BS.drop 58 hdr
          in if endField /= BSC.pack "`\n"
               then []
               else
                 let size = readInt sizeField
                     (fileData, rest1) = BS.splitAt size rest0
                     rest = if odd size then BS.drop 1 rest1 else rest1
                     (mname, payload, extTable') = decodeName nameField fileData extTable
                 in case mname of
                      "" -> parseMembers rest extTable'
                      "/" -> parseMembers rest extTable' -- symbol table
                      "//" -> parseMembers rest (Just fileData)
                      _ | isSpecialMember mname -> parseMembers rest extTable'
                        | otherwise -> (mname, payload) : parseMembers rest extTable'

    isSpecialMember nm = "__.SYMDEF" `isPrefixOf` nm

    readInt s =
      let digits = BSC.unpack (BSC.takeWhile (/= ' ') s)
      in if null digits then 0 else read digits

    decodeName nameField fileData extTable
      | BSC.take 2 nameField == BSC.pack "#1" =
          let len = readInt (BS.drop 3 nameField)
              (nbytes, rest) = BS.splitAt len fileData
          in (stripNull (BSC.unpack nbytes), rest, extTable)
      | BSC.take 2 nameField == BSC.pack "//" = ("//", fileData, Just fileData)
      | BSC.take 1 nameField == BSC.pack "/" =
          case extTable of
            Nothing -> ("/", fileData, extTable)
            Just tbl ->
              let off = readInt (BS.drop 1 nameField)
                  nm = BSC.takeWhile (/= '/') (BS.drop off tbl)
              in (stripName nm, fileData, extTable)
      | otherwise = (stripName nameField, fileData, extTable)

    stripName = BSC.unpack . BSC.takeWhile (/= '/') . BSC.takeWhile (/= ' ')
    stripNull = takeWhile (/= '\0')

--------------------------------------------------------------------------------
-- ELF parsing
--------------------------------------------------------------------------------

parseElfObject :: FilePath -> ByteString -> IO ParsedObject
parseElfObject path bs = do
  hdr <- parseElfHeader path bs
  when (ehMachine hdr /= emX86_64) $
    throwIO (ErrorCall ("loadObj " <> show path <> ": unsupported machine"))
  let shnum = fromIntegral (ehShnum hdr)
      shentsize = fromIntegral (ehShentsize hdr)
      shoff = fromIntegral (ehShoff hdr)
  let shdrs = [ parseSectionHeader bs (shoff + i * shentsize) | i <- [0 .. shnum - 1] ]
      shdrArr = listArray (0, shnum - 1) shdrs
      shstr = sectionBytes bs (shdrArr ! fromIntegral (ehShstrndx hdr))
      secNames = map (sectionName shstr) shdrs
      secNamesArr = listArray (0, shnum - 1) secNames
      secKinds = zipWith sectionKind shdrs secNames
      secKindsArr = listArray (0, shnum - 1) secKinds

  (_symtabIdx, symtabHdr) <- findSymtab shdrs
  let strtab = sectionBytes bs (shdrArr ! fromIntegral (shLink symtabHdr))
      symbols = parseSymbols bs symtabHdr strtab
      symArr = listArray (0, length symbols - 1) symbols

  let relocs = parseRelocations bs shdrArr
  pure ParsedObject
    { poPath = path
    , poBytes = bs
    , poShdrs = shdrArr
    , poSecNames = secNamesArr
    , poSecKinds = secKindsArr
    , poSymbols = symArr
    , poRelocs = relocs
    }

instantiateObject :: ParsedObject -> IO LoadedObject
instantiateObject parsed = do
  let path = poPath parsed
  (secInfos, symAddrs, memSize, extrasPtrOffset, memBase) <-
        allocateImage path (poBytes parsed) (poShdrs parsed)
                      (poSecNames parsed) (poSymbols parsed)
  let fp = hashBytes (poBytes parsed)
  pure LoadedObject
    { loPath = path
    , loMemBase = memBase
    , loMemSize = memSize
    , loSections = secInfos
    , loSectionKinds = poSecKinds parsed
    , loSymbols = poSymbols parsed
    , loSymAddrs = symAddrs
    , loRelocs = poRelocs parsed
    , loResolved = False
    , loExtras = memBase `plusPtr` extrasPtrOffset
    , loFingerprint = fp
    , loOcPtr = nullPtr
    }

elfMagic :: ByteString
elfMagic = BS.pack [0x7f, 0x45, 0x4c, 0x46]

parseElfHeader :: FilePath -> ByteString -> IO ElfHeader
parseElfHeader path bs = do
  let hdr = BS.take 64 bs
  when (BS.length hdr < 64) $
    throwIO (ErrorCall ("loadObj " <> show path <> ": truncated header"))
  let magic = BS.take 4 hdr
  when (magic /= elfMagic) $
    throwIO (ErrorCall ("loadObj " <> show path <> ": invalid ELF magic"))
  let cls = BS.index hdr 4
      dataEnc = BS.index hdr 5
  when (cls /= 2 || dataEnc /= 1) $
    throwIO (ErrorCall ("loadObj " <> show path <> ": unsupported ELF class"))
  let getW16 off = getWord16le bs off
      getW64 off = getWord64le bs off
      machine = getW16 18
      shoff = getW64 40
      shentsize = getW16 58
      shnum = getW16 60
      shstrndx = getW16 62
  pure ElfHeader
    { ehMachine = machine
    , ehShoff = shoff
    , ehShentsize = shentsize
    , ehShnum = shnum
    , ehShstrndx = shstrndx
    }

parseSectionHeader :: ByteString -> Int -> SectionHeader
parseSectionHeader bs off =
  SectionHeader
    { shName = getWord32le bs off
    , shType = getWord32le bs (off + 4)
    , shFlags = getWord64le bs (off + 8)
    , shAddr = getWord64le bs (off + 16)
    , shOffset = getWord64le bs (off + 24)
    , shSize = getWord64le bs (off + 32)
    , shLink = getWord32le bs (off + 40)
    , shInfo = getWord32le bs (off + 44)
    , shAddralign = getWord64le bs (off + 48)
    , shEntsize = getWord64le bs (off + 56)
    }

findSymtab :: [SectionHeader] -> IO (Int, SectionHeader)
findSymtab shdrs =
  case findIndex (\s -> shType s == shtSymtab) shdrs of
    Nothing -> throwIO (ErrorCall "loadObj: no symbol table")
    Just idx -> pure (idx, shdrs !! idx)

sectionBytes :: ByteString -> SectionHeader -> ByteString
sectionBytes bs sh = BS.take (fromIntegral (shSize sh))
                   $ BS.drop (fromIntegral (shOffset sh)) bs

sectionName :: ByteString -> SectionHeader -> String
sectionName shstr sh =
  let off = fromIntegral (shName sh)
  in BSC.unpack (BSC.takeWhile (/= '\0') (BS.drop off shstr))

parseSymbols :: ByteString -> SectionHeader -> ByteString -> [ElfSymbol]
parseSymbols bs symtab strtab =
  let entSize = if shEntsize symtab == 0 then 24 else fromIntegral (shEntsize symtab)
      total = fromIntegral (shSize symtab)
      count = total `div` entSize
      base = fromIntegral (shOffset symtab)
  in [ parseSymbol bs (base + i * entSize) strtab | i <- [0 .. count - 1] ]

parseSymbol :: ByteString -> Int -> ByteString -> ElfSymbol
parseSymbol bs off strtab =
  let st_name = getWord32le bs off
      st_info = BS.index bs (off + 4)
      st_other = BS.index bs (off + 5)
      st_shndx = getWord16le bs (off + 6)
      st_value = getWord64le bs (off + 8)
      st_size = getWord64le bs (off + 16)
      name = if st_name == 0 then "" else
               let n = fromIntegral st_name
               in BSC.unpack (BSC.takeWhile (/= '\0') (BS.drop n strtab))
      bind = st_info `shiftR` 4
      typ = st_info .&. 0x0f
      vis = st_other .&. 0x3
  in ElfSymbol name bind typ vis st_shndx st_value st_size

parseRelocations :: ByteString -> Array Int SectionHeader -> [RelocSection]
parseRelocations bs shdrArr =
  concatMap parseReloc (assocs shdrArr)
  where
    parseReloc (_idx, sh)
      | shType sh == shtRela =
          let target = fromIntegral (shInfo sh)
          in if isAllocSection (shdrArr ! target)
                then [ RelocSection { rsTarget = target, rsEntries = parseRelaEntries bs sh } ]
                else []
      | shType sh == shtRel =
          let target = fromIntegral (shInfo sh)
          in if isAllocSection (shdrArr ! target)
                then [ RelocSection { rsTarget = target, rsEntries = parseRelEntries bs sh } ]
                else []
      | otherwise = []

parseRelaEntries :: ByteString -> SectionHeader -> [RelocEntry]
parseRelaEntries bs sh =
  let entSize = if shEntsize sh == 0 then 24 else fromIntegral (shEntsize sh)
      total = fromIntegral (shSize sh)
      count = total `div` entSize
      base = fromIntegral (shOffset sh)
  in [ parseRela bs (base + i * entSize) | i <- [0 .. count - 1] ]

parseRela :: ByteString -> Int -> RelocEntry
parseRela bs off =
  let r_offset = getWord64le bs off
      r_info = getWord64le bs (off + 8)
      r_addend = getInt64le bs (off + 16)
  in RelocEntry r_offset r_info r_addend

parseRelEntries :: ByteString -> SectionHeader -> [RelocEntry]
parseRelEntries bs sh =
  let entSize = if shEntsize sh == 0 then 16 else fromIntegral (shEntsize sh)
      total = fromIntegral (shSize sh)
      count = total `div` entSize
      base = fromIntegral (shOffset sh)
  in [ parseRel bs (base + i * entSize) | i <- [0 .. count - 1] ]

parseRel :: ByteString -> Int -> RelocEntry
parseRel bs off =
  let r_offset = getWord64le bs off
      r_info = getWord64le bs (off + 8)
  in RelocEntry r_offset r_info 0

allocateImage
  :: FilePath
  -> ByteString
  -> Array Int SectionHeader
  -> Array Int String
  -> Array Int ElfSymbol
  -> IO (Array Int SectionInfo, Array Int (Maybe (Ptr Word8)), Int, Int, Ptr Word8)
allocateImage path bs shdrArr nameArr symArr = do
  pageSz <- fromIntegral <$> c_pageSize
  let shBounds@(lo, hi) = bounds shdrArr
      shIndexes = [lo .. hi]
      allocSections = filter (isAllocSection . (shdrArr !)) shIndexes
      (secOffsets, sizeAfterSections) = layoutSections shdrArr allocSections
      (commonOffsets, sizeAfterCommon) = layoutCommon symArr sizeAfterSections
      extrasOffset = alignUp sizeAfterCommon 16
      extrasSize = (snd (bounds symArr) + 1) * symbolExtraSize
      totalSize = alignUp (extrasOffset + extrasSize) pageSz
  mem <- c_mmap (fromIntegral totalSize) 1
  when (mem == nullPtr) $
    throwIO (ErrorCall ("loadObj " <> show path <> ": mmap failed"))
  fillBytes mem 0 totalSize
  -- fill sections
  secInfos <- forM shIndexes $ \i -> do
    let sh = shdrArr ! i
        name = nameArr ! i
    if isAllocSection sh
      then do
        let offset = secOffsets Map.! i
            addr = mem `plusPtr` offset
            size = fromIntegral (shSize sh)
        if shType sh /= shtNoBits && size > 0
          then BS.useAsCString (sectionBytes bs sh) $ \src ->
                 copyBytes addr (castPtr src) size
          else pure ()
        pure (SectionInfo name addr size)
      else pure (SectionInfo name nullPtr 0)
  let secArr = listArray shBounds secInfos
  -- compute symbol addrs
  symAddrs <- forM (assocs symArr) $ \(idx, sym) -> do
    let shndx = esShndx sym
    if shndx == shnUndef
      then pure (idx, Nothing)
      else if shndx == shnCommon
        then case Map.lookup idx commonOffsets of
          Nothing -> pure (idx, Nothing)
          Just off -> pure (idx, Just (mem `plusPtr` off))
        else if shndx == shnAbs
          then pure (idx, Just (word64ToPtr (esValue sym)))
          else do
            let SectionInfo _ base _ = secArr ! fromIntegral shndx
            pure (idx, Just (base `plusPtr` fromIntegral (esValue sym)))
  let symAddrArr = array (bounds symArr) symAddrs
  pure (secArr, symAddrArr, totalSize, extrasOffset, mem)
  where
    layoutSections shdrs idxs =
      let (offsets, size) = foldl' step (Map.empty, 0) idxs
      in (offsets, size)
      where
        step (m, off) i =
          let sh = shdrs ! i
              align = max 1 (fromIntegral (shAddralign sh))
              off' = alignUp off align
              size = fromIntegral (shSize sh)
          in (Map.insert i off' m, off' + size)

    layoutCommon syms start =
      let (m, end) = foldl' step (Map.empty, start) (assocs syms)
      in (m, end)
      where
        step (m, off) (idx, sym) =
          if esShndx sym == shnCommon
            then
              let align = max 1 (fromIntegral (esValue sym))
                  off' = alignUp off align
                  size = fromIntegral (esSize sym)
              in (Map.insert idx off' m, off' + size)
            else (m, off)

isAllocSection :: SectionHeader -> Bool
isAllocSection sh = shFlags sh .&. shfAlloc /= 0

sectionKind :: SectionHeader -> String -> CInt
sectionKind sh name
  | shType sh == shtProgbits && isAlloc && isExec = sectionKindCodeOrROData
  | shType sh == shtProgbits && isAlloc && isWrite = sectionKindRWData
  | shType sh == shtProgbits && isAlloc && not isWrite = sectionKindCodeOrROData
  | shType sh == shtInitArray && isAlloc && isWrite = sectionKindInitArray
  | shType sh == shtFiniArray && isAlloc && isWrite = sectionKindFiniArray
  | shType sh == shtNoBits && isAlloc && isWrite = sectionKindRWData
  | ".init_array" `isPrefixOf` name = sectionKindInitArray
  | ".ctors" `isPrefixOf` name = sectionKindInitArray
  | ".fini_array" `isPrefixOf` name = sectionKindFiniArray
  | ".dtors" `isPrefixOf` name = sectionKindFiniArray
  | otherwise = sectionKindOther
  where
    flags = shFlags sh
    isAlloc = flags .&. shfAlloc /= 0
    isWrite = flags .&. shfWrite /= 0
    isExec = flags .&. shfExecInstr /= 0

--------------------------------------------------------------------------------
-- Init section classification
--------------------------------------------------------------------------------

parseInitSection :: String -> Ptr Word8 -> Int -> Maybe InitSection
parseInitSection name addr size
  | ".init_array" `isPrefixOf` name =
      let prio = parsePriority name + 0x10000
      in Just (InitSection InitArray prio addr size)
  | ".fini_array" `isPrefixOf` name =
      let prio = parsePriority name + 0x10000
      in Just (InitSection FiniArray prio addr size)
  | ".ctors" `isPrefixOf` name =
      let prio = 0xffff - parsePriority name
      in Just (InitSection InitCtors prio addr size)
  | ".dtors" `isPrefixOf` name =
      let prio = 0xffff - parsePriority name
      in Just (InitSection InitDtors prio addr size)
  | ".init" `isPrefixOf` name = Just (InitSection InitInit 0 addr size)
  | ".fini" `isPrefixOf` name = Just (InitSection InitFini 0 addr size)
  | otherwise = Nothing

parsePriority :: String -> Word32
parsePriority name =
  let (revDigits, rest) = span isDigit (reverse name)
  in case revDigits of
       [] -> 0
       _  -> case rest of
               ('.':_) -> fromIntegral (read (reverse revDigits) :: Int)
               _       -> 0

--------------------------------------------------------------------------------
-- Symbol extras / GOT
--------------------------------------------------------------------------------

symbolExtraSize :: Int
symbolExtraSize = 16

ensureSymbolExtra :: LoadedObject -> Int -> Word64 -> IO (Ptr Word8)
ensureSymbolExtra obj symIdx target = do
  let extraPtr = loExtras obj `plusPtr` (symIdx * symbolExtraSize)
  pokeWord64LE extraPtr target
  -- write jump island bytes
  let jmpBytes = BS.pack [0xFF, 0x25, 0xF2, 0xFF, 0xFF, 0xFF, 0x00, 0x00]
  BS.useAsCString jmpBytes $ \src ->
    copyBytes (extraPtr `plusPtr` 8) (castPtr src) 8
  pure (extraPtr `plusPtr` 8)

ensureGotEntry :: LoadedObject -> Int -> Word64 -> IO (Ptr Word8)
ensureGotEntry obj symIdx target = do
  let extraPtr = loExtras obj `plusPtr` (symIdx * symbolExtraSize)
  pokeWord64LE extraPtr target
  pure extraPtr

--------------------------------------------------------------------------------
-- RTS symbol table
--------------------------------------------------------------------------------

loadRtsSymbols :: IO (Map String [SymbolInfo])
loadRtsSymbols = do
  baseCount <- c_rtsSymsCount
  baseSyms <- forM [0 .. baseCount - 1] $ \i -> do
    name <- c_rtsSymName i >>= peekCString
    addr <- c_rtsSymAddr i
    strength <- c_rtsSymStrength i
    ty <- c_rtsSymType i
    let info = SymbolInfo
          { symAddr = addr
          , symStrength = if strength == 1 then SymWeak else SymStrong
          , symKind = if ty .&. symTypeCode /= 0 then SymCode else SymData
          , symHidden = False
          , symOrigin = OriginRTS
          }
    pure (name, info)
  extraCount <- c_rtsExtraSymsCount
  extraSyms <- forM [0 .. extraCount - 1] $ \i -> do
    name <- c_rtsExtraSymName i >>= peekCString
    addr <- c_rtsExtraSymAddr i
    strength <- c_rtsExtraSymStrength i
    ty <- c_rtsExtraSymType i
    let info = SymbolInfo
          { symAddr = addr
          , symStrength = if strength == 1 then SymWeak else SymStrong
          , symKind = if ty .&. symTypeCode /= 0 then SymCode else SymData
          , symHidden = False
          , symOrigin = OriginRTS
          }
    pure (name, info)
  let allSyms = baseSyms ++ extraSyms
  pure (foldl' insert Map.empty allSyms)
  where
    insert m (n, info) =
      case insertSymbol n info m of
        Left _ -> m
        Right m' -> m'

updateNewCAF :: Bool -> Map String [SymbolInfo] -> Map String [SymbolInfo]
updateNewCAF retain syms =
  let addr = if retain then c_newRetainedCAF else c_newGCdCAF
      info = SymbolInfo addr SymStrong SymCode False OriginRTS
      syms' = Map.delete "newCAF" syms
  in case insertSymbol "newCAF" info syms' of
       Left _ -> syms'
       Right m -> m

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

getWord16le :: ByteString -> Int -> Word16
getWord16le bs off =
  let b0 = fromIntegral (BS.index bs off) :: Word16
      b1 = fromIntegral (BS.index bs (off+1)) :: Word16
  in b0 .|. (b1 `shiftL` 8)

hashBytes :: ByteString -> Word64
hashBytes = BS.foldl' step 14695981039346656037
  where
    step h b = (h `xor` fromIntegral b) * 1099511628211

getWord32le :: ByteString -> Int -> Word32
getWord32le bs off =
  let b0 = fromIntegral (BS.index bs off) :: Word32
      b1 = fromIntegral (BS.index bs (off+1)) :: Word32
      b2 = fromIntegral (BS.index bs (off+2)) :: Word32
      b3 = fromIntegral (BS.index bs (off+3)) :: Word32
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)

getWord64le :: ByteString -> Int -> Word64
getWord64le bs off =
  let w0 = fromIntegral (getWord32le bs off) :: Word64
      w1 = fromIntegral (getWord32le bs (off+4)) :: Word64
  in w0 .|. (w1 `shiftL` 32)

getInt64le :: ByteString -> Int -> Int64
getInt64le bs off = fromIntegral (getWord64le bs off)

alignUp :: Int -> Int -> Int
alignUp n a
  | a <= 1 = n
  | otherwise = ((n + a - 1) `div` a) * a

ptrToWord64 :: Ptr a -> Word64
ptrToWord64 = fromIntegral . ptrToWordPtr

word64ToPtr :: Word64 -> Ptr Word8
word64ToPtr w = wordPtrToPtr (fromIntegral w)

fitsInt32 :: Int64 -> Bool
fitsInt32 x = x == fromIntegral (fromIntegral x :: Int32)

fitsWord32 :: Int64 -> Bool
fitsWord32 x = x >= 0 && x == fromIntegral (fromIntegral x :: Word32)

pokeWord32LE :: Ptr Word8 -> Word32 -> IO ()
pokeWord32LE ptr v = poke (castPtr ptr) v

pokeWord64LE :: Ptr Word8 -> Word64 -> IO ()
pokeWord64LE ptr v = poke (castPtr ptr) v

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

emX86_64 :: Word16
emX86_64 = 62

shtSymtab, shtRela, shtRel, shtNoBits, shtProgbits, shtInitArray, shtFiniArray :: Word32
shtSymtab = 2
shtRela = 4
shtRel = 9
shtNoBits = 8
shtProgbits = 1
shtInitArray = 14
shtFiniArray = 15

shfAlloc, shfWrite, shfExecInstr :: Word64
shfAlloc = 0x2
shfWrite = 0x1
shfExecInstr = 0x4

sectionKindCodeOrROData, sectionKindRWData, sectionKindInitArray, sectionKindFiniArray, sectionKindOther :: CInt
sectionKindCodeOrROData = 0
sectionKindRWData = 1
sectionKindInitArray = 2
sectionKindFiniArray = 3
sectionKindOther = 4

objectReady :: CInt
objectReady = 3

shnUndef, shnAbs, shnCommon :: Word16
shnUndef = 0
shnAbs = 0xfff1
shnCommon = 0xfff2

stBindLocal, stBindWeak, stTypeFunc :: Word8
stBindLocal = 0
stBindWeak = 2
stTypeFunc = 2

stVisInternal, stVisHidden :: Word8
stVisInternal = 1
stVisHidden = 2

symTypeCode :: CInt
symTypeCode = 1

isHiddenSym :: ElfSymbol -> Bool
isHiddenSym sym = esVis sym == stVisHidden || esVis sym == stVisInternal

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------

foreign import ccall unsafe "ghci_mmap_exec"
  c_mmap :: CSize -> CInt -> IO (Ptr Word8)

foreign import ccall unsafe "ghci_munmap"
  c_munmap :: Ptr () -> CSize -> IO ()

foreign import ccall unsafe "ghci_page_size"
  c_pageSize :: IO CLong

foreign import ccall unsafe "ghci_dlopen"
  c_dlopen :: CString -> CInt -> IO (Ptr LoadedDLL)

foreign import ccall unsafe "ghci_dlsym"
  c_dlsym :: Ptr LoadedDLL -> CString -> IO (Ptr ())

foreign import ccall unsafe "ghci_dlsym_default"
  c_dlsym_default :: CString -> IO (Ptr ())

foreign import ccall unsafe "ghci_dlclose"
  c_dlclose :: Ptr LoadedDLL -> IO CInt

foreign import ccall unsafe "ghci_dlerror"
  c_dlerror :: IO CString

foreign import ccall unsafe "ghci_rts_syms_count"
  c_rtsSymsCount :: IO Int

foreign import ccall unsafe "ghci_rts_sym_name"
  c_rtsSymName :: Int -> IO CString

foreign import ccall unsafe "ghci_rts_sym_addr"
  c_rtsSymAddr :: Int -> IO (Ptr ())

foreign import ccall unsafe "ghci_rts_sym_strength"
  c_rtsSymStrength :: Int -> IO CInt

foreign import ccall unsafe "ghci_rts_sym_type"
  c_rtsSymType :: Int -> IO CInt

foreign import ccall unsafe "ghci_rts_extra_syms_count"
  c_rtsExtraSymsCount :: IO Int

foreign import ccall unsafe "ghci_rts_extra_sym_name"
  c_rtsExtraSymName :: Int -> IO CString

foreign import ccall unsafe "ghci_rts_extra_sym_addr"
  c_rtsExtraSymAddr :: Int -> IO (Ptr ())

foreign import ccall unsafe "ghci_rts_extra_sym_strength"
  c_rtsExtraSymStrength :: Int -> IO CInt

foreign import ccall unsafe "ghci_rts_extra_sym_type"
  c_rtsExtraSymType :: Int -> IO CInt

foreign import ccall unsafe "ghci_free_object_code_exports"
  c_freeObjectCodeExports :: Ptr () -> IO ()

foreign import ccall unsafe "ghci_init_unload_check"
  c_initUnloadCheck :: IO ()

foreign import ccall unsafe "ghci_register_object"
  c_registerObject :: CString
                   -> CInt
                   -> Ptr (Ptr Word8)
                   -> Ptr CSize
                   -> Ptr CInt
                   -> Ptr Word8
                   -> CSize
                   -> IO (Ptr ())

foreign import ccall unsafe "ghci_set_object_status"
  c_setObjectStatus :: Ptr () -> CInt -> IO ()

foreign import ccall unsafe "ghci_unload_object"
  c_unloadObject :: Ptr () -> CInt -> IO ()

foreign import ccall unsafe "ghci_get_object_status"
  c_getObjectStatus :: CString -> IO CInt

foreign import ccall unsafe "ghci_linker_optimistic"
  c_linkerOptimistic :: IO CInt

foreign import ccall unsafe "ghci_report_missing_symbol"
  c_reportMissingSymbol :: CString -> CInt -> IO ()

foreign import ccall unsafe "foreignExportsLoadingObject"
  c_foreignExportsLoadingObject :: Ptr () -> IO ()

foreign import ccall unsafe "foreignExportsFinishedLoadingObject"
  c_foreignExportsFinishedLoadingObject :: IO ()

foreign import ccall unsafe "&newRetainedCAF"
  c_newRetainedCAF :: Ptr ()

foreign import ccall unsafe "&newGCdCAF"
  c_newGCdCAF :: Ptr ()

foreign import ccall unsafe "getProgArgv"
  c_getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

foreign import ccall unsafe "ghci_getProgEnvv"
  c_getProgEnvv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

foreign import ccall unsafe "ghci_freeProgEnvv"
  c_freeProgEnvv :: CInt -> Ptr CString -> IO ()

foreign import ccall "dynamic"
  mkInitFun :: FunPtr InitFun -> InitFun

foreign import ccall "dynamic"
  mkFiniFun :: FunPtr FiniFun -> FiniFun

withProgEnv :: (CInt -> Ptr CString -> Ptr CString -> IO a) -> IO a
withProgEnv k =
  alloca $ \argcPtr ->
    alloca $ \argvPtr ->
      alloca $ \envcPtr ->
        alloca $ \envvPtr -> do
          c_getProgArgv argcPtr argvPtr
          c_getProgEnvv envcPtr envvPtr
          argc <- peek argcPtr
          argv <- peek argvPtr
          envv <- peek envvPtr
          r <- k argc argv envv
          envc <- peek envcPtr
          c_freeProgEnvv envc envv
          pure r

loadDLL :: String -> IO (Either String (Ptr LoadedDLL))
loadDLL path
  | '/' `elem` path = loadDLLAtPath path
  | otherwise = do
      dirs <- getLibrarySearchPaths
      let candidates = [dir </> path | dir <- dirs]
      existing <- filterM doesFileExist candidates
      tryPaths existing
  where
    loadDLLAtPath p = do
      mbs <- (Just <$> BS.readFile p) `catch` \(_ :: SomeException) -> pure Nothing
      case mbs of
        Just bs | not (isElf bs) -> do
          let base = takeDirectory p
          let resolved = resolveLdScript base (parseLdScript bs)
          tryResolved resolved (loadDLLRaw p)
        _ -> loadDLLRaw p

    isElf bs = BS.take 4 bs == elfMagic

    loadDLLRaw p = withCString p $ \cpath -> do
      h <- c_dlopen cpath rtldNowGlobal
      if h == nullPtr
        then do
          err <- c_dlerror >>= peekCString
          pure (Left err)
        else pure (Right h)

    tryPaths [] = loadDLLRaw path
    tryPaths (p:ps) = do
      r <- loadDLLAtPath p
      case r of
        Right _ -> pure r
        Left _ -> tryPaths ps

    tryResolved [] fallback = fallback
    tryResolved (p:ps) fallback = do
      r <- loadDLLRaw p
      case r of
        Right _ -> pure r
        Left _ -> tryResolved ps fallback

    resolveLdScript base toks =
      nub (concatMap (resolveToken base) toks)

    resolveToken base t
      | isAbsolutePath t = [t]
      | null base = [t]
      | otherwise = [base </> t, t]

    isAbsolutePath t = case t of
      '/':_ -> True
      _ -> False

    parseLdScript bs =
      let s = BSC.unpack bs
          sanitized = map (\c -> if isSpace c || c == '(' || c == ')' then ' ' else c) s
          toks = words sanitized
          isLib t = ".so" `isInfixOf` t && not (".a" `isSuffixOf` t)
      in nub (filter isLib toks)

rtldNowGlobal :: CInt
rtldNowGlobal = 0x2 .|. 0x100

--------------------------------------------------------------------------------
-- C API exports (compatibility)
--------------------------------------------------------------------------------

foreign export ccall "hs_initLinker" hs_initLinker :: IO ()
foreign export ccall "hs_initLinker_" hs_initLinker_ :: CInt -> IO ()
foreign export ccall "hs_loadObj" hs_loadObj :: CString -> IO CInt
foreign export ccall "hs_loadArchive" hs_loadArchive :: CString -> IO CInt
foreign export ccall "hs_resolveObjs" hs_resolveObjs :: IO CInt
foreign export ccall "hs_unloadObj" hs_unloadObj :: CString -> IO CInt
foreign export ccall "hs_purgeObj" hs_purgeObj :: CString -> IO CInt
foreign export ccall "hs_lookupSymbol" hs_lookupSymbol :: CString -> IO (Ptr ())
foreign export ccall "hs_lookupSymbolInNativeObj" hs_lookupSymbolInNativeObj :: Ptr LoadedDLL -> CString -> IO (Ptr ())
foreign export ccall "hs_loadNativeObj" hs_loadNativeObj :: CString -> Ptr CString -> IO (Ptr LoadedDLL)
foreign export ccall "hs_unloadNativeObj" hs_unloadNativeObj :: Ptr LoadedDLL -> IO CInt
foreign export ccall "hs_addDLL" hs_addDLL :: CString -> IO CString
foreign export ccall "hs_getObjectLoadStatus" hs_getObjectLoadStatus :: CString -> IO CInt

hs_initLinker :: IO ()
hs_initLinker = initObjLinker True

hs_initLinker_ :: CInt -> IO ()
hs_initLinker_ v = initObjLinker (v /= 0)

hs_loadObj :: CString -> IO CInt
hs_loadObj cpath = do
  path <- peekCString cpath
  (loadObj path >> pure 1) `catch` \(_ :: SomeException) -> pure 0

hs_loadArchive :: CString -> IO CInt
hs_loadArchive cpath = do
  path <- peekCString cpath
  (loadArchive path >> pure 1) `catch` \(_ :: SomeException) -> pure 0

hs_resolveObjs :: IO CInt
hs_resolveObjs = do
  ok <- resolveObjs
  pure (if ok then 1 else 0)

hs_unloadObj :: CString -> IO CInt
hs_unloadObj cpath = do
  path <- peekCString cpath
  (unloadObj path >> pure 1) `catch` \(_ :: SomeException) -> pure 0

hs_purgeObj :: CString -> IO CInt
hs_purgeObj cpath = do
  path <- peekCString cpath
  (purgeObj path >> pure 1) `catch` \(_ :: SomeException) -> pure 0

hs_lookupSymbol :: CString -> IO (Ptr ())
hs_lookupSymbol cname = do
  name <- peekCString cname
  m <- lookupSymbol name
  pure (fromMaybe nullPtr m)

hs_lookupSymbolInNativeObj :: Ptr LoadedDLL -> CString -> IO (Ptr ())
hs_lookupSymbolInNativeObj h cname = do
  name <- peekCString cname
  m <- lookupSymbolInDLL h name
  pure (fromMaybe nullPtr m)

hs_loadNativeObj :: CString -> Ptr CString -> IO (Ptr LoadedDLL)
hs_loadNativeObj cpath perr = do
  path <- peekCString cpath
  r <- loadDLL path
  case r of
    Left err -> do
      cstr <- newCString err
      poke perr cstr
      pure nullPtr
    Right h -> do
      poke perr nullPtr
      pure h

hs_unloadNativeObj :: Ptr LoadedDLL -> IO CInt
hs_unloadNativeObj h = c_dlclose h

hs_addDLL :: CString -> IO CString
hs_addDLL cpath = do
  path <- peekCString cpath
  r <- loadDLLs [path]
  case r of
    Left err -> newCString err
    Right _ -> pure nullPtr

hs_getObjectLoadStatus :: CString -> IO CInt
hs_getObjectLoadStatus cpath = do
  path <- peekCString cpath
  npath <- normalizePath path
  withCString npath c_getObjectStatus

#else

-- Unsupported platforms: stub out

initObjLinker :: Bool -> IO ()
initObjLinker _ = throwIO (ErrorCall "Haskell RTS linker: unsupported platform")

loadDLLs :: [String] -> IO (Either String [Ptr LoadedDLL])
loadDLLs _ = pure (Left "Haskell RTS linker: unsupported platform")

loadArchive :: String -> IO ()
loadArchive _ = throwIO (ErrorCall "Haskell RTS linker: unsupported platform")

loadObj :: String -> IO ()
loadObj _ = throwIO (ErrorCall "Haskell RTS linker: unsupported platform")

unloadObj :: String -> IO ()
unloadObj _ = throwIO (ErrorCall "Haskell RTS linker: unsupported platform")

purgeObj :: String -> IO ()
purgeObj _ = throwIO (ErrorCall "Haskell RTS linker: unsupported platform")

lookupSymbol :: String -> IO (Maybe (Ptr a))
lookupSymbol _ = pure Nothing

lookupSymbolInDLL :: Ptr LoadedDLL -> String -> IO (Maybe (Ptr a))
lookupSymbolInDLL _ _ = pure Nothing

resolveObjs :: IO Bool
resolveObjs = pure False

addLibrarySearchPath :: String -> IO (Ptr ())
addLibrarySearchPath _ = pure nullPtr

removeLibrarySearchPath :: Ptr () -> IO Bool
removeLibrarySearchPath _ = pure False

findSystemLibrary :: String -> IO (Maybe String)
findSystemLibrary _ = pure Nothing

#endif
