{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

-- only for DB.Binary instances on Module
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Object
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Sylvain Henry  <sylvain.henry@iohk.io>
--                Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
--  Serialization/deserialization of binary .o files for the JavaScript backend
--  The .o files contain dependency information and generated code.
--  All strings are mapped to a central string table, which helps reduce
--  file size and gives us efficient hash consing on read
--
--  Binary intermediate JavaScript object files:
--   serialized [Text] -> ([ClosureInfo], JStat) blocks
--
--  file layout:
--   - header ["GHCJSOBJ", length of symbol table, length of dependencies, length of index]
--   - compiler version tag
--   - symbol table
--   - dependency info
--   - closureinfo index
--   - closureinfo data (offsets described by index)
--
-----------------------------------------------------------------------------

module GHC.StgToJS.Object
  ( object
  , object'
  , readDepsFile
  , readDepsFileEither
  , hReadDeps
  , hReadDepsEither
  , readDeps, readDepsMaybe
  , readObjectFile
  , readObjectFileKeys
  , readObject
  , readObjectKeys
  , serializeStat
  , emptySymbolTable
  , isGlobalUnit
  , isExportsUnit -- XXX verify that this is used
  -- XXX probably should instead do something that just inspects the header instead of exporting it
  , Header(..), getHeader, moduleNameTag
  , SymbolTable
  , ObjUnit (..)
  , Deps (..), BlockDeps (..), DepsLocation (..)
  , ExpFun (..), ExportedFun (..)
  , versionTag, versionTagLength
  )
where

import GHC.Prelude

import           Control.Exception (bracket)
import           Control.Monad

import           Data.Array
import           Data.Monoid
import qualified Data.Binary     as DB
import qualified Data.Binary.Get as DB
import qualified Data.Binary.Put as DB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C8 (pack, unpack)
import qualified Data.ByteString.Short as SBS
import           Data.Function (on)
import           Data.Int
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import           Data.IORef
import           Data.List (sortBy, sortOn)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import           Data.Word
import           Data.Char (isSpace)

import           GHC.Generics
import           GHC.Settings.Constants (hiVersion)

import           System.IO (openBinaryFile, withBinaryFile, Handle,
                            hClose, hSeek, SeekMode(..), IOMode(..) )

import GHC.JS.Syntax
import GHC.StgToJS.Types

import GHC.Unit.Module

import GHC.Data.FastString

import GHC.Types.Unique.Map
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import GHC.Utils.Binary hiding (SymbolTable)
import GHC.Utils.Misc
import GHC.Utils.Outputable (ppr, Outputable, hcat, vcat, text)

data Header = Header
  { hdrModuleName :: !BS.ByteString
  , hdrSymbsLen   :: !Int64
  , hdrDepsLen    :: !Int64
  , hdrIdxLen     :: !Int64
  } deriving (Eq, Ord, Show)

-- | dependencies for a single module
data Deps = Deps
  { depsModule          :: !Module                 -- ^ module
  , depsRequired        :: !IntSet                 -- ^ blocks that always need to be linked when this object is loaded (e.g. everything that contains initializer code or foreign exports)
  , depsHaskellExported :: !(Map ExportedFun Int)  -- ^ exported Haskell functions -> block
  , depsBlocks          :: !(Array Int BlockDeps)  -- ^ info about each block
  } deriving (Generic)

instance Outputable Deps where
  ppr d = vcat
    [ hcat [ text "module: ", pprModule (depsModule d) ]
    , hcat [ text "exports: ", ppr (M.keys (depsHaskellExported d)) ]
    ]

-- | Where are the dependencies
data DepsLocation = ObjectFile  FilePath           -- ^ In an object file at path
                  | ArchiveFile FilePath           -- ^ In a Ar file at path
                  | InMemory    String ByteString  -- ^ In memory
                  deriving (Eq, Show)

instance Outputable DepsLocation where
  ppr x = text (show x)

data BlockDeps = BlockDeps
  { blockBlockDeps       :: [Int]         -- ^ dependencies on blocks in this object
  , blockFunDeps         :: [ExportedFun] -- ^ dependencies on exported symbols in other objects
  -- , blockForeignExported :: [ExpFun]
  -- , blockForeignImported :: [ForeignRef]
  } deriving (Generic)

data ExpFun = ExpFun
  { isIO   :: !Bool
  , args   :: [JSFFIType]
  , result :: !JSFFIType
  } deriving (Eq, Ord, Show)

trim :: String -> String
trim = let f = dropWhile isSpace . reverse in f . f

{- | we use the convention that the first unit (0) is a module-global
     unit that's always included when something from the module
     is loaded. everything in a module implicitly depends on the
     global block. the global unit itself can't have dependencies
 -}
isGlobalUnit :: Int -> Bool
isGlobalUnit n = n == 0

isExportsUnit :: Int -> Bool
isExportsUnit n = n == 1

data JSFFIType
  = Int8Type
  | Int16Type
  | Int32Type
  | Int64Type
  | Word8Type
  | Word16Type
  | Word32Type
  | Word64Type
  | DoubleType
  | ByteArrayType
  | PtrType
  | RefType
  deriving (Show, Ord, Eq, Enum)

data ExportedFun = ExportedFun
  { funModule  :: !Module
  , funSymbol  :: !LexicalFastString
  } deriving (Eq, Ord)

instance Outputable ExportedFun where
  ppr (ExportedFun m f) = vcat
    [ hcat [ text "module: ", pprModule m ]
    , hcat [ text "symbol: ", ppr f ]
    ]

-- we need to store the size separately, since getting a HashMap's size is O(n)
data SymbolTable
  = SymbolTable !Int !(UniqMap FastString Int)

emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable 0 emptyUniqMap

insertSymbol :: FastString -> SymbolTable -> (SymbolTable, Int)
insertSymbol s st@(SymbolTable n t) =
  case lookupUniqMap t s of
    Just k  -> (st, k)
    Nothing -> (SymbolTable (n+1) (addToUniqMap t s n), n)

data ObjEnv = ObjEnv
  { oeSymbols :: SymbolTableR
  , _oeName    :: String
  }

data SymbolTableR = SymbolTableR
  { strText   :: Array Int FastString
  , _strString :: Array Int String
  }

runGetS :: HasDebugCallStack => String -> SymbolTableR -> (BinHandle -> IO a) -> ByteString -> IO a
runGetS name st m bl = do
  let bs = B.toStrict bl
  bh0 <- unpackBinBuffer (BS.length bs) bs
  let bh = setUserData bh0 (newReadState undefined (readTable (ObjEnv st name)))
  m bh

runPutS :: SymbolTable -> (BinHandle -> IO ()) -> IO (SymbolTable, ByteString)
runPutS st ps = do
  bh0 <- openBinMem (1024 * 1024)
  t_r <- newIORef st
  let bh = setUserData bh0 (newWriteState undefined undefined (insertTable t_r))
  ps bh
  (,) <$> readIORef t_r <*> (B.fromStrict <$> packBinBuffer bh)

insertTable :: IORef SymbolTable -> BinHandle -> FastString -> IO ()
insertTable t_r bh s = do
  t <- readIORef t_r
  let (t', n) = insertSymbol s t
  writeIORef t_r t'
  put_ bh n
  return ()

readTable :: ObjEnv -> BinHandle -> IO FastString
readTable e bh = do
  n :: Int <- get bh
  return $ strText (oeSymbols e) ! fromIntegral n

-- unexpected :: String -> GetS a
-- unexpected err = ask >>= \e ->
--   error (oeName e ++ ": " ++ err)

-- one toplevel block in the object file
data ObjUnit = ObjUnit
  { oiSymbols  :: [FastString]   -- toplevel symbols (stored in index)
  , oiClInfo   :: [ClosureInfo]  -- closure information of all closures in block
  , oiStatic   :: [StaticInfo]   -- static closure data
  , oiStat     :: JStat          -- the code
  , oiRaw      :: FastString     -- raw JS code
  , oiFExports :: [ExpFun]
  , oiFImports :: [ForeignJSRef]
  }

-- | build an object file
object :: ModuleName     -- ^ the module name
       -> Deps           -- ^ the dependencies
       -> [ObjUnit]      -- ^ units, the first unit is the module-global one
       -> IO ByteString  -- ^ serialized object
object mname ds units = do
  (xs, symbs) <- go emptySymbolTable units
  object' mname symbs ds xs
  where
    go st0 (ObjUnit sy cl si st str fe fi : ys) = do
      (st1, bs ) <- serializeStat st0 cl si st str fe fi
      (bss, st2) <- go st1 ys
      return ((sy,B.fromChunks [bs]):bss, st2)
    go st0 [] = return ([], st0)

serializeStat :: SymbolTable
              -> [ClosureInfo]
              -> [StaticInfo]
              -> JStat
              -> FastString
              -> [ExpFun]
              -> [ForeignJSRef]
              -> IO (SymbolTable, BS.ByteString)
serializeStat st ci si s sraw fe fi = do
  -- TODO: Did any of the Objectable instances previously used here interact with the `State`?
  (st', bs) <- runPutS st $ \bh -> do
                  put_ bh ci
                  put_ bh si
                  put_ bh s
                  put_ bh sraw
                  put_ bh fe
                  put_ bh fi
  return (st', B.toStrict bs)

-- tag to store the module name in the object file
moduleNameTag :: ModuleName -> BS.ByteString
moduleNameTag (ModuleName fs) = case compare len moduleNameLength of
  EQ -> tag
  LT -> tag <> BS.replicate (moduleNameLength - len) 0 -- pad with 0s
  GT -> BS.drop (len - moduleNameLength) tag           -- take only the ending chars
  where
    !tag = SBS.fromShort (fs_sbs fs)
    !len = n_chars fs

object'
  :: ModuleName                  -- ^ module
  -> SymbolTable                 -- ^ final symbol table
  -> Deps                        -- ^ dependencies
  -> [([FastString],ByteString)] -- ^ serialized units and their exported symbols, the first unit is module-global
  -> IO ByteString
object' mod_name st0 deps0 os = do
  (sti, idx) <- putIndex st0 os
  let symbs  =  putSymbolTable sti
  deps1      <- putDepsSection deps0
  let hdr = putHeader (Header (moduleNameTag mod_name) (bl symbs) (bl deps1) (bl idx))
  return $ hdr <> symbs <> deps1 <> idx <> mconcat (map snd os)
  where
    bl = fromIntegral . B.length

putIndex :: SymbolTable -> [([FastString], ByteString)] -> IO (SymbolTable, ByteString)
putIndex st xs = runPutS st (\bh -> put_ bh $ zip symbols offsets)
  where
    (symbols, values) = unzip xs
    offsets = scanl (+) 0 (map B.length values)

getIndex :: HasDebugCallStack => String -> SymbolTableR -> ByteString -> IO [([FastString], Int64)]
getIndex name st bs = runGetS name st get bs

putDeps :: SymbolTable -> Deps -> IO (SymbolTable, ByteString)
putDeps st deps = runPutS st (\bh -> put_ bh deps)

getDeps :: HasDebugCallStack => String -> SymbolTableR -> ByteString -> IO Deps
getDeps name st bs = runGetS name st get bs

toI32 :: Int -> Int32
toI32 = fromIntegral

fromI32 :: Int32 -> Int
fromI32 = fromIntegral

putDepsSection :: Deps -> IO ByteString
putDepsSection deps = do
  (st, depsbs) <- putDeps emptySymbolTable deps
  let stbs     = putSymbolTable st
  return $ DB.runPut (DB.putWord32le (fromIntegral $ B.length stbs)) <> stbs <> depsbs

getDepsSection :: HasDebugCallStack => String -> ByteString -> IO Deps
getDepsSection name bs =
  let symbsLen = fromIntegral $ DB.runGet DB.getWord32le bs
      symbs    = getSymbolTable (B.drop 4 bs)
  in  getDeps name symbs (B.drop (4+symbsLen) bs)

instance Binary Deps where
  put_ bh (Deps m r e b) = do
      put_ bh m
      put_ bh (map toI32 $ IS.toList r)
      put_ bh (map (\(x,y) -> (x, toI32 y)) $ M.toList e)
      put_ bh (elems b)
  get bh = Deps <$> get bh
             <*> (IS.fromList . map fromI32 <$> get bh)
             <*> (M.fromList . map (\(x,y) -> (x, fromI32 y)) <$> get bh)
             <*> ((\xs -> listArray (0, length xs - 1) xs) <$> get bh)

instance Binary BlockDeps where
  put_ bh (BlockDeps bbd bfd) = put_ bh bbd >> put_ bh bfd
  get bh = BlockDeps <$> get bh <*> get bh

instance Binary ForeignJSRef where
  put_ bh (ForeignJSRef span pat safety cconv arg_tys res_ty) =
    put_ bh span >> put_ bh pat >> putEnum bh safety >> putEnum bh cconv >> put_ bh arg_tys >> put_ bh res_ty
  get bh = ForeignJSRef <$> get bh <*> get bh <*> getEnum bh <*> getEnum bh <*> get bh <*> get bh

instance Binary ExpFun where
  put_ bh (ExpFun isIO args res) = put_ bh isIO >> put_ bh args >> put_ bh res
  get bh                        = ExpFun <$> get bh <*> get bh <*> get bh

-- | reads only the part necessary to get bh the dependencies
--   so it's potentially more efficient than readDeps <$> B.readFile file
readDepsFile :: FilePath -> IO Deps
readDepsFile file = withBinaryFile file ReadMode (hReadDeps file)

readDepsFileEither :: FilePath -> IO (Either String Deps)
readDepsFileEither file = withBinaryFile file ReadMode (hReadDepsEither file)

hReadDeps :: String -> Handle -> IO Deps
hReadDeps name h = do
  res <- hReadDepsEither name h
  case res of
    Left err -> error ("hReadDeps: not a valid GHCJS object: " ++ name ++ "\n    " ++ err)
    Right deps -> pure deps

hReadDepsEither :: String -> Handle -> IO (Either String Deps)
hReadDepsEither name h = do
  mhdr <- getHeader <$> B.hGet h headerLength
  case mhdr of
    Left err -> pure (Left err)
    Right hdr -> do
      hSeek h RelativeSeek (fromIntegral $ hdrSymbsLen hdr)
      Right <$> (getDepsSection name =<< B.hGet h (fromIntegral $ hdrDepsLen hdr))

readDepsEither :: String -> ByteString -> IO (Either String Deps)
readDepsEither name bs =
  case getHeader bs of
    Left err -> return $ Left err
    Right hdr ->
      let depsStart = fromIntegral headerLength + fromIntegral (hdrSymbsLen hdr)
      in  Right <$> getDepsSection name (B.drop depsStart bs)


-- | call with contents of the file
readDeps :: String -> B.ByteString -> IO Deps
readDeps name bs = do
  mdeps <- readDepsEither name bs
  case mdeps of
    Left err -> error ("readDeps: not a valid GHCJS object: " ++ name ++ "\n   " ++ err)
    Right deps -> return deps

readDepsMaybe :: String -> ByteString -> IO (Maybe Deps)
readDepsMaybe name bs = either (const Nothing) Just <$> readDepsEither name bs

-- | extract the linkable units from an object file
readObjectFile :: FilePath -> IO [ObjUnit]
readObjectFile = readObjectFileKeys (\_ _ -> True)

readObjectFileKeys :: (Int -> [FastString] -> Bool) -> FilePath -> IO [ObjUnit]
readObjectFileKeys p file = bracket (openBinaryFile file ReadMode) hClose $ \h -> do
  mhdr <- getHeader <$> B.hGet h headerLength
  case mhdr of
    Left err -> error ("readObjectFileKeys: not a valid GHCJS object: " ++ file ++ "\n    " ++ err)
    Right hdr -> do
      bss <- B.hGet h (fromIntegral $ hdrSymbsLen hdr)
      hSeek h RelativeSeek (fromIntegral $ hdrDepsLen hdr)
      bsi <- B.fromStrict <$> BS.hGetContents h
      readObjectKeys' file p (getSymbolTable bss) bsi (B.drop (fromIntegral $ hdrIdxLen hdr) bsi)

readObject :: String -> ByteString -> IO [ObjUnit]
readObject name = readObjectKeys name (\_ _ -> True)

readObjectKeys :: HasDebugCallStack => String -> (Int -> [FastString] -> Bool) -> ByteString -> IO [ObjUnit]
readObjectKeys name p bs =
  case getHeader bs of
    Left err -> error ("readObjectKeys: not a valid GHCJS object: " ++ name ++ "\n    " ++ err)
    Right hdr ->
      let bssymbs = B.drop (fromIntegral headerLength) bs
          bsidx   = B.drop (fromIntegral $ hdrSymbsLen hdr + hdrDepsLen hdr) bssymbs
          bsobjs  = B.drop (fromIntegral $ hdrIdxLen hdr) bsidx
      in readObjectKeys' name p (getSymbolTable bssymbs) bsidx bsobjs

readObjectKeys' :: HasDebugCallStack
                => String
                -> (Int -> [FastString] -> Bool)
                -> SymbolTableR
                -> ByteString
                -> ByteString
                -> IO [ObjUnit]
readObjectKeys' name p st bsidx bsobjs = do
  idx <- getIndex name st bsidx
  catMaybes <$> zipWithM readObj [0..] idx
  where
    readObj n (x,off)
      | p n x = do
         (ci, si, s, sraw, fe, fi) <- runGetS name st getOU (B.drop off bsobjs)
         return $ Just (ObjUnit x ci si s sraw fe fi)
      | otherwise = return Nothing
    getOU bh = (,,,,,) <$> get bh <*> get bh <*> get bh <*> get bh <*> get bh <*> get bh

getSymbolTable :: HasDebugCallStack => ByteString -> SymbolTableR
getSymbolTable bs = SymbolTableR (listArray (0,n-1) xs) (listArray (0,n-1) (map unpackFS xs))
  where
    (n,xs) = DB.runGet getter bs
    getter :: DB.Get (Int, [FastString])
    getter = do
      l <- DB.getWord32le
      let l' = fromIntegral l
      (l',) <$> replicateM l' DB.get

putSymbolTable :: SymbolTable -> ByteString
putSymbolTable (SymbolTable _ hm) = st
    where
      st = DB.runPut $ do
              DB.putWord32le (fromIntegral $ length xs)
              mapM_ DB.put xs
      xs :: [FastString]
      xs = map fst . sortBy (compare `on` snd) . nonDetEltsUniqMap $ hm
      -- We can use `nonDetEltsUniqMap` because the paired `Int`s introduce ordering.

headerLength :: Int
headerLength = 32 + versionTagLength + moduleNameLength

-- human readable version string in object
versionTag :: ByteString
versionTag = B.take 32 . C8.pack $ show hiVersion ++ replicate versionTagLength ' '

versionTagLength :: Int
versionTagLength = 32

-- last part of the module name, to disambiguate files
moduleNameLength :: Int
moduleNameLength = 128

getHeader :: HasDebugCallStack => ByteString -> Either String Header
getHeader bs
  | B.length bs < fromIntegral headerLength = Left "not enough input, file truncated?"
  | magic /= "GHCJSOBJ"                     = Left $ "magic number incorrect, not a JavaScript .o file?"
  | tag   /= versionTag                     = Left $ "incorrect version, expected " ++ show hiVersion ++
                                                     " but got " ++ (trim . C8.unpack $ tag)
  | otherwise                               = Right (Header mn sl dl il)
   where
     g                    = fromIntegral <$> DB.getWord64le
     (magic, tag, mn, sl, dl, il) = DB.runGet ((,,,,,) <$> DB.getByteString 8
                                                       <*> DB.getLazyByteString (fromIntegral versionTagLength)
                                                       <*> DB.getByteString (fromIntegral moduleNameLength)
                                                       <*> g
                                                       <*> g
                                                       <*> g
                                      ) bs

putHeader :: Header -> ByteString
putHeader (Header mn sl dl il) = DB.runPut $ do
  DB.putByteString "GHCJSOBJ"
  DB.putLazyByteString versionTag
  DB.putByteString mn
  mapM_ (DB.putWord64le . fromIntegral) [sl, dl, il]

tag :: BinHandle -> Word8 -> IO ()
tag = put_

getTag :: BinHandle -> IO Word8
getTag = get

-- instance Binary ShortText where
--   put_ bh t = put_ bh (mkFastString $ ST.unpack t)
--   get bh = ST.pack . unpackFS <$> get bh
  -- put_ bh t = do
    -- symbols <- St.get
    -- let (symbols', n) = insertSymbol t symbols
    -- St.put symbols'
    -- lift (DB.putWord32le $ fromIntegral n)
  -- get bh = do
    -- st <- oeSymbols <$> ask
    -- n <- lift DB.getWord32le
    -- return (strText st ! fromIntegral n)

instance Binary JStat where
  put_ bh (DeclStat i)         = tag bh 1  >> put_ bh i
  put_ bh (ReturnStat e)       = tag bh 2  >> put_ bh e
  put_ bh (IfStat e s1 s2)     = tag bh 3  >> put_ bh e  >> put_ bh s1 >> put_ bh s2
  put_ bh (WhileStat b e s)    = tag bh 4  >> put_ bh b  >> put_ bh e  >> put_ bh s
  put_ bh (ForInStat b i e s)  = tag bh 5  >> put_ bh b  >> put_ bh i  >> put_ bh e  >> put_ bh s
  put_ bh (SwitchStat e ss s)  = tag bh 6  >> put_ bh e  >> put_ bh ss >> put_ bh s
  put_ bh (TryStat s1 i s2 s3) = tag bh 7  >> put_ bh s1 >> put_ bh i  >> put_ bh s2 >> put_ bh s3
  put_ bh (BlockStat xs)       = tag bh 8  >> put_ bh xs
  put_ bh (ApplStat e es)      = tag bh 9  >> put_ bh e  >> put_ bh es
  put_ bh (UOpStat o e)        = tag bh 10 >> put_ bh o  >> put_ bh e
  put_ bh (AssignStat e1 e2)   = tag bh 11 >> put_ bh e1 >> put_ bh e2
  put_ _  (UnsatBlock {})      = error "put_ bh JStat: UnsatBlock"
  put_ bh (LabelStat l s)      = tag bh 12 >> put_ bh l  >> put_ bh s
  put_ bh (BreakStat ml)       = tag bh 13 >> put_ bh ml
  put_ bh (ContinueStat ml)    = tag bh 14 >> put_ bh ml
  get bh = getTag bh >>= \case
    1  -> DeclStat     <$> get bh
    2  -> ReturnStat   <$> get bh
    3  -> IfStat       <$> get bh <*> get bh <*> get bh
    4  -> WhileStat    <$> get bh <*> get bh <*> get bh
    5  -> ForInStat    <$> get bh <*> get bh <*> get bh <*> get bh
    6  -> SwitchStat   <$> get bh <*> get bh <*> get bh
    7  -> TryStat      <$> get bh <*> get bh <*> get bh <*> get bh
    8  -> BlockStat    <$> get bh
    9  -> ApplStat     <$> get bh <*> get bh
    10 -> UOpStat      <$> get bh <*> get bh
    11 -> AssignStat   <$> get bh <*> get bh
    12 -> LabelStat    <$> get bh <*> get bh
    13 -> BreakStat    <$> get bh
    14 -> ContinueStat <$> get bh
    n -> error ("Binary get bh JStat: invalid tag: " ++ show n)

instance Binary JExpr where
  put_ bh (ValExpr v)          = tag bh 1 >> put_ bh v
  put_ bh (SelExpr e i)        = tag bh 2 >> put_ bh e  >> put_ bh i
  put_ bh (IdxExpr e1 e2)      = tag bh 3 >> put_ bh e1 >> put_ bh e2
  put_ bh (InfixExpr o e1 e2)  = tag bh 4 >> put_ bh o  >> put_ bh e1 >> put_ bh e2
  put_ bh (UOpExpr o e)        = tag bh 5 >> put_ bh o  >> put_ bh e
  put_ bh (IfExpr e1 e2 e3)    = tag bh 6 >> put_ bh e1 >> put_ bh e2 >> put_ bh e3
  put_ bh (ApplExpr e es)      = tag bh 7 >> put_ bh e  >> put_ bh es
  put_ _  (UnsatExpr {})       = error "put_ bh JExpr: UnsatExpr"
  get bh = getTag bh >>= \case
    1 -> ValExpr   <$> get bh
    2 -> SelExpr   <$> get bh <*> get bh
    3 -> IdxExpr   <$> get bh <*> get bh
    4 -> InfixExpr <$> get bh <*> get bh <*> get bh
    5 -> UOpExpr   <$> get bh <*> get bh
    6 -> IfExpr    <$> get bh <*> get bh <*> get bh
    7 -> ApplExpr  <$> get bh <*> get bh
    n -> error ("Binary get bh JExpr: invalid tag: " ++ show n)

instance Binary JVal where
  put_ bh (JVar i)      = tag bh 1 >> put_ bh i
  put_ bh (JList es)    = tag bh 2 >> put_ bh es
  put_ bh (JDouble d)   = tag bh 3 >> put_ bh d
  put_ bh (JInt i)      = tag bh 4 >> put_ bh i
  put_ bh (JStr xs)     = tag bh 5 >> put_ bh xs
  put_ bh (JRegEx xs)   = tag bh 6 >> put_ bh xs
  put_ bh (JHash m)     = tag bh 7 >> put_ bh (sortOn (LexicalFastString . fst) $ nonDetEltsUniqMap m)
  put_ bh (JFunc is s)  = tag bh 8 >> put_ bh is >> put_ bh s
  put_ _  (UnsatVal {}) = error "put_ bh JVal: UnsatVal"
  get bh = getTag bh >>= \case
    1 -> JVar    <$> get bh
    2 -> JList   <$> get bh
    3 -> JDouble <$> get bh
    4 -> JInt    <$> get bh
    5 -> JStr    <$> get bh
    6 -> JRegEx  <$> get bh
    7 -> JHash . listToUniqMap <$> get bh
    8 -> JFunc   <$> get bh <*> get bh
    n -> error ("Binary get bh JVal: invalid tag: " ++ show n)

instance Binary Ident where
  put_ bh (TxtI xs) = put_ bh xs
  get bh = TxtI <$> get bh

-- we need to preserve NaN and infinities, unfortunately the Binary instance for Double does not do this
instance Binary SaneDouble where
  put_ bh (SaneDouble d)
    | isNaN d               = tag bh 1
    | isInfinite d && d > 0 = tag bh 2
    | isInfinite d && d < 0 = tag bh 3
    | isNegativeZero d      = tag bh 4
    | otherwise             = tag bh 5 >> put_ bh (castDoubleToWord64 d)
  get bh = getTag bh >>= \case
    1 -> pure $ SaneDouble (0    / 0)
    2 -> pure $ SaneDouble (1    / 0)
    3 -> pure $ SaneDouble ((-1) / 0)
    4 -> pure $ SaneDouble (-0)
    5 -> SaneDouble . castWord64ToDouble <$> get bh
    n -> error ("Binary get bh SaneDouble: invalid tag: " ++ show n)

instance Binary ClosureInfo where
  put_ bh (ClosureInfo v regs name layo typ static) = do
    put_ bh v >> put_ bh regs >> put_ bh name >> put_ bh layo >> put_ bh typ >> put_ bh static
  get bh = ClosureInfo <$> get bh <*> get bh <*> get bh <*> get bh <*> get bh <*> get bh

instance Binary JSFFIType where
  put_ bh = putEnum bh
  get bh = getEnum bh

instance Binary VarType where
  put_ bh = putEnum bh
  get bh = getEnum bh

instance Binary CIRegs where
  put_ bh CIRegsUnknown       = tag bh 1
  put_ bh (CIRegs skip types) = tag bh 2 >> put_ bh skip >> put_ bh types
  get bh = getTag bh >>= \case
    1 -> pure CIRegsUnknown
    2 -> CIRegs <$> get bh <*> get bh
    n -> error ("Binary get bh CIRegs: invalid tag: " ++ show n)

instance Binary JOp where
  put_ bh = putEnum bh
  get bh = getEnum bh

instance Binary JUOp where
  put_ bh = putEnum bh
  get bh = getEnum bh

-- 16 bit sizes should be enough...
instance Binary CILayout where
  put_ bh CILayoutVariable           = tag bh 1
  put_ bh (CILayoutUnknown size)     = tag bh 2 >> put_ bh size
  put_ bh (CILayoutFixed size types) = tag bh 3 >> put_ bh size >> put_ bh types
  get bh = getTag bh >>= \case
    1 -> pure CILayoutVariable
    2 -> CILayoutUnknown <$> get bh
    3 -> CILayoutFixed   <$> get bh <*> get bh
    n -> error ("Binary get bh CILayout: invalid tag: " ++ show n)

instance Binary CIStatic where
  put_ bh (CIStaticRefs refs) = tag bh 1 >> put_ bh refs
  get bh = getTag bh >>= \case
    1 -> CIStaticRefs <$> get bh
    n -> error ("Binary get bh CIStatic: invalid tag: " ++ show n)

instance Binary CIType where
  put_ bh (CIFun arity regs) = tag bh 1 >> put_ bh arity >> put_ bh regs
  put_ bh CIThunk            = tag bh 2
  put_ bh (CICon conTag)     = tag bh 3 >> put_ bh conTag
  put_ bh CIPap              = tag bh 4
  put_ bh CIBlackhole        = tag bh 5
  put_ bh CIStackFrame       = tag bh 6
  get bh = getTag bh >>= \case
    1 -> CIFun <$> get bh <*> get bh
    2 -> pure CIThunk
    3 -> CICon <$> get bh
    4 -> pure CIPap
    5 -> pure CIBlackhole
    6 -> pure CIStackFrame
    n -> error ("Binary get bh CIType: invalid tag: " ++ show n)

instance Binary ExportedFun where
  put_ bh (ExportedFun modu symb) = put_ bh modu >> put_ bh symb
  get bh = ExportedFun <$> get bh <*> get bh

instance DB.Binary Module where
  put (Module unit mod_name) = DB.put unit >> DB.put mod_name
  get = Module <$> DB.get <*> DB.get

instance DB.Binary ModuleName where
  put (ModuleName fs) = DB.put fs
  get = ModuleName <$> DB.get

instance DB.Binary Unit where
  put = \case
    RealUnit (Definite uid) -> DB.put (0 :: Int) >> DB.put uid
    VirtUnit uid            -> DB.put (1 :: Int) >> DB.put uid
    HoleUnit                -> DB.put (2 :: Int)
  get = DB.get >>= \case
    (0 :: Int) -> RealUnit . Definite <$> DB.get
    1          -> VirtUnit              <$> DB.get
    _          -> pure HoleUnit

instance DB.Binary UnitId where
  put (UnitId fs) = DB.put fs
  get = UnitId <$> DB.get

instance DB.Binary InstantiatedUnit where
  put indef = do
    DB.put (instUnitInstanceOf indef)
    DB.put (instUnitInsts indef)
  get = mkInstantiatedUnitSorted <$> DB.get <*> DB.get

instance DB.Binary FastString where
  put fs = DB.put (unpackFS fs)
  get = mkFastString <$> DB.get

putEnum :: Enum a => BinHandle -> a -> IO ()
putEnum bh x | n > 65535 = error ("putEnum: out of range: " ++ show n)
             | otherwise = put_ bh n
  where n = fromIntegral $ fromEnum x :: Word16

getEnum :: Enum a => BinHandle -> IO a
getEnum bh = toEnum . fromIntegral <$> (get bh :: IO Word16)

instance Binary StaticInfo where
  put_ bh (StaticInfo ident val cc) = put_ bh ident >> put_ bh val >> put_ bh cc
  get bh = StaticInfo <$> get bh <*> get bh <*> get bh

instance Binary StaticVal where
  put_ bh (StaticFun f args)   = tag bh 1 >> put_ bh f  >> put_ bh args
  put_ bh (StaticThunk t)      = tag bh 2 >> put_ bh t
  put_ bh (StaticUnboxed u)    = tag bh 3 >> put_ bh u
  put_ bh (StaticData dc args) = tag bh 4 >> put_ bh dc >> put_ bh args
  put_ bh (StaticList xs t)    = tag bh 5 >> put_ bh xs >> put_ bh t
  get bh = getTag bh >>= \case
    1 -> StaticFun     <$> get bh <*> get bh
    2 -> StaticThunk   <$> get bh
    3 -> StaticUnboxed <$> get bh
    4 -> StaticData    <$> get bh <*> get bh
    5 -> StaticList    <$> get bh <*> get bh
    n -> error ("Binary get bh StaticVal: invalid tag " ++ show n)

instance Binary StaticUnboxed where
  put_ bh (StaticUnboxedBool b)           = tag bh 1 >> put_ bh b
  put_ bh (StaticUnboxedInt i)            = tag bh 2 >> put_ bh i
  put_ bh (StaticUnboxedDouble d)         = tag bh 3 >> put_ bh d
  put_ bh (StaticUnboxedString str)       = tag bh 4 >> put_ bh str
  put_ bh (StaticUnboxedStringOffset str) = tag bh 5 >> put_ bh str
  get bh = getTag bh >>= \case
    1 -> StaticUnboxedBool         <$> get bh
    2 -> StaticUnboxedInt          <$> get bh
    3 -> StaticUnboxedDouble       <$> get bh
    4 -> StaticUnboxedString       <$> get bh
    5 -> StaticUnboxedStringOffset <$> get bh
    n -> error ("Binary get bh StaticUnboxed: invalid tag " ++ show n)

instance Binary StaticArg where
  put_ bh (StaticObjArg i)      = tag bh 1 >> put_ bh i
  put_ bh (StaticLitArg p)      = tag bh 2 >> put_ bh p
  put_ bh (StaticConArg c args) = tag bh 3 >> put_ bh c >> put_ bh args
  get bh = getTag bh >>= \case
    1 -> StaticObjArg <$> get bh
    2 -> StaticLitArg <$> get bh
    3 -> StaticConArg <$> get bh <*> get bh
    n -> error ("Binary get bh StaticArg: invalid tag " ++ show n)

instance Binary StaticLit where
  put_ bh (BoolLit b)    = tag bh 1 >> put_ bh b
  put_ bh (IntLit i)     = tag bh 2 >> put_ bh i
  put_ bh NullLit        = tag bh 3
  put_ bh (DoubleLit d)  = tag bh 4 >> put_ bh d
  put_ bh (StringLit t)  = tag bh 5 >> put_ bh t
  put_ bh (BinLit b)     = tag bh 6 >> put_ bh b
  put_ bh (LabelLit b t) = tag bh 7 >> put_ bh b >> put_ bh t
  get bh = getTag bh >>= \case
    1 -> BoolLit   <$> get bh
    2 -> IntLit    <$> get bh
    3 -> pure NullLit
    4 -> DoubleLit <$> get bh
    5 -> StringLit <$> get bh
    6 -> BinLit    <$> get bh
    7 -> LabelLit  <$> get bh <*> get bh
    n -> error ("Binary get bh StaticLit: invalid tag " ++ show n)
