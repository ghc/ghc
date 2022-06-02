{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Serialization/deserialization of binary .o files for the JavaScript backend
--
-- The .o files contain dependency information and generated code.
--
-- All strings are mapped to a central string table, which helps reduce
-- file size and gives us efficient hash consing on read
--
-- Binary intermediate JavaScript object files:
--  serialized [Text] -> ([ClosureInfo], JStat) blocks
--
-- file layout:
--  - header ["GHCJSOBJ", length of symbol table, length of dependencies, length of index]
--  - compiler version tag
--  - symbol table
--  - dependency info
--  - closureinfo index
--  - closureinfo data (offsets described by index)
--
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
  , Deps (..), BlockDeps (..)
  , ExpFun (..), ExportedFun (..)
  , versionTag, versionTagLength
  )
where

import GHC.Prelude

import           Control.Exception (bracket)
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State as St

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
import           Data.List (sortBy)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import           Data.Set (Set)
import qualified Data.Set as S
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
import GHC.Data.ShortText as ST

import GHC.Utils.Misc

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
  , depsBlocks          :: !(Array Int  BlockDeps) -- ^ info about each block
  } deriving (Generic)

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

-- fixme document, exports unit is always linked
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
  , funSymbol  :: !ShortText
  } deriving (Eq, Ord)

-- we need to store the size separately, since getting a HashMap's size is O(n)
data SymbolTable
  = SymbolTable !Int !(Map ShortText Int)
  deriving (Show)

emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable 0 M.empty

insertSymbol :: ShortText -> SymbolTable -> (SymbolTable, Int)
insertSymbol s st@(SymbolTable n t) =
  case M.lookup s t of
    Just k  -> (st, k)
    Nothing -> (SymbolTable (n+1) (M.insert s n t), n)

data ObjEnv = ObjEnv
  { oeSymbols :: SymbolTableR
  , oeName    :: String
  }

data SymbolTableR = SymbolTableR
  { strText   :: Array Int ShortText
  , strString :: Array Int String
  }

type PutSM = St.StateT SymbolTable DB.PutM -- FIXME: StateT isn't strict enough apparently
type PutS  = PutSM ()
type GetS  = ReaderT ObjEnv DB.Get

class Objectable a where
  put :: a -> PutS
  get :: GetS a
  putList :: [a] -> PutS
  putList = putListOf put
  getList :: GetS [a]
  getList = getListOf get

runGetS :: HasDebugCallStack => String -> SymbolTableR -> GetS a -> ByteString -> a
runGetS name st m bs = DB.runGet (runReaderT m (ObjEnv st name)) bs

runPutS :: SymbolTable -> PutS -> (SymbolTable, ByteString)
runPutS st ps = DB.runPutM (St.execStateT ps st)

unexpected :: String -> GetS a
unexpected err = ask >>= \e ->
  error (oeName e ++ ": " ++ err)

-- one toplevel block in the object file
data ObjUnit = ObjUnit
  { oiSymbols  :: [ShortText]    -- toplevel symbols (stored in index)
  , oiClInfo   :: [ClosureInfo]  -- closure information of all closures in block
  , oiStatic   :: [StaticInfo]   -- static closure data
  , oiStat     :: JStat          -- the code
  , oiRaw      :: ShortText      -- raw JS code
  , oiFExports :: [ExpFun]
  , oiFImports :: [ForeignJSRef]
  }

-- | build an object file
object :: ModuleName  -- ^ the module name
       -> Deps        -- ^ the dependencies
       -> [ObjUnit]   -- ^ units, the first unit is the module-global one
       -> ByteString  -- ^ serialized object
object mname ds units = object' mname symbs ds xs
  where
    (xs, symbs) = go emptySymbolTable units
    go st0 (ObjUnit sy cl si st str fe fi : ys) =
      let (st1, bs)  = serializeStat st0 cl si st str fe fi
          (bss, st2) = go st1 ys
      in  ((sy,B.fromChunks [bs]):bss, st2)
    go st0 [] = ([], st0)

serializeStat :: SymbolTable
              -> [ClosureInfo]
              -> [StaticInfo]
              -> JStat
              -> ShortText
              -> [ExpFun]
              -> [ForeignJSRef]
              -> (SymbolTable, BS.ByteString)
serializeStat st ci si s sraw fe fi =
  let (st', bs) = runPutS st $ do
                    put ci
                    put si
                    put s
                    put sraw
                    put fe
                    put fi
      bs' = B.toStrict bs
  in  (st', bs')

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
  :: ModuleName                 -- ^ module
  -> SymbolTable                -- ^ final symbol table
  -> Deps                       -- ^ dependencies
  -> [([ShortText],ByteString)] -- ^ serialized units and their exported symbols, the first unit is module-global
  -> ByteString
object' mod_name st0 deps0 os = hdr <> symbs <> deps1 <> idx <> mconcat (map snd os)
  where
    hdr          = putHeader (Header (moduleNameTag mod_name) (bl symbs) (bl deps1) (bl idx))
    bl           = fromIntegral . B.length
    deps1        = putDepsSection deps0
    (sti, idx)   = putIndex st0 os
    symbs        = putSymbolTable sti

putIndex :: SymbolTable -> [([ShortText], ByteString)] -> (SymbolTable, ByteString)
putIndex st xs = runPutS st (put $ zip symbols offsets)
  where
    (symbols, values) = unzip xs
    offsets = scanl (+) 0 (map B.length values)

getIndex :: HasDebugCallStack => String -> SymbolTableR -> ByteString -> [([ShortText], Int64)]
getIndex name st bs = runGetS name st get bs

putDeps :: SymbolTable -> Deps -> (SymbolTable, ByteString)
putDeps st deps = runPutS st (put deps)

getDeps :: HasDebugCallStack => String -> SymbolTableR -> ByteString -> Deps
getDeps name st bs = runGetS name st get bs

toI32 :: Int -> Int32
toI32 = fromIntegral

fromI32 :: Int32 -> Int
fromI32 = fromIntegral

putDepsSection :: Deps -> ByteString
putDepsSection deps =
  let (st, depsbs) = putDeps emptySymbolTable deps
      stbs         = putSymbolTable st
  in  DB.runPut (DB.putWord32le (fromIntegral $ B.length stbs)) <> stbs <> depsbs

getDepsSection :: HasDebugCallStack => String -> ByteString -> Deps
getDepsSection name bs =
  let symbsLen = fromIntegral $ DB.runGet DB.getWord32le bs
      symbs    = getSymbolTable (B.drop 4 bs)
  in  getDeps name symbs (B.drop (4+symbsLen) bs)

instance Objectable Deps where
  put (Deps m r e b) = do
      put m
      put (map toI32 $ IS.toList r)
      put (map (\(x,y) -> (x, toI32 y)) $ M.toList e)
      put (elems b)
  get = Deps <$> get
             <*> (IS.fromList . map fromI32 <$> get)
             <*> (M.fromList . map (\(x,y) -> (x, fromI32 y)) <$> get)
             <*> ((\xs -> listArray (0, length xs - 1) xs) <$> get)

instance Objectable BlockDeps where
  put (BlockDeps bbd bfd) = put bbd >> put bfd
  get = BlockDeps <$> get <*> get

instance Objectable ForeignJSRef where
  put (ForeignJSRef span pat safety cconv arg_tys res_ty) =
    put span >> put pat >> putEnum safety >> putEnum cconv >> put arg_tys >> put res_ty
  get = ForeignJSRef <$> get <*> get <*> getEnum <*> getEnum <*> get <*> get

instance Objectable ExpFun where
  put (ExpFun isIO args res) = put isIO >> put args >> put res
  get                        = ExpFun <$> get <*> get <*> get

-- | reads only the part necessary to get the dependencies
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
      Right . getDepsSection name <$> B.hGet h (fromIntegral $ hdrDepsLen hdr)

readDepsEither :: String -> ByteString -> Either String Deps
readDepsEither name bs =
  case getHeader bs of
    Left err -> Left err
    Right hdr ->
      let depsStart = fromIntegral headerLength + fromIntegral (hdrSymbsLen hdr)
      in  Right $ getDepsSection name (B.drop depsStart bs)


-- | call with contents of the file
readDeps :: String -> ByteString -> Deps
readDeps name bs =
  case readDepsEither name bs of
    Left err -> error ("readDeps: not a valid GHCJS object: " ++ name ++ "\n   " ++ err)
    Right deps -> deps

readDepsMaybe :: String -> ByteString -> Maybe Deps
readDepsMaybe name bs = either (const Nothing) Just (readDepsEither name bs)

-- | extract the linkable units from an object file
readObjectFile :: FilePath -> IO [ObjUnit]
readObjectFile = readObjectFileKeys (\_ _ -> True)

readObjectFileKeys :: (Int -> [ShortText] -> Bool) -> FilePath -> IO [ObjUnit]
readObjectFileKeys p file = bracket (openBinaryFile file ReadMode) hClose $ \h -> do
  mhdr <- getHeader <$> B.hGet h headerLength
  case mhdr of
    Left err -> error ("readObjectFileKeys: not a valid GHCJS object: " ++ file ++ "\n    " ++ err)
    Right hdr -> do
      bss <- B.hGet h (fromIntegral $ hdrSymbsLen hdr)
      hSeek h RelativeSeek (fromIntegral $ hdrDepsLen hdr)
      bsi <- B.fromStrict <$> BS.hGetContents h
      return $ readObjectKeys' file p (getSymbolTable bss) bsi (B.drop (fromIntegral $ hdrIdxLen hdr) bsi)

readObject :: String -> ByteString -> [ObjUnit]
readObject name = readObjectKeys name (\_ _ -> True)

readObjectKeys :: HasDebugCallStack => String -> (Int -> [ShortText] -> Bool) -> ByteString -> [ObjUnit]
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
                -> (Int -> [ShortText] -> Bool)
                -> SymbolTableR
                -> ByteString
                -> ByteString
                -> [ObjUnit]
readObjectKeys' name p st bsidx bsobjs = catMaybes (zipWith readObj [0..] idx)
    where
      idx = getIndex name st bsidx
      readObj n (x,off)
        | p n x     = let (ci, si, s, sraw, fe, fi) = runGetS name st ((,,,,,) <$> get <*> get <*> get <*> get <*> get <*> get) (B.drop off bsobjs)
                      in  Just (ObjUnit x ci si s sraw fe fi)
        | otherwise = Nothing

getSymbolTable :: HasDebugCallStack => ByteString -> SymbolTableR
getSymbolTable bs = SymbolTableR (listArray (0,n-1) xs) (listArray (0,n-1) (map ST.unpack xs))
  where
    (n,xs) = DB.runGet getter bs
    getter :: DB.Get (Int, [ShortText])
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
              -- fixme: this is a workaround for some weird issue sometimes causing zero-length
              --        strings when using the Data.Text instance directly
              -- mapM_ (DB.put . TE.encodeUtf8) xs
      xs :: [ShortText]
      xs = map fst . sortBy (compare `on` snd) . M.toList $ hm

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

tag :: Word8 -> PutS
tag x = lift (DB.putWord8 x)

getTag :: GetS Word8
getTag = lift DB.getWord8

instance (Objectable a, Objectable b) => Objectable (a, b) where
  put (x, y) = put x >> put y
  get = (,) <$> get <*> get

instance Objectable a => Objectable [a] where
  put = putList
  get = getList

instance Objectable Char where
  put = lift . DB.putWord32le . fromIntegral . fromEnum
  get = toEnum . fromIntegral <$> lift DB.getWord32le
  putList = put . ST.pack
  getList = do
    st <- oeSymbols <$> ask
    n <- lift DB.getWord32le
    return (strString st ! fromIntegral n)

putListOf :: (a -> PutS) -> [a] -> PutS
putListOf p xs = do
  lift (DB.putWord32le (fromIntegral $ length xs))
  mapM_ p xs

getListOf :: GetS a -> GetS [a]
getListOf g = do
  l <- lift DB.getWord32le
  replicateM (fromIntegral l) g

instance (Ord k, Objectable k, Objectable v) => Objectable (Map k v) where
  put = put . M.toList
  get = M.fromList <$> get

instance (Ord a, Objectable a) => Objectable (Set a) where
  put = put . S.toList
  get = S.fromList <$> get

instance Objectable Word64 where
  put = lift . DB.putWord64le
  get = lift DB.getWord64le

instance Objectable Int64 where
  put = lift . DB.putWord64le . fromIntegral
  get = fromIntegral <$> lift DB.getWord64le

instance Objectable Word32 where
  put = lift . DB.putWord32le
  get = lift DB.getWord32le

instance Objectable Int32 where
  put = lift . DB.putWord32le . fromIntegral
  get = fromIntegral <$> lift DB.getWord32le

instance Objectable a => Objectable (Maybe a) where
  put Nothing  = tag 1
  put (Just x) = tag 2 >> put x
  get = getTag >>= \case
    1 -> pure Nothing
    2 -> Just <$> get
    n -> unexpected ("Objectable get Maybe: invalid tag: " ++ show n)

instance Objectable ShortText where
  put t = do
    symbols <- St.get
    let (symbols', n) = insertSymbol t symbols
    St.put symbols'
    lift (DB.putWord32le $ fromIntegral n)
  get = do
    st <- oeSymbols <$> ask
    n <- lift DB.getWord32le
    return (strText st ! fromIntegral n)

instance Objectable JStat where
  put (DeclStat i)         = tag 1  >> put i
  put (ReturnStat e)       = tag 2  >> put e
  put (IfStat e s1 s2)     = tag 3  >> put e  >> put s1 >> put s2
  put (WhileStat b e s)    = tag 4  >> put b  >> put e  >> put s
  put (ForInStat b i e s)  = tag 5  >> put b  >> put i  >> put e  >> put s
  put (SwitchStat e ss s)  = tag 6  >> put e  >> put ss >> put s
  put (TryStat s1 i s2 s3) = tag 7  >> put s1 >> put i  >> put s2 >> put s3
  put (BlockStat xs)       = tag 8  >> put xs
  put (ApplStat e es)      = tag 9  >> put e  >> put es
  put (UOpStat o e)        = tag 10 >> put o  >> put e
  put (AssignStat e1 e2)   = tag 11 >> put e1 >> put e2
  put (UnsatBlock {})      = error "put JStat: UnsatBlock"
  put (LabelStat l s)      = tag 12 >> put l  >> put s
  put (BreakStat ml)       = tag 13 >> put ml
  put (ContinueStat ml)    = tag 14 >> put ml
  get = getTag >>= \case
    1  -> DeclStat     <$> get
    2  -> ReturnStat   <$> get
    3  -> IfStat       <$> get <*> get <*> get
    4  -> WhileStat    <$> get <*> get <*> get
    5  -> ForInStat    <$> get <*> get <*> get <*> get
    6  -> SwitchStat   <$> get <*> get <*> get
    7  -> TryStat      <$> get <*> get <*> get <*> get
    8  -> BlockStat    <$> get
    9  -> ApplStat     <$> get <*> get
    10 -> UOpStat      <$> get <*> get
    11 -> AssignStat   <$> get <*> get
    12 -> LabelStat    <$> get <*> get
    13 -> BreakStat    <$> get
    14 -> ContinueStat <$> get
    n -> unexpected ("Objectable get JStat: invalid tag: " ++ show n)

instance Objectable JExpr where
  put (ValExpr v)          = tag 1 >> put v
  put (SelExpr e i)        = tag 2 >> put e  >> put i
  put (IdxExpr e1 e2)      = tag 3 >> put e1 >> put e2
  put (InfixExpr o e1 e2)  = tag 4 >> put o  >> put e1 >> put e2
  put (UOpExpr o e)        = tag 5 >> put o  >> put e
  put (IfExpr e1 e2 e3)    = tag 6 >> put e1 >> put e2 >> put e3
  put (ApplExpr e es)      = tag 7 >> put e  >> put es
  put (UnsatExpr {})       = error "put JExpr: UnsatExpr"
  get = getTag >>= \case
    1 -> ValExpr   <$> get
    2 -> SelExpr   <$> get <*> get
    3 -> IdxExpr   <$> get <*> get
    4 -> InfixExpr <$> get <*> get <*> get
    5 -> UOpExpr   <$> get <*> get
    6 -> IfExpr    <$> get <*> get <*> get
    7 -> ApplExpr  <$> get <*> get
    n -> unexpected ("Objectable get JExpr: invalid tag: " ++ show n)

instance Objectable JVal where
  put (JVar i)      = tag 1 >> put i
  put (JList es)    = tag 2 >> put es
  put (JDouble d)   = tag 3 >> put d
  put (JInt i)      = tag 4 >> put i
  put (JStr xs)     = tag 5 >> put xs
  put (JRegEx xs)   = tag 6 >> put xs
  put (JHash m)     = tag 7 >> put (M.toList m)
  put (JFunc is s)  = tag 8 >> put is >> put s
  put (UnsatVal {}) = error "put JVal: UnsatVal"
  get = getTag >>= \case
    1 -> JVar    <$> get
    2 -> JList   <$> get
    3 -> JDouble <$> get
    4 -> JInt    <$> get
    5 -> JStr    <$> get
    6 -> JRegEx  <$> get
    7 -> JHash . M.fromList  <$> get
    8 -> JFunc   <$> get <*> get
    n -> unexpected ("Objectable get JVal: invalid tag: " ++ show n)

instance Objectable Ident where
  put (TxtI xs) = put xs
  get = TxtI <$> get

instance Objectable Integer where
  put = lift . DB.put
  get = lift DB.get

-- we need to preserve NaN and infinities, unfortunately the Binary instance for Double does not do this
instance Objectable SaneDouble where
  put (SaneDouble d)
    | isNaN d               = tag 1
    | isInfinite d && d > 0 = tag 2
    | isInfinite d && d < 0 = tag 3
    | isNegativeZero d      = tag 4
    | otherwise             = tag 5 >> lift (DB.put d)
  get = getTag >>= \case
    1 -> pure $ SaneDouble (0    / 0)
    2 -> pure $ SaneDouble (1    / 0)
    3 -> pure $ SaneDouble ((-1) / 0)
    4 -> pure $ SaneDouble (-0)
    5 -> SaneDouble <$> lift DB.get
    n -> unexpected ("Objectable get SaneDouble: invalid tag: " ++ show n)

instance Objectable ClosureInfo where
  put (ClosureInfo v regs name layo typ static) = do
    put v >> put regs >> put name >> put layo >> put typ >> put static
  get = ClosureInfo <$> get <*> get <*> get <*> get <*> get <*> get

instance Objectable JSFFIType where
  put = putEnum
  get = getEnum

instance Objectable VarType where
  put = putEnum
  get = getEnum

instance Objectable CIRegs where
  put CIRegsUnknown       = tag 1
  put (CIRegs skip types) = tag 2 >> putIW16 skip >> put types
  get = getTag >>= \case
    1 -> pure CIRegsUnknown
    2 -> CIRegs <$> getIW16 <*> get
    n -> unexpected ("Objectable get CIRegs: invalid tag: " ++ show n)

instance Objectable JOp where
  put = putEnum
  get = getEnum

instance Objectable JUOp where
  put = putEnum
  get = getEnum

-- 16 bit sizes should be enough...
instance Objectable CILayout where
  put CILayoutVariable           = tag 1
  put (CILayoutUnknown size)     = tag 2 >> putIW16 size
  put (CILayoutFixed size types) = tag 3 >> putIW16 size >> put types
  get = getTag >>= \case
    1 -> pure CILayoutVariable
    2 -> CILayoutUnknown <$> getIW16
    3 -> CILayoutFixed   <$> getIW16 <*> get
    n -> unexpected ("Objectable get CILayout: invalid tag: " ++ show n)

instance Objectable CIStatic where
  put (CIStaticRefs refs) = tag 1 >> put refs
  get = getTag >>= \case
    1 -> CIStaticRefs <$> get
    n -> unexpected ("Objectable get CIStatic: invalid tag: " ++ show n)

instance Objectable CIType where
  put (CIFun arity regs) = tag 1 >> putIW16 arity >> putIW16 regs
  put CIThunk            = tag 2
  put (CICon conTag)     = tag 3 >> putIW16 conTag
  put CIPap              = tag 4
  put CIBlackhole        = tag 5
  put CIStackFrame       = tag 6
  get = getTag >>= \case
    1 -> CIFun <$> getIW16 <*> getIW16
    2 -> pure CIThunk
    3 -> CICon <$> getIW16
    4 -> pure CIPap
    5 -> pure CIBlackhole
    6 -> pure CIStackFrame
    n -> unexpected ("Objectable get CIType: invalid tag: " ++ show n)

-- put an Int as a Word16, little endian. useful for many small values
putIW16 :: Int -> PutS
putIW16 i | i > 65535 || i < 0 = error ("putIW16: out of range: " ++ show i)
          | otherwise          = lift $ DB.putWord16le (fromIntegral i)

getIW16 :: GetS Int
getIW16 = lift (fmap fromIntegral DB.getWord16le)

-- the binary instance stores ints as 64 bit
instance Objectable Int where
  put = lift . DB.put
  get = lift DB.get

instance Objectable ExportedFun where
  put (ExportedFun modu symb) = put modu >> put symb
  get = ExportedFun <$> get <*> get

instance Objectable Module where
  put (Module unit mod_name) = put unit >> put mod_name
  get = Module <$> get <*> get

instance Objectable ModuleName where
  put (ModuleName fs) = put fs
  get = ModuleName <$> get

instance Objectable Unit where
  put = \case
    RealUnit (Definite uid) -> tag 0 >> put uid
    VirtUnit uid            -> tag 1 >> put uid
    HoleUnit                -> tag 2
  get = getTag >>= \case
    0 -> (RealUnit . Definite) <$> get
    1 -> VirtUnit              <$> get
    _ -> pure HoleUnit

instance Objectable FastString where
  put fs = put (unpackFS fs)
  get = mkFastString <$> get

instance Objectable UnitId where
  put (UnitId fs) = put fs
  get = UnitId <$> get

instance Objectable InstantiatedUnit where
  put indef = do
    put (instUnitInstanceOf indef)
    put (instUnitInsts indef)
  get = mkInstantiatedUnitSorted <$> get <*> get

putEnum :: Enum a => a -> PutS
putEnum x | n > 65535 = error ("putEnum: out of range: " ++ show n)
          | otherwise = putIW16 n
  where n = fromEnum x

getEnum :: Enum a => GetS a
getEnum = toEnum <$> getIW16

instance Objectable Bool where
  put False = tag 1
  put True  = tag 2
  get = getTag >>= \case
    1 -> return False
    2 -> return True
    n -> unexpected ("Objectable get Bool: invalid tag: " ++ show n)

instance Objectable StaticInfo where
  put (StaticInfo ident val cc) = put ident >> put val >> put cc
  get = StaticInfo <$> get <*> get <*> get

instance Objectable StaticVal where
  put (StaticFun f args)   = tag 1 >> put f  >> put args
  put (StaticThunk t)      = tag 2 >> put t
  put (StaticUnboxed u)    = tag 3 >> put u
  put (StaticData dc args) = tag 4 >> put dc >> put args
  put (StaticList xs t)    = tag 5 >> put xs >> put t
  get = getTag >>= \case
    1 -> StaticFun     <$> get <*> get
    2 -> StaticThunk   <$> get
    3 -> StaticUnboxed <$> get
    4 -> StaticData    <$> get <*> get
    5 -> StaticList    <$> get <*> get
    n -> unexpected ("Objectable get StaticVal: invalid tag " ++ show n)

instance Objectable StaticUnboxed where
  put (StaticUnboxedBool b)           = tag 1 >> put b
  put (StaticUnboxedInt i)            = tag 2 >> put i
  put (StaticUnboxedDouble d)         = tag 3 >> put d
  put (StaticUnboxedString str)       = tag 4 >> put str
  put (StaticUnboxedStringOffset str) = tag 5 >> put str
  get = getTag >>= \case
    1 -> StaticUnboxedBool         <$> get
    2 -> StaticUnboxedInt          <$> get
    3 -> StaticUnboxedDouble       <$> get
    4 -> StaticUnboxedString       <$> get
    5 -> StaticUnboxedStringOffset <$> get
    n -> unexpected ("Objectable get StaticUnboxed: invalid tag " ++ show n)

instance Objectable StaticArg where
  put (StaticObjArg i)      = tag 1 >> put i
  put (StaticLitArg p)      = tag 2 >> put p
  put (StaticConArg c args) = tag 3 >> put c >> put args
  get = getTag >>= \case
    1 -> StaticObjArg <$> get
    2 -> StaticLitArg <$> get
    3 -> StaticConArg <$> get <*> get
    n -> unexpected ("Objectable get StaticArg: invalid tag " ++ show n)

instance Objectable StaticLit where
  put (BoolLit b)    = tag 1 >> put b
  put (IntLit i)     = tag 2 >> put i
  put NullLit        = tag 3
  put (DoubleLit d)  = tag 4 >> put d
  put (StringLit t)  = tag 5 >> put t
  put (BinLit b)     = tag 6 >> put b
  put (LabelLit b t) = tag 7 >> put b >> put t
  get = getTag >>= \case
    1 -> BoolLit   <$> get
    2 -> IntLit    <$> get
    3 -> pure NullLit
    4 -> DoubleLit <$> get
    5 -> StringLit <$> get
    6 -> BinLit    <$> get
    7 -> LabelLit  <$> get <*> get
    n -> unexpected ("Objectable get StaticLit: invalid tag " ++ show n)

instance Objectable BS.ByteString where
  put = lift . DB.put
  get = lift DB.get
