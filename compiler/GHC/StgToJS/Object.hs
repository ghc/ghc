{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiWayIf                 #-}

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
--
-----------------------------------------------------------------------------

module GHC.StgToJS.Object
  ( ObjectKind(..)
  , getObjectKind
  , getObjectKindBS
  -- * JS object
  , JSOptions(..)
  , defaultJSOptions
  , getOptionsFromJsFile
  , writeJSObject
  , readJSObject
  , parseJSObject
  , parseJSObjectBS
  -- * HS object
  , putObject
  , getObjectHeader
  , getObjectBody
  , getObject
  , readObject
  , getObjectBlocks
  , readObjectBlocks
  , readObjectBlockInfo
  , isGlobalBlock
  , Object(..)
  , IndexEntry(..)
  , LocatedBlockInfo (..)
  , BlockInfo (..)
  , BlockDeps (..)
  , BlockLocation (..)
  , BlockId
  , BlockIds
  , BlockRef (..)
  , ExportedFun (..)
  )
where

import GHC.Prelude

import           Control.Monad

import           Data.Array
import qualified Data.ByteString          as B
import qualified Data.ByteString.Unsafe   as B
import           Data.Char (isSpace)
import           Data.Int
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import           Data.List (sortOn)
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Word
import           Data.Semigroup
import           System.IO

import GHC.Settings.Constants (hiVersion)

import GHC.JS.Ident
import qualified GHC.JS.Syntax as Sat
import GHC.StgToJS.Types

import GHC.Unit.Module

import GHC.Data.FastString

import GHC.Types.Unique.Map

import GHC.Utils.Binary hiding (SymbolTable)
import GHC.Utils.Outputable (ppr, Outputable, hcat, vcat, text, hsep)
import GHC.Utils.Monad (mapMaybeM)
import GHC.Utils.Panic
import GHC.Utils.Misc (dropWhileEndLE)
import System.IO.Unsafe
import qualified Control.Exception as Exception

----------------------------------------------
-- The JS backend supports 3 kinds of objects:
--   1. HS objects: produced from Haskell sources
--   2. JS objects: produced from JS sources
--   3. Cc objects: produced by emcc (e.g. from C sources)
--
-- They all have a different header that allows them to be distinguished.
-- See ObjectKind type.
----------------------------------------------

-- | Different kinds of object (.o) supported by the JS backend
data ObjectKind
  = ObjJs -- ^ JavaScript source embedded in a .o
  | ObjHs -- ^ JS backend object for Haskell code
  | ObjCc -- ^ Wasm module object as produced by emcc
  deriving (Show,Eq,Ord)

-- | Get the kind of a file object, if any
getObjectKind :: FilePath -> IO (Maybe ObjectKind)
getObjectKind fp = withBinaryFile fp ReadMode $ \h -> do
  let !max_header_length = max (B.length jsHeader)
                           $ max (B.length wasmHeader)
                                 (B.length hsHeader)

  bs <- B.hGet h max_header_length
  pure $! getObjectKindBS bs

-- | Get the kind of an object stored in a bytestring, if any
getObjectKindBS :: B.ByteString -> Maybe ObjectKind
getObjectKindBS bs
  | jsHeader   `B.isPrefixOf` bs = Just ObjJs
  | hsHeader   `B.isPrefixOf` bs = Just ObjHs
  | wasmHeader `B.isPrefixOf` bs = Just ObjCc
  | otherwise                    = Nothing

-- Header added to JS sources to discriminate them from other object files.
-- They all have .o extension but JS sources have this header.
jsHeader :: B.ByteString
jsHeader = unsafePerformIO $ B.unsafePackAddressLen 8 "GHCJS_JS"#

hsHeader :: B.ByteString
hsHeader = unsafePerformIO $ B.unsafePackAddressLen 8 "GHCJS_HS"#

wasmHeader :: B.ByteString
wasmHeader = unsafePerformIO $ B.unsafePackAddressLen 4 "\0asm"#



------------------------------------------------
-- HS objects
--
--  file layout:
--   - magic "GHCJS_HS"
--   - compiler version tag
--   - module name
--   - offsets of string table
--   - dependencies
--   - offset of the index
--   - unit infos
--   - index
--   - string table
--
------------------------------------------------

-- | A HS object file
data Object = Object
  { objModuleName    :: !ModuleName
    -- ^ name of the module
  , objHandle        :: !ReadBinHandle
    -- ^ BinHandle that can be used to read the ObjBlocks
  , objPayloadOffset :: !(Bin ObjBlock)
    -- ^ Offset of the payload (units)
  , objBlockInfo     :: !BlockInfo
    -- ^ Information about blocks
  , objIndex         :: !Index
    -- ^ Block index: symbols per block and block offset in the object file
  }

type BlockId  = Int
type BlockIds = IntSet

-- | Information about blocks (linkable units)
data BlockInfo = BlockInfo
  { bi_module     :: !Module
      -- ^ Module they were generated from
  , bi_must_link  :: !BlockIds
      -- ^ blocks that always need to be linked when this object is loaded (e.g.
      -- everything that contains initializer code or foreign exports)
  , bi_exports    :: !(Map ExportedFun BlockId)
      -- ^ exported Haskell functions -> block
  , bi_block_deps :: !(Array BlockId BlockDeps)
      -- ^ dependencies of each block
  }

data LocatedBlockInfo = LocatedBlockInfo
  { lbi_loc  :: !BlockLocation -- ^ Where to find the blocks
  , lbi_info :: !BlockInfo     -- ^ Block information
  }

instance Outputable BlockInfo where
  ppr d = vcat
    [ hcat [ text "module: ", pprModule (bi_module d) ]
    , hcat [ text "exports: ", ppr (M.keys (bi_exports d)) ]
    ]

-- | Where are the blocks
data BlockLocation
  = ObjectFile  FilePath       -- ^ In an object file at path
  | ArchiveFile FilePath       -- ^ In a Ar file at path
  | InMemory    String Object  -- ^ In memory

instance Outputable BlockLocation where
  ppr = \case
    ObjectFile fp  -> hsep [text "ObjectFile", text fp]
    ArchiveFile fp -> hsep [text "ArchiveFile", text fp]
    InMemory s o   -> hsep [text "InMemory", text s, ppr (objModuleName o)]

-- | A @BlockRef@ is a pair of a module and the index of the block in the
-- object file
data BlockRef = BlockRef
  { block_ref_mod :: !Module  -- ^ Module
  , block_ref_idx :: !BlockId -- ^ Block index in the object file
  }
  deriving (Eq,Ord)

data BlockDeps = BlockDeps
  { blockBlockDeps       :: [BlockId]     -- ^ dependencies on blocks in this object
  , blockFunDeps         :: [ExportedFun] -- ^ dependencies on exported symbols in other objects
  -- , blockForeignExported :: [ExpFun]
  -- , blockForeignImported :: [ForeignRef]
  }

-- | we use the convention that the first block (0) is a module-global block
-- that's always included when something from the module is loaded. everything
-- in a module implicitly depends on the global block. The global block itself
-- can't have dependencies
isGlobalBlock :: BlockId -> Bool
isGlobalBlock n = n == 0

-- | Exported Functions
data ExportedFun = ExportedFun
  { funModule  :: !Module              -- ^ The module containing the function
  , funSymbol  :: !LexicalFastString   -- ^ The function
  } deriving (Eq, Ord)

instance Outputable ExportedFun where
  ppr (ExportedFun m f) = vcat
    [ hcat [ text "module: ", pprModule m ]
    , hcat [ text "symbol: ", ppr f ]
    ]

-- | Write an ObjBlock, except for the top level symbols which are stored in the
-- index
putObjBlock :: WriteBinHandle -> ObjBlock -> IO ()
putObjBlock bh (ObjBlock _syms b c d e f g) = do
    lazyPut bh b
    lazyPut bh c
    lazyPut bh d
    lazyPut bh e
    lazyPut bh f
    lazyPut bh g

-- | Read an ObjBlock and associate it to the given symbols (that must have been
-- read from the index)
getObjBlock :: [FastString] -> ReadBinHandle -> IO ObjBlock
getObjBlock syms bh = do
    b <- lazyGet bh
    c <- lazyGet bh
    d <- lazyGet bh
    e <- lazyGet bh
    f <- lazyGet bh
    g <- lazyGet bh
    pure $ ObjBlock
      { oiSymbols  = syms
      , oiClInfo   = b
      , oiStatic   = c
      , oiStat     = d
      , oiRaw      = e
      , oiFExports = f
      , oiFImports = g
      }


-- | Serialized block indexes and their exported symbols
-- (the first block is module-global)
type Index = [IndexEntry]
data IndexEntry = IndexEntry
  { idxSymbols :: ![FastString]  -- ^ Symbols exported by a block
  , idxOffset  :: !(Bin ObjBlock) -- ^ Offset of the block in the object file
  }


--------------------------------------------------------------------------------
-- Essential operations on Objects
--------------------------------------------------------------------------------

-- | Given a handle to a Binary payload, add the module, 'mod_name', its
-- dependencies, 'deps', and its linkable units to the payload.
putObject
  :: WriteBinHandle
  -> ModuleName -- ^ module
  -> BlockInfo  -- ^ block infos
  -> [ObjBlock] -- ^ linkable units and their symbols
  -> IO ()
putObject bh mod_name deps os = do
  putByteString bh hsHeader
  put_ bh (show hiVersion)

  -- we store the module name as a String because we don't want to have to
  -- decode the FastString table just to decode it when we're looking for an
  -- object in an archive.
  put_ bh (moduleNameString mod_name)

  (fs_tbl, fs_writer) <- initFastStringWriterTable
  let bh_fs = addWriterToUserData fs_writer bh

  forwardPut_ bh (const (putTable fs_tbl bh_fs)) $ do
    put_ bh_fs deps

    -- forward put the index
    forwardPut_ bh_fs (put_ bh_fs) $ do
      idx <- forM os $ \o -> do
        p <- tellBinWriter bh_fs
        -- write units without their symbols
        putObjBlock bh_fs o
        -- return symbols and offset to store in the index
        pure (oiSymbols o,p)
      pure idx

-- | Parse object header
getObjectHeader :: ReadBinHandle -> IO (Either String ModuleName)
getObjectHeader bh = do
  magic <- getByteString bh (B.length hsHeader)
  case magic == hsHeader of
    False -> pure (Left "invalid magic header for HS object")
    True  -> do
      is_correct_version <- ((== hiVersion) . read) <$> get bh
      case is_correct_version of
        False -> pure (Left "invalid header version")
        True  -> do
          mod_name <- get bh
          pure (Right (mkModuleName (mod_name)))


-- | Parse object body. Must be called after a successful getObjectHeader
getObjectBody :: ReadBinHandle -> ModuleName -> IO Object
getObjectBody bh0 mod_name = do
  -- Read the string table
  dict <- forwardGet bh0 (getDictionary bh0)
  let bh = setReaderUserData bh0 $ newReadState (panic "No name allowed") (getDictFastString dict)

  block_info  <- get bh
  idx         <- forwardGet bh (get bh)
  payload_pos <- tellBinReader bh

  pure $ Object
    { objModuleName    = mod_name
    , objHandle        = bh
    , objPayloadOffset = payload_pos
    , objBlockInfo     = block_info
    , objIndex         = idx
    }

-- | Parse object
getObject :: ReadBinHandle -> IO (Maybe Object)
getObject bh = do
  getObjectHeader bh >>= \case
    Left _err      -> pure Nothing
    Right mod_name -> Just <$> getObjectBody bh mod_name

-- | Read object from file
--
-- The object is still in memory after this (see objHandle).
readObject :: FilePath -> IO (Maybe Object)
readObject file = do
  bh <- readBinMem file
  getObject bh

-- | Reads only the part necessary to get the block info
readObjectBlockInfo :: FilePath -> IO (Maybe BlockInfo)
readObjectBlockInfo file = do
  bh <- readBinMem file
  getObject bh >>= \case
    Just obj -> pure $! Just $! objBlockInfo obj
    Nothing  -> pure Nothing

-- | Get blocks in the object file, using the given filtering function
getObjectBlocks :: Object -> BlockIds -> IO [ObjBlock]
getObjectBlocks obj bids = mapMaybeM read_entry (zip (objIndex obj) [0..])
  where
    bh = objHandle obj
    read_entry (IndexEntry syms offset,i)
      | IS.member i bids = do
          seekBinReader bh offset
          Just <$> getObjBlock syms bh
      | otherwise = pure Nothing

-- | Read blocks in the object file, using the given filtering function
readObjectBlocks :: FilePath -> BlockIds -> IO [ObjBlock]
readObjectBlocks file bids = do
  readObject file >>= \case
    Nothing  -> pure []
    Just obj -> getObjectBlocks obj bids


--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

putEnum :: Enum a => WriteBinHandle -> a -> IO ()
putEnum bh x | n > 65535 = error ("putEnum: out of range: " ++ show n)
             | otherwise = put_ bh n
  where n = fromIntegral $ fromEnum x :: Word16

getEnum :: Enum a => ReadBinHandle -> IO a
getEnum bh = toEnum . fromIntegral <$> (get bh :: IO Word16)

-- | Helper to convert Int to Int32
toI32 :: Int -> Int32
toI32 = fromIntegral

-- | Helper to convert Int32 to Int
fromI32 :: Int32 -> Int
fromI32 = fromIntegral


--------------------------------------------------------------------------------
-- Binary Instances
--------------------------------------------------------------------------------

instance Binary IndexEntry where
  put_ bh (IndexEntry a b) = put_ bh a >> put_ bh b
  get bh = IndexEntry <$> get bh <*> get bh

instance Binary BlockInfo where
  put_ bh (BlockInfo m r e b) = do
      put_ bh m
      put_ bh (map toI32 $ IS.toList r)
      put_ bh (map (\(x,y) -> (x, toI32 y)) $ M.toList e)
      put_ bh (elems b)
  get bh = BlockInfo <$> get bh
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

instance Binary Sat.JStat where
  put_ bh (Sat.DeclStat i e)       = putByte bh 1  >> put_ bh i >> put_ bh e
  put_ bh (Sat.ReturnStat e)       = putByte bh 2  >> put_ bh e
  put_ bh (Sat.IfStat e s1 s2)     = putByte bh 3  >> put_ bh e  >> put_ bh s1 >> put_ bh s2
  put_ bh (Sat.WhileStat b e s)    = putByte bh 4  >> put_ bh b  >> put_ bh e  >> put_ bh s
  put_ bh (Sat.ForStat is c s bd)  = putByte bh 5 >> put_ bh is  >> put_ bh c >> put_ bh s >> put_ bh bd
  put_ bh (Sat.ForInStat b i e s)  = putByte bh 6  >> put_ bh b  >> put_ bh i  >> put_ bh e  >> put_ bh s
  put_ bh (Sat.SwitchStat e ss s)  = putByte bh 7  >> put_ bh e  >> put_ bh ss >> put_ bh s
  put_ bh (Sat.TryStat s1 i s2 s3) = putByte bh 8  >> put_ bh s1 >> put_ bh i  >> put_ bh s2 >> put_ bh s3
  put_ bh (Sat.BlockStat xs)       = putByte bh 9  >> put_ bh xs
  put_ bh (Sat.ApplStat e es)      = putByte bh 10 >> put_ bh e  >> put_ bh es
  put_ bh (Sat.UOpStat o e)        = putByte bh 11 >> put_ bh o  >> put_ bh e
  put_ bh (Sat.AssignStat e1 op e2) = putByte bh 12 >> put_ bh e1 >> put_ bh op >> put_ bh e2
  put_ bh (Sat.LabelStat l s)      = putByte bh 13 >> put_ bh l  >> put_ bh s
  put_ bh (Sat.BreakStat ml)       = putByte bh 14 >> put_ bh ml
  put_ bh (Sat.ContinueStat ml)    = putByte bh 15 >> put_ bh ml
  put_ bh (Sat.FuncStat i is b)    = putByte bh 16 >> put_ bh i >> put_ bh is >> put_ bh b
  get bh = getByte bh >>= \case
    1  -> Sat.DeclStat     <$> get bh <*> get bh
    2  -> Sat.ReturnStat   <$> get bh
    3  -> Sat.IfStat       <$> get bh <*> get bh <*> get bh
    4  -> Sat.WhileStat    <$> get bh <*> get bh <*> get bh
    5  -> Sat.ForStat      <$> get bh <*> get bh <*> get bh <*> get bh
    6  -> Sat.ForInStat    <$> get bh <*> get bh <*> get bh <*> get bh
    7  -> Sat.SwitchStat   <$> get bh <*> get bh <*> get bh
    8  -> Sat.TryStat      <$> get bh <*> get bh <*> get bh <*> get bh
    9  -> Sat.BlockStat    <$> get bh
    10 -> Sat.ApplStat     <$> get bh <*> get bh
    11 -> Sat.UOpStat      <$> get bh <*> get bh
    12 -> Sat.AssignStat   <$> get bh <*> get bh <*> get bh
    13 -> Sat.LabelStat    <$> get bh <*> get bh
    14 -> Sat.BreakStat    <$> get bh
    15 -> Sat.ContinueStat <$> get bh
    16 -> Sat.FuncStat     <$> get bh <*> get bh <*> get bh
    n -> error ("Binary get bh JStat: invalid tag: " ++ show n)


instance Binary Sat.JExpr where
  put_ bh (Sat.ValExpr v)          = putByte bh 1 >> put_ bh v
  put_ bh (Sat.SelExpr e i)        = putByte bh 2 >> put_ bh e  >> put_ bh i
  put_ bh (Sat.IdxExpr e1 e2)      = putByte bh 3 >> put_ bh e1 >> put_ bh e2
  put_ bh (Sat.InfixExpr o e1 e2)  = putByte bh 4 >> put_ bh o  >> put_ bh e1 >> put_ bh e2
  put_ bh (Sat.UOpExpr o e)        = putByte bh 5 >> put_ bh o  >> put_ bh e
  put_ bh (Sat.IfExpr e1 e2 e3)    = putByte bh 6 >> put_ bh e1 >> put_ bh e2 >> put_ bh e3
  put_ bh (Sat.ApplExpr e es)      = putByte bh 7 >> put_ bh e  >> put_ bh es
  get bh = getByte bh >>= \case
    1 -> Sat.ValExpr   <$> get bh
    2 -> Sat.SelExpr   <$> get bh <*> get bh
    3 -> Sat.IdxExpr   <$> get bh <*> get bh
    4 -> Sat.InfixExpr <$> get bh <*> get bh <*> get bh
    5 -> Sat.UOpExpr   <$> get bh <*> get bh
    6 -> Sat.IfExpr    <$> get bh <*> get bh <*> get bh
    7 -> Sat.ApplExpr  <$> get bh <*> get bh
    n -> error ("Binary get bh UnsatExpr: invalid tag: " ++ show n)


instance Binary Sat.JVal where
  put_ bh (Sat.JVar i)      = putByte bh 1 >> put_ bh i
  put_ bh (Sat.JList es)    = putByte bh 2 >> put_ bh es
  put_ bh (Sat.JDouble d)   = putByte bh 3 >> put_ bh d
  put_ bh (Sat.JInt i)      = putByte bh 4 >> put_ bh i
  put_ bh (Sat.JStr xs)     = putByte bh 5 >> put_ bh xs
  put_ bh (Sat.JRegEx xs)   = putByte bh 6 >> put_ bh xs
  put_ bh (Sat.JBool b)     = putByte bh 7 >> put_ bh b
  put_ bh (Sat.JHash m)     = putByte bh 8 >> put_ bh (sortOn (LexicalFastString . fst) $ nonDetUniqMapToList m)
  put_ bh (Sat.JFunc is s)  = putByte bh 9 >> put_ bh is >> put_ bh s
  get bh = getByte bh >>= \case
    1 -> Sat.JVar    <$> get bh
    2 -> Sat.JList   <$> get bh
    3 -> Sat.JDouble <$> get bh
    4 -> Sat.JInt    <$> get bh
    5 -> Sat.JStr    <$> get bh
    6 -> Sat.JRegEx  <$> get bh
    7 -> Sat.JBool   <$> get bh
    8 -> Sat.JHash . listToUniqMap <$> get bh
    9 -> Sat.JFunc   <$> get bh <*> get bh
    n -> error ("Binary get bh Sat.JVal: invalid tag: " ++ show n)

instance Binary Ident where
  put_ bh (identFS -> xs) = put_ bh xs
  get  bh                 = name <$> get bh

instance Binary ClosureInfo where
  put_ bh (ClosureInfo v regs name layo typ static) = do
    put_ bh v >> put_ bh regs >> put_ bh name >> put_ bh layo >> put_ bh typ >> put_ bh static
  get bh = ClosureInfo <$> get bh <*> get bh <*> get bh <*> get bh <*> get bh <*> get bh

instance Binary JSFFIType where
  put_ bh = putEnum bh
  get bh = getEnum bh

instance Binary JSRep where
  put_ bh = putEnum bh
  get bh = getEnum bh

instance Binary CIRegs where
  put_ bh CIRegsUnknown       = putByte bh 1
  put_ bh (CIRegs skip types) = putByte bh 2 >> put_ bh skip >> put_ bh types
  get bh = getByte bh >>= \case
    1 -> pure CIRegsUnknown
    2 -> CIRegs <$> get bh <*> get bh
    n -> error ("Binary get bh CIRegs: invalid tag: " ++ show n)

instance Binary Sat.Op where
  put_ bh = putEnum bh
  get bh = getEnum bh

instance Binary Sat.UOp where
  put_ bh = putEnum bh
  get bh = getEnum bh

instance Binary Sat.AOp where
  put_ bh = putEnum bh
  get bh = getEnum bh

-- 16 bit sizes should be enough...
instance Binary CILayout where
  put_ bh CILayoutVariable           = putByte bh 1
  put_ bh (CILayoutUnknown size)     = putByte bh 2 >> put_ bh size
  put_ bh (CILayoutFixed size types) = putByte bh 3 >> put_ bh size >> put_ bh types
  get bh = getByte bh >>= \case
    1 -> pure CILayoutVariable
    2 -> CILayoutUnknown <$> get bh
    3 -> CILayoutFixed   <$> get bh <*> get bh
    n -> error ("Binary get bh CILayout: invalid tag: " ++ show n)

instance Binary CIStatic where
  put_ bh (CIStaticRefs refs) = putByte bh 1 >> put_ bh refs
  get bh = getByte bh >>= \case
    1 -> CIStaticRefs <$> get bh
    n -> error ("Binary get bh CIStatic: invalid tag: " ++ show n)

instance Binary CIType where
  put_ bh (CIFun arity regs) = putByte bh 1 >> put_ bh arity >> put_ bh regs
  put_ bh CIThunk            = putByte bh 2
  put_ bh (CICon conTag)     = putByte bh 3 >> put_ bh conTag
  put_ bh CIPap              = putByte bh 4
  put_ bh CIBlackhole        = putByte bh 5
  put_ bh CIStackFrame       = putByte bh 6
  get bh = getByte bh >>= \case
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

instance Binary StaticInfo where
  put_ bh (StaticInfo ident val cc) = put_ bh ident >> put_ bh val >> put_ bh cc
  get bh = StaticInfo <$> get bh <*> get bh <*> get bh

instance Binary StaticVal where
  put_ bh (StaticApp SAKFun   f  args)   = putByte bh 1 >> put_ bh f  >> put_ bh args
  put_ bh (StaticApp SAKThunk f  args)   = putByte bh 2 >> put_ bh f  >> put_ bh args
  put_ bh (StaticUnboxed u)              = putByte bh 3 >> put_ bh u
  put_ bh (StaticApp SAKData  dc args)   = putByte bh 4 >> put_ bh dc >> put_ bh args
  put_ bh (StaticList xs t)              = putByte bh 5 >> put_ bh xs >> put_ bh t
  get bh = getByte bh >>= \case
    1 -> StaticApp SAKFun <$> get bh <*> get bh
    2 -> StaticApp SAKThunk <$> get bh <*> get bh
    3 -> StaticUnboxed <$> get bh
    4 -> StaticApp SAKData <$> get bh <*> get bh
    5 -> StaticList    <$> get bh <*> get bh
    n -> error ("Binary get bh StaticVal: invalid tag " ++ show n)

instance Binary StaticUnboxed where
  put_ bh (StaticUnboxedBool b)           = putByte bh 1 >> put_ bh b
  put_ bh (StaticUnboxedInt i)            = putByte bh 2 >> put_ bh i
  put_ bh (StaticUnboxedDouble d)         = putByte bh 3 >> put_ bh d
  put_ bh (StaticUnboxedString str)       = putByte bh 4 >> put_ bh str
  put_ bh (StaticUnboxedStringOffset str) = putByte bh 5 >> put_ bh str
  get bh = getByte bh >>= \case
    1 -> StaticUnboxedBool         <$> get bh
    2 -> StaticUnboxedInt          <$> get bh
    3 -> StaticUnboxedDouble       <$> get bh
    4 -> StaticUnboxedString       <$> get bh
    5 -> StaticUnboxedStringOffset <$> get bh
    n -> error ("Binary get bh StaticUnboxed: invalid tag " ++ show n)

instance Binary StaticArg where
  put_ bh (StaticObjArg i)      = putByte bh 1 >> put_ bh i
  put_ bh (StaticLitArg p)      = putByte bh 2 >> put_ bh p
  put_ bh (StaticConArg c args) = putByte bh 3 >> put_ bh c >> put_ bh args
  get bh = getByte bh >>= \case
    1 -> StaticObjArg <$> get bh
    2 -> StaticLitArg <$> get bh
    3 -> StaticConArg <$> get bh <*> get bh
    n -> error ("Binary get bh StaticArg: invalid tag " ++ show n)

instance Binary StaticLit where
  put_ bh (BoolLit b)    = putByte bh 1 >> put_ bh b
  put_ bh (IntLit i)     = putByte bh 2 >> put_ bh i
  put_ bh NullLit        = putByte bh 3
  put_ bh (DoubleLit d)  = putByte bh 4 >> put_ bh d
  put_ bh (StringLit t)  = putByte bh 5 >> put_ bh t
  put_ bh (BinLit b)     = putByte bh 6 >> put_ bh b
  put_ bh (LabelLit b t) = putByte bh 7 >> put_ bh b >> put_ bh t
  get bh = getByte bh >>= \case
    1 -> BoolLit   <$> get bh
    2 -> IntLit    <$> get bh
    3 -> pure NullLit
    4 -> DoubleLit <$> get bh
    5 -> StringLit <$> get bh
    6 -> BinLit    <$> get bh
    7 -> LabelLit  <$> get bh <*> get bh
    n -> error ("Binary get bh StaticLit: invalid tag " ++ show n)


------------------------------------------------
-- JS objects
------------------------------------------------

-- | Options obtained from pragmas in JS files
data JSOptions = JSOptions
  { enableCPP                  :: !Bool     -- ^ Enable CPP on the JS file
  , emccExtraOptions           :: ![String] -- ^ Pass additional options to emcc at link time
  , emccExportedFunctions      :: ![String] -- ^ Arguments for `-sEXPORTED_FUNCTIONS`
  , emccExportedRuntimeMethods :: ![String] -- ^ Arguments for `-sEXPORTED_RUNTIME_METHODS`
  }
  deriving (Eq, Ord)


instance Binary JSOptions where
  put_ bh (JSOptions a b c d) = do
    put_ bh a
    put_ bh b
    put_ bh c
    put_ bh d
  get bh = JSOptions <$> get bh <*> get bh <*> get bh <*> get bh

instance Semigroup JSOptions where
  a <> b = JSOptions
    { enableCPP                  = enableCPP a || enableCPP b
    , emccExtraOptions           = emccExtraOptions a ++ emccExtraOptions b
    , emccExportedFunctions      = List.nub (List.sort (emccExportedFunctions a ++ emccExportedFunctions b))
    , emccExportedRuntimeMethods = List.nub (List.sort (emccExportedRuntimeMethods a ++ emccExportedRuntimeMethods b))
    }

defaultJSOptions :: JSOptions
defaultJSOptions = JSOptions
  { enableCPP                  = False
  , emccExtraOptions           = []
  , emccExportedRuntimeMethods = []
  , emccExportedFunctions      = []
  }

-- mimics `lines` implementation
splitOnComma :: String -> [String]
splitOnComma s = cons $ case break (== ',') s of
                                   (l, s') -> (l, case s' of
                                                    []      -> []
                                                    _:s''   -> splitOnComma s'')
  where
    cons ~(h, t)        =  h : t



-- | Get the JS option pragmas from .js files
getJsOptions :: Handle -> IO JSOptions
getJsOptions handle = do
  hSetEncoding handle utf8
  let trim = dropWhileEndLE isSpace . dropWhile isSpace
  let go opts = do
        hIsEOF handle >>= \case
          True -> pure opts
          False -> do
            xs <- hGetLine handle
            if not ("//#OPTIONS:" `List.isPrefixOf` xs)
              then pure opts
              else do
                -- drop prefix and spaces
                let ys = trim (drop 11 xs)
                let opts' = if
                      | ys == "CPP"
                      -> opts {enableCPP = True}

                      | Just s <- List.stripPrefix "EMCC:EXPORTED_FUNCTIONS=" ys
                      , fns <- fmap trim (splitOnComma s)
                      -> opts { emccExportedFunctions = emccExportedFunctions opts ++ fns }

                      | Just s <- List.stripPrefix "EMCC:EXPORTED_RUNTIME_METHODS=" ys
                      , fns <- fmap trim (splitOnComma s)
                      -> opts { emccExportedRuntimeMethods = emccExportedRuntimeMethods opts ++ fns }

                      | Just s <- List.stripPrefix "EMCC:EXTRA=" ys
                      -> opts { emccExtraOptions = emccExtraOptions opts ++ [s] }

                      | otherwise
                      -> panic ("Unrecognized JS pragma: " ++ ys)

                go opts'
  go defaultJSOptions

-- | Parse option pragma in JS file
getOptionsFromJsFile :: FilePath     -- ^ Input file
                     -> IO JSOptions -- ^ Parsed options.
getOptionsFromJsFile filename
    = Exception.bracket
              (openBinaryFile filename ReadMode)
              hClose
              getJsOptions


-- | Write a JS object (embed some handwritten JS code)
writeJSObject :: JSOptions -> B.ByteString -> FilePath -> IO ()
writeJSObject opts contents output_fn = do
  bh <- openBinMem (B.length contents + 1000)

  putByteString bh jsHeader
  put_ bh opts
  put_ bh contents

  writeBinMem bh output_fn


-- | Read a JS object from BinHandle
parseJSObject :: ReadBinHandle -> IO (JSOptions, B.ByteString)
parseJSObject bh = do
  magic <- getByteString bh (B.length jsHeader)
  case magic == jsHeader of
    False -> panic "invalid magic header for JS object"
    True  -> do
      opts     <- get bh
      contents <- get bh
      pure (opts,contents)

-- | Read a JS object from ByteString
parseJSObjectBS :: B.ByteString -> IO (JSOptions, B.ByteString)
parseJSObjectBS bs = do
  bh <- unsafeUnpackBinBuffer bs
  parseJSObject bh

-- | Read a JS object from file
readJSObject :: FilePath -> IO (JSOptions, B.ByteString)
readJSObject input_fn = do
  bh <- readBinMem input_fn
  parseJSObject bh
