{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

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
--   - magic "GHCJSOBJ"
--   - compiler version tag
--   - module name
--   - offsets of string table
--   - dependencies
--   - offset of the index
--   - unit infos
--   - index
--   - string table
--
-----------------------------------------------------------------------------

module GHC.StgToJS.Object
  ( putObject
  , getObjectHeader
  , getObjectBody
  , getObject
  , readObject
  , getObjectUnits
  , readObjectUnits
  , readObjectDeps
  , isGlobalUnit
  , isJsObjectFile
  , Object(..)
  , IndexEntry(..)
  , Deps (..), BlockDeps (..), DepsLocation (..)
  , ExportedFun (..)
  )
where

import GHC.Prelude

import           Control.Monad

import           Data.Array
import           Data.Int
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import           Data.List (sortOn)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Word
import           Data.Char
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO

import GHC.Settings.Constants (hiVersion)

import GHC.JS.Unsat.Syntax
import qualified GHC.JS.Syntax as Sat
import GHC.StgToJS.Types

import GHC.Unit.Module

import GHC.Data.FastString

import GHC.Types.Unique.Map
import GHC.Float (castDoubleToWord64, castWord64ToDouble)

import GHC.Utils.Binary hiding (SymbolTable)
import GHC.Utils.Outputable (ppr, Outputable, hcat, vcat, text, hsep)
import GHC.Utils.Monad (mapMaybeM)

-- | An object file
data Object = Object
  { objModuleName    :: !ModuleName
    -- ^ name of the module
  , objHandle        :: !BinHandle
    -- ^ BinHandle that can be used to read the ObjUnits
  , objPayloadOffset :: !(Bin ObjUnit)
    -- ^ Offset of the payload (units)
  , objDeps          :: !Deps
    -- ^ Dependencies
  , objIndex         :: !Index
    -- ^ The Index, serialed unit indices and their linkable units
  }

type BlockId  = Int
type BlockIds = IntSet

-- | dependencies for a single module
data Deps = Deps
  { depsModule          :: !Module
      -- ^ module
  , depsRequired        :: !BlockIds
      -- ^ blocks that always need to be linked when this object is loaded (e.g.
      -- everything that contains initializer code or foreign exports)
  , depsHaskellExported :: !(Map ExportedFun BlockId)
      -- ^ exported Haskell functions -> block
  , depsBlocks          :: !(Array BlockId BlockDeps)
      -- ^ info about each block
  }

instance Outputable Deps where
  ppr d = vcat
    [ hcat [ text "module: ", pprModule (depsModule d) ]
    , hcat [ text "exports: ", ppr (M.keys (depsHaskellExported d)) ]
    ]

-- | Where are the dependencies
data DepsLocation
  = ObjectFile  FilePath       -- ^ In an object file at path
  | ArchiveFile FilePath       -- ^ In a Ar file at path
  | InMemory    String Object  -- ^ In memory

instance Outputable DepsLocation where
  ppr = \case
    ObjectFile fp  -> hsep [text "ObjectFile", text fp]
    ArchiveFile fp -> hsep [text "ArchiveFile", text fp]
    InMemory s o   -> hsep [text "InMemory", text s, ppr (objModuleName o)]

data BlockDeps = BlockDeps
  { blockBlockDeps       :: [Int]         -- ^ dependencies on blocks in this object
  , blockFunDeps         :: [ExportedFun] -- ^ dependencies on exported symbols in other objects
  -- , blockForeignExported :: [ExpFun]
  -- , blockForeignImported :: [ForeignRef]
  }

{- | we use the convention that the first unit (0) is a module-global
     unit that's always included when something from the module
     is loaded. everything in a module implicitly depends on the
     global block. the global unit itself can't have dependencies
 -}
isGlobalUnit :: Int -> Bool
isGlobalUnit n = n == 0

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

-- | Write an ObjUnit, except for the top level symbols which are stored in the
-- index
putObjUnit :: BinHandle -> ObjUnit -> IO ()
putObjUnit bh (ObjUnit _syms b c d e f g) = do
    put_ bh b
    put_ bh c
    lazyPut bh d
    put_ bh e
    put_ bh f
    put_ bh g

-- | Read an ObjUnit and associate it to the given symbols (that must have been
-- read from the index)
getObjUnit :: [FastString] -> BinHandle -> IO ObjUnit
getObjUnit syms bh = do
    b <- get bh
    c <- get bh
    d <- lazyGet bh
    e <- get bh
    f <- get bh
    g <- get bh
    pure $ ObjUnit
      { oiSymbols  = syms
      , oiClInfo   = b
      , oiStatic   = c
      , oiStat     = d
      , oiRaw      = e
      , oiFExports = f
      , oiFImports = g
      }


-- | A tag that determines the kind of payload in the .o file. See
-- @StgToJS.Linker.Arhive.magic@ for another kind of magic
magic :: String
magic = "GHCJSOBJ"

-- | Serialized unit indexes and their exported symbols
-- (the first unit is module-global)
type Index = [IndexEntry]
data IndexEntry = IndexEntry
  { idxSymbols :: ![FastString]  -- ^ Symbols exported by a unit
  , idxOffset  :: !(Bin ObjUnit) -- ^ Offset of the unit in the object file
  }


--------------------------------------------------------------------------------
-- Essential oeprations on Objects
--------------------------------------------------------------------------------

-- | Given a handle to a Binary payload, add the module, 'mod_name', its
-- dependencies, 'deps', and its linkable units to the payload.
putObject
  :: BinHandle
  -> ModuleName -- ^ module
  -> Deps       -- ^ dependencies
  -> [ObjUnit]  -- ^ linkable units and their symbols
  -> IO ()
putObject bh mod_name deps os = do
  forM_ magic (putByte bh . fromIntegral . ord)
  put_ bh (show hiVersion)

  -- we store the module name as a String because we don't want to have to
  -- decode the FastString table just to decode it when we're looking for an
  -- object in an archive.
  put_ bh (moduleNameString mod_name)

  (bh_fs, _bin_dict, put_dict) <- initFSTable bh

  forwardPut_ bh (const put_dict) $ do
    put_ bh_fs deps

    -- forward put the index
    forwardPut_ bh_fs (put_ bh_fs) $ do
      idx <- forM os $ \o -> do
        p <- tellBin bh_fs
        -- write units without their symbols
        putObjUnit bh_fs o
        -- return symbols and offset to store in the index
        pure (oiSymbols o,p)
      pure idx

-- | Test if the object file is a JS object
isJsObjectFile :: FilePath -> IO Bool
isJsObjectFile fp = do
  let !n = length magic
  withBinaryFile fp ReadMode $ \hdl -> do
    allocaArray n $ \ptr -> do
      n' <- hGetBuf hdl ptr n
      if (n' /= n)
        then pure False
        else checkMagic (peekElemOff ptr)

-- | Check magic
checkMagic :: (Int -> IO Word8) -> IO Bool
checkMagic get_byte = do
  let go_magic !i = \case
        []     -> pure True
        (e:es) -> get_byte i >>= \case
          c | fromIntegral (ord e) == c -> go_magic (i+1) es
            | otherwise                 -> pure False
  go_magic 0 magic

-- | Parse object magic
getCheckMagic :: BinHandle -> IO Bool
getCheckMagic bh = checkMagic (const (getByte bh))

-- | Parse object header
getObjectHeader :: BinHandle -> IO (Either String ModuleName)
getObjectHeader bh = do
  is_magic <- getCheckMagic bh
  case is_magic of
    False -> pure (Left "invalid magic header")
    True  -> do
      is_correct_version <- ((== hiVersion) . read) <$> get bh
      case is_correct_version of
        False -> pure (Left "invalid header version")
        True  -> do
          mod_name <- get bh
          pure (Right (mkModuleName (mod_name)))


-- | Parse object body. Must be called after a sucessful getObjectHeader
getObjectBody :: BinHandle -> ModuleName -> IO Object
getObjectBody bh0 mod_name = do
  -- Read the string table
  dict <- forwardGet bh0 (getDictionary bh0)
  let bh = setUserData bh0 $ noUserData { ud_get_fs = getDictFastString dict }

  deps     <- get bh
  idx      <- forwardGet bh (get bh)
  payload_pos <- tellBin bh

  pure $ Object
    { objModuleName    = mod_name
    , objHandle        = bh
    , objPayloadOffset = payload_pos
    , objDeps          = deps
    , objIndex         = idx
    }

-- | Parse object
getObject :: BinHandle -> IO (Maybe Object)
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

-- | Reads only the part necessary to get the dependencies
readObjectDeps :: FilePath -> IO (Maybe Deps)
readObjectDeps file = do
  bh <- readBinMem file
  getObject bh >>= \case
    Just obj -> pure $! Just $! objDeps obj
    Nothing  -> pure Nothing

-- | Get units in the object file, using the given filtering function
getObjectUnits :: Object -> (Word -> IndexEntry -> Bool) -> IO [ObjUnit]
getObjectUnits obj pred = mapMaybeM read_entry (zip (objIndex obj) [0..])
  where
    bh = objHandle obj
    read_entry (e@(IndexEntry syms offset),i)
      | pred i e  = do
          seekBin bh offset
          Just <$> getObjUnit syms bh
      | otherwise = pure Nothing

-- | Read units in the object file, using the given filtering function
readObjectUnits :: FilePath -> (Word -> IndexEntry -> Bool) -> IO [ObjUnit]
readObjectUnits file pred = do
  readObject file >>= \case
    Nothing  -> pure []
    Just obj -> getObjectUnits obj pred


--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

putEnum :: Enum a => BinHandle -> a -> IO ()
putEnum bh x | n > 65535 = error ("putEnum: out of range: " ++ show n)
             | otherwise = put_ bh n
  where n = fromIntegral $ fromEnum x :: Word16

getEnum :: Enum a => BinHandle -> IO a
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

instance Binary Sat.JStat where
  put_ bh (Sat.DeclStat i e)       = putByte bh 1  >> put_ bh i >> put_ bh e
  put_ bh (Sat.ReturnStat e)       = putByte bh 2  >> put_ bh e
  put_ bh (Sat.IfStat e s1 s2)     = putByte bh 3  >> put_ bh e  >> put_ bh s1 >> put_ bh s2
  put_ bh (Sat.WhileStat b e s)    = putByte bh 4  >> put_ bh b  >> put_ bh e  >> put_ bh s
  put_ bh (Sat.ForInStat b i e s)  = putByte bh 5  >> put_ bh b  >> put_ bh i  >> put_ bh e  >> put_ bh s
  put_ bh (Sat.SwitchStat e ss s)  = putByte bh 6  >> put_ bh e  >> put_ bh ss >> put_ bh s
  put_ bh (Sat.TryStat s1 i s2 s3) = putByte bh 7  >> put_ bh s1 >> put_ bh i  >> put_ bh s2 >> put_ bh s3
  put_ bh (Sat.BlockStat xs)       = putByte bh 8  >> put_ bh xs
  put_ bh (Sat.ApplStat e es)      = putByte bh 9  >> put_ bh e  >> put_ bh es
  put_ bh (Sat.UOpStat o e)        = putByte bh 10 >> put_ bh o  >> put_ bh e
  put_ bh (Sat.AssignStat e1 e2)   = putByte bh 11 >> put_ bh e1 >> put_ bh e2
  put_ bh (Sat.LabelStat l s)      = putByte bh 12 >> put_ bh l  >> put_ bh s
  put_ bh (Sat.BreakStat ml)       = putByte bh 13 >> put_ bh ml
  put_ bh (Sat.ContinueStat ml)    = putByte bh 14 >> put_ bh ml
  get bh = getByte bh >>= \case
    1  -> Sat.DeclStat     <$> get bh <*> get bh
    2  -> Sat.ReturnStat   <$> get bh
    3  -> Sat.IfStat       <$> get bh <*> get bh <*> get bh
    4  -> Sat.WhileStat    <$> get bh <*> get bh <*> get bh
    5  -> Sat.ForInStat    <$> get bh <*> get bh <*> get bh <*> get bh
    6  -> Sat.SwitchStat   <$> get bh <*> get bh <*> get bh
    7  -> Sat.TryStat      <$> get bh <*> get bh <*> get bh <*> get bh
    8  -> Sat.BlockStat    <$> get bh
    9  -> Sat.ApplStat     <$> get bh <*> get bh
    10 -> Sat.UOpStat      <$> get bh <*> get bh
    11 -> Sat.AssignStat   <$> get bh <*> get bh
    12 -> Sat.LabelStat    <$> get bh <*> get bh
    13 -> Sat.BreakStat    <$> get bh
    14 -> Sat.ContinueStat <$> get bh
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
  put_ bh (Sat.JHash m)     = putByte bh 7 >> put_ bh (sortOn (LexicalFastString . fst) $ nonDetUniqMapToList m)
  put_ bh (Sat.JFunc is s)  = putByte bh 8 >> put_ bh is >> put_ bh s
  get bh = getByte bh >>= \case
    1 -> Sat.JVar    <$> get bh
    2 -> Sat.JList   <$> get bh
    3 -> Sat.JDouble <$> get bh
    4 -> Sat.JInt    <$> get bh
    5 -> Sat.JStr    <$> get bh
    6 -> Sat.JRegEx  <$> get bh
    7 -> Sat.JHash . listToUniqMap <$> get bh
    8 -> Sat.JFunc   <$> get bh <*> get bh
    n -> error ("Binary get bh Sat.JVal: invalid tag: " ++ show n)

instance Binary Ident where
  put_ bh (TxtI xs) = put_ bh xs
  get bh = TxtI <$> get bh

-- we need to preserve NaN and infinities, unfortunately the Binary instance for Double does not do this
instance Binary Sat.SaneDouble where
  put_ bh (Sat.SaneDouble d)
    | isNaN d               = putByte bh 1
    | isInfinite d && d > 0 = putByte bh 2
    | isInfinite d && d < 0 = putByte bh 3
    | isNegativeZero d      = putByte bh 4
    | otherwise             = putByte bh 5 >> put_ bh (castDoubleToWord64 d)
  get bh = getByte bh >>= \case
    1 -> pure $ Sat.SaneDouble (0    / 0)
    2 -> pure $ Sat.SaneDouble (1    / 0)
    3 -> pure $ Sat.SaneDouble ((-1) / 0)
    4 -> pure $ Sat.SaneDouble (-0)
    5 -> Sat.SaneDouble . castWord64ToDouble <$> get bh
    n -> error ("Binary get bh SaneDouble: invalid tag: " ++ show n)

-- FIXME: remove after Unsat replaces JStat
-- we need to preserve NaN and infinities, unfortunately the Binary instance for Double does not do this
instance Binary SaneDouble where
  put_ bh (SaneDouble d)
    | isNaN d               = putByte bh 1
    | isInfinite d && d > 0 = putByte bh 2
    | isInfinite d && d < 0 = putByte bh 3
    | isNegativeZero d      = putByte bh 4
    | otherwise             = putByte bh 5 >> put_ bh (castDoubleToWord64 d)
  get bh = getByte bh >>= \case
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
  put_ bh (StaticFun f args)   = putByte bh 1 >> put_ bh f  >> put_ bh args
  put_ bh (StaticThunk t)      = putByte bh 2 >> put_ bh t
  put_ bh (StaticUnboxed u)    = putByte bh 3 >> put_ bh u
  put_ bh (StaticData dc args) = putByte bh 4 >> put_ bh dc >> put_ bh args
  put_ bh (StaticList xs t)    = putByte bh 5 >> put_ bh xs >> put_ bh t
  get bh = getByte bh >>= \case
    1 -> StaticFun     <$> get bh <*> get bh
    2 -> StaticThunk   <$> get bh
    3 -> StaticUnboxed <$> get bh
    4 -> StaticData    <$> get bh <*> get bh
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
