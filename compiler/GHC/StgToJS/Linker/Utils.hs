{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE MultiWayIf  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Linker.Utils
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
-- Various utilies used in the JS Linker
--
-----------------------------------------------------------------------------

module GHC.StgToJS.Linker.Utils
  ( jsExeFileName
  , getInstalledPackageLibDirs
  , getInstalledPackageHsLibs
  , commonCppDefs
  , decodeModifiedUTF8
  )
where

import           System.FilePath
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import           Data.ByteString (ByteString)

import          GHC.Driver.Session

import          GHC.Data.ShortText
import          GHC.Unit.State
import          GHC.Unit.Types

import          GHC.StgToJS.Types

import           Prelude
import GHC.Platform
import Data.List (isPrefixOf)

import GHC.Builtin.Types
import Language.Haskell.Syntax.Basic
import GHC.Types.Name
import GHC.StgToJS.Ids
import GHC.JS.Ident
import GHC.Core.DataCon
import GHC.Data.FastString

-- | Retrieve library directories provided by the @UnitId@ in @UnitState@
getInstalledPackageLibDirs :: UnitState -> UnitId -> [ShortText]
getInstalledPackageLibDirs us = maybe mempty unitLibraryDirs . lookupUnitId us

-- | Retrieve the names of the libraries provided by @UnitId@
getInstalledPackageHsLibs :: UnitState -> UnitId -> [ShortText]
getInstalledPackageHsLibs us = maybe mempty unitLibraries . lookupUnitId us

-- | A constant holding the JavaScript executable Filename extension
jsexeExtension :: String
jsexeExtension = "jsexe"

-- | CPP definitions that are inserted into every .pp file
commonCppDefs :: Bool -> ByteString
commonCppDefs profiling = case profiling of
  True  -> commonCppDefs_profiled
  False -> commonCppDefs_vanilla

-- | CPP definitions for normal operation and profiling. Use CAFs for
-- commonCppDefs_* so that they are shared for every CPP file
commonCppDefs_vanilla, commonCppDefs_profiled :: ByteString
commonCppDefs_vanilla  = genCommonCppDefs False
commonCppDefs_profiled = genCommonCppDefs True

-- | Generate macros MK_TUP* for tuples sized 2+.
genMkTup :: Bool -> Int -> ByteString
genMkTup profiling n = mconcat
  [ "#define MK_TUP", sn                                        -- #define MK_TUPn
  , "(", B.intercalate "," xs, ")"                              -- (x1,x2,...)
  , "(h$c", sn, "("                                             -- (h$cn(
  , bytesFS symbol, ","                                         -- h$ghczminternalZCGHCziInternalziTupleziZnT_con_e,
  , B.intercalate "," $ map (\x -> "(" <> x <> ")") xs          -- (x1),(x2),(...)
  , if profiling                                                -- ,h$currentThread?h$currentThread.ccs:h$CCS_SYSTEM
      then ",h$currentThread?h$currentThread.ccs:h$CCS_SYSTEM"
      else ""
  , "))\n"                                                      -- ))\n
  ]
  where
    xs = take n $ map (("x" <>) . Char8.pack . show) ([1..] :: [Int])
    sn = Char8.pack $ show n
    symbol = identFS $ makeIdentForId (dataConWorkId $ tupleDataCon Boxed n) Nothing IdConEntry mod
    name = tupleDataConName Boxed n
    mod = case nameModule_maybe name of
      Just m -> m
      Nothing -> error "Tuple constructor is missing a module"

-- | Generate CPP Definitions depending on a profiled or normal build. This
-- occurs at link time.
genCommonCppDefs :: Bool -> ByteString
genCommonCppDefs profiling = mconcat
  [
  -- constants
    let mk_int_def n v   = "#define " <> Char8.pack n <> " (" <> Char8.pack (show v) <> ")\n"
        -- generate "#define CLOSURE_TYPE_xyz (num)" defines
        mk_closure_def t = mk_int_def (ctJsName t) (ctNum t)
        closure_defs     = map mk_closure_def [minBound..maxBound]
        -- generate "#define THREAD_xyz_xyz (num)" defines
        mk_thread_def t  = mk_int_def (threadStatusJsName t) (threadStatusNum t)
        thread_defs      = map mk_thread_def [minBound..maxBound]
    in mconcat (closure_defs ++ thread_defs)

  -- low-level heap object manipulation macros
  , mconcat (map (genMkTup profiling) [2..10])

  , "#define TUP2_1(x) ((x).d1)\n"
  , "#define TUP2_2(x) ((x).d2)\n"

  -- GHCJS.Prim.JSVal
  , if profiling
      then "#define MK_JSVAL(x) (h$c1(h$ghczminternalZCGHCziInternalziJSziPrimziJSVal_con_e, (x), h$CCS_SYSTEM))\n"
      else "#define MK_JSVAL(x) (h$c1(h$ghczminternalZCGHCziInternalziJSziPrimziJSVal_con_e, (x)))\n"
  ,  "#define JSVAL_VAL(x) ((x).d1)\n"

  -- GHCJS.Prim.JSException
  , if profiling
      then "#define MK_JSEXCEPTION(msg,hsMsg) (h$c2(h$ghczminternalZCGHCziInternalziJSziPrimziJSException_con_e,(msg),(hsMsg),h$CCS_SYSTEM))\n"
      else "#define MK_JSEXCEPTION(msg,hsMsg) (h$c2(h$ghczminternalZCGHCziInternalziJSziPrimziJSException_con_e,(msg),(hsMsg)))\n"

  -- Exception dictionary for JSException
  , "#define HS_JSEXCEPTION_EXCEPTION h$ghczminternalZCGHCziInternalziJSziPrimzizdfExceptionJSException\n"

  -- SomeException
  , if profiling
      then "#define MK_SOMEEXCEPTION(dict,except) (h$c2(h$ghczminternalZCGHCziInternalziExceptionziTypeziSomeException_con_e,(dict),(except),h$CCS_SYSTEM))\n"
      else "#define MK_SOMEEXCEPTION(dict,except) (h$c2(h$ghczminternalZCGHCziInternalziExceptionziTypeziSomeException_con_e,(dict),(except)))\n"

  -- GHC.Ptr.Ptr
  , if profiling
      then "#define MK_PTR(val,offset) (h$c2(h$ghczminternalZCGHCziInternalziPtrziPtr_con_e, (val), (offset), h$CCS_SYSTEM))\n"
      else "#define MK_PTR(val,offset) (h$c2(h$ghczminternalZCGHCziInternalziPtrziPtr_con_e, (val), (offset)))\n"

  -- Put Addr# in ByteArray# or at Addr# (same thing)
  , "#define PUT_ADDR(a,o,va,vo) if (!(a).arr) (a).arr = []; (a).arr[o] = va; (a).dv.setInt32(o,vo,true);\n"
  , "#define GET_ADDR(a,o,ra,ro) var ra = (((a).arr && (a).arr[o]) ? (a).arr[o] : null); var ro = (a).dv.getInt32(o,true);\n"

  -- Data.Maybe.Maybe
  , "#define HS_NOTHING h$ghczminternalZCGHCziInternalziMaybeziNothing\n"
  , "#define IS_NOTHING(cl) ((cl).f === h$ghczminternalZCGHCziInternalziMaybeziNothing_con_e)\n"
  , "#define IS_JUST(cl) ((cl).f === h$ghczminternalZCGHCziInternalziMaybeziJust_con_e)\n"
  , "#define JUST_VAL(jj) ((jj).d1)\n"
  -- "#define HS_NOTHING h$nothing\n"
  , if profiling
      then "#define MK_JUST(val) (h$c1(h$ghczminternalZCGHCziInternalziMaybeziJust_con_e, (val), h$CCS_SYSTEM))\n"
      else "#define MK_JUST(val) (h$c1(h$ghczminternalZCGHCziInternalziMaybeziJust_con_e, (val)))\n"

  -- Data.List
  , "#define HS_NIL h$ghczminternalZCGHCziInternalziTypesziZMZN\n"
  , "#define HS_NIL_CON h$ghczminternalZCGHCziInternalziTypesziZMZN_con_e\n"
  , "#define IS_CONS(cl) ((cl).f === h$ghczminternalZCGHCziInternalziTypesziZC_con_e)\n"
  , "#define IS_NIL(cl) ((cl).f === h$ghczminternalZCGHCziInternalziTypesziZMZN_con_e)\n"
  , "#define CONS_HEAD(cl) ((cl).d1)\n"
  , "#define CONS_TAIL(cl) ((cl).d2)\n"
  , if profiling
      then mconcat
        [ "#define MK_CONS(head,tail) (h$c2(h$ghczminternalZCGHCziInternalziTypesziZC_con_e, (head), (tail), h$CCS_SYSTEM))\n"
        , "#define MK_CONS_CC(head,tail,cc) (h$c2(h$ghczminternalZCGHCziInternalziTypesziZC_con_e, (head), (tail), (cc)))\n"
        ]
      else mconcat
        [ "#define MK_CONS(head,tail) (h$c2(h$ghczminternalZCGHCziInternalziTypesziZC_con_e, (head), (tail)))\n"
        , "#define MK_CONS_CC(head,tail,cc) (h$c2(h$ghczminternalZCGHCziInternalziTypesziZC_con_e, (head), (tail)))\n"
        ]

  -- Data.Text
  , "#define DATA_TEXT_ARRAY(x) ((x).d1)\n"
  , "#define DATA_TEXT_OFFSET(x) ((x).d2.d1)\n"
  , "#define DATA_TEXT_LENGTH(x) ((x).d2.d2)\n"

  -- Data.Text.Lazy
  , "#define LAZY_TEXT_IS_CHUNK(x) ((x).f.a === 2)\n"
  , "#define LAZY_TEXT_IS_NIL(x) ((x).f.a === 1)\n"
  , "#define LAZY_TEXT_CHUNK_HEAD(x) ((x))\n"
  , "#define LAZY_TEXT_CHUNK_TAIL(x) ((x).d2.d3)\n"

  -- black holes
  -- can we skip the indirection for black holes?
  , "#define IS_BLACKHOLE(x) (typeof (x) === 'object' && (x) && (x).f && (x).f.t === CLOSURE_TYPE_BLACKHOLE)\n"
  , "#define BLACKHOLE_TID(bh) ((bh).d1)\n"
  , "#define SET_BLACKHOLE_TID(bh,tid) ((bh).d1 = (tid))\n"
  , "#define BLACKHOLE_QUEUE(bh) ((bh).d2)\n"
  , "#define SET_BLACKHOLE_QUEUE(bh,val) ((bh).d2 = (val))\n"

  -- resumable thunks
  , "#define MAKE_RESUMABLE(closure,stack) { (closure).f = h$resume_e; (closure).d1 = (stack), (closure).d2 = null; }\n"

  -- making a thunk
  , "#define MK_UPD_THUNK(closure) h$c1(h$upd_thunk_e,(closure))\n"

  -- general deconstruction
  , "#define IS_THUNK(x) ((x).f.t === CLOSURE_TYPE_THUNK)\n"
  , "#define CONSTR_TAG(x) ((x).f.a)\n"

  -- retrieve a numeric value that's possibly stored as an indirection
  , "#define IS_WRAPPED_NUMBER(val) ((typeof(val)==='object')&&(val).f === h$unbox_e)\n"
  , "#define UNWRAP_NUMBER(val) ((typeof(val) === 'number')?(val):(val).d1)\n"

  -- generic lazy values
  , if profiling
      then mconcat
        [ "#define MK_LAZY(fun) (h$c1(h$lazy_e, (fun), h$CCS_SYSTEM))\n"
        , "#define MK_LAZY_CC(fun,cc) (h$c1(h$lazy_e, (fun), (cc)))\n"
        ]
      else mconcat
        [ "#define MK_LAZY(fun) (h$c1(h$lazy_e, (fun)))\n"
        , "#define MK_LAZY_CC(fun,cc) (h$c1(h$lazy_e, (fun)))\n"
        ]

  -- generic data constructors and selectors
  , if profiling
      then mconcat
        [ "#define MK_DATA1_1(val) (h$c1(h$data1_e, (val), h$CCS_SYSTEM))\n"
        , "#define MK_DATA1_2(val1,val2) (h$c2(h$data1_e, (val1), (val2), h$CCS_SYSTEM))\n"
        , "#define MK_DATA2_1(val) (h$c1(h$data2_e, (val), h$CCS_SYSTEM))\n"
        , "#define MK_DATA2_2(val1,val2) (h$c2(h$data1_e, (val1), (val2), h$CCS_SYSTEM))\n"
        , "#define MK_SELECT1(val) (h$c1(h$select1_e, (val), h$CCS_SYSTEM))\n"
        , "#define MK_SELECT2(val) (h$c1(h$select2_e, (val), h$CCS_SYSTEM))\n"
        , "#define MK_AP1(fun,val) (h$c2(h$ap1_e, (fun), (val), h$CCS_SYSTEM))\n"
        , "#define MK_AP2(fun,val1,val2) (h$c3(h$ap2_e, (fun), (val1), (val2), h$CCS_SYSTEM))\n"
        , "#define MK_AP3(fun,val1,val2,val3) (h$c4(h$ap3_e, (fun), (val1), (val2), (val3), h$CCS_SYSTEM))\n"
        ]
      else mconcat
        [ "#define MK_DATA1_1(val) (h$c1(h$data1_e, (val)))\n"
        , "#define MK_DATA1_2(val1,val2) (h$c2(h$data1_e, (val1), (val2)))\n"
        , "#define MK_DATA2_1(val) (h$c1(h$data2_e, (val)))\n"
        , "#define MK_DATA2_2(val1,val2) (h$c2(h$data2_e, (val1), (val2)))\n"
        , "#define MK_SELECT1(val) (h$c1(h$select1_e, (val)))\n"
        , "#define MK_SELECT2(val) (h$c1(h$select2_e, (val)))\n"
        , "#define MK_AP1(fun,val) (h$c2(h$ap1_e,(fun),(val)))\n"
        , "#define MK_AP2(fun,val1,val2) (h$c3(h$ap2_e,(fun),(val1),(val2)))\n"
        , "#define MK_AP3(fun,val1,val2,val3) (h$c4(h$ap3_e, (fun), (val1), (val2), (val3)))\n"
        ]

  -- unboxed tuple returns
  -- , "#define RETURN_UBX_TUP1(x) return x;\n"
  , "#define RETURN_UBX_TUP2(x1,x2) { h$ret1 = (x2); return (x1); }\n"
  , "#define RETURN_UBX_TUP3(x1,x2,x3) { h$ret1 = (x2); h$ret2 = (x3); return (x1); }\n"
  , "#define RETURN_UBX_TUP4(x1,x2,x3,x4) { h$ret1 = (x2); h$ret2 = (x3); h$ret3 = (x4); return (x1); }\n"
  , "#define RETURN_UBX_TUP5(x1,x2,x3,x4,x5) { h$ret1 = (x2); h$ret2 = (x3); h$ret3 = (x4); h$ret4 = (x5); return (x1); }\n"
  , "#define RETURN_UBX_TUP6(x1,x2,x3,x4,x5,x6) { h$ret1 = (x2); h$ret2 = (x3); h$ret3 = (x4); h$ret4 = (x5); h$ret5 = (x6); return (x1); }\n"
  , "#define RETURN_UBX_TUP7(x1,x2,x3,x4,x5,x6,x7) { h$ret1 = (x2); h$ret2 = (x3); h$ret3 = (x4); h$ret4 = (x5); h$ret5 = (x6); h$ret6 = (x7); return (x1); }\n"
  , "#define RETURN_UBX_TUP8(x1,x2,x3,x4,x5,x6,x7,x8) { h$ret1 = (x2); h$ret2 = (x3); h$ret3 = (x4); h$ret4 = (x5); h$ret5 = (x6); h$ret6 = (x7); h$ret7 = (x8); return (x1); }\n"
  , "#define RETURN_UBX_TUP9(x1,x2,x3,x4,x5,x6,x7,x8,x9) { h$ret1 = (x2); h$ret2 = (x3); h$ret3 = (x4); h$ret4 = (x5); h$ret5 = (x6); h$ret6 = (x7); h$ret7 = (x8); h$ret8 = (x9); return (x1); }\n"
  , "#define RETURN_UBX_TUP10(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) { h$ret1 = (x2); h$ret2 = (x3); h$ret3 = (x4); h$ret4 = (x5); h$ret5 = (x6); h$ret6 = (x7); h$ret7 = (x8); h$ret8 = (x9); h$ret9 = (x10); return (x1); }\n"

  , "#define RETURN_INT64(h,l) RETURN_UBX_TUP2((h)|0,(l)>>>0)\n"
  , "#define RETURN_WORD64(h,l) RETURN_UBX_TUP2((h)>>>0,(l)>>>0)\n"
  , "#define RETURN_ADDR(a,o) RETURN_UBX_TUP2(a,o)\n"

  , "#define CALL_UBX_TUP2(r1,r2,c) { (r1) = (c); (r2) = h$ret1; }\n"
  , "#define CALL_UBX_TUP3(r1,r2,r3,c) { (r1) = (c); (r2) = h$ret1; (r3) = h$ret2; }\n"
  , "#define CALL_UBX_TUP4(r1,r2,r3,r4,c) { (r1) = (c); (r2) = h$ret1; (r3) = h$ret2; (r4) = h$ret3; }\n"
  , "#define CALL_UBX_TUP5(r1,r2,r3,r4,r5,c) { (r1) = (c); (r2) = h$ret1; (r3) = h$ret2; (r4) = h$ret3; (r5) = h$ret4; }\n"
  , "#define CALL_UBX_TUP6(r1,r2,r3,r4,r5,r6,c) { (r1) = (c); (r2) = h$ret1; (r3) = h$ret2; (r4) = h$ret3; (r5) = h$ret4; (r6) = h$ret5; }\n"
  , "#define CALL_UBX_TUP7(r1,r2,r3,r4,r5,r6,r7,c) { (r1) = (c); (r2) = h$ret1; (r3) = h$ret2; (r4) = h$ret3; (r5) = h$ret4; (r6) = h$ret5; (r7) = h$ret6; }\n"
  , "#define CALL_UBX_TUP8(r1,r2,r3,r4,r5,r6,r7,r8,c) { (r1) = (c); (r2) = h$ret1; (r3) = h$ret2; (r4) = h$ret3; (r5) = h$ret4; (r6) = h$ret5; (r7) = h$ret6; (r8) = h$ret7; }\n"
  , "#define CALL_UBX_TUP9(r1,r2,r3,r4,r5,r6,r7,r8,r9,c) { (r1) = (c); (r2) = h$ret1; (r3) = h$ret2; (r4) = h$ret3; (r5) = h$ret4; (r6) = h$ret5; (r7) = h$ret6; (r8) = h$ret7; (r9) = h$ret8; }\n"
  , "#define CALL_UBX_TUP10(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,c) { (r1) = (c); (r2) = h$ret1; (r3) = h$ret2; (r4) = h$ret3; (r5) = h$ret4; (r6) = h$ret5; (r7) = h$ret6; (r8) = h$ret7; (r9) = h$ret8; (r10) = h$ret9; }\n"
  ]

-- | Construct the Filename for the "binary" of Haskell code compiled to
-- JavaScript.
jsExeFileName :: DynFlags -> FilePath
jsExeFileName dflags
  | Just s <- outputFile_ dflags =
      -- unmunge the extension
      let s' = dropPrefix "js_" (drop 1 $ takeExtension s)
      in if Prelude.null s'
           then dropExtension s <.> jsexeExtension
           else dropExtension s <.> s'
  | otherwise =
      if platformOS (targetPlatform dflags) == OSMinGW32
           then "main.jsexe"
           else "a.jsexe"
  where
    dropPrefix prefix xs
      | prefix `isPrefixOf` xs = drop (length prefix) xs
      | otherwise              = xs

-- GHC produces string literals in ByteString.
-- When ByteString has all bytes UTF-8 compatbile we make attempt to
-- represent it as FastString.
-- Otherwise (for example when string literal encodes long integers or zero bytes) we
-- leave it as is.
-- Having zero bytes points that this literal never was assumed to be a Modified UTF8 compatible.
decodeModifiedUTF8 :: B.ByteString -> Maybe FastString
decodeModifiedUTF8 bs
  | B.any (==0) bs         = Nothing
  | not $ B.isValidUtf8 bs = Nothing
  | otherwise              = Just . mkFastStringByteString $ bs
