
{- ------------------------------------------------------------------------

(c) The GHC Team, 1992-2012

DeriveConstants is a program that extracts information from the C
declarations in the header files (primarily struct field offsets)
and generates various files, such as a header file that can be #included
into non-C source containing this information.

------------------------------------------------------------------------ -}

import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Numeric
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Info
import System.Process

main :: IO ()
main = do opts <- parseArgs
          let getOption descr opt = case opt opts of
                                    Just x -> return x
                                    Nothing -> die ("No " ++ descr ++ " given")
          mode <- getOption "mode" o_mode
          fn <- getOption "output filename" o_outputFilename
          case mode of
              Gen_Haskell_Type     -> writeHaskellType     fn haskellWanteds
              Gen_Haskell_Wrappers -> writeHaskellWrappers fn haskellWanteds
              Gen_Haskell_Exports  -> writeHaskellExports  fn haskellWanteds
              Gen_Computed cm ->
                  do tmpdir  <- getOption "tmpdir"      o_tmpdir
                     gccProg <- getOption "gcc program" o_gccProg
                     nmProg  <- getOption "nm program"  o_nmProg
                     let verbose = o_verbose opts
                         gccFlags = o_gccFlags opts
                     rs <- getWanted verbose tmpdir gccProg gccFlags nmProg
                     let haskellRs = [ what
                                     | (wh, what) <- rs
                                     , wh `elem` [Haskell, Both] ]
                         cRs = [ what
                               | (wh, what) <- rs
                               , wh `elem` [C, Both] ]
                     case cm of
                         ComputeHaskell -> writeHaskellValue fn haskellRs
                         ComputeHeader  -> writeHeader       fn cRs
    where haskellWanteds = [ what | (wh, what) <- wanteds,
                                    wh `elem` [Haskell, Both] ]

data Options = Options {
                   o_verbose :: Bool,
                   o_mode :: Maybe Mode,
                   o_tmpdir :: Maybe FilePath,
                   o_outputFilename :: Maybe FilePath,
                   o_gccProg :: Maybe FilePath,
                   o_gccFlags :: [String],
                   o_nmProg :: Maybe FilePath
               }

parseArgs :: IO Options
parseArgs = do args <- getArgs
               opts <- f emptyOptions args
               return (opts {o_gccFlags = reverse (o_gccFlags opts)})
    where emptyOptions = Options {
                             o_verbose = False,
                             o_mode = Nothing,
                             o_tmpdir = Nothing,
                             o_outputFilename = Nothing,
                             o_gccProg = Nothing,
                             o_gccFlags = [],
                             o_nmProg = Nothing
                         }
          f opts [] = return opts
          f opts ("-v" : args')
              = f (opts {o_verbose = True}) args'
          f opts ("--gen-haskell-type" : args')
              = f (opts {o_mode = Just Gen_Haskell_Type}) args'
          f opts ("--gen-haskell-value" : args')
              = f (opts {o_mode = Just (Gen_Computed ComputeHaskell)}) args'
          f opts ("--gen-haskell-wrappers" : args')
              = f (opts {o_mode = Just Gen_Haskell_Wrappers}) args'
          f opts ("--gen-haskell-exports" : args')
              = f (opts {o_mode = Just Gen_Haskell_Exports}) args'
          f opts ("--gen-header" : args')
              = f (opts {o_mode = Just (Gen_Computed ComputeHeader)}) args'
          f opts ("--tmpdir" : dir : args')
              = f (opts {o_tmpdir = Just dir}) args'
          f opts ("-o" : fn : args')
              = f (opts {o_outputFilename = Just fn}) args'
          f opts ("--gcc-program" : prog : args')
              = f (opts {o_gccProg = Just prog}) args'
          f opts ("--gcc-flag" : flag : args')
              = f (opts {o_gccFlags = flag : o_gccFlags opts}) args'
          f opts ("--nm-program" : prog : args')
              = f (opts {o_nmProg = Just prog}) args'
          f _ (flag : _) = die ("Unrecognised flag: " ++ show flag)

data Mode = Gen_Haskell_Type
          | Gen_Haskell_Wrappers
          | Gen_Haskell_Exports
          | Gen_Computed ComputeMode

data ComputeMode = ComputeHaskell | ComputeHeader

type Wanteds = [(Where, What Fst)]
type Results = [(Where, What Snd)]

type Name = String
newtype CExpr = CExpr String
newtype CPPExpr = CPPExpr String
data What f = GetFieldType   Name (f CExpr   Integer)
            | GetClosureSize Name (f CExpr   Integer)
            | GetWord        Name (f CExpr   Integer)
            | GetInt         Name (f CExpr   Integer)
            | GetNatural     Name (f CExpr   Integer)
            | GetBool        Name (f CPPExpr Bool)
            | StructFieldMacro    Name
            | ClosureFieldMacro   Name
            | ClosurePayloadMacro Name
            | FieldTypeGcptrMacro Name

data Fst a b = Fst a
data Snd a b = Snd b

data Where = C | Haskell | Both
    deriving Eq

constantInt :: Where -> Name -> String -> Wanteds
constantInt w name expr = [(w, GetInt name (Fst (CExpr expr)))]

constantWord :: Where -> Name -> String -> Wanteds
constantWord w name expr = [(w, GetWord name (Fst (CExpr expr)))]

constantNatural :: Where -> Name -> String -> Wanteds
constantNatural w name expr = [(w, GetNatural name (Fst (CExpr expr)))]

constantBool :: Where -> Name -> String -> Wanteds
constantBool w name expr = [(w, GetBool name (Fst (CPPExpr expr)))]

fieldOffset :: Where -> String -> String -> Wanteds
fieldOffset w theType theField = fieldOffset_ w nameBase theType theField
    where nameBase = theType ++ "_" ++ theField

fieldOffset_ :: Where -> Name -> String -> String -> Wanteds
fieldOffset_ w nameBase theType theField = [(w, GetWord name (Fst (CExpr expr)))]
    where name = "OFFSET_" ++ nameBase
          expr = "offsetof(" ++ theType ++ ", " ++ theField ++ ")"

-- FieldType is for defining REP_x to be b32 etc
-- These are both the C-- types used in a load
--    e.g.  b32[addr]
-- and the names of the CmmTypes in the compiler
--    b32 :: CmmType
fieldType' :: Where -> String -> String -> Wanteds
fieldType' w theType theField
    = fieldType_' w nameBase theType theField
    where nameBase = theType ++ "_" ++ theField

fieldType_' :: Where -> Name -> String -> String -> Wanteds
fieldType_' w nameBase theType theField
    = [(w, GetFieldType name (Fst (CExpr expr)))]
    where name = "REP_" ++ nameBase
          expr = "FIELD_SIZE(" ++ theType ++ ", " ++ theField ++ ")"

structField :: Where -> String -> String -> Wanteds
structField = structFieldHelper C

structFieldH :: Where -> String -> String -> Wanteds
structFieldH w = structFieldHelper w w

structField_ :: Where -> Name -> String -> String -> Wanteds
structField_ w nameBase theType theField
    = fieldOffset_ w nameBase theType theField
   ++ fieldType_' C nameBase theType theField
   ++ structFieldMacro nameBase

structFieldMacro :: Name -> Wanteds
structFieldMacro nameBase = [(C, StructFieldMacro nameBase)]

-- Outputs the byte offset and MachRep for a field
structFieldHelper :: Where -> Where -> String -> String -> Wanteds
structFieldHelper wFT w theType theField = fieldOffset w theType theField
                                        ++ fieldType' wFT theType theField
                                        ++ structFieldMacro nameBase
    where nameBase = theType ++ "_" ++ theField

closureFieldMacro :: Name -> Wanteds
closureFieldMacro nameBase = [(C, ClosureFieldMacro nameBase)]

closurePayload :: Where -> String -> String -> Wanteds
closurePayload w theType theField
    = closureFieldOffset_ w nameBase theType theField
   ++ closurePayloadMacro nameBase
    where nameBase = theType ++ "_" ++ theField

closurePayloadMacro :: Name -> Wanteds
closurePayloadMacro nameBase = [(C, ClosurePayloadMacro nameBase)]

-- Byte offset and MachRep for a closure field, minus the header
closureField_ :: Where -> Name -> String -> String -> Wanteds
closureField_ w nameBase theType theField
    = closureFieldOffset_ w nameBase theType theField
   ++ fieldType_' C nameBase theType theField
   ++ closureFieldMacro nameBase

closureField :: Where -> String -> String -> Wanteds
closureField w theType theField = closureField_ w nameBase theType theField
    where nameBase = theType ++ "_" ++ theField

closureFieldOffset_ :: Where -> Name -> String -> String -> Wanteds
closureFieldOffset_ w nameBase theType theField
    = defOffset w nameBase (CExpr ("offsetof(" ++ theType ++ ", " ++ theField ++ ") - TYPE_SIZE(StgHeader)"))

-- Size of a closure type, minus the header, named SIZEOF_<type>_NoHdr
-- Also, we #define SIZEOF_<type> to be the size of the whole closure for .cmm.
closureSize :: Where -> String -> Wanteds
closureSize w theType = defSize        w (theType ++ "_NoHdr") (CExpr expr)
                     ++ defClosureSize C theType               (CExpr expr)
    where expr = "TYPE_SIZE(" ++ theType ++ ") - TYPE_SIZE(StgHeader)"

-- Byte offset and MachRep for a closure field, minus the header
closureFieldGcptr :: Where -> String -> String -> Wanteds
closureFieldGcptr w theType theField
    = closureFieldOffset_ w nameBase theType theField
   ++ fieldTypeGcptr nameBase
   ++ closureFieldMacro nameBase
    where nameBase = theType ++ "_" ++ theField

fieldTypeGcptr :: Name -> Wanteds
fieldTypeGcptr nameBase = [(C, FieldTypeGcptrMacro nameBase)]

closureFieldOffset :: Where -> String -> String -> Wanteds
closureFieldOffset w theType theField
    = defOffset w nameBase (CExpr expr)
    where nameBase = theType ++ "_" ++ theField
          expr = "offsetof(" ++ theType ++ ", " ++ theField ++ ") - TYPE_SIZE(StgHeader)"

thunkSize :: Where -> String -> Wanteds
thunkSize w theType
    = defSize w (theType ++ "_NoThunkHdr") (CExpr expr)
  ++ closureSize w theType
    where expr = "TYPE_SIZE(" ++ theType ++ ") - TYPE_SIZE(StgThunkHeader)"

defIntOffset :: Where -> Name -> String -> Wanteds
defIntOffset w nameBase cExpr = [(w, GetInt ("OFFSET_" ++ nameBase) (Fst (CExpr cExpr)))]

defOffset :: Where -> Name -> CExpr -> Wanteds
defOffset w nameBase cExpr = [(w, GetWord ("OFFSET_" ++ nameBase) (Fst cExpr))]

structSize :: Where -> String -> Wanteds
structSize w theType = defSize w theType (CExpr ("TYPE_SIZE(" ++ theType ++ ")"))

defSize :: Where -> Name -> CExpr -> Wanteds
defSize w nameBase cExpr = [(w, GetWord ("SIZEOF_" ++ nameBase) (Fst cExpr))]

defClosureSize :: Where -> Name -> CExpr -> Wanteds
defClosureSize w nameBase cExpr = [(w, GetClosureSize ("SIZEOF_" ++ nameBase) (Fst cExpr))]

haskellise :: Name -> Name
haskellise (c : cs) = toLower c : cs
haskellise "" = ""

wanteds :: Wanteds
wanteds = concat
          [-- Closure header sizes.
           constantWord Both "STD_HDR_SIZE"
                             -- grrr.. PROFILING is on so we need to
                             -- subtract sizeofW(StgProfHeader)
                             "sizeofW(StgHeader) - sizeofW(StgProfHeader)"
          ,constantWord Both "PROF_HDR_SIZE" "sizeofW(StgProfHeader)"

           -- Size of a storage manager block (in bytes).
          ,constantWord Both "BLOCK_SIZE"  "BLOCK_SIZE"
          ,constantWord C    "MBLOCK_SIZE" "MBLOCK_SIZE"
           -- blocks that fit in an MBlock, leaving space for the block
           -- descriptors
          ,constantWord Both "BLOCKS_PER_MBLOCK" "BLOCKS_PER_MBLOCK"
           -- could be derived, but better to save doing the calculation twice

          ,fieldOffset Both "StgRegTable" "rR1"
          ,fieldOffset Both "StgRegTable" "rR2"
          ,fieldOffset Both "StgRegTable" "rR3"
          ,fieldOffset Both "StgRegTable" "rR4"
          ,fieldOffset Both "StgRegTable" "rR5"
          ,fieldOffset Both "StgRegTable" "rR6"
          ,fieldOffset Both "StgRegTable" "rR7"
          ,fieldOffset Both "StgRegTable" "rR8"
          ,fieldOffset Both "StgRegTable" "rR9"
          ,fieldOffset Both "StgRegTable" "rR10"
          ,fieldOffset Both "StgRegTable" "rF1"
          ,fieldOffset Both "StgRegTable" "rF2"
          ,fieldOffset Both "StgRegTable" "rF3"
          ,fieldOffset Both "StgRegTable" "rF4"
          ,fieldOffset Both "StgRegTable" "rF5"
          ,fieldOffset Both "StgRegTable" "rF6"
          ,fieldOffset Both "StgRegTable" "rD1"
          ,fieldOffset Both "StgRegTable" "rD2"
          ,fieldOffset Both "StgRegTable" "rD3"
          ,fieldOffset Both "StgRegTable" "rD4"
          ,fieldOffset Both "StgRegTable" "rD5"
          ,fieldOffset Both "StgRegTable" "rD6"
          ,fieldOffset Both "StgRegTable" "rXMM1"
          ,fieldOffset Both "StgRegTable" "rXMM2"
          ,fieldOffset Both "StgRegTable" "rXMM3"
          ,fieldOffset Both "StgRegTable" "rXMM4"
          ,fieldOffset Both "StgRegTable" "rXMM5"
          ,fieldOffset Both "StgRegTable" "rXMM6"
          ,fieldOffset Both "StgRegTable" "rYMM1"
          ,fieldOffset Both "StgRegTable" "rYMM2"
          ,fieldOffset Both "StgRegTable" "rYMM3"
          ,fieldOffset Both "StgRegTable" "rYMM4"
          ,fieldOffset Both "StgRegTable" "rYMM5"
          ,fieldOffset Both "StgRegTable" "rYMM6"
          ,fieldOffset Both "StgRegTable" "rZMM1"
          ,fieldOffset Both "StgRegTable" "rZMM2"
          ,fieldOffset Both "StgRegTable" "rZMM3"
          ,fieldOffset Both "StgRegTable" "rZMM4"
          ,fieldOffset Both "StgRegTable" "rZMM5"
          ,fieldOffset Both "StgRegTable" "rZMM6"
          ,fieldOffset Both "StgRegTable" "rL1"
          ,fieldOffset Both "StgRegTable" "rSp"
          ,fieldOffset Both "StgRegTable" "rSpLim"
          ,fieldOffset Both "StgRegTable" "rHp"
          ,fieldOffset Both "StgRegTable" "rHpLim"
          ,fieldOffset Both "StgRegTable" "rCCCS"
          ,fieldOffset Both "StgRegTable" "rCurrentTSO"
          ,fieldOffset Both "StgRegTable" "rCurrentNursery"
          ,fieldOffset Both "StgRegTable" "rHpAlloc"
          ,structField C    "StgRegTable" "rRet"
          ,structField C    "StgRegTable" "rNursery"

          ,defIntOffset Both "stgEagerBlackholeInfo"
                             "FUN_OFFSET(stgEagerBlackholeInfo)"
          ,defIntOffset Both "stgGCEnter1" "FUN_OFFSET(stgGCEnter1)"
          ,defIntOffset Both "stgGCFun"    "FUN_OFFSET(stgGCFun)"

          ,fieldOffset Both "Capability" "r"
          ,fieldOffset C    "Capability" "lock"
          ,structField C    "Capability" "no"
          ,structField C    "Capability" "mut_lists"
          ,structField C    "Capability" "context_switch"
          ,structField C    "Capability" "interrupt"
          ,structField C    "Capability" "sparks"

          ,structField Both "bdescr" "start"
          ,structField Both "bdescr" "free"
          ,structField Both "bdescr" "blocks"
          ,structField C    "bdescr" "gen_no"
          ,structField C    "bdescr" "link"

          ,structSize C  "generation"
          ,structField C "generation" "n_new_large_words"
          ,structField C "generation" "weak_ptr_list"

          ,structSize Both   "CostCentreStack"
          ,structField C     "CostCentreStack" "ccsID"
          ,structFieldH Both "CostCentreStack" "mem_alloc"
          ,structFieldH Both "CostCentreStack" "scc_count"
          ,structField C     "CostCentreStack" "prevStack"

          ,structField C "CostCentre" "ccID"
          ,structField C "CostCentre" "link"

          ,structField C     "StgHeader" "info"
          ,structField_ Both "StgHeader_ccs" "StgHeader" "prof.ccs"
          ,structField_ Both "StgHeader_ldvw" "StgHeader" "prof.hp.ldvw"

          ,structSize Both "StgSMPThunkHeader"

          ,closurePayload C "StgClosure" "payload"

          ,structFieldH Both "StgEntCounter" "allocs"
          ,structFieldH Both "StgEntCounter" "allocd"
          ,structField  Both "StgEntCounter" "registeredp"
          ,structField  Both "StgEntCounter" "link"
          ,structField  Both "StgEntCounter" "entry_count"

          ,closureSize Both "StgUpdateFrame"
          ,closureSize C    "StgCatchFrame"
          ,closureSize C    "StgStopFrame"

          ,closureSize  Both "StgMutArrPtrs"
          ,closureField Both "StgMutArrPtrs" "ptrs"
          ,closureField Both "StgMutArrPtrs" "size"

          ,closureSize    Both "StgArrWords"
          ,closureField   Both "StgArrWords" "bytes"
          ,closurePayload C    "StgArrWords" "payload"

          ,closureField  C    "StgTSO"      "_link"
          ,closureField  C    "StgTSO"      "global_link"
          ,closureField  C    "StgTSO"      "what_next"
          ,closureField  C    "StgTSO"      "why_blocked"
          ,closureField  C    "StgTSO"      "block_info"
          ,closureField  C    "StgTSO"      "blocked_exceptions"
          ,closureField  C    "StgTSO"      "id"
          ,closureField  C    "StgTSO"      "cap"
          ,closureField  C    "StgTSO"      "saved_errno"
          ,closureField  C    "StgTSO"      "trec"
          ,closureField  C    "StgTSO"      "flags"
          ,closureField  C    "StgTSO"      "dirty"
          ,closureField  C    "StgTSO"      "bq"
          ,closureField_ Both "StgTSO_cccs" "StgTSO" "prof.cccs"
          ,closureField  Both "StgTSO"      "stackobj"

          ,closureField       Both "StgStack" "sp"
          ,closureFieldOffset Both "StgStack" "stack"
          ,closureField       C    "StgStack" "stack_size"
          ,closureField       C    "StgStack" "dirty"

          ,structSize C "StgTSOProfInfo"

          ,closureField Both "StgUpdateFrame" "updatee"

          ,closureField C "StgCatchFrame" "handler"
          ,closureField C "StgCatchFrame" "exceptions_blocked"

          ,closureSize       C "StgPAP"
          ,closureField      C "StgPAP" "n_args"
          ,closureFieldGcptr C "StgPAP" "fun"
          ,closureField      C "StgPAP" "arity"
          ,closurePayload    C "StgPAP" "payload"

          ,thunkSize         C "StgAP"
          ,closureField      C "StgAP" "n_args"
          ,closureFieldGcptr C "StgAP" "fun"
          ,closurePayload    C "StgAP" "payload"

          ,thunkSize         C "StgAP_STACK"
          ,closureField      C "StgAP_STACK" "size"
          ,closureFieldGcptr C "StgAP_STACK" "fun"
          ,closurePayload    C "StgAP_STACK" "payload"

          ,thunkSize C "StgSelector"

          ,closureFieldGcptr C "StgInd" "indirectee"

          ,closureSize  C "StgMutVar"
          ,closureField C "StgMutVar" "var"

          ,closureSize  C "StgAtomicallyFrame"
          ,closureField C "StgAtomicallyFrame" "code"
          ,closureField C "StgAtomicallyFrame" "next_invariant_to_check"
          ,closureField C "StgAtomicallyFrame" "result"

          ,closureField C "StgInvariantCheckQueue" "invariant"
          ,closureField C "StgInvariantCheckQueue" "my_execution"
          ,closureField C "StgInvariantCheckQueue" "next_queue_entry"

          ,closureField C "StgAtomicInvariant" "code"

          ,closureField C "StgTRecHeader" "enclosing_trec"

          ,closureSize  C "StgCatchSTMFrame"
          ,closureField C "StgCatchSTMFrame" "handler"
          ,closureField C "StgCatchSTMFrame" "code"

          ,closureSize  C "StgCatchRetryFrame"
          ,closureField C "StgCatchRetryFrame" "running_alt_code"
          ,closureField C "StgCatchRetryFrame" "first_code"
          ,closureField C "StgCatchRetryFrame" "alt_code"

          ,closureField C "StgTVarWatchQueue" "closure"
          ,closureField C "StgTVarWatchQueue" "next_queue_entry"
          ,closureField C "StgTVarWatchQueue" "prev_queue_entry"

          ,closureSize  C "StgTVar"
          ,closureField C "StgTVar" "current_value"
          ,closureField C "StgTVar" "first_watch_queue_entry"
          ,closureField C "StgTVar" "num_updates"

          ,closureSize  C "StgWeak"
          ,closureField C "StgWeak" "link"
          ,closureField C "StgWeak" "key"
          ,closureField C "StgWeak" "value"
          ,closureField C "StgWeak" "finalizer"
          ,closureField C "StgWeak" "cfinalizers"

          ,closureSize  C "StgCFinalizerList"
          ,closureField C "StgCFinalizerList" "link"
          ,closureField C "StgCFinalizerList" "fptr"
          ,closureField C "StgCFinalizerList" "ptr"
          ,closureField C "StgCFinalizerList" "eptr"
          ,closureField C "StgCFinalizerList" "flag"

          ,closureSize  C "StgMVar"
          ,closureField C "StgMVar" "head"
          ,closureField C "StgMVar" "tail"
          ,closureField C "StgMVar" "value"

          ,closureSize  C "StgMVarTSOQueue"
          ,closureField C "StgMVarTSOQueue" "link"
          ,closureField C "StgMVarTSOQueue" "tso"

          ,closureSize    C "StgBCO"
          ,closureField   C "StgBCO" "instrs"
          ,closureField   C "StgBCO" "literals"
          ,closureField   C "StgBCO" "ptrs"
          ,closureField   C "StgBCO" "arity"
          ,closureField   C "StgBCO" "size"
          ,closurePayload C "StgBCO" "bitmap"

          ,closureSize  C "StgStableName"
          ,closureField C "StgStableName" "sn"

          ,closureSize  C "StgBlockingQueue"
          ,closureField C "StgBlockingQueue" "bh"
          ,closureField C "StgBlockingQueue" "owner"
          ,closureField C "StgBlockingQueue" "queue"
          ,closureField C "StgBlockingQueue" "link"

          ,closureSize  C "MessageBlackHole"
          ,closureField C "MessageBlackHole" "link"
          ,closureField C "MessageBlackHole" "tso"
          ,closureField C "MessageBlackHole" "bh"

          ,structField_ C "RtsFlags_ProfFlags_showCCSOnException"
                          "RTS_FLAGS" "ProfFlags.showCCSOnException"
          ,structField_ C "RtsFlags_DebugFlags_apply"
                          "RTS_FLAGS" "DebugFlags.apply"
          ,structField_ C "RtsFlags_DebugFlags_sanity"
                          "RTS_FLAGS" "DebugFlags.sanity"
          ,structField_ C "RtsFlags_DebugFlags_weak"
                          "RTS_FLAGS" "DebugFlags.weak"
          ,structField_ C "RtsFlags_GcFlags_initialStkSize"
                          "RTS_FLAGS" "GcFlags.initialStkSize"
          ,structField_ C "RtsFlags_MiscFlags_tickInterval"
                          "RTS_FLAGS" "MiscFlags.tickInterval"

          ,structSize   C "StgFunInfoExtraFwd"
          ,structField  C "StgFunInfoExtraFwd" "slow_apply"
          ,structField  C "StgFunInfoExtraFwd" "fun_type"
          ,structFieldH Both "StgFunInfoExtraFwd" "arity"
          ,structField_ C "StgFunInfoExtraFwd_bitmap" "StgFunInfoExtraFwd" "b.bitmap"

          ,structSize   Both "StgFunInfoExtraRev"
          ,structField  C    "StgFunInfoExtraRev" "slow_apply_offset"
          ,structField  C    "StgFunInfoExtraRev" "fun_type"
          ,structFieldH Both "StgFunInfoExtraRev" "arity"
          ,structField_ C    "StgFunInfoExtraRev_bitmap" "StgFunInfoExtraRev" "b.bitmap"

          ,structField C "StgLargeBitmap" "size"
          ,fieldOffset C "StgLargeBitmap" "bitmap"

          ,structSize  C "snEntry"
          ,structField C "snEntry" "sn_obj"
          ,structField C "snEntry" "addr"

          ,structSize  C "spEntry"
          ,structField C "spEntry" "addr"

           -- Note that this conditional part only affects the C headers.
           -- That's important, as it means we get the same PlatformConstants
           -- type on all platforms.
          ,if os == "mingw32"
           then concat [structSize  C "StgAsyncIOResult"
                       ,structField C "StgAsyncIOResult" "reqID"
                       ,structField C "StgAsyncIOResult" "len"
                       ,structField C "StgAsyncIOResult" "errCode"]
           else []

          -- pre-compiled thunk types
          ,constantWord Haskell "MAX_SPEC_SELECTEE_SIZE" "MAX_SPEC_SELECTEE_SIZE"
          ,constantWord Haskell "MAX_SPEC_AP_SIZE"       "MAX_SPEC_AP_SIZE"

          -- closure sizes: these do NOT include the header (see below for
          -- header sizes)
          ,constantWord Haskell "MIN_PAYLOAD_SIZE" "MIN_PAYLOAD_SIZE"

          ,constantInt  Haskell "MIN_INTLIKE" "MIN_INTLIKE"
          ,constantWord Haskell "MAX_INTLIKE" "MAX_INTLIKE"

          ,constantWord Haskell "MIN_CHARLIKE" "MIN_CHARLIKE"
          ,constantWord Haskell "MAX_CHARLIKE" "MAX_CHARLIKE"

          ,constantWord Haskell "MUT_ARR_PTRS_CARD_BITS" "MUT_ARR_PTRS_CARD_BITS"

          -- A section of code-generator-related MAGIC CONSTANTS.
          ,constantWord Haskell "MAX_Vanilla_REG"      "MAX_VANILLA_REG"
          ,constantWord Haskell "MAX_Float_REG"        "MAX_FLOAT_REG"
          ,constantWord Haskell "MAX_Double_REG"       "MAX_DOUBLE_REG"
          ,constantWord Haskell "MAX_Long_REG"         "MAX_LONG_REG"
          ,constantWord Haskell "MAX_XMM_REG"          "MAX_XMM_REG"
          ,constantWord Haskell "MAX_Real_Vanilla_REG" "MAX_REAL_VANILLA_REG"
          ,constantWord Haskell "MAX_Real_Float_REG"   "MAX_REAL_FLOAT_REG"
          ,constantWord Haskell "MAX_Real_Double_REG"  "MAX_REAL_DOUBLE_REG"
          ,constantWord Haskell "MAX_Real_XMM_REG"     "MAX_REAL_XMM_REG"
          ,constantWord Haskell "MAX_Real_Long_REG"    "MAX_REAL_LONG_REG"

          -- This tells the native code generator the size of the spill
          -- area is has available.
          ,constantWord Haskell "RESERVED_C_STACK_BYTES" "RESERVED_C_STACK_BYTES"
          -- The amount of (Haskell) stack to leave free for saving
          -- registers when returning to the scheduler.
          ,constantWord Haskell "RESERVED_STACK_WORDS" "RESERVED_STACK_WORDS"
          -- Continuations that need more than this amount of stack
          -- should do their own stack check (see bug #1466).
          ,constantWord Haskell "AP_STACK_SPLIM" "AP_STACK_SPLIM"

          -- Size of a word, in bytes
          ,constantWord Haskell "WORD_SIZE" "SIZEOF_HSWORD"

          -- Size of a double in StgWords.
          ,constantWord Haskell "DOUBLE_SIZE" "SIZEOF_DOUBLE"

          -- Size of a C int, in bytes. May be smaller than wORD_SIZE.
          ,constantWord Haskell "CINT_SIZE"       "SIZEOF_INT"
          ,constantWord Haskell "CLONG_SIZE"      "SIZEOF_LONG"
          ,constantWord Haskell "CLONG_LONG_SIZE" "SIZEOF_LONG_LONG"

          -- Number of bits to shift a bitfield left by in an info table.
          ,constantWord Haskell "BITMAP_BITS_SHIFT" "BITMAP_BITS_SHIFT"

          -- Amount of pointer bits used for semi-tagging constructor closures
          ,constantWord Haskell "TAG_BITS" "TAG_BITS"

          ,constantBool Haskell "WORDS_BIGENDIAN"    "defined(WORDS_BIGENDIAN)"
          ,constantBool Haskell "DYNAMIC_BY_DEFAULT" "defined(DYNAMIC_BY_DEFAULT)"

          ,constantWord    Haskell "LDV_SHIFT"         "LDV_SHIFT"
          ,constantNatural Haskell "ILDV_CREATE_MASK"  "LDV_CREATE_MASK"
          ,constantNatural Haskell "ILDV_STATE_CREATE" "LDV_STATE_CREATE"
          ,constantNatural Haskell "ILDV_STATE_USE"    "LDV_STATE_USE"
          ]

getWanted :: Bool -> FilePath -> FilePath -> [String] -> FilePath -> IO Results
getWanted verbose tmpdir gccProgram gccFlags nmProgram
    = do let cStuff = unlines (headers ++ concatMap (doWanted . snd) wanteds)
             cFile = tmpdir </> "tmp.c"
             oFile = tmpdir </> "tmp.o"
         writeFile cFile cStuff
         execute verbose gccProgram (gccFlags ++ ["-c", cFile, "-o", oFile])
         xs <- readProcess nmProgram ["-P", oFile] ""
         let ls = lines xs
             ms = map parseNmLine ls
             m = Map.fromList $ catMaybes ms
         rs <- mapM (lookupResult m) wanteds
         return rs
    where headers = ["#define IN_STG_CODE 0",
                     "",
                     "/*",
                     " * We need offsets of profiled things...",
                     " * better be careful that this doesn't",
                     " * affect the offsets of anything else.",
                     " */",
                     "",
                     "#define PROFILING",
                     "#define THREADED_RTS",
                     "",
                     "#include \"PosixSource.h\"",
                     "#include \"Rts.h\"",
                     "#include \"Stable.h\"",
                     "#include \"Capability.h\"",
                     "",
                     "#include <inttypes.h>",
                     "#include <stddef.h>",
                     "#include <stdio.h>",
                     "#include <string.h>",
                     "",
                     "#define FIELD_SIZE(s_type, field) ((size_t)sizeof(((s_type*)0)->field))",
                     "#define TYPE_SIZE(type) (sizeof(type))",
                     "#define FUN_OFFSET(sym) (offsetof(Capability,f.sym) - offsetof(Capability,r))",
                     "",
                     "#pragma GCC poison sizeof"
                     ]

          prefix = "derivedConstant"
          mkFullName name = prefix ++ name

          -- We add 1 to the value, as some platforms will make a symbol
          -- of size 1 when for
          --     char foo[0];
          -- We then subtract 1 again when parsing.
          doWanted (GetFieldType name (Fst (CExpr cExpr)))
              = ["char " ++ mkFullName name ++ "[1 + " ++ cExpr ++ "];"]
          doWanted (GetClosureSize name (Fst (CExpr cExpr)))
              = ["char " ++ mkFullName name ++ "[1 + " ++ cExpr ++ "];"]
          doWanted (GetWord name (Fst (CExpr cExpr)))
              = ["char " ++ mkFullName name ++ "[1 + " ++ cExpr ++ "];"]
          doWanted (GetInt name (Fst (CExpr cExpr)))
              = ["char " ++ mkFullName name ++ "Mag[1 + ((intptr_t)(" ++ cExpr ++ ") >= 0 ? (" ++ cExpr ++ ") : -(" ++ cExpr ++ "))];",
                 "char " ++ mkFullName name ++ "Sig[(intptr_t)(" ++ cExpr ++ ") >= 0 ? 3 : 1];"]
          doWanted (GetNatural name (Fst (CExpr cExpr)))
              = -- These casts fix "right shift count >= width of type"
                -- warnings
                let cExpr' = "(uint64_t)(size_t)(" ++ cExpr ++ ")"
                in ["char " ++ mkFullName name ++ "0[1 + ((" ++ cExpr' ++ ") & 0xFFFF)];",
                    "char " ++ mkFullName name ++ "1[1 + (((" ++ cExpr' ++ ") >> 16) & 0xFFFF)];",
                    "char " ++ mkFullName name ++ "2[1 + (((" ++ cExpr' ++ ") >> 32) & 0xFFFF)];",
                    "char " ++ mkFullName name ++ "3[1 + (((" ++ cExpr' ++ ") >> 48) & 0xFFFF)];"]
          doWanted (GetBool name (Fst (CPPExpr cppExpr)))
              = ["#if " ++ cppExpr,
                 "char " ++ mkFullName name ++ "[1];",
                 "#else",
                 "char " ++ mkFullName name ++ "[2];",
                 "#endif"]
          doWanted (StructFieldMacro {}) = []
          doWanted (ClosureFieldMacro {}) = []
          doWanted (ClosurePayloadMacro {}) = []
          doWanted (FieldTypeGcptrMacro {}) = []

          -- parseNmLine parses "nm -P" output that looks like
          -- "_derivedConstantMAX_Vanilla_REG C b 0" Mac OS X
          -- "derivedConstantMAX_Vanilla_REG C 0000000b 0000000b" GNU
          -- "derivedConstantMAX_Vanilla_REG D        1        b" Solaris
          -- and returns ("MAX_Vanilla_REG", 11)
          parseNmLine xs0 = case words xs0 of
                            [x0, x1, x2, x3] -> case stripPrefix prefix $ dropWhile (== '_') x0 of
                              Just name -> case readHex $ if x1 == "C" then x2 else x3 of
                                [(size, "")] -> Just (name, size)
                                _ -> Nothing
                              _ -> Nothing
                            _ -> Nothing

          -- If an Int value is larger than 2^28 or smaller
          -- than -2^28, then fail.
          -- This test is a bit conservative, but if any
          -- constants are roughly maxBound or minBound then
          -- we probably need them to be Integer rather than
          -- Int so that -- cross-compiling between 32bit and
          -- 64bit platforms works.
          lookupSmall :: Map String Integer -> Name -> IO Integer
          lookupSmall m name
              = case Map.lookup name m of
                Just v
                 | v >   2^(28 :: Int) ||
                   v < -(2^(28 :: Int)) ->
                    die ("Value too large for GetWord: " ++ show v)
                 | otherwise -> return v
                Nothing -> die ("Can't find " ++ show name)

          lookupResult :: Map String Integer -> (Where, What Fst)
                       -> IO (Where, What Snd)
          lookupResult m (w, GetWord name _)
              = do v <- lookupSmall m name
                   return (w, GetWord name (Snd (v - 1)))
          lookupResult m (w, GetInt name _)
              = do mag <- lookupSmall m (name ++ "Mag")
                   sig <- lookupSmall m (name ++ "Sig")
                   return (w, GetWord name (Snd ((mag - 1) * (sig - 2))))
          lookupResult m (w, GetNatural name _)
              = do v0 <- lookupSmall m (name ++ "0")
                   v1 <- lookupSmall m (name ++ "1")
                   v2 <- lookupSmall m (name ++ "2")
                   v3 <- lookupSmall m (name ++ "3")
                   let v = (v0 - 1)
                         + shiftL (v1 - 1) 16
                         + shiftL (v2 - 1) 32
                         + shiftL (v3 - 1) 48
                   return (w, GetWord name (Snd v))
          lookupResult m (w, GetBool name _)
              = do v <- lookupSmall m name
                   case v of
                       1 -> return (w, GetBool name (Snd True))
                       2 -> return (w, GetBool name (Snd False))
                       _ -> die ("Bad boolean: " ++ show v)
          lookupResult m (w, GetFieldType name _)
              = do v <- lookupSmall m name
                   return (w, GetFieldType name (Snd (v - 1)))
          lookupResult m (w, GetClosureSize name _)
              = do v <- lookupSmall m name
                   return (w, GetClosureSize name (Snd (v - 1)))
          lookupResult _ (w, StructFieldMacro name)
              = return (w, StructFieldMacro name)
          lookupResult _ (w, ClosureFieldMacro name)
              = return (w, ClosureFieldMacro name)
          lookupResult _ (w, ClosurePayloadMacro name)
              = return (w, ClosurePayloadMacro name)
          lookupResult _ (w, FieldTypeGcptrMacro name)
              = return (w, FieldTypeGcptrMacro name)

writeHaskellType :: FilePath -> [What Fst] -> IO ()
writeHaskellType fn ws = writeFile fn xs
    where xs = unlines (headers ++ body ++ footers)
          headers = ["data PlatformConstants = PlatformConstants {"
                     -- Now a kludge that allows the real entries to
                     -- all start with a comma, which makes life a
                     -- little easier
                    ,"    pc_platformConstants :: ()"]
          footers = ["  } deriving Read"]
          body = concatMap doWhat ws
          doWhat (GetClosureSize name _) = ["    , pc_" ++ name ++ " :: Int"]
          doWhat (GetFieldType   name _) = ["    , pc_" ++ name ++ " :: Int"]
          doWhat (GetWord        name _) = ["    , pc_" ++ name ++ " :: Int"]
          doWhat (GetInt         name _) = ["    , pc_" ++ name ++ " :: Int"]
          doWhat (GetNatural     name _) = ["    , pc_" ++ name ++ " :: Integer"]
          doWhat (GetBool        name _) = ["    , pc_" ++ name ++ " :: Bool"]
          doWhat (StructFieldMacro {}) = []
          doWhat (ClosureFieldMacro {}) = []
          doWhat (ClosurePayloadMacro {}) = []
          doWhat (FieldTypeGcptrMacro {}) = []

writeHaskellValue :: FilePath -> [What Snd] -> IO ()
writeHaskellValue fn rs = writeFile fn xs
    where xs = unlines (headers ++ body ++ footers)
          headers = ["PlatformConstants {"
                    ,"    pc_platformConstants = ()"]
          footers = ["  }"]
          body = concatMap doWhat rs
          doWhat (GetClosureSize name (Snd v)) = ["    , pc_" ++ name ++ " = " ++ show v]
          doWhat (GetFieldType   name (Snd v)) = ["    , pc_" ++ name ++ " = " ++ show v]
          doWhat (GetWord        name (Snd v)) = ["    , pc_" ++ name ++ " = " ++ show v]
          doWhat (GetInt         name (Snd v)) = ["    , pc_" ++ name ++ " = " ++ show v]
          doWhat (GetNatural     name (Snd v)) = ["    , pc_" ++ name ++ " = " ++ show v]
          doWhat (GetBool        name (Snd v)) = ["    , pc_" ++ name ++ " = " ++ show v]
          doWhat (StructFieldMacro {}) = []
          doWhat (ClosureFieldMacro {}) = []
          doWhat (ClosurePayloadMacro {}) = []
          doWhat (FieldTypeGcptrMacro {}) = []

writeHaskellWrappers :: FilePath -> [What Fst] -> IO ()
writeHaskellWrappers fn ws = writeFile fn xs
    where xs = unlines body
          body = concatMap doWhat ws
          doWhat (GetFieldType {}) = []
          doWhat (GetClosureSize {}) = []
          doWhat (GetWord name _) = [haskellise name ++ " :: DynFlags -> Int",
                                    haskellise name ++ " dflags = pc_" ++ name ++ " (sPlatformConstants (settings dflags))"]
          doWhat (GetInt name _) = [haskellise name ++ " :: DynFlags -> Int",
                                   haskellise name ++ " dflags = pc_" ++ name ++ " (sPlatformConstants (settings dflags))"]
          doWhat (GetNatural name _) = [haskellise name ++ " :: DynFlags -> Integer",
                                        haskellise name ++ " dflags = pc_" ++ name ++ " (sPlatformConstants (settings dflags))"]
          doWhat (GetBool name _) = [haskellise name ++ " :: DynFlags -> Bool",
                                     haskellise name ++ " dflags = pc_" ++ name ++ " (sPlatformConstants (settings dflags))"]
          doWhat (StructFieldMacro {}) = []
          doWhat (ClosureFieldMacro {}) = []
          doWhat (ClosurePayloadMacro {}) = []
          doWhat (FieldTypeGcptrMacro {}) = []

writeHaskellExports :: FilePath -> [What Fst] -> IO ()
writeHaskellExports fn ws = writeFile fn xs
    where xs = unlines body
          body = concatMap doWhat ws
          doWhat (GetFieldType {}) = []
          doWhat (GetClosureSize {}) = []
          doWhat (GetWord    name _) = ["    " ++ haskellise name ++ ","]
          doWhat (GetInt     name _) = ["    " ++ haskellise name ++ ","]
          doWhat (GetNatural name _) = ["    " ++ haskellise name ++ ","]
          doWhat (GetBool    name _) = ["    " ++ haskellise name ++ ","]
          doWhat (StructFieldMacro {}) = []
          doWhat (ClosureFieldMacro {}) = []
          doWhat (ClosurePayloadMacro {}) = []
          doWhat (FieldTypeGcptrMacro {}) = []

writeHeader :: FilePath -> [What Snd] -> IO ()
writeHeader fn rs = writeFile fn xs
    where xs = unlines (headers ++ body)
          headers = ["/* This file is created automatically.  Do not edit by hand.*/", ""]
          body = concatMap doWhat rs
          doWhat (GetFieldType   name (Snd v)) = ["#define " ++ name ++ " b" ++ show (v * 8)]
          doWhat (GetClosureSize name (Snd v)) = ["#define " ++ name ++ " (SIZEOF_StgHeader+" ++ show v ++ ")"]
          doWhat (GetWord        name (Snd v)) = ["#define " ++ name ++ " " ++ show v]
          doWhat (GetInt         name (Snd v)) = ["#define " ++ name ++ " " ++ show v]
          doWhat (GetNatural     name (Snd v)) = ["#define " ++ name ++ " " ++ show v]
          doWhat (GetBool        name (Snd v)) = ["#define " ++ name ++ " " ++ show (fromEnum v)]
          doWhat (StructFieldMacro nameBase) =
                     ["#define " ++ nameBase ++ "(__ptr__) REP_" ++ nameBase ++ "[__ptr__+OFFSET_" ++ nameBase ++ "]"]
          doWhat (ClosureFieldMacro nameBase) =
                     ["#define " ++ nameBase ++ "(__ptr__) REP_" ++ nameBase ++ "[__ptr__+SIZEOF_StgHeader+OFFSET_" ++ nameBase ++ "]"]
          doWhat (ClosurePayloadMacro nameBase) =
                     ["#define " ++ nameBase ++ "(__ptr__,__ix__) W_[__ptr__+SIZEOF_StgHeader+OFFSET_" ++ nameBase ++ " + WDS(__ix__)]"]
          doWhat (FieldTypeGcptrMacro nameBase) =
                     ["#define REP_" ++ nameBase ++ " gcptr"]

die :: String -> IO a
die err = do hPutStrLn stderr err
             exitFailure

execute :: Bool -> FilePath -> [String] -> IO ()
execute verbose prog args
 = do when verbose $ putStrLn $ showCommandForUser prog args
      ec <- rawSystem prog args
      unless (ec == ExitSuccess) $
          die ("Executing " ++ show prog ++ " failed")

