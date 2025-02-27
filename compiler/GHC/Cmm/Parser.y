-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2004-2012
--
-- Parser for concrete Cmm.
--
-----------------------------------------------------------------------------

{-
Note [Syntax of .cmm files]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
NOTE: You are very much on your own in .cmm.  There is very little
error checking at all:

  * Type errors are detected by the (optional) -dcmm-lint pass, if you
    don't turn this on then a type error will likely result in a panic
    from the native code generator.

  * Passing the wrong number of arguments or arguments of the wrong
    type is usually not detected.

There are two ways to write .cmm code:

 (1) High-level Cmm code delegates the stack handling to GHC, and
     never explicitly mentions Sp or registers.

 (2) Low-level Cmm manages the stack itself, and must know about
     calling conventions.

Whether you want high-level or low-level Cmm is indicated by the
presence of an argument list on a procedure.  For example:

foo ( gcptr a, bits32 b )
{
  // this is high-level cmm code

  if (b > 0) {
     // we can make tail calls passing arguments:
     jump stg_ap_0_fast(a);
  }

  push (stg_upd_frame_info, a) {
    // stack frames can be explicitly pushed

    (x,y) = call wibble(a,b,3,4);
      // calls pass arguments and return results using the native
      // Haskell calling convention.  The code generator will automatically
      // construct a stack frame and an info table for the continuation.

    return (x,y);
      // we can return multiple values from the current proc
  }
}

bar
{
  // this is low-level cmm code, indicated by the fact that we did not
  // put an argument list on bar.

  x = R1;  // the calling convention is explicit: better be careful
           // that this works on all platforms!

  jump %ENTRY_CODE(Sp(0))
}

Here is a list of rules for high-level and low-level code.  If you
break the rules, you get a panic (for using a high-level construct in
a low-level proc), or wrong code (when using low-level code in a
high-level proc).  This stuff isn't checked! (TODO!)

High-level only:

  - tail-calls with arguments, e.g.
    jump stg_fun (arg1, arg2);

  - function calls:
    (ret1,ret2) = call stg_fun (arg1, arg2);

    This makes a call with the NativeNodeCall convention, and the
    values are returned to the following code using the NativeReturn
    convention.

  - returning:
    return (ret1, ret2)

    These use the NativeReturn convention to return zero or more
    results to the caller.

  - pushing stack frames:
    push (info_ptr, field1, ..., fieldN) { ... statements ... }

  - reserving temporary stack space:

      reserve N = x { ... }

    this reserves an area of size N (words) on the top of the stack,
    and binds its address to x (a local register).  Typically this is
    used for allocating temporary storage for passing to foreign
    functions.

    Note that if you make any native calls or invoke the GC in the
    scope of the reserve block, you are responsible for ensuring that
    the stack you reserved is laid out correctly with an info table.

Low-level only:

  - References to Sp, R1-R8, F1-F4 etc.

    NB. foreign calls may clobber the argument registers R1-R8, F1-F4
    etc., so ensure they are saved into variables around foreign
    calls.

  - SAVE_THREAD_STATE() and LOAD_THREAD_STATE(), which modify Sp
    directly.

Both high-level and low-level code can use a raw tail-call:

    jump stg_fun [R1,R2]

NB. you *must* specify the list of GlobalRegs that are passed via a
jump, otherwise the register allocator will assume that all the
GlobalRegs are dead at the jump.


Calling Conventions
-------------------

High-level procedures use the NativeNode calling convention, or the
NativeReturn convention if the 'return' keyword is used (see Stack
Frames below).

Low-level procedures implement their own calling convention, so it can
be anything at all.

If a low-level procedure implements the NativeNode calling convention,
then it can be called by high-level code using an ordinary function
call.  In general this is hard to arrange because the calling
convention depends on the number of physical registers available for
parameter passing, but there are two cases where the calling
convention is platform-independent:

 - Zero arguments.

 - One argument of pointer or non-pointer word type; this is always
   passed in R1 according to the NativeNode convention.

 - Returning a single value; these conventions are fixed and platform
   independent.


Stack Frames
------------

A stack frame is written like this:

INFO_TABLE_RET ( label, FRAME_TYPE, info_ptr, field1, ..., fieldN )
               return ( arg1, ..., argM )
{
  ... code ...
}

where field1 ... fieldN are the fields of the stack frame (with types)
arg1...argN are the values returned to the stack frame (with types).
The return values are assumed to be passed according to the
NativeReturn convention.

On entry to the code, the stack frame looks like:

   |----------|
   | fieldN   |
   |   ...    |
   | field1   |
   |----------|
   | info_ptr |
   |----------|
   |  argN    |
   |   ...    | <- Sp

and some of the args may be in registers.

We prepend the code by a copyIn of the args, and assign all the stack
frame fields to their formals.  The initial "arg offset" for stack
layout purposes consists of the whole stack frame plus any args that
might be on the stack.

A tail-call may pass a stack frame to the callee using the following
syntax:

jump f (info_ptr, field1,..,fieldN) (arg1,..,argN)

where info_ptr and field1..fieldN describe the stack frame, and
arg1..argN are the arguments passed to f using the NativeNodeCall
convention. Note if a field is longer than a word (e.g. a D_ on
a 32-bit machine) then the call will push as many words as
necessary to the stack to accommodate it (e.g. 2).

Memory ordering
---------------

Cmm respects the C11 memory model and distinguishes between non-atomic and
atomic memory accesses. In C11 fashion, atomic accesses can provide a variety of
memory ordering guarantees. These supported as statements in Cmm syntax as
follows:

    W_[ptr] = ...;            // a non-atomic store
    %relaxed W_[ptr] = ...;   // an atomic store with relaxed ordering semantics
    %release W_[ptr] = ...;   // an atomic store with release ordering semantics

    x = W_[ptr];              // a non-atomic load
    x = %relaxed W_[ptr];     // an atomic load with relaxed ordering
    x = %acquire W_[ptr];     // an atomic load with acquire ordering
    // or equivalently...
    x = prim %load_acquire64(ptr);

Here we used W_ as an example but these operations can be used on all Cmm
types.

Sometimes it is also necessary to perform atomic but non-ordered loads in an
expression context. For this we provide the MO_RelaxedRead MachOp, expressed in
Cmm syntax as

    x = W_![ptr];

This operation and syntax was primarily added to support hand-written Cmm,
where sometimes such atomic loads are unavoidable deep inside expressions (e.g.
see the CHECK_GC macro). Since one should be explicit about program order when
writing operations with ordered semantics, we do not provide similar MachOps
for acquire and release reads.

See Note [Heap memory barriers] in SMP.h for details.

----------------------------------------------------------------------------- -}

{
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module GHC.Cmm.Parser ( parseCmmFile, CmmParserConfig(..) ) where

import GHC.Prelude
import qualified Prelude -- for happy-generated code

import GHC.Platform
import GHC.Platform.Profile

import GHC.StgToCmm.ExtCode
import GHC.StgToCmm.Heap
import GHC.StgToCmm.Monad hiding ( getCode, getCodeR, getCodeScoped, emitLabel, emit
                                 , emitStore, emitAssign, emitOutOfLine, withUpdFrameOff
                                 , getUpdFrameOff, getProfile, getPlatform, getContext)
import qualified GHC.StgToCmm.Monad as F
import GHC.StgToCmm.Utils
import GHC.StgToCmm.Foreign
import GHC.StgToCmm.Expr
import GHC.StgToCmm.Lit
import GHC.StgToCmm.Closure
import GHC.StgToCmm.Config
import GHC.StgToCmm.Layout     hiding (ArgRep(..))
import GHC.StgToCmm.Ticky
import GHC.StgToCmm.Prof
import GHC.StgToCmm.Bind  ( emitBlackHoleCode, emitUpdateFrame )
import GHC.StgToCmm.InfoTableProv

import GHC.Cmm.Opt
import GHC.Cmm.Graph
import GHC.Cmm
import GHC.Cmm.Reg        ( GlobalArgRegs(..) )
import GHC.Cmm.Utils
import GHC.Cmm.Switch     ( mkSwitchTargets )
import GHC.Cmm.Info
import GHC.Cmm.BlockId
import GHC.Cmm.Lexer
import GHC.Cmm.CLabel
import GHC.Cmm.Parser.Config
import GHC.Cmm.Parser.Monad hiding (getPlatform, getProfile)
import qualified GHC.Cmm.Parser.Monad as PD
import GHC.Cmm.CallConv
import GHC.Runtime.Heap.Layout
import GHC.Parser.Lexer
import GHC.Parser.Errors.Types
import GHC.Parser.Errors.Ppr

import GHC.Types.Unique.DSM
import GHC.Types.CostCentre
import GHC.Types.ForeignCall
import GHC.Unit.Module
import GHC.Unit.Home
import GHC.Types.Literal
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.SrcLoc
import GHC.Types.Tickish  ( GenTickish(SourceNote) )
import GHC.Utils.Error
import GHC.Data.StringBuffer
import GHC.Data.FastString
import GHC.Utils.Panic
import GHC.Settings.Constants
import GHC.Utils.Outputable
import GHC.Types.Basic
import GHC.Data.Bag     ( Bag, emptyBag, unitBag, isEmptyBag )
import GHC.Types.Var

import Control.Monad
import Data.Array
import Data.Char        ( ord )
import System.Exit
import Data.Maybe
import Data.Type.Equality
import Data.Type.Ord
import GHC.TypeNats
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS8
}

%expect 0

%token
        ':'     { L _ (CmmT_SpecChar ':') }
        ';'     { L _ (CmmT_SpecChar ';') }
        '{'     { L _ (CmmT_SpecChar '{') }
        '}'     { L _ (CmmT_SpecChar '}') }
        '['     { L _ (CmmT_SpecChar '[') }
        ']'     { L _ (CmmT_SpecChar ']') }
        '('     { L _ (CmmT_SpecChar '(') }
        ')'     { L _ (CmmT_SpecChar ')') }
        '='     { L _ (CmmT_SpecChar '=') }
        '`'     { L _ (CmmT_SpecChar '`') }
        '~'     { L _ (CmmT_SpecChar '~') }
        '/'     { L _ (CmmT_SpecChar '/') }
        '*'     { L _ (CmmT_SpecChar '*') }
        '%'     { L _ (CmmT_SpecChar '%') }
        '-'     { L _ (CmmT_SpecChar '-') }
        '+'     { L _ (CmmT_SpecChar '+') }
        '&'     { L _ (CmmT_SpecChar '&') }
        '^'     { L _ (CmmT_SpecChar '^') }
        '|'     { L _ (CmmT_SpecChar '|') }
        '>'     { L _ (CmmT_SpecChar '>') }
        '<'     { L _ (CmmT_SpecChar '<') }
        ','     { L _ (CmmT_SpecChar ',') }
        '!'     { L _ (CmmT_SpecChar '!') }

        '..'    { L _ (CmmT_DotDot) }
        '::'    { L _ (CmmT_DoubleColon) }
        '>>'    { L _ (CmmT_Shr) }
        '<<'    { L _ (CmmT_Shl) }
        '>='    { L _ (CmmT_Ge) }
        '<='    { L _ (CmmT_Le) }
        '=='    { L _ (CmmT_Eq) }
        '!='    { L _ (CmmT_Ne) }
        '&&'    { L _ (CmmT_BoolAnd) }
        '||'    { L _ (CmmT_BoolOr) }

        'True'  { L _ (CmmT_True ) }
        'False' { L _ (CmmT_False) }
        'likely'{ L _ (CmmT_likely)}
        'relaxed'{ L _ (CmmT_Relaxed)}
        'acquire'{ L _ (CmmT_Acquire)}
        'release'{ L _ (CmmT_Release)}
        'seq_cst'{ L _ (CmmT_SeqCst)}

        'CLOSURE'       { L _ (CmmT_CLOSURE) }
        'INFO_TABLE'    { L _ (CmmT_INFO_TABLE) }
        'INFO_TABLE_RET'{ L _ (CmmT_INFO_TABLE_RET) }
        'INFO_TABLE_FUN'{ L _ (CmmT_INFO_TABLE_FUN) }
        'INFO_TABLE_CONSTR'{ L _ (CmmT_INFO_TABLE_CONSTR) }
        'INFO_TABLE_SELECTOR'{ L _ (CmmT_INFO_TABLE_SELECTOR) }
        'else'          { L _ (CmmT_else) }
        'export'        { L _ (CmmT_export) }
        'section'       { L _ (CmmT_section) }
        'goto'          { L _ (CmmT_goto) }
        'if'            { L _ (CmmT_if) }
        'call'          { L _ (CmmT_call) }
        'jump'          { L _ (CmmT_jump) }
        'foreign'       { L _ (CmmT_foreign) }
        'never'         { L _ (CmmT_never) }
        'prim'          { L _ (CmmT_prim) }
        'reserve'       { L _ (CmmT_reserve) }
        'return'        { L _ (CmmT_return) }
        'returns'       { L _ (CmmT_returns) }
        'import'        { L _ (CmmT_import) }
        'switch'        { L _ (CmmT_switch) }
        'case'          { L _ (CmmT_case) }
        'default'       { L _ (CmmT_default) }
        'push'          { L _ (CmmT_push) }
        'unwind'        { L _ (CmmT_unwind) }
        'bits8'         { L _ (CmmT_bits8) }
        'bits16'        { L _ (CmmT_bits16) }
        'bits32'        { L _ (CmmT_bits32) }
        'bits64'        { L _ (CmmT_bits64) }
        'vec128'        { L _ (CmmT_vec128) }
        'vec256'        { L _ (CmmT_vec256) }
        'vec512'        { L _ (CmmT_vec512) }
        'float32'       { L _ (CmmT_float32) }
        'float64'       { L _ (CmmT_float64) }
        'gcptr'         { L _ (CmmT_gcptr) }

        GLOBALREG       { L _ (CmmT_GlobalReg $$) }
        NAME            { L _ (CmmT_Name      $$) }
        STRING          { L _ (CmmT_String    $$) }
        INT             { L _ (CmmT_Int       $$) }
        FLOAT           { L _ (CmmT_Float     $$) }

        GP_ARG_REGS     { L _ (CmmT_GlobalArgRegs GP_ARG_REGS) }
        SCALAR_ARG_REGS { L _ (CmmT_GlobalArgRegs SCALAR_ARG_REGS) }
        V16_ARG_REGS    { L _ (CmmT_GlobalArgRegs V16_ARG_REGS) }
        V32_ARG_REGS    { L _ (CmmT_GlobalArgRegs V32_ARG_REGS) }
        V64_ARG_REGS    { L _ (CmmT_GlobalArgRegs V64_ARG_REGS) }

%monad { PD } { >>= } { return }
%lexer { cmmlex } { L _ CmmT_EOF }
%name cmmParse cmm
%tokentype { Located CmmToken }

-- C-- operator precedences, taken from the C-- spec
%right '||'     -- non-std extension, called %disjoin in C--
%right '&&'     -- non-std extension, called %conjoin in C--
%right '!'
%nonassoc '>=' '>' '<=' '<' '!=' '=='
%left '|'
%left '^'
%left '&'
%left '>>' '<<'
%left '-' '+'
%left '/' '*' '%'
%right '~'

%%

cmm     :: { CmmParse () }
        : {- empty -}                   { return () }
        | cmmtop cmm                    { do $1; $2 }

cmmtop  :: { CmmParse () }
        : cmmproc                       { $1 }
        | cmmdata                       { $1 }
        | decl                          { $1 }
        | 'CLOSURE' '(' NAME ',' NAME lits ')' ';'
                {% do
                      home_unit_id <- getHomeUnitId
                      liftP $ pure $ do
                        lits <- sequence $6;
                        staticClosure home_unit_id $3 $5 (map getLit lits) }

-- The only static closures in the RTS are dummy closures like
-- stg_END_TSO_QUEUE_closure and stg_dummy_ret.  We don't need
-- to provide the full generality of static closures here.
-- In particular:
--      * CCS can always be CCS_DONT_CARE
--      * closure is always extern
--      * payload is always empty
--      * we can derive closure and info table labels from a single NAME

cmmdata :: { CmmParse () }
        : 'section' STRING '{' data_label statics '}'
                { do lbl <- $4;
                     ss <- sequence $5;
                     code (emitDecl (CmmData (Section (section $2) lbl) (CmmStaticsRaw lbl (concat ss)))) }

data_label :: { CmmParse CLabel }
    : NAME ':'
                {% do
                   home_unit_id <- getHomeUnitId
                   liftP $ pure $ do
                     pure (mkCmmDataLabel home_unit_id (NeedExternDecl False) $1) }

statics :: { [CmmParse [CmmStatic]] }
        : {- empty -}                   { [] }
        | static statics                { $1 : $2 }

static  :: { CmmParse [CmmStatic] }
        : type expr ';' { do e <- $2;
                             return [CmmStaticLit (getLit e)] }
        | type ';'                      { return [CmmUninitialised
                                                        (widthInBytes (typeWidth $1))] }
        | 'bits8' '[' ']' STRING ';'    { return [mkString $4] }
        | 'bits8' '[' INT ']' ';'       { return [CmmUninitialised
                                                        (fromIntegral $3)] }
        | typenot8 '[' INT ']' ';'      { return [CmmUninitialised
                                                (widthInBytes (typeWidth $1) *
                                                        fromIntegral $3)] }
        | 'CLOSURE' '(' NAME lits ')'
                { do { lits <- sequence $4
                ; profile <- getProfile
                     ; return $ map CmmStaticLit $
                        mkStaticClosure profile (mkForeignLabel $3 ForeignLabelInExternalPackage IsData)
                         -- mkForeignLabel because these are only used
                         -- for CHARLIKE and INTLIKE closures in the RTS.
                        dontCareCCS (map getLit lits) [] [] [] [] } }
        -- arrays of closures required for the CHARLIKE & INTLIKE arrays

lits    :: { [CmmParse CmmExpr] }
        : {- empty -}           { [] }
        | ',' expr lits         { $2 : $3 }

cmmproc :: { CmmParse () }
        : info maybe_conv maybe_formals maybe_body
                { do ((entry_ret_label, info, stk_formals, formals), agraph) <-
                       getCodeScoped $ loopDecls $ do {
                         (entry_ret_label, info, stk_formals) <- $1;
                         platform <- getPlatform;
                         ctx      <- getContext;
                         formals <- sequence (fromMaybe [] $3);
                         withName (showSDocOneLine ctx (pprCLabel platform entry_ret_label))
                           $4;
                         return (entry_ret_label, info, stk_formals, formals) }
                     let do_layout = isJust $3
                     code (emitProcWithStackFrame $2 info
                                entry_ret_label stk_formals formals agraph
                                do_layout ) }

maybe_conv :: { Convention }
           : {- empty -}        { NativeNodeCall }
           | 'return'           { NativeReturn }

maybe_body :: { CmmParse () }
           : ';'                { return () }
           | '{' body '}'       { withSourceNote $1 $3 $2 }

info    :: { CmmParse (CLabel, Maybe CmmInfoTable, [LocalReg]) }
        : NAME
                {% do
                     home_unit_id <- getHomeUnitId
                     liftP $ pure $ do
                       newFunctionName $1 home_unit_id
                       return (mkCmmCodeLabel home_unit_id $1, Nothing, []) }


        | 'INFO_TABLE' '(' NAME ',' INT ',' INT ',' INT ',' STRING ',' STRING ')'
                -- ptrs, nptrs, closure type, description, type
                {% do
                      home_unit_id <- getHomeUnitId
                      liftP $ pure $ do
                        profile <- getProfile
                        let prof = profilingInfo profile $11 $13
                            rep  = mkRTSRep (fromIntegral $9) $
                                     mkHeapRep profile False (fromIntegral $5)
                                                     (fromIntegral $7) Thunk
                                -- not really Thunk, but that makes the info table
                                -- we want.
                        return (mkCmmEntryLabel home_unit_id $3,
                                Just $ CmmInfoTable { cit_lbl = mkCmmInfoLabel home_unit_id $3
                                             , cit_rep = rep
                                             , cit_prof = prof, cit_srt = Nothing, cit_clo = Nothing },
                                []) }

        | 'INFO_TABLE_FUN' '(' NAME ',' INT ',' INT ',' INT ',' STRING ',' STRING ',' INT ',' INT ')'
                -- ptrs, nptrs, closure type, description, type, arity, fun type
                {% do
                      home_unit_id <- getHomeUnitId
                      liftP $ pure $ do
                        profile <- getProfile
                        let prof = profilingInfo profile $11 $13
                            ty   = Fun (fromIntegral $15) (ArgSpec (fromIntegral $17))
                            rep = mkRTSRep (fromIntegral $9) $
                                      mkHeapRep profile False (fromIntegral $5)
                                                      (fromIntegral $7) ty
                        return (mkCmmEntryLabel home_unit_id $3,
                                Just $ CmmInfoTable { cit_lbl = mkCmmInfoLabel home_unit_id $3
                                             , cit_rep = rep
                                             , cit_prof = prof, cit_srt = Nothing, cit_clo = Nothing },
                                []) }
                -- we leave most of the fields zero here.  This is only used
                -- to generate the BCO and CONTINUATION info tables in the RTS at the moment.

        | 'INFO_TABLE_CONSTR' '(' NAME ',' INT ',' INT ',' INT ',' INT ',' STRING ',' STRING ')'
                -- ptrs, nptrs, tag, closure type, description, type
                {% do
                      home_unit_id <- getHomeUnitId
                      liftP $ pure $ do
                        profile <- getProfile
                        let prof = profilingInfo profile $13 $15
                            ty  = Constr (fromIntegral $9)  -- Tag
                                         (BS8.pack $13)
                            rep = mkRTSRep (fromIntegral $11) $
                                    mkHeapRep profile False (fromIntegral $5)
                                                    (fromIntegral $7) ty
                        return (mkCmmEntryLabel home_unit_id $3,
                                Just $ CmmInfoTable { cit_lbl = mkCmmInfoLabel home_unit_id $3
                                             , cit_rep = rep
                                             , cit_prof = prof, cit_srt = Nothing,cit_clo = Nothing },
                                []) }

                     -- If profiling is on, this string gets duplicated,
                     -- but that's the way the old code did it we can fix it some other time.

        | 'INFO_TABLE_SELECTOR' '(' NAME ',' INT ',' INT ',' STRING ',' STRING ')'
                -- selector, closure type, description, type
                {% do
                      home_unit_id <- getHomeUnitId
                      liftP $ pure $ do
                        profile <- getProfile
                        let prof = profilingInfo profile $9 $11
                            ty  = ThunkSelector (fromIntegral $5)
                            rep = mkRTSRep (fromIntegral $7) $
                                     mkHeapRep profile False 0 0 ty
                        return (mkCmmEntryLabel home_unit_id $3,
                                Just $ CmmInfoTable { cit_lbl = mkCmmInfoLabel home_unit_id $3
                                             , cit_rep = rep
                                             , cit_prof = prof, cit_srt = Nothing, cit_clo = Nothing },
                                []) }

        | 'INFO_TABLE_RET' '(' NAME ',' INT ')'
                -- closure type (no live regs)
                {% do
                      home_unit_id <- getHomeUnitId
                      liftP $ pure $ do
                        let prof = NoProfilingInfo
                            rep  = mkRTSRep (fromIntegral $5) $ mkStackRep []
                        return (mkCmmRetLabel home_unit_id $3,
                                Just $ CmmInfoTable { cit_lbl = mkCmmRetInfoLabel home_unit_id $3
                                             , cit_rep = rep
                                             , cit_prof = prof, cit_srt = Nothing, cit_clo = Nothing },
                                []) }

        | 'INFO_TABLE_RET' '(' NAME ',' INT ',' formals0 ')'
                -- closure type, live regs
                {% do
                      home_unit_id <- getHomeUnitId
                      liftP $ pure $ do
                        platform <- getPlatform
                        live <- sequence $7
                        let prof = NoProfilingInfo
                            -- drop one for the info pointer
                            bitmap = mkLiveness platform (drop 1 live)
                            rep  = mkRTSRep (fromIntegral $5) $ mkStackRep bitmap
                        return (mkCmmRetLabel home_unit_id $3,
                                Just $ CmmInfoTable { cit_lbl = mkCmmRetInfoLabel home_unit_id $3
                                             , cit_rep = rep
                                             , cit_prof = prof, cit_srt = Nothing, cit_clo = Nothing },
                                live) }

body    :: { CmmParse () }
        : {- empty -}                   { return () }
        | decl body                     { do $1; $2 }
        | stmt body                     { do $1; $2 }

decl    :: { CmmParse () }
        : type names ';'                { mapM_ (newLocal $1) $2 }
        | 'import' importNames ';'      { mapM_ newImport $2 }
        | 'export' names ';'            { return () }  -- ignore exports


-- an imported function name, with optional packageId
importNames
        :: { [(FastString, CLabel)] }
        : importName                    { [$1] }
        | importName ',' importNames    { $1 : $3 }

importName
        :: { (FastString,  CLabel) }

        -- A label imported without an explicit packageId.
        --      These are taken to come from some foreign, unnamed package.
        : NAME
        { ($1, mkForeignLabel $1 ForeignLabelInExternalPackage IsFunction) }

        -- as previous 'NAME', but 'IsData'
        | 'CLOSURE' NAME
        { ($2, mkForeignLabel $2 ForeignLabelInExternalPackage IsData) }

        -- A label imported with an explicit UnitId.
        | STRING NAME
        { ($2, mkCmmCodeLabel (UnitId (mkFastString $1)) $2) }


names   :: { [FastString] }
        : NAME                          { [$1] }
        | NAME ',' names                { $1 : $3 }

stmt    :: { CmmParse () }
        : ';'                                   { return () }

        | NAME ':'
                { do l <- newLabel $1; emitLabel l }



        | lreg '=' expr ';'
                { do reg <- $1; e <- $3; withSourceNote $2 $4 (emitAssign reg e) }

        -- Use lreg instead of local_reg to avoid ambiguity
        | lreg '=' mem_ordering type '[' expr ']' ';'
                { do reg <- $1;
                     let lreg = case reg of
                                  { CmmLocal r -> r
                                  ; other -> pprPanic "CmmParse:" (ppr reg <> text "not a local register")
                                  } ;
                     mord <- $3;
                     let { ty = $4; w = typeWidth ty };
                     e <- $6;
                     let op = MO_AtomicRead w mord;
                     withSourceNote $2 $7 $ code (emitPrimCall [lreg] op [e]) }
        | mem_ordering type '[' expr ']' '=' expr ';'
                { do mord <- $1; withSourceNote $3 $8 (doStore (Just mord) $2 $4 $7) }
        | type '[' expr ']' '=' expr ';'
                { withSourceNote $2 $7 (doStore Nothing $1 $3 $6) }

        -- Gah! We really want to say "foreign_results" but that causes
        -- a shift/reduce conflict with assignment.  We either
        -- we expand out the no-result and single result cases or
        -- we tweak the syntax to avoid the conflict.  The later
        -- option is taken here because the other way would require
        -- multiple levels of expanding and get unwieldy.
        | foreign_results 'foreign' STRING foreignLabel '(' cmm_hint_exprs0 ')' safety opt_never_returns ';'
                {% foreignCall $3 $1 $4 $6 $8 $9 }
        | foreign_results 'prim' '%' NAME '(' exprs0 ')' ';'
                {% primCall $1 $4 $6 }
        -- stmt-level macros, stealing syntax from ordinary C-- function calls.
        -- Perhaps we ought to use the %%-form?
        | NAME '(' exprs0 ')' ';'
                {% stmtMacro $1 $3  }
        | 'switch' maybe_range expr '{' arms default '}'
                { do as <- sequence $5; doSwitch $2 $3 as $6 }
        | 'goto' NAME ';'
                { do l <- lookupLabel $2; emit (mkBranch l) }
        | 'return' '(' exprs0 ')' ';'
                { doReturn $3 }
        | 'jump' expr vols ';'
                { doRawJump $2 $3 }
        | 'jump' expr '(' exprs0 ')' ';'
                { doJumpWithStack $2 [] $4 }
        | 'jump' expr '(' exprs0 ')' '(' exprs0 ')' ';'
                { doJumpWithStack $2 $4 $7 }
        | 'call' expr '(' exprs0 ')' ';'
                { doCall $2 [] $4 }
        | '(' formals ')' '=' 'call' expr '(' exprs0 ')' ';'
                { doCall $6 $2 $8 }
        | 'if' bool_expr cond_likely 'goto' NAME
                { do l <- lookupLabel $5; cmmRawIf $2 l $3 }
        | 'if' bool_expr cond_likely '{' body '}' else
                { cmmIfThenElse $2 (withSourceNote $4 $6 $5) $7 $3 }
        | 'push' '(' exprs0 ')' maybe_body
                { pushStackFrame $3 $5 }
        | 'reserve' expr '=' lreg maybe_body
                { reserveStackFrame $2 $4 $5 }
        | 'unwind' unwind_regs ';'
                { $2 >>= code . emitUnwind }

unwind_regs
        :: { CmmParse [(GlobalReg, Maybe CmmExpr)] }
        : GLOBALREG '=' expr_or_unknown ',' unwind_regs
                { do e <- $3; rest <- $5; return ((globalRegUse_reg $1, e) : rest) }
        | GLOBALREG '=' expr_or_unknown
                { do e <- $3; return [(globalRegUse_reg $1, e)] }

-- | A memory ordering
mem_ordering :: { CmmParse MemoryOrdering }
mem_ordering
        : 'relaxed' { do return MemOrderRelaxed }
        | 'release' { do return MemOrderRelease }
        | 'acquire' { do return MemOrderAcquire }
        | 'seq_cst' { do return MemOrderSeqCst }

-- | Used by unwind to indicate unknown unwinding values.
expr_or_unknown
        :: { CmmParse (Maybe CmmExpr) }
        : 'return'
                { do return Nothing }
        | expr
                { do e <- $1; return (Just e) }

foreignLabel     :: { CmmParse CmmExpr }
        : NAME                          { return (CmmLit (CmmLabel (mkForeignLabel $1 ForeignLabelInThisPackage IsFunction))) }

opt_never_returns :: { CmmReturnInfo }
        :                               { CmmMayReturn }
        | 'never' 'returns'             { CmmNeverReturns }

bool_expr :: { CmmParse BoolExpr }
        : bool_op                       { $1 }
        | expr                          { do e <- $1; return (BoolTest e) }

bool_op :: { CmmParse BoolExpr }
        : bool_expr '&&' bool_expr      { do e1 <- $1; e2 <- $3;
                                          return (BoolAnd e1 e2) }
        | bool_expr '||' bool_expr      { do e1 <- $1; e2 <- $3;
                                          return (BoolOr e1 e2)  }
        | '!' bool_expr                 { do e <- $2; return (BoolNot e) }
        | '(' bool_op ')'               { $2 }

safety  :: { Safety }
        : {- empty -}                   { PlayRisky }
        | STRING                        {% parseSafety $1 }

vols    :: { [GlobalRegUse] }
        : '[' ']'                       { [] }
        | GP_ARG_REGS                   {% do platform <- PD.getPlatform;
                                              return
                                                [ GlobalRegUse r (globalRegSpillType platform r)
                                                | r <- realArgRegsCover platform GP_ARG_REGS ] }
        | SCALAR_ARG_REGS               {% do platform <- PD.getPlatform;
                                              return
                                                [ GlobalRegUse r (globalRegSpillType platform r)
                                                | r <- realArgRegsCover platform SCALAR_ARG_REGS ] }
        | V16_ARG_REGS                  {% do platform <- PD.getPlatform;
                                              return
                                                [ GlobalRegUse r (globalRegSpillType platform r)
                                                | r <- realArgRegsCover platform V16_ARG_REGS ] }
        | V32_ARG_REGS                  {% do platform <- PD.getPlatform;
                                              return
                                                [ GlobalRegUse r (globalRegSpillType platform r)
                                                | r <- realArgRegsCover platform V32_ARG_REGS ] }
        | V64_ARG_REGS                  {% do platform <- PD.getPlatform;
                                              return
                                                [ GlobalRegUse r (globalRegSpillType platform r)
                                                | r <- realArgRegsCover platform V64_ARG_REGS ] }
        | '[' globals ']'               { $2 }

globals :: { [GlobalRegUse] }
        : GLOBALREG                     { [$1] }
        | GLOBALREG ',' globals         { $1 : $3 }

maybe_range :: { Maybe (Integer,Integer) }
        : '[' INT '..' INT ']'  { Just ($2, $4) }
        | {- empty -}           { Nothing }

arms    :: { [CmmParse ([Integer],Either BlockId (CmmParse ()))] }
        : {- empty -}                   { [] }
        | arm arms                      { $1 : $2 }

arm     :: { CmmParse ([Integer],Either BlockId (CmmParse ())) }
        : 'case' ints ':' arm_body      { do b <- $4; return ($2, b) }

arm_body :: { CmmParse (Either BlockId (CmmParse ())) }
        : '{' body '}'                  { return (Right (withSourceNote $1 $3 $2)) }
        | 'goto' NAME ';'               { do l <- lookupLabel $2; return (Left l) }

ints    :: { [Integer] }
        : INT                           { [ $1 ] }
        | INT ',' ints                  { $1 : $3 }

default :: { Maybe (CmmParse ()) }
        : 'default' ':' '{' body '}'    { Just (withSourceNote $3 $5 $4) }
        -- taking a few liberties with the C-- syntax here; C-- doesn't have
        -- 'default' branches
        | {- empty -}                   { Nothing }

-- Note: OldCmm doesn't support a first class 'else' statement, though
-- CmmNode does.
else    :: { CmmParse () }
        : {- empty -}                   { return () }
        | 'else' '{' body '}'           { withSourceNote $2 $4 $3 }

cond_likely :: { Maybe Bool }
        : '(' 'likely' ':' 'True'  ')'  { Just True  }
        | '(' 'likely' ':' 'False' ')'  { Just False }
        | {- empty -}                   { Nothing }


-- we have to write this out longhand so that Happy's precedence rules
-- can kick in.
expr    :: { CmmParse CmmExpr }
        : expr '/' expr                 { mkMachOp MO_U_Quot (TupleG2 $1 $3) }
        | expr '*' expr                 { mkMachOp MO_Mul (TupleG2 $1 $3) }
        | expr '%' expr                 { mkMachOp MO_U_Rem (TupleG2 $1 $3) }
        | expr '-' expr                 { mkMachOp MO_Sub (TupleG2 $1 $3) }
        | expr '+' expr                 { mkMachOp MO_Add (TupleG2 $1 $3) }
        | expr '>>' expr                { mkMachOp MO_U_Shr (TupleG2 $1 $3) }
        | expr '<<' expr                { mkMachOp MO_Shl (TupleG2 $1 $3) }
        | expr '&' expr                 { mkMachOp MO_And (TupleG2 $1 $3) }
        | expr '^' expr                 { mkMachOp MO_Xor (TupleG2 $1 $3) }
        | expr '|' expr                 { mkMachOp MO_Or (TupleG2 $1 $3) }
        | expr '>=' expr                { mkMachOp MO_U_Ge (TupleG2 $1 $3) }
        | expr '>' expr                 { mkMachOp MO_U_Gt (TupleG2 $1 $3) }
        | expr '<=' expr                { mkMachOp MO_U_Le (TupleG2 $1 $3) }
        | expr '<' expr                 { mkMachOp MO_U_Lt (TupleG2 $1 $3) }
        | expr '!=' expr                { mkMachOp MO_Ne (TupleG2 $1 $3) }
        | expr '==' expr                { mkMachOp MO_Eq (TupleG2 $1 $3) }
        | '~' expr                      { mkMachOp MO_Not (TupleG1 $2) }
        | '-' expr                      { mkMachOp MO_S_Neg (TupleG1 $2) }
        | expr0 '`' NAME '`' expr0      {% parseInfixMachOp $1 $3 $5 }
        | expr0                         { $1 }

expr0   :: { CmmParse CmmExpr }
        : INT   maybe_ty         { return (CmmLit (CmmInt $1 (typeWidth $2))) }
        | FLOAT maybe_ty         { return (CmmLit (CmmFloat $1 (typeWidth $2))) }
        | STRING                 { do s <- code (newStringCLit $1);
                                      return (CmmLit s) }
        | reg                    { $1 }
        | type '!' '[' expr ']'  { do ptr <- $4; return (CmmMachOp (MO_RelaxedRead (typeWidth $1)) (TupleG1 ptr)) }
        | type '^' '[' expr ']'  { do ptr <- $4; return (CmmLoad ptr $1 Unaligned) }
        | type '[' expr ']'      { do ptr <- $3; return (CmmLoad ptr $1 NaturallyAligned) }
        | '%' NAME '(' exprs0 ')' {% exprOp $2 $4 }
        | '(' expr ')'           { $2 }

-- leaving out the type of a literal gives you the native word size in C--
maybe_ty :: { CmmType }
        : {- empty -}                   {% do platform <- PD.getPlatform; return $ bWord platform }
        | '::' type                     { $2 }

cmm_hint_exprs0 :: { [CmmParse (CmmExpr, ForeignHint)] }
        : {- empty -}                   { [] }
        | cmm_hint_exprs                { $1 }

cmm_hint_exprs :: { [CmmParse (CmmExpr, ForeignHint)] }
        : cmm_hint_expr                 { [$1] }
        | cmm_hint_expr ',' cmm_hint_exprs      { $1 : $3 }

cmm_hint_expr :: { CmmParse (CmmExpr, ForeignHint) }
        : expr                          { do e <- $1;
                                             return (e, inferCmmHint e) }
        | expr STRING                   {% do h <- parseCmmHint $2;
                                              return $ do
                                                e <- $1; return (e, h) }

exprs0  :: { [CmmParse CmmExpr] }
        : {- empty -}                   { [] }
        | exprs                         { $1 }

exprs   :: { [CmmParse CmmExpr] }
        : expr                          { [ $1 ] }
        | expr ',' exprs                { $1 : $3 }

reg     :: { CmmParse CmmExpr }
        : NAME                  { lookupName $1 }
        | GLOBALREG             { return (CmmReg (CmmGlobal $1)) }

foreign_results :: { [CmmParse (LocalReg, ForeignHint)] }
        : {- empty -}                   { [] }
        | '(' foreign_formals ')' '='   { $2 }

foreign_formals :: { [CmmParse (LocalReg, ForeignHint)] }
        : foreign_formal                        { [$1] }
        | foreign_formal ','                    { [$1] }
        | foreign_formal ',' foreign_formals    { $1 : $3 }

foreign_formal :: { CmmParse (LocalReg, ForeignHint) }
        : local_lreg            { do e <- $1; return (e, inferCmmHint (CmmReg (CmmLocal e))) }
        | STRING local_lreg     {% do h <- parseCmmHint $1;
                                      return $ do
                                         e <- $2; return (e,h) }

local_lreg :: { CmmParse LocalReg }
        : NAME                  { do e <- lookupName $1;
                                     return $
                                       case e of
                                        CmmReg (CmmLocal r) -> r
                                        other -> pprPanic "CmmParse:" (ftext $1 <> text " not a local register") }

lreg    :: { CmmParse CmmReg }
        : NAME                  { do e <- lookupName $1;
                                     return $
                                       case e of
                                        CmmReg r -> r
                                        other -> pprPanic "CmmParse:" (ftext $1 <> text " not a register") }
        | GLOBALREG             { return (CmmGlobal $1) }

maybe_formals :: { Maybe [CmmParse LocalReg] }
        : {- empty -}           { Nothing }
        | '(' formals0 ')'      { Just $2 }

formals0 :: { [CmmParse LocalReg] }
        : {- empty -}           { [] }
        | formals               { $1 }

formals :: { [CmmParse LocalReg] }
        : formal ','            { [$1] }
        | formal                { [$1] }
        | formal ',' formals       { $1 : $3 }

formal :: { CmmParse LocalReg }
        : type NAME             { newLocal $1 $2 }

type    :: { CmmType }
        : 'bits8'               { b8 }
        | typenot8              { $1 }

typenot8 :: { CmmType }
        : 'bits16'              { b16 }
        | 'bits32'              { b32 }
        | 'bits64'              { b64 }
        | 'vec128'              { cmmVec 2 f64 }
        | 'vec256'              { cmmVec 4 f64 }
        | 'vec512'              { cmmVec 8 f64 }
        | 'float32'             { f32 }
        | 'float64'             { f64 }
        | 'gcptr'               {% do platform <- PD.getPlatform; return $ gcWord platform }

{
section :: String -> SectionType
section "text"      = Text
section "data"      = Data
section "rodata"    = ReadOnlyData
section "relrodata" = RelocatableReadOnlyData
section "bss"       = UninitialisedData
section s           = OtherSection s

mkString :: String -> CmmStatic
mkString s = CmmString (BS8.pack s)

-- mkMachOp infers the type of the MachOp from the type of its first
-- argument.  We assume that this is correct: for MachOps that don't have
-- symmetrical args (e.g. shift ops), the first arg determines the type of
-- the op.
mkMachOp :: (a > 0) => (Width -> MachOp a) -> SizedTupleGADT a (CmmParse CmmExpr) -> CmmParse CmmExpr
mkMachOp fn args = do
  platform <- getPlatform
  arg_exprs <- sequence args
  return (CmmMachOp (fn (typeWidth (cmmExprType platform (firstOfTupleGADT arg_exprs)))) arg_exprs)

parseInfixMachOp :: CmmParse CmmExpr -> FastString -> CmmParse CmmExpr -> PD (CmmParse CmmExpr)
parseInfixMachOp leftArg opName rightArg = do
  MkWidthToMachOpWithArity mo arity <- nameToMachOp opName
  case testEquality arity (SNat @2) of
    Just Refl -> pure $ mkMachOp mo (TupleG2 leftArg rightArg)
    Nothing   -> failMsgPD $ \span -> mkPlainErrorMsgEnvelope span $
        PsErrCmmParser (CmmBadPrimitiveArity
                             { cbpa_op_name = opName
                             , cbpa_real_arity = fromIntegral @Natural @Int (fromSNat arity)
                             , cbpa_num_given_args = 2
                             })

getLit :: CmmExpr -> CmmLit
getLit (CmmLit l) = l
getLit (CmmMachOp (MO_S_Neg _) (TupleG1 (CmmLit (CmmInt i r))))  = CmmInt (negate i) r
getLit _ = panic "invalid literal" -- TODO messy failure

nameToMachOp :: FastString -> PD WidthToMachOpWithArity
nameToMachOp name =
  case lookupUFM machOps name of
        Nothing -> failMsgPD $ \span -> mkPlainErrorMsgEnvelope span $ PsErrCmmParser (CmmUnknownPrimitive name)
        Just m  -> return m

exprOp :: FastString -> [CmmParse CmmExpr] -> PD (CmmParse CmmExpr)
exprOp name args_code = do
  pdc     <- PD.getPDConfig
  let profile = PD.pdProfile pdc
  let align_check = PD.pdSanitizeAlignment pdc
  case lookupUFM (exprMacros profile align_check) name of
     Just f  -> return $ do
        args <- sequence args_code
        return (f args)
     Nothing -> do
        MkWidthToMachOpWithArity mo arity <- nameToMachOp name
        case listToSizedTupleGADT_maybe args_code of
          Just args_code' -> return $ mkMachOp mo args_code'
          Nothing -> failMsgPD $ \span -> mkPlainErrorMsgEnvelope span $
            PsErrCmmParser (CmmBadPrimitiveArity
                             { cbpa_op_name = name
                             , cbpa_real_arity = fromIntegral @Natural @Int (fromSNat arity)
                             , cbpa_num_given_args = length args_code
                             })

exprMacros :: Profile -> DoAlignSanitisation -> UniqFM FastString ([CmmExpr] -> CmmExpr)
exprMacros profile align_check = listToUFM [
  ( fsLit "ENTRY_CODE",   \ [x] -> entryCode platform x ),
  ( fsLit "INFO_PTR",     \ [x] -> closureInfoPtr platform align_check x ),
  ( fsLit "STD_INFO",     \ [x] -> infoTable    profile x ),
  ( fsLit "FUN_INFO",     \ [x] -> funInfoTable profile x ),
  ( fsLit "GET_ENTRY",    \ [x] -> entryCode platform   (closureInfoPtr platform align_check x) ),
  ( fsLit "GET_STD_INFO", \ [x] -> infoTable profile    (closureInfoPtr platform align_check x) ),
  ( fsLit "GET_FUN_INFO", \ [x] -> funInfoTable profile (closureInfoPtr platform align_check x) ),
  ( fsLit "INFO_TYPE",    \ [x] -> infoTableClosureType profile x ),
  ( fsLit "INFO_PTRS",    \ [x] -> infoTablePtrs profile x ),
  ( fsLit "INFO_NPTRS",   \ [x] -> infoTableNonPtrs profile x )
  ]
  where
    platform = profilePlatform profile

data WidthToMachOpWithArity
  = forall (arity :: Natural).
  (KnownNat arity, arity > 0, arity < 4) =>
  MkWidthToMachOpWithArity (Width -> MachOp arity) (SNat arity)

-- we understand a subset of C-- primitives:
machOps :: UniqFM FastString WidthToMachOpWithArity
machOps = listToUFM $
        map (\(x, y) -> (mkFastString x, y)) [
        ( "add",        mk_op MO_Add ),
        ( "sub",        mk_op MO_Sub ),
        ( "eq",         mk_op MO_Eq ),
        ( "ne",         mk_op MO_Ne ),
        ( "mul",        mk_op MO_Mul ),
        ( "mulmayoflo", mk_op MO_S_MulMayOflo ),
        ( "neg",        mk_op MO_S_Neg ),
        ( "quot",       mk_op MO_S_Quot ),
        ( "rem",        mk_op MO_S_Rem ),
        ( "divu",       mk_op MO_U_Quot ),
        ( "modu",       mk_op MO_U_Rem ),

        ( "ge",         mk_op MO_S_Ge ),
        ( "le",         mk_op MO_S_Le ),
        ( "gt",         mk_op MO_S_Gt ),
        ( "lt",         mk_op MO_S_Lt ),

        ( "geu",        mk_op MO_U_Ge ),
        ( "leu",        mk_op MO_U_Le ),
        ( "gtu",        mk_op MO_U_Gt ),
        ( "ltu",        mk_op MO_U_Lt ),

        ( "and",        mk_op MO_And ),
        ( "or",         mk_op MO_Or ),
        ( "xor",        mk_op MO_Xor ),
        ( "com",        mk_op MO_Not ),
        ( "shl",        mk_op MO_Shl ),
        ( "shrl",       mk_op MO_U_Shr ),
        ( "shra",       mk_op MO_S_Shr ),

        ( "fadd",       mk_op MO_F_Add ),
        ( "fsub",       mk_op MO_F_Sub ),
        ( "fneg",       mk_op MO_F_Neg ),
        ( "fmul",       mk_op MO_F_Mul ),
        ( "fquot",      mk_op MO_F_Quot ),
        ( "fmin",       mk_op MO_F_Min ),
        ( "fmax",       mk_op MO_F_Max ),

        ( "fmadd" ,     mk_op (MO_FMA FMAdd  1) ),
        ( "fmsub" ,     mk_op (MO_FMA FMSub  1) ),
        ( "fnmadd",     mk_op (MO_FMA FNMAdd 1) ),
        ( "fnmsub",     mk_op (MO_FMA FNMSub 1) ),

        ( "feq",        mk_op MO_F_Eq ),
        ( "fne",        mk_op MO_F_Ne ),
        ( "fge",        mk_op MO_F_Ge ),
        ( "fle",        mk_op MO_F_Le ),
        ( "fgt",        mk_op MO_F_Gt ),
        ( "flt",        mk_op MO_F_Lt ),

        ( "lobits8",  mk_op (flip MO_UU_Conv W8 ) ),
        ( "lobits16", mk_op (flip MO_UU_Conv W16) ),
        ( "lobits32", mk_op (flip MO_UU_Conv W32) ),
        ( "lobits64", mk_op (flip MO_UU_Conv W64) ),

        ( "zx16",     mk_op (flip MO_UU_Conv W16) ),
        ( "zx32",     mk_op (flip MO_UU_Conv W32) ),
        ( "zx64",     mk_op (flip MO_UU_Conv W64) ),

        ( "sx16",     mk_op (flip MO_SS_Conv W16) ),
        ( "sx32",     mk_op (flip MO_SS_Conv W32) ),
        ( "sx64",     mk_op (flip MO_SS_Conv W64) ),

        ( "f2f32",    mk_op (flip MO_FF_Conv W32) ),  -- TODO; rounding mode
        ( "f2f64",    mk_op (flip MO_FF_Conv W64) ),  -- TODO; rounding mode
        ( "f2i8",     mk_op (flip MO_FS_Truncate W8) ),
        ( "f2i16",    mk_op (flip MO_FS_Truncate W16) ),
        ( "f2i32",    mk_op (flip MO_FS_Truncate W32) ),
        ( "f2i64",    mk_op (flip MO_FS_Truncate W64) ),
        ( "i2f32",    mk_op (flip MO_SF_Round W32) ),
        ( "i2f64",    mk_op (flip MO_SF_Round W64) ),

        ( "w2f_bitcast", mk_op (MO_WF_Bitcast) ),
        ( "f2w_bitcast", mk_op (MO_FW_Bitcast) )
        ]
  where mk_op :: forall (arity :: Natural). (KnownNat arity, arity > 0, arity < 4)
              => (Width -> MachOp arity) -> WidthToMachOpWithArity
        mk_op fun = MkWidthToMachOpWithArity fun SNat

callishMachOps :: Platform -> UniqFM FastString ([CmmExpr] -> (CallishMachOp, [CmmExpr]))
callishMachOps platform = listToUFM $
        map (\(x, y) -> (mkFastString x, y)) [

        ( "pow64f", (MO_F64_Pwr,) ),
        ( "sin64f", (MO_F64_Sin,) ),
        ( "cos64f", (MO_F64_Cos,) ),
        ( "tan64f", (MO_F64_Tan,) ),
        ( "sinh64f", (MO_F64_Sinh,) ),
        ( "cosh64f", (MO_F64_Cosh,) ),
        ( "tanh64f", (MO_F64_Tanh,) ),
        ( "asin64f", (MO_F64_Asin,) ),
        ( "acos64f", (MO_F64_Acos,) ),
        ( "atan64f", (MO_F64_Atan,) ),
        ( "asinh64f", (MO_F64_Asinh,) ),
        ( "acosh64f", (MO_F64_Acosh,) ),
        ( "log64f", (MO_F64_Log,) ),
        ( "log1p64f", (MO_F64_Log1P,) ),
        ( "exp64f", (MO_F64_Exp,) ),
        ( "expM164f", (MO_F64_ExpM1,) ),
        ( "fabs64f", (MO_F64_Fabs,) ),
        ( "sqrt64f", (MO_F64_Sqrt,) ),

        ( "pow32f", (MO_F32_Pwr,) ),
        ( "sin32f", (MO_F32_Sin,) ),
        ( "cos32f", (MO_F32_Cos,) ),
        ( "tan32f", (MO_F32_Tan,) ),
        ( "sinh32f", (MO_F32_Sinh,) ),
        ( "cosh32f", (MO_F32_Cosh,) ),
        ( "tanh32f", (MO_F32_Tanh,) ),
        ( "asin32f", (MO_F32_Asin,) ),
        ( "acos32f", (MO_F32_Acos,) ),
        ( "atan32f", (MO_F32_Atan,) ),
        ( "asinh32f", (MO_F32_Asinh,) ),
        ( "acosh32f", (MO_F32_Acosh,) ),
        ( "log32f", (MO_F32_Log,) ),
        ( "log1p32f", (MO_F32_Log1P,) ),
        ( "exp32f", (MO_F32_Exp,) ),
        ( "expM132f", (MO_F32_ExpM1,) ),
        ( "fabs32f", (MO_F32_Fabs,) ),
        ( "sqrt32f", (MO_F32_Sqrt,) ),

        -- TODO: It would be nice to rename the following operations to
        -- acquire_fence and release_fence. Be aware that there'll be issues
        -- with an overlapping token ('acquire') in the lexer.
        ( "fence_acquire", (MO_AcquireFence,)),
        ( "fence_release", (MO_ReleaseFence,)),
        ( "fence_seq_cst", (MO_SeqCstFence,)),

        ( "memcpy", memcpyLikeTweakArgs MO_Memcpy ),
        ( "memset", memcpyLikeTweakArgs MO_Memset ),
        ( "memmove", memcpyLikeTweakArgs MO_Memmove ),
        ( "memcmp", memcpyLikeTweakArgs MO_Memcmp ),

        ( "suspendThread", (MO_SuspendThread,) ),
        ( "resumeThread",  (MO_ResumeThread,) ),

        ( "prefetch0", (MO_Prefetch_Data 0,)),
        ( "prefetch1", (MO_Prefetch_Data 1,)),
        ( "prefetch2", (MO_Prefetch_Data 2,)),
        ( "prefetch3", (MO_Prefetch_Data 3,)),

        ( "bswap16", (MO_BSwap W16,) ),
        ( "bswap32", (MO_BSwap W32,) ),
        ( "bswap64", (MO_BSwap W64,) )
    ] ++ concat
    [ allWidths "popcnt" MO_PopCnt
    , allWidths "pdep" MO_Pdep
    , allWidths "pext" MO_Pext
    , allWidths "cmpxchg" MO_Cmpxchg
    , allWidths "xchg" MO_Xchg
    , allWidths "load_relaxed" (\w -> MO_AtomicRead w MemOrderAcquire)
    , allWidths "load_acquire" (\w -> MO_AtomicRead w MemOrderAcquire)
    , allWidths "load_seqcst" (\w -> MO_AtomicRead w MemOrderSeqCst)
    , allWidths "store_release" (\w -> MO_AtomicWrite w MemOrderRelease)
    , allWidths "store_seqcst" (\w -> MO_AtomicWrite w MemOrderSeqCst)
    , allWidths "fetch_add" (\w -> MO_AtomicRMW w AMO_Add)
    , allWidths "fetch_sub" (\w -> MO_AtomicRMW w AMO_Sub)
    , allWidths "fetch_and" (\w -> MO_AtomicRMW w AMO_And)
    , allWidths "fetch_nand" (\w -> MO_AtomicRMW w AMO_Nand)
    , allWidths "fetch_or" (\w -> MO_AtomicRMW w AMO_Or)
    , allWidths "fetch_xor" (\w -> MO_AtomicRMW w AMO_Xor)
    , allWidths "mul2_" (\w -> MO_S_Mul2 w)
    , allWidths "mul2u_" (\w -> MO_U_Mul2 w)
    ]
  where
    allWidths
        :: String
        -> (Width -> CallishMachOp)
        -> [(FastString, a -> (CallishMachOp, a))]
    allWidths name f =
        [ (mkFastString $ name ++ show (widthInBits w), (f w,))
        | w <- [W8, W16, W32, W64]
        ]

    memcpyLikeTweakArgs :: (Int -> CallishMachOp) -> [CmmExpr] -> (CallishMachOp, [CmmExpr])
    memcpyLikeTweakArgs op [] = pgmError "memcpy-like function requires at least one argument"
    memcpyLikeTweakArgs op args@(_:_) =
        (op align, args')
      where
        args' = init args
        align = case last args of
          CmmLit (CmmInt alignInteger _) -> fromInteger alignInteger
          e -> pgmErrorDoc "Non-constant alignment in memcpy-like function:" (pdoc platform e)
        -- The alignment of memcpy-ish operations must be a
        -- compile-time constant. We verify this here, passing it around
        -- in the MO_* constructor. In order to do this, however, we
        -- must intercept the arguments in primCall.

parseSafety :: String -> PD Safety
parseSafety "safe"   = return PlaySafe
parseSafety "unsafe" = return PlayRisky
parseSafety "interruptible" = return PlayInterruptible
parseSafety str      = failMsgPD $ \span -> mkPlainErrorMsgEnvelope span $
                                              PsErrCmmParser (CmmUnrecognisedSafety str)

parseCmmHint :: String -> PD ForeignHint
parseCmmHint "ptr"    = return AddrHint
parseCmmHint "signed" = return SignedHint
parseCmmHint str      = failMsgPD $ \span -> mkPlainErrorMsgEnvelope span $
                                               PsErrCmmParser (CmmUnrecognisedHint str)

-- labels are always pointers, so we might as well infer the hint
inferCmmHint :: CmmExpr -> ForeignHint
inferCmmHint (CmmLit (CmmLabel _))
  = AddrHint
inferCmmHint (CmmReg (CmmGlobal reg))
  | isPtrGlobalRegUse reg
  = AddrHint
inferCmmHint _
  = NoHint

isPtrGlobalRegUse :: GlobalRegUse -> Bool
isPtrGlobalRegUse (GlobalRegUse reg ty)
  | VanillaReg {} <- reg
  , isGcPtrType ty
  = True
  | otherwise
  = go reg
  where
    go Sp             = True
    go SpLim          = True
    go Hp             = True
    go HpLim          = True
    go CCCS           = True
    go CurrentTSO     = True
    go CurrentNursery = True
    go _              = False

happyError :: PD a
happyError = PD $ \_ _ s -> unP srcParseFail s

-- -----------------------------------------------------------------------------
-- Statement-level macros

stmtMacro :: FastString -> [CmmParse CmmExpr] -> PD (CmmParse ())
stmtMacro fun args_code = do
  case lookupUFM stmtMacros fun of
    Nothing -> failMsgPD $ \span -> mkPlainErrorMsgEnvelope span $ PsErrCmmParser (CmmUnknownMacro fun)
    Just fcode -> return $ do
        args <- sequence args_code
        code (fcode args)

stmtMacros :: UniqFM FastString ([CmmExpr] -> FCode ())
stmtMacros = listToUFM [
  ( fsLit "CCS_ALLOC",             \[words,ccs]  -> profAlloc words ccs ),
  ( fsLit "ENTER_CCS_THUNK",       \[e] -> enterCostCentreThunk e ),

  ( fsLit "CLOSE_NURSERY",         \[]  -> emitCloseNursery ),
  ( fsLit "OPEN_NURSERY",          \[]  -> emitOpenNursery ),

  -- completely generic heap and stack checks, for use in high-level cmm.
  ( fsLit "HP_CHK_GEN",            \[bytes] ->
                                      heapStackCheckGen Nothing (Just bytes) ),
  ( fsLit "STK_CHK_GEN",           \[] ->
                                      heapStackCheckGen (Just (CmmLit CmmHighStackMark)) Nothing ),

  -- A stack check for a fixed amount of stack.  Sounds a bit strange, but
  -- we use the stack for a bit of temporary storage in a couple of primops
  ( fsLit "STK_CHK_GEN_N",         \[bytes] ->
                                      heapStackCheckGen (Just bytes) Nothing ),

  -- A stack check on entry to a thunk, where the argument is the thunk pointer.
  ( fsLit "STK_CHK_NP"   ,         \[node] -> entryHeapCheck' False node 0 [] (return ())),

  ( fsLit "LOAD_THREAD_STATE",     \[] -> emitLoadThreadState ),
  ( fsLit "SAVE_THREAD_STATE",     \[] -> emitSaveThreadState ),

  ( fsLit "SAVE_GP_ARG_REGS",         \[] -> emitSaveRegs GP_ARG_REGS ),
  ( fsLit "RESTORE_GP_ARG_REGS",      \[] -> emitRestoreRegs GP_ARG_REGS ),
  ( fsLit "SAVE_SCALAR_ARG_REGS",     \[] -> emitSaveRegs SCALAR_ARG_REGS ),
  ( fsLit "RESTORE_SCALAR_ARG_REGS",  \[] -> emitRestoreRegs SCALAR_ARG_REGS ),
  ( fsLit "SAVE_V16_ARG_REGS",        \[] -> emitSaveRegs V16_ARG_REGS ),
  ( fsLit "RESTORE_V16_ARG_REGS",     \[] -> emitRestoreRegs V16_ARG_REGS ),
  ( fsLit "SAVE_V32_ARG_REGS",        \[] -> emitSaveRegs V32_ARG_REGS ),
  ( fsLit "RESTORE_V32_ARG_REGS",     \[] -> emitRestoreRegs V32_ARG_REGS ),
  ( fsLit "SAVE_V64_ARG_REGS",         \[] -> emitSaveRegs V64_ARG_REGS ),
  ( fsLit "RESTORE_V64_ARG_REGS",      \[] -> emitRestoreRegs V64_ARG_REGS ),

  ( fsLit "PUSH_SCALAR_ARG_REGS",     \[live_regs] -> emitPushArgRegs SCALAR_ARG_REGS live_regs ),
  ( fsLit "POP_SCALAR_ARG_REGS",      \[live_regs] -> emitPopArgRegs SCALAR_ARG_REGS live_regs ),

  ( fsLit "LDV_ENTER",             \[e] -> ldvEnter e ),
  ( fsLit "PROF_HEADER_CREATE",     \[e] -> profHeaderCreate e ),

  ( fsLit "PUSH_UPD_FRAME",        \[sp,e] -> emitPushUpdateFrame sp e ),
  ( fsLit "SET_HDR",               \[ptr,info,ccs] ->
                                        emitSetDynHdr ptr info ccs ),
  ( fsLit "TICK_ALLOC_PRIM",       \[hdr,goods,slop] ->
                                        tickyAllocPrim hdr goods slop ),
  ( fsLit "TICK_ALLOC_PAP",        \[goods,slop] ->
                                        tickyAllocPAP goods slop ),
  ( fsLit "TICK_ALLOC_UP_THK",     \[goods,slop] ->
                                        tickyAllocThunk goods slop ),
  ( fsLit "UPD_BH_UPDATABLE",      \[reg] -> emitBlackHoleCode reg )
 ]

emitPushUpdateFrame :: CmmExpr -> CmmExpr -> FCode ()
emitPushUpdateFrame sp e = do
  emitUpdateFrame sp mkUpdInfoLabel e

pushStackFrame :: [CmmParse CmmExpr] -> CmmParse () -> CmmParse ()
pushStackFrame fields body = do
  profile <- getProfile
  exprs <- sequence fields
  updfr_off <- getUpdFrameOff
  let (new_updfr_off, _, g) = copyOutOflow profile NativeReturn Ret Old
                                           [] updfr_off exprs
  emit g
  withUpdFrameOff new_updfr_off body

reserveStackFrame
  :: CmmParse CmmExpr
  -> CmmParse CmmReg
  -> CmmParse ()
  -> CmmParse ()
reserveStackFrame psize preg body = do
  platform <- getPlatform
  old_updfr_off <- getUpdFrameOff
  reg <- preg
  esize <- psize
  let size = case constantFoldExpr platform esize of
               CmmLit (CmmInt n _) -> n
               _other -> pprPanic "CmmParse: not a compile-time integer: "
                            (pdoc platform esize)
  let frame = old_updfr_off + platformWordSizeInBytes platform * fromIntegral size
  emitAssign reg (CmmStackSlot Old frame)
  withUpdFrameOff frame body

profilingInfo profile desc_str ty_str
  = if not (profileIsProfiling profile)
    then NoProfilingInfo
    else ProfilingInfo (BS8.pack desc_str) (BS8.pack ty_str)

staticClosure :: UnitId -> FastString -> FastString -> [CmmLit] -> CmmParse ()
staticClosure pkg cl_label info payload
  = do profile <- getProfile
       let lits = mkStaticClosure profile (mkCmmInfoLabel pkg info) dontCareCCS payload [] [] [] []
       code $ emitDataLits (mkCmmDataLabel pkg (NeedExternDecl True) cl_label) lits

foreignCall
        :: String
        -> [CmmParse (LocalReg, ForeignHint)]
        -> CmmParse CmmExpr
        -> [CmmParse (CmmExpr, ForeignHint)]
        -> Safety
        -> CmmReturnInfo
        -> PD (CmmParse ())
foreignCall conv_string results_code expr_code args_code safety ret
  = do  conv <- case conv_string of
          "C"       -> return CCallConv
          "stdcall" -> return StdCallConv
          _         -> failMsgPD $ \span -> mkPlainErrorMsgEnvelope span $
                                              PsErrCmmParser (CmmUnknownCConv conv_string)
        return $ do
          platform <- getPlatform
          results <- sequence results_code
          expr <- expr_code
          args <- sequence args_code
          let
                  (arg_exprs, arg_hints) = unzip args
                  (res_regs,  res_hints) = unzip results
                  fc = ForeignConvention conv arg_hints res_hints ret
                  target = ForeignTarget expr fc
          _ <- code $ emitForeignCall safety res_regs target arg_exprs
          return ()


doReturn :: [CmmParse CmmExpr] -> CmmParse ()
doReturn exprs_code = do
  profile <- getProfile
  exprs <- sequence exprs_code
  updfr_off <- getUpdFrameOff
  emit (mkReturnSimple profile exprs updfr_off)

mkReturnSimple  :: Profile -> [CmmActual] -> UpdFrameOffset -> CmmAGraph
mkReturnSimple profile actuals updfr_off =
  mkReturn profile e actuals updfr_off
  where e = entryCode platform (cmmLoadGCWord platform (CmmStackSlot Old updfr_off))
        platform = profilePlatform profile

doRawJump :: CmmParse CmmExpr -> [GlobalRegUse] -> CmmParse ()
doRawJump expr_code vols = do
  profile <- getProfile
  expr <- expr_code
  updfr_off <- getUpdFrameOff
  emit (mkRawJump profile expr updfr_off vols)

doJumpWithStack :: CmmParse CmmExpr -> [CmmParse CmmExpr]
                -> [CmmParse CmmExpr] -> CmmParse ()
doJumpWithStack expr_code stk_code args_code = do
  profile <- getProfile
  expr <- expr_code
  stk_args <- sequence stk_code
  args <- sequence args_code
  updfr_off <- getUpdFrameOff
  emit (mkJumpExtra profile NativeNodeCall expr args updfr_off stk_args)

doCall :: CmmParse CmmExpr -> [CmmParse LocalReg] -> [CmmParse CmmExpr]
       -> CmmParse ()
doCall expr_code res_code args_code = do
  expr <- expr_code
  args <- sequence args_code
  ress <- sequence res_code
  updfr_off <- getUpdFrameOff
  c <- code $ mkCall expr (NativeNodeCall,NativeReturn) ress args updfr_off []
  emit c

primCall
        :: [CmmParse (CmmFormal, ForeignHint)]
        -> FastString
        -> [CmmParse CmmExpr]
        -> PD (CmmParse ())
primCall results_code name args_code
  = do
    platform <- PD.getPlatform
    case lookupUFM (callishMachOps platform) name of
        Nothing -> failMsgPD $ \span -> mkPlainErrorMsgEnvelope span $ PsErrCmmParser (CmmUnknownPrimitive name)
        Just f  -> return $ do
                results <- sequence results_code
                args <- sequence args_code
                let (p, args') = f args
                code (emitPrimCall (map fst results) p args')

doStore :: Maybe MemoryOrdering
        -> CmmType
        -> CmmParse CmmExpr   -- ^ address
        -> CmmParse CmmExpr   -- ^ value
        -> CmmParse ()
doStore mem_ord rep addr_code val_code
  = do platform <- getPlatform
       addr <- addr_code
       val <- val_code
        -- if the specified store type does not match the type of the expr
        -- on the rhs, then we insert a coercion that will cause the type
        -- mismatch to be flagged by cmm-lint.  If we don't do this, then
        -- the store will happen at the wrong type, and the error will not
        -- be noticed.
       let val_width = typeWidth (cmmExprType platform val)
           rep_width = typeWidth rep
       let coerce_val
                | val_width /= rep_width = CmmMachOp (MO_UU_Conv val_width rep_width) (TupleG1 val)
                | otherwise              = val
       emitStore mem_ord addr coerce_val

-- -----------------------------------------------------------------------------
-- If-then-else and boolean expressions

data BoolExpr
  = BoolExpr `BoolAnd` BoolExpr
  | BoolExpr `BoolOr`  BoolExpr
  | BoolNot BoolExpr
  | BoolTest CmmExpr

-- ToDo: smart constructors which simplify the boolean expression.

cmmIfThenElse cond then_part else_part likely = do
     then_id <- newBlockId
     join_id <- newBlockId
     c <- cond
     emitCond c then_id likely
     else_part
     emit (mkBranch join_id)
     emitLabel then_id
     then_part
     -- fall through to join
     emitLabel join_id

cmmRawIf cond then_id likely = do
    c <- cond
    emitCond c then_id likely

-- 'emitCond cond true_id'  emits code to test whether the cond is true,
-- branching to true_id if so, and falling through otherwise.
emitCond (BoolTest e) then_id likely = do
  else_id <- newBlockId
  emit (mkCbranch e then_id else_id likely)
  emitLabel else_id
emitCond (BoolNot (BoolTest (CmmMachOp op args))) then_id likely
  | Just op' <- maybeInvertComparison op
  = emitCond (BoolTest (CmmMachOp op' args)) then_id (not <$> likely)
emitCond (BoolNot e) then_id likely = do
  else_id <- newBlockId
  emitCond e else_id likely
  emit (mkBranch then_id)
  emitLabel else_id
emitCond (e1 `BoolOr` e2) then_id likely = do
  emitCond e1 then_id likely
  emitCond e2 then_id likely
emitCond (e1 `BoolAnd` e2) then_id likely = do
        -- we'd like to invert one of the conditionals here to avoid an
        -- extra branch instruction, but we can't use maybeInvertComparison
        -- here because we can't look too closely at the expression since
        -- we're in a loop.
  and_id <- newBlockId
  else_id <- newBlockId
  emitCond e1 and_id likely
  emit (mkBranch else_id)
  emitLabel and_id
  emitCond e2 then_id likely
  emitLabel else_id

-- -----------------------------------------------------------------------------
-- Source code notes

-- | Generate a source note spanning from "a" to "b" (inclusive), then
-- proceed with parsing. This allows debugging tools to reason about
-- locations in Cmm code.
withSourceNote :: Located a -> Located b -> CmmParse c -> CmmParse c
withSourceNote a b parse = do
  name <- getName
  case combineSrcSpans (getLoc a) (getLoc b) of
    RealSrcSpan span _ -> code (emitTick (SourceNote span $ LexicalFastString $ mkFastString name)) >> parse
    _other           -> parse

-- -----------------------------------------------------------------------------
-- Table jumps

-- We use a simplified form of C-- switch statements for now.  A
-- switch statement always compiles to a table jump.  Each arm can
-- specify a list of values (not ranges), and there can be a single
-- default branch.  The range of the table is given either by the
-- optional range on the switch (eg. switch [0..7] {...}), or by
-- the minimum/maximum values from the branches.

doSwitch :: Maybe (Integer,Integer)
         -> CmmParse CmmExpr
         -> [([Integer],Either BlockId (CmmParse ()))]
         -> Maybe (CmmParse ()) -> CmmParse ()
doSwitch mb_range scrut arms deflt
   = do
        -- Compile code for the default branch
        dflt_entry <-
                case deflt of
                  Nothing -> return Nothing
                  Just e  -> do b <- forkLabelledCode e; return (Just b)

        -- Compile each case branch
        table_entries <- mapM emitArm arms
        let table = M.fromList (concat table_entries)

        platform <- getPlatform
        let range = fromMaybe (0, platformMaxWord platform) mb_range

        expr <- scrut
        -- ToDo: check for out of range and jump to default if necessary
        emit $ mkSwitch expr (mkSwitchTargets False range dflt_entry table)
   where
        emitArm :: ([Integer],Either BlockId (CmmParse ())) -> CmmParse [(Integer,BlockId)]
        emitArm (ints,Left blockid) = return [ (i,blockid) | i <- ints ]
        emitArm (ints,Right code) = do
           blockid <- forkLabelledCode code
           return [ (i,blockid) | i <- ints ]

forkLabelledCode :: CmmParse () -> CmmParse BlockId
forkLabelledCode p = do
  (_,ag) <- getCodeScoped p
  l <- newBlockId
  emitOutOfLine l ag
  return l

-- -----------------------------------------------------------------------------
-- Putting it all together

-- The initial environment: we define some constants that the compiler
-- knows about here.
initEnv :: Profile -> Env
initEnv profile = listToUFM [
  ( fsLit "SIZEOF_StgHeader",
    VarN (CmmLit (CmmInt (fromIntegral (fixedHdrSize profile)) (wordWidth platform)) )),
  ( fsLit "SIZEOF_StgInfoTable",
    VarN (CmmLit (CmmInt (fromIntegral (stdInfoTableSizeB profile)) (wordWidth platform)) ))
  ]
  where platform = profilePlatform profile

parseCmmFile :: CmmParserConfig
             -> Module
             -> HomeUnit
             -> FilePath
             -> IO (Messages PsMessage, Messages PsMessage, Maybe (DCmmGroup, [InfoProvEnt]))
parseCmmFile cmmpConfig this_mod home_unit filename = do
  buf <- hGetStringBuffer filename
  let
        init_loc = mkRealSrcLoc (mkFastString filename) 1 1
        init_state = (initParserState (cmmpParserOpts cmmpConfig) buf init_loc) { lex_state = [0] }
                -- reset the lex_state: the Lexer monad leaves some stuff
                -- in there we don't want.
        pdConfig = cmmpPDConfig cmmpConfig
  case unPD cmmParse pdConfig home_unit init_state of
    PFailed pst -> do
        let (warnings,errors) = getPsMessages pst
        return (warnings, errors, Nothing)
    POk pst code -> do
        st <- initC
        let fstate = F.initFCodeState (profilePlatform $ pdProfile pdConfig)
        let fcode = do
              ((), cmm) <- getCmm $ unEC code "global" (initEnv (pdProfile pdConfig)) [] >> return ()
              -- See Note [Mapping Info Tables to Source Positions] (IPE Maps)
              let used_info
                    | do_ipe    = map (cmmInfoTableToInfoProvEnt this_mod) (mapMaybe topInfoTableD cmm)
                    | otherwise = []
                    where
                      do_ipe = stgToCmmInfoTableMap $ cmmpStgToCmmConfig cmmpConfig
              -- We need to pass a deterministic unique supply to generate IPE
              -- symbols deterministically. The symbols created by
              -- emitIpeBufferListNode must all be local to the object (see
              -- comment on its definition). If the symbols weren't local, using a
              -- counter starting from zero for every Cmm file would cause
              -- conflicts when compiling more than one Cmm file together.
              (_, cmm2) <- getCmm $ emitIpeBufferListNode this_mod used_info (initDUniqSupply 'P' 0)
              return (cmm ++ cmm2, used_info)
            (cmm, _) = runC (cmmpStgToCmmConfig cmmpConfig) fstate st fcode
            (warnings,errors) = getPsMessages pst
        if not (isEmptyMessages errors)
         then return (warnings, errors, Nothing)
         else return (warnings, errors, Just cmm)

}
