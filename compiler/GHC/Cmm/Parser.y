-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2004-2012
--
-- Parser for concrete Cmm.
--
-----------------------------------------------------------------------------

{- -----------------------------------------------------------------------------
Note [Syntax of .cmm files]

NOTE: You are very much on your own in .cmm.  There is very little
error checking at all:

  * Type errors are detected by the (optional) -dcmm-lint pass, if you
    don't turn this on then a type error will likely result in a panic
    from the native code generator.

  * Passing the wrong number of arguments or arguments of the wrong
    type is not detected.

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


----------------------------------------------------------------------------- -}

{
{-# LANGUAGE TupleSections #-}

module GHC.Cmm.Parser ( parseCmmFile ) where

import GhcPrelude

import GHC.StgToCmm.ExtCode
import GHC.Cmm.CallConv
import GHC.StgToCmm.Prof
import GHC.StgToCmm.Heap
import GHC.StgToCmm.Monad hiding ( getCode, getCodeR, getCodeScoped, emitLabel, emit
                               , emitStore, emitAssign, emitOutOfLine, withUpdFrameOff
                               , getUpdFrameOff )
import qualified GHC.StgToCmm.Monad as F
import GHC.StgToCmm.Utils
import GHC.StgToCmm.Foreign
import GHC.StgToCmm.Expr
import GHC.StgToCmm.Closure
import GHC.StgToCmm.Layout     hiding (ArgRep(..))
import GHC.StgToCmm.Ticky
import GHC.StgToCmm.Bind  ( emitBlackHoleCode, emitUpdateFrame )
import GHC.Core           ( Tickish(SourceNote) )

import GHC.Cmm.Opt
import GHC.Cmm.Graph
import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Cmm.Switch     ( mkSwitchTargets )
import GHC.Cmm.Info
import GHC.Cmm.BlockId
import GHC.Cmm.Lexer
import GHC.Cmm.CLabel
import GHC.Cmm.Monad
import GHC.Runtime.Heap.Layout
import Lexer

import GHC.Types.CostCentre
import GHC.Types.ForeignCall
import GHC.Types.Module
import GHC.Platform
import GHC.Types.Literal
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.SrcLoc
import GHC.Driver.Session
import ErrUtils
import StringBuffer
import FastString
import Panic
import Constants
import Outputable
import GHC.Types.Basic
import Bag              ( emptyBag, unitBag )
import GHC.Types.Var

import Control.Monad
import Data.Array
import Data.Char        ( ord )
import System.Exit
import Data.Maybe
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS8

#include "HsVersions.h"
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
        'bits128'       { L _ (CmmT_bits128) }
        'bits256'       { L _ (CmmT_bits256) }
        'bits512'       { L _ (CmmT_bits512) }
        'float32'       { L _ (CmmT_float32) }
        'float64'       { L _ (CmmT_float64) }
        'gcptr'         { L _ (CmmT_gcptr) }

        GLOBALREG       { L _ (CmmT_GlobalReg   $$) }
        NAME            { L _ (CmmT_Name        $$) }
        STRING          { L _ (CmmT_String      $$) }
        INT             { L _ (CmmT_Int         $$) }
        FLOAT           { L _ (CmmT_Float       $$) }

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
                {% liftP . withThisPackage $ \pkg ->
                   do lits <- sequence $6;
                      staticClosure pkg $3 $5 (map getLit lits) }

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
                {% liftP . withThisPackage $ \pkg ->
                   return (mkCmmDataLabel pkg $1) }

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
                ; dflags <- getDynFlags
                     ; return $ map CmmStaticLit $
                        mkStaticClosure dflags (mkForeignLabel $3 Nothing ForeignLabelInExternalPackage IsData)
                         -- mkForeignLabel because these are only used
                         -- for CHARLIKE and INTLIKE closures in the RTS.
                        dontCareCCS (map getLit lits) [] [] [] } }
        -- arrays of closures required for the CHARLIKE & INTLIKE arrays

lits    :: { [CmmParse CmmExpr] }
        : {- empty -}           { [] }
        | ',' expr lits         { $2 : $3 }

cmmproc :: { CmmParse () }
        : info maybe_conv maybe_formals maybe_body
                { do ((entry_ret_label, info, stk_formals, formals), agraph) <-
                       getCodeScoped $ loopDecls $ do {
                         (entry_ret_label, info, stk_formals) <- $1;
                         dflags <- getDynFlags;
                         formals <- sequence (fromMaybe [] $3);
                         withName (showSDoc dflags (ppr entry_ret_label))
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
                {% liftP . withThisPackage $ \pkg ->
                   do   newFunctionName $1 pkg
                        return (mkCmmCodeLabel pkg $1, Nothing, []) }


        | 'INFO_TABLE' '(' NAME ',' INT ',' INT ',' INT ',' STRING ',' STRING ')'
                -- ptrs, nptrs, closure type, description, type
                {% liftP . withThisPackage $ \pkg ->
                   do dflags <- getDynFlags
                      let prof = profilingInfo dflags $11 $13
                          rep  = mkRTSRep (fromIntegral $9) $
                                   mkHeapRep dflags False (fromIntegral $5)
                                                   (fromIntegral $7) Thunk
                              -- not really Thunk, but that makes the info table
                              -- we want.
                      return (mkCmmEntryLabel pkg $3,
                              Just $ CmmInfoTable { cit_lbl = mkCmmInfoLabel pkg $3
                                           , cit_rep = rep
                                           , cit_prof = prof, cit_srt = Nothing, cit_clo = Nothing },
                              []) }

        | 'INFO_TABLE_FUN' '(' NAME ',' INT ',' INT ',' INT ',' STRING ',' STRING ',' INT ')'
                -- ptrs, nptrs, closure type, description, type, fun type
                {% liftP . withThisPackage $ \pkg ->
                   do dflags <- getDynFlags
                      let prof = profilingInfo dflags $11 $13
                          ty   = Fun 0 (ArgSpec (fromIntegral $15))
                                -- Arity zero, arg_type $15
                          rep = mkRTSRep (fromIntegral $9) $
                                    mkHeapRep dflags False (fromIntegral $5)
                                                    (fromIntegral $7) ty
                      return (mkCmmEntryLabel pkg $3,
                              Just $ CmmInfoTable { cit_lbl = mkCmmInfoLabel pkg $3
                                           , cit_rep = rep
                                           , cit_prof = prof, cit_srt = Nothing, cit_clo = Nothing },
                              []) }
                -- we leave most of the fields zero here.  This is only used
                -- to generate the BCO info table in the RTS at the moment.

        | 'INFO_TABLE_CONSTR' '(' NAME ',' INT ',' INT ',' INT ',' INT ',' STRING ',' STRING ')'
                -- ptrs, nptrs, tag, closure type, description, type
                {% liftP . withThisPackage $ \pkg ->
                   do dflags <- getDynFlags
                      let prof = profilingInfo dflags $13 $15
                          ty  = Constr (fromIntegral $9)  -- Tag
                                       (BS8.pack $13)
                          rep = mkRTSRep (fromIntegral $11) $
                                  mkHeapRep dflags False (fromIntegral $5)
                                                  (fromIntegral $7) ty
                      return (mkCmmEntryLabel pkg $3,
                              Just $ CmmInfoTable { cit_lbl = mkCmmInfoLabel pkg $3
                                           , cit_rep = rep
                                           , cit_prof = prof, cit_srt = Nothing,cit_clo = Nothing },
                              []) }

                     -- If profiling is on, this string gets duplicated,
                     -- but that's the way the old code did it we can fix it some other time.

        | 'INFO_TABLE_SELECTOR' '(' NAME ',' INT ',' INT ',' STRING ',' STRING ')'
                -- selector, closure type, description, type
                {% liftP . withThisPackage $ \pkg ->
                   do dflags <- getDynFlags
                      let prof = profilingInfo dflags $9 $11
                          ty  = ThunkSelector (fromIntegral $5)
                          rep = mkRTSRep (fromIntegral $7) $
                                   mkHeapRep dflags False 0 0 ty
                      return (mkCmmEntryLabel pkg $3,
                              Just $ CmmInfoTable { cit_lbl = mkCmmInfoLabel pkg $3
                                           , cit_rep = rep
                                           , cit_prof = prof, cit_srt = Nothing, cit_clo = Nothing },
                              []) }

        | 'INFO_TABLE_RET' '(' NAME ',' INT ')'
                -- closure type (no live regs)
                {% liftP . withThisPackage $ \pkg ->
                   do let prof = NoProfilingInfo
                          rep  = mkRTSRep (fromIntegral $5) $ mkStackRep []
                      return (mkCmmRetLabel pkg $3,
                              Just $ CmmInfoTable { cit_lbl = mkCmmRetInfoLabel pkg $3
                                           , cit_rep = rep
                                           , cit_prof = prof, cit_srt = Nothing, cit_clo = Nothing },
                              []) }

        | 'INFO_TABLE_RET' '(' NAME ',' INT ',' formals0 ')'
                -- closure type, live regs
                {% liftP . withThisPackage $ \pkg ->
                   do dflags <- getDynFlags
                      let platform = targetPlatform dflags
                      live <- sequence $7
                      let prof = NoProfilingInfo
                          -- drop one for the info pointer
                          bitmap = mkLiveness platform (drop 1 live)
                          rep  = mkRTSRep (fromIntegral $5) $ mkStackRep bitmap
                      return (mkCmmRetLabel pkg $3,
                              Just $ CmmInfoTable { cit_lbl = mkCmmRetInfoLabel pkg $3
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
        { ($1, mkForeignLabel $1 Nothing ForeignLabelInExternalPackage IsFunction) }

        -- as previous 'NAME', but 'IsData'
        | 'CLOSURE' NAME
        { ($2, mkForeignLabel $2 Nothing ForeignLabelInExternalPackage IsData) }

        -- A label imported with an explicit packageId.
        | STRING NAME
        { ($2, mkCmmCodeLabel (fsToUnitId (mkFastString $1)) $2) }


names   :: { [FastString] }
        : NAME                          { [$1] }
        | NAME ',' names                { $1 : $3 }

stmt    :: { CmmParse () }
        : ';'                                   { return () }

        | NAME ':'
                { do l <- newLabel $1; emitLabel l }



        | lreg '=' expr ';'
                { do reg <- $1; e <- $3; withSourceNote $2 $4 (emitAssign reg e) }
        | type '[' expr ']' '=' expr ';'
                { withSourceNote $2 $7 (doStore $1 $3 $6) }

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
                { do e <- $3; rest <- $5; return (($1, e) : rest) }
        | GLOBALREG '=' expr_or_unknown
                { do e <- $3; return [($1, e)] }

-- | Used by unwind to indicate unknown unwinding values.
expr_or_unknown
        :: { CmmParse (Maybe CmmExpr) }
        : 'return'
                { do return Nothing }
        | expr
                { do e <- $1; return (Just e) }

foreignLabel     :: { CmmParse CmmExpr }
        : NAME                          { return (CmmLit (CmmLabel (mkForeignLabel $1 Nothing ForeignLabelInThisPackage IsFunction))) }

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

vols    :: { [GlobalReg] }
        : '[' ']'                       { [] }
        | '[' '*' ']'                   {% do df <- getDynFlags
                                         ; return (realArgRegsCover df) }
                                           -- All of them. See comment attached
                                           -- to realArgRegsCover
        | '[' globals ']'               { $2 }

globals :: { [GlobalReg] }
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
        : expr '/' expr                 { mkMachOp MO_U_Quot [$1,$3] }
        | expr '*' expr                 { mkMachOp MO_Mul [$1,$3] }
        | expr '%' expr                 { mkMachOp MO_U_Rem [$1,$3] }
        | expr '-' expr                 { mkMachOp MO_Sub [$1,$3] }
        | expr '+' expr                 { mkMachOp MO_Add [$1,$3] }
        | expr '>>' expr                { mkMachOp MO_U_Shr [$1,$3] }
        | expr '<<' expr                { mkMachOp MO_Shl [$1,$3] }
        | expr '&' expr                 { mkMachOp MO_And [$1,$3] }
        | expr '^' expr                 { mkMachOp MO_Xor [$1,$3] }
        | expr '|' expr                 { mkMachOp MO_Or [$1,$3] }
        | expr '>=' expr                { mkMachOp MO_U_Ge [$1,$3] }
        | expr '>' expr                 { mkMachOp MO_U_Gt [$1,$3] }
        | expr '<=' expr                { mkMachOp MO_U_Le [$1,$3] }
        | expr '<' expr                 { mkMachOp MO_U_Lt [$1,$3] }
        | expr '!=' expr                { mkMachOp MO_Ne [$1,$3] }
        | expr '==' expr                { mkMachOp MO_Eq [$1,$3] }
        | '~' expr                      { mkMachOp MO_Not [$2] }
        | '-' expr                      { mkMachOp MO_S_Neg [$2] }
        | expr0 '`' NAME '`' expr0      {% do { mo <- nameToMachOp $3 ;
                                                return (mkMachOp mo [$1,$5]) } }
        | expr0                         { $1 }

expr0   :: { CmmParse CmmExpr }
        : INT   maybe_ty         { return (CmmLit (CmmInt $1 (typeWidth $2))) }
        | FLOAT maybe_ty         { return (CmmLit (CmmFloat $1 (typeWidth $2))) }
        | STRING                 { do s <- code (newStringCLit $1);
                                      return (CmmLit s) }
        | reg                    { $1 }
        | type '[' expr ']'      { do e <- $3; return (CmmLoad e $1) }
        | '%' NAME '(' exprs0 ')' {% exprOp $2 $4 }
        | '(' expr ')'           { $2 }


-- leaving out the type of a literal gives you the native word size in C--
maybe_ty :: { CmmType }
        : {- empty -}                   {% do dflags <- getDynFlags; return $ bWord (targetPlatform dflags) }
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
        | 'bits128'             { b128 }
        | 'bits256'             { b256 }
        | 'bits512'             { b512 }
        | 'float32'             { f32 }
        | 'float64'             { f64 }
        | 'gcptr'               {% do dflags <- getDynFlags; return $ gcWord (targetPlatform dflags) }

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
mkMachOp :: (Width -> MachOp) -> [CmmParse CmmExpr] -> CmmParse CmmExpr
mkMachOp fn args = do
  dflags <- getDynFlags
  let platform = targetPlatform dflags
  arg_exprs <- sequence args
  return (CmmMachOp (fn (typeWidth (cmmExprType platform (head arg_exprs)))) arg_exprs)

getLit :: CmmExpr -> CmmLit
getLit (CmmLit l) = l
getLit (CmmMachOp (MO_S_Neg _) [CmmLit (CmmInt i r)])  = CmmInt (negate i) r
getLit _ = panic "invalid literal" -- TODO messy failure

nameToMachOp :: FastString -> PD (Width -> MachOp)
nameToMachOp name =
  case lookupUFM machOps name of
        Nothing -> failMsgPD ("unknown primitive " ++ unpackFS name)
        Just m  -> return m

exprOp :: FastString -> [CmmParse CmmExpr] -> PD (CmmParse CmmExpr)
exprOp name args_code = do
  dflags <- getDynFlags
  case lookupUFM (exprMacros dflags) name of
     Just f  -> return $ do
        args <- sequence args_code
        return (f args)
     Nothing -> do
        mo <- nameToMachOp name
        return $ mkMachOp mo args_code

exprMacros :: DynFlags -> UniqFM ([CmmExpr] -> CmmExpr)
exprMacros dflags = listToUFM [
  ( fsLit "ENTRY_CODE",   \ [x] -> entryCode dflags x ),
  ( fsLit "INFO_PTR",     \ [x] -> closureInfoPtr dflags x ),
  ( fsLit "STD_INFO",     \ [x] -> infoTable dflags x ),
  ( fsLit "FUN_INFO",     \ [x] -> funInfoTable dflags x ),
  ( fsLit "GET_ENTRY",    \ [x] -> entryCode dflags (closureInfoPtr dflags x) ),
  ( fsLit "GET_STD_INFO", \ [x] -> infoTable dflags (closureInfoPtr dflags x) ),
  ( fsLit "GET_FUN_INFO", \ [x] -> funInfoTable dflags (closureInfoPtr dflags x) ),
  ( fsLit "INFO_TYPE",    \ [x] -> infoTableClosureType dflags x ),
  ( fsLit "INFO_PTRS",    \ [x] -> infoTablePtrs dflags x ),
  ( fsLit "INFO_NPTRS",   \ [x] -> infoTableNonPtrs dflags x )
  ]

-- we understand a subset of C-- primitives:
machOps = listToUFM $
        map (\(x, y) -> (mkFastString x, y)) [
        ( "add",        MO_Add ),
        ( "sub",        MO_Sub ),
        ( "eq",         MO_Eq ),
        ( "ne",         MO_Ne ),
        ( "mul",        MO_Mul ),
        ( "neg",        MO_S_Neg ),
        ( "quot",       MO_S_Quot ),
        ( "rem",        MO_S_Rem ),
        ( "divu",       MO_U_Quot ),
        ( "modu",       MO_U_Rem ),

        ( "ge",         MO_S_Ge ),
        ( "le",         MO_S_Le ),
        ( "gt",         MO_S_Gt ),
        ( "lt",         MO_S_Lt ),

        ( "geu",        MO_U_Ge ),
        ( "leu",        MO_U_Le ),
        ( "gtu",        MO_U_Gt ),
        ( "ltu",        MO_U_Lt ),

        ( "and",        MO_And ),
        ( "or",         MO_Or ),
        ( "xor",        MO_Xor ),
        ( "com",        MO_Not ),
        ( "shl",        MO_Shl ),
        ( "shrl",       MO_U_Shr ),
        ( "shra",       MO_S_Shr ),

        ( "fadd",       MO_F_Add ),
        ( "fsub",       MO_F_Sub ),
        ( "fneg",       MO_F_Neg ),
        ( "fmul",       MO_F_Mul ),
        ( "fquot",      MO_F_Quot ),

        ( "feq",        MO_F_Eq ),
        ( "fne",        MO_F_Ne ),
        ( "fge",        MO_F_Ge ),
        ( "fle",        MO_F_Le ),
        ( "fgt",        MO_F_Gt ),
        ( "flt",        MO_F_Lt ),

        ( "lobits8",  flip MO_UU_Conv W8  ),
        ( "lobits16", flip MO_UU_Conv W16 ),
        ( "lobits32", flip MO_UU_Conv W32 ),
        ( "lobits64", flip MO_UU_Conv W64 ),

        ( "zx16",     flip MO_UU_Conv W16 ),
        ( "zx32",     flip MO_UU_Conv W32 ),
        ( "zx64",     flip MO_UU_Conv W64 ),

        ( "sx16",     flip MO_SS_Conv W16 ),
        ( "sx32",     flip MO_SS_Conv W32 ),
        ( "sx64",     flip MO_SS_Conv W64 ),

        ( "f2f32",    flip MO_FF_Conv W32 ),  -- TODO; rounding mode
        ( "f2f64",    flip MO_FF_Conv W64 ),  -- TODO; rounding mode
        ( "f2i8",     flip MO_FS_Conv W8 ),
        ( "f2i16",    flip MO_FS_Conv W16 ),
        ( "f2i32",    flip MO_FS_Conv W32 ),
        ( "f2i64",    flip MO_FS_Conv W64 ),
        ( "i2f32",    flip MO_SF_Conv W32 ),
        ( "i2f64",    flip MO_SF_Conv W64 )
        ]

callishMachOps :: UniqFM ([CmmExpr] -> (CallishMachOp, [CmmExpr]))
callishMachOps = listToUFM $
        map (\(x, y) -> (mkFastString x, y)) [
        ( "read_barrier", (MO_ReadBarrier,)),
        ( "write_barrier", (MO_WriteBarrier,)),
        ( "memcpy", memcpyLikeTweakArgs MO_Memcpy ),
        ( "memset", memcpyLikeTweakArgs MO_Memset ),
        ( "memmove", memcpyLikeTweakArgs MO_Memmove ),
        ( "memcmp", memcpyLikeTweakArgs MO_Memcmp ),

        ("prefetch0", (MO_Prefetch_Data 0,)),
        ("prefetch1", (MO_Prefetch_Data 1,)),
        ("prefetch2", (MO_Prefetch_Data 2,)),
        ("prefetch3", (MO_Prefetch_Data 3,)),

        ( "popcnt8",  (MO_PopCnt W8,)),
        ( "popcnt16", (MO_PopCnt W16,)),
        ( "popcnt32", (MO_PopCnt W32,)),
        ( "popcnt64", (MO_PopCnt W64,)),

        ( "pdep8",  (MO_Pdep W8,)),
        ( "pdep16", (MO_Pdep W16,)),
        ( "pdep32", (MO_Pdep W32,)),
        ( "pdep64", (MO_Pdep W64,)),

        ( "pext8",  (MO_Pext W8,)),
        ( "pext16", (MO_Pext W16,)),
        ( "pext32", (MO_Pext W32,)),
        ( "pext64", (MO_Pext W64,)),

        ( "cmpxchg8",  (MO_Cmpxchg W8,)),
        ( "cmpxchg16", (MO_Cmpxchg W16,)),
        ( "cmpxchg32", (MO_Cmpxchg W32,)),
        ( "cmpxchg64", (MO_Cmpxchg W64,))

        -- ToDo: the rest, maybe
        -- edit: which rest?
        -- also: how do we tell CMM Lint how to type check callish macops?
    ]
  where
    memcpyLikeTweakArgs :: (Int -> CallishMachOp) -> [CmmExpr] -> (CallishMachOp, [CmmExpr])
    memcpyLikeTweakArgs op [] = pgmError "memcpy-like function requires at least one argument"
    memcpyLikeTweakArgs op args@(_:_) =
        (op align, args')
      where
        args' = init args
        align = case last args of
          CmmLit (CmmInt alignInteger _) -> fromInteger alignInteger
          e -> pprPgmError "Non-constant alignment in memcpy-like function:" (ppr e)
        -- The alignment of memcpy-ish operations must be a
        -- compile-time constant. We verify this here, passing it around
        -- in the MO_* constructor. In order to do this, however, we
        -- must intercept the arguments in primCall.

parseSafety :: String -> PD Safety
parseSafety "safe"   = return PlaySafe
parseSafety "unsafe" = return PlayRisky
parseSafety "interruptible" = return PlayInterruptible
parseSafety str      = failMsgPD ("unrecognised safety: " ++ str)

parseCmmHint :: String -> PD ForeignHint
parseCmmHint "ptr"    = return AddrHint
parseCmmHint "signed" = return SignedHint
parseCmmHint str      = failMsgPD ("unrecognised hint: " ++ str)

-- labels are always pointers, so we might as well infer the hint
inferCmmHint :: CmmExpr -> ForeignHint
inferCmmHint (CmmLit (CmmLabel _)) = AddrHint
inferCmmHint (CmmReg (CmmGlobal g)) | isPtrGlobalReg g = AddrHint
inferCmmHint _ = NoHint

isPtrGlobalReg Sp                    = True
isPtrGlobalReg SpLim                 = True
isPtrGlobalReg Hp                    = True
isPtrGlobalReg HpLim                 = True
isPtrGlobalReg CCCS                  = True
isPtrGlobalReg CurrentTSO            = True
isPtrGlobalReg CurrentNursery        = True
isPtrGlobalReg (VanillaReg _ VGcPtr) = True
isPtrGlobalReg _                     = False

happyError :: PD a
happyError = PD $ \_ s -> unP srcParseFail s

-- -----------------------------------------------------------------------------
-- Statement-level macros

stmtMacro :: FastString -> [CmmParse CmmExpr] -> PD (CmmParse ())
stmtMacro fun args_code = do
  case lookupUFM stmtMacros fun of
    Nothing -> failMsgPD ("unknown macro: " ++ unpackFS fun)
    Just fcode -> return $ do
        args <- sequence args_code
        code (fcode args)

stmtMacros :: UniqFM ([CmmExpr] -> FCode ())
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

  ( fsLit "LDV_ENTER",             \[e] -> ldvEnter e ),
  ( fsLit "LDV_RECORD_CREATE",     \[e] -> ldvRecordCreate e ),

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
  dflags <- getDynFlags
  emitUpdateFrame dflags sp mkUpdInfoLabel e

pushStackFrame :: [CmmParse CmmExpr] -> CmmParse () -> CmmParse ()
pushStackFrame fields body = do
  dflags <- getDynFlags
  exprs <- sequence fields
  updfr_off <- getUpdFrameOff
  let (new_updfr_off, _, g) = copyOutOflow dflags NativeReturn Ret Old
                                           [] updfr_off exprs
  emit g
  withUpdFrameOff new_updfr_off body

reserveStackFrame
  :: CmmParse CmmExpr
  -> CmmParse CmmReg
  -> CmmParse ()
  -> CmmParse ()
reserveStackFrame psize preg body = do
  dflags <- getDynFlags
  let platform = targetPlatform dflags
  old_updfr_off <- getUpdFrameOff
  reg <- preg
  esize <- psize
  let size = case constantFoldExpr platform esize of
               CmmLit (CmmInt n _) -> n
               _other -> pprPanic "CmmParse: not a compile-time integer: "
                            (ppr esize)
  let frame = old_updfr_off + platformWordSizeInBytes platform * fromIntegral size
  emitAssign reg (CmmStackSlot Old frame)
  withUpdFrameOff frame body

profilingInfo dflags desc_str ty_str
  = if not (gopt Opt_SccProfilingOn dflags)
    then NoProfilingInfo
    else ProfilingInfo (BS8.pack desc_str) (BS8.pack ty_str)

staticClosure :: UnitId -> FastString -> FastString -> [CmmLit] -> CmmParse ()
staticClosure pkg cl_label info payload
  = do dflags <- getDynFlags
       let lits = mkStaticClosure dflags (mkCmmInfoLabel pkg info) dontCareCCS payload [] [] []
       code $ emitDataLits (mkCmmDataLabel pkg cl_label) lits

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
          "C" -> return CCallConv
          "stdcall" -> return StdCallConv
          _ -> failMsgPD ("unknown calling convention: " ++ conv_string)
        return $ do
          dflags <- getDynFlags
          results <- sequence results_code
          expr <- expr_code
          args <- sequence args_code
          let
                  platform = targetPlatform dflags
                  expr' = adjCallTarget platform conv expr args
                  (arg_exprs, arg_hints) = unzip args
                  (res_regs,  res_hints) = unzip results
                  fc = ForeignConvention conv arg_hints res_hints ret
                  target = ForeignTarget expr' fc
          _ <- code $ emitForeignCall safety res_regs target arg_exprs
          return ()


doReturn :: [CmmParse CmmExpr] -> CmmParse ()
doReturn exprs_code = do
  dflags <- getDynFlags
  exprs <- sequence exprs_code
  updfr_off <- getUpdFrameOff
  emit (mkReturnSimple dflags exprs updfr_off)

mkReturnSimple  :: DynFlags -> [CmmActual] -> UpdFrameOffset -> CmmAGraph
mkReturnSimple dflags actuals updfr_off =
  mkReturn dflags e actuals updfr_off
  where e = entryCode dflags (CmmLoad (CmmStackSlot Old updfr_off)
                             (gcWord platform))
        platform = targetPlatform dflags

doRawJump :: CmmParse CmmExpr -> [GlobalReg] -> CmmParse ()
doRawJump expr_code vols = do
  dflags <- getDynFlags
  expr <- expr_code
  updfr_off <- getUpdFrameOff
  emit (mkRawJump dflags expr updfr_off vols)

doJumpWithStack :: CmmParse CmmExpr -> [CmmParse CmmExpr]
                -> [CmmParse CmmExpr] -> CmmParse ()
doJumpWithStack expr_code stk_code args_code = do
  dflags <- getDynFlags
  expr <- expr_code
  stk_args <- sequence stk_code
  args <- sequence args_code
  updfr_off <- getUpdFrameOff
  emit (mkJumpExtra dflags NativeNodeCall expr args updfr_off stk_args)

doCall :: CmmParse CmmExpr -> [CmmParse LocalReg] -> [CmmParse CmmExpr]
       -> CmmParse ()
doCall expr_code res_code args_code = do
  expr <- expr_code
  args <- sequence args_code
  ress <- sequence res_code
  updfr_off <- getUpdFrameOff
  c <- code $ mkCall expr (NativeNodeCall,NativeReturn) ress args updfr_off []
  emit c

adjCallTarget :: Platform -> CCallConv -> CmmExpr -> [(CmmExpr, ForeignHint) ]
              -> CmmExpr
-- On Windows, we have to add the '@N' suffix to the label when making
-- a call with the stdcall calling convention.
adjCallTarget platform StdCallConv (CmmLit (CmmLabel lbl)) args
 | platformOS platform == OSMinGW32
  = CmmLit (CmmLabel (addLabelSize lbl (sum (map size args))))
  where size (e, _) = max (platformWordSizeInBytes platform) (widthInBytes (typeWidth (cmmExprType platform e)))
                 -- c.f. CgForeignCall.emitForeignCall
adjCallTarget _ _ expr _
  = expr

primCall
        :: [CmmParse (CmmFormal, ForeignHint)]
        -> FastString
        -> [CmmParse CmmExpr]
        -> PD (CmmParse ())
primCall results_code name args_code
  = case lookupUFM callishMachOps name of
        Nothing -> failMsgPD ("unknown primitive " ++ unpackFS name)
        Just f  -> return $ do
                results <- sequence results_code
                args <- sequence args_code
                let (p, args') = f args
                code (emitPrimCall (map fst results) p args')

doStore :: CmmType -> CmmParse CmmExpr  -> CmmParse CmmExpr -> CmmParse ()
doStore rep addr_code val_code
  = do dflags <- getDynFlags
       addr <- addr_code
       val <- val_code
        -- if the specified store type does not match the type of the expr
        -- on the rhs, then we insert a coercion that will cause the type
        -- mismatch to be flagged by cmm-lint.  If we don't do this, then
        -- the store will happen at the wrong type, and the error will not
        -- be noticed.
       let val_width = typeWidth (cmmExprType platform val)
           rep_width = typeWidth rep
           platform  = targetPlatform dflags
       let coerce_val
                | val_width /= rep_width = CmmMachOp (MO_UU_Conv val_width rep_width) [val]
                | otherwise              = val
       emitStore addr coerce_val

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
    RealSrcSpan span _ -> code (emitTick (SourceNote span name)) >> parse
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

        dflags <- getDynFlags
        let platform = targetPlatform dflags
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
initEnv :: DynFlags -> Env
initEnv dflags = listToUFM [
  ( fsLit "SIZEOF_StgHeader",
    VarN (CmmLit (CmmInt (fromIntegral (fixedHdrSize dflags)) (wordWidth platform)) )),
  ( fsLit "SIZEOF_StgInfoTable",
    VarN (CmmLit (CmmInt (fromIntegral (stdInfoTableSizeB dflags)) (wordWidth platform)) ))
  ]
  where platform = targetPlatform dflags

parseCmmFile :: DynFlags -> FilePath -> IO (Messages, Maybe CmmGroup)
parseCmmFile dflags filename = withTiming dflags (text "ParseCmm"<+>brackets (text filename)) (\_ -> ()) $ do
  buf <- hGetStringBuffer filename
  let
        init_loc = mkRealSrcLoc (mkFastString filename) 1 1
        init_state = (mkPState dflags buf init_loc) { lex_state = [0] }
                -- reset the lex_state: the Lexer monad leaves some stuff
                -- in there we don't want.
  case unPD cmmParse dflags init_state of
    PFailed pst ->
        return (getMessages pst dflags, Nothing)
    POk pst code -> do
        st <- initC
        let fcode = getCmm $ unEC code "global" (initEnv dflags) [] >> return ()
            (cmm,_) = runC dflags no_module st fcode
        let ms = getMessages pst dflags
        if (errorsFound dflags ms)
         then return (ms, Nothing)
         else return (ms, Just cmm)
  where
        no_module = panic "parseCmmFile: no module"
}
