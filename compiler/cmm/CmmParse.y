-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2004-2006
--
-- Parser for concrete Cmm.
--
-----------------------------------------------------------------------------

{
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module CmmParse ( parseCmmFile ) where

import CgMonad
import CgHeapery
import CgUtils
import CgProf
import CgTicky
import CgInfoTbls
import CgForeignCall
import CgTailCall
import CgStackery
import ClosureInfo
import CgCallConv
import CgClosure
import CostCentre

import Cmm
import PprCmm
import CmmUtils
import CmmLex
import CLabel
import MachOp
import SMRep
import Lexer

import ForeignCall
import Literal
import Unique
import UniqFM
import SrcLoc
import DynFlags
import StaticFlags
import ErrUtils
import StringBuffer
import FastString
import Panic
import Constants
import Outputable

import Control.Monad
import Data.Char	( ord )
import System.Exit

#include "HsVersions.h"
}

%token
	':'	{ L _ (CmmT_SpecChar ':') }
	';'	{ L _ (CmmT_SpecChar ';') }
	'{'	{ L _ (CmmT_SpecChar '{') }
	'}'	{ L _ (CmmT_SpecChar '}') }
	'['	{ L _ (CmmT_SpecChar '[') }
	']'	{ L _ (CmmT_SpecChar ']') }
	'('	{ L _ (CmmT_SpecChar '(') }
	')'	{ L _ (CmmT_SpecChar ')') }
	'='	{ L _ (CmmT_SpecChar '=') }
	'`'	{ L _ (CmmT_SpecChar '`') }
	'~'	{ L _ (CmmT_SpecChar '~') }
	'/'	{ L _ (CmmT_SpecChar '/') }
	'*'	{ L _ (CmmT_SpecChar '*') }
	'%'	{ L _ (CmmT_SpecChar '%') }
	'-'	{ L _ (CmmT_SpecChar '-') }
	'+'	{ L _ (CmmT_SpecChar '+') }
	'&'	{ L _ (CmmT_SpecChar '&') }
	'^'	{ L _ (CmmT_SpecChar '^') }
	'|'	{ L _ (CmmT_SpecChar '|') }
	'>'	{ L _ (CmmT_SpecChar '>') }
	'<'	{ L _ (CmmT_SpecChar '<') }
	','	{ L _ (CmmT_SpecChar ',') }
	'!'	{ L _ (CmmT_SpecChar '!') }

 	'..'	{ L _ (CmmT_DotDot) }
 	'::'	{ L _ (CmmT_DoubleColon) }
	'>>'	{ L _ (CmmT_Shr) }
	'<<'	{ L _ (CmmT_Shl) }
	'>='	{ L _ (CmmT_Ge) }
	'<='	{ L _ (CmmT_Le) }
	'=='	{ L _ (CmmT_Eq) }
	'!='	{ L _ (CmmT_Ne) }
        '&&'    { L _ (CmmT_BoolAnd) }
        '||'    { L _ (CmmT_BoolOr) }

	'CLOSURE'	{ L _ (CmmT_CLOSURE) }
	'INFO_TABLE'	{ L _ (CmmT_INFO_TABLE) }
	'INFO_TABLE_RET'{ L _ (CmmT_INFO_TABLE_RET) }
	'INFO_TABLE_FUN'{ L _ (CmmT_INFO_TABLE_FUN) }
	'INFO_TABLE_CONSTR'{ L _ (CmmT_INFO_TABLE_CONSTR) }
	'INFO_TABLE_SELECTOR'{ L _ (CmmT_INFO_TABLE_SELECTOR) }
	'else'		{ L _ (CmmT_else) }
	'export'	{ L _ (CmmT_export) }
	'section'	{ L _ (CmmT_section) }
	'align'		{ L _ (CmmT_align) }
	'goto'		{ L _ (CmmT_goto) }
	'if'		{ L _ (CmmT_if) }
	'jump'		{ L _ (CmmT_jump) }
	'foreign'	{ L _ (CmmT_foreign) }
	'never'		{ L _ (CmmT_never) }
	'prim'		{ L _ (CmmT_prim) }
	'return'	{ L _ (CmmT_return) }
	'returns'	{ L _ (CmmT_returns) }
	'import'	{ L _ (CmmT_import) }
	'switch'	{ L _ (CmmT_switch) }
	'case'		{ L _ (CmmT_case) }
	'default'	{ L _ (CmmT_default) }
	'bits8'		{ L _ (CmmT_bits8) }
	'bits16'	{ L _ (CmmT_bits16) }
	'bits32'	{ L _ (CmmT_bits32) }
	'bits64'	{ L _ (CmmT_bits64) }
	'float32'	{ L _ (CmmT_float32) }
	'float64'	{ L _ (CmmT_float64) }

	GLOBALREG	{ L _ (CmmT_GlobalReg   $$) }
  	NAME		{ L _ (CmmT_Name	$$) }
	STRING		{ L _ (CmmT_String	$$) }
	INT		{ L _ (CmmT_Int		$$) }
	FLOAT		{ L _ (CmmT_Float	$$) }

%monad { P } { >>= } { return }
%lexer { cmmlex } { L _ CmmT_EOF }
%name cmmParse cmm
%tokentype { Located CmmToken }

-- C-- operator precedences, taken from the C-- spec
%right '||'	-- non-std extension, called %disjoin in C--
%right '&&'	-- non-std extension, called %conjoin in C--
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

cmm	:: { ExtCode }
	: {- empty -}			{ return () }
	| cmmtop cmm			{ do $1; $2 }

cmmtop	:: { ExtCode }
	: cmmproc			{ $1 }
	| cmmdata			{ $1 }
	| decl				{ $1 } 
	| 'CLOSURE' '(' NAME ',' NAME lits ')' ';'  
		{ do lits <- sequence $6;
		     staticClosure $3 $5 (map getLit lits) }

-- The only static closures in the RTS are dummy closures like
-- stg_END_TSO_QUEUE_closure and stg_dummy_ret.  We don't need
-- to provide the full generality of static closures here.
-- In particular:
-- 	* CCS can always be CCS_DONT_CARE
-- 	* closure is always extern
-- 	* payload is always empty
--	* we can derive closure and info table labels from a single NAME

cmmdata :: { ExtCode }
	: 'section' STRING '{' statics '}' 
		{ do ss <- sequence $4;
		     code (emitData (section $2) (concat ss)) }

statics	:: { [ExtFCode [CmmStatic]] }
	: {- empty -}			{ [] }
	| static statics		{ $1 : $2 }

-- Strings aren't used much in the RTS HC code, so it doesn't seem
-- worth allowing inline strings.  C-- doesn't allow them anyway.
static 	:: { ExtFCode [CmmStatic] }
	: NAME ':'	{ return [CmmDataLabel (mkRtsDataLabelFS $1)] }
	| type expr ';'	{ do e <- $2;
			     return [CmmStaticLit (getLit e)] }
	| type ';'			{ return [CmmUninitialised
							(machRepByteWidth $1)] }
        | 'bits8' '[' ']' STRING ';'	{ return [mkString $4] }
        | 'bits8' '[' INT ']' ';'	{ return [CmmUninitialised 
							(fromIntegral $3)] }
        | typenot8 '[' INT ']' ';'	{ return [CmmUninitialised 
						(machRepByteWidth $1 * 
							fromIntegral $3)] }
	| 'align' INT ';'		{ return [CmmAlign (fromIntegral $2)] }
	| 'CLOSURE' '(' NAME lits ')'
		{ do lits <- sequence $4;
		     return $ map CmmStaticLit $
		       mkStaticClosure (mkRtsInfoLabelFS $3) 
			 dontCareCCS (map getLit lits) [] [] [] }
	-- arrays of closures required for the CHARLIKE & INTLIKE arrays

lits	:: { [ExtFCode CmmExpr] }
	: {- empty -}		{ [] }
	| ',' expr lits		{ $2 : $3 }

cmmproc :: { ExtCode }
-- TODO: add real SRT/info tables to parsed Cmm
	: info maybe_formals_without_kinds maybe_gc_block maybe_frame '{' body '}'
		{ do ((entry_ret_label, info, live, formals, gc_block, frame), stmts) <-
		       getCgStmtsEC' $ loopDecls $ do {
		         (entry_ret_label, info, live) <- $1;
		         formals <- sequence $2;
		         gc_block <- $3;
		         frame <- $4;
		         $6;
		         return (entry_ret_label, info, live, formals, gc_block, frame) }
		     blks <- code (cgStmtsToBlocks stmts)
		     code (emitInfoTableAndCode entry_ret_label (CmmInfo gc_block frame info) formals blks) }

	| info maybe_formals_without_kinds ';'
		{ do (entry_ret_label, info, live) <- $1;
		     formals <- sequence $2;
		     code (emitInfoTableAndCode entry_ret_label (CmmInfo Nothing Nothing info) formals []) }

	| NAME maybe_formals_without_kinds maybe_gc_block maybe_frame '{' body '}'
		{ do ((formals, gc_block, frame), stmts) <-
			getCgStmtsEC' $ loopDecls $ do {
		          formals <- sequence $2;
		          gc_block <- $3;
			  frame <- $4;
		          $6;
		          return (formals, gc_block, frame) }
                     blks <- code (cgStmtsToBlocks stmts)
		     code (emitProc (CmmInfo gc_block frame CmmNonInfoTable) (mkRtsCodeLabelFS $1) formals blks) }

info	:: { ExtFCode (CLabel, CmmInfoTable, [Maybe LocalReg]) }
	: 'INFO_TABLE' '(' NAME ',' INT ',' INT ',' INT ',' STRING ',' STRING ')'
		-- ptrs, nptrs, closure type, description, type
		{ do prof <- profilingInfo $11 $13
		     return (mkRtsEntryLabelFS $3,
			CmmInfoTable prof (fromIntegral $9)
				     (ThunkInfo (fromIntegral $5, fromIntegral $7) NoC_SRT),
			[]) }
	
	| 'INFO_TABLE_FUN' '(' NAME ',' INT ',' INT ',' INT ',' STRING ',' STRING ',' INT ')'
		-- ptrs, nptrs, closure type, description, type, fun type
		{ do prof <- profilingInfo $11 $13
		     return (mkRtsEntryLabelFS $3,
			CmmInfoTable prof (fromIntegral $9)
				     (FunInfo (fromIntegral $5, fromIntegral $7) NoC_SRT (fromIntegral $15) 0
				      (ArgSpec 0)
				      zeroCLit),
			[]) }
		-- we leave most of the fields zero here.  This is only used
		-- to generate the BCO info table in the RTS at the moment.

	-- A variant with a non-zero arity (needed to write Main_main in Cmm)
	| 'INFO_TABLE_FUN' '(' NAME ',' INT ',' INT ',' INT ',' STRING ',' STRING ',' INT ',' INT ')'
		-- ptrs, nptrs, closure type, description, type, fun type, arity
		{ do prof <- profilingInfo $11 $13
		     return (mkRtsEntryLabelFS $3,
			CmmInfoTable prof (fromIntegral $9)
				     (FunInfo (fromIntegral $5, fromIntegral $7) NoC_SRT (fromIntegral $15) (fromIntegral $17)
				      (ArgSpec 0)
				      zeroCLit),
			[]) }
		-- we leave most of the fields zero here.  This is only used
		-- to generate the BCO info table in the RTS at the moment.
	
	| 'INFO_TABLE_CONSTR' '(' NAME ',' INT ',' INT ',' INT ',' INT ',' STRING ',' STRING ')'
		-- ptrs, nptrs, tag, closure type, description, type
		{ do prof <- profilingInfo $13 $15
		     -- If profiling is on, this string gets duplicated,
		     -- but that's the way the old code did it we can fix it some other time.
		     desc_lit <- code $ mkStringCLit $13
		     return (mkRtsEntryLabelFS $3,
			CmmInfoTable prof (fromIntegral $11)
				     (ConstrInfo (fromIntegral $5, fromIntegral $7) (fromIntegral $9) desc_lit),
			[]) }
	
	| 'INFO_TABLE_SELECTOR' '(' NAME ',' INT ',' INT ',' STRING ',' STRING ')'
		-- selector, closure type, description, type
		{ do prof <- profilingInfo $9 $11
		     return (mkRtsEntryLabelFS $3,
			CmmInfoTable prof (fromIntegral $7)
				     (ThunkSelectorInfo (fromIntegral $5) NoC_SRT),
			[]) }

	| 'INFO_TABLE_RET' '(' NAME ',' INT ')'
		-- closure type (no live regs)
		{ do let infoLabel = mkRtsInfoLabelFS $3
		     return (mkRtsRetLabelFS $3,
			CmmInfoTable (ProfilingInfo zeroCLit zeroCLit) (fromIntegral $5)
				     (ContInfo [] NoC_SRT),
			[]) }

	| 'INFO_TABLE_RET' '(' NAME ',' INT ',' formals_without_kinds0 ')'
		-- closure type, live regs
		{ do live <- sequence (map (liftM Just) $7)
		     return (mkRtsRetLabelFS $3,
			CmmInfoTable (ProfilingInfo zeroCLit zeroCLit) (fromIntegral $5)
			             (ContInfo live NoC_SRT),
			live) }

body	:: { ExtCode }
	: {- empty -}			{ return () }
	| decl body			{ do $1; $2 }
	| stmt body			{ do $1; $2 }

decl	:: { ExtCode }
	: type names ';'		{ mapM_ (newLocal defaultKind $1) $2 }
	| STRING type names ';'		{% do k <- parseGCKind $1;
					      return $ mapM_ (newLocal k $2) $3 }

	| 'import' names ';'		{ mapM_ newImport $2 }
	| 'export' names ';'		{ return () }  -- ignore exports

names 	:: { [FastString] }
	: NAME			{ [$1] }
	| NAME ',' names	{ $1 : $3 }

stmt	:: { ExtCode }
	: ';'					{ nopEC }

	| NAME ':'
		{ do l <- newLabel $1; code (labelC l) }

	| lreg '=' expr ';'
		{ do reg <- $1; e <- $3; stmtEC (CmmAssign reg e) }
	| type '[' expr ']' '=' expr ';'
		{ doStore $1 $3 $6 }

	-- Gah! We really want to say "maybe_results" but that causes
	-- a shift/reduce conflict with assignment.  We either
	-- we expand out the no-result and single result cases or
	-- we tweak the syntax to avoid the conflict.  The later
	-- option is taken here because the other way would require
	-- multiple levels of expanding and get unwieldy.
	| maybe_results 'foreign' STRING expr '(' cmm_kind_exprs0 ')' safety vols opt_never_returns ';'
		{% foreignCall $3 $1 $4 $6 $9 $8 $10 }
	| maybe_results 'prim' '%' NAME '(' cmm_kind_exprs0 ')' safety vols ';'
		{% primCall $1 $4 $6 $9 $8 }
	-- stmt-level macros, stealing syntax from ordinary C-- function calls.
	-- Perhaps we ought to use the %%-form?
	| NAME '(' exprs0 ')' ';'
		{% stmtMacro $1 $3  }
	| 'switch' maybe_range expr '{' arms default '}'
		{ doSwitch $2 $3 $5 $6 }
	| 'goto' NAME ';'
		{ do l <- lookupLabel $2; stmtEC (CmmBranch l) }
	| 'jump' expr maybe_actuals ';'
		{ do e1 <- $2; e2 <- sequence $3; stmtEC (CmmJump e1 e2) }
        | 'return' maybe_actuals ';'
		{ do e <- sequence $2; stmtEC (CmmReturn e) }
	| 'if' bool_expr '{' body '}' else 	
		{ ifThenElse $2 $4 $6 }

opt_never_returns :: { CmmReturnInfo }
        :                               { CmmMayReturn }
        | 'never' 'returns'             { CmmNeverReturns }

bool_expr :: { ExtFCode BoolExpr }
	: bool_op			{ $1 }
	| expr				{ do e <- $1; return (BoolTest e) }

bool_op :: { ExtFCode BoolExpr }
	: bool_expr '&&' bool_expr 	{ do e1 <- $1; e2 <- $3; 
					  return (BoolAnd e1 e2) }
	| bool_expr '||' bool_expr	{ do e1 <- $1; e2 <- $3; 
					  return (BoolOr e1 e2)  }
	| '!' bool_expr			{ do e <- $2; return (BoolNot e) }
	| '(' bool_op ')'		{ $2 }

-- This is not C-- syntax.  What to do?
safety  :: { CmmSafety }
	: {- empty -}			{ CmmUnsafe } -- Default may change soon
	| STRING			{% parseSafety $1 }

-- This is not C-- syntax.  What to do?
vols 	:: { Maybe [GlobalReg] }
	: {- empty -}			{ Nothing }
	| '[' ']'		        { Just [] }
	| '[' globals ']'		{ Just $2 }

globals :: { [GlobalReg] }
	: GLOBALREG			{ [$1] }
	| GLOBALREG ',' globals		{ $1 : $3 }

maybe_range :: { Maybe (Int,Int) }
	: '[' INT '..' INT ']'	{ Just (fromIntegral $2, fromIntegral $4) }
	| {- empty -}		{ Nothing }

arms	:: { [([Int],ExtCode)] }
	: {- empty -}			{ [] }
	| arm arms			{ $1 : $2 }

arm	:: { ([Int],ExtCode) }
	: 'case' ints ':' '{' body '}'	{ ($2, $5) }

ints	:: { [Int] }
	: INT				{ [ fromIntegral $1 ] }
	| INT ',' ints			{ fromIntegral $1 : $3 }

default :: { Maybe ExtCode }
	: 'default' ':' '{' body '}'	{ Just $4 }
	-- taking a few liberties with the C-- syntax here; C-- doesn't have
	-- 'default' branches
	| {- empty -}			{ Nothing }

else 	:: { ExtCode }
	: {- empty -}			{ nopEC }
	| 'else' '{' body '}'		{ $3 }

-- we have to write this out longhand so that Happy's precedence rules
-- can kick in.
expr	:: { ExtFCode CmmExpr } 
	: expr '/' expr			{ mkMachOp MO_U_Quot [$1,$3] }
	| expr '*' expr			{ mkMachOp MO_Mul [$1,$3] }
	| expr '%' expr			{ mkMachOp MO_U_Rem [$1,$3] }
	| expr '-' expr			{ mkMachOp MO_Sub [$1,$3] }
	| expr '+' expr			{ mkMachOp MO_Add [$1,$3] }
	| expr '>>' expr		{ mkMachOp MO_U_Shr [$1,$3] }
	| expr '<<' expr		{ mkMachOp MO_Shl [$1,$3] }
	| expr '&' expr			{ mkMachOp MO_And [$1,$3] }
	| expr '^' expr			{ mkMachOp MO_Xor [$1,$3] }
	| expr '|' expr			{ mkMachOp MO_Or [$1,$3] }
	| expr '>=' expr		{ mkMachOp MO_U_Ge [$1,$3] }
	| expr '>' expr			{ mkMachOp MO_U_Gt [$1,$3] }
	| expr '<=' expr		{ mkMachOp MO_U_Le [$1,$3] }
	| expr '<' expr			{ mkMachOp MO_U_Lt [$1,$3] }
	| expr '!=' expr		{ mkMachOp MO_Ne [$1,$3] }
	| expr '==' expr		{ mkMachOp MO_Eq [$1,$3] }
	| '~' expr			{ mkMachOp MO_Not [$2] }
	| '-' expr			{ mkMachOp MO_S_Neg [$2] }
	| expr0 '`' NAME '`' expr0  	{% do { mo <- nameToMachOp $3 ;
					        return (mkMachOp mo [$1,$5]) } }
	| expr0				{ $1 }

expr0	:: { ExtFCode CmmExpr }
	: INT   maybe_ty	 { return (CmmLit (CmmInt $1 $2)) }
	| FLOAT maybe_ty	 { return (CmmLit (CmmFloat $1 $2)) }
	| STRING		 { do s <- code (mkStringCLit $1); 
				      return (CmmLit s) }
	| reg			 { $1 }
	| type '[' expr ']'	 { do e <- $3; return (CmmLoad e $1) }
	| '%' NAME '(' exprs0 ')' {% exprOp $2 $4 }
	| '(' expr ')'		 { $2 }


-- leaving out the type of a literal gives you the native word size in C--
maybe_ty :: { MachRep }
	: {- empty -}			{ wordRep }
	| '::' type			{ $2 }

maybe_actuals :: { [ExtFCode CmmActual] }
	: {- empty -}		{ [] }
	| '(' cmm_kind_exprs0 ')'	{ $2 }

cmm_kind_exprs0 :: { [ExtFCode CmmActual] }
	: {- empty -}			{ [] }
	| cmm_kind_exprs			{ $1 }

cmm_kind_exprs :: { [ExtFCode CmmActual] }
	: cmm_kind_expr			{ [$1] }
	| cmm_kind_expr ',' cmm_kind_exprs	{ $1 : $3 }

cmm_kind_expr :: { ExtFCode CmmActual }
	: expr				{ do e <- $1; return (e, inferCmmKind e) }
	| expr STRING			{% do h <- parseCmmKind $2;
					      return $ do
						e <- $1; return (e,h) }

exprs0  :: { [ExtFCode CmmExpr] }
	: {- empty -}			{ [] }
	| exprs				{ $1 }

exprs	:: { [ExtFCode CmmExpr] }
	: expr				{ [ $1 ] }
	| expr ',' exprs		{ $1 : $3 }

reg	:: { ExtFCode CmmExpr }
	: NAME			{ lookupName $1 }
	| GLOBALREG		{ return (CmmReg (CmmGlobal $1)) }

maybe_results :: { [ExtFCode CmmFormal] }
	: {- empty -}		{ [] }
	| '(' cmm_formals ')' '='	{ $2 }

cmm_formals :: { [ExtFCode CmmFormal] }
	: cmm_formal			{ [$1] }
	| cmm_formal ','			{ [$1] }
	| cmm_formal ',' cmm_formals	{ $1 : $3 }

cmm_formal :: { ExtFCode CmmFormal }
	: local_lreg			{ do e <- $1; return (e, inferCmmKind (CmmReg (CmmLocal e))) }
	| STRING local_lreg		{% do h <- parseCmmKind $1;
					      return $ do
						e <- $2; return (e,h) }

local_lreg :: { ExtFCode LocalReg }
	: NAME			{ do e <- lookupName $1;
				     return $
				       case e of 
					CmmReg (CmmLocal r) -> r
					other -> pprPanic "CmmParse:" (ftext $1 <> text " not a local register") }

lreg	:: { ExtFCode CmmReg }
	: NAME			{ do e <- lookupName $1;
				     return $
				       case e of 
					CmmReg r -> r
					other -> pprPanic "CmmParse:" (ftext $1 <> text " not a register") }
	| GLOBALREG		{ return (CmmGlobal $1) }

maybe_formals_without_kinds :: { [ExtFCode LocalReg] }
	: {- empty -}		{ [] }
	| '(' formals_without_kinds0 ')'	{ $2 }

formals_without_kinds0 :: { [ExtFCode LocalReg] }
	: {- empty -}		{ [] }
	| formals_without_kinds		{ $1 }

formals_without_kinds :: { [ExtFCode LocalReg] }
	: formal_without_kind ','		{ [$1] }
	| formal_without_kind		{ [$1] }
	| formal_without_kind ',' formals_without_kinds	{ $1 : $3 }

formal_without_kind :: { ExtFCode LocalReg }
	: type NAME		{ newLocal defaultKind $1 $2 }
	| STRING type NAME	{% do k <- parseGCKind $1;
				     return $ newLocal k $2 $3 }

maybe_frame :: { ExtFCode (Maybe UpdateFrame) }
	: {- empty -}			{ return Nothing }
	| 'jump' expr '(' exprs0 ')'	{ do { target <- $2;
					       args <- sequence $4;
					       return $ Just (UpdateFrame target args) } }

maybe_gc_block :: { ExtFCode (Maybe BlockId) }
	: {- empty -}			{ return Nothing }
	| 'goto' NAME
		{ do l <- lookupLabel $2; return (Just l) }

type	:: { MachRep }
	: 'bits8'		{ I8 }
	| typenot8		{ $1 }

typenot8 :: { MachRep }
	: 'bits16'		{ I16 }
	| 'bits32'		{ I32 }
	| 'bits64'		{ I64 }
	| 'float32'		{ F32 }
	| 'float64'		{ F64 }
{
section :: String -> Section
section "text"	 = Text
section "data" 	 = Data
section "rodata" = ReadOnlyData
section "relrodata" = RelocatableReadOnlyData
section "bss"	 = UninitialisedData
section s	 = OtherSection s

mkString :: String -> CmmStatic
mkString s = CmmString (map (fromIntegral.ord) s)

-- mkMachOp infers the type of the MachOp from the type of its first
-- argument.  We assume that this is correct: for MachOps that don't have
-- symmetrical args (e.g. shift ops), the first arg determines the type of
-- the op.
mkMachOp :: (MachRep -> MachOp) -> [ExtFCode CmmExpr] -> ExtFCode CmmExpr
mkMachOp fn args = do
  arg_exprs <- sequence args
  return (CmmMachOp (fn (cmmExprRep (head arg_exprs))) arg_exprs)

getLit :: CmmExpr -> CmmLit
getLit (CmmLit l) = l
getLit (CmmMachOp (MO_S_Neg _) [CmmLit (CmmInt i r)])  = CmmInt (negate i) r
getLit _ = panic "invalid literal" -- TODO messy failure

nameToMachOp :: FastString -> P (MachRep -> MachOp)
nameToMachOp name = 
  case lookupUFM machOps name of
	Nothing -> fail ("unknown primitive " ++ unpackFS name)
	Just m  -> return m

exprOp :: FastString -> [ExtFCode CmmExpr] -> P (ExtFCode CmmExpr)
exprOp name args_code =
  case lookupUFM exprMacros name of
     Just f  -> return $ do
        args <- sequence args_code
	return (f args)
     Nothing -> do
	mo <- nameToMachOp name
	return $ mkMachOp mo args_code

exprMacros :: UniqFM ([CmmExpr] -> CmmExpr)
exprMacros = listToUFM [
  ( FSLIT("ENTRY_CODE"),   \ [x] -> entryCode x ),
  ( FSLIT("INFO_PTR"),     \ [x] -> closureInfoPtr x ),
  ( FSLIT("STD_INFO"),     \ [x] -> infoTable x ),
  ( FSLIT("FUN_INFO"),     \ [x] -> funInfoTable x ),
  ( FSLIT("GET_ENTRY"),    \ [x] -> entryCode (closureInfoPtr x) ),
  ( FSLIT("GET_STD_INFO"), \ [x] -> infoTable (closureInfoPtr x) ),
  ( FSLIT("GET_FUN_INFO"), \ [x] -> funInfoTable (closureInfoPtr x) ),
  ( FSLIT("INFO_TYPE"),    \ [x] -> infoTableClosureType x ),
  ( FSLIT("INFO_PTRS"),    \ [x] -> infoTablePtrs x ),
  ( FSLIT("INFO_NPTRS"),   \ [x] -> infoTableNonPtrs x )
  ]

-- we understand a subset of C-- primitives:
machOps = listToUFM $
	map (\(x, y) -> (mkFastString x, y)) [
	( "add",	MO_Add ),
	( "sub",	MO_Sub ),
	( "eq",		MO_Eq ),
	( "ne",		MO_Ne ),
	( "mul",	MO_Mul ),
	( "neg",	MO_S_Neg ),
	( "quot",	MO_S_Quot ),
	( "rem",	MO_S_Rem ),
	( "divu",	MO_U_Quot ),
	( "modu",	MO_U_Rem ),

	( "ge",		MO_S_Ge ),
	( "le",		MO_S_Le ),
	( "gt",		MO_S_Gt ),
	( "lt",		MO_S_Lt ),

	( "geu",	MO_U_Ge ),
	( "leu",	MO_U_Le ),
	( "gtu",	MO_U_Gt ),
	( "ltu",	MO_U_Lt ),

	( "flt",	MO_S_Lt ),
	( "fle",	MO_S_Le ),
	( "feq",	MO_Eq ),
	( "fne",	MO_Ne ),
	( "fgt",	MO_S_Gt ),
	( "fge",	MO_S_Ge ),
	( "fneg",	MO_S_Neg ),

	( "and",	MO_And ),
	( "or",		MO_Or ),
	( "xor",	MO_Xor ),
	( "com",	MO_Not ),
	( "shl",	MO_Shl ),
	( "shrl",	MO_U_Shr ),
	( "shra",	MO_S_Shr ),

	( "lobits8",  flip MO_U_Conv I8  ),
	( "lobits16", flip MO_U_Conv I16 ),
	( "lobits32", flip MO_U_Conv I32 ),
	( "lobits64", flip MO_U_Conv I64 ),
	( "sx16",     flip MO_S_Conv I16 ),
	( "sx32",     flip MO_S_Conv I32 ),
	( "sx64",     flip MO_S_Conv I64 ),
	( "zx16",     flip MO_U_Conv I16 ),
	( "zx32",     flip MO_U_Conv I32 ),
	( "zx64",     flip MO_U_Conv I64 ),
	( "f2f32",    flip MO_S_Conv F32 ),  -- TODO; rounding mode
	( "f2f64",    flip MO_S_Conv F64 ),  -- TODO; rounding mode
	( "f2i8",     flip MO_S_Conv I8 ),
	( "f2i16",    flip MO_S_Conv I16 ),
	( "f2i32",    flip MO_S_Conv I32 ),
	( "f2i64",    flip MO_S_Conv I64 ),
	( "i2f32",    flip MO_S_Conv F32 ),
	( "i2f64",    flip MO_S_Conv F64 )
	]

callishMachOps = listToUFM $
	map (\(x, y) -> (mkFastString x, y)) [
        ( "write_barrier", MO_WriteBarrier )
        -- ToDo: the rest, maybe
    ]

parseSafety :: String -> P CmmSafety
parseSafety "safe"   = return (CmmSafe NoC_SRT)
parseSafety "unsafe" = return CmmUnsafe
parseSafety str      = fail ("unrecognised safety: " ++ str)

parseCmmKind :: String -> P CmmKind
parseCmmKind "ptr"    = return PtrHint
parseCmmKind "signed" = return SignedHint
parseCmmKind "float"  = return FloatHint
parseCmmKind str      = fail ("unrecognised hint: " ++ str)

parseGCKind :: String -> P GCKind
parseGCKind "ptr"    = return GCKindPtr
parseGCKind str      = fail ("unrecognized kin: " ++ str)

defaultKind :: GCKind
defaultKind = GCKindNonPtr

-- labels are always pointers, so we might as well infer the hint
inferCmmKind :: CmmExpr -> CmmKind
inferCmmKind (CmmLit (CmmLabel _)) = PtrHint
inferCmmKind (CmmReg (CmmGlobal g)) | isPtrGlobalReg g = PtrHint
inferCmmKind _ = NoHint

isPtrGlobalReg Sp		= True
isPtrGlobalReg SpLim		= True
isPtrGlobalReg Hp		= True
isPtrGlobalReg HpLim		= True
isPtrGlobalReg CurrentTSO	= True
isPtrGlobalReg CurrentNursery	= True
isPtrGlobalReg _		= False

happyError :: P a
happyError = srcParseFail

-- -----------------------------------------------------------------------------
-- Statement-level macros

stmtMacro :: FastString -> [ExtFCode CmmExpr] -> P ExtCode
stmtMacro fun args_code = do
  case lookupUFM stmtMacros fun of
    Nothing -> fail ("unknown macro: " ++ unpackFS fun)
    Just fcode -> return $ do
	args <- sequence args_code
	code (fcode args)

stmtMacros :: UniqFM ([CmmExpr] -> Code)
stmtMacros = listToUFM [
  ( FSLIT("CCS_ALLOC"),		   \[words,ccs]  -> profAlloc words ccs ),
  ( FSLIT("CLOSE_NURSERY"),	   \[]  -> emitCloseNursery ),
  ( FSLIT("ENTER_CCS_PAP_CL"),     \[e] -> enterCostCentrePAP e ),
  ( FSLIT("ENTER_CCS_THUNK"),      \[e] -> enterCostCentreThunk e ),
  ( FSLIT("HP_CHK_GEN"),           \[words,liveness,reentry] -> 
                                      hpChkGen words liveness reentry ),
  ( FSLIT("HP_CHK_NP_ASSIGN_SP0"), \[e,f] -> hpChkNodePointsAssignSp0 e f ),
  ( FSLIT("LOAD_THREAD_STATE"),    \[] -> emitLoadThreadState ),
  ( FSLIT("LDV_ENTER"),            \[e] -> ldvEnter e ),
  ( FSLIT("LDV_RECORD_CREATE"),    \[e] -> ldvRecordCreate e ),
  ( FSLIT("OPEN_NURSERY"),	   \[]  -> emitOpenNursery ),
  ( FSLIT("PUSH_UPD_FRAME"),	   \[sp,e] -> emitPushUpdateFrame sp e ),
  ( FSLIT("SAVE_THREAD_STATE"),    \[] -> emitSaveThreadState ),
  ( FSLIT("SET_HDR"),		   \[ptr,info,ccs] -> 
					emitSetDynHdr ptr info ccs ),
  ( FSLIT("STK_CHK_GEN"),          \[words,liveness,reentry] -> 
                                      stkChkGen words liveness reentry ),
  ( FSLIT("STK_CHK_NP"),	   \[e] -> stkChkNodePoints e ),
  ( FSLIT("TICK_ALLOC_PRIM"), 	   \[hdr,goods,slop] -> 
					tickyAllocPrim hdr goods slop ),
  ( FSLIT("TICK_ALLOC_PAP"),       \[goods,slop] -> 
					tickyAllocPAP goods slop ),
  ( FSLIT("TICK_ALLOC_UP_THK"),    \[goods,slop] -> 
					tickyAllocThunk goods slop ),
  ( FSLIT("UPD_BH_UPDATABLE"),       \[] -> emitBlackHoleCode False ),
  ( FSLIT("UPD_BH_SINGLE_ENTRY"),    \[] -> emitBlackHoleCode True ),

  ( FSLIT("RET_P"),	\[a] ->       emitRetUT [(PtrArg,a)]),
  ( FSLIT("RET_N"),	\[a] ->       emitRetUT [(NonPtrArg,a)]),
  ( FSLIT("RET_PP"),	\[a,b] ->     emitRetUT [(PtrArg,a),(PtrArg,b)]),
  ( FSLIT("RET_NN"),	\[a,b] ->     emitRetUT [(NonPtrArg,a),(NonPtrArg,b)]),
  ( FSLIT("RET_NP"),	\[a,b] ->     emitRetUT [(NonPtrArg,a),(PtrArg,b)]),
  ( FSLIT("RET_PPP"),	\[a,b,c] ->   emitRetUT [(PtrArg,a),(PtrArg,b),(PtrArg,c)]),
  ( FSLIT("RET_NPP"),	\[a,b,c] ->   emitRetUT [(NonPtrArg,a),(PtrArg,b),(PtrArg,c)]),
  ( FSLIT("RET_NNP"),	\[a,b,c] ->   emitRetUT [(NonPtrArg,a),(NonPtrArg,b),(PtrArg,c)]),
  ( FSLIT("RET_NNNP"),	\[a,b,c,d] -> emitRetUT [(NonPtrArg,a),(NonPtrArg,b),(NonPtrArg,c),(PtrArg,d)]),
  ( FSLIT("RET_NPNP"),	\[a,b,c,d] -> emitRetUT [(NonPtrArg,a),(PtrArg,b),(NonPtrArg,c),(PtrArg,d)])

 ]

-- -----------------------------------------------------------------------------
-- Our extended FCode monad.

-- We add a mapping from names to CmmExpr, to support local variable names in
-- the concrete C-- code.  The unique supply of the underlying FCode monad
-- is used to grab a new unique for each local variable.

-- In C--, a local variable can be declared anywhere within a proc,
-- and it scopes from the beginning of the proc to the end.  Hence, we have
-- to collect declarations as we parse the proc, and feed the environment
-- back in circularly (to avoid a two-pass algorithm).

data Named = Var CmmExpr | Label BlockId
type Decls = [(FastString,Named)]
type Env   = UniqFM Named

newtype ExtFCode a = EC { unEC :: Env -> Decls -> FCode (Decls, a) }

type ExtCode = ExtFCode ()

returnExtFC a = EC $ \e s -> return (s, a)
thenExtFC (EC m) k = EC $ \e s -> do (s',r) <- m e s; unEC (k r) e s'

instance Monad ExtFCode where
  (>>=) = thenExtFC
  return = returnExtFC

-- This function takes the variable decarations and imports and makes 
-- an environment, which is looped back into the computation.  In this
-- way, we can have embedded declarations that scope over the whole
-- procedure, and imports that scope over the entire module.
-- Discards the local declaration contained within decl'
loopDecls :: ExtFCode a -> ExtFCode a
loopDecls (EC fcode) =
      EC $ \e globalDecls -> do
	(decls', a) <- fixC (\ ~(decls,a) -> fcode (addListToUFM e (decls ++ globalDecls)) globalDecls)
	return (globalDecls, a)

getEnv :: ExtFCode Env
getEnv = EC $ \e s -> return (s, e)

addVarDecl :: FastString -> CmmExpr -> ExtCode
addVarDecl var expr = EC $ \e s -> return ((var, Var expr):s, ())

addLabel :: FastString -> BlockId -> ExtCode
addLabel name block_id = EC $ \e s -> return ((name, Label block_id):s, ())

newLocal :: GCKind -> MachRep -> FastString -> ExtFCode LocalReg
newLocal kind ty name = do
   u <- code newUnique
   let reg = LocalReg u ty kind
   addVarDecl name (CmmReg (CmmLocal reg))
   return reg

-- Creates a foreign label in the import. CLabel's labelDynamic
-- classifies these labels as dynamic, hence the code generator emits the
-- PIC code for them.
newImport :: FastString -> ExtFCode ()
newImport name
   = addVarDecl name (CmmLit (CmmLabel (mkForeignLabel name Nothing True)))

newLabel :: FastString -> ExtFCode BlockId
newLabel name = do
   u <- code newUnique
   addLabel name (BlockId u)
   return (BlockId u)

lookupLabel :: FastString -> ExtFCode BlockId
lookupLabel name = do
  env <- getEnv
  return $ 
     case lookupUFM env name of
	Just (Label l) -> l
	_other -> BlockId (newTagUnique (getUnique name) 'L')

-- Unknown names are treated as if they had been 'import'ed.
-- This saves us a lot of bother in the RTS sources, at the expense of
-- deferring some errors to link time.
lookupName :: FastString -> ExtFCode CmmExpr
lookupName name = do
  env <- getEnv
  return $ 
     case lookupUFM env name of
	Just (Var e) -> e
	_other -> CmmLit (CmmLabel (mkRtsCodeLabelFS name))

-- Lifting FCode computations into the ExtFCode monad:
code :: FCode a -> ExtFCode a
code fc = EC $ \e s -> do r <- fc; return (s, r)

code2 :: (FCode (Decls,b) -> FCode ((Decls,b),c))
	 -> ExtFCode b -> ExtFCode c
code2 f (EC ec) = EC $ \e s -> do ((s',b),c) <- f (ec e s); return (s',c)

nopEC = code nopC
stmtEC stmt = code (stmtC stmt)
stmtsEC stmts = code (stmtsC stmts)
getCgStmtsEC = code2 getCgStmts'
getCgStmtsEC' = code2 (\m -> getCgStmts' m >>= f)
  where f ((decl, b), c) = return ((decl, b), (b, c))

forkLabelledCodeEC ec = do
  stmts <- getCgStmtsEC ec
  code (forkCgStmts stmts)


profilingInfo desc_str ty_str = do
  lit1 <- if opt_SccProfilingOn 
		   then code $ mkStringCLit desc_str
		   else return (mkIntCLit 0)
  lit2 <- if opt_SccProfilingOn 
		   then code $ mkStringCLit ty_str
		   else return (mkIntCLit 0)
  return (ProfilingInfo lit1 lit2)


staticClosure :: FastString -> FastString -> [CmmLit] -> ExtCode
staticClosure cl_label info payload
  = code $ emitDataLits (mkRtsDataLabelFS cl_label) lits
  where  lits = mkStaticClosure (mkRtsInfoLabelFS info) dontCareCCS payload [] [] []

foreignCall
	:: String
	-> [ExtFCode CmmFormal]
	-> ExtFCode CmmExpr
	-> [ExtFCode CmmActual]
	-> Maybe [GlobalReg]
        -> CmmSafety
        -> CmmReturnInfo
        -> P ExtCode
foreignCall conv_string results_code expr_code args_code vols safety ret
  = do  convention <- case conv_string of
          "C" -> return CCallConv
          "stdcall" -> return StdCallConv
          "C--" -> return CmmCallConv
          _ -> fail ("unknown calling convention: " ++ conv_string)
	return $ do
	  results <- sequence results_code
	  expr <- expr_code
	  args <- sequence args_code
	  --code (stmtC (CmmCall (CmmCallee expr convention) results args safety))
          case convention of
            -- Temporary hack so at least some functions are CmmSafe
            CmmCallConv -> code (stmtC (CmmCall (CmmCallee expr convention) results args safety ret))
            _ ->
              let expr' = adjCallTarget convention expr args in
              case safety of
	      CmmUnsafe ->
                code (emitForeignCall' PlayRisky results 
                   (CmmCallee expr' convention) args vols NoC_SRT ret)
              CmmSafe srt ->
                code (emitForeignCall' (PlaySafe unused) results 
                   (CmmCallee expr' convention) args vols NoC_SRT ret) where
	        unused = panic "not used by emitForeignCall'"

adjCallTarget :: CCallConv -> CmmExpr -> [(CmmExpr,MachHint)] -> CmmExpr
#ifdef mingw32_TARGET_OS
-- On Windows, we have to add the '@N' suffix to the label when making
-- a call with the stdcall calling convention.
adjCallTarget StdCallConv (CmmLit (CmmLabel lbl)) args
  = CmmLit (CmmLabel (addLabelSize lbl (sum (map size args))))
  where size (e,_) = max wORD_SIZE (machRepByteWidth (cmmExprRep e))
                 -- c.f. CgForeignCall.emitForeignCall
#endif
adjCallTarget _ expr _
  = expr

primCall
	:: [ExtFCode CmmFormal]
	-> FastString
	-> [ExtFCode CmmActual]
	-> Maybe [GlobalReg]
        -> CmmSafety
        -> P ExtCode
primCall results_code name args_code vols safety
  = case lookupUFM callishMachOps name of
	Nothing -> fail ("unknown primitive " ++ unpackFS name)
	Just p  -> return $ do
		results <- sequence results_code
		args <- sequence args_code
		case safety of
		  CmmUnsafe ->
		    code (emitForeignCall' PlayRisky results
		      (CmmPrim p) args vols NoC_SRT CmmMayReturn)
		  CmmSafe srt ->
		    code (emitForeignCall' (PlaySafe unused) results 
		      (CmmPrim p) args vols NoC_SRT CmmMayReturn) where
		    unused = panic "not used by emitForeignCall'"

doStore :: MachRep -> ExtFCode CmmExpr  -> ExtFCode CmmExpr -> ExtCode
doStore rep addr_code val_code
  = do addr <- addr_code
       val <- val_code
	-- if the specified store type does not match the type of the expr
	-- on the rhs, then we insert a coercion that will cause the type
	-- mismatch to be flagged by cmm-lint.  If we don't do this, then
	-- the store will happen at the wrong type, and the error will not
	-- be noticed.
       let coerce_val 
		| cmmExprRep val /= rep = CmmMachOp (MO_U_Conv rep rep) [val]
		| otherwise             = val
       stmtEC (CmmStore addr coerce_val)

-- Return an unboxed tuple.
emitRetUT :: [(CgRep,CmmExpr)] -> Code
emitRetUT args = do
  tickyUnboxedTupleReturn (length args)  -- TICK
  (sp, stmts) <- pushUnboxedTuple 0 args
  emitStmts stmts
  when (sp /= 0) $ stmtC (CmmAssign spReg (cmmRegOffW spReg (-sp)))
  stmtC (CmmJump (entryCode (CmmLoad (cmmRegOffW spReg sp) wordRep)) [])
  -- TODO (when using CPS): emitStmt (CmmReturn (map snd args))

-- -----------------------------------------------------------------------------
-- If-then-else and boolean expressions

data BoolExpr
  = BoolExpr `BoolAnd` BoolExpr
  | BoolExpr `BoolOr`  BoolExpr
  | BoolNot BoolExpr
  | BoolTest CmmExpr

-- ToDo: smart constructors which simplify the boolean expression.

ifThenElse cond then_part else_part = do
     then_id <- code newLabelC
     join_id <- code newLabelC
     c <- cond
     emitCond c then_id
     else_part
     stmtEC (CmmBranch join_id)
     code (labelC then_id)
     then_part
     -- fall through to join
     code (labelC join_id)

-- 'emitCond cond true_id'  emits code to test whether the cond is true,
-- branching to true_id if so, and falling through otherwise.
emitCond (BoolTest e) then_id = do
  stmtEC (CmmCondBranch e then_id)
emitCond (BoolNot (BoolTest (CmmMachOp op args))) then_id
  | Just op' <- maybeInvertComparison op
  = emitCond (BoolTest (CmmMachOp op' args)) then_id
emitCond (BoolNot e) then_id = do
  else_id <- code newLabelC
  emitCond e else_id
  stmtEC (CmmBranch then_id)
  code (labelC else_id)
emitCond (e1 `BoolOr` e2) then_id = do
  emitCond e1 then_id
  emitCond e2 then_id
emitCond (e1 `BoolAnd` e2) then_id = do
	-- we'd like to invert one of the conditionals here to avoid an
	-- extra branch instruction, but we can't use maybeInvertComparison
	-- here because we can't look too closely at the expression since
	-- we're in a loop.
  and_id <- code newLabelC
  else_id <- code newLabelC
  emitCond e1 and_id
  stmtEC (CmmBranch else_id)
  code (labelC and_id)
  emitCond e2 then_id
  code (labelC else_id)


-- -----------------------------------------------------------------------------
-- Table jumps

-- We use a simplified form of C-- switch statements for now.  A
-- switch statement always compiles to a table jump.  Each arm can
-- specify a list of values (not ranges), and there can be a single
-- default branch.  The range of the table is given either by the
-- optional range on the switch (eg. switch [0..7] {...}), or by
-- the minimum/maximum values from the branches.

doSwitch :: Maybe (Int,Int) -> ExtFCode CmmExpr -> [([Int],ExtCode)]
         -> Maybe ExtCode -> ExtCode
doSwitch mb_range scrut arms deflt
   = do 
	-- Compile code for the default branch
	dflt_entry <- 
		case deflt of
		  Nothing -> return Nothing
		  Just e  -> do b <- forkLabelledCodeEC e; return (Just b)

	-- Compile each case branch
	table_entries <- mapM emitArm arms

	-- Construct the table
	let
	    all_entries = concat table_entries
	    ixs = map fst all_entries
	    (min,max) 
		| Just (l,u) <- mb_range = (l,u)
		| otherwise              = (minimum ixs, maximum ixs)

	    entries = elems (accumArray (\_ a -> Just a) dflt_entry (min,max)
				all_entries)
	expr <- scrut
	-- ToDo: check for out of range and jump to default if necessary
        stmtEC (CmmSwitch expr entries)
   where
	emitArm :: ([Int],ExtCode) -> ExtFCode [(Int,BlockId)]
	emitArm (ints,code) = do
	   blockid <- forkLabelledCodeEC code
	   return [ (i,blockid) | i <- ints ]


-- -----------------------------------------------------------------------------
-- Putting it all together

-- The initial environment: we define some constants that the compiler
-- knows about here.
initEnv :: Env
initEnv = listToUFM [
  ( FSLIT("SIZEOF_StgHeader"), 
    Var (CmmLit (CmmInt (fromIntegral (fixedHdrSize * wORD_SIZE)) wordRep) )),
  ( FSLIT("SIZEOF_StgInfoTable"),
    Var (CmmLit (CmmInt (fromIntegral stdInfoTableSizeB) wordRep) ))
  ]

parseCmmFile :: DynFlags -> FilePath -> IO (Maybe Cmm)
parseCmmFile dflags filename = do
  showPass dflags "ParseCmm"
  buf <- hGetStringBuffer filename
  let
	init_loc = mkSrcLoc (mkFastString filename) 1 0
	init_state = (mkPState buf init_loc dflags) { lex_state = [0] }
		-- reset the lex_state: the Lexer monad leaves some stuff
		-- in there we don't want.
  case unP cmmParse init_state of
    PFailed span err -> do printError span err; return Nothing
    POk pst code -> do
	cmm <- initC dflags no_module (getCmm (unEC code initEnv [] >> return ()))
	let ms = getMessages pst
	printErrorsAndWarnings dflags ms
        when (errorsFound dflags ms) $ exitWith (ExitFailure 1)
        dumpIfSet_dyn dflags Opt_D_dump_cmm "Cmm" (ppr cmm)
	return (Just cmm)
  where
	no_module = panic "parseCmmFile: no module"
}
