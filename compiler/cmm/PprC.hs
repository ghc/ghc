-----------------------------------------------------------------------------
--
-- Pretty-printing of Cmm as C, suitable for feeding gcc
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

--
-- Print Cmm as real C, for -fvia-C
--
-- This is simpler than the old PprAbsC, because Cmm is "macro-expanded"
-- relative to the old AbstractC, and many oddities/decorations have
-- disappeared from the data type.
--

-- ToDo: save/restore volatile registers around calls.

module PprC (
        writeCs,
        pprStringInCStyle 
  ) where

#include "HsVersions.h"

-- Cmm stuff
import Cmm
import CLabel
import MachOp
import ForeignCall

-- Utils
import DynFlags
import Unique
import UniqSet
import FiniteMap
import UniqFM
import FastString
import Outputable
import Constants

-- The rest
import Data.List
import Data.Bits
import Data.Char
import System.IO
import Data.Word

#ifdef DEBUG
import PprCmm		() -- instances only
-- import Debug.Trace
#endif

#if __GLASGOW_HASKELL__ >= 504
import Data.Array.ST
#endif
import Control.Monad.ST

-- --------------------------------------------------------------------------
-- Top level

pprCs :: DynFlags -> [Cmm] -> SDoc
pprCs dflags cmms
 = pprCode CStyle (vcat $ map (\c -> split_marker $$ pprC c) cmms)
 where
   split_marker
     | dopt Opt_SplitObjs dflags = ptext SLIT("__STG_SPLIT_MARKER")
     | otherwise     	         = empty

writeCs :: DynFlags -> Handle -> [Cmm] -> IO ()
writeCs dflags handle cmms 
  = printForC handle (pprCs dflags cmms)

-- --------------------------------------------------------------------------
-- Now do some real work
--
-- for fun, we could call cmmToCmm over the tops...
--

pprC :: Cmm -> SDoc
pprC (Cmm tops) = vcat $ intersperse (text "") $ map pprTop tops

--
-- top level procs
-- 
pprTop :: CmmTop -> SDoc
pprTop (CmmProc info clbl _params blocks) =
    (if not (null info)
        then pprDataExterns info $$
             pprWordArray (entryLblToInfoLbl clbl) info
        else empty) $$
    (case blocks of
        [] -> empty
         -- the first block doesn't get a label:
        (BasicBlock _ stmts : rest) -> vcat [
	   text "",
	   extern_decls,
           (if (externallyVisibleCLabel clbl)
                    then mkFN_ else mkIF_) (pprCLabel clbl) <+> lbrace,
           nest 8 temp_decls,
           nest 8 mkFB_,
           nest 8 (vcat (map pprStmt stmts)) $$
              vcat (map pprBBlock rest),
           nest 8 mkFE_,
           rbrace ]
    )
  where
	(temp_decls, extern_decls) = pprTempAndExternDecls blocks 


-- Chunks of static data.

-- We only handle (a) arrays of word-sized things and (b) strings.

pprTop (CmmData _section _ds@[CmmDataLabel lbl, CmmString str]) = 
  hcat [
    pprLocalness lbl, ptext SLIT("char "), pprCLabel lbl,
    ptext SLIT("[] = "), pprStringInCStyle str, semi
  ]

pprTop (CmmData _section _ds@[CmmDataLabel lbl, CmmUninitialised size]) = 
  hcat [
    pprLocalness lbl, ptext SLIT("char "), pprCLabel lbl,
    brackets (int size), semi
  ]

pprTop top@(CmmData _section (CmmDataLabel lbl : lits)) = 
  pprDataExterns lits $$
  pprWordArray lbl lits  

-- these shouldn't appear?
pprTop (CmmData _ _) = panic "PprC.pprTop: can't handle this data"


-- --------------------------------------------------------------------------
-- BasicBlocks are self-contained entities: they always end in a jump.
--
-- Like nativeGen/AsmCodeGen, we could probably reorder blocks to turn
-- as many jumps as possible into fall throughs.
--

pprBBlock :: CmmBasicBlock -> SDoc
pprBBlock (BasicBlock lbl stmts) = 
    if null stmts then
        pprTrace "pprC.pprBBlock: curious empty code block for" 
                        (pprBlockId lbl) empty
    else 
        nest 4 (pprBlockId lbl <> colon) $$
        nest 8 (vcat (map pprStmt stmts))

-- --------------------------------------------------------------------------
-- Info tables. Just arrays of words. 
-- See codeGen/ClosureInfo, and nativeGen/PprMach

pprWordArray :: CLabel -> [CmmStatic] -> SDoc
pprWordArray lbl ds
  = hcat [ pprLocalness lbl, ptext SLIT("StgWord")
         , space, pprCLabel lbl, ptext SLIT("[] = {") ] 
    $$ nest 8 (commafy (pprStatics ds))
    $$ ptext SLIT("};")

--
-- has to be static, if it isn't globally visible
--
pprLocalness :: CLabel -> SDoc
pprLocalness lbl | not $ externallyVisibleCLabel lbl = ptext SLIT("static ")
                 | otherwise = empty

-- --------------------------------------------------------------------------
-- Statements.
--

pprStmt :: CmmStmt -> SDoc

pprStmt stmt = case stmt of
    CmmNop       -> empty
    CmmComment s -> (hang (ptext SLIT("/*")) 3 (ftext s)) $$ ptext SLIT("*/")

    CmmAssign dest src -> pprAssign dest src

    CmmStore  dest src
	| rep == I64 && wordRep /= I64
	-> ptext SLIT("ASSIGN_Word64") <> 
		parens (mkP_ <> pprExpr1 dest <> comma <> pprExpr src) <> semi

	| rep == F64 && wordRep /= I64
	-> ptext SLIT("ASSIGN_DBL") <> 
		parens (mkP_ <> pprExpr1 dest <> comma <> pprExpr src) <> semi

 	| otherwise
	-> hsep [ pprExpr (CmmLoad dest rep), equals, pprExpr src <> semi ]
	where
	  rep = cmmExprRep src

    CmmCall (CmmForeignCall fn cconv) results args volatile -> 
	-- Controversial: leave this out for now.
	-- pprUndef fn $$

	pprCall ppr_fn cconv results args volatile
	where
    	ppr_fn = case fn of
		   CmmLit (CmmLabel lbl) -> pprCLabel lbl
		   _other -> parens (cCast (pprCFunType cconv results args) fn)
			-- for a dynamic call, cast the expression to
			-- a function of the right type (we hope).

	-- we #undef a function before calling it: the FFI is supposed to be
	-- an interface specifically to C, not to C+CPP.  For one thing, this
	-- makes the via-C route more compatible with the NCG.  If macros
	-- are being used for optimisation, then inline functions are probably
	-- better anyway.
	pprUndef (CmmLit (CmmLabel lbl)) = 
	   ptext SLIT("#undef") <+> pprCLabel lbl
	pprUndef _ = empty

    CmmCall (CmmPrim op) results args volatile -> 
	pprCall ppr_fn CCallConv results args volatile
	where
    	ppr_fn = pprCallishMachOp_for_C op

    CmmBranch ident          -> pprBranch ident
    CmmCondBranch expr ident -> pprCondBranch expr ident
    CmmJump lbl _params      -> mkJMP_(pprExpr lbl) <> semi
    CmmSwitch arg ids        -> pprSwitch arg ids

pprCFunType :: CCallConv -> [(CmmReg,MachHint)] -> [(CmmExpr,MachHint)] -> SDoc
pprCFunType cconv ress args
  = hcat [
	res_type ress,
	parens (text (ccallConvAttribute cconv) <>  char '*'),
	parens (commafy (map arg_type args))
   ]
  where
	res_type [] = ptext SLIT("void")
	res_type [(one,hint)] = machRepHintCType (cmmRegRep one) hint

	arg_type (expr,hint) = machRepHintCType (cmmExprRep expr) hint

-- ---------------------------------------------------------------------
-- unconditional branches
pprBranch :: BlockId -> SDoc
pprBranch ident = ptext SLIT("goto") <+> pprBlockId ident <> semi


-- ---------------------------------------------------------------------
-- conditional branches to local labels
pprCondBranch :: CmmExpr -> BlockId -> SDoc
pprCondBranch expr ident 
        = hsep [ ptext SLIT("if") , parens(pprExpr expr) ,
                        ptext SLIT("goto") , (pprBlockId ident) <> semi ]


-- ---------------------------------------------------------------------
-- a local table branch
--
-- we find the fall-through cases
--
-- N.B. we remove Nothing's from the list of branches, as they are
-- 'undefined'. However, they may be defined one day, so we better
-- document this behaviour.
--
pprSwitch :: CmmExpr -> [ Maybe BlockId ] -> SDoc
pprSwitch e maybe_ids 
  = let pairs  = [ (ix, ident) | (ix,Just ident) <- zip [0..] maybe_ids ]
	pairs2 = [ (map fst as, snd (head as)) | as <- groupBy sndEq pairs ]
    in 
        (hang (ptext SLIT("switch") <+> parens ( pprExpr e ) <+> lbrace)
                4 (vcat ( map caseify pairs2 )))
        $$ rbrace

  where
    sndEq (_,x) (_,y) = x == y

    -- fall through case
    caseify (ix:ixs, ident) = vcat (map do_fallthrough ixs) $$ final_branch ix
	where 
	do_fallthrough ix =
                 hsep [ ptext SLIT("case") , pprHexVal ix wordRep <> colon ,
                        ptext SLIT("/* fall through */") ]

	final_branch ix = 
	        hsep [ ptext SLIT("case") , pprHexVal ix wordRep <> colon ,
                       ptext SLIT("goto") , (pprBlockId ident) <> semi ]

-- ---------------------------------------------------------------------
-- Expressions.
--

-- C Types: the invariant is that the C expression generated by
--
--	pprExpr e
--
-- has a type in C which is also given by
--
--	machRepCType (cmmExprRep e)
--
-- (similar invariants apply to the rest of the pretty printer).

pprExpr :: CmmExpr -> SDoc
pprExpr e = case e of
    CmmLit lit -> pprLit lit

    CmmLoad e I64 | wordRep /= I64
	-> ptext SLIT("PK_Word64") <> parens (mkP_ <> pprExpr1 e)

    CmmLoad e F64 | wordRep /= I64
	-> ptext SLIT("PK_DBL") <> parens (mkP_ <> pprExpr1 e)

    CmmLoad (CmmReg r) rep 
	| isPtrReg r && rep == wordRep
	-> char '*' <> pprAsPtrReg r

    CmmLoad (CmmRegOff r 0) rep 
	| isPtrReg r && rep == wordRep
	-> char '*' <> pprAsPtrReg r

    CmmLoad (CmmRegOff r off) rep
	| isPtrReg r && rep == wordRep 
	-- ToDo: check that the offset is a word multiple?
	-> pprAsPtrReg r <> brackets (ppr (off `shiftR` wordShift))

    CmmLoad expr rep ->
	-- the general case:
	char '*' <> parens (cCast (machRepPtrCType rep) expr)

    CmmReg reg      -> pprCastReg reg
    CmmRegOff reg 0 -> pprCastReg reg

    CmmRegOff reg i
	| i >  0    -> pprRegOff (char '+') i
	| otherwise -> pprRegOff (char '-') (-i)
      where
	pprRegOff op i' = pprCastReg reg <> op <> int i'

    CmmMachOp mop args -> pprMachOpApp mop args

pprExpr1 :: CmmExpr -> SDoc
pprExpr1 (CmmLit lit) 	  = pprLit1 lit
pprExpr1 e@(CmmReg _reg)  = pprExpr e
pprExpr1 other            = parens (pprExpr other)

-- --------------------------------------------------------------------------
-- MachOp applications

pprMachOpApp :: MachOp -> [CmmExpr] -> SDoc

pprMachOpApp op args
  | isMulMayOfloOp op
  = ptext SLIT("mulIntMayOflo") <> parens (commafy (map pprExpr args))
  where isMulMayOfloOp (MO_U_MulMayOflo _) = True
	isMulMayOfloOp (MO_S_MulMayOflo _) = True
	isMulMayOfloOp _ = False

pprMachOpApp mop args
 = case args of
    -- dyadic
    [x,y] -> pprArg x <+> pprMachOp_for_C mop <+> pprArg y

    -- unary
    [x]   -> pprMachOp_for_C mop <> parens (pprArg x)

    _     -> panic "PprC.pprMachOp : machop with wrong number of args"

  where
    pprArg e | signedOp mop = cCast (machRepSignedCType (cmmExprRep e)) e
 	     | otherwise    = pprExpr1 e

-- --------------------------------------------------------------------------
-- Literals

pprLit :: CmmLit -> SDoc
pprLit lit = case lit of
    CmmInt i rep      -> pprHexVal i rep
    CmmFloat f rep     -> parens (machRepCType rep) <> (rational f)
    CmmLabel clbl      -> mkW_ <> pprCLabelAddr clbl
    CmmLabelOff clbl i -> mkW_ <> pprCLabelAddr clbl <> char '+' <> int i
    CmmLabelDiffOff clbl1 clbl2 i
        -- WARNING:
        --  * the lit must occur in the info table clbl2
        --  * clbl1 must be an SRT, a slow entry point or a large bitmap
        -- The Mangler is expected to convert any reference to an SRT,
        -- a slow entry point or a large bitmap
        -- from an info table to an offset.
        -> mkW_ <> pprCLabelAddr clbl1 <> char '+' <> int i

pprCLabelAddr lbl = char '&' <> pprCLabel lbl

pprLit1 :: CmmLit -> SDoc
pprLit1 lit@(CmmLabelOff _ _) = parens (pprLit lit)
pprLit1 lit@(CmmLabelDiffOff _ _ _) = parens (pprLit lit)
pprLit1 lit@(CmmFloat _ _)    = parens (pprLit lit)
pprLit1 other = pprLit other

-- ---------------------------------------------------------------------------
-- Static data

pprStatics :: [CmmStatic] -> [SDoc]
pprStatics [] = []
pprStatics (CmmStaticLit (CmmFloat f F32) : rest) 
  = pprLit1 (floatToWord f) : pprStatics rest
pprStatics (CmmStaticLit (CmmFloat f F64) : rest)
  = map pprLit1 (doubleToWords f) ++ pprStatics rest
pprStatics (CmmStaticLit (CmmInt i I64) : rest)
  | machRepByteWidth I32 == wORD_SIZE
#ifdef WORDS_BIGENDIAN
  = pprStatics (CmmStaticLit (CmmInt q I32) : 
		CmmStaticLit (CmmInt r I32) : rest)
#else
  = pprStatics (CmmStaticLit (CmmInt r I32) : 
		CmmStaticLit (CmmInt q I32) : rest)
#endif
  where r = i .&. 0xffffffff
	q = i `shiftR` 32
pprStatics (CmmStaticLit lit : rest)
  = pprLit1 lit : pprStatics rest
pprStatics (other : rest)
  = pprPanic "pprWord" (pprStatic other)

pprStatic :: CmmStatic -> SDoc
pprStatic s = case s of

    CmmStaticLit lit   -> nest 4 (pprLit lit)
    CmmAlign i         -> nest 4 (ptext SLIT("/* align */") <+> int i)
    CmmDataLabel clbl  -> pprCLabel clbl <> colon
    CmmUninitialised i -> nest 4 (mkC_ <> brackets (int i))

    -- these should be inlined, like the old .hc
    CmmString s'       -> nest 4 (mkW_ <> parens(pprStringInCStyle s'))


-- ---------------------------------------------------------------------------
-- Block Ids

pprBlockId :: BlockId -> SDoc
pprBlockId b = char '_' <> ppr (getUnique b)

-- --------------------------------------------------------------------------
-- Print a MachOp in a way suitable for emitting via C.
--

pprMachOp_for_C :: MachOp -> SDoc

pprMachOp_for_C mop = case mop of 

        -- Integer operations
        MO_Add          _ -> char '+'
        MO_Sub          _ -> char '-'
        MO_Eq           _ -> ptext SLIT("==")
        MO_Ne           _ -> ptext SLIT("!=")
        MO_Mul          _ -> char '*'

        MO_S_Quot       _ -> char '/'
        MO_S_Rem        _ -> char '%'
        MO_S_Neg        _ -> char '-'

        MO_U_Quot       _ -> char '/'
        MO_U_Rem        _ -> char '%'

        -- Signed comparisons (floating-point comparisons also use these)
        -- & Unsigned comparisons
        MO_S_Ge         _ -> ptext SLIT(">=")
        MO_S_Le         _ -> ptext SLIT("<=")
        MO_S_Gt         _ -> char '>'
        MO_S_Lt         _ -> char '<'

        MO_U_Ge         _ -> ptext SLIT(">=")
        MO_U_Le         _ -> ptext SLIT("<=")
        MO_U_Gt         _ -> char '>'
        MO_U_Lt         _ -> char '<'

        -- Bitwise operations.  Not all of these may be supported at all
        -- sizes, and only integral MachReps are valid.
        MO_And          _ -> char '&'
        MO_Or           _ -> char '|'
        MO_Xor          _ -> char '^'
        MO_Not          _ -> char '~'
        MO_Shl          _ -> ptext SLIT("<<")
        MO_U_Shr        _ -> ptext SLIT(">>") -- unsigned shift right
        MO_S_Shr        _ -> ptext SLIT(">>") -- signed shift right

-- Conversions.  Some of these will be NOPs.
-- Floating-point conversions use the signed variant.
-- We won't know to generate (void*) casts here, but maybe from
-- context elsewhere

-- noop casts
        MO_U_Conv I8 I8     -> empty
        MO_U_Conv I16 I16   -> empty
        MO_U_Conv I32 I32   -> empty
        MO_U_Conv I64 I64   -> empty
        MO_U_Conv I128 I128 -> empty
        MO_S_Conv I8 I8     -> empty
        MO_S_Conv I16 I16   -> empty
        MO_S_Conv I32 I32   -> empty
        MO_S_Conv I64 I64   -> empty
        MO_S_Conv I128 I128 -> empty

	MO_U_Conv _from to  -> parens (machRepCType to)
	MO_S_Conv _from to  -> parens (machRepSignedCType to)

        _ -> panic "PprC.pprMachOp_for_C: unknown machop"

signedOp :: MachOp -> Bool
signedOp (MO_S_Quot _)	 = True
signedOp (MO_S_Rem  _)	 = True
signedOp (MO_S_Neg  _)	 = True
signedOp (MO_S_Ge   _)	 = True
signedOp (MO_S_Le   _)	 = True
signedOp (MO_S_Gt   _)	 = True
signedOp (MO_S_Lt   _)	 = True
signedOp (MO_S_Shr  _)	 = True
signedOp (MO_S_Conv _ _) = True
signedOp _ = False

-- ---------------------------------------------------------------------
-- tend to be implemented by foreign calls

pprCallishMachOp_for_C :: CallishMachOp -> SDoc

pprCallishMachOp_for_C mop 
    = case mop of
        MO_F64_Pwr  -> ptext SLIT("pow")
        MO_F64_Sin  -> ptext SLIT("sin")
        MO_F64_Cos  -> ptext SLIT("cos")
        MO_F64_Tan  -> ptext SLIT("tan")
        MO_F64_Sinh -> ptext SLIT("sinh")
        MO_F64_Cosh -> ptext SLIT("cosh")
        MO_F64_Tanh -> ptext SLIT("tanh")
        MO_F64_Asin -> ptext SLIT("asin")
        MO_F64_Acos -> ptext SLIT("acos")
        MO_F64_Atan -> ptext SLIT("atan")
        MO_F64_Log  -> ptext SLIT("log")
        MO_F64_Exp  -> ptext SLIT("exp")
        MO_F64_Sqrt -> ptext SLIT("sqrt")
        MO_F32_Pwr  -> ptext SLIT("powf")
        MO_F32_Sin  -> ptext SLIT("sinf")
        MO_F32_Cos  -> ptext SLIT("cosf")
        MO_F32_Tan  -> ptext SLIT("tanf")
        MO_F32_Sinh -> ptext SLIT("sinhf")
        MO_F32_Cosh -> ptext SLIT("coshf")
        MO_F32_Tanh -> ptext SLIT("tanhf")
        MO_F32_Asin -> ptext SLIT("asinf")
        MO_F32_Acos -> ptext SLIT("acosf")
        MO_F32_Atan -> ptext SLIT("atanf")
        MO_F32_Log  -> ptext SLIT("logf")
        MO_F32_Exp  -> ptext SLIT("expf")
        MO_F32_Sqrt -> ptext SLIT("sqrtf")
	MO_WriteBarrier -> ptext SLIT("write_barrier")

-- ---------------------------------------------------------------------
-- Useful #defines
--

mkJMP_, mkFN_, mkIF_ :: SDoc -> SDoc

mkJMP_ i = ptext SLIT("JMP_") <> parens i
mkFN_  i = ptext SLIT("FN_")  <> parens i -- externally visible function
mkIF_  i = ptext SLIT("IF_")  <> parens i -- locally visible


mkFB_, mkFE_ :: SDoc
mkFB_ = ptext SLIT("FB_") -- function code begin
mkFE_ = ptext SLIT("FE_") -- function code end

-- from includes/Stg.h
--
mkC_,mkW_,mkP_,mkPP_,mkI_,mkA_,mkD_,mkF_,mkB_,mkL_,mkLI_,mkLW_ :: SDoc

mkC_  = ptext SLIT("(C_)")        -- StgChar
mkW_  = ptext SLIT("(W_)")        -- StgWord
mkP_  = ptext SLIT("(P_)")        -- StgWord*
mkPP_ = ptext SLIT("(PP_)")       -- P_*
mkI_  = ptext SLIT("(I_)")        -- StgInt
mkA_  = ptext SLIT("(A_)")        -- StgAddr
mkD_  = ptext SLIT("(D_)")        -- const StgWord*
mkF_  = ptext SLIT("(F_)")        -- StgFunPtr
mkB_  = ptext SLIT("(B_)")        -- StgByteArray
mkL_  = ptext SLIT("(L_)")        -- StgClosurePtr

mkLI_ = ptext SLIT("(LI_)")       -- StgInt64
mkLW_ = ptext SLIT("(LW_)")       -- StgWord64


-- ---------------------------------------------------------------------
--
-- Assignments
--
-- Generating assignments is what we're all about, here
--
pprAssign :: CmmReg -> CmmExpr -> SDoc

-- dest is a reg, rhs is a reg
pprAssign r1 (CmmReg r2)
   | not (isStrangeTypeReg r1) && not (isStrangeTypeReg r2)
   || isPtrReg r1 && isPtrReg r2
   = hcat [ pprAsPtrReg r1, equals, pprAsPtrReg r2, semi ]

-- dest is a reg, rhs is a CmmRegOff
pprAssign r1 (CmmRegOff r2 off)
   | not (isStrangeTypeReg r1) && not (isStrangeTypeReg r2)
   || isPtrReg r1 && isPtrReg r2
   = hcat [ pprAsPtrReg r1, equals, pprAsPtrReg r2, op, int off', semi ]
  where
	off1 | isPtrReg r2 = off `shiftR` wordShift
	     | otherwise   = off

	(op,off') | off >= 0  = (char '+', off1)
		  | otherwise = (char '-', -off1)

-- dest is a reg, rhs is anything.
-- We can't cast the lvalue, so we have to cast the rhs if necessary.  Casting
-- the lvalue elicits a warning from new GCC versions (3.4+).
pprAssign r1 r2
  | isPtrReg r1
  = pprAsPtrReg r1 <> ptext SLIT(" = ") <> mkP_ <> pprExpr1 r2 <> semi
  | Just ty <- strangeRegType r1
  = pprReg r1 <> ptext SLIT(" = ") <> parens ty <> pprExpr1 r2 <> semi
  | otherwise
  = pprReg r1 <> ptext SLIT(" = ") <> pprExpr r2 <> semi

-- ---------------------------------------------------------------------
-- Registers

pprCastReg reg
   | isStrangeTypeReg reg = mkW_ <> pprReg reg
   | otherwise            = pprReg reg

-- True if the register has type StgPtr in C, otherwise it has an
-- integer type.  We need to take care with pointer arithmetic on registers
-- with type StgPtr.
isPtrReg :: CmmReg -> Bool
isPtrReg (CmmLocal _) = False
isPtrReg (CmmGlobal r) = isPtrGlobalReg r

isPtrGlobalReg :: GlobalReg -> Bool
isPtrGlobalReg (VanillaReg n) 	= True
isPtrGlobalReg Sp 		= True
isPtrGlobalReg Hp 		= True
isPtrGlobalReg HpLim 		= True
isPtrGlobalReg SpLim 		= True
isPtrGlobalReg _ 		= False

-- True if in C this register doesn't have the type given by 
-- (machRepCType (cmmRegRep reg)), so it has to be cast.
isStrangeTypeReg :: CmmReg -> Bool
isStrangeTypeReg (CmmLocal _) 	= False
isStrangeTypeReg (CmmGlobal g) 	= isStrangeTypeGlobal g

isStrangeTypeGlobal :: GlobalReg -> Bool
isStrangeTypeGlobal CurrentTSO		= True
isStrangeTypeGlobal CurrentNursery 	= True
isStrangeTypeGlobal BaseReg	 	= True
isStrangeTypeGlobal r 			= isPtrGlobalReg r

strangeRegType :: CmmReg -> Maybe SDoc
strangeRegType (CmmGlobal CurrentTSO) = Just (ptext SLIT("struct StgTSO_ *"))
strangeRegType (CmmGlobal CurrentNursery) = Just (ptext SLIT("struct bdescr_ *"))
strangeRegType (CmmGlobal BaseReg) = Just (ptext SLIT("struct StgRegTable_ *"))
strangeRegType _ = Nothing

-- pprReg just prints the register name.
--
pprReg :: CmmReg -> SDoc
pprReg r = case r of
        CmmLocal  local  -> pprLocalReg local
        CmmGlobal global -> pprGlobalReg global
		
pprAsPtrReg :: CmmReg -> SDoc
pprAsPtrReg (CmmGlobal (VanillaReg n)) = char 'R' <> int n <> ptext SLIT(".p")
pprAsPtrReg other_reg = pprReg other_reg

pprGlobalReg :: GlobalReg -> SDoc
pprGlobalReg gr = case gr of
    VanillaReg n   -> char 'R' <> int n  <> ptext SLIT(".w")
    FloatReg   n   -> char 'F' <> int n
    DoubleReg  n   -> char 'D' <> int n
    LongReg    n   -> char 'L' <> int n
    Sp             -> ptext SLIT("Sp")
    SpLim          -> ptext SLIT("SpLim")
    Hp             -> ptext SLIT("Hp")
    HpLim          -> ptext SLIT("HpLim")
    CurrentTSO     -> ptext SLIT("CurrentTSO")
    CurrentNursery -> ptext SLIT("CurrentNursery")
    HpAlloc        -> ptext SLIT("HpAlloc")
    BaseReg        -> ptext SLIT("BaseReg")
    GCEnter1       -> ptext SLIT("stg_gc_enter_1")
    GCFun          -> ptext SLIT("stg_gc_fun")

pprLocalReg :: LocalReg -> SDoc
pprLocalReg (LocalReg uniq _rep) = char '_' <> ppr uniq

-- -----------------------------------------------------------------------------
-- Foreign Calls

pprCall :: SDoc -> CCallConv -> [(CmmReg,MachHint)] -> [(CmmExpr,MachHint)]
	-> Maybe [GlobalReg] -> SDoc

pprCall ppr_fn cconv results args vols
  | not (is_cish cconv)
  = panic "pprCall: unknown calling convention"

  | otherwise
  = save vols $$
    ptext SLIT("CALLER_SAVE_SYSTEM") $$
#if x86_64_TARGET_ARCH
	-- HACK around gcc optimisations.
	-- x86_64 needs a __DISCARD__() here, to create a barrier between
	-- putting the arguments into temporaries and passing the arguments
	-- to the callee, because the argument expressions may refer to
	-- machine registers that are also used for passing arguments in the
	-- C calling convention.
    (if (not opt_Unregisterised) 
	then ptext SLIT("__DISCARD__();") 
	else empty) $$
#endif
    ppr_assign results (ppr_fn <> parens (commafy (map pprArg args))) <> semi $$
    ptext SLIT("CALLER_RESTORE_SYSTEM") $$
    restore vols
  where 
     ppr_assign []           rhs = rhs
     ppr_assign [(reg@(CmmGlobal BaseReg), hint)] rhs
	 | Just ty <- strangeRegType reg
	 = ptext SLIT("ASSIGN_BaseReg") <> parens (parens ty <> rhs)
	 -- BaseReg is special, sometimes it isn't an lvalue and we
	 -- can't assign to it.
     ppr_assign [(one,hint)] rhs
	 | Just ty <- strangeRegType one
	 = pprReg one <> ptext SLIT(" = ") <> parens ty <> rhs
	 | otherwise
	 = pprReg one <> ptext SLIT(" = ")
		 <> pprUnHint hint (cmmRegRep one) <> rhs
     ppr_assign _other _rhs = panic "pprCall: multiple results"

     pprArg (expr, PtrHint)
   	= cCast (ptext SLIT("void *")) expr
	-- see comment by machRepHintCType below
     pprArg (expr, SignedHint)
	= cCast (machRepSignedCType (cmmExprRep expr)) expr
     pprArg (expr, _other)
	= pprExpr expr

     pprUnHint PtrHint    rep = parens (machRepCType rep)
     pprUnHint SignedHint rep = parens (machRepCType rep)
     pprUnHint _          _   = empty

     save    = save_restore SLIT("CALLER_SAVE")
     restore = save_restore SLIT("CALLER_RESTORE")

	-- Nothing says "I don't know what's live; save everything"
	-- CALLER_SAVE_USER is defined in ghc/includes/Regs.h
     save_restore txt Nothing     = ptext txt <> ptext SLIT("_USER")
     save_restore txt (Just these) = vcat (map saveRestoreGlobal these)
	where saveRestoreGlobal r = ptext txt <> char '_' <> pprGlobalRegName r

pprGlobalRegName :: GlobalReg -> SDoc
pprGlobalRegName gr = case gr of
    VanillaReg n   -> char 'R' <> int n  -- without the .w suffix
    _              -> pprGlobalReg gr

-- Currently we only have these two calling conventions, but this might
-- change in the future...
is_cish CCallConv   = True
is_cish StdCallConv = True

-- ---------------------------------------------------------------------
-- Find and print local and external declarations for a list of
-- Cmm statements.
-- 
pprTempAndExternDecls :: [CmmBasicBlock] -> (SDoc{-temps-}, SDoc{-externs-})
pprTempAndExternDecls stmts 
  = (vcat (map pprTempDecl (eltsUFM temps)), 
     vcat (map (pprExternDecl False{-ToDo-}) (keysFM lbls)))
  where (temps, lbls) = runTE (mapM_ te_BB stmts)

pprDataExterns :: [CmmStatic] -> SDoc
pprDataExterns statics
  = vcat (map (pprExternDecl False{-ToDo-}) (keysFM lbls))
  where (_, lbls) = runTE (mapM_ te_Static statics)

pprTempDecl :: LocalReg -> SDoc
pprTempDecl l@(LocalReg _uniq rep)
  = hcat [ machRepCType rep, space, pprLocalReg l, semi ]

pprExternDecl :: Bool -> CLabel -> SDoc
pprExternDecl in_srt lbl
  -- do not print anything for "known external" things
  | not (needsCDecl lbl) = empty
  | otherwise		    = 
	hcat [ visibility, label_type (labelType lbl), 
	       lparen, dyn_wrapper (pprCLabel lbl), text ");" ]
 where
  dyn_wrapper d
    | in_srt && labelDynamic lbl = text "DLL_IMPORT_DATA_VAR" <> parens d
    | otherwise			 = d

  label_type CodeLabel = ptext SLIT("F_")
  label_type DataLabel = ptext SLIT("I_")

  visibility
     | externallyVisibleCLabel lbl = char 'E'
     | otherwise		   = char 'I'


type TEState = (UniqSet LocalReg, FiniteMap CLabel ())
newtype TE a = TE { unTE :: TEState -> (a, TEState) }

instance Monad TE where
   TE m >>= k  = TE $ \s -> case m s of (a, s') -> unTE (k a) s'
   return a    = TE $ \s -> (a, s)

te_lbl :: CLabel -> TE ()
te_lbl lbl = TE $ \(temps,lbls) -> ((), (temps, addToFM lbls lbl ()))

te_temp :: LocalReg -> TE ()
te_temp r = TE $ \(temps,lbls) -> ((), (addOneToUniqSet temps r, lbls))

runTE :: TE () -> TEState
runTE (TE m) = snd (m (emptyUniqSet, emptyFM))

te_Static :: CmmStatic -> TE ()
te_Static (CmmStaticLit lit) = te_Lit lit
te_Static _ = return ()

te_BB :: CmmBasicBlock -> TE ()
te_BB (BasicBlock _ ss)		= mapM_ te_Stmt ss

te_Lit :: CmmLit -> TE ()
te_Lit (CmmLabel l) = te_lbl l
te_Lit (CmmLabelOff l _) = te_lbl l
te_Lit (CmmLabelDiffOff l1 l2 _) = te_lbl l1
te_Lit _ = return ()

te_Stmt :: CmmStmt -> TE ()
te_Stmt (CmmAssign r e)		= te_Reg r >> te_Expr e
te_Stmt (CmmStore l r)		= te_Expr l >> te_Expr r
te_Stmt (CmmCall _ rs es _)	= mapM_ (te_Reg.fst) rs >>
				  mapM_ (te_Expr.fst) es
te_Stmt (CmmCondBranch e _)	= te_Expr e
te_Stmt (CmmSwitch e _)		= te_Expr e
te_Stmt (CmmJump e _)		= te_Expr e
te_Stmt _			= return ()

te_Expr :: CmmExpr -> TE ()
te_Expr (CmmLit lit)		= te_Lit lit
te_Expr (CmmLoad e _)		= te_Expr e
te_Expr (CmmReg r)		= te_Reg r
te_Expr (CmmMachOp _ es) 	= mapM_ te_Expr es
te_Expr (CmmRegOff r _) 	= te_Reg r

te_Reg :: CmmReg -> TE ()
te_Reg (CmmLocal l) = te_temp l
te_Reg _            = return ()


-- ---------------------------------------------------------------------
-- C types for MachReps

cCast :: SDoc -> CmmExpr -> SDoc
cCast ty expr = parens ty <> pprExpr1 expr

-- This is for finding the types of foreign call arguments.  For a pointer
-- argument, we always cast the argument to (void *), to avoid warnings from
-- the C compiler.
machRepHintCType :: MachRep -> MachHint -> SDoc
machRepHintCType rep PtrHint    = ptext SLIT("void *")
machRepHintCType rep SignedHint = machRepSignedCType rep
machRepHintCType rep _other     = machRepCType rep

machRepPtrCType :: MachRep -> SDoc
machRepPtrCType r | r == wordRep = ptext SLIT("P_")
	          | otherwise    = machRepCType r <> char '*'

machRepCType :: MachRep -> SDoc
machRepCType r | r == wordRep = ptext SLIT("W_")
	       | otherwise    = sized_type
  where sized_type = case r of
			I8	-> ptext SLIT("StgWord8")
			I16	-> ptext SLIT("StgWord16")
			I32	-> ptext SLIT("StgWord32")
			I64	-> ptext SLIT("StgWord64")
			F32	-> ptext SLIT("StgFloat") -- ToDo: correct?
			F64	-> ptext SLIT("StgDouble")
			_  -> panic "machRepCType"

machRepSignedCType :: MachRep -> SDoc
machRepSignedCType r | r == wordRep = ptext SLIT("I_")
                     | otherwise    = sized_type
  where sized_type = case r of
			I8	-> ptext SLIT("StgInt8")
			I16	-> ptext SLIT("StgInt16")
			I32	-> ptext SLIT("StgInt32")
			I64	-> ptext SLIT("StgInt64")
			F32	-> ptext SLIT("StgFloat") -- ToDo: correct?
			F64	-> ptext SLIT("StgDouble")
			_ -> panic "machRepCType"

-- ---------------------------------------------------------------------
-- print strings as valid C strings

pprStringInCStyle :: [Word8] -> SDoc
pprStringInCStyle s = doubleQuotes (text (concatMap charToC s))

charToC :: Word8 -> String
charToC w = 
  case chr (fromIntegral w) of
	'\"' -> "\\\""
	'\'' -> "\\\'"
	'\\' -> "\\\\"
	c | c >= ' ' && c <= '~' -> [c]
          | otherwise -> ['\\',
                         chr (ord '0' + ord c `div` 64),
                         chr (ord '0' + ord c `div` 8 `mod` 8),
                         chr (ord '0' + ord c         `mod` 8)]

-- ---------------------------------------------------------------------------
-- Initialising static objects with floating-point numbers.  We can't
-- just emit the floating point number, because C will cast it to an int
-- by rounding it.  We want the actual bit-representation of the float.

-- This is a hack to turn the floating point numbers into ints that we
-- can safely initialise to static locations.

big_doubles 
  | machRepByteWidth F64 == 2 * wORD_SIZE  = True
  | machRepByteWidth F64 == wORD_SIZE      = False
  | otherwise = panic "big_doubles"

#if __GLASGOW_HASKELL__ >= 504
newFloatArray :: (Int,Int) -> ST s (STUArray s Int Float)
newFloatArray = newArray_

newDoubleArray :: (Int,Int) -> ST s (STUArray s Int Double)
newDoubleArray = newArray_

castFloatToIntArray :: STUArray s Int Float -> ST s (STUArray s Int Int)
castFloatToIntArray = castSTUArray

castDoubleToIntArray :: STUArray s Int Double -> ST s (STUArray s Int Int)
castDoubleToIntArray = castSTUArray

writeFloatArray :: STUArray s Int Float -> Int -> Float -> ST s ()
writeFloatArray = writeArray

writeDoubleArray :: STUArray s Int Double -> Int -> Double -> ST s ()
writeDoubleArray = writeArray

readIntArray :: STUArray s Int Int -> Int -> ST s Int
readIntArray = readArray

#else

castFloatToIntArray :: MutableByteArray s t -> ST s (MutableByteArray s t)
castFloatToIntArray = return

castDoubleToIntArray :: MutableByteArray s t -> ST s (MutableByteArray s t)
castDoubleToIntArray = return

#endif

-- floats are always 1 word
floatToWord :: Rational -> CmmLit
floatToWord r
  = runST (do
	arr <- newFloatArray ((0::Int),0)
	writeFloatArray arr 0 (fromRational r)
	arr' <- castFloatToIntArray arr
	i <- readIntArray arr' 0
	return (CmmInt (toInteger i) wordRep)
    )

doubleToWords :: Rational -> [CmmLit]
doubleToWords r
  | big_doubles				-- doubles are 2 words
  = runST (do
	arr <- newDoubleArray ((0::Int),1)
	writeDoubleArray arr 0 (fromRational r)
	arr' <- castDoubleToIntArray arr
	i1 <- readIntArray arr' 0
	i2 <- readIntArray arr' 1
	return [ CmmInt (toInteger i1) wordRep
	       , CmmInt (toInteger i2) wordRep
	       ]
    )
  | otherwise				-- doubles are 1 word
  = runST (do
	arr <- newDoubleArray ((0::Int),0)
	writeDoubleArray arr 0 (fromRational r)
	arr' <- castDoubleToIntArray arr
	i <- readIntArray arr' 0
	return [ CmmInt (toInteger i) wordRep ]
    )

-- ---------------------------------------------------------------------------
-- Utils

wordShift :: Int
wordShift = machRepLogWidth wordRep

commafy :: [SDoc] -> SDoc
commafy xs = hsep $ punctuate comma xs

-- Print in C hex format: 0x13fa
pprHexVal :: Integer -> MachRep -> SDoc
pprHexVal 0 _ = ptext SLIT("0x0")
pprHexVal w rep
  | w < 0     = parens (char '-' <> ptext SLIT("0x") <> go (-w) <> repsuffix rep)
  | otherwise = ptext SLIT("0x") <> go w <> repsuffix rep
  where
  	-- type suffix for literals:
	-- Integer literals are unsigned in Cmm/C.  We explicitly cast to
	-- signed values for doing signed operations, but at all other
	-- times values are unsigned.  This also helps eliminate occasional
	-- warnings about integer overflow from gcc.

	-- on 32-bit platforms, add "ULL" to 64-bit literals
      repsuffix I64 | wORD_SIZE == 4 = ptext SLIT("ULL")
      	-- on 64-bit platforms with 32-bit int, add "L" to 64-bit literals
      repsuffix I64 | cINT_SIZE == 4 = ptext SLIT("UL")
      repsuffix _ = char 'U'
      
      go 0 = empty
      go w' = go q <> dig
           where
             (q,r) = w' `quotRem` 16
             dig | r < 10    = char (chr (fromInteger r + ord '0'))
                 | otherwise = char (chr (fromInteger r - 10 + ord 'a'))

