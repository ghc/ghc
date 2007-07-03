----------------------------------------------------------------------------
--
-- Pretty-printing of Cmm as (a superset of) C--
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

--
-- This is where we walk over Cmm emitting an external representation,
-- suitable for parsing, in a syntax strongly reminiscent of C--. This
-- is the "External Core" for the Cmm layer.
--
-- As such, this should be a well-defined syntax: we want it to look nice.
-- Thus, we try wherever possible to use syntax defined in [1],
-- "The C-- Reference Manual", http://www.cminusminus.org/. We differ
-- slightly, in some cases. For one, we use I8 .. I64 for types, rather
-- than C--'s bits8 .. bits64.
--
-- We try to ensure that all information available in the abstract
-- syntax is reproduced, or reproducible, in the concrete syntax.
-- Data that is not in printed out can be reconstructed according to
-- conventions used in the pretty printer. There are at least two such
-- cases:
--      1) if a value has wordRep type, the type is not appended in the
--      output.
--      2) MachOps that operate over wordRep type are printed in a
--      C-style, rather than as their internal MachRep name.
--
-- These conventions produce much more readable Cmm output.
--
-- A useful example pass over Cmm is in nativeGen/MachCodeGen.hs
--

module PprCmm ( 	
	writeCmms, pprCmms, pprCmm, pprStmt, pprExpr
  ) where

#include "HsVersions.h"

import Cmm
import CmmUtils
import MachOp
import CLabel

import ForeignCall
import Unique
import Outputable
import FastString

import Data.List
import System.IO
import Data.Maybe

pprCmms :: (Outputable info) => [GenCmm CmmStatic info CmmStmt] -> SDoc
pprCmms cmms = pprCode CStyle (vcat (intersperse separator $ map ppr cmms))
        where
          separator = space $$ ptext SLIT("-------------------") $$ space

writeCmms :: Handle -> [Cmm] -> IO ()
writeCmms handle cmms = printForC handle (pprCmms cmms)

-----------------------------------------------------------------------------

instance (Outputable info) => Outputable (GenCmm CmmStatic info CmmStmt) where
    ppr c = pprCmm c

instance (Outputable info) => Outputable (GenCmmTop CmmStatic info CmmStmt) where
    ppr t = pprTop t

instance Outputable CmmBasicBlock where
    ppr b = pprBBlock b

instance Outputable CmmStmt where
    ppr s = pprStmt s

instance Outputable CmmExpr where
    ppr e = pprExpr e

instance Outputable CmmReg where
    ppr e = pprReg e

instance Outputable LocalReg where
    ppr e = pprLocalReg e

instance Outputable GlobalReg where
    ppr e = pprGlobalReg e

instance Outputable CmmStatic where
    ppr e = pprStatic e

instance Outputable CmmInfo where
    ppr e = pprInfo e

-----------------------------------------------------------------------------

pprCmm :: (Outputable info) => GenCmm CmmStatic info CmmStmt -> SDoc
pprCmm (Cmm tops) = vcat $ intersperse (text "") $ map pprTop tops

-- --------------------------------------------------------------------------
-- Top level `procedure' blocks.
--
pprTop :: (Outputable info) => GenCmmTop CmmStatic info CmmStmt -> SDoc
pprTop (CmmProc info lbl params blocks )

  = vcat [ pprCLabel lbl <> parens (commafy $ map ppr params) <+> lbrace
         , nest 8 $ lbrace <+> ppr info $$ rbrace
         , nest 4 $ vcat (map ppr blocks)
         , rbrace ]

-- --------------------------------------------------------------------------
-- We follow [1], 4.5
--
--      section "data" { ... }
--
pprTop (CmmData section ds) = 
    (hang (pprSection section <+> lbrace) 4 (vcat (map pprStatic ds)))
    $$ rbrace

-- --------------------------------------------------------------------------
instance Outputable CmmSafety where
  ppr CmmUnsafe = ptext SLIT("_unsafe_call_")
  ppr (CmmSafe srt) = ppr srt

-- --------------------------------------------------------------------------
-- Info tables. The current pretty printer needs refinement
-- but will work for now.
--
-- For ideas on how to refine it, they used to be printed in the
-- style of C--'s 'stackdata' declaration, just inside the proc body,
-- and were labelled with the procedure name ++ "_info".
pprInfo (CmmInfo gc_target update_frame CmmNonInfoTable) =
    vcat [ptext SLIT("gc_target: ") <>
                maybe (ptext SLIT("<none>")) pprBlockId gc_target,
          ptext SLIT("update_frame: ") <>
                maybe (ptext SLIT("<none>")) pprUpdateFrame update_frame]
pprInfo (CmmInfo gc_target update_frame
         (CmmInfoTable (ProfilingInfo closure_type closure_desc) tag info)) =
    vcat [ptext SLIT("gc_target: ") <>
                maybe (ptext SLIT("<none>")) pprBlockId gc_target,
          ptext SLIT("update_frame: ") <>
                maybe (ptext SLIT("<none>")) pprUpdateFrame update_frame,
          ptext SLIT("type: ") <> pprLit closure_type,
          ptext SLIT("desc: ") <> pprLit closure_desc,
          ptext SLIT("tag: ") <> integer (toInteger tag),
          pprTypeInfo info]

pprTypeInfo (ConstrInfo layout constr descr) =
    vcat [ptext SLIT("ptrs: ") <> integer (toInteger (fst layout)),
          ptext SLIT("nptrs: ") <> integer (toInteger (snd layout)),
          ptext SLIT("constructor: ") <> integer (toInteger constr),
          pprLit descr]
pprTypeInfo (FunInfo layout srt fun_type arity args slow_entry) =
    vcat [ptext SLIT("ptrs: ") <> integer (toInteger (fst layout)),
          ptext SLIT("nptrs: ") <> integer (toInteger (snd layout)),
          ptext SLIT("srt: ") <> ppr srt,
          ptext SLIT("fun_type: ") <> integer (toInteger fun_type),
          ptext SLIT("arity: ") <> integer (toInteger arity),
          --ppr args, -- TODO: needs to be printed
          ptext SLIT("slow: ") <> pprLit slow_entry
         ]
pprTypeInfo (ThunkInfo layout srt) =
    vcat [ptext SLIT("ptrs: ") <> integer (toInteger (fst layout)),
          ptext SLIT("nptrs: ") <> integer (toInteger (snd layout)),
          ptext SLIT("srt: ") <> ppr srt]
pprTypeInfo (ThunkSelectorInfo offset srt) =
    vcat [ptext SLIT("ptrs: ") <> integer (toInteger offset),
          ptext SLIT("srt: ") <> ppr srt]
pprTypeInfo (ContInfo stack srt) =
    vcat [ptext SLIT("stack: ") <> ppr stack,
          ptext SLIT("srt: ") <> ppr srt]

pprUpdateFrame :: UpdateFrame -> SDoc
pprUpdateFrame (UpdateFrame expr args) = 
    hcat [ ptext SLIT("jump")
         , space
         , if isTrivialCmmExpr expr
                then pprExpr expr
                else case expr of
                    CmmLoad (CmmReg _) _ -> pprExpr expr 
                    _ -> parens (pprExpr expr)
         , space
         , parens  ( commafy $ map ppr args ) ]


-- --------------------------------------------------------------------------
-- Basic blocks look like assembly blocks.
--      lbl: stmt ; stmt ; .. 
pprBBlock :: CmmBasicBlock -> SDoc
pprBBlock (BasicBlock ident stmts) =
    hang (pprBlockId ident <> colon) 4 (vcat (map ppr stmts))

-- --------------------------------------------------------------------------
-- Statements. C-- usually, exceptions to this should be obvious.
--
pprStmt :: CmmStmt -> SDoc    
pprStmt stmt = case stmt of

    -- ;
    CmmNop -> semi

    --  // text
    CmmComment s -> text "//" <+> ftext s

    -- reg = expr;
    CmmAssign reg expr -> ppr reg <+> equals <+> ppr expr <> semi

    -- rep[lv] = expr;
    CmmStore lv expr -> rep <> brackets(ppr lv) <+> equals <+> ppr expr <> semi
        where
          rep = ppr ( cmmExprRep expr )

    -- call "ccall" foo(x, y)[r1, r2];
    -- ToDo ppr volatile
    CmmCall (CmmForeignCall fn cconv) results args safety ->
        hcat [ if null results
                  then empty
                  else parens (commafy $ map ppr results) <>
                       ptext SLIT(" = "),
               ptext SLIT("call"), space, 
               doubleQuotes(ppr cconv), space,
               target fn, parens  ( commafy $ map ppr args ),
               brackets (ppr safety), semi ]
        where
            target (CmmLit lit) = pprLit lit
            target fn'          = parens (ppr fn')

    CmmCall (CmmPrim op) results args safety ->
        pprStmt (CmmCall (CmmForeignCall (CmmLit lbl) CCallConv)
                        results args safety)
        where
          lbl = CmmLabel (mkForeignLabel (mkFastString (show op)) Nothing False)

    CmmBranch ident          -> genBranch ident
    CmmCondBranch expr ident -> genCondBranch expr ident
    CmmJump expr params      -> genJump expr params
    CmmReturn params         -> genReturn params
    CmmSwitch arg ids        -> genSwitch arg ids

-- --------------------------------------------------------------------------
-- goto local label. [1], section 6.6
--
--     goto lbl;
--
genBranch :: BlockId -> SDoc
genBranch ident = 
    ptext SLIT("goto") <+> pprBlockId ident <> semi

-- --------------------------------------------------------------------------
-- Conditional. [1], section 6.4
--
--     if (expr) { goto lbl; } 
--
genCondBranch :: CmmExpr -> BlockId -> SDoc
genCondBranch expr ident =
    hsep [ ptext SLIT("if")
         , parens(ppr expr)
         , ptext SLIT("goto")
         , pprBlockId ident <> semi ]

-- --------------------------------------------------------------------------
-- A tail call. [1], Section 6.9
--
--     jump foo(a, b, c);
--
genJump :: CmmExpr -> [(CmmExpr, MachHint)] -> SDoc
genJump expr args = 

    hcat [ ptext SLIT("jump")
         , space
         , if isTrivialCmmExpr expr
                then pprExpr expr
                else case expr of
                    CmmLoad (CmmReg _) _ -> pprExpr expr 
                    _ -> parens (pprExpr expr)
         , space
         , parens  ( commafy $ map ppr args )
         , semi ]

-- --------------------------------------------------------------------------
-- Return from a function. [1], Section 6.8.2 of version 1.128
--
--     return (a, b, c);
--
genReturn :: [(CmmExpr, MachHint)] -> SDoc
genReturn args = 

    hcat [ ptext SLIT("return")
         , space
         , parens  ( commafy $ map ppr args )
         , semi ]

-- --------------------------------------------------------------------------
-- Tabled jump to local label
--
-- The syntax is from [1], section 6.5
--
--      switch [0 .. n] (expr) { case ... ; }
--
genSwitch :: CmmExpr -> [Maybe BlockId] -> SDoc
genSwitch expr maybe_ids 

    = let pairs = groupBy snds (zip [0 .. ] maybe_ids )

      in hang (hcat [ ptext SLIT("switch [0 .. ") 
                    , int (length maybe_ids - 1)
                    , ptext SLIT("] ")
                    , if isTrivialCmmExpr expr
                        then pprExpr expr
                        else parens (pprExpr expr)
                    , ptext SLIT(" {") 
                    ]) 
            4 (vcat ( map caseify pairs )) $$ rbrace

    where
      snds a b = (snd a) == (snd b)

      caseify :: [(Int,Maybe BlockId)] -> SDoc
      caseify ixs@((i,Nothing):_)
        = ptext SLIT("/* impossible: ") <> hcat (intersperse comma (map (int.fst) ixs))
		<> ptext SLIT(" */")
      caseify as 
        = let (is,ids) = unzip as 
          in hsep [ ptext SLIT("case")
                  , hcat (punctuate comma (map int is))
                  , ptext SLIT(": goto")
                  , pprBlockId (head [ id | Just id <- ids]) <> semi ]

-- --------------------------------------------------------------------------
-- Expressions
--

pprExpr :: CmmExpr -> SDoc
pprExpr e 
    = case e of
        CmmRegOff reg i -> 
		pprExpr (CmmMachOp (MO_Add rep)
			   [CmmReg reg, CmmLit (CmmInt (fromIntegral i) rep)])
		where rep = cmmRegRep reg	
	CmmLit lit -> pprLit lit
	_other     -> pprExpr1 e

-- Here's the precedence table from CmmParse.y:
-- %nonassoc '>=' '>' '<=' '<' '!=' '=='
-- %left '|'
-- %left '^'
-- %left '&'
-- %left '>>' '<<'
-- %left '-' '+'
-- %left '/' '*' '%'
-- %right '~'

-- We just cope with the common operators for now, the rest will get
-- a default conservative behaviour.

-- %nonassoc '>=' '>' '<=' '<' '!=' '=='
pprExpr1 (CmmMachOp op [x,y]) | Just doc <- infixMachOp1 op
   = pprExpr7 x <+> doc <+> pprExpr7 y
pprExpr1 e = pprExpr7 e

infixMachOp1 (MO_Eq     _) = Just (ptext SLIT("=="))
infixMachOp1 (MO_Ne     _) = Just (ptext SLIT("!="))
infixMachOp1 (MO_Shl    _) = Just (ptext SLIT("<<"))
infixMachOp1 (MO_U_Shr  _) = Just (ptext SLIT(">>"))
infixMachOp1 (MO_U_Ge   _) = Just (ptext SLIT(">="))
infixMachOp1 (MO_U_Le   _) = Just (ptext SLIT("<="))
infixMachOp1 (MO_U_Gt   _) = Just (char '>')
infixMachOp1 (MO_U_Lt   _) = Just (char '<')
infixMachOp1 _             = Nothing

-- %left '-' '+'
pprExpr7 (CmmMachOp (MO_Add rep1) [x, CmmLit (CmmInt i rep2)]) | i < 0
   = pprExpr7 (CmmMachOp (MO_Sub rep1) [x, CmmLit (CmmInt (negate i) rep2)])
pprExpr7 (CmmMachOp op [x,y]) | Just doc <- infixMachOp7 op
   = pprExpr7 x <+> doc <+> pprExpr8 y
pprExpr7 e = pprExpr8 e

infixMachOp7 (MO_Add _)  = Just (char '+')
infixMachOp7 (MO_Sub _)  = Just (char '-')
infixMachOp7 _           = Nothing

-- %left '/' '*' '%'
pprExpr8 (CmmMachOp op [x,y]) | Just doc <- infixMachOp8 op
   = pprExpr8 x <+> doc <+> pprExpr9 y
pprExpr8 e = pprExpr9 e

infixMachOp8 (MO_U_Quot _) = Just (char '/')
infixMachOp8 (MO_Mul _)    = Just (char '*')
infixMachOp8 (MO_U_Rem _)  = Just (char '%')
infixMachOp8 _             = Nothing

pprExpr9 :: CmmExpr -> SDoc
pprExpr9 e = 
   case e of
        CmmLit    lit       -> pprLit1 lit
        CmmLoad   expr rep  -> ppr rep <> brackets( ppr expr )
        CmmReg    reg       -> ppr reg
        CmmRegOff reg off   -> parens (ppr reg <+> char '+' <+> int off)
	CmmMachOp mop args  -> genMachOp mop args

genMachOp :: MachOp -> [CmmExpr] -> SDoc
genMachOp mop args
   | Just doc <- infixMachOp mop = case args of
        -- dyadic
        [x,y] -> pprExpr9 x <+> doc <+> pprExpr9 y

        -- unary
        [x]   -> doc <> pprExpr9 x

        _     -> pprTrace "PprCmm.genMachOp: machop with strange number of args"
                          (pprMachOp mop <+>
                            parens (hcat $ punctuate comma (map pprExpr args)))
                          empty

   | isJust (infixMachOp1 mop)
   || isJust (infixMachOp7 mop)
   || isJust (infixMachOp8 mop)	 = parens (pprExpr (CmmMachOp mop args))

   | otherwise = char '%' <> ppr_op <> parens (commafy (map pprExpr args))
        where ppr_op = text (map (\c -> if c == ' ' then '_' else c)
                                 (show mop))
                -- replace spaces in (show mop) with underscores,

--
-- Unsigned ops on the word size of the machine get nice symbols.
-- All else get dumped in their ugly format.
--
infixMachOp :: MachOp -> Maybe SDoc
infixMachOp mop
	= case mop of
            MO_And    _ -> Just $ char '&'
            MO_Or     _ -> Just $ char '|'
            MO_Xor    _ -> Just $ char '^'
            MO_Not    _ -> Just $ char '~'
            MO_S_Neg  _ -> Just $ char '-' -- there is no unsigned neg :)
            _ -> Nothing

-- --------------------------------------------------------------------------
-- Literals.
--  To minimise line noise we adopt the convention that if the literal
--  has the natural machine word size, we do not append the type
--
pprLit :: CmmLit -> SDoc
pprLit lit = case lit of
    CmmInt i rep ->
        hcat [ (if i < 0 then parens else id)(integer i)
             , (if rep == wordRep 
                    then empty 
                    else space <> dcolon <+> ppr rep) ]

    CmmFloat f rep     -> hsep [ rational f, dcolon, ppr rep ]
    CmmLabel clbl      -> pprCLabel clbl
    CmmLabelOff clbl i -> pprCLabel clbl <> ppr_offset i
    CmmLabelDiffOff clbl1 clbl2 i -> pprCLabel clbl1 <> char '-'  
                                  <> pprCLabel clbl2 <> ppr_offset i

pprLit1 lit@(CmmLabelOff clbl i) = parens (pprLit lit)
pprLit1 lit                      = pprLit lit

ppr_offset :: Int -> SDoc
ppr_offset i
    | i==0      = empty
    | i>=0      = char '+' <> int i
    | otherwise = char '-' <> int (-i)

-- --------------------------------------------------------------------------
-- Static data.
--      Strings are printed as C strings, and we print them as I8[],
--      following C--
--
pprStatic :: CmmStatic -> SDoc
pprStatic s = case s of
    CmmStaticLit lit   -> nest 4 $ ptext SLIT("const") <+> pprLit lit <> semi
    CmmUninitialised i -> nest 4 $ text "I8" <> brackets (int i)
    CmmAlign i         -> nest 4 $ text "align" <+> int i
    CmmDataLabel clbl  -> pprCLabel clbl <> colon
    CmmString s'       -> nest 4 $ text "I8[]" <+> text (show s')

-- --------------------------------------------------------------------------
-- Registers, whether local (temps) or global
--
pprReg :: CmmReg -> SDoc
pprReg r 
    = case r of
        CmmLocal  local  -> pprLocalReg local
        CmmGlobal global -> pprGlobalReg global

--
-- We only print the type of the local reg if it isn't wordRep
--
pprLocalReg :: LocalReg -> SDoc
pprLocalReg (LocalReg uniq rep follow) 
    = hcat [ char '_', ppr uniq, ty ] where
  ty = if rep == wordRep && follow == KindNonPtr
                then empty
                else dcolon <> ptr <> ppr rep
  ptr = if follow == KindNonPtr
                then empty
                else doubleQuotes (text "ptr")

-- needs to be kept in syn with Cmm.hs.GlobalReg
--
pprGlobalReg :: GlobalReg -> SDoc
pprGlobalReg gr 
    = case gr of
        VanillaReg n   -> char 'R' <> int n
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
        GCEnter1       -> ptext SLIT("stg_gc_enter_1")
        GCFun          -> ptext SLIT("stg_gc_fun")
        BaseReg        -> ptext SLIT("BaseReg")
        PicBaseReg     -> ptext SLIT("PicBaseReg")

-- --------------------------------------------------------------------------
-- data sections
--
pprSection :: Section -> SDoc
pprSection s = case s of
    Text              -> section <+> doubleQuotes (ptext SLIT("text"))
    Data              -> section <+> doubleQuotes (ptext SLIT("data"))
    ReadOnlyData      -> section <+> doubleQuotes (ptext SLIT("readonly"))
    RelocatableReadOnlyData
                      -> section <+> doubleQuotes (ptext SLIT("relreadonly"))
    UninitialisedData -> section <+> doubleQuotes (ptext SLIT("uninitialised"))
    OtherSection s'   -> section <+> doubleQuotes (text s')
 where
    section = ptext SLIT("section")
       
-- --------------------------------------------------------------------------
-- Basic block ids
--
pprBlockId :: BlockId -> SDoc
pprBlockId b = ppr $ getUnique b

-----------------------------------------------------------------------------

commafy :: [SDoc] -> SDoc
commafy xs = hsep $ punctuate comma xs

