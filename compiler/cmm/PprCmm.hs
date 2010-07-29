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

module PprCmm
    ( writeCmms, pprCmms, pprCmm, pprStmt, pprExpr, 
      pprSection, pprStatic, pprLit
    )
where

import BlockId
import Cmm
import CmmUtils
import CLabel
import BasicTypes


import ForeignCall
import Outputable
import FastString

import Data.List
import System.IO
import Data.Maybe

-- Temp Jan08
import SMRep
import ClosureInfo
#include "../includes/rts/storage/FunTypes.h"


pprCmms :: (Outputable info, Outputable g) => [GenCmm CmmStatic info g] -> SDoc
pprCmms cmms = pprCode CStyle (vcat (intersperse separator $ map ppr cmms))
        where
          separator = space $$ ptext (sLit "-------------------") $$ space

writeCmms :: Handle -> [Cmm] -> IO ()
writeCmms handle cmms = printForC handle (pprCmms cmms)

-----------------------------------------------------------------------------

instance (Outputable d, Outputable info, Outputable g)
    => Outputable (GenCmm d info g) where
    ppr c = pprCmm c

instance (Outputable d, Outputable info, Outputable i)
	=> Outputable (GenCmmTop d info i) where
    ppr t = pprTop t

instance (Outputable instr) => Outputable (ListGraph instr) where
    ppr (ListGraph blocks) = vcat (map ppr blocks)

instance (Outputable instr) => Outputable (GenBasicBlock instr) where
    ppr b = pprBBlock b

instance Outputable CmmStmt where
    ppr s = pprStmt s

instance Outputable CmmExpr where
    ppr e = pprExpr e

instance Outputable CmmReg where
    ppr e = pprReg e

instance Outputable CmmLit where
    ppr l = pprLit l

instance Outputable LocalReg where
    ppr e = pprLocalReg e

instance Outputable Area where
    ppr e = pprArea e

instance Outputable GlobalReg where
    ppr e = pprGlobalReg e

instance Outputable CmmStatic where
    ppr e = pprStatic e

instance Outputable CmmInfo where
    ppr e = pprInfo e



-----------------------------------------------------------------------------

pprCmm :: (Outputable d, Outputable info, Outputable g) => GenCmm d info g -> SDoc
pprCmm (Cmm tops) = vcat $ intersperse blankLine $ map pprTop tops

-- --------------------------------------------------------------------------
-- Top level `procedure' blocks.
--
pprTop 	:: (Outputable d, Outputable info, Outputable i)
	=> GenCmmTop d info i -> SDoc

pprTop (CmmProc info lbl params graph )

  = vcat [ pprCLabel lbl <> parens (commafy $ map ppr params)
         , nest 8 $ lbrace <+> ppr info $$ rbrace
         , nest 4 $ ppr graph
         , rbrace ]

-- --------------------------------------------------------------------------
-- We follow [1], 4.5
--
--      section "data" { ... }
--
pprTop (CmmData section ds) = 
    (hang (pprSection section <+> lbrace) 4 (vcat (map ppr ds)))
    $$ rbrace

-- --------------------------------------------------------------------------
instance Outputable CmmSafety where
  ppr CmmUnsafe = ptext (sLit "_unsafe_call_")
  ppr (CmmSafe srt) = ppr srt

-- --------------------------------------------------------------------------
-- Info tables. The current pretty printer needs refinement
-- but will work for now.
--
-- For ideas on how to refine it, they used to be printed in the
-- style of C--'s 'stackdata' declaration, just inside the proc body,
-- and were labelled with the procedure name ++ "_info".
pprInfo :: CmmInfo -> SDoc
pprInfo (CmmInfo _gc_target update_frame CmmNonInfoTable) =
    vcat [{-ptext (sLit "gc_target: ") <>
                maybe (ptext (sLit "<none>")) ppr gc_target,-}
          ptext (sLit "update_frame: ") <>
                maybe (ptext (sLit "<none>")) pprUpdateFrame update_frame]
pprInfo (CmmInfo _gc_target update_frame
         (CmmInfoTable stat_clos (ProfilingInfo closure_type closure_desc) tag info)) =
    vcat [{-ptext (sLit "gc_target: ") <>
                maybe (ptext (sLit "<none>")) ppr gc_target,-}
          ptext (sLit "has static closure: ") <> ppr stat_clos <+>
          ptext (sLit "update_frame: ") <>
                maybe (ptext (sLit "<none>")) pprUpdateFrame update_frame,
          ptext (sLit "type: ") <> pprLit closure_type,
          ptext (sLit "desc: ") <> pprLit closure_desc,
          ptext (sLit "tag: ") <> integer (toInteger tag),
          pprTypeInfo info]

pprTypeInfo :: ClosureTypeInfo -> SDoc
pprTypeInfo (ConstrInfo layout constr descr) =
    vcat [ptext (sLit "ptrs: ") <> integer (toInteger (fst layout)),
          ptext (sLit "nptrs: ") <> integer (toInteger (snd layout)),
          ptext (sLit "constructor: ") <> integer (toInteger constr),
          pprLit descr]
pprTypeInfo (FunInfo layout srt arity _args slow_entry) =
    vcat [ptext (sLit "ptrs: ") <> integer (toInteger (fst layout)),
          ptext (sLit "nptrs: ") <> integer (toInteger (snd layout)),
          ptext (sLit "srt: ") <> ppr srt,
-- Temp Jan08
          ptext (sLit ("fun_type: ")) <> integer (toInteger (argDescrType _args)),

          ptext (sLit "arity: ") <> integer (toInteger arity),
          --ptext (sLit "args: ") <> ppr args, -- TODO: needs to be printed
          ptext (sLit "slow: ") <> pprLit slow_entry
         ]
pprTypeInfo (ThunkInfo layout srt) =
    vcat [ptext (sLit "ptrs: ") <> integer (toInteger (fst layout)),
          ptext (sLit "nptrs: ") <> integer (toInteger (snd layout)),
          ptext (sLit "srt: ") <> ppr srt]
pprTypeInfo (ThunkSelectorInfo offset srt) =
    vcat [ptext (sLit "ptrs: ") <> integer (toInteger offset),
          ptext (sLit "srt: ") <> ppr srt]
pprTypeInfo (ContInfo stack srt) =
    vcat [ptext (sLit "stack: ") <> ppr stack,
          ptext (sLit "srt: ") <> ppr srt]

-- Temp Jan08
argDescrType :: ArgDescr -> StgHalfWord
-- The "argument type" RTS field type
argDescrType (ArgSpec n) = n
argDescrType (ArgGen liveness)
  | isBigLiveness liveness = ARG_GEN_BIG
  | otherwise		   = ARG_GEN

-- Temp Jan08
isBigLiveness :: Liveness -> Bool
isBigLiveness (BigLiveness _)   = True
isBigLiveness (SmallLiveness _) = False


pprUpdateFrame :: UpdateFrame -> SDoc
pprUpdateFrame (UpdateFrame expr args) = 
    hcat [ ptext (sLit "jump")
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
pprBBlock :: Outputable stmt => GenBasicBlock stmt -> SDoc
pprBBlock (BasicBlock ident stmts) =
    hang (ppr ident <> colon) 4 (vcat (map ppr stmts))

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
          rep = ppr ( cmmExprType expr )

    -- call "ccall" foo(x, y)[r1, r2];
    -- ToDo ppr volatile
    CmmCall (CmmCallee fn cconv) results args safety ret ->
        sep  [ pp_lhs <+> pp_conv
	     , nest 2 (pprExpr9 fn <> 
		       parens (commafy (map ppr_ar args)))
               <> brackets (ppr safety)
             , case ret of CmmMayReturn -> empty
                           CmmNeverReturns -> ptext $ sLit (" never returns")
             ] <> semi
        where
	  pp_lhs | null results = empty
		 | otherwise    = commafy (map ppr_ar results) <+> equals
		-- Don't print the hints on a native C-- call

          ppr_ar :: Outputable a => CmmHinted a -> SDoc
	  ppr_ar (CmmHinted ar k) = case cconv of
			    CmmCallConv -> ppr ar
			    _           -> ppr (ar,k)
	  pp_conv = case cconv of
		      CmmCallConv -> empty
		      _           -> ptext (sLit("foreign")) <+> doubleQuotes (ppr cconv)

    -- Call a CallishMachOp, like sin or cos that might be implemented as a library call.
    CmmCall (CmmPrim op) results args safety ret ->
        pprStmt (CmmCall (CmmCallee (CmmLit lbl) CCallConv)
                        results args safety ret)
        where
	  -- HACK: A CallishMachOp doesn't really correspond to a ForeignLabel, but we
	  --	   use one to get the label printed.
          lbl = CmmLabel (mkForeignLabel 
	  			(mkFastString (show op)) 
	  			Nothing ForeignLabelInThisPackage IsFunction)

    CmmBranch ident          -> genBranch ident
    CmmCondBranch expr ident -> genCondBranch expr ident
    CmmJump expr params      -> genJump expr params
    CmmReturn params         -> genReturn params
    CmmSwitch arg ids        -> genSwitch arg ids

instance Outputable ForeignHint where
  ppr NoHint     = empty
  ppr SignedHint = quotes(text "signed")
--  ppr AddrHint   = quotes(text "address")
-- Temp Jan08
  ppr AddrHint   = (text "PtrHint")

-- Just look like a tuple, since it was a tuple before
-- ... is that a good idea? --Isaac Dupree
instance (Outputable a) => Outputable (CmmHinted a) where
  ppr (CmmHinted a k) = ppr (a, k)

-- --------------------------------------------------------------------------
-- goto local label. [1], section 6.6
--
--     goto lbl;
--
genBranch :: BlockId -> SDoc
genBranch ident = 
    ptext (sLit "goto") <+> ppr ident <> semi

-- --------------------------------------------------------------------------
-- Conditional. [1], section 6.4
--
--     if (expr) { goto lbl; } 
--
genCondBranch :: CmmExpr -> BlockId -> SDoc
genCondBranch expr ident =
    hsep [ ptext (sLit "if")
         , parens(ppr expr)
         , ptext (sLit "goto")
         , ppr ident <> semi ]

-- --------------------------------------------------------------------------
-- A tail call. [1], Section 6.9
--
--     jump foo(a, b, c);
--
genJump :: CmmExpr -> [CmmHinted CmmExpr] -> SDoc
genJump expr args = 
    hcat [ ptext (sLit "jump")
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
genReturn :: [CmmHinted CmmExpr] -> SDoc
genReturn args = 
    hcat [ ptext (sLit "return")
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

      in hang (hcat [ ptext (sLit "switch [0 .. ") 
                    , int (length maybe_ids - 1)
                    , ptext (sLit "] ")
                    , if isTrivialCmmExpr expr
                        then pprExpr expr
                        else parens (pprExpr expr)
                    , ptext (sLit " {") 
                    ]) 
            4 (vcat ( map caseify pairs )) $$ rbrace

    where
      snds a b = (snd a) == (snd b)

      caseify :: [(Int,Maybe BlockId)] -> SDoc
      caseify ixs@((_,Nothing):_)
        = ptext (sLit "/* impossible: ") <> hcat (intersperse comma (map (int.fst) ixs))
		<> ptext (sLit " */")
      caseify as 
        = let (is,ids) = unzip as 
          in hsep [ ptext (sLit "case")
                  , hcat (punctuate comma (map int is))
                  , ptext (sLit ": goto")
                  , ppr (head [ id | Just id <- ids]) <> semi ]

-- --------------------------------------------------------------------------
-- Expressions
--

pprExpr :: CmmExpr -> SDoc
pprExpr e 
    = case e of
        CmmRegOff reg i -> 
		pprExpr (CmmMachOp (MO_Add rep)
			   [CmmReg reg, CmmLit (CmmInt (fromIntegral i) rep)])
		where rep = typeWidth (cmmRegType reg)
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
pprExpr1, pprExpr7, pprExpr8 :: CmmExpr -> SDoc
pprExpr1 (CmmMachOp op [x,y]) | Just doc <- infixMachOp1 op
   = pprExpr7 x <+> doc <+> pprExpr7 y
pprExpr1 e = pprExpr7 e

infixMachOp1, infixMachOp7, infixMachOp8 :: MachOp -> Maybe SDoc

infixMachOp1 (MO_Eq     _) = Just (ptext (sLit "=="))
infixMachOp1 (MO_Ne     _) = Just (ptext (sLit "!="))
infixMachOp1 (MO_Shl    _) = Just (ptext (sLit "<<"))
infixMachOp1 (MO_U_Shr  _) = Just (ptext (sLit ">>"))
infixMachOp1 (MO_U_Ge   _) = Just (ptext (sLit ">="))
infixMachOp1 (MO_U_Le   _) = Just (ptext (sLit "<="))
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
        CmmRegOff  reg off  -> parens (ppr reg <+> char '+' <+> int off)
        CmmStackSlot a off  -> parens (ppr a   <+> char '+' <+> int off)
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
             , ppUnless (rep == wordWidth) $
               space <> dcolon <+> ppr rep ]

    CmmFloat f rep     -> hsep [ rational f, dcolon, ppr rep ]
    CmmLabel clbl      -> pprCLabel clbl
    CmmLabelOff clbl i -> pprCLabel clbl <> ppr_offset i
    CmmLabelDiffOff clbl1 clbl2 i -> pprCLabel clbl1 <> char '-'  
                                  <> pprCLabel clbl2 <> ppr_offset i
    CmmBlock id        -> ppr id
    CmmHighStackMark -> text "<highSp>"

pprLit1 :: CmmLit -> SDoc
pprLit1 lit@(CmmLabelOff {}) = parens (pprLit lit)
pprLit1 lit                  = pprLit lit

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
    CmmStaticLit lit   -> nest 4 $ ptext (sLit "const") <+> pprLit lit <> semi
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
        CmmLocal  local  -> pprLocalReg  local
        CmmGlobal global -> pprGlobalReg global

--
-- We only print the type of the local reg if it isn't wordRep
--
pprLocalReg :: LocalReg -> SDoc
pprLocalReg (LocalReg uniq rep) 
--   = ppr rep <> char '_' <> ppr uniq
-- Temp Jan08
   = char '_' <> ppr uniq <> 
       (if isWord32 rep -- && not (isGcPtrType rep) -- Temp Jan08		-- sigh
                    then dcolon <> ptr <> ppr rep
                    else dcolon <> ptr <> ppr rep)
   where
     ptr = empty
	 --if isGcPtrType rep
	 --      then doubleQuotes (text "ptr")
         --      else empty

-- Stack areas
pprArea :: Area -> SDoc
pprArea (RegSlot r)   = hcat [ text "slot<", ppr r, text ">" ]
pprArea (CallArea id) = pprAreaId id

pprAreaId :: AreaId -> SDoc
pprAreaId Old        = text "old"
pprAreaId (Young id) = hcat [ text "young<", ppr id, text ">" ]

-- needs to be kept in syn with Cmm.hs.GlobalReg
--
pprGlobalReg :: GlobalReg -> SDoc
pprGlobalReg gr 
    = case gr of
        VanillaReg n _ -> char 'R' <> int n
-- Temp Jan08
--        VanillaReg n VNonGcPtr -> char 'R' <> int n
--        VanillaReg n VGcPtr    -> char 'P' <> int n
        FloatReg   n   -> char 'F' <> int n
        DoubleReg  n   -> char 'D' <> int n
        LongReg    n   -> char 'L' <> int n
        Sp             -> ptext (sLit "Sp")
        SpLim          -> ptext (sLit "SpLim")
        Hp             -> ptext (sLit "Hp")
        HpLim          -> ptext (sLit "HpLim")
        CurrentTSO     -> ptext (sLit "CurrentTSO")
        CurrentNursery -> ptext (sLit "CurrentNursery")
        HpAlloc        -> ptext (sLit "HpAlloc")
        EagerBlackholeInfo -> ptext (sLit "stg_EAGER_BLACKHOLE_info")
        GCEnter1       -> ptext (sLit "stg_gc_enter_1")
        GCFun          -> ptext (sLit "stg_gc_fun")
        BaseReg        -> ptext (sLit "BaseReg")
        PicBaseReg     -> ptext (sLit "PicBaseReg")

-- --------------------------------------------------------------------------
-- data sections
--
pprSection :: Section -> SDoc
pprSection s = case s of
    Text              -> section <+> doubleQuotes (ptext (sLit "text"))
    Data              -> section <+> doubleQuotes (ptext (sLit "data"))
    ReadOnlyData      -> section <+> doubleQuotes (ptext (sLit "readonly"))
    ReadOnlyData16    -> section <+> doubleQuotes (ptext (sLit "readonly16"))
    RelocatableReadOnlyData
                      -> section <+> doubleQuotes (ptext (sLit "relreadonly"))
    UninitialisedData -> section <+> doubleQuotes (ptext (sLit "uninitialised"))
    OtherSection s'   -> section <+> doubleQuotes (text s')
 where
    section = ptext (sLit "section")

-----------------------------------------------------------------------------

commafy :: [SDoc] -> SDoc
commafy xs = fsep $ punctuate comma xs
