
{------------------------------------------------------------------------}
{--- An assembly code annotator for gcc >= 2.7.X on x86-linux-2.X     ---}
{---                                                      CacheAnn.hs ---}
{------------------------------------------------------------------------}

{- 
   This file is part of Cacheprof, a profiling tool for finding
   sources of cache misses in programs.

   Copyright (C) 1999 Julian Seward (jseward@acm.org)
   Home page: http://www.cacheprof.org

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file LICENSE.
-}

module Main ( main ) where
import Data.Char
import Data.List
import System.IO
import System.Environment
import System.Exit
import Arch_x86
import Generics


{-----------------------------------------------------------}
{--- Stage 1.  Break input string into pre-parsed lines  ---}
{-----------------------------------------------------------}

-- This stage is separated from instruction parsing
-- proper mostly for conceptual cleanliness.

-- Lines can either be:
--    a label definition, on its own (Label)
--    an instruction (Real)
--    anything else (Pseudo)
-- If instruction counting is to work properly,
-- labels should not be concealed inside Pseudos. 

data PreLine
   = PrePseudo Int String
   | PreLabel  Int String
   | PreReal   Int String
     deriving Show

instance PP PreLine where
   pp m (PrePseudo ln s)  = "preP: " ++ s
   pp m (PreLabel  ln s)  = "preL: " ++ s
   pp m (PreReal   ln s)  = "preR: " ++ s

-- section-main
preparse :: String -> [PreLine]
preparse  
   = concatMap preparseLine . zip [1..] . lines


preparseLine :: (Int, String) -> [PreLine]
preparseLine (line_number,s)
   | null cleaned 
   = []
   | looks_like_label cleaned
   = case span isLabelIsh cleaned of
        (label_name, rest) 
           -> (PreLabel line_number (label_name ++ [head rest]))
              : preparseLine (line_number, tail rest)
   | head cleaned `elem` ".#"
   = [PrePseudo line_number s]
   | otherwise
   = case span (/= ';') cleaned of
        (presemi, postsemi)
           -> (PreReal line_number presemi)
              : preparseLine (line_number, drop 1 postsemi)

     where
        cleaned    = dropWhile isSpace s
        untabbed x = not (null x) && head x /= '\t'

        looks_like_label :: String -> Bool
        looks_like_label x 
           = case span isLabelIsh x of
                (label_name, rest) 
                   -> not (null label_name) 
                      && take 1 rest == ":"
                      && (null (tail rest) 
                          || isSpace (head (tail rest)))


{-----------------------------------------------------------}
{--- Stage 2.  Parse instructions.                       ---}
{-----------------------------------------------------------}

-- Turn the list of PreLines into Lines by parsing
-- the instructions.

data PPM 
   = PPM_Debug | PPM_User
     deriving Eq

class PP a where
   pp  :: PPM -> a -> String
   ppu :: a -> String
   ppd :: a -> String
   ppl :: PPM -> [a] -> String

   ppu = pp PPM_User
   ppd = pp PPM_Debug
   ppl m = concat . intersperse "," . map (pp m)


data Line 
   = Pseudo Int String
   | Label  Int String
   | Real   Int CC Insn
     deriving (Show, Eq)

instance PP Line where
   pp PPM_User  (Pseudo ln s)     = s
   pp PPM_Debug (Pseudo ln s)     = "P: " ++ s
   pp PPM_User  (Label ln s)      = s
   pp PPM_Debug (Label ln s)      = "L: " ++ s

   pp PPM_User  (Real ln cc insn) 
      = "\t" ++ pp PPM_User insn
   pp PPM_Debug (Real ln cc insn) 
      = "R:        " ++ pp PPM_Debug insn ++
       if   isNoCC cc 
       then ""
       else "\n             CC = " ++ pp PPM_Debug cc

getLineNo (Pseudo ln s)    = ln
getLineNo (Label  ln s)    = ln
getLineNo (Real   ln cc i) = ln

insnOfLine (Real ln cc i) = i
insnOfLine other          = internal "insnOfLine"

isReal (Real ln cc i) = True
isReal other          = False

isPseudo (Pseudo ln s) = True
isPseudo other         = False

data CC
   = NoCC
   | CC String Int String  -- file name, line no, fn name
     deriving (Show, Eq)

instance PP CC where
   pp ppm NoCC = "NoCC"
   pp ppm (CC filename lineno fnname)
      = filename ++ ":" ++ show lineno ++ " " ++ fnname

setCC (Real ln oldcc i) cc = Real ln cc i
setCC other             cc = internal "setCC"

getCC (Real ln cc i) = cc
getCC other          = NoCC

isNoCC NoCC       = True
isNoCC (CC _ _ _) = False

ccGetFileNm (CC filenm ln funcnm) = filenm
ccGetLineNo (CC filenm ln funcnm) = ln
ccGetFuncNm (CC filenm ln funcnm) = funcnm



-- section-main
parse :: [PreLine] -> [Line]
parse 
   = map f
     where 
        f (PrePseudo ln s) = Pseudo ln s
        f (PreLabel  ln s) = Label  ln s
        f (PreReal   ln s)
           = case pInsn (olex s) of
                POk i [] -> Real ln NoCC i
                _        -> bomb ln s
   
        bomb ln s
           = inputerr ("(stdin):" ++ show ln
                       ++ ": syntax error on `" ++ s ++ "'\n" )
 

{-------------------------------------------}
{--- an lexer for x86,                   ---}
{--- using the AT&T syntax               ---}
{-------------------------------------------}

olex :: String -> [Lex]

olex [] = []
olex (c:cs)
   | isSpace c = olex cs
   | c == '('  = LLParen : olex cs
   | c == ')'  = LRParen : olex cs
   | c == ','  = LComma  : olex cs
   | c == '+'  = LPlus   : olex cs
   | c == '-'  = LMinus  : olex cs
   | c == '*'  = LStar   : olex cs
   | c == '$'  = LDollar : olex cs
   | c == '#'  = []   -- comment; arch specific

   | c == '%'
   = case span isAlpha cs of 
        (rname, rest)
           | rname == "st" && not (null rest) 
             && head rest == '('
           -> case span (`elem` "(01234567)") rest of
                 (frname,rest2) -> (LReg (c:rname++frname)) : olex rest2
           | (c:rname) `elem` reg_names
           -> (LReg (c:rname)) : olex rest
           | otherwise
           -> barf (c:cs)
   | isDigit c
   = case span isDigitish cs of 
        (num, rest) -> (LNum (c:num)) : olex rest
   | isAlpha c || c == '_'
   = case span isNameIsh cs of
        (nmcs, rest) -> (LName (c:nmcs)) : olex rest
   | c == '.'
   = case span isLabelIsh cs of
        (lbcs, rest) -> (LLabel (c:lbcs)) : olex rest

   | otherwise
   = barf (c:cs)


isDigitish c = isDigit c || c `elem` "xabcdefABCDEF"
isNameIsh c  = isAlpha c || isDigit c || c == '_' || c == '.'
isLabelIsh c = isAlpha c || isDigit c || c == '.' || c == '_'
isRegChar c  = isAlpha c || c `elem` "(0)"

barf s = inputerr ( "lexical error on: `" ++ s ++ "'")


{-------------------------------------------}
{--- an instruction parser for x86,      ---}
{--- using the AT&T syntax               ---}
{-------------------------------------------}


{- operand ::= reg
             | $ const
             | const
             | const amode
             | amode

   amode ::= (reg)          -- B
           | (reg,reg)      -- B I
           | (,reg,num)     -- I S
           | (reg,reg,num)  -- B I S 

   const  ::= (OPTIONAL '-') const_factor 
                             (ZEROORMORE signed_const_factor)

   signed_const_factor ::=   + const_factor
                         |   - const_factor

   const_factor ::= const_atom
                  | const_atom '*' const_factor
                  | '(' const_factor ')'

   const_atom ::= number
                | label
                | name

   reg ::=  %eax | %ebx | %ecx | %edx | %esi | %edi | %ebp | %esp ...
-}

data Annot
   = AnnR Int Operand
   | AnnM Int Operand
   | AnnW Int Operand
   | AnnC String        -- just a comment
     deriving (Show, Eq)

getAnnOp (AnnR w o) = o
getAnnOp (AnnM w o) = o
getAnnOp (AnnW w o) = o

isAnnC (AnnC _) = True
isAnnC _        = False

mkAnnC comment = SomeAnns [AnnC comment]
mkNoAnns = SomeAnns []

hasRealAnns (Insn ann _ _)
   = (not . null . filter (not.isAnnC) . getAnns) ann

data Anns
   = DontAnnMe
   | SomeAnns [Annot]
     deriving (Show, Eq)

getAnns DontAnnMe       = []
getAnns (SomeAnns anns) = anns

isDontAnnMe DontAnnMe = True
isDontAnnMe _         = False

data Insn 
   = Insn Anns Opcode [Operand]
     deriving (Show, Eq)

annsOfInsn   (Insn anns opcode operand) = anns
opcodeOfInsn (Insn anns opcode operand) = opcode

data Operand
   = OP_REG   Reg
   | OP_LIT   Const
   | OP_D     Const
   | OP_DA    Const AMode
   | OP_A     AMode
   | OP_STAR  Operand
     deriving (Show, Eq)

data AMode
   = AM_B   Reg  
   | AM_BI  Reg Reg
   | AM_IS  Reg String
   | AM_BIS Reg Reg String
     deriving (Show, Eq)



newtype Const 
   = Const [SignedFactor]
     deriving (Show, Eq)

data SignedFactor  = Neg UnsignedFactor | Pos UnsignedFactor
     deriving (Show, Eq)

data UnsignedFactor
   = UF_NUM String  
   | UF_NAME String 
   | UF_LABEL String
   | UF_TIMES UnsignedFactor UnsignedFactor
     deriving (Show, Eq)

data Reg 
   = EAX | EBX | ECX | EDX | EDI | ESI | EBP | ESP
   | AX | BX | CX | DX | SI | DI | BP
   | AL | BL | CL | DL
   | AH | BH | CH | DH
   | ST_0 | ST_1 | ST_2 | ST_3 
   | ST_4 | ST_5 | ST_6 | ST_7
     deriving (Show, Eq)

pOpcode :: Parser Opcode
pOpcode
   = pAlts (map (\o -> pName (drop 2 (show (fst o))) (fst o)) x86info)

pInsn :: Parser Insn
pInsn 
   = p2 (Insn (SomeAnns [])) pOpcode (pStarComma pOperand)

pOperand :: Parser Operand
pOperand
   = pAlts [
        pApply OP_REG pReg,
        p2 (\_ c -> OP_LIT c) pLDollar pConst,
        p2 (\c a -> OP_DA c a) pConst pAMode,
        pApply (\c -> OP_D c) pConst,
        pApply (\a -> OP_A a) pAMode,
        p2 (\_ operand -> OP_STAR operand) pLStar pOperand
     ]

pAMode :: Parser AMode
pAMode
   = pInParens (
        pAlts [
           p3 AM_BIS pReg (pPreComma pReg) (pPreComma pLNum),
           p2 AM_BI  pReg (pPreComma pReg),
           p2 AM_IS  (pPreComma pReg) (pPreComma pLNum),
           pApply AM_B pReg
        ]
     )

pUnsignedA :: Parser UnsignedFactor
pUnsignedA
   = pAlts [
        pApply UF_NUM pLNum,
        pApply UF_NAME pLName,
        pApply UF_LABEL pLLabel
     ]

pUnsignedF :: Parser UnsignedFactor
pUnsignedF
   = pAlts [
        p3 (\x times y -> UF_TIMES x y) pUnsignedA pLStar pUnsignedF,
        p3 (\left x right -> x) pLLParen pUnsignedF pLRParen,
        pUnsignedA
     ]

pSignedF :: Parser SignedFactor
pSignedF
   = pAlts [
        p2 (\_ ca -> Pos ca) pLPlus  pUnsignedF,
        p2 (\_ ca -> Neg ca) pLMinus pUnsignedF
     ]

pConst :: Parser Const
pConst
   = pAlts [
        p2 (\  ca cas -> Const ((Pos ca):cas))
           pUnsignedF (pStar pSignedF),
        p3 (\_ ca cas -> Const ((Neg ca):cas)) 
           pLMinus pUnsignedF (pStar pSignedF)
     ]

pReg
   = pApply findReg pLReg
     where
        findReg r = case lookup r reg_map of
                       Nothing  -> incomplete ("findReg: `" ++ r ++ "'")
                       Just reg -> reg

test = pInsn . olex

pLLiteral = pApply unLLiteral (pSat isLLiteral)
pLNum     = pApply unLNum     (pSat isLNum)
pLReg     = pApply unLReg     (pSat isLReg)
pLName    = pApply unLName    (pSat isLName)
pLLabel   = pApply unLLabel   (pSat isLLabel)

reg_map
   = [("%eax",EAX),("%ebx",EBX),("%ecx",ECX),("%edx",EDX),
      ("%edi",EDI),("%esi",ESI),("%ebp",EBP),("%esp",ESP),

      ("%ax",AX), ("%bx",BX), ("%cx",CX), ("%dx",DX), 
      ("%si",SI), ("%di",DI), ("%bp",BP),

      ("%al",AL), ("%bl",BL), ("%cl",CL), ("%dl",DL),
      ("%ah",AH), ("%bh",BH), ("%ch",CH), ("%dh",DH),

      ("%st", ST_0), ("%st(0)", ST_0), 
      ("%st(1)", ST_1), ("%st(2)", ST_2), ("%st(3)", ST_3), 
      ("%st(4)", ST_4), ("%st(5)", ST_5), ("%st(6)", ST_6),
      ("%st(7)", ST_7)
     ]

reg_names
   = map fst reg_map




instance PP Insn where
   pp ppm insn@(Insn ann opcode operands)
      = main_part 
        ++ (if   ppm == PPM_User
                 || null (getAnns ann)
            then [] 
            else take (max 0 (36 - length main_part)) (repeat ' ')
                 ++ (if hasRealAnns insn 
                     then " # ANN " else " #     ") 
                 ++ ppl ppm (getAnns ann)
           )
        where
           main_part
              = pp ppm opcode 
                ++ (if   null operands 
                    then [] 
                    else " " ++ ppl ppm operands)

instance PP Annot where
   pp ppm (AnnR w op) = "r" ++ show w ++ ": " ++ pp ppm op
   pp ppm (AnnM w op) = "m" ++ show w ++ ": " ++ pp ppm op
   pp ppm (AnnW w op) = "w" ++ show w ++ ": " ++ pp ppm op
   pp ppm (AnnC comm) = comm
   ppl ppm = concat . intersperse "   " . map (pp ppm)

instance PP Operand where
   pp ppm (OP_REG r)  = pp ppm r
   pp ppm (OP_LIT c)  = "$" ++ pp ppm c
   pp ppm (OP_D c)    = pp ppm c
   pp ppm (OP_A a)    = pp ppm a
   pp ppm (OP_DA c a) = pp ppm c ++ pp ppm a
   pp ppm (OP_STAR o) = "*" ++ pp ppm o

instance PP AMode where
   pp ppm (AM_B r1)        = paren (pp ppm r1)
   pp ppm (AM_BI r1 r2)    = paren (pp ppm r1 ++ "," ++ pp ppm r2)
   pp ppm (AM_IS r1  n)    = paren ("," ++ pp ppm r1 ++ "," ++ n)
   pp ppm (AM_BIS r1 r2 n) 
      = paren (pp ppm r1 ++ "," ++ pp ppm r2 ++ "," ++ n)

instance PP Const where
   pp ppm (Const signed_factors)
      = dropWhile (== '+') (concatMap (pp ppm) signed_factors)

instance PP SignedFactor where
   pp ppm (Neg factor) = "-" ++ pp ppm factor
   pp ppm (Pos factor) = "+" ++ pp ppm factor

instance PP UnsignedFactor where
   pp ppm (UF_NUM n)     = n
   pp ppm (UF_NAME n)    = n
   pp ppm (UF_LABEL l)   = l
   pp ppm (UF_TIMES a b) = pp ppm a ++ "*" ++ pp ppm b

instance PP Reg where
   pp ppm ST_0 = "%st"
   pp ppm ST_1 = "%st(1)"
   pp ppm ST_2 = "%st(2)"
   pp ppm ST_3 = "%st(3)"
   pp ppm ST_4 = "%st(4)"
   pp ppm ST_5 = "%st(5)"
   pp ppm ST_6 = "%st(6)"
   pp ppm ST_7 = "%st(7)"
   pp ppm r    = "%" ++ map toLower (show r)

instance PP Opcode where
   pp ppm o = (drop 2 . show) o

paren s = "(" ++ s ++ ")"

{-----------------------------------------------------------}
{--- Stage 3.  Simplify some complex instructions into   ---}
{--- equivalent sequences of simpler ones.               ---}
{-----------------------------------------------------------}

-- we carry along a counter `lc' so as to be able to
-- manufacture labels.

-- section-main
simplify :: [Line] -> [Line]
simplify = simpl_wrk 0


simpl_wrk lc []
   = []

simpl_wrk lc ((Real ln cc (Insn (SomeAnns []) O_rep [])) :
              (Real _   _ (Insn (SomeAnns []) o_op [])) : lines)
   | o_op `elem` [O_movsl, O_movsw, O_movsb, 
                  O_stosl, O_stosw, O_stosb]
   = let (l1,l2)
            = (lc,lc+1)
         -- This lot gratuitiously cloned below
         labelName n
            = "cacheprof_x86_rep_expansion" ++ show n
         mkInsn oc ops
            = Real ln cc (Insn DontAnnMe oc ops)
         mkInsnA oc ops
            = Real ln cc (Insn (SomeAnns []) oc ops)
         mkLabelD ln
            = Pseudo ln (mk_arch_label_def (labelName ln))
         mkLabelU ln
            = OP_D (Const [Pos (UF_LABEL 
                               (mk_arch_label_use (labelName ln)))])
     in
         [mkInsn   O_pushfl  [],
          mkLabelD l1,
          mkInsn   O_testl  [OP_REG ECX, OP_REG ECX],
          mkInsn   O_jz     [mkLabelU l2],
          mkInsn   O_decl   [OP_REG ECX],
          mkInsnA  o_op     [],
          mkInsn   O_jmp    [mkLabelU l1],
          mkLabelD l2,
          mkInsn   O_popfl   []
         ]
         ++ simpl_wrk (lc+2) lines

simpl_wrk lc ((Real ln cc (Insn (SomeAnns []) o_reppy [])) :
              (Real _   _ (Insn (SomeAnns []) o_op [])) : lines)
   | o_reppy `elem` [O_repz, O_repnz]
     && o_op `elem` [O_cmpsb, O_scasb] -- also w and l sizes
   = let o_exit
            | o_reppy `elem` [O_repnz]
            = O_jz
            | o_reppy `elem` [O_repz]
            = O_jnz
            | otherwise
            = incomplete ("simpl_wrk rep: can't handle " 
                          ++ show (o_reppy, o_op) ++ "\n")
         (l1,l2,l3)
            = (lc,lc+1,lc+2)

         -- This lot gratuitiously cloned from above
         labelName n
            = "cacheprof_x86_rep_expansion" ++ show n
         mkInsn oc ops
            = Real ln cc (Insn DontAnnMe oc ops)
         mkInsnA oc ops
            = Real ln cc (Insn (SomeAnns []) oc ops)
         mkLabelD ln
            = Pseudo ln (mk_arch_label_def (labelName ln))
         mkLabelU ln
            = OP_D (Const [Pos (UF_LABEL 
                               (mk_arch_label_use (labelName ln)))])


     in
         [mkLabelD l1,
          mkInsn   O_pushfl [],
          mkInsn   O_testl  [OP_REG ECX, OP_REG ECX],
          mkInsn   O_jz     [mkLabelU l2],
          mkInsn   O_popfl  [],
          mkInsnA  o_op     [],
          mkInsn   O_pushfl [],
          mkInsn   O_decl   [OP_REG ECX],
          mkInsn   O_popfl  [],
          mkInsn   o_exit   [mkLabelU l3],
          mkInsn   O_jmp    [mkLabelU l1],
          mkLabelD l2,
          mkInsn   O_popfl  [],
          mkLabelD l3
         ]
         ++ simpl_wrk (lc+3) lines

simpl_wrk lc ((Real ln cc (Insn (SomeAnns []) O_leave [])):lines)
   = [Real ln cc (Insn mkNoAnns O_movl [OP_REG EBP, OP_REG ESP]),
      Real ln cc (Insn mkNoAnns O_popl [OP_REG EBP])]
     ++ simpl_wrk lc lines

simpl_wrk lc (line:lines)
   = line : simpl_wrk lc lines


{-----------------------------------------------------------}
{--- Stage 4.  Identify basic blocks.                    ---}
{-----------------------------------------------------------}

-- This is to make instruction counting tolerably
-- efficient.  It's safe but inefficient to put
-- each instruction into its own basic block.

newtype BB = BB [Line]

instance PP BB where
   pp ppm (BB ls)
      = "{ -- basic block\n" 
        ++ unlines (map (pp ppm) ls)
        ++ "}"

-- section-main
identify_bbs :: [Line] -> [BB]
{-
-- brain-dead, reference implementation
identify_bbs = map (\line -> BB [line])
-}

-- something a bit better
-- It could still be improved.  
-- Use --ddump-ident-bbs to get ideas.
identify_bbs 
   = merge_bbs . map (\line -> BB [line])
     where
        merge_bbs []   = []
        merge_bbs [bb] = [bb]
        merge_bbs (bb1@(BB lines1) : bb2@(BB [line]) : bbs)

           | isPseudo line
           = let bigger_bb = BB (lines1++[line])
             in  merge_bbs (bigger_bb : bbs)

           | isOriginalInsn line 
             && any isReal lines1
             && isOriginalInsn last_Real_lines1
             && opcodeOfInsn (insnOfLine last_Real_lines1)
                   `elem` nonJumpyOpcodes

           = let bigger_bb = BB (lines1++[line])
             in  merge_bbs (bigger_bb : bbs)

           | otherwise
           = bb1 : merge_bbs (bb2:bbs)

             where
                last_Real_lines1 = last (filter isReal lines1)

{-----------------------------------------------------------}
{--- Stage 5.  Add insn count annotations to BBs.        ---}
{-----------------------------------------------------------}

-- section-main
use_bbs :: [BB] -> [Line]
use_bbs = concatMap use_bb

use_bb :: BB -> [Line]
use_bb (BB [])
   = internal "use_bb: empty bb"
use_bb (BB lines)
   = let n_original_insns
            = length (filter isOriginalInsn lines)
         lineNo
            = getLineNo (head lines)
         synthd_insns
            = map (Real lineNo NoCC)
                  (incSequence n_original_insns "cacheprof_icount")
     in
         if   n_original_insns == 0
         then lines
         else synthd_insns ++ lines

-- Instructions haven't been annotated yet.
-- So the way to detect an original insn (ie, one
-- not generated by simplification) is:
--    insns created by simplify have DontAnnMe,
--    whereas originals have SomeAnns [].

isOriginalInsn (Real ln cc insn)
   = case annsOfInsn insn of
        DontAnnMe   -> False
        SomeAnns [] -> True
        other       -> internal "isOriginalInsn"
isOriginalInsn other
   = False

{-----------------------------------------------------------}
{--- Stage 6.  Annotate instructions with memory         ---}
{--- read/modify/write info.                             ---}
{-----------------------------------------------------------}

-- section-main
annotate :: [Line] -> [Line]
annotate
   = map f
     where
        f (Real ln cc insn) = Real ln cc (annotate_insn insn)
        f label_or_pseudo   = label_or_pseudo

annotate_insn :: Insn -> Insn
annotate_insn insn@(Insn old_ann opcode operands)
   | isDontAnnMe old_ann
   = insn
   | otherwise
   = Insn (SomeAnns (filter (isMemOp.getAnnOp) 
                            (annsOf opcode operands)))
          opcode operands

isMemOp (OP_REG r)  = False
isMemOp (OP_LIT c)  = False
isMemOp (OP_D d)    = True
isMemOp (OP_DA d a) = True
isMemOp (OP_A a)    = True
isMemOp (OP_STAR o) = incomplete "isMemOp: not sure about *-form"

the_edi       = OP_A (AM_B EDI)
the_esi       = OP_A (AM_B ESI)
the_sp        = OP_A (AM_B ESP)
the_sp_plus_4 = OP_DA (Const [Pos (UF_NUM "4")]) (AM_B ESP)
the_sp_plus_8 = OP_DA (Const [Pos (UF_NUM "8")]) (AM_B ESP)

annsOf :: Opcode -> [Operand] -> [Annot]
annsOf opcode operands
   = let opInfo
            = getOperandInfo opcode
         no_applicable_info
            = incomplete ("operand info (" 
                     ++ show opInfo
                     ++ ") doesn't match operand(s): "
                     ++ ppd (Insn mkNoAnns opcode operands))

     in case opInfo of

           OI effects
              -> case annsFromEffects effects operands of
                    Just anns -> anns
                    Nothing   -> no_applicable_info

           OI_Jumpy
              -> case operands of 
                    [op1] -> case op1 of 
                                { OP_STAR o -> [AnnR 4 o]; _ -> [] }
                    other -> no_applicable_info

           OI_NoEffect
              -> []
   
           OI_Error
              -> internal ( "unsimplified opcode: "
                            ++ ppd (Insn mkNoAnns opcode operands))
   
           OI_Special
              |  opcode == O_pushl
              -> case operands of 
                    [op1] -> [AnnR 4 op1, AnnW 4 the_sp_plus_4]
                    other -> no_applicable_info
   
              |  opcode == O_call
              -> case operands of 
                    [op1] -> case op1 of
                       OP_STAR o -> [AnnR 4 o, AnnW 4 the_sp_plus_4]
                       direct    -> [AnnW 4 the_sp_plus_4]
                    other -> no_applicable_info

              |  opcode == O_popl
              -> case operands of 
                    [op1] -> [AnnR 4 the_sp_plus_8, AnnW 4 op1]
   
              |  opcode == O_ret
              -> [AnnR 4 the_sp_plus_8]
   
              |  opcode == O_scasb
              -> [AnnR 1 the_edi]
              |  opcode == O_cmpsb
              -> [AnnR 1 the_edi, AnnR 1 the_esi]
              |  opcode == O_movsl
              -> [AnnR 4 the_esi, AnnW 4 the_edi]
              |  opcode == O_movsw
              -> [AnnR 2 the_esi, AnnW 2 the_edi]
              |  opcode == O_movsb
              -> [AnnR 1 the_esi, AnnW 1 the_edi]
              |  opcode == O_stosl
              -> [AnnW 4 the_edi]  -- a guess
              |  opcode == O_stosw
              -> [AnnW 2 the_edi]  -- a guess
              |  opcode == O_stosb
              -> [AnnW 1 the_edi]  -- a guess
           other
              -> incomplete ("\nunclassifiable opcode: " 
                             ++ ppd (Insn mkNoAnns opcode operands) )
     

annsFromEffects :: [OperandEffect] -> [Operand] -> Maybe [Annot]
annsFromEffects effects operands
   | null effects
   = Nothing
   | otherwise
   = let mismatch = annsFromEffects (tail effects) operands
     in
     case head effects of

        OE_RR s1 s2
           -> case operands of 
                 [op1, op2] -> Just [AnnR s1 op1, AnnR s2 op2]
                 other      -> mismatch

        OE_RM s1 s2
           -> case operands of 
                 [op1, op2] -> Just [AnnR s1 op1, AnnM s2 op2]
                 other      -> mismatch

        OE_RW s1 s2
           -> case operands of 
                 [op1, op2] -> Just [AnnR s1 op1, AnnW s2 op2]
                 other      -> mismatch
   
        OE_R s1
           -> case operands of 
                 [op1] -> Just [AnnR s1 op1]
                 other -> mismatch
   
        OE_M s1
           -> case operands of 
                 [op1] -> Just [AnnM s1 op1]
                 other -> mismatch
   
        OE_W s1
           -> case operands of 
                 [op1] -> Just [AnnW s1 op1]
                 other -> mismatch
      
        OE_nW s2
           -> case operands of 
                 [op1,op2] -> Just [AnnW s2 op2]
                 other     -> mismatch
   
        OE_RRM s1 s2 s3
           -> case operands of 
                 [op1,op2,op3] -> Just [AnnR s1 op1, AnnR s2 op2,
                                                     AnnM s3 op3]
                 other         -> mismatch


getOperandInfo :: Opcode -> OperandInfo
getOperandInfo opcR
   = case [oi | (opc, oi) <- x86info, opc == opcR] of
        [oi] -> oi
        _    -> incomplete ("getOperandInfo: no info for: " 
                            ++ show opcR ++ "\n")


{-----------------------------------------------------------}
{--- Stage 7a for level 2 profiling.  Look at the        ---}
{--- debugging info, so as to guess file and function    ---}
{--- names, and line numbers.  Stick this info onto      ---}
{--- every instruction for which we want to bill a       ---}
{--- memory transaction.                                 ---}
{-----------------------------------------------------------}

-- section-main
addCCs :: [Line] -> [Line]
addCCs 
   = addCCs_wrk (Guessed "_unknown_file_")
                0 
                (Guessed "_unknown_function_")

data PossiblyString
   = Stated  String
   | Guessed String

isGuessed ps = case ps of { Guessed _ -> True;  _ -> False }
getTheString (Stated s)  = s
getTheString (Guessed s) = s

-- file name, line number, function name
addCCs_wrk :: PossiblyString -> Int -> PossiblyString
              -> [Line] -> [Line]
addCCs_wrk inm lno fnm [] 
   = []
addCCs_wrk inm lno fnm (l:ls)
   = case l of
        Real _ _ insn
           |  hasRealAnns insn
           -> setCC l (CC (getTheString inm) lno (getTheString fnm))
              : addCCs_wrk inm lno fnm ls
           |  otherwise
           -> l : addCCs_wrk inm lno fnm ls
        Pseudo ln s
           -> case updCC s of
                 (inm2, lno2, fnm2)
                    -> l : addCCs_wrk inm2 lno2 fnm2 ls
        Label ln s
           -> case updL s of
                 (inm2, lno2, fnm2)
                    -> l : addCCs_wrk inm2 lno2 fnm2 ls
     where
        updCC s 
            = upd2 (words (dropWhile isSpace s))

        upd2 (".stabn" : args : _)
            = case splitArgs args of
                 ("68":_:lns:_) -> (inm, read lns, fnm)
                 _              -> (inm, lno, fnm)

        upd2 (".stabs" : args : _)
            = case splitArgs args of
                 (filenm:"100":_) | last filenm /= '/'
                                  -> (Stated (deQuote filenm), lno, fnm)
                 (filenm:"132":_) -> (Stated (deQuote filenm), lno, fnm)
                 (fnnm:"36":_)    |  (not.null.deQuote) fnnm
                                  -> (inm, lno, 
                                      Stated (deQuote 
                                                (takeWhile (/=':') fnnm)))
                 _                -> (inm, lno, fnm)
 
        upd2 [".file", filenm]
           | isGuessed inm && isQuoted filenm
           = (Guessed (deQuote filenm), lno, fnm)

        upd2 _
            = (inm, lno, fnm)

        -- Try to guess function names from labels
        -- if no debugging info is available.
        -- If debugging info is available, don't override it.
        updL label_text
            | isGuessed fnm && take 2 cleaned /= ".L"
            = (inm, lno, Guessed (init cleaned))
            | otherwise
            = (inm, lno, fnm)
              where 
                 cleaned = dropWhile isSpace label_text

        splitArgs = breakOnComma . zapCIQ

        zapCIQ s = out s   -- zap commas inside quotes
        out []                  = []
        out (c:cs) | c == '"'   = c   : inn cs
                   | otherwise  = c   : out cs

        inn []                  = []
        inn (c:cs) | c == '"'   = c   : out cs
                   | c == ','   = '_' : inn cs
                   | otherwise  = c   : inn cs

        breakOnComma :: String -> [String]
        breakOnComma [] = []
        breakOnComma s
           = case span (/= ',') s of
                (pre,post) -> pre : breakOnComma (drop 1 post)

        isQuoted s
           = length s >= 2 && head s == '"' && last s == '"'
        deQuote s 
           = filter (/= '"') s    -- " fool Haskell-mode highlighting


{-----------------------------------------------------------}
{--- Stage 7b for level 2 profiling.  Examine the CC     ---}
{--- descriptors that stage 7a created.  Each one will   ---}
{--- require some storage in the final assembly output.  ---}
{--- Also, run over the annotated instructions, and      ---}
{--- insert calls to the cache simulator.                ---}
{-----------------------------------------------------------}

{-- A complex stage.

    1.  Round up the CCs that Stage 7a attached.

    2.  Condense them into a convenient form
        holding the names of source files, source functions 
        and sourcepoints mentioned in this file.
        (this is makeCCdescriptors)

    3.  Using (2), generate a data area in which holds
        the file & function names, and the array of counters,
        one per source point.
        (this is useCCdescriptors)

    4.  Independently of 1, 2 and 3, travel over the
        output of section 7a, and insert calls to the
        cache simulator around every insn marked as doing
        a memory access which we want to know about.
        (mapAccumL synthLine)
---}


-- section-main
synth_2 :: [Line] -> [Line]
synth_2 ccd_assy
   = let -- get the ccs (part 1)
         ccs
            = map getCC ccd_assy
         -- make a handy package (part 2)
         cc_descriptors
            = makeCCdescriptors ccs
         -- generate the data areas (part 3)
         data_areas
            = useCCdescriptors cc_descriptors
         -- insert calls to cache simulator (part 4)
         num_ccs_avail
            = case cc_descriptors of
                 (dbg, file_names, fn_names, src_points)
                    -> length src_points
         (num_ccs_used, synthd_assy_grps)
            = mapAccumL synthLine 0 ccd_assy
         synthd_assy
            = if   num_ccs_used == num_ccs_avail -- paranoid :-)
              then concat synthd_assy_grps
              else internal "doFile: cc supply/usage mismatch\n"
     in
         synthd_assy ++ data_areas


{-------------------------------------------}
{--- part 2.                             ---}
{--- Roll CC info into a handy package.  ---}
{--- Is arch independant.                ---}
{-------------------------------------------}

makeCCdescriptors :: [CC] -> ([String],[String],[String],[(Int,Int,Int)])
makeCCdescriptors allCcs
   = let -- interesting ccs
         ccs = filter (not.isNoCC) allCcs

         -- the filenames
         filenames = nub (map ccGetFileNm ccs)
         
         -- make a map from each function to its
         -- canonical name, by adding the name of the
         -- function in which it first appears
         canonical_fn_map
            = canonicalise (
                 zip (map ccGetFuncNm ccs) (map ccGetFileNm ccs))
         (canonical_fn_map_fsts, canonical_fn_map_snds)
            = unzip canonical_fn_map

         canonicalise [] = []
         canonicalise ((fn,file):rest)
            = (fn,fn++"("++file++")")
              : canonicalise (filter ((/= fn).fst) rest)

         toSrcPoint :: CC -> (Int,Int,Int)
         toSrcPoint cc
            = (indexOf filenames (ccGetFileNm cc),
               ccGetLineNo cc,
               indexOf canonical_fn_map_fsts (ccGetFuncNm cc))

         srcPoints = map toSrcPoint ccs

         debugging_text
            = ["file names:"]
              ++ map indent filenames
              ++ ["canonicalised function names:"]
              ++ map indent canonical_fn_map_snds
              ++ ["raw source points:"]
              ++ map indent (map show ccs)
              ++ ["cooked source points:"]
              ++ map indent (map show srcPoints)

         indent s = "   " ++ s
         indexOf xs y
            = f 0 xs 
              where
                 f n [] = internal ("indexOf: " ++ show y ++ "\n")
                 f n (z:zs) = if y == z then n else f (n+1) zs
     in
         (debugging_text, 
          filenames,
          canonical_fn_map_snds,
          srcPoints)



{-------------------------------------------}
{--- part 3.                             ---}
{--- Generate data area from the handy   ---}
{--- package.  Is arch dependant.        ---}
{-------------------------------------------}

{- Generate assembly code to define a data area like this:

      <msb_first_word> #filenames
      <msb_first_word> #funcnames
      <msb_first_word> #sourcepoints
      filenames, 0 terminated, end-to-end
      funcnames, 0 terminated, end-to-end
      sourcepoints

   The comment text can be included too.

   Each sourcepoint is a 28-byte area with
      the following format:
         4 bytes    file number
         4 bytes    line number
         4 bytes    function number
         8 bytes    number of references, initially zero
         8 bytes    number of misses, initially zero
   The fields are regarded as integers stored in the
   native endianness, ie on x86 the byte order is
   3 2 1 0 for the first three and 7 6 5 4 3 2 1 0
   for the last two.
-}

mk_arch_label_def s = ".L" ++ s ++ ":"
mk_arch_label_use s = ".L" ++ s
tABLE_START_STRING  = "cacheprof_magic_table"
bUCKETS_START_STRING = "cacheprof_magic_buckets"

useCCdescriptors :: ([String],[String],[String],[(Int,Int,Int)]) 
                    -> [Line]
useCCdescriptors (debugging_text, filenames, funcnames, points)
   = let length_words
            = [mk_comment "number of filenames, funcnames, points, cc addr"]
              ++ map mk_word [length filenames, 
                              length funcnames, 
                              length points]
              ++ [mk_word_l bUCKETS_START_STRING]
         strings
            = filenames ++ funcnames
         string_bytes
            = concatMap mk_string strings
         point_words
            = concatMap mk_point_words points
         comments
            = map mk_comment debugging_text
         pre_comments
            = map mk_comment ["", "---- start of the cost centers ----"]
         post_comments
            = map mk_comment ["", "---- end of the cost centers ----"]
         mk_point_words p@(fileno,lineno,funcno)
            = mk_comment (show p)
              : map mk_word [fileno, lineno, funcno, 0,0, 0,0]
         preamble
            = mk_dataSeg ++ mk_align

         mk_Pseudo      = Pseudo 0
         mk_label_def s = mk_arch_label_def s
         mk_word i      = "\t.long " ++ show i
         mk_word_l l    = "\t.long " ++ mk_arch_label_use l
         mk_byte i      = "\t.byte " ++ show i
         mk_comment s   = "\t# " ++ s
         mk_string s    = [mk_comment (show s)]
                          ++ map (mk_byte.ord) s
                          ++ [mk_byte 0]
         mk_dataSeg     = ["\t.data"]
         mk_align       = ["\t.align 4"]
     in
         map mk_Pseudo (
            concat [pre_comments, comments, 
                    preamble,
                    [mk_label_def tABLE_START_STRING],
                    length_words, string_bytes,
                    mk_align,
                    [mk_label_def bUCKETS_START_STRING],
                    point_words, post_comments]
         )



{-------------------------------------------}
{--- Generate calls to the               ---}
{--- cache simulator (part 4)            ---}
{-------------------------------------------}

synthLine :: Int -> Line -> (Int, [Line])
synthLine nextcc (Pseudo ln stuff) 
   = (nextcc, [Pseudo ln stuff])
synthLine nextcc (Label ln stuff) 
   = (nextcc, [Label ln stuff])
synthLine nextcc (Real ln cc insn)
   | hasRealAnns insn
   = (nextcc+1, map (Real ln cc) (synth_wrk nextcc insn))
   | otherwise
   = (nextcc, [Real ln cc insn])



synth_wrk :: Int -> Insn -> [Insn]
synth_wrk ccid_to_use insn@(Insn ann opcode operands)
   = concatMap (useAnnot ccid_to_use) (getAnns ann)
     ++ [insn]

insn_pushl reg
   = Insn (mkAnnC "save") O_pushl [OP_REG reg]
insn_popl reg
   = Insn (mkAnnC "rest") O_popl [OP_REG reg]

std_preamble
   = [insn_pushl EAX, insn_pushl EBX]
std_postamble
   = [insn_popl EBX, insn_popl EAX]

useAnnot :: Int -> Annot -> [Insn]
useAnnot ccid (AnnC c) 
   = internal "useAnnot on comment annotation"

useAnnot ccid (AnnR sz op)
   = std_preamble 
     ++
     [ Insn (mkAnnC "rd-1") O_leal 
                            [op, OP_REG EAX],
       Insn (mkAnnC "rd-2") O_movl 
                            [mk_bucket_addr ccid, OP_REG EBX],
       Insn (mkAnnC "rd-3") O_call 
                            [OP_D (Const [Pos (mk_rd_hook_name sz)])]
     ]
     ++ std_postamble

useAnnot ccid (AnnM sz op)
   = std_preamble 
     ++
     [ Insn (mkAnnC "mo-1") O_leal 
                            [op, OP_REG EAX],
       Insn (mkAnnC "mo-2") O_movl 
                            [mk_bucket_addr ccid, OP_REG EBX],
       Insn (mkAnnC "mo-3") O_call 
                            [OP_D (Const [Pos (mk_mo_hook_name sz)])]
     ]
     ++ std_postamble

useAnnot ccid (AnnW sz op)
   = std_preamble
     ++ 
     [ Insn (mkAnnC "wr-1") O_leal 
                            [op, OP_REG EAX],
       Insn (mkAnnC "wr-2") O_movl 
                            [mk_bucket_addr ccid, OP_REG EBX],
       Insn (mkAnnC "wr-3") O_call 
                            [OP_D (Const [Pos (mk_wr_hook_name sz)])]
     ]
     ++ std_postamble

mk_bucket_addr ccid
   = OP_D (Const [Pos (UF_LABEL 
                         ("$" ++ mk_arch_label_use bUCKETS_START_STRING)),
                  Pos (UF_NUM (show (28 * ccid)))])

mk_rd_hook_name sz
   = UF_NAME ("cacheprof_hook_Rd" ++ show sz)
mk_mo_hook_name sz
   = UF_NAME ("cacheprof_hook_Mo" ++ show sz)
mk_wr_hook_name sz
   = UF_NAME ("cacheprof_hook_Wr" ++ show sz)


{-----------------------------------------------------------}
{--- Stage 7 for level 1 profiling.  At each notifiable  ---}
{--- memory reference (ie, at each place where level 2   ---}
{--- profiling would insert a call to the cache          ---}
{--- simulator, just increment the total read/write      ---}
{--- counts.                                             ---}
{-----------------------------------------------------------}

-- section-main
synth_1 :: [Line] -> [Line]
synth_1 = concatMap synth_1_wrk

synth_1_wrk :: Line -> [Line]
synth_1_wrk (Pseudo ln stuff) 
   = [Pseudo ln stuff]
synth_1_wrk (Label ln stuff) 
   = [Label ln stuff]
synth_1_wrk line@(Real ln cc insn@(Insn ann opcode operands))
   | hasRealAnns insn
   = map (Real ln cc) (concatMap useIncAnns (getAnns ann))
     ++ [line]
   | otherwise
   = [Real ln cc insn]
     where
        useIncAnns (AnnW sz op)
           = incSequence 1 "cacheprof_level1_writes"
        useIncAnns (AnnR sz op)
           = incSequence 1 "cacheprof_level1_reads"
        useIncAnns (AnnM sz op)
           = incSequence 1 "cacheprof_level1_reads" ++
             incSequence 1 "cacheprof_level1_writes"

-- generate a sequence to increment a 64-bit counter in
-- memory, labelled "name", by k
incSequence :: Int -> String -> [Insn]
incSequence k name
   = [Insn DontAnnMe O_pushfl [],
      Insn DontAnnMe O_addl
                [OP_LIT (Const [Pos (UF_NUM (show k))]),
                 OP_D   (Const [Pos (UF_NAME name)])],
      Insn DontAnnMe O_adcl
                [OP_LIT (Const [Pos (UF_NUM "0")]),
                 OP_D   (Const [Pos (UF_NUM "4"),
                                Pos (UF_NAME name)])],
      Insn DontAnnMe O_popfl []
     ]


{-----------------------------------------------------------}
{--- Stage 8.  Peephole opt to remove some stupidities.  ---}
{-----------------------------------------------------------}

{- The idea is to clean up (eg)
        pushfl
        addl $3,cacheprof_icount
        adcl $0,4+cacheprof_icount
        popfl
        pushfl
        addl $1,cacheprof_level1_writes
        adcl $0,4+cacheprof_level1_writes
        popfl
   into 
        pushfl
        addl $3,cacheprof_icount
        adcl $0,4+cacheprof_icount
        addl $1,cacheprof_level1_writes
        adcl $0,4+cacheprof_level1_writes
        popfl
-}

-- section-main
peephole :: [Line] -> [Line]

peephole ( line1@(Real ln1 cc1 insn1) :
           line2@(Real ln2 cc2 insn2) :
           line3@(Real ln3 cc3 (Insn anns3 O_popfl [])) :
           line4@(Real ln4 cc4 (Insn anns4 O_pushfl [])) :
           line5@(Real ln5 cc5 insn5) :
           line6@(Real ln6 cc6 insn6) :
           the_rest )
   | incs_a_counter insn1 insn2 
     && incs_a_counter insn5 insn6
   = peephole (line1 : line2 : line5 : line6 : the_rest)

peephole ( line: the_rest)
   = line : peephole the_rest
peephole []
   = []


-- Say after me: We love pattern matching
incs_a_counter (Insn anns1 O_addl
                     [OP_LIT (Const [Pos (UF_NUM n)]),
                      OP_D   (Const [Pos (UF_NAME name1)])])
               (Insn anns2 O_adcl
                     [OP_LIT (Const [Pos (UF_NUM zero)]),
                      OP_D   (Const [Pos (UF_NUM four),
                                     Pos (UF_NAME name2)])])
   = take 10 name1 == "cacheprof_"
     && take 10 name2 == "cacheprof_"
     && zero == "0" && four == "4"

incs_a_counter insn1 insn2
   = False


{-----------------------------------------------------------}
{--- Stage 9.  Final cleanup -- zap debugging info.      ---}
{-----------------------------------------------------------}

-- section-main
final_cleanup :: [Line] -> String
final_cleanup
   = unlines . map ppu . filter (not.isStabLine)
     where
        isStabLine (Pseudo ln s) 
           = take 5 (dropWhile isSpace s) == ".stab"
        isStabLine other
           = False


{-----------------------------------------------------------}
{--- Main!                                               ---}
{-----------------------------------------------------------}

main = seq stderr (       -- avoid bug in ghc-4.04
       do args <- --getArgs
                  return ["--level2"]

          let prof_level
                 = if "--level0" `elem` args then 0
                   else if "--level1" `elem` args then 1
                   else if "--level2" `elem` args then 2
                   else internal 
                           "profiling level not supplied by `cacheprof'"

          let bad_ddump_flags
                 = filter (`notElem` ddump_flags) 
                      (filter ((== "--ddump-") . take 8) args)

          if (not (null bad_ddump_flags))
           then do hPutStr stderr (
                        "cacheann: bad debugging flag(s): " ++ 
                        unwords bad_ddump_flags ++ 
                        "\n   valid debugging flags are\n" ++
                        unlines (map ("      "++) ddump_flags)
                      )
                   exitWith (ExitFailure 1)
           else return ()

          ifVerb args (hPutStr stderr "cacheann-0.01: annotating ...\n")
          f   <- getContents
          aux <- case prof_level of
                  0 -> return ""
                  1 -> readFile "runtime_files/cacheprof_hooks1_x86.s"
                  2 -> readFile "runtime_files/cacheprof_hooks2_x86.s"

          out <- doFile prof_level args f
          putStr out
          putStr aux
          ifVerb args (hPutStr stderr "cacheann-0.01: done\n")
       )

ifVerb :: [String] -> IO () -> IO ()
ifVerb flags ioact 
   = if "-v" `elem` flags then ioact else return ()

doFile :: Int -> [String] -> String -> IO String
doFile prof_level args input_text
   = let preparsed      = preparse input_text
         parsed         = map forceLine (parse preparsed)
         simplified     = simplify parsed
         with_bbs_ident = identify_bbs simplified
         with_icounts   = use_bbs with_bbs_ident
         annotated      = annotate with_icounts
         with_ccs       = addCCs annotated
         with_synth_2   = synth_2 with_ccs
         with_synth_1   = synth_1 annotated
         with_synth     = case prof_level of
                             0 -> simplified
                             1 -> with_synth_1
                             2 -> with_synth_2
         peepholed      = peephole with_synth
         final          = final_cleanup peepholed

         debugging_io
            = do ifopt [0,1,2] ddump_preparsed  preparsed
                 ifopt [0,1,2] ddump_parsed     parsed
                 ifopt [0,1,2] ddump_simplified simplified
                 ifopt   [1,2] ddump_ident_bbs  with_bbs_ident
                 ifopt   [1,2] ddump_use_bbs    with_icounts
                 ifopt   [1,2] ddump_annotated  annotated
                 ifopt     [2] ddump_ccs        with_ccs
                 ifopt [0,1,2] ddump_synth      with_synth
                 ifopt [0,1,2] ddump_peephole   peepholed

         ifopt valid_levels flag stuff
            | prof_level `elem` valid_levels 
              && flag `elem` args
            = hPutStr stderr (
                 "\n\n-------- DEBUGGING OUTPUT FOR " 
                 ++ flag ++ ":\n\n"
                 ++ unlines (map ppd stuff)
                 ++ "\n\n" )
            | otherwise
            = return ()

         forceLine :: Line -> Line
         forceLine line | line == line = line
                        | otherwise    = internal "forceLine"
     in
         debugging_io >> return final


ddump_preparsed   = "--ddump-preparsed"
ddump_parsed      = "--ddump-parsed"
ddump_simplified  = "--ddump-simplified"
ddump_ident_bbs   = "--ddump-ident-bbs"
ddump_use_bbs     = "--ddump-use-bbs"
ddump_annotated   = "--ddump-annotated"
ddump_ccs         = "--ddump-ccs"
ddump_synth       = "--ddump-synth"
ddump_peephole    = "--ddump-peephole"

ddump_flags
   = [ddump_preparsed, ddump_parsed, ddump_simplified,
      ddump_ident_bbs, ddump_use_bbs, ddump_annotated,
      ddump_ccs, ddump_synth, ddump_peephole]

{------------------------------------------------------------------------}
{--- end                                                  CacheAnn.hs ---}
{------------------------------------------------------------------------}
