-----------------------------------------------------------------------------
--
-- Code generator utilities; mostly monadic
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module CgUtils (
        addIdReps,
        cgLit,
        emitDataLits, mkDataLits,
        emitRODataLits, mkRODataLits,
        emitIf, emitIfThenElse,
        emitRtsCall, emitRtsCallWithVols, emitRtsCallWithResult,
        emitRtsCallGen,
        assignTemp, assignTemp_, newTemp,
        emitSimultaneously,
        emitSwitch, emitLitSwitch,
        tagToClosure,

        callerSaves, callerSaveVolatileRegs, get_GlobalReg_addr,
        activeStgRegs, fixStgRegisters,

        cmmAndWord, cmmOrWord, cmmNegate, cmmEqWord, cmmNeWord,
        cmmUGtWord, cmmSubWord, cmmMulWord, cmmAddWord, cmmUShrWord,
        cmmOffsetExprW, cmmOffsetExprB,
        cmmRegOffW, cmmRegOffB,
        cmmLabelOffW, cmmLabelOffB,
        cmmOffsetW, cmmOffsetB,
        cmmOffsetLitW, cmmOffsetLitB,
        cmmLoadIndexW,
        cmmConstrTag, cmmConstrTag1,

        tagForCon, tagCons, isSmallFamily,
        cmmUntag, cmmIsTagged, cmmGetTag,

        addToMem, addToMemE,
        mkWordCLit,
        newStringCLit, newByteStringCLit,
        packHalfWordsCLit,
        blankWord,

        getSRTInfo
  ) where

#include "HsVersions.h"
#include "../includes/stg/MachRegs.h"

import BlockId
import CgMonad
import TyCon
import DataCon
import Id
import IdInfo
import Constants
import SMRep
import OldCmm
import OldCmmUtils
import CLabel
import ForeignCall
import ClosureInfo
import StgSyn (SRT(..))
import Module
import Literal
import Digraph
import ListSetOps
import Util
import DynFlags
import FastString
import Outputable

import Data.Char
import Data.Word
import Data.List
import Data.Maybe
import Data.Ord

-------------------------------------------------------------------------
--
--      Random small functions
--
-------------------------------------------------------------------------

addIdReps :: [Id] -> [(CgRep, Id)]
addIdReps ids = [(idCgRep id, id) | id <- ids]

-------------------------------------------------------------------------
--
--      Literals
--
-------------------------------------------------------------------------

cgLit :: Literal -> FCode CmmLit
cgLit (MachStr s) = newByteStringCLit (bytesFS s)
 -- not unpackFS; we want the UTF-8 byte stream.
cgLit other_lit   = return (mkSimpleLit other_lit)

mkSimpleLit :: Literal -> CmmLit
mkSimpleLit (MachChar   c)    = CmmInt (fromIntegral (ord c)) wordWidth
mkSimpleLit MachNullAddr      = zeroCLit
mkSimpleLit (MachInt i)       = CmmInt i wordWidth
mkSimpleLit (MachInt64 i)     = CmmInt i W64
mkSimpleLit (MachWord i)      = CmmInt i wordWidth
mkSimpleLit (MachWord64 i)    = CmmInt i W64
mkSimpleLit (MachFloat r)     = CmmFloat r W32
mkSimpleLit (MachDouble r)    = CmmFloat r W64
mkSimpleLit (MachLabel fs ms fod)
        = CmmLabel (mkForeignLabel fs ms labelSrc fod)
        where
                -- TODO: Literal labels might not actually be in the current package...
                labelSrc = ForeignLabelInThisPackage
mkSimpleLit (MachStr _) = panic "mkSimpleLit: MachStr"
-- No LitInteger's should be left by the time this is called. CorePrep
-- should have converted them all to a real core representation.
mkSimpleLit (LitInteger {}) = panic "mkSimpleLit: LitInteger"

mkLtOp :: Literal -> MachOp
-- On signed literals we must do a signed comparison
mkLtOp (MachInt _)    = MO_S_Lt wordWidth
mkLtOp (MachFloat _)  = MO_F_Lt W32
mkLtOp (MachDouble _) = MO_F_Lt W64
mkLtOp lit            = MO_U_Lt (typeWidth (cmmLitType (mkSimpleLit lit)))


---------------------------------------------------
--
--      Cmm data type functions
--
---------------------------------------------------



{-
   The family size of a data type (the number of constructors)
   can be either:
    * small, if the family size < 2**tag_bits
    * big, otherwise.

   Small families can have the constructor tag in the tag
   bits.
   Big families only use the tag value 1 to represent
   evaluatedness.
-}
isSmallFamily :: Int -> Bool
isSmallFamily fam_size = fam_size <= mAX_PTR_TAG

tagForCon :: DataCon -> ConTagZ
tagForCon con = tag
    where
    con_tag           = dataConTagZ con
    fam_size   = tyConFamilySize (dataConTyCon con)
    tag | isSmallFamily fam_size = con_tag + 1
        | otherwise              = 1

--Tag an expression, to do: refactor, this appears in some other module.
tagCons :: DataCon -> CmmExpr -> CmmExpr
tagCons con expr = cmmOffsetB expr (tagForCon con)

--------------------------------------------------------------------------
--
-- Incrementing a memory location
--
--------------------------------------------------------------------------

addToMem :: Width       -- rep of the counter
         -> CmmExpr     -- Address
         -> Int         -- What to add (a word)
         -> CmmStmt
addToMem width ptr n = addToMemE width ptr (CmmLit (CmmInt (toInteger n) width))

addToMemE :: Width      -- rep of the counter
          -> CmmExpr    -- Address
          -> CmmExpr    -- What to add (a word-typed expression)
          -> CmmStmt
addToMemE width ptr n
  = CmmStore ptr (CmmMachOp (MO_Add width) [CmmLoad ptr (cmmBits width), n])

-------------------------------------------------------------------------
--
--      Converting a closure tag to a closure for enumeration types
--      (this is the implementation of tagToEnum#).
--
-------------------------------------------------------------------------

tagToClosure :: TyCon -> CmmExpr -> CmmExpr
tagToClosure tycon tag
  = CmmLoad (cmmOffsetExprW closure_tbl tag) gcWord
  where closure_tbl = CmmLit (CmmLabel lbl)
        lbl = mkClosureTableLabel (tyConName tycon) NoCafRefs

-------------------------------------------------------------------------
--
--      Conditionals and rts calls
--
-------------------------------------------------------------------------

emitIf :: CmmExpr       -- Boolean
       -> Code          -- Then part
       -> Code
-- Emit (if e then x)
-- ToDo: reverse the condition to avoid the extra branch instruction if possible
-- (some conditionals aren't reversible. eg. floating point comparisons cannot
-- be inverted because there exist some values for which both comparisons
-- return False, such as NaN.)
emitIf cond then_part
  = do { then_id <- newLabelC
       ; join_id <- newLabelC
       ; stmtC (CmmCondBranch cond then_id)
       ; stmtC (CmmBranch join_id)
       ; labelC then_id
       ; then_part
       ; labelC join_id
       }

emitIfThenElse :: CmmExpr       -- Boolean
                -> Code         -- Then part
                -> Code         -- Else part
                -> Code
-- Emit (if e then x else y)
emitIfThenElse cond then_part else_part
  = do { then_id <- newLabelC
       ; join_id <- newLabelC
       ; stmtC (CmmCondBranch cond then_id)
       ; else_part
       ; stmtC (CmmBranch join_id)
       ; labelC then_id
       ; then_part
       ; labelC join_id
       }


-- | Emit code to call a Cmm function.
emitRtsCall
   :: PackageId                 -- ^ package the function is in
   -> FastString                -- ^ name of function
   -> [CmmHinted CmmExpr]       -- ^ function args
   -> Code                      -- ^ cmm code

emitRtsCall pkg fun args = emitRtsCallGen [] pkg fun args Nothing
   -- The 'Nothing' says "save all global registers"

emitRtsCallWithVols :: PackageId -> FastString -> [CmmHinted CmmExpr] -> [GlobalReg] -> Code
emitRtsCallWithVols pkg fun args vols
   = emitRtsCallGen [] pkg fun args (Just vols)

emitRtsCallWithResult
   :: LocalReg -> ForeignHint
   -> PackageId -> FastString
   -> [CmmHinted CmmExpr] -> Code

emitRtsCallWithResult res hint pkg fun args
   = emitRtsCallGen [CmmHinted res hint] pkg fun args Nothing

-- Make a call to an RTS C procedure
emitRtsCallGen
   :: [CmmHinted LocalReg]
   -> PackageId
   -> FastString
   -> [CmmHinted CmmExpr]
   -> Maybe [GlobalReg]
   -> Code
emitRtsCallGen res pkg fun args vols = do
  stmtsC caller_save
  stmtC (CmmCall target res args CmmMayReturn)
  stmtsC caller_load
  where
    (caller_save, caller_load) = callerSaveVolatileRegs vols
    target   = CmmCallee fun_expr CCallConv
    fun_expr = mkLblExpr (mkCmmCodeLabel pkg fun)

-----------------------------------------------------------------------------
--
--      Caller-Save Registers
--
-----------------------------------------------------------------------------

-- Here we generate the sequence of saves/restores required around a
-- foreign call instruction.

-- TODO: reconcile with includes/Regs.h
--  * Regs.h claims that BaseReg should be saved last and loaded first
--    * This might not have been tickled before since BaseReg is callee save
--  * Regs.h saves SparkHd, ParkT1, SparkBase and SparkLim
callerSaveVolatileRegs :: Maybe [GlobalReg] -> ([CmmStmt], [CmmStmt])
callerSaveVolatileRegs vols = (caller_save, caller_load)
  where
    caller_save = foldr ($!) [] (map callerSaveGlobalReg    regs_to_save)
    caller_load = foldr ($!) [] (map callerRestoreGlobalReg regs_to_save)

    system_regs = [Sp,SpLim,Hp,HpLim,CCCS,CurrentTSO,CurrentNursery,
                   {-SparkHd,SparkTl,SparkBase,SparkLim,-}BaseReg ]

    regs_to_save = system_regs ++ vol_list

    vol_list = case vols of Nothing -> all_of_em; Just regs -> regs

    all_of_em = [ VanillaReg n VNonGcPtr | n <- [0..mAX_Vanilla_REG] ]
                        -- The VNonGcPtr is a lie, but I don't think it matters
             ++ [ FloatReg   n | n <- [0..mAX_Float_REG] ]
             ++ [ DoubleReg  n | n <- [0..mAX_Double_REG] ]
             ++ [ LongReg    n | n <- [0..mAX_Long_REG] ]

    callerSaveGlobalReg reg next
        | callerSaves reg =
                CmmStore (get_GlobalReg_addr reg)
                         (CmmReg (CmmGlobal reg)) : next
        | otherwise = next

    callerRestoreGlobalReg reg next
        | callerSaves reg =
                CmmAssign (CmmGlobal reg)
                          (CmmLoad (get_GlobalReg_addr reg) (globalRegType reg))
                        : next
        | otherwise = next


-- | Returns @True@ if this global register is stored in a caller-saves
-- machine register.

callerSaves :: GlobalReg -> Bool

#ifdef CALLER_SAVES_Base
callerSaves BaseReg             = True
#endif
#ifdef CALLER_SAVES_R1
callerSaves (VanillaReg 1 _)    = True
#endif
#ifdef CALLER_SAVES_R2
callerSaves (VanillaReg 2 _)    = True
#endif
#ifdef CALLER_SAVES_R3
callerSaves (VanillaReg 3 _)    = True
#endif
#ifdef CALLER_SAVES_R4
callerSaves (VanillaReg 4 _)    = True
#endif
#ifdef CALLER_SAVES_R5
callerSaves (VanillaReg 5 _)    = True
#endif
#ifdef CALLER_SAVES_R6
callerSaves (VanillaReg 6 _)    = True
#endif
#ifdef CALLER_SAVES_R7
callerSaves (VanillaReg 7 _)    = True
#endif
#ifdef CALLER_SAVES_R8
callerSaves (VanillaReg 8 _)    = True
#endif
#ifdef CALLER_SAVES_R9
callerSaves (VanillaReg 9 _)    = True
#endif
#ifdef CALLER_SAVES_R10
callerSaves (VanillaReg 10 _)   = True
#endif
#ifdef CALLER_SAVES_F1
callerSaves (FloatReg 1)        = True
#endif
#ifdef CALLER_SAVES_F2
callerSaves (FloatReg 2)        = True
#endif
#ifdef CALLER_SAVES_F3
callerSaves (FloatReg 3)        = True
#endif
#ifdef CALLER_SAVES_F4
callerSaves (FloatReg 4)        = True
#endif
#ifdef CALLER_SAVES_D1
callerSaves (DoubleReg 1)       = True
#endif
#ifdef CALLER_SAVES_D2
callerSaves (DoubleReg 2)       = True
#endif
#ifdef CALLER_SAVES_L1
callerSaves (LongReg 1)         = True
#endif
#ifdef CALLER_SAVES_Sp
callerSaves Sp                  = True
#endif
#ifdef CALLER_SAVES_SpLim
callerSaves SpLim               = True
#endif
#ifdef CALLER_SAVES_Hp
callerSaves Hp                  = True
#endif
#ifdef CALLER_SAVES_HpLim
callerSaves HpLim               = True
#endif
#ifdef CALLER_SAVES_CCCS
callerSaves CCCS                = True
#endif
#ifdef CALLER_SAVES_CurrentTSO
callerSaves CurrentTSO          = True
#endif
#ifdef CALLER_SAVES_CurrentNursery
callerSaves CurrentNursery      = True
#endif
callerSaves _                   = False


-- -----------------------------------------------------------------------------
-- Information about global registers

baseRegOffset :: GlobalReg -> Int

baseRegOffset (VanillaReg 1 _)    = oFFSET_StgRegTable_rR1
baseRegOffset (VanillaReg 2 _)    = oFFSET_StgRegTable_rR2
baseRegOffset (VanillaReg 3 _)    = oFFSET_StgRegTable_rR3
baseRegOffset (VanillaReg 4 _)    = oFFSET_StgRegTable_rR4
baseRegOffset (VanillaReg 5 _)    = oFFSET_StgRegTable_rR5
baseRegOffset (VanillaReg 6 _)    = oFFSET_StgRegTable_rR6
baseRegOffset (VanillaReg 7 _)    = oFFSET_StgRegTable_rR7
baseRegOffset (VanillaReg 8 _)    = oFFSET_StgRegTable_rR8
baseRegOffset (VanillaReg 9 _)    = oFFSET_StgRegTable_rR9
baseRegOffset (VanillaReg 10 _)   = oFFSET_StgRegTable_rR10
baseRegOffset (VanillaReg n _)    = panic ("Registers above R10 are not supported (tried to use R" ++ show n ++ ")")
baseRegOffset (FloatReg  1)       = oFFSET_StgRegTable_rF1
baseRegOffset (FloatReg  2)       = oFFSET_StgRegTable_rF2
baseRegOffset (FloatReg  3)       = oFFSET_StgRegTable_rF3
baseRegOffset (FloatReg  4)       = oFFSET_StgRegTable_rF4
baseRegOffset (FloatReg  n)       = panic ("Registers above F4 are not supported (tried to use F" ++ show n ++ ")")
baseRegOffset (DoubleReg 1)       = oFFSET_StgRegTable_rD1
baseRegOffset (DoubleReg 2)       = oFFSET_StgRegTable_rD2
baseRegOffset (DoubleReg n)       = panic ("Registers above D2 are not supported (tried to use D" ++ show n ++ ")")
baseRegOffset Sp                  = oFFSET_StgRegTable_rSp
baseRegOffset SpLim               = oFFSET_StgRegTable_rSpLim
baseRegOffset (LongReg 1)         = oFFSET_StgRegTable_rL1
baseRegOffset (LongReg n)         = panic ("Registers above L1 are not supported (tried to use L" ++ show n ++ ")")
baseRegOffset Hp                  = oFFSET_StgRegTable_rHp
baseRegOffset HpLim               = oFFSET_StgRegTable_rHpLim
baseRegOffset CCCS                = oFFSET_StgRegTable_rCCCS
baseRegOffset CurrentTSO          = oFFSET_StgRegTable_rCurrentTSO
baseRegOffset CurrentNursery      = oFFSET_StgRegTable_rCurrentNursery
baseRegOffset HpAlloc             = oFFSET_StgRegTable_rHpAlloc
baseRegOffset EagerBlackholeInfo  = oFFSET_stgEagerBlackholeInfo
baseRegOffset GCEnter1            = oFFSET_stgGCEnter1
baseRegOffset GCFun               = oFFSET_stgGCFun
baseRegOffset BaseReg             = panic "baseRegOffset:BaseReg"
baseRegOffset PicBaseReg          = panic "baseRegOffset:PicBaseReg"


-------------------------------------------------------------------------
--
--      Strings generate a top-level data block
--
-------------------------------------------------------------------------

emitDataLits :: CLabel -> [CmmLit] -> Code
-- Emit a data-segment data block
emitDataLits lbl lits = emitDecl (mkDataLits Data lbl lits)

emitRODataLits :: String -> CLabel -> [CmmLit] -> Code
-- Emit a read-only data block
emitRODataLits _caller lbl lits
  = emitDecl (mkRODataLits lbl lits)

newStringCLit :: String -> FCode CmmLit
-- Make a global definition for the string,
-- and return its label
newStringCLit str = newByteStringCLit (map (fromIntegral.ord) str)

newByteStringCLit :: [Word8] -> FCode CmmLit
newByteStringCLit bytes
  = do  { uniq <- newUnique
        ; let (lit, decl) = mkByteStringCLit uniq bytes
        ; emitDecl decl
        ; return lit }

-------------------------------------------------------------------------
--
--      Assigning expressions to temporaries
--
-------------------------------------------------------------------------

-- | If the expression is trivial, return it.  Otherwise, assign the
-- expression to a temporary register and return an expression
-- referring to this register.
assignTemp :: CmmExpr -> FCode CmmExpr
-- For a non-trivial expression, e, create a local
-- variable and assign the expression to it
assignTemp e
  | isTrivialCmmExpr e = return e
  | otherwise          = do { reg <- newTemp (cmmExprType e)
                            ; stmtC (CmmAssign (CmmLocal reg) e)
                            ; return (CmmReg (CmmLocal reg)) }

-- | If the expression is trivial and doesn't refer to a global
-- register, return it.  Otherwise, assign the expression to a
-- temporary register and return an expression referring to this
-- register.
assignTemp_ :: CmmExpr -> FCode CmmExpr
assignTemp_ e
    | isTrivialCmmExpr e && hasNoGlobalRegs e = return e
    | otherwise = do
        reg <- newTemp (cmmExprType e)
        stmtC (CmmAssign (CmmLocal reg) e)
        return (CmmReg (CmmLocal reg))

newTemp :: CmmType -> FCode LocalReg
newTemp rep = do { uniq <- newUnique; return (LocalReg uniq rep) }

-------------------------------------------------------------------------
--
--      Building case analysis
--
-------------------------------------------------------------------------

emitSwitch
        :: CmmExpr                -- Tag to switch on
        -> [(ConTagZ, CgStmts)]   -- Tagged branches
        -> Maybe CgStmts          -- Default branch (if any)
        -> ConTagZ -> ConTagZ     -- Min and Max possible values; behaviour
                                  --    outside this range is undefined
        -> Code

-- ONLY A DEFAULT BRANCH: no case analysis to do
emitSwitch _ [] (Just stmts) _ _
  = emitCgStmts stmts

-- Right, off we go
emitSwitch tag_expr branches mb_deflt lo_tag hi_tag
  =     -- Just sort the branches before calling mk_sritch
    do  { mb_deflt_id <-
                case mb_deflt of
                  Nothing    -> return Nothing
                  Just stmts -> do id <- forkCgStmts stmts; return (Just id)

        ; dflags <- getDynFlags
        ; let via_C | HscC <- hscTarget dflags = True
                    | otherwise                = False

        ; stmts <- mk_switch tag_expr (sortBy (comparing fst) branches)
                        mb_deflt_id lo_tag hi_tag via_C
        ; emitCgStmts stmts
        }


mk_switch :: CmmExpr -> [(ConTagZ, CgStmts)]
          -> Maybe BlockId -> ConTagZ -> ConTagZ -> Bool
          -> FCode CgStmts

-- SINGLETON TAG RANGE: no case analysis to do
mk_switch _tag_expr [(tag,stmts)] _ lo_tag hi_tag _via_C
  | lo_tag == hi_tag
  = ASSERT( tag == lo_tag )
    return stmts

-- SINGLETON BRANCH, NO DEFAULT: no case analysis to do
mk_switch _tag_expr [(_tag,stmts)] Nothing _lo_tag _hi_tag _via_C
  = return stmts
        -- The simplifier might have eliminated a case
        --       so we may have e.g. case xs of
        --                               [] -> e
        -- In that situation we can be sure the (:) case
        -- can't happen, so no need to test

-- SINGLETON BRANCH: one equality check to do
mk_switch tag_expr [(tag,stmts)] (Just deflt) _lo_tag _hi_tag _via_C
  = return (CmmCondBranch cond deflt `consCgStmt` stmts)
  where
    cond  =  cmmNeWord tag_expr (CmmLit (mkIntCLit tag))
        -- We have lo_tag < hi_tag, but there's only one branch,
        -- so there must be a default

-- ToDo: we might want to check for the two branch case, where one of
-- the branches is the tag 0, because comparing '== 0' is likely to be
-- more efficient than other kinds of comparison.

-- DENSE TAG RANGE: use a switch statment.
--
-- We also use a switch uncoditionally when compiling via C, because
-- this will get emitted as a C switch statement and the C compiler
-- should do a good job of optimising it.  Also, older GCC versions
-- (2.95 in particular) have problems compiling the complicated
-- if-trees generated by this code, so compiling to a switch every
-- time works around that problem.
--
mk_switch tag_expr branches mb_deflt lo_tag hi_tag via_C
  | use_switch  -- Use a switch
  = do  { branch_ids <- mapM forkCgStmts (map snd branches)
        ; let
                tagged_blk_ids = zip (map fst branches) (map Just branch_ids)

                find_branch :: ConTagZ -> Maybe BlockId
                find_branch i = assocDefault mb_deflt tagged_blk_ids i

                -- NB. we have eliminated impossible branches at
                -- either end of the range (see below), so the first
                -- tag of a real branch is real_lo_tag (not lo_tag).
                arms = [ find_branch i | i <- [real_lo_tag..real_hi_tag]]

                switch_stmt = CmmSwitch (cmmOffset tag_expr (- real_lo_tag)) arms

        ; ASSERT(not (all isNothing arms))
          return (oneCgStmt switch_stmt)
        }

  -- if we can knock off a bunch of default cases with one if, then do so
  | Just deflt <- mb_deflt, (lowest_branch - lo_tag) >= n_branches
  = do { (assign_tag, tag_expr') <- assignTemp' tag_expr
       ; let cond = cmmULtWord tag_expr' (CmmLit (mkIntCLit lowest_branch))
             branch = CmmCondBranch cond deflt
       ; stmts <- mk_switch tag_expr' branches mb_deflt
                        lowest_branch hi_tag via_C
       ; return (assign_tag `consCgStmt` (branch `consCgStmt` stmts))
       }

  | Just deflt <- mb_deflt, (hi_tag - highest_branch) >= n_branches
  = do { (assign_tag, tag_expr') <- assignTemp' tag_expr
       ; let cond = cmmUGtWord tag_expr' (CmmLit (mkIntCLit highest_branch))
             branch = CmmCondBranch cond deflt
       ; stmts <- mk_switch tag_expr' branches mb_deflt
                        lo_tag highest_branch via_C
       ; return (assign_tag `consCgStmt` (branch `consCgStmt` stmts))
       }

  | otherwise   -- Use an if-tree
  = do  { (assign_tag, tag_expr') <- assignTemp' tag_expr
                -- To avoid duplication
        ; lo_stmts <- mk_switch tag_expr' lo_branches mb_deflt
                                lo_tag (mid_tag-1) via_C
        ; hi_stmts <- mk_switch tag_expr' hi_branches mb_deflt
                                mid_tag hi_tag via_C
        ; hi_id <- forkCgStmts hi_stmts
        ; let cond = cmmUGeWord tag_expr' (CmmLit (mkIntCLit mid_tag))
              branch_stmt = CmmCondBranch cond hi_id
        ; return (assign_tag `consCgStmt` (branch_stmt `consCgStmt` lo_stmts))
        }
        -- we test (e >= mid_tag) rather than (e < mid_tag), because
        -- the former works better when e is a comparison, and there
        -- are two tags 0 & 1 (mid_tag == 1).  In this case, the code
        -- generator can reduce the condition to e itself without
        -- having to reverse the sense of the comparison: comparisons
        -- can't always be easily reversed (eg. floating
        -- pt. comparisons).
  where
    use_switch   = {- pprTrace "mk_switch" (
                        ppr tag_expr <+> text "n_tags:" <+> int n_tags <+>
                        text "branches:" <+> ppr (map fst branches) <+>
                        text "n_branches:" <+> int n_branches <+>
                        text "lo_tag:" <+> int lo_tag <+>
                        text "hi_tag:" <+> int hi_tag <+>
                        text "real_lo_tag:" <+> int real_lo_tag <+>
                        text "real_hi_tag:" <+> int real_hi_tag) $ -}
                   ASSERT( n_branches > 1 && n_tags > 1 )
                   n_tags > 2 && (via_C || (dense && big_enough))
                 -- up to 4 branches we use a decision tree, otherwise
                 -- a switch (== jump table in the NCG).  This seems to be
                 -- optimal, and corresponds with what gcc does.
    big_enough   = n_branches > 4
    dense        = n_branches > (n_tags `div` 2)
    n_branches   = length branches

    -- ignore default slots at each end of the range if there's
    -- no default branch defined.
    lowest_branch  = fst (head branches)
    highest_branch = fst (last branches)

    real_lo_tag
        | isNothing mb_deflt = lowest_branch
        | otherwise          = lo_tag

    real_hi_tag
        | isNothing mb_deflt = highest_branch
        | otherwise          = hi_tag

    n_tags = real_hi_tag - real_lo_tag + 1

        -- INVARIANT: Provided hi_tag > lo_tag (which is true)
        --      lo_tag <= mid_tag < hi_tag
        --      lo_branches have tags <  mid_tag
        --      hi_branches have tags >= mid_tag

    (mid_tag,_) = branches !! (n_branches `div` 2)
        -- 2 branches => n_branches `div` 2 = 1
        --            => branches !! 1 give the *second* tag
        -- There are always at least 2 branches here

    (lo_branches, hi_branches) = span is_lo branches
    is_lo (t,_) = t < mid_tag

assignTemp' :: CmmExpr -> FCode (CmmStmt, CmmExpr)
assignTemp' e
  | isTrivialCmmExpr e = return (CmmNop, e)
  | otherwise          = do { reg <- newTemp (cmmExprType e)
                            ; return (CmmAssign (CmmLocal reg) e, CmmReg (CmmLocal reg)) }

emitLitSwitch :: CmmExpr                        -- Tag to switch on
              -> [(Literal, CgStmts)]           -- Tagged branches
              -> CgStmts                        -- Default branch (always)
              -> Code                           -- Emit the code
-- Used for general literals, whose size might not be a word,
-- where there is always a default case, and where we don't know
-- the range of values for certain.  For simplicity we always generate a tree.
--
-- ToDo: for integers we could do better here, perhaps by generalising
-- mk_switch and using that.  --SDM 15/09/2004
emitLitSwitch _     []       deflt = emitCgStmts deflt
emitLitSwitch scrut branches deflt_blk
  = do  { scrut' <- assignTemp scrut
        ; deflt_blk_id <- forkCgStmts deflt_blk
        ; blk <- mk_lit_switch scrut' deflt_blk_id (sortBy (comparing fst) branches)
        ; emitCgStmts blk }

mk_lit_switch :: CmmExpr -> BlockId
              -> [(Literal,CgStmts)]
              -> FCode CgStmts
mk_lit_switch scrut deflt_blk_id [(lit,blk)]
  = return (consCgStmt if_stmt blk)
  where
    cmm_lit = mkSimpleLit lit
    rep     = cmmLitType cmm_lit
    ne      = if isFloatType rep then MO_F_Ne else MO_Ne
    cond    = CmmMachOp (ne (typeWidth rep)) [scrut, CmmLit cmm_lit]
    if_stmt = CmmCondBranch cond deflt_blk_id

mk_lit_switch scrut deflt_blk_id branches
  = do  { hi_blk <- mk_lit_switch scrut deflt_blk_id hi_branches
        ; lo_blk <- mk_lit_switch scrut deflt_blk_id lo_branches
        ; lo_blk_id <- forkCgStmts lo_blk
        ; let if_stmt = CmmCondBranch cond lo_blk_id
        ; return (if_stmt `consCgStmt` hi_blk) }
  where
    n_branches = length branches
    (mid_lit,_) = branches !! (n_branches `div` 2)
        -- See notes above re mid_tag

    (lo_branches, hi_branches) = span is_lo branches
    is_lo (t,_) = t < mid_lit

    cond    = CmmMachOp (mkLtOp mid_lit)
                        [scrut, CmmLit (mkSimpleLit mid_lit)]

-------------------------------------------------------------------------
--
--      Simultaneous assignment
--
-------------------------------------------------------------------------


emitSimultaneously :: CmmStmts -> Code
-- Emit code to perform the assignments in the
-- input simultaneously, using temporary variables when necessary.
--
-- The Stmts must be:
--      CmmNop, CmmComment, CmmAssign, CmmStore
-- and nothing else


-- We use the strongly-connected component algorithm, in which
--      * the vertices are the statements
--      * an edge goes from s1 to s2 iff
--              s1 assigns to something s2 uses
--        that is, if s1 should *follow* s2 in the final order

type CVertex = (Int, CmmStmt)   -- Give each vertex a unique number,
                                -- for fast comparison

emitSimultaneously stmts
  = codeOnly $
    case filterOut isNopStmt (stmtList stmts) of
        -- Remove no-ops
      []        -> nopC
      [stmt]    -> stmtC stmt   -- It's often just one stmt
      stmt_list -> doSimultaneously1 (zip [(1::Int)..] stmt_list)

doSimultaneously1 :: [CVertex] -> Code
doSimultaneously1 vertices
  = let
        edges = [ (vertex, key1, edges_from stmt1)
                | vertex@(key1, stmt1) <- vertices
                ]
        edges_from stmt1 = [ key2 | (key2, stmt2) <- vertices,
                                    stmt1 `mustFollow` stmt2
                           ]
        components = stronglyConnCompFromEdgedVertices edges

        -- do_components deal with one strongly-connected component
        -- Not cyclic, or singleton?  Just do it
        do_component (AcyclicSCC (_n, stmt))  = stmtC stmt
        do_component (CyclicSCC [])
            = panic "doSimultaneously1: do_component (CyclicSCC [])"
        do_component (CyclicSCC [(_n, stmt)]) = stmtC stmt

                -- Cyclic?  Then go via temporaries.  Pick one to
                -- break the loop and try again with the rest.
        do_component (CyclicSCC ((_n, first_stmt) : rest))
          = do  { from_temp <- go_via_temp first_stmt
                ; doSimultaneously1 rest
                ; stmtC from_temp }

        go_via_temp (CmmAssign dest src)
          = do  { tmp <- newTemp (cmmRegType dest) -- TODO FIXME NOW if the pair of assignments move across a call this will be wrong
                ; stmtC (CmmAssign (CmmLocal tmp) src)
                ; return (CmmAssign dest (CmmReg (CmmLocal tmp))) }
        go_via_temp (CmmStore dest src)
          = do  { tmp <- newTemp (cmmExprType src) -- TODO FIXME NOW if the pair of assignments move across a call this will be wrong
                ; stmtC (CmmAssign (CmmLocal tmp) src)
                ; return (CmmStore dest (CmmReg (CmmLocal tmp))) }
        go_via_temp _ = panic "doSimultaneously1: go_via_temp"
    in
    mapCs do_component components

mustFollow :: CmmStmt -> CmmStmt -> Bool
CmmAssign reg _  `mustFollow` stmt = anySrc (reg `regUsedIn`) stmt
CmmStore loc e   `mustFollow` stmt = anySrc (locUsedIn loc (cmmExprType e)) stmt
CmmNop           `mustFollow` _    = False
CmmComment _     `mustFollow` _    = False
_                `mustFollow` _    = panic "mustFollow"


anySrc :: (CmmExpr -> Bool) -> CmmStmt -> Bool
-- True if the fn is true of any input of the stmt
anySrc p (CmmAssign _ e)    = p e
anySrc p (CmmStore e1 e2)   = p e1 || p e2      -- Might be used in either side
anySrc _ (CmmComment _)     = False
anySrc _ CmmNop             = False
anySrc _ _                  = True              -- Conservative

locUsedIn :: CmmExpr -> CmmType -> CmmExpr -> Bool
-- (locUsedIn a r e) checks whether writing to r[a] could affect the value of
-- 'e'.  Returns True if it's not sure.
locUsedIn _   _   (CmmLit _)         = False
locUsedIn loc rep (CmmLoad e ld_rep) = possiblySameLoc loc rep e ld_rep
locUsedIn _   _   (CmmReg _)         = False
locUsedIn _   _   (CmmRegOff _ _)    = False
locUsedIn loc rep (CmmMachOp _ es)   = any (locUsedIn loc rep) es
locUsedIn _   _   (CmmStackSlot _ _) = panic "locUsedIn: CmmStackSlot"

possiblySameLoc :: CmmExpr -> CmmType -> CmmExpr -> CmmType -> Bool
-- Assumes that distinct registers (eg Hp, Sp) do not
-- point to the same location, nor any offset thereof.
possiblySameLoc (CmmReg r1)           _    (CmmReg r2)           _ = r1 == r2
possiblySameLoc (CmmReg r1)           _    (CmmRegOff r2 0)      _ = r1 == r2
possiblySameLoc (CmmRegOff r1 0)      _    (CmmReg r2)           _ = r1 == r2
possiblySameLoc (CmmRegOff r1 start1) rep1 (CmmRegOff r2 start2) rep2
  = r1==r2 && end1 > start2 && end2 > start1
  where
    end1 = start1 + widthInBytes (typeWidth rep1)
    end2 = start2 + widthInBytes (typeWidth rep2)

possiblySameLoc _  _    (CmmLit _) _    = False
possiblySameLoc _  _    _          _    = True  -- Conservative

-------------------------------------------------------------------------
--
--      Static Reference Tables
--
-------------------------------------------------------------------------

-- There is just one SRT for each top level binding; all the nested
-- bindings use sub-sections of this SRT.  The label is passed down to
-- the nested bindings via the monad.

getSRTInfo :: FCode C_SRT
getSRTInfo = do
  srt_lbl <- getSRTLabel
  srt <- getSRT
  case srt of
    -- TODO: Should we panic in this case?
    -- Someone obviously thinks there should be an SRT
    NoSRT -> return NoC_SRT
    SRTEntries {} -> panic "getSRTInfo: SRTEntries.  Perhaps you forgot to run SimplStg?"
    SRT off len bmp
      | len > hALF_WORD_SIZE_IN_BITS || bmp == [fromIntegral srt_escape]
      -> do id <- newUnique
            let srt_desc_lbl = mkLargeSRTLabel id
            emitRODataLits "getSRTInfo" srt_desc_lbl
             ( cmmLabelOffW srt_lbl off
               : mkWordCLit (fromIntegral len)
               : map mkWordCLit bmp)
            return (C_SRT srt_desc_lbl 0 srt_escape)

      | otherwise
      -> return (C_SRT srt_lbl off (fromIntegral (head bmp)))
                -- The fromIntegral converts to StgHalfWord

srt_escape :: StgHalfWord
srt_escape = -1

-- -----------------------------------------------------------------------------
--
-- STG/Cmm GlobalReg
--
-- -----------------------------------------------------------------------------

-- | Here is where the STG register map is defined for each target arch.
-- The order matters (for the llvm backend anyway)! We must make sure to
-- maintain the order here with the order used in the LLVM calling conventions.
-- Note that also, this isn't all registers, just the ones that are currently
-- possbily mapped to real registers.
activeStgRegs :: [GlobalReg]
activeStgRegs = [
#ifdef REG_Base
    BaseReg
#endif
#ifdef REG_Sp
    ,Sp
#endif
#ifdef REG_Hp
    ,Hp
#endif
#ifdef REG_R1
    ,VanillaReg 1 VGcPtr
#endif
#ifdef REG_R2
    ,VanillaReg 2 VGcPtr
#endif
#ifdef REG_R3
    ,VanillaReg 3 VGcPtr
#endif
#ifdef REG_R4
    ,VanillaReg 4 VGcPtr
#endif
#ifdef REG_R5
    ,VanillaReg 5 VGcPtr
#endif
#ifdef REG_R6
    ,VanillaReg 6 VGcPtr
#endif
#ifdef REG_R7
    ,VanillaReg 7 VGcPtr
#endif
#ifdef REG_R8
    ,VanillaReg 8 VGcPtr
#endif
#ifdef REG_R9
    ,VanillaReg 9 VGcPtr
#endif
#ifdef REG_R10
    ,VanillaReg 10 VGcPtr
#endif
#ifdef REG_SpLim
    ,SpLim
#endif
#ifdef REG_F1
    ,FloatReg 1
#endif
#ifdef REG_F2
    ,FloatReg 2
#endif
#ifdef REG_F3
    ,FloatReg 3
#endif
#ifdef REG_F4
    ,FloatReg 4
#endif
#ifdef REG_D1
    ,DoubleReg 1
#endif
#ifdef REG_D2
    ,DoubleReg 2
#endif
    ]

-- | We map STG registers onto appropriate CmmExprs.  Either they map
-- to real machine registers or stored as offsets from BaseReg.  Given
-- a GlobalReg, get_GlobalReg_addr always produces the
-- register table address for it.
get_GlobalReg_addr :: GlobalReg -> CmmExpr
get_GlobalReg_addr BaseReg = regTableOffset 0
get_GlobalReg_addr mid     = get_Regtable_addr_from_offset
                                (globalRegType mid) (baseRegOffset mid)

-- Calculate a literal representing an offset into the register table.
-- Used when we don't have an actual BaseReg to offset from.
regTableOffset :: Int -> CmmExpr
regTableOffset n =
  CmmLit (CmmLabelOff mkMainCapabilityLabel (oFFSET_Capability_r + n))

get_Regtable_addr_from_offset   :: CmmType -> Int -> CmmExpr
get_Regtable_addr_from_offset _ offset =
#ifdef REG_Base
  CmmRegOff (CmmGlobal BaseReg) offset
#else
  regTableOffset offset
#endif

-- | Fixup global registers so that they assign to locations within the
-- RegTable if they aren't pinned for the current target.
fixStgRegisters :: RawCmmDecl -> RawCmmDecl
fixStgRegisters top@(CmmData _ _) = top

fixStgRegisters (CmmProc info lbl (ListGraph blocks)) =
  let blocks' = map fixStgRegBlock blocks
  in CmmProc info lbl $ ListGraph blocks'

fixStgRegBlock :: CmmBasicBlock -> CmmBasicBlock
fixStgRegBlock (BasicBlock id stmts) =
  let stmts' = map fixStgRegStmt stmts
  in BasicBlock id stmts'

fixStgRegStmt :: CmmStmt -> CmmStmt
fixStgRegStmt stmt
  = case stmt of
        CmmAssign (CmmGlobal reg) src ->
            let src' = fixStgRegExpr src
                baseAddr = get_GlobalReg_addr reg
            in case reg `elem` activeStgRegs of
                True  -> CmmAssign (CmmGlobal reg) src'
                False -> CmmStore baseAddr src'

        CmmAssign reg src ->
            let src' = fixStgRegExpr src
            in CmmAssign reg src'

        CmmStore addr src -> CmmStore (fixStgRegExpr addr) (fixStgRegExpr src)

        CmmCall target regs args returns ->
            let target' = case target of
                    CmmCallee e conv -> CmmCallee (fixStgRegExpr e) conv
                    CmmPrim op mStmts ->
                        CmmPrim op (fmap (map fixStgRegStmt) mStmts)
                args' = map (\(CmmHinted arg hint) ->
                                (CmmHinted (fixStgRegExpr arg) hint)) args
            in CmmCall target' regs args' returns

        CmmCondBranch test dest -> CmmCondBranch (fixStgRegExpr test) dest

        CmmSwitch expr ids -> CmmSwitch (fixStgRegExpr expr) ids

        CmmJump addr live -> CmmJump (fixStgRegExpr addr) live

        -- CmmNop, CmmComment, CmmBranch, CmmReturn
        _other -> stmt


fixStgRegExpr :: CmmExpr ->  CmmExpr
fixStgRegExpr expr
  = case expr of
        CmmLoad addr ty -> CmmLoad (fixStgRegExpr addr) ty

        CmmMachOp mop args -> CmmMachOp mop args'
            where args' = map fixStgRegExpr args

        CmmReg (CmmGlobal reg) ->
            -- Replace register leaves with appropriate StixTrees for
            -- the given target.  MagicIds which map to a reg on this
            -- arch are left unchanged.  For the rest, BaseReg is taken
            -- to mean the address of the reg table in MainCapability,
            -- and for all others we generate an indirection to its
            -- location in the register table.
            case reg `elem` activeStgRegs of
                True  -> expr
                False ->
                    let baseAddr = get_GlobalReg_addr reg
                    in case reg of
                        BaseReg -> fixStgRegExpr baseAddr
                        _other  -> fixStgRegExpr
                                    (CmmLoad baseAddr (globalRegType reg))

        CmmRegOff (CmmGlobal reg) offset ->
            -- RegOf leaves are just a shorthand form. If the reg maps
            -- to a real reg, we keep the shorthand, otherwise, we just
            -- expand it and defer to the above code.
            case reg `elem` activeStgRegs of
                True  -> expr
                False -> fixStgRegExpr (CmmMachOp (MO_Add wordWidth) [
                                    CmmReg (CmmGlobal reg),
                                    CmmLit (CmmInt (fromIntegral offset)
                                                wordWidth)])

        -- CmmLit, CmmReg (CmmLocal), CmmStackSlot
        _other -> expr

