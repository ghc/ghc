-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2004-2006
--
-- CmmLint: checking the correctness of Cmm statements and expressions
--
-----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module OldCmmLint (
  cmmLint, cmmLintTop
  ) where

import BlockId
import OldCmm
import CLabel
import Outputable
import OldPprCmm()
import Constants
import FastString
import Platform

import Data.Maybe

-- -----------------------------------------------------------------------------
-- Exported entry points:

cmmLint :: (Outputable d, Outputable h)
        => Platform -> GenCmmGroup d h (ListGraph CmmStmt) -> Maybe SDoc
cmmLint platform tops = runCmmLint platform (mapM_ (lintCmmDecl platform)) tops

cmmLintTop :: (Outputable d, Outputable h)
           => Platform -> GenCmmDecl d h (ListGraph CmmStmt) -> Maybe SDoc
cmmLintTop platform top = runCmmLint platform (lintCmmDecl platform) top

runCmmLint :: Outputable a
           => Platform -> (a -> CmmLint b) -> a -> Maybe SDoc
runCmmLint _ l p =
   case unCL (l p) of
   Left err -> Just (vcat [ptext $ sLit ("Cmm lint error:"),
                           nest 2 err,
                           ptext $ sLit ("Program was:"),
                           nest 2 (ppr p)])
   Right _  -> Nothing

lintCmmDecl :: Platform -> (GenCmmDecl h i (ListGraph CmmStmt)) -> CmmLint ()
lintCmmDecl platform (CmmProc _ lbl (ListGraph blocks))
  = addLintInfo (text "in proc " <> pprCLabel platform lbl) $
        let labels = foldl (\s b -> setInsert (blockId b) s) setEmpty blocks
	in  mapM_ (lintCmmBlock platform labels) blocks

lintCmmDecl _ (CmmData {})
  = return ()

lintCmmBlock :: Platform -> BlockSet -> GenBasicBlock CmmStmt -> CmmLint ()
lintCmmBlock platform labels (BasicBlock id stmts)
  = addLintInfo (text "in basic block " <> ppr id) $
	mapM_ (lintCmmStmt platform labels) stmts

-- -----------------------------------------------------------------------------
-- lintCmmExpr

-- Checks whether a CmmExpr is "type-correct", and check for obvious-looking
-- byte/word mismatches.

lintCmmExpr :: Platform -> CmmExpr -> CmmLint CmmType
lintCmmExpr platform (CmmLoad expr rep) = do
  _ <- lintCmmExpr platform expr
  -- Disabled, if we have the inlining phase before the lint phase,
  -- we can have funny offsets due to pointer tagging. -- EZY
  -- when (widthInBytes (typeWidth rep) >= wORD_SIZE) $
  --   cmmCheckWordAddress expr
  return rep
lintCmmExpr platform expr@(CmmMachOp op args) = do
  tys <- mapM (lintCmmExpr platform) args
  if map (typeWidth . cmmExprType) args == machOpArgReps op
  	then cmmCheckMachOp op args tys
	else cmmLintMachOpErr expr (map cmmExprType args) (machOpArgReps op)
lintCmmExpr platform (CmmRegOff reg offset)
  = lintCmmExpr platform (CmmMachOp (MO_Add rep)
		[CmmReg reg, CmmLit (CmmInt (fromIntegral offset) rep)])
  where rep = typeWidth (cmmRegType reg)
lintCmmExpr _ expr =
  return (cmmExprType expr)

-- Check for some common byte/word mismatches (eg. Sp + 1)
cmmCheckMachOp   :: MachOp -> [CmmExpr] -> [CmmType] -> CmmLint CmmType
cmmCheckMachOp op [lit@(CmmLit (CmmInt { })), reg@(CmmReg _)] tys
  = cmmCheckMachOp op [reg, lit] tys
cmmCheckMachOp op _ tys
  = return (machOpResultType op tys)

isOffsetOp :: MachOp -> Bool
isOffsetOp (MO_Add _) = True
isOffsetOp (MO_Sub _) = True
isOffsetOp _ = False

-- This expression should be an address from which a word can be loaded:
-- check for funny-looking sub-word offsets.
_cmmCheckWordAddress :: CmmExpr -> CmmLint ()
_cmmCheckWordAddress e@(CmmMachOp op [arg, CmmLit (CmmInt i _)])
  | isOffsetOp op && notNodeReg arg && i `rem` fromIntegral wORD_SIZE /= 0
  = cmmLintDubiousWordOffset e
_cmmCheckWordAddress e@(CmmMachOp op [CmmLit (CmmInt i _), arg])
  | isOffsetOp op && notNodeReg arg && i `rem` fromIntegral wORD_SIZE /= 0
  = cmmLintDubiousWordOffset e
_cmmCheckWordAddress _
  = return ()

-- No warnings for unaligned arithmetic with the node register,
-- which is used to extract fields from tagged constructor closures.
notNodeReg :: CmmExpr -> Bool
notNodeReg (CmmReg reg) | reg == nodeReg = False
notNodeReg _                             = True

lintCmmStmt :: Platform -> BlockSet -> CmmStmt -> CmmLint ()
lintCmmStmt platform labels = lint
    where lint (CmmNop) = return ()
          lint (CmmComment {}) = return ()
          lint stmt@(CmmAssign reg expr) = do
            erep <- lintCmmExpr platform expr
	    let reg_ty = cmmRegType reg
            if (erep `cmmEqType_ignoring_ptrhood` reg_ty)
                then return ()
                else cmmLintAssignErr stmt erep reg_ty
          lint (CmmStore l r) = do
            _ <- lintCmmExpr platform l
            _ <- lintCmmExpr platform r
            return ()
          lint (CmmCall target _res args _) =
              do lintTarget platform labels target
                 mapM_ (lintCmmExpr platform . hintlessCmm) args
          lint (CmmCondBranch e id) = checkTarget id >> lintCmmExpr platform e >> checkCond e
          lint (CmmSwitch e branches) = do
            mapM_ checkTarget $ catMaybes branches
            erep <- lintCmmExpr platform e
            if (erep `cmmEqType_ignoring_ptrhood` bWord)
              then return ()
              else cmmLintErr (text "switch scrutinee is not a word: " <> ppr e <>
                               text " :: " <> ppr erep)
          lint (CmmJump e _) = lintCmmExpr platform e >> return ()
          lint (CmmReturn) = return ()
          lint (CmmBranch id)    = checkTarget id
          checkTarget id = if setMember id labels then return ()
                           else cmmLintErr (text "Branch to nonexistent id" <+> ppr id)

lintTarget :: Platform -> BlockSet -> CmmCallTarget -> CmmLint ()
lintTarget platform _      (CmmCallee e _) = do _ <- lintCmmExpr platform e
                                                return ()
lintTarget _        _      (CmmPrim _ Nothing) = return ()
lintTarget platform labels (CmmPrim _ (Just stmts))
    = mapM_ (lintCmmStmt platform labels) stmts


checkCond :: CmmExpr -> CmmLint ()
checkCond (CmmMachOp mop _) | isComparisonMachOp mop = return ()
checkCond (CmmLit (CmmInt x t)) | x == 0 || x == 1, t == wordWidth = return () -- constant values
checkCond expr
    = cmmLintErr (hang (text "expression is not a conditional:") 2
                       (ppr expr))

-- -----------------------------------------------------------------------------
-- CmmLint monad

-- just a basic error monad:

newtype CmmLint a = CmmLint { unCL :: Either SDoc a }

instance Monad CmmLint where
  CmmLint m >>= k = CmmLint $ case m of 
				Left e -> Left e
				Right a -> unCL (k a)
  return a = CmmLint (Right a)

cmmLintErr :: SDoc -> CmmLint a
cmmLintErr msg = CmmLint (Left msg)

addLintInfo :: SDoc -> CmmLint a -> CmmLint a
addLintInfo info thing = CmmLint $ 
   case unCL thing of
	Left err -> Left (hang info 2 err)
	Right a  -> Right a

cmmLintMachOpErr :: CmmExpr -> [CmmType] -> [Width] -> CmmLint a
cmmLintMachOpErr expr argsRep opExpectsRep
     = cmmLintErr (text "in MachOp application: " $$ 
					nest 2 (ppr expr) $$
				        (text "op is expecting: " <+> ppr opExpectsRep) $$
					(text "arguments provide: " <+> ppr argsRep))

cmmLintAssignErr :: CmmStmt -> CmmType -> CmmType -> CmmLint a
cmmLintAssignErr stmt e_ty r_ty
  = cmmLintErr (text "in assignment: " $$ 
		nest 2 (vcat [ppr stmt, 
			      text "Reg ty:" <+> ppr r_ty,
			      text "Rhs ty:" <+> ppr e_ty]))
			 
					

cmmLintDubiousWordOffset :: CmmExpr -> CmmLint a
cmmLintDubiousWordOffset expr
   = cmmLintErr (text "offset is not a multiple of words: " $$
			nest 2 (ppr expr))
