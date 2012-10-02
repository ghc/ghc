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
import Outputable
import OldPprCmm()
import FastString
import DynFlags

import Data.Maybe

-- -----------------------------------------------------------------------------
-- Exported entry points:

cmmLint :: (Outputable d, Outputable h)
        => DynFlags -> GenCmmGroup d h (ListGraph CmmStmt) -> Maybe SDoc
cmmLint dflags tops = runCmmLint dflags (mapM_ (lintCmmDecl dflags)) tops

cmmLintTop :: (Outputable d, Outputable h)
           => DynFlags -> GenCmmDecl d h (ListGraph CmmStmt) -> Maybe SDoc
cmmLintTop dflags top = runCmmLint dflags (lintCmmDecl dflags) top

runCmmLint :: Outputable a
           => DynFlags -> (a -> CmmLint b) -> a -> Maybe SDoc
runCmmLint _ l p =
   case unCL (l p) of
   Left err -> Just (vcat [ptext $ sLit ("Cmm lint error:"),
                           nest 2 err,
                           ptext $ sLit ("Program was:"),
                           nest 2 (ppr p)])
   Right _  -> Nothing

lintCmmDecl :: DynFlags -> (GenCmmDecl h i (ListGraph CmmStmt)) -> CmmLint ()
lintCmmDecl dflags (CmmProc _ lbl (ListGraph blocks))
  = addLintInfo (text "in proc " <> ppr lbl) $
        let labels = foldl (\s b -> setInsert (blockId b) s) setEmpty blocks
        in  mapM_ (lintCmmBlock dflags labels) blocks

lintCmmDecl _ (CmmData {})
  = return ()

lintCmmBlock :: DynFlags -> BlockSet -> GenBasicBlock CmmStmt -> CmmLint ()
lintCmmBlock dflags labels (BasicBlock id stmts)
  = addLintInfo (text "in basic block " <> ppr id) $
	mapM_ (lintCmmStmt dflags labels) stmts

-- -----------------------------------------------------------------------------
-- lintCmmExpr

-- Checks whether a CmmExpr is "type-correct", and check for obvious-looking
-- byte/word mismatches.

lintCmmExpr :: DynFlags -> CmmExpr -> CmmLint CmmType
lintCmmExpr dflags (CmmLoad expr rep) = do
  _ <- lintCmmExpr dflags expr
  -- Disabled, if we have the inlining phase before the lint phase,
  -- we can have funny offsets due to pointer tagging. -- EZY
  -- when (widthInBytes (typeWidth rep) >= wORD_SIZE) $
  --   cmmCheckWordAddress expr
  return rep
lintCmmExpr dflags expr@(CmmMachOp op args) = do
  tys <- mapM (lintCmmExpr dflags) args
  if map (typeWidth . cmmExprType dflags) args == machOpArgReps dflags op
  	then cmmCheckMachOp dflags op args tys
	else cmmLintMachOpErr expr (map (cmmExprType dflags) args) (machOpArgReps dflags op)
lintCmmExpr dflags (CmmRegOff reg offset)
  = lintCmmExpr dflags (CmmMachOp (MO_Add rep)
		[CmmReg reg, CmmLit (CmmInt (fromIntegral offset) rep)])
  where rep = typeWidth (cmmRegType dflags reg)
lintCmmExpr dflags expr =
  return (cmmExprType dflags expr)

-- Check for some common byte/word mismatches (eg. Sp + 1)
cmmCheckMachOp   :: DynFlags -> MachOp -> [CmmExpr] -> [CmmType] -> CmmLint CmmType
cmmCheckMachOp dflags op [lit@(CmmLit (CmmInt { })), reg@(CmmReg _)] tys
  = cmmCheckMachOp dflags op [reg, lit] tys
cmmCheckMachOp dflags op _ tys
  = return (machOpResultType dflags op tys)

{-
isOffsetOp :: MachOp -> Bool
isOffsetOp (MO_Add _) = True
isOffsetOp (MO_Sub _) = True
isOffsetOp _ = False

-- This expression should be an address from which a word can be loaded:
-- check for funny-looking sub-word offsets.
_cmmCheckWordAddress :: CmmExpr -> CmmLint ()
_cmmCheckWordAddress e@(CmmMachOp op [arg, CmmLit (CmmInt i _)])
  | isOffsetOp op && notNodeReg arg && i `rem` fromIntegral (wORD_SIZE dflags) /= 0
  = cmmLintDubiousWordOffset e
_cmmCheckWordAddress e@(CmmMachOp op [CmmLit (CmmInt i _), arg])
  | isOffsetOp op && notNodeReg arg && i `rem` fromIntegral (wORD_SIZE dflags) /= 0
  = cmmLintDubiousWordOffset e
_cmmCheckWordAddress _
  = return ()

-- No warnings for unaligned arithmetic with the node register,
-- which is used to extract fields from tagged constructor closures.
notNodeReg :: CmmExpr -> Bool
notNodeReg (CmmReg reg) | reg == nodeReg = False
notNodeReg _                             = True
-}

lintCmmStmt :: DynFlags -> BlockSet -> CmmStmt -> CmmLint ()
lintCmmStmt dflags labels = lint
    where lint (CmmNop) = return ()
          lint (CmmComment {}) = return ()
          lint stmt@(CmmAssign reg expr) = do
            erep <- lintCmmExpr dflags expr
            let reg_ty = cmmRegType dflags reg
            if (erep `cmmEqType_ignoring_ptrhood` reg_ty)
                then return ()
                else cmmLintAssignErr stmt erep reg_ty
          lint (CmmStore l r) = do
            _ <- lintCmmExpr dflags l
            _ <- lintCmmExpr dflags r
            return ()
          lint (CmmCall target _res args _) =
              do lintTarget dflags labels target
                 mapM_ (lintCmmExpr dflags . hintlessCmm) args
          lint (CmmCondBranch e id) = checkTarget id >> lintCmmExpr dflags e >> checkCond dflags e
          lint (CmmSwitch e branches) = do
            mapM_ checkTarget $ catMaybes branches
            erep <- lintCmmExpr dflags e
            if (erep `cmmEqType_ignoring_ptrhood` bWord dflags)
              then return ()
              else cmmLintErr (text "switch scrutinee is not a word: " <> ppr e <>
                               text " :: " <> ppr erep)
          lint (CmmJump e _) = lintCmmExpr dflags e >> return ()
          lint (CmmReturn) = return ()
          lint (CmmBranch id)    = checkTarget id
          checkTarget id = if setMember id labels then return ()
                           else cmmLintErr (text "Branch to nonexistent id" <+> ppr id)

lintTarget :: DynFlags -> BlockSet -> CmmCallTarget -> CmmLint ()
lintTarget dflags _      (CmmCallee e _) = do _ <- lintCmmExpr dflags e
                                              return ()
lintTarget _      _      (CmmPrim _ Nothing) = return ()
lintTarget dflags labels (CmmPrim _ (Just stmts))
    = mapM_ (lintCmmStmt dflags labels) stmts


checkCond :: DynFlags -> CmmExpr -> CmmLint ()
checkCond _      (CmmMachOp mop _) | isComparisonMachOp mop = return ()
checkCond dflags (CmmLit (CmmInt x t)) | x == 0 || x == 1, t == wordWidth dflags = return () -- constant values
checkCond _      expr
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
			 
					

{-
cmmLintDubiousWordOffset :: CmmExpr -> CmmLint a
cmmLintDubiousWordOffset expr
   = cmmLintErr (text "offset is not a multiple of words: " $$
			nest 2 (ppr expr))
-}

