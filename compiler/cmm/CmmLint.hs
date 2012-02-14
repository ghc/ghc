-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2011
--
-- CmmLint: checking the correctness of Cmm statements and expressions
--
-----------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}
module CmmLint (
    cmmLint, cmmLintDecl, cmmLintGraph
  ) where

import Hoopl
import Cmm
import CmmUtils
import PprCmm ()
import BlockId
import FastString
import CLabel
import Platform
import Outputable
import Constants

import Data.Maybe

-- Things to check:
--     - invariant on CmmBlock in CmmExpr (see comment there)
--     - check for branches to blocks that don't exist
--     - check types

-- -----------------------------------------------------------------------------
-- Exported entry points:

cmmLint :: (PlatformOutputable d, PlatformOutputable h)
        => Platform -> GenCmmGroup d h CmmGraph -> Maybe SDoc
cmmLint platform tops = runCmmLint platform (mapM_ lintCmmDecl) tops

cmmLintDecl :: (PlatformOutputable d, PlatformOutputable h)
           => Platform -> GenCmmDecl d h CmmGraph -> Maybe SDoc
cmmLintDecl platform top = runCmmLint platform lintCmmDecl top

cmmLintGraph :: Platform -> CmmGraph -> Maybe SDoc
cmmLintGraph platform g = runCmmLint platform lintCmmGraph g

runCmmLint :: PlatformOutputable a
           => Platform -> (a -> CmmLint b) -> a -> Maybe SDoc
runCmmLint platform l p =
   case unCL (l p) platform of
   Left err -> Just (vcat [ptext $ sLit ("Cmm lint error:"),
                           nest 2 err,
                           ptext $ sLit ("Program was:"),
                           nest 2 (pprPlatform platform p)])
   Right _  -> Nothing

lintCmmDecl :: GenCmmDecl h i CmmGraph -> CmmLint ()
lintCmmDecl (CmmProc _ lbl g)
  = addLintInfo (\platform -> text "in proc " <> pprCLabel platform lbl) $
        lintCmmGraph g
lintCmmDecl (CmmData {})
  = return ()


lintCmmGraph :: CmmGraph -> CmmLint ()
lintCmmGraph g = mapM_ (lintCmmBlock labels) blocks
  where
       blocks = toBlockList g
       labels = setFromList (map entryLabel blocks)


lintCmmBlock :: BlockSet -> CmmBlock -> CmmLint ()
lintCmmBlock labels block
  = addLintInfo (\_ -> text "in basic block " <> ppr (entryLabel block)) $ do
        let (_, middle, last) = blockSplit block
        mapM_ lintCmmMiddle (blockToList middle)
        lintCmmLast labels last

-- -----------------------------------------------------------------------------
-- lintCmmExpr

-- Checks whether a CmmExpr is "type-correct", and check for obvious-looking
-- byte/word mismatches.

lintCmmExpr :: CmmExpr -> CmmLint CmmType
lintCmmExpr (CmmLoad expr rep) = do
  _ <- lintCmmExpr expr
  -- Disabled, if we have the inlining phase before the lint phase,
  -- we can have funny offsets due to pointer tagging. -- EZY
  -- when (widthInBytes (typeWidth rep) >= wORD_SIZE) $
  --   cmmCheckWordAddress expr
  return rep
lintCmmExpr expr@(CmmMachOp op args) = do
  tys <- mapM lintCmmExpr args
  if map (typeWidth . cmmExprType) args == machOpArgReps op
        then cmmCheckMachOp op args tys
        else cmmLintMachOpErr expr (map cmmExprType args) (machOpArgReps op)
lintCmmExpr (CmmRegOff reg offset)
  = lintCmmExpr (CmmMachOp (MO_Add rep)
                [CmmReg reg, CmmLit (CmmInt (fromIntegral offset) rep)])
  where rep = typeWidth (cmmRegType reg)
lintCmmExpr expr =
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

lintCmmMiddle :: CmmNode O O -> CmmLint ()
lintCmmMiddle node = case node of
  CmmComment _ -> return ()

  CmmAssign reg expr -> do
            erep <- lintCmmExpr expr
            let reg_ty = cmmRegType reg
            if (erep `cmmEqType_ignoring_ptrhood` reg_ty)
                then return ()
                else cmmLintAssignErr (CmmAssign reg expr) erep reg_ty

  CmmStore l r -> do
            _ <- lintCmmExpr l
            _ <- lintCmmExpr r
            return ()

  CmmUnsafeForeignCall target _formals actuals -> do
            lintTarget target
            mapM_ lintCmmExpr actuals


lintCmmLast :: BlockSet -> CmmNode O C -> CmmLint ()
lintCmmLast labels node = case node of
  CmmBranch id -> checkTarget id

  CmmCondBranch e t f -> do
            mapM_ checkTarget [t,f]
            _ <- lintCmmExpr e
            checkCond e

  CmmSwitch e branches -> do
            mapM_ checkTarget $ catMaybes branches
            erep <- lintCmmExpr e
            if (erep `cmmEqType_ignoring_ptrhood` bWord)
              then return ()
              else cmmLintErr (\platform ->
                               text "switch scrutinee is not a word: " <>
                               pprPlatform platform e <>
                               text " :: " <> ppr erep)

  CmmCall { cml_target = target, cml_cont = cont } -> do
          _ <- lintCmmExpr target
          maybe (return ()) checkTarget cont

  CmmForeignCall tgt _ args succ _ _ -> do
          lintTarget tgt
          mapM_ lintCmmExpr args
          checkTarget succ
 where
  checkTarget id
     | setMember id labels = return ()
     | otherwise = cmmLintErr (\_ -> text "Branch to nonexistent id" <+> ppr id)


lintTarget :: ForeignTarget -> CmmLint ()
lintTarget (ForeignTarget e _) = lintCmmExpr e >> return ()
lintTarget (PrimTarget {})     = return ()


checkCond :: CmmExpr -> CmmLint ()
checkCond (CmmMachOp mop _) | isComparisonMachOp mop = return ()
checkCond (CmmLit (CmmInt x t)) | x == 0 || x == 1, t == wordWidth = return () -- constant values
checkCond expr
    = cmmLintErr (\platform -> hang (text "expression is not a conditional:") 2
                         (pprPlatform platform expr))

-- -----------------------------------------------------------------------------
-- CmmLint monad

-- just a basic error monad:

newtype CmmLint a = CmmLint { unCL :: Platform -> Either SDoc a }

instance Monad CmmLint where
  CmmLint m >>= k = CmmLint $ \p -> case m p of
                                      Left e -> Left e
                                      Right a -> unCL (k a) p
  return a = CmmLint (\_ -> Right a)

cmmLintErr :: (Platform -> SDoc) -> CmmLint a
cmmLintErr msg = CmmLint (\p -> Left (msg p))

addLintInfo :: (Platform -> SDoc) -> CmmLint a -> CmmLint a
addLintInfo info thing = CmmLint $ \p ->
   case unCL thing p of
        Left err -> Left (hang (info p) 2 err)
        Right a  -> Right a

cmmLintMachOpErr :: CmmExpr -> [CmmType] -> [Width] -> CmmLint a
cmmLintMachOpErr expr argsRep opExpectsRep
     = cmmLintErr (\platform -> text "in MachOp application: " $$
                                        nest 2 (pprPlatform platform expr) $$
                                        (text "op is expecting: " <+> ppr opExpectsRep) $$
                                        (text "arguments provide: " <+> ppr argsRep))

cmmLintAssignErr :: CmmNode e x -> CmmType -> CmmType -> CmmLint a
cmmLintAssignErr stmt e_ty r_ty
  = cmmLintErr (\platform -> text "in assignment: " $$
               nest 2 (vcat [pprPlatform platform stmt,
                                text "Reg ty:" <+> ppr r_ty,
                                text "Rhs ty:" <+> ppr e_ty]))


cmmLintDubiousWordOffset :: CmmExpr -> CmmLint a
cmmLintDubiousWordOffset expr
   = cmmLintErr (\platform -> text "offset is not a multiple of words: " $$
                              nest 2 (pprPlatform platform expr))
