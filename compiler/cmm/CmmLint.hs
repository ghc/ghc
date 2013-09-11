-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2011
--
-- CmmLint: checking the correctness of Cmm statements and expressions
--
-----------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}
module CmmLint (
    cmmLint, cmmLintGraph
  ) where

import Hoopl
import Cmm
import CmmUtils
import CmmLive
import PprCmm ()
import BlockId
import FastString
import Outputable
import DynFlags

import Data.Maybe
import Control.Monad (liftM, ap)
import Control.Applicative (Applicative(..))

-- Things to check:
--     - invariant on CmmBlock in CmmExpr (see comment there)
--     - check for branches to blocks that don't exist
--     - check types

-- -----------------------------------------------------------------------------
-- Exported entry points:

cmmLint :: (Outputable d, Outputable h)
        => DynFlags -> GenCmmGroup d h CmmGraph -> Maybe SDoc
cmmLint dflags tops = runCmmLint dflags (mapM_ (lintCmmDecl dflags)) tops

cmmLintGraph :: DynFlags -> CmmGraph -> Maybe SDoc
cmmLintGraph dflags g = runCmmLint dflags (lintCmmGraph dflags) g

runCmmLint :: Outputable a => DynFlags -> (a -> CmmLint b) -> a -> Maybe SDoc
runCmmLint dflags l p =
   case unCL (l p) dflags of
     Left err -> Just (vcat [ptext $ sLit ("Cmm lint error:"),
                             nest 2 err,
                             ptext $ sLit ("Program was:"),
                             nest 2 (ppr p)])
     Right _  -> Nothing

lintCmmDecl :: DynFlags -> GenCmmDecl h i CmmGraph -> CmmLint ()
lintCmmDecl dflags (CmmProc _ lbl _ g)
  = addLintInfo (text "in proc " <> ppr lbl) $ lintCmmGraph dflags g
lintCmmDecl _ (CmmData {})
  = return ()


lintCmmGraph :: DynFlags -> CmmGraph -> CmmLint ()
lintCmmGraph dflags g =
    cmmLocalLiveness dflags g `seq` mapM_ (lintCmmBlock labels) blocks
    -- cmmLiveness throws an error if there are registers
    -- live on entry to the graph (i.e. undefined
    -- variables)
  where
       blocks = toBlockList g
       labels = setFromList (map entryLabel blocks)


lintCmmBlock :: BlockSet -> CmmBlock -> CmmLint ()
lintCmmBlock labels block
  = addLintInfo (text "in basic block " <> ppr (entryLabel block)) $ do
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
  dflags <- getDynFlags
  tys <- mapM lintCmmExpr args
  if map (typeWidth . cmmExprType dflags) args == machOpArgReps dflags op
        then cmmCheckMachOp op args tys
        else cmmLintMachOpErr expr (map (cmmExprType dflags) args) (machOpArgReps dflags op)
lintCmmExpr (CmmRegOff reg offset)
  = do dflags <- getDynFlags
       let rep = typeWidth (cmmRegType dflags reg)
       lintCmmExpr (CmmMachOp (MO_Add rep)
                [CmmReg reg, CmmLit (CmmInt (fromIntegral offset) rep)])
lintCmmExpr expr =
  do dflags <- getDynFlags
     return (cmmExprType dflags expr)

-- Check for some common byte/word mismatches (eg. Sp + 1)
cmmCheckMachOp   :: MachOp -> [CmmExpr] -> [CmmType] -> CmmLint CmmType
cmmCheckMachOp op [lit@(CmmLit (CmmInt { })), reg@(CmmReg _)] tys
  = cmmCheckMachOp op [reg, lit] tys
cmmCheckMachOp op _ tys
  = do dflags <- getDynFlags
       return (machOpResultType dflags op tys)

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

lintCmmMiddle :: CmmNode O O -> CmmLint ()
lintCmmMiddle node = case node of
  CmmComment _ -> return ()

  CmmAssign reg expr -> do
            dflags <- getDynFlags
            erep <- lintCmmExpr expr
            let reg_ty = cmmRegType dflags reg
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
            dflags <- getDynFlags
            mapM_ checkTarget [t,f]
            _ <- lintCmmExpr e
            checkCond dflags e

  CmmSwitch e branches -> do
            dflags <- getDynFlags
            mapM_ checkTarget $ catMaybes branches
            erep <- lintCmmExpr e
            if (erep `cmmEqType_ignoring_ptrhood` bWord dflags)
              then return ()
              else cmmLintErr (text "switch scrutinee is not a word: " <>
                               ppr e <> text " :: " <> ppr erep)

  CmmCall { cml_target = target, cml_cont = cont } -> do
          _ <- lintCmmExpr target
          maybe (return ()) checkTarget cont

  CmmForeignCall tgt _ args succ _ _ _ -> do
          lintTarget tgt
          mapM_ lintCmmExpr args
          checkTarget succ
 where
  checkTarget id
     | setMember id labels = return ()
     | otherwise = cmmLintErr (text "Branch to nonexistent id" <+> ppr id)


lintTarget :: ForeignTarget -> CmmLint ()
lintTarget (ForeignTarget e _) = lintCmmExpr e >> return ()
lintTarget (PrimTarget {})     = return ()


checkCond :: DynFlags -> CmmExpr -> CmmLint ()
checkCond _ (CmmMachOp mop _) | isComparisonMachOp mop = return ()
checkCond dflags (CmmLit (CmmInt x t)) | x == 0 || x == 1, t == wordWidth dflags = return () -- constant values
checkCond _ expr
    = cmmLintErr (hang (text "expression is not a conditional:") 2
                         (ppr expr))

-- -----------------------------------------------------------------------------
-- CmmLint monad

-- just a basic error monad:

newtype CmmLint a = CmmLint { unCL :: DynFlags -> Either SDoc a }

instance Functor CmmLint where
      fmap = liftM

instance Applicative CmmLint where
      pure = return
      (<*>) = ap

instance Monad CmmLint where
  CmmLint m >>= k = CmmLint $ \dflags ->
                                case m dflags of
                                Left e -> Left e
                                Right a -> unCL (k a) dflags
  return a = CmmLint (\_ -> Right a)

instance HasDynFlags CmmLint where
    getDynFlags = CmmLint (\dflags -> Right dflags)

cmmLintErr :: SDoc -> CmmLint a
cmmLintErr msg = CmmLint (\_ -> Left msg)

addLintInfo :: SDoc -> CmmLint a -> CmmLint a
addLintInfo info thing = CmmLint $ \dflags ->
   case unCL thing dflags of
        Left err -> Left (hang info 2 err)
        Right a  -> Right a

cmmLintMachOpErr :: CmmExpr -> [CmmType] -> [Width] -> CmmLint a
cmmLintMachOpErr expr argsRep opExpectsRep
     = cmmLintErr (text "in MachOp application: " $$
                   nest 2 (ppr  expr) $$
                      (text "op is expecting: " <+> ppr opExpectsRep) $$
                      (text "arguments provide: " <+> ppr argsRep))

cmmLintAssignErr :: CmmNode e x -> CmmType -> CmmType -> CmmLint a
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

