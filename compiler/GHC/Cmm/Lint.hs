-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2011
--
-- CmmLint: checking the correctness of Cmm statements and expressions
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module GHC.Cmm.Lint (
    cmmLint, cmmLintGraph
  ) where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Regs (callerSaves)
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm
import GHC.Cmm.Liveness
import GHC.Cmm.Switch (switchTargetsToList)
import GHC.Cmm.CLabel (pprDebugCLabel)
import GHC.Utils.Outputable

import Control.Monad (unless)
import Control.Monad.Trans.Except (ExceptT (..), Except)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Functor.Identity (Identity (..))

-- Things to check:
--     - invariant on CmmBlock in GHC.Cmm.Expr (see comment there)
--     - check for branches to blocks that don't exist
--     - check types

-- -----------------------------------------------------------------------------
-- Exported entry points:

cmmLint :: (OutputableP Platform d, OutputableP Platform h)
        => Platform -> GenCmmGroup d h CmmGraph -> Maybe SDoc
cmmLint platform tops = runCmmLint platform (mapM_ lintCmmDecl) tops

cmmLintGraph :: Platform -> CmmGraph -> Maybe SDoc
cmmLintGraph platform g = runCmmLint platform lintCmmGraph g

runCmmLint :: OutputableP Platform a => Platform -> (a -> CmmLint b) -> a -> Maybe SDoc
runCmmLint platform l p =
   case unCL (l p) platform of
     Left err -> Just (withPprStyle defaultDumpStyle $ vcat
                            [text "Cmm lint error:",
                             nest 2 err,
                             text "Program was:",
                             nest 2 (pdoc platform p)])
     Right _  -> Nothing

lintCmmDecl :: GenCmmDecl h i CmmGraph -> CmmLint ()
lintCmmDecl (CmmProc _ lbl _ g)
  = do
    platform <- getPlatform
    addLintInfo (text "in proc " <> pprDebugCLabel platform lbl) $ lintCmmGraph g
lintCmmDecl (CmmData {})
  = return ()


lintCmmGraph :: CmmGraph -> CmmLint ()
lintCmmGraph g = do
   platform <- getPlatform
   let
      blocks = toBlockList g
      labels = setFromList (map entryLabel blocks)
   cmmLocalLiveness platform g `seq` mapM_ (lintCmmBlock labels) blocks
   -- cmmLiveness throws an error if there are registers
   -- live on entry to the graph (i.e. undefined
   -- variables)


lintCmmBlock :: LabelSet -> CmmBlock -> CmmLint ()
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
lintCmmExpr (CmmLoad expr rep _alignment) = do
  _ <- lintCmmExpr expr
  -- Disabled, if we have the inlining phase before the lint phase,
  -- we can have funny offsets due to pointer tagging. -- EZY
  -- when (widthInBytes (typeWidth rep) >= platformWordSizeInBytes platform) $
  --   cmmCheckWordAddress expr
  return rep
lintCmmExpr expr@(CmmMachOp op args) = do
  platform <- getPlatform
  tys <- mapM lintCmmExpr args
  lintShiftOp op (zip args tys)
  if map (typeWidth . cmmExprType platform) args == machOpArgReps platform op
        then cmmCheckMachOp op args tys
        else cmmLintMachOpErr expr (map (cmmExprType platform) args) (machOpArgReps platform op)
lintCmmExpr (CmmRegOff reg offset)
  = do let rep = typeWidth (cmmRegType reg)
       lintCmmExpr (CmmMachOp (MO_Add rep)
                [CmmReg reg, CmmLit (CmmInt (fromIntegral offset) rep)])
lintCmmExpr expr =
  do platform <- getPlatform
     return (cmmExprType platform expr)

-- | Check for obviously out-of-bounds shift operations
lintShiftOp :: MachOp -> [(CmmExpr, CmmType)] -> CmmLint ()
lintShiftOp op [(_, arg_ty), (CmmLit (CmmInt n _), _)]
  | isShiftOp op
  , n >= fromIntegral (widthInBits (typeWidth arg_ty))
  = cmmLintErr (text "Shift operation" <+> pprMachOp op
                <+> text "has out-of-range offset" <+> ppr n
                <> text ". This will result in undefined behavior")
lintShiftOp _ _ = return ()

isShiftOp :: MachOp -> Bool
isShiftOp (MO_Shl _)   = True
isShiftOp (MO_U_Shr _) = True
isShiftOp (MO_S_Shr _) = True
isShiftOp _            = False

-- Check for some common byte/word mismatches (eg. Sp + 1)
cmmCheckMachOp   :: MachOp -> [CmmExpr] -> [CmmType] -> CmmLint CmmType
cmmCheckMachOp op [lit@(CmmLit (CmmInt { })), reg@(CmmReg _)] tys
  = cmmCheckMachOp op [reg, lit] tys
cmmCheckMachOp op _ tys
  = do platform <- getPlatform
       return (machOpResultType platform op tys)

{-
isOffsetOp :: MachOp -> Bool
isOffsetOp (MO_Add _) = True
isOffsetOp (MO_Sub _) = True
isOffsetOp _ = False

-- This expression should be an address from which a word can be loaded:
-- check for funny-looking sub-word offsets.
_cmmCheckWordAddress :: CmmExpr -> CmmLint ()
_cmmCheckWordAddress e@(CmmMachOp op [arg, CmmLit (CmmInt i _)])
  | isOffsetOp op && notNodeReg arg && i `rem` fromIntegral (platformWordSizeInBytes platform) /= 0
  = cmmLintDubiousWordOffset e
_cmmCheckWordAddress e@(CmmMachOp op [CmmLit (CmmInt i _), arg])
  | isOffsetOp op && notNodeReg arg && i `rem` fromIntegral (platformWordSizeInBytes platform) /= 0
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
  CmmTick _    -> return ()
  CmmUnwind{}  -> return ()

  CmmAssign reg expr -> do
            erep <- lintCmmExpr expr
            let reg_ty = cmmRegType reg
            unless (erep `cmmEqType_ignoring_ptrhood` reg_ty) $
              cmmLintAssignErr (CmmAssign reg expr) erep reg_ty

  CmmStore l r _alignment -> do
            _ <- lintCmmExpr l
            _ <- lintCmmExpr r
            return ()

  CmmUnsafeForeignCall target _formals actuals -> do
            lintTarget target
            let lintArg expr = do
                  -- Arguments can't mention caller-saved
                  -- registers. See Note [Register parameter passing].
                  mayNotMentionCallerSavedRegs (text "foreign call argument") expr
                  lintCmmExpr expr

            mapM_ lintArg actuals


lintCmmLast :: LabelSet -> CmmNode O C -> CmmLint ()
lintCmmLast labels node = case node of
  CmmBranch id -> checkTarget id

  CmmCondBranch e t f _ -> do
            platform <- getPlatform
            mapM_ checkTarget [t,f]
            _ <- lintCmmExpr e
            checkCond platform e

  CmmSwitch e ids -> do
            platform <- getPlatform
            mapM_ checkTarget $ switchTargetsToList ids
            erep <- lintCmmExpr e
            unless (isWordAny erep) $
              cmmLintErr (text "switch scrutinee is not a word (of any size): " <>
                          pdoc platform e <> text " :: " <> ppr erep)

  CmmCall { cml_target = target, cml_cont = cont } -> do
          _ <- lintCmmExpr target
          maybe (return ()) checkTarget cont

  CmmForeignCall tgt _ args succ _ _ _ _track_safe-> do
          lintTarget tgt
          let lintArg expr = do
                -- Arguments can't mention caller-saved
                -- registers. See Note [Register
                -- parameter passing].
                -- N.B. This won't catch local registers
                -- which the NCG's register allocator later
                -- places in caller-saved registers.
                mayNotMentionCallerSavedRegs (text "foreign call argument") expr
                lintCmmExpr expr
          mapM_ lintArg args
          checkTarget succ
 where
  checkTarget id
     | setMember id labels = return ()
     | otherwise = cmmLintErr (text "Branch to nonexistent id" <+> ppr id)

lintTarget :: ForeignTarget -> CmmLint ()
lintTarget (ForeignTarget e _) = do
    mayNotMentionCallerSavedRegs (text "foreign target") e
    _ <- lintCmmExpr e
    return ()
lintTarget (PrimTarget {})     = return ()

-- | As noted in Note [Register parameter passing], the arguments and
-- 'ForeignTarget' of a foreign call mustn't mention
-- caller-saved registers.
mayNotMentionCallerSavedRegs :: (UserOfRegs GlobalReg a, OutputableP Platform a)
                             => SDoc -> a -> CmmLint ()
mayNotMentionCallerSavedRegs what thing = do
    platform <- getPlatform
    let badRegs = filter (callerSaves platform)
                  $ foldRegsUsed platform (flip (:)) [] thing
    unless (null badRegs)
      $ cmmLintErr (what <+> text "mentions caller-saved registers: " <> ppr badRegs $$ pdoc platform thing)

checkCond :: Platform -> CmmExpr -> CmmLint ()
checkCond _ (CmmMachOp mop _) | isComparisonMachOp mop = return ()
checkCond platform (CmmLit (CmmInt x t)) | x == 0 || x == 1, t == wordWidth platform = return () -- constant values
checkCond platform expr
    = cmmLintErr (hang (text "expression is not a conditional:") 2
                         (pdoc platform expr))

-- -----------------------------------------------------------------------------
-- CmmLint monad

-- just a basic error monad:

newtype CmmLint a = CmmLint { unCL :: Platform -> Either SDoc a }
  deriving stock (Functor)
  deriving (Applicative, Monad) via ReaderT Platform (Except SDoc)

getPlatform :: CmmLint Platform
getPlatform = CmmLint $ \platform -> Right platform

cmmLintErr :: SDoc -> CmmLint a
cmmLintErr msg = CmmLint (\_ -> Left msg)

addLintInfo :: SDoc -> CmmLint a -> CmmLint a
addLintInfo info thing = CmmLint $ \platform ->
   case unCL thing platform of
        Left err -> Left (hang info 2 err)
        Right a  -> Right a

cmmLintMachOpErr :: CmmExpr -> [CmmType] -> [Width] -> CmmLint a
cmmLintMachOpErr expr argsRep opExpectsRep
     = do
       platform <- getPlatform
       cmmLintErr (text "in MachOp application: " $$
                   nest 2 (pdoc platform expr) $$
                      (text "op is expecting: " <+> ppr opExpectsRep) $$
                      (text "arguments provide: " <+> ppr argsRep))

cmmLintAssignErr :: CmmNode e x -> CmmType -> CmmType -> CmmLint a
cmmLintAssignErr stmt e_ty r_ty
  = do
    platform <- getPlatform
    cmmLintErr (text "in assignment: " $$
                nest 2 (vcat [pdoc platform stmt,
                              text "Reg ty:" <+> ppr r_ty,
                              text "Rhs ty:" <+> ppr e_ty]))


{-
cmmLintDubiousWordOffset :: CmmExpr -> CmmLint a
cmmLintDubiousWordOffset expr
   = cmmLintErr (text "offset is not a multiple of words: " $$
                 nest 2 (ppr expr))
-}

