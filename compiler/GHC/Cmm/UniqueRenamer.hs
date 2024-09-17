{-# LANGUAGE LambdaCase, RecordWildCards, MagicHash, UnboxedTuples, PatternSynonyms, ExplicitNamespaces #-}
module GHC.Cmm.UniqueRenamer
  ( detRenameCmmGroup
  , detRenameIPEMap
  , MonadGetUnique(..)

  -- Careful! Not for general use!
  , DetUniqFM, emptyDetUFM

  , module GHC.Types.Unique.DSM
  )
  where

import GHC.Prelude
import GHC.Utils.Monad.State.Strict
import Data.Tuple (swap)
import GHC.Word
import GHC.Cmm
import GHC.Cmm.CLabel
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Switch
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Unique.DFM
import GHC.Utils.Outputable as Outputable
import GHC.Types.Id
import GHC.Types.Unique.DSM
import GHC.Types.Name hiding (varName)
import GHC.Types.Var
import GHC.Types.IPE

{-
Note [Renaming uniques deterministically]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As mentioned by Note [Object determinism], a key step in producing
deterministic objects is to rename all existing uniques deterministically.

An important observation is that GHC already produces code in a deterministic
order, both declarations (say, A_closure always comes before B_closure) and the
instructions and data within.

We can leverage this /deterministic order/ to
rename all uniques deterministically, by traversing, specifically, Cmm code
fresh off of StgToCmm and assigning a new unique from a deterministic supply
(an incrementing counter) to every non-external unique in the order they are found.

Since the order is deterministic across runs, so will the renamed uniques.

This Cmm renaming pass is guarded by -fobject-determinism because it means the
compiler must do more work. However, performance profiling has shown the impact
to be small enough that we should consider enabling -fobject-determinism by
default instead eventually.
-}

-- | A mapping from non-deterministic uniques to deterministic uniques, to
-- rename local symbols with the end goal of producing deterministic object files.
-- See Note [Renaming uniques deterministically]
data DetUniqFM = DetUniqFM
  { mapping :: !(UniqFM Unique Unique)
  , supply  :: !Word64
  }

instance Outputable DetUniqFM where
  ppr DetUniqFM{mapping, supply} =
    ppr mapping $$
    text "supply:" Outputable.<> ppr supply

type DetRnM = State DetUniqFM

emptyDetUFM :: DetUniqFM
emptyDetUFM = DetUniqFM
  { mapping = emptyUFM
  -- NB: A lower initial value can get us label `Lsl` which is not parsed
  -- correctly in older versions of LLVM assembler (llvm-project#80571)
  -- So we use an `x` s.t. w64ToBase62 x > "R" > "L" > "r" > "l"
  , supply = 54
  }

renameDetUniq :: Unique -> DetRnM Unique
renameDetUniq uq = do
  m <- gets mapping
  case lookupUFM m uq of
    Nothing -> do
      new_w <- gets supply -- New deterministic unique in this `DetRnM`
      let --(_tag, _) = unpkUnique uq
          det_uniq = mkUnique 'Q' new_w
      modify (\DetUniqFM{mapping, supply} ->
        -- Update supply and mapping
        DetUniqFM
          { mapping = addToUFM mapping uq det_uniq
          , supply = supply + 1
          })
      return det_uniq
    Just det_uniq ->
      return det_uniq

-- The most important function here, which does the actual renaming.
detRenameCLabel :: CLabel -> DetRnM CLabel
detRenameCLabel = mapInternalNonDetUniques renameDetUniq

-- | We want to rename uniques in Ids, but ONLY internal ones.
detRenameId :: Id -> DetRnM Id
detRenameId i
  | isExternalName (varName i) = return i
  | otherwise = setIdUnique i <$> renameDetUniq (getUnique i)

-- | Similar to `detRenameId`, but for `Name`.
detRenameName :: Name -> DetRnM Name
detRenameName n
  | isExternalName n = return n
  | otherwise = setNameUnique n <$> renameDetUniq (getUnique n)

detRenameCmmGroup :: DetUniqFM -> DCmmGroup -> (DetUniqFM, CmmGroup)
detRenameCmmGroup dufm group = swap (runState (mapM detRenameCmmDecl group) dufm)
  where
    detRenameCmmDecl :: DCmmDecl -> DetRnM CmmDecl
    detRenameCmmDecl (CmmProc h lbl regs g)
      = do
        h' <- detRenameCmmTop h
        lbl' <- detRenameCLabel lbl
        regs' <- mapM detRenameGlobalRegUse regs
        g' <- detRenameCmmGraph g
        return (CmmProc h' lbl' regs' g')
    detRenameCmmDecl (CmmData sec d)
      = CmmData <$> detRenameSection sec <*> detRenameCmmStatics d

    detRenameCmmTop :: DCmmTopInfo -> DetRnM CmmTopInfo
    detRenameCmmTop (TopInfo (DWrap i) b)
      = TopInfo . mapFromList <$> mapM (detRenamePair detRenameLabel detRenameCmmInfoTable) i <*> pure b

    detRenameCmmGraph :: DCmmGraph -> DetRnM CmmGraph
    detRenameCmmGraph (CmmGraph entry bs)
      = CmmGraph <$> detRenameLabel entry <*> detRenameGraph bs

    detRenameGraph = \case
      GNil  -> pure GNil
      GUnit block -> GUnit <$> detRenameBlock block
      GMany m1 b m2 -> GMany <$> detRenameMaybeBlock m1 <*> detRenameBody b <*> detRenameMaybeBlock m2

    detRenameBody (DWrap b)
      = mapFromList <$> mapM (detRenamePair detRenameLabel detRenameBlock) b

    detRenameCmmStatics :: CmmStatics -> DetRnM CmmStatics
    detRenameCmmStatics
      (CmmStatics clbl info ccs lits1 lits2)
        = CmmStatics <$> detRenameCLabel clbl <*> detRenameCmmInfoTable info <*> pure ccs <*> mapM detRenameCmmLit lits1 <*> mapM detRenameCmmLit lits2
    detRenameCmmStatics
      (CmmStaticsRaw lbl sts)
        = CmmStaticsRaw <$> detRenameCLabel lbl <*> mapM detRenameCmmStatic sts

    detRenameCmmInfoTable :: CmmInfoTable -> DetRnM CmmInfoTable
    detRenameCmmInfoTable
      CmmInfoTable{cit_lbl, cit_rep, cit_prof, cit_srt, cit_clo}
        = CmmInfoTable <$> detRenameCLabel cit_lbl <*> pure cit_rep <*> pure cit_prof <*> detRenameMaybe detRenameCLabel cit_srt <*>
           (case cit_clo of
              Nothing -> pure Nothing
              Just (an_id, ccs) -> Just . (,ccs) <$> detRenameId an_id)

    detRenameCmmStatic :: CmmStatic -> DetRnM CmmStatic
    detRenameCmmStatic = \case
      CmmStaticLit l -> CmmStaticLit <$> detRenameCmmLit l
      CmmUninitialised x -> pure $ CmmUninitialised x
      CmmString x -> pure $ CmmString x
      CmmFileEmbed f i -> pure $ CmmFileEmbed f i

    detRenameCmmLit :: CmmLit -> DetRnM CmmLit
    detRenameCmmLit = \case
      CmmInt i w -> pure $ CmmInt i w
      CmmFloat r w -> pure $ CmmFloat r w
      CmmVec lits -> CmmVec <$> mapM detRenameCmmLit lits
      CmmLabel lbl -> CmmLabel <$> detRenameCLabel lbl
      CmmLabelOff lbl i -> CmmLabelOff <$> detRenameCLabel lbl <*> pure i
      CmmLabelDiffOff lbl1 lbl2 i w ->
        CmmLabelDiffOff <$> detRenameCLabel lbl1 <*> detRenameCLabel lbl2 <*> pure i <*> pure w
      CmmBlock bid -> CmmBlock <$> detRenameLabel bid
      CmmHighStackMark -> pure CmmHighStackMark

    detRenameMaybeBlock :: MaybeO n (Block CmmNode a b) -> DetRnM (MaybeO n (Block CmmNode a b))
    detRenameMaybeBlock (JustO x) = JustO <$> detRenameBlock x
    detRenameMaybeBlock NothingO = pure NothingO

    detRenameBlock :: Block CmmNode n m -> DetRnM (Block CmmNode n m)
    detRenameBlock = \case
      BlockCO n bn -> BlockCO <$> detRenameCmmNode n <*> detRenameBlock bn
      BlockCC n1 bn n2 -> BlockCC <$> detRenameCmmNode n1 <*> detRenameBlock bn <*> detRenameCmmNode n2
      BlockOC bn n -> BlockOC <$> detRenameBlock bn <*> detRenameCmmNode n
      BNil    -> pure BNil
      BMiddle n -> BMiddle <$> detRenameCmmNode n
      BCat    b1 b2 -> BCat <$> detRenameBlock b1 <*> detRenameBlock b2
      BSnoc   bn n -> BSnoc <$> detRenameBlock bn <*> detRenameCmmNode n
      BCons   n bn -> BCons <$> detRenameCmmNode n <*> detRenameBlock bn

    detRenameCmmNode :: CmmNode n m -> DetRnM (CmmNode n m)
    detRenameCmmNode = \case
      CmmEntry l t -> CmmEntry <$> detRenameLabel l <*> detRenameCmmTick t
      CmmComment fs -> pure $ CmmComment fs
      CmmTick tickish -> pure $ CmmTick tickish
      CmmUnwind xs -> CmmUnwind <$> mapM (detRenamePair detRenameGlobalReg (detRenameMaybe detRenameCmmExpr)) xs
      CmmAssign reg e -> CmmAssign <$> detRenameCmmReg reg <*> detRenameCmmExpr e
      CmmStore e1 e2 align -> CmmStore <$> detRenameCmmExpr e1 <*> detRenameCmmExpr e2 <*> pure align
      CmmUnsafeForeignCall ftgt cmmformal cmmactual ->
        CmmUnsafeForeignCall <$> detRenameForeignTarget ftgt <*> mapM detRenameLocalReg cmmformal <*> mapM detRenameCmmExpr cmmactual
      CmmBranch l -> CmmBranch <$> detRenameLabel l
      CmmCondBranch pred t f likely ->
        CmmCondBranch <$> detRenameCmmExpr pred <*> detRenameLabel t <*> detRenameLabel f <*> pure likely
      CmmSwitch e sts -> CmmSwitch <$> detRenameCmmExpr e <*> mapSwitchTargetsA detRenameLabel sts
      CmmCall tgt cont regs args retargs retoff ->
        CmmCall <$> detRenameCmmExpr tgt <*> detRenameMaybe detRenameLabel cont <*> mapM detRenameGlobalRegUse regs
                <*> pure args <*> pure retargs <*> pure retoff
      CmmForeignCall tgt res args succ retargs retoff intrbl ->
        CmmForeignCall <$> detRenameForeignTarget tgt <*> mapM detRenameLocalReg res <*> mapM detRenameCmmExpr args
                       <*> detRenameLabel succ <*> pure retargs <*> pure retoff <*> pure intrbl

    detRenameCmmExpr :: CmmExpr -> DetRnM CmmExpr
    detRenameCmmExpr = \case
      CmmLit l -> CmmLit <$> detRenameCmmLit l
      CmmLoad e t a -> CmmLoad <$> detRenameCmmExpr e <*> pure t <*> pure a
      CmmReg r -> CmmReg <$> detRenameCmmReg r
      CmmMachOp mop es -> CmmMachOp mop <$> mapM detRenameCmmExpr es
      CmmStackSlot a i -> CmmStackSlot <$> detRenameArea a <*> pure i
      CmmRegOff r i -> CmmRegOff <$> detRenameCmmReg r <*> pure i

    detRenameForeignTarget :: ForeignTarget -> DetRnM ForeignTarget
    detRenameForeignTarget = \case
        ForeignTarget e fc -> ForeignTarget <$> detRenameCmmExpr e <*> pure fc
        PrimTarget cmop -> pure $ PrimTarget cmop

    detRenameArea :: Area -> DetRnM Area
    detRenameArea Old = pure Old
    detRenameArea (Young l) = Young <$> detRenameLabel l

    detRenameLabel :: Label -> DetRnM Label
    detRenameLabel lbl
      = mkHooplLabel . getKey <$> renameDetUniq (getUnique lbl)

    detRenameSection :: Section -> DetRnM Section
    detRenameSection (Section ty lbl)
      = Section ty <$> detRenameCLabel lbl

    detRenameCmmReg :: CmmReg -> DetRnM CmmReg
    detRenameCmmReg = \case
      CmmLocal l -> CmmLocal <$> detRenameLocalReg l
      CmmGlobal x -> pure $ CmmGlobal x

    detRenameLocalReg :: LocalReg -> DetRnM LocalReg
    detRenameLocalReg (LocalReg uq t)
      = LocalReg <$> renameDetUniq uq <*> pure t

    -- Global registers don't need to be renamed.
    detRenameGlobalReg :: GlobalReg -> DetRnM GlobalReg
    detRenameGlobalReg = pure
    detRenameGlobalRegUse :: GlobalRegUse -> DetRnM GlobalRegUse
    detRenameGlobalRegUse = pure

    -- todo: We may have to change this to get deterministic objects with ticks.
    detRenameCmmTick :: CmmTickScope -> DetRnM CmmTickScope
    detRenameCmmTick = pure

    detRenameMaybe _ Nothing = pure Nothing
    detRenameMaybe f (Just x) = Just <$> f x

    detRenamePair f g (a, b) = (,) <$> f a <*> g b

detRenameIPEMap :: DetUniqFM -> InfoTableProvMap -> (DetUniqFM, InfoTableProvMap)
detRenameIPEMap dufm InfoTableProvMap{ provDC, provClosure, provInfoTables } =
    (dufm2, InfoTableProvMap { provDC, provClosure = cm, provInfoTables })
  where
    (cm, dufm2) = runState (detRenameClosureMap provClosure) dufm

    detRenameClosureMap :: ClosureMap -> DetRnM ClosureMap
    detRenameClosureMap m =
      -- `eltsUDFM` preserves the deterministic order, but it doesn't matter
      -- since we will rename all uniques deterministically, thus the
      -- reconstructed map will necessarily be deterministic too.
      listToUDFM <$> mapM (\(n,r) -> do
        n' <- detRenameName n
        return (n', (n', r))
        ) (eltsUDFM m)
