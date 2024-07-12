{-# LANGUAGE LambdaCase, RecordWildCards, MagicHash, UnboxedTuples, PatternSynonyms, ExplicitNamespaces #-}
module GHC.Cmm.UniqueRenamer
  ( detRenameCmmGroup
  , MonadGetUnique(..)

  -- Careful! Not for general use!
  , DetUniqFM, emptyDetUFM

  , module GHC.Types.Unique.DSM
  )
  where

import Prelude
import Control.Monad.Trans.State
import GHC.Word
import GHC.Cmm
import GHC.Cmm.CLabel
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import qualified GHC.Cmm.Dataflow.Label as Det
import GHC.Cmm.Switch
-- import GHC.Cmm.Info.Build
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Utils.Outputable as Outputable
import Data.Tuple (swap)
import GHC.Types.Id
import GHC.Types.Unique.DSM
import GHC.Types.Name hiding (varName)
import GHC.Types.Var


{-
--------------------------------------------------------------------------------
-- * Deterministic Objects
--------------------------------------------------------------------------------

** Write many notes in a collective note.

Topics:
* Before generating Code, we rename all uniques of local symbols deterministically
* The code generation (like Assembly fix ups) need

-}

-- | A mapping from non-deterministic uniques to deterministic uniques, to
-- rename local symbols with the end goal of producing deterministic object files.
-- See Note [....TODO]
data DetUniqFM = DetUniqFM
  { mapping :: UniqFM Unique Unique
  , supply :: !Word64
  }

instance Outputable DetUniqFM where
  ppr DetUniqFM{mapping, supply} =
    ppr mapping $$
    text "supply:" Outputable.<> ppr supply

-- ToDo: Use ReaderT UniqDSM instead of this?
type DetRnM = State DetUniqFM

emptyDetUFM :: DetUniqFM
emptyDetUFM = DetUniqFM
  { mapping = emptyUFM
  -- NB: A lower initial value can get us label `Lsl` which is not parsed
  -- correctly in older versions of LLVM assembler (llvm-project#80571)
  -- So we use a x s.t. w64ToBase62 x > "R" > "L" > "r" > "l"
  , supply = 54
  }

renameDetUniq :: Unique -> DetRnM Unique
renameDetUniq uq = do
  m <- gets mapping
  case lookupUFM m uq of
    Nothing -> do
      new_w <- gets supply -- New deterministic unique in this `DetRnM`
      let --(_, _) = unpkUnique uq
          det_uniq = mkUnique 'Q' new_w
      modify' (\DetUniqFM{mapping, supply} ->
        -- Update supply and mapping
        DetUniqFM
          { mapping = addToUFM mapping uq det_uniq
          , supply = supply + 1
          })
      return det_uniq
    Just det_uniq ->
      return det_uniq

-- Rename local symbols deterministically (in order of appearance)
--detRename0Uniques :: UniqRenamable a => DetUniqFM -> a -> (DetUniqFM, a)
--detRenameUniques dufm x = swap $ runState (uniqRename x) dufm

detRenameCmmGroup :: DetUniqFM -> DCmmGroup -> (DetUniqFM, CmmGroup)
detRenameCmmGroup dufm group = swap (runState (mapM go group) dufm)
  where
    go :: DCmmDecl -> State DetUniqFM CmmDecl
    go (CmmProc h lbl regs g)
      = do
        g' <- uniqRename g
        regs' <- uniqRename regs
        lbl' <- uniqRename lbl
        --- rename h last!!! (TODO: Check if this is really still needed now that LabelMap is deterministic. My guess is this is not needed at all.
        h' <- goTop h
        return $ CmmProc h' lbl' regs' g'
    go (CmmData sec d)
      = CmmData <$> uniqRename sec <*> uniqRename d

    goTop :: DCmmTopInfo -> State DetUniqFM CmmTopInfo
    goTop (TopInfo (DWrap i) b) = TopInfo . Det.mapFromList <$> uniqRename i <*> pure b

-- The most important function here, which does the actual renaming.
-- Arguably, maybe we should rename this to CLabelRenamer
detRenameCLabel :: CLabel -> DetRnM CLabel
detRenameCLabel = mapInternalNonDetUniques renameDetUniq

-- | We want to rename uniques in Ids, but ONLY internal ones.
detRenameId :: Id -> DetRnM Id
detRenameId i
  | isExternalName (varName i) = return i
  | otherwise = setIdUnique i <$> renameDetUniq (getUnique i)

--------------------------------------------------------------------------------
-- Traversals
--------------------------------------------------------------------------------
-- I think I should be able to implement this using some generic traversal,
-- which would be cleaner

class UniqRenamable a where
  uniqRename :: a -> DetRnM a

instance UniqRenamable Unique where
  uniqRename = renameDetUniq

instance UniqRenamable CLabel where
  -- The most important renaming. The rest are just traversals.
  uniqRename = detRenameCLabel

instance UniqRenamable LocalReg where
  uniqRename (LocalReg uq t) = LocalReg <$> renameDetUniq uq <*> pure t
  -- uniqRename (LocalReg uq t) = pure $ LocalReg uq t
    -- ROMES:TODO: This has unique r1, we're debugging. this may still be a source of non determinism.

instance UniqRenamable Det.Label where
  uniqRename lbl = Det.mkHooplLabel . getKey <$> renameDetUniq (getUnique lbl)

instance UniqRenamable CmmTickScope where
  -- ROMES:TODO: We may have to change this to get deterministic objects with ticks.
  uniqRename = pure

instance UniqRenamable CmmDataDecl where
  uniqRename (CmmData sec d)
    = CmmData <$> uniqRename sec <*> uniqRename d
  uniqRename _ = error "impossible"

instance UniqRenamable CmmTopInfo where
  uniqRename TopInfo{info_tbls, stack_info}
    = TopInfo <$> uniqRename info_tbls <*> pure stack_info

instance UniqRenamable CmmStatics where
  uniqRename (CmmStatics clbl info ccs lits1 lits2)
    = CmmStatics <$> uniqRename clbl <*> uniqRename info <*> pure ccs <*> mapM uniqRename lits1 <*> mapM uniqRename lits2
  uniqRename (CmmStaticsRaw lbl sts)
    = CmmStaticsRaw <$> uniqRename lbl <*> mapM uniqRename sts

instance UniqRenamable CmmInfoTable where
  uniqRename CmmInfoTable{cit_lbl, cit_rep, cit_prof, cit_srt, cit_clo}
      = CmmInfoTable <$> uniqRename cit_lbl <*> pure cit_rep <*> pure cit_prof <*> uniqRename cit_srt <*>
         (case cit_clo of
            Nothing -> pure Nothing
            Just (an_id, ccs) -> Just . (,ccs) <$> detRenameId an_id)

instance UniqRenamable Section where
  uniqRename (Section ty lbl) = Section ty <$> uniqRename lbl

instance UniqRenamable RawCmmStatics where
  uniqRename (CmmStaticsRaw lbl sts)
    = CmmStaticsRaw <$> uniqRename lbl <*> mapM uniqRename sts

instance UniqRenamable CmmStatic where
  uniqRename = \case
    CmmStaticLit l -> CmmStaticLit <$> uniqRename l
    CmmUninitialised x -> pure $ CmmUninitialised x
    CmmString x -> pure $ CmmString x
    CmmFileEmbed f i -> pure $ CmmFileEmbed f i

instance UniqRenamable CmmLit where
  uniqRename = \case
    CmmInt i w -> pure $ CmmInt i w
    CmmFloat r w -> pure $ CmmFloat r w
    CmmVec lits -> CmmVec <$> mapM uniqRename lits
    CmmLabel lbl -> CmmLabel <$> uniqRename lbl
    CmmLabelOff lbl i -> CmmLabelOff <$> uniqRename lbl <*> pure i
    CmmLabelDiffOff lbl1 lbl2 i w ->
      CmmLabelDiffOff <$> uniqRename lbl1 <*> uniqRename lbl2 <*> pure i <*> pure w
    CmmBlock bid -> CmmBlock <$> uniqRename bid
    CmmHighStackMark -> pure CmmHighStackMark

-- This is fine because LabelMap is backed by a deterministic UDFM
instance UniqRenamable a {- for 'Body' and on 'RawCmmStatics' -}
  => UniqRenamable (Det.LabelMap a) where
  uniqRename lm = Det.mapFromListWith panicMapKeysNotInjective <$> traverse (\(l,x) -> (,) <$> uniqRename l <*> uniqRename x) (Det.mapToList lm)

instance UniqRenamable CmmGraph where
  uniqRename (CmmGraph e g) = CmmGraph <$> uniqRename e <*> uniqRename g

instance UniqRenamable (Graph CmmNode n m) where
  uniqRename = \case
    GNil  -> pure GNil
    GUnit block -> GUnit <$> uniqRename block
    GMany m1 b m2 -> GMany <$> uniqRename m1 <*> uniqRename b <*> uniqRename m2

instance UniqRenamable t => UniqRenamable (MaybeO n t) where
  uniqRename (JustO x) = JustO <$> uniqRename x
  uniqRename NothingO = pure NothingO

instance UniqRenamable (Block CmmNode n m) where
  uniqRename = \case
    BlockCO n bn -> BlockCO <$> uniqRename n <*> uniqRename bn
    BlockCC n1 bn n2 -> BlockCC <$> uniqRename n1 <*> uniqRename bn <*> uniqRename n2
    BlockOC bn n -> BlockOC <$> uniqRename bn <*> uniqRename n
    BNil    -> pure BNil
    BMiddle n -> BMiddle <$> uniqRename n
    BCat    b1 b2 -> BCat <$> uniqRename b1 <*> uniqRename b2
    BSnoc   bn n -> BSnoc <$> uniqRename bn <*> uniqRename n
    BCons   n bn -> BCons <$> uniqRename n <*> uniqRename bn

instance UniqRenamable (CmmNode n m) where
  uniqRename = \case
    CmmEntry l t -> CmmEntry <$> uniqRename l <*> uniqRename t
    CmmComment fs -> pure $ CmmComment fs
    CmmTick tickish -> pure $ CmmTick tickish
    CmmUnwind xs -> CmmUnwind <$> mapM uniqRename xs
    CmmAssign reg e -> CmmAssign <$> uniqRename reg <*> uniqRename e
    CmmStore e1 e2 align -> CmmStore <$> uniqRename e1 <*> uniqRename e2 <*> pure align
    CmmUnsafeForeignCall ftgt cmmformal cmmactual ->
      CmmUnsafeForeignCall <$> uniqRename ftgt <*> mapM uniqRename cmmformal <*> mapM uniqRename cmmactual
    CmmBranch l -> CmmBranch <$> uniqRename l
    CmmCondBranch pred t f likely ->
      CmmCondBranch <$> uniqRename pred <*> uniqRename t <*> uniqRename f <*> pure likely
    CmmSwitch e sts -> CmmSwitch <$> uniqRename e <*> mapSwitchTargetsA uniqRename sts
    CmmCall tgt cont regs args retargs retoff ->
      CmmCall <$> uniqRename tgt <*> uniqRename cont <*> mapM uniqRename regs
              <*> pure args <*> pure retargs <*> pure retoff
    CmmForeignCall tgt res args succ retargs retoff intrbl ->
      CmmForeignCall <$> uniqRename tgt <*> mapM uniqRename res <*> mapM uniqRename args
                     <*> uniqRename succ <*> pure retargs <*> pure retoff <*> pure intrbl

instance UniqRenamable GlobalReg where
  uniqRename = pure

instance UniqRenamable CmmExpr where
  uniqRename = \case
    CmmLit l -> CmmLit <$> uniqRename l
    CmmLoad e t a -> CmmLoad <$> uniqRename e <*> pure t <*> pure a
    CmmReg r -> CmmReg <$> uniqRename r
    CmmMachOp mop es -> CmmMachOp mop <$> mapM uniqRename es
    CmmStackSlot a i -> CmmStackSlot <$> uniqRename a <*> pure i
    CmmRegOff r i -> CmmRegOff <$> uniqRename r <*> pure i

instance UniqRenamable Area where
  uniqRename Old = pure Old
  uniqRename (Young l) = Young <$> uniqRename l

instance UniqRenamable ForeignTarget where
  uniqRename = \case
    ForeignTarget e fc -> ForeignTarget <$> uniqRename e <*> pure fc
    PrimTarget cmop -> pure $ PrimTarget cmop

instance UniqRenamable CmmReg where
  uniqRename = \case
    CmmLocal l -> CmmLocal <$> uniqRename l
    CmmGlobal x -> pure $ CmmGlobal x

instance UniqRenamable a => UniqRenamable [a] where
  uniqRename = mapM uniqRename

instance (UniqRenamable a, UniqRenamable b) => UniqRenamable (a, b) where
  uniqRename (a, b) = (,) <$> uniqRename a <*> uniqRename b

instance (UniqRenamable a) => UniqRenamable (Maybe a) where
  uniqRename Nothing = pure Nothing
  uniqRename (Just x) = Just <$> uniqRename x

-- | Utility panic used by UniqRenamable instances for Map-like datatypes
panicMapKeysNotInjective :: a -> b -> c
panicMapKeysNotInjective _ _ = error "this should be impossible because the function which maps keys should be injective"

