
-- | Clean out unneeded spill\/reload instructions.
--
--   Handling of join points
--   ~~~~~~~~~~~~~~~~~~~~~~~
--
--   B1:                          B2:
--    ...                          ...
--       RELOAD SLOT(0), %r1          RELOAD SLOT(0), %r1
--       ... A ...                    ... B ...
--       jump B3                      jump B3
--
--                B3: ... C ...
--                    RELOAD SLOT(0), %r1
--                    ...
--
--   The Plan
--   ~~~~~~~~
--   As long as %r1 hasn't been written to in A, B or C then we don't need
--   the reload in B3.
--
--   What we really care about here is that on the entry to B3, %r1 will
--   always have the same value that is in SLOT(0) (ie, %r1 is _valid_)
--
--   This also works if the reloads in B1\/B2 were spills instead, because
--   spilling %r1 to a slot makes that slot have the same value as %r1.
--
module RegAlloc.Graph.SpillClean (
        cleanSpills
) where
import RegAlloc.Liveness
import Instruction
import Reg

import BlockId
import Cmm
import UniqSet
import UniqFM
import Unique
import State
import Outputable
import Platform

import Data.List
import Data.Maybe
import Data.Map                 (Map)
import Data.Set                 (Set)
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-- | The identification number of a spill slot.
--   A value is stored in a spill slot when we don't have a free 
--   register to hold it.
type Slot = Int


-- | Clean out unneeded spill\/reloads from this top level thing.
cleanSpills
        :: Instruction instr
        => Platform 
        -> LiveCmmDecl statics instr 
        -> LiveCmmDecl statics instr

cleanSpills platform cmm
        = evalState (cleanSpin platform 0 cmm) initCleanS


-- | Do one pass of cleaning.
cleanSpin
        :: Instruction instr
        => Platform
        -> Int                              -- ^ Iteration number for the cleaner.
        -> LiveCmmDecl statics instr        -- ^ Liveness annotated code to clean.
        -> CleanM (LiveCmmDecl statics instr)

cleanSpin platform spinCount code
 = do
        -- Initialise count of cleaned spill and reload instructions.
        modify $ \s -> s
                { sCleanedSpillsAcc     = 0
                , sCleanedReloadsAcc    = 0
                , sReloadedBy           = emptyUFM }

        code_forward    <- mapBlockTopM (cleanBlockForward platform) code
        code_backward   <- cleanTopBackward code_forward
        
        -- During the cleaning of each block we collected information about
        -- what regs were valid across each jump. Based on this, work out
        -- whether it will be safe to erase reloads after join points for
        -- the next pass.
        collateJoinPoints

        -- Remember how many spill and reload instructions we cleaned in this pass.
        spills          <- gets sCleanedSpillsAcc
        reloads         <- gets sCleanedReloadsAcc
        modify $ \s -> s
                { sCleanedCount = (spills, reloads) : sCleanedCount s }

        -- If nothing was cleaned in this pass or the last one
        --      then we're done and it's time to bail out.
        cleanedCount    <- gets sCleanedCount
        if take 2 cleanedCount == [(0, 0), (0, 0)]
           then return code

        -- otherwise go around again
           else cleanSpin platform (spinCount + 1) code_backward


-------------------------------------------------------------------------------
-- | Clean out unneeded reload instructions,
--   while walking forward over the code.
cleanBlockForward
        :: Instruction instr
        => Platform
        -> LiveBasicBlock instr
        -> CleanM (LiveBasicBlock instr)

cleanBlockForward platform (BasicBlock blockId instrs)
 = do
        -- See if we have a valid association for the entry to this block.
        jumpValid       <- gets sJumpValid
        let assoc       = case lookupUFM jumpValid blockId of
                                Just assoc      -> assoc
                                Nothing         -> emptyAssoc

        instrs_reload   <- cleanForward platform blockId assoc [] instrs
        return  $ BasicBlock blockId instrs_reload



-- | Clean out unneeded reload instructions.
--
--   Walking forwards across the code
--     On a reload, if we know a reg already has the same value as a slot
--     then we don't need to do the reload.
--
cleanForward
        :: Instruction instr
        => Platform
        -> BlockId                  -- ^ the block that we're currently in
        -> Assoc Store              -- ^ two store locations are associated if
                                    --     they have the same value
        -> [LiveInstr instr]        -- ^ acc
        -> [LiveInstr instr]        -- ^ instrs to clean (in backwards order)
        -> CleanM [LiveInstr instr] -- ^ cleaned instrs  (in forward   order)

cleanForward _ _ _ acc []
        = return acc

-- Rewrite live range joins via spill slots to just a spill and a reg-reg move
-- hopefully the spill will be also be cleaned in the next pass
cleanForward platform blockId assoc acc (li1 : li2 : instrs)

        | LiveInstr (SPILL  reg1  slot1) _      <- li1
        , LiveInstr (RELOAD slot2 reg2)  _      <- li2
        , slot1 == slot2
        = do
                modify $ \s -> s { sCleanedReloadsAcc = sCleanedReloadsAcc s + 1 }
                cleanForward platform blockId assoc acc
                 $ li1 : LiveInstr (mkRegRegMoveInstr platform reg1 reg2) Nothing 
                       : instrs

cleanForward platform blockId assoc acc (li@(LiveInstr i1 _) : instrs)
        | Just (r1, r2) <- takeRegRegMoveInstr i1
        = if r1 == r2
                -- Erase any left over nop reg reg moves while we're here
                -- this will also catch any nop moves that the previous case
                -- happens to add.
                then cleanForward platform blockId assoc acc instrs

                -- If r1 has the same value as some slots and we copy r1 to r2,
                --      then r2 is now associated with those slots instead
                else do let assoc'      = addAssoc (SReg r1) (SReg r2)
                                        $ delAssoc (SReg r2)
                                        $ assoc

                        cleanForward platform blockId assoc' (li : acc) instrs


cleanForward platform blockId assoc acc (li : instrs)

        -- Update association due to the spill.
        | LiveInstr (SPILL reg slot) _  <- li
        = let   assoc'  = addAssoc (SReg reg)  (SSlot slot)
                        $ delAssoc (SSlot slot)
                        $ assoc
          in    cleanForward platform blockId assoc' (li : acc) instrs

        -- Clean a reload instr.
        | LiveInstr (RELOAD{}) _        <- li
        = do    (assoc', mli)   <- cleanReload platform blockId assoc li
                case mli of
                 Nothing        -> cleanForward platform blockId assoc' acc
                                                instrs

                 Just li'       -> cleanForward platform blockId assoc' (li' : acc)
                                                instrs

        -- Remember the association over a jump.
        | LiveInstr instr _     <- li
        , targets               <- jumpDestsOfInstr instr
        , not $ null targets
        = do    mapM_ (accJumpValid assoc) targets
                cleanForward platform blockId assoc (li : acc) instrs

        -- Writing to a reg changes its value.
        | LiveInstr instr _     <- li
        , RU _ written          <- regUsageOfInstr platform instr
        = let assoc'    = foldr delAssoc assoc (map SReg $ nub written)
          in  cleanForward platform blockId assoc' (li : acc) instrs



-- | Try and rewrite a reload instruction to something more pleasing
cleanReload
        :: Instruction instr
        => Platform
        -> BlockId
        -> Assoc Store
        -> LiveInstr instr
        -> CleanM (Assoc Store, Maybe (LiveInstr instr))

cleanReload platform blockId assoc li@(LiveInstr (RELOAD slot reg) _)

        -- If the reg we're reloading already has the same value as the slot
        --      then we can erase the instruction outright.
        | elemAssoc (SSlot slot) (SReg reg) assoc
        = do    modify  $ \s -> s { sCleanedReloadsAcc = sCleanedReloadsAcc s + 1 }
                return  (assoc, Nothing)

        -- If we can find another reg with the same value as this slot then
        --      do a move instead of a reload.
        | Just reg2     <- findRegOfSlot assoc slot
        = do    modify $ \s -> s { sCleanedReloadsAcc = sCleanedReloadsAcc s + 1 }

                let assoc'      = addAssoc (SReg reg) (SReg reg2)
                                $ delAssoc (SReg reg)
                                $ assoc

                return  ( assoc'
                        , Just $ LiveInstr (mkRegRegMoveInstr platform reg2 reg) Nothing)

        -- Gotta keep this instr.
        | otherwise
        = do    -- Update the association.
                let assoc'
                        = addAssoc (SReg reg)  (SSlot slot)     
                                -- doing the reload makes reg and slot the same value
                        $ delAssoc (SReg reg)
                                -- reg value changes on reload
                        $ assoc

                -- Remember that this block reloads from this slot.
                accBlockReloadsSlot blockId slot

                return  (assoc', Just li)

cleanReload _ _ _ _
        = panic "RegSpillClean.cleanReload: unhandled instr"


-------------------------------------------------------------------------------
-- | Clean out unneeded spill instructions,
--   while walking backwards over the code.
--
--      If there were no reloads from a slot between a spill and the last one
--      then the slot was never read and we don't need the spill.
--
--      SPILL   r0 -> s1
--      RELOAD  s1 -> r2
--      SPILL   r3 -> s1        <--- don't need this spill
--      SPILL   r4 -> s1
--      RELOAD  s1 -> r5
--
--      Maintain a set of
--              "slots which were spilled to but not reloaded from yet"
--
--      Walking backwards across the code:
--       a) On a reload from a slot, remove it from the set.
--
--       a) On a spill from a slot
--              If the slot is in set then we can erase the spill,
--               because it won't be reloaded from until after the next spill.
--
--              otherwise
--               keep the spill and add the slot to the set
--
-- TODO: This is mostly inter-block
--       we should really be updating the noReloads set as we cross jumps also.
--
-- TODO: generate noReloads from liveSlotsOnEntry
-- 
cleanTopBackward
        :: Instruction instr
        => LiveCmmDecl statics instr
        -> CleanM (LiveCmmDecl statics instr)

cleanTopBackward cmm
 = case cmm of
        CmmData{}
         -> return cmm
        
        CmmProc info label live sccs
         | LiveInfo _ _ _ liveSlotsOnEntry <- info
         -> do  sccs'   <- mapM (mapSCCM (cleanBlockBackward liveSlotsOnEntry)) sccs
                return  $ CmmProc info label live sccs' 


cleanBlockBackward 
        :: Instruction instr
        => Map BlockId (Set Int)
        -> LiveBasicBlock instr 
        -> CleanM (LiveBasicBlock instr)

cleanBlockBackward liveSlotsOnEntry (BasicBlock blockId instrs)
 = do   instrs_spill    <- cleanBackward liveSlotsOnEntry  emptyUniqSet  [] instrs
        return  $ BasicBlock blockId instrs_spill



cleanBackward
        :: Instruction instr
        => Map BlockId (Set Int)    -- ^ Slots live on entry to each block
        -> UniqSet Int              -- ^ Slots that have been spilled, but not reloaded from
        -> [LiveInstr instr]        -- ^ acc
        -> [LiveInstr instr]        -- ^ Instrs to clean (in forwards order)
        -> CleanM [LiveInstr instr] -- ^ Cleaned instrs  (in backwards order)

cleanBackward liveSlotsOnEntry noReloads acc lis
 = do   reloadedBy      <- gets sReloadedBy
        cleanBackward' liveSlotsOnEntry reloadedBy noReloads acc lis


cleanBackward' 
        :: Instruction instr
        => Map BlockId (Set Int)
        -> UniqFM [BlockId]
        -> UniqSet Int
        -> [LiveInstr instr]
        -> [LiveInstr instr]
        -> State CleanS [LiveInstr instr]

cleanBackward' _ _ _      acc []
        = return  acc

cleanBackward' liveSlotsOnEntry reloadedBy noReloads acc (li : instrs)

        -- If nothing ever reloads from this slot then we don't need the spill.
        | LiveInstr (SPILL _ slot) _    <- li
        , Nothing       <- lookupUFM reloadedBy (SSlot slot)
        = do    modify $ \s -> s { sCleanedSpillsAcc = sCleanedSpillsAcc s + 1 }
                cleanBackward liveSlotsOnEntry noReloads acc instrs

        | LiveInstr (SPILL _ slot) _    <- li
        = if elementOfUniqSet slot noReloads

           -- We can erase this spill because the slot won't be read until
           -- after the next one
           then do
                modify $ \s -> s { sCleanedSpillsAcc = sCleanedSpillsAcc s + 1 }
                cleanBackward liveSlotsOnEntry noReloads acc instrs

           else do
                -- This slot is being spilled to, but we haven't seen any reloads yet.
                let noReloads'  = addOneToUniqSet noReloads slot
                cleanBackward liveSlotsOnEntry noReloads' (li : acc) instrs

        -- if we reload from a slot then it's no longer unused
        | LiveInstr (RELOAD slot _) _   <- li
        , noReloads'            <- delOneFromUniqSet noReloads slot
        = cleanBackward liveSlotsOnEntry noReloads' (li : acc) instrs

        -- If a slot is live in a jump target then assume it's reloaded there.
        --
        -- TODO: A real dataflow analysis would do a better job here.
        --       If the target block _ever_ used the slot then we assume
        --       it always does, but if those reloads are cleaned the slot
        --       liveness map doesn't get updated.
        | LiveInstr instr _     <- li
        , targets               <- jumpDestsOfInstr instr
        = do    
                let slotsReloadedByTargets
                        = Set.unions
                        $ catMaybes
                        $ map (flip Map.lookup liveSlotsOnEntry) 
                        $ targets
                
                let noReloads'
                        = foldl' delOneFromUniqSet noReloads 
                        $ Set.toList slotsReloadedByTargets
                
                cleanBackward liveSlotsOnEntry noReloads' (li : acc) instrs

        -- some other instruction
        | otherwise
        = cleanBackward liveSlotsOnEntry noReloads (li : acc) instrs


-- | Combine the associations from all the inward control flow edges.
--
collateJoinPoints :: CleanM ()
collateJoinPoints
 = modify $ \s -> s
        { sJumpValid    = mapUFM intersects (sJumpValidAcc s)
        , sJumpValidAcc = emptyUFM }

intersects :: [Assoc Store]     -> Assoc Store
intersects []           = emptyAssoc
intersects assocs       = foldl1' intersectAssoc assocs


-- | See if we have a reg with the same value as this slot in the association table.
findRegOfSlot :: Assoc Store -> Int -> Maybe Reg
findRegOfSlot assoc slot
        | close                 <- closeAssoc (SSlot slot) assoc
        , Just (SReg reg)       <- find isStoreReg $ uniqSetToList close
        = Just reg

        | otherwise
        = Nothing


-------------------------------------------------------------------------------
-- | Cleaner monad.
type CleanM 
        = State CleanS

-- | Cleaner state.
data CleanS
        = CleanS
        { -- | Regs which are valid at the start of each block.
          sJumpValid            :: UniqFM (Assoc Store)

          -- | Collecting up what regs were valid across each jump.
          --    in the next pass we can collate these and write the results
          --    to sJumpValid.
        , sJumpValidAcc         :: UniqFM [Assoc Store]

          -- | Map of (slot -> blocks which reload from this slot)
          --    used to decide if whether slot spilled to will ever be
          --    reloaded from on this path.
        , sReloadedBy           :: UniqFM [BlockId]

          -- | Spills and reloads cleaned each pass (latest at front)
        , sCleanedCount         :: [(Int, Int)]

          -- | Spills and reloads that have been cleaned in this pass so far.
        , sCleanedSpillsAcc     :: Int
        , sCleanedReloadsAcc    :: Int }


-- | Construct the initial cleaner state.
initCleanS :: CleanS
initCleanS
        = CleanS
        { sJumpValid            = emptyUFM
        , sJumpValidAcc         = emptyUFM

        , sReloadedBy           = emptyUFM

        , sCleanedCount         = []

        , sCleanedSpillsAcc     = 0
        , sCleanedReloadsAcc    = 0 }


-- | Remember the associations before a jump.
accJumpValid :: Assoc Store -> BlockId -> CleanM ()
accJumpValid assocs target
 = modify $ \s -> s {
        sJumpValidAcc = addToUFM_C (++)
                                (sJumpValidAcc s)
                                target
                                [assocs] }


accBlockReloadsSlot :: BlockId -> Slot -> CleanM ()
accBlockReloadsSlot blockId slot
 = modify $ \s -> s {
        sReloadedBy = addToUFM_C (++)
                                (sReloadedBy s)
                                (SSlot slot)
                                [blockId] }


-------------------------------------------------------------------------------
-- A store location can be a stack slot or a register
data Store
        = SSlot Int
        | SReg  Reg


-- | Check if this is a reg store.
isStoreReg :: Store -> Bool
isStoreReg ss
 = case ss of
        SSlot _ -> False
        SReg  _ -> True


-- Spill cleaning is only done once all virtuals have been allocated to realRegs
instance Uniquable Store where
    getUnique (SReg  r)
        | RegReal (RealRegSingle i)     <- r
        = mkRegSingleUnique i

        | RegReal (RealRegPair r1 r2)   <- r
        = mkRegPairUnique (r1 * 65535 + r2)

        | otherwise
        = error $ "RegSpillClean.getUnique: found virtual reg during spill clean,"
                ++ "only real regs expected."

    getUnique (SSlot i) = mkRegSubUnique i    -- [SLPJ] I hope "SubUnique" is ok


instance Outputable Store where
        ppr (SSlot i)   = text "slot" <> int i
        ppr (SReg  r)   = ppr r


-------------------------------------------------------------------------------
-- Association graphs.
-- In the spill cleaner, two store locations are associated if they are known
-- to hold the same value.
--
type Assoc a    = UniqFM (UniqSet a)

-- | An empty association
emptyAssoc :: Assoc a
emptyAssoc      = emptyUFM


-- | Add an association between these two things.
addAssoc :: Uniquable a
         => a -> a -> Assoc a -> Assoc a

addAssoc a b m
 = let  m1      = addToUFM_C unionUniqSets m  a (unitUniqSet b)
        m2      = addToUFM_C unionUniqSets m1 b (unitUniqSet a)
   in   m2


-- | Delete all associations to a node.
delAssoc :: (Outputable a, Uniquable a)
         => a -> Assoc a -> Assoc a

delAssoc a m
        | Just aSet     <- lookupUFM  m a
        , m1            <- delFromUFM m a
        = foldUniqSet (\x m -> delAssoc1 x a m) m1 aSet

        | otherwise     = m


-- | Delete a single association edge (a -> b).
delAssoc1 :: Uniquable a
          => a -> a -> Assoc a -> Assoc a

delAssoc1 a b m
        | Just aSet     <- lookupUFM m a
        = addToUFM m a (delOneFromUniqSet aSet b)

        | otherwise     = m


-- | Check if these two things are associated.
elemAssoc :: (Outputable a, Uniquable a)
          => a -> a -> Assoc a -> Bool

elemAssoc a b m
        = elementOfUniqSet b (closeAssoc a m)


-- | Find the refl. trans. closure of the association from this point.
closeAssoc :: (Outputable a, Uniquable a)
        => a -> Assoc a -> UniqSet a

closeAssoc a assoc
 =      closeAssoc' assoc emptyUniqSet (unitUniqSet a)
 where
        closeAssoc' assoc visited toVisit
         = case uniqSetToList toVisit of

                -- nothing else to visit, we're done
                []      -> visited

                (x:_)
                 -- we've already seen this node
                 |  elementOfUniqSet x visited
                 -> closeAssoc' assoc visited (delOneFromUniqSet toVisit x)

                 -- haven't seen this node before,
                 --     remember to visit all its neighbors
                 |  otherwise
                 -> let neighbors
                         = case lookupUFM assoc x of
                                Nothing         -> emptyUniqSet
                                Just set        -> set

                   in closeAssoc' assoc
                        (addOneToUniqSet visited x)
                        (unionUniqSets   toVisit neighbors)

-- | Intersect two associations.
intersectAssoc
        :: Uniquable a
        => Assoc a -> Assoc a -> Assoc a

intersectAssoc a b
        = intersectUFM_C (intersectUniqSets) a b

