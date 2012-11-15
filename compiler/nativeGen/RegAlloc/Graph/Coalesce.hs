-- | Register coalescing.
module RegAlloc.Graph.Coalesce (
        regCoalesce,
        slurpJoinMovs
) where
import RegAlloc.Liveness
import Instruction
import Reg

import Cmm
import Bag
import Digraph
import UniqFM
import UniqSet
import UniqSupply

import Data.List


-- | Do register coalescing on this top level thing
--
--   For Reg -> Reg moves, if the first reg dies at the same time the
--   second reg is born then the mov only serves to join live ranges.
--   The two regs can be renamed to be the same and the move instruction
--   safely erased.
regCoalesce 
        :: Instruction instr
        => [LiveCmmDecl statics instr] 
        -> UniqSM [LiveCmmDecl statics instr]

regCoalesce code
 = do   
        let joins       = foldl' unionBags emptyBag
                        $ map slurpJoinMovs code

        let alloc       = foldl' buildAlloc emptyUFM 
                        $ bagToList joins

        let patched     = map (patchEraseLive (sinkReg alloc)) code
                        
        return patched


-- | Add a v1 = v2 register renaming to the map.
--   The register with the lowest lexical name is set as the 
--   canonical version.
buildAlloc :: UniqFM Reg -> (Reg, Reg) -> UniqFM Reg
buildAlloc fm (r1, r2)
 = let  rmin    = min r1 r2
        rmax    = max r1 r2
   in   addToUFM fm rmax rmin


-- | Determine the canonical name for a register by following
--   v1 = v2 renamings in this map.
sinkReg :: UniqFM Reg -> Reg -> Reg
sinkReg fm r
 = case lookupUFM fm r of
        Nothing -> r
        Just r' -> sinkReg fm r'        
        

-- | Slurp out mov instructions that only serve to join live ranges.
--
--   During a mov, if the source reg dies and the destiation reg is
--   born then we can rename the two regs to the same thing and
--   eliminate the move.
slurpJoinMovs 
        :: Instruction instr
        => LiveCmmDecl statics instr 
        -> Bag (Reg, Reg)

slurpJoinMovs live
        = slurpCmm emptyBag live
 where  
        slurpCmm   rs  CmmData{} 
         = rs

        slurpCmm   rs (CmmProc _ _ _ sccs)
         = foldl' slurpBlock rs (flattenSCCs sccs)

        slurpBlock rs (BasicBlock _ instrs)
         = foldl' slurpLI    rs instrs
                
        slurpLI    rs (LiveInstr _      Nothing)    = rs
        slurpLI    rs (LiveInstr instr (Just live))
                | Just (r1, r2) <- takeRegRegMoveInstr instr
                , elementOfUniqSet r1 $ liveDieRead live
                , elementOfUniqSet r2 $ liveBorn live

                -- only coalesce movs between two virtuals for now,
                -- else we end up with allocatable regs in the live
                -- regs list.. 
                , isVirtualReg r1 && isVirtualReg r2
                = consBag (r1, r2) rs
                
                | otherwise
                = rs
        
