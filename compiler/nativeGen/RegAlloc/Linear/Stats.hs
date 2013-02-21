module RegAlloc.Linear.Stats (
        binSpillReasons,
        countRegRegMovesNat,
        pprStats
)

where

import RegAlloc.Linear.Base
import RegAlloc.Liveness
import Instruction

import UniqFM
import Outputable

import Data.List
import State

-- | Build a map of how many times each reg was alloced, clobbered, loaded etc.
binSpillReasons
        :: [SpillReason] -> UniqFM [Int]

binSpillReasons reasons
        = addListToUFM_C
                (zipWith (+))
                emptyUFM
                (map (\reason -> case reason of
                        SpillAlloc r    -> (r, [1, 0, 0, 0, 0])
                        SpillClobber r  -> (r, [0, 1, 0, 0, 0])
                        SpillLoad r     -> (r, [0, 0, 1, 0, 0])
                        SpillJoinRR r   -> (r, [0, 0, 0, 1, 0])
                        SpillJoinRM r   -> (r, [0, 0, 0, 0, 1])) reasons)


-- | Count reg-reg moves remaining in this code.
countRegRegMovesNat 
        :: Instruction instr
        => NatCmmDecl statics instr -> Int

countRegRegMovesNat cmm
        = execState (mapGenBlockTopM countBlock cmm) 0
 where
        countBlock b@(BasicBlock _ instrs)
         = do   mapM_ countInstr instrs
                return  b

        countInstr instr
                | Just _        <- takeRegRegMoveInstr instr
                = do    modify (+ 1)
                        return instr

                | otherwise
                =       return instr


-- | Pretty print some RegAllocStats
pprStats 
        :: Instruction instr 
        => [NatCmmDecl statics instr] -> [RegAllocStats] -> SDoc

pprStats code statss
 = let  -- sum up all the instrs inserted by the spiller
        spills          = foldl' (plusUFM_C (zipWith (+)))
                                emptyUFM
                        $ map ra_spillInstrs statss

        spillTotals     = foldl' (zipWith (+))
                                [0, 0, 0, 0, 0]
                        $ eltsUFM spills

        -- count how many reg-reg-moves remain in the code
        moves           = sum $ map countRegRegMovesNat code

        pprSpill (reg, spills)
                = parens $ (hcat $ punctuate (text ", ")  (doubleQuotes (ppr reg) : map ppr spills))

   in   (  text "-- spills-added-total"
        $$ text "--    (allocs, clobbers, loads, joinRR, joinRM, reg_reg_moves_remaining)"
        $$ (parens $ (hcat $ punctuate (text ", ") (map ppr spillTotals ++ [ppr moves])))
        $$ text ""
        $$ text "-- spills-added"
        $$ text "--    (reg_name, allocs, clobbers, loads, joinRR, joinRM)"
        $$ (vcat $ map pprSpill
                 $ ufmToList spills)
        $$ text "")

