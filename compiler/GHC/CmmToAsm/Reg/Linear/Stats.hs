module GHC.CmmToAsm.Reg.Linear.Stats (
        binSpillReasons,
        countRegRegMovesNat,
        pprStats
)

where

import GHC.Prelude

import GHC.CmmToAsm.Reg.Linear.Base
import GHC.CmmToAsm.Reg.Liveness
import GHC.CmmToAsm.Instr
import GHC.Types.Unique (Unique)
import GHC.CmmToAsm.Types

import GHC.Types.Unique.FM

import GHC.Utils.Outputable
import GHC.Utils.Monad.State.Strict
import GHC.Platform (Platform)

-- | Build a map of how many times each reg was alloced, clobbered, loaded etc.
binSpillReasons
        :: [SpillReason] -> UniqFM Unique [Int]
        -- See Note [UniqFM and the register allocator]
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
        => Platform
        -> NatCmmDecl statics instr -> Int

countRegRegMovesNat platform cmm
        = execState (mapGenBlockTopM countBlock cmm) 0
 where
        countBlock b@(BasicBlock _ instrs)
         = do   mapM_ countInstr instrs
                return  b

        countInstr instr
                | Just _        <- takeRegRegMoveInstr platform instr
                = do    modify (+ 1)
                        return instr

                | otherwise
                =       return instr


-- | Pretty print some RegAllocStats
pprStats
        :: Instruction instr
        => Platform -> [NatCmmDecl statics instr] -> [RegAllocStats] -> SDoc

pprStats platform code statss
 = let  -- sum up all the instrs inserted by the spiller
        -- See Note [UniqFM and the register allocator]
        spills :: UniqFM Unique [Int]
        spills          = foldl' (plusUFM_C (zipWith (+)))
                                emptyUFM
                        $ map ra_spillInstrs statss

        spillTotals     = foldl' (zipWith (+))
                                [0, 0, 0, 0, 0]
                        $ nonDetEltsUFM spills
                        -- See Note [Unique Determinism and code generation]

        -- count how many reg-reg-moves remain in the code
        moves           = sum $ map (countRegRegMovesNat platform) code

        pprSpill (reg, spills)
                = parens $ (hcat $ punctuate (text ", ")  (doubleQuotes (ppr reg) : map ppr spills))

   in   (  text "-- spills-added-total"
        $$ text "--    (allocs, clobbers, loads, joinRR, joinRM, reg_reg_moves_remaining)"
        $$ (parens $ (hcat $ punctuate (text ", ") (map ppr spillTotals ++ [ppr moves])))
        $$ text ""
        $$ text "-- spills-added"
        $$ text "--    (reg_name, allocs, clobbers, loads, joinRR, joinRM)"
        $$ (pprUFMWithKeys spills (vcat . map pprSpill))
        $$ text "")

