
-- | A description of the register set of the X86.
--
--   This isn't used directly in GHC proper.
--
--   See RegArchBase.hs for the reference.
--   See MachRegs.hs for the actual trivColorable function used in GHC.
--
module RegAlloc.Graph.ArchX86 (
        classOfReg,
        regsOfClass,
        regName,
        regAlias,
        worst,
        squeese,
) where
import RegAlloc.Graph.ArchBase  (Reg(..), RegSub(..), RegClass(..))
import UniqSet


-- | Determine the class of a register
classOfReg :: Reg -> RegClass
classOfReg reg
 = case reg of
        Reg c _         -> c
        
        RegSub SubL16 _ -> ClassG16
        RegSub SubL8  _ -> ClassG8
        RegSub SubL8H _ -> ClassG8

        
-- | Determine all the regs that make up a certain class.
regsOfClass :: RegClass -> UniqSet Reg
regsOfClass c
 = case c of
        ClassG32        
         -> mkUniqSet   [ Reg ClassG32  i                     
                        | i <- [0..7] ]

        ClassG16        
         -> mkUniqSet   [ RegSub SubL16 (Reg ClassG32 i)
                        | i <- [0..7] ]

        ClassG8 
         -> unionUniqSets
                (mkUniqSet [ RegSub SubL8  (Reg ClassG32 i) | i <- [0..3] ])
                (mkUniqSet [ RegSub SubL8H (Reg ClassG32 i) | i <- [0..3] ])
                        
        ClassF64        
         -> mkUniqSet   [ Reg ClassF64  i
                        | i <- [0..5] ]
        

-- | Determine the common name of a reg
--      returns Nothing if this reg is not part of the machine.
regName :: Reg -> Maybe String
regName reg
 = case reg of
        Reg ClassG32 i  
         | i <= 7-> Just $ [ "eax", "ebx", "ecx", "edx"
                           , "ebp", "esi", "edi", "esp" ] !! i

        RegSub SubL16 (Reg ClassG32 i)
         | i <= 7 -> Just $ [ "ax", "bx", "cx", "dx"
                            , "bp", "si", "di", "sp"] !! i
         
        RegSub SubL8  (Reg ClassG32 i)
         | i <= 3 -> Just $ [ "al", "bl", "cl", "dl"] !! i
         
        RegSub SubL8H (Reg ClassG32 i)
         | i <= 3 -> Just $ [ "ah", "bh", "ch", "dh"] !! i

        _         -> Nothing

        
-- | Which regs alias what other regs.
regAlias :: Reg -> UniqSet Reg
regAlias reg
 = case reg of

        -- 32 bit regs alias all of the subregs
        Reg ClassG32 i
         
         -- for eax, ebx, ecx, eds
         |  i <= 3              
         -> mkUniqSet 
         $ [ Reg ClassG32 i,   RegSub SubL16 reg
           , RegSub SubL8 reg, RegSub SubL8H reg ]
         
         -- for esi, edi, esp, ebp
         | 4 <= i && i <= 7     
         -> mkUniqSet 
         $ [ Reg ClassG32 i,   RegSub SubL16 reg ]
        
        -- 16 bit subregs alias the whole reg
        RegSub SubL16 r@(Reg ClassG32 _)        
         ->     regAlias r
        
        -- 8 bit subregs alias the 32 and 16, but not the other 8 bit subreg
        RegSub SubL8  r@(Reg ClassG32 _)
         -> mkUniqSet $ [ r, RegSub SubL16 r, RegSub SubL8 r ]

        RegSub SubL8H r@(Reg ClassG32 _)
         -> mkUniqSet $ [ r, RegSub SubL16 r, RegSub SubL8H r ]
        
        -- fp
        Reg ClassF64 _  
         -> unitUniqSet reg

        _ -> error "regAlias: invalid register"


-- | Optimised versions of RegColorBase.{worst, squeese} specific to x86
worst :: Int -> RegClass -> RegClass -> Int
worst n classN classC
 = case classN of
        ClassG32
         -> case classC of
                ClassG32        -> min n 8
                ClassG16        -> min n 8
                ClassG8         -> min n 4
                ClassF64        -> 0
                
        ClassG16
         -> case classC of
                ClassG32        -> min n 8
                ClassG16        -> min n 8
                ClassG8         -> min n 4
                ClassF64        -> 0
                
        ClassG8
         -> case classC of
                ClassG32        -> min (n*2) 8
                ClassG16        -> min (n*2) 8
                ClassG8         -> min n 8
                ClassF64        -> 0
                
        ClassF64
         -> case classC of
                ClassF64        -> min n 6
                _               -> 0
                
squeese :: RegClass -> [(Int, RegClass)] -> Int
squeese classN countCs
        = sum (map (\(i, classC) -> worst i classN classC) countCs)
        
