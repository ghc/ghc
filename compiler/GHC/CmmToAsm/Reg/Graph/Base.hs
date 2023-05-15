
-- | Utils for calculating general worst, bound, squeese and free, functions.
--
--   as per: "A Generalized Algorithm for Graph-Coloring Register Allocation"
--           Michael Smith, Normal Ramsey, Glenn Holloway.
--           PLDI 2004
--
--   These general versions are not used in GHC proper because they are too slow.
--   Instead, hand written optimised versions are provided for each architecture
--   in MachRegs*.hs
--
--   This code is here because we can test the architecture specific code against
--   it.
--
module GHC.CmmToAsm.Reg.Graph.Base (
        RegClass(..),
        Reg(..),
        RegSub(..),

        worst,
        bound,
        squeese
) where

import GHC.Prelude

import GHC.Types.Unique.Set
import GHC.Types.Unique.FM
import GHC.Types.Unique
import GHC.Builtin.Uniques
import GHC.Utils.Monad (concatMapM)

import Data.List.NonEmpty (NonEmpty (..))

-- Some basic register classes.
--      These aren't necessarily in 1-to-1 correspondence with the allocatable
--      RegClasses in MachRegs.hs
data RegClass
        -- general purpose regs
        = ClassG32      -- 32 bit GPRs
        | ClassG16      -- 16 bit GPRs
        | ClassG8       -- 8  bit GPRs

        -- floating point regs
        | ClassF64      -- 64 bit FPRs
        deriving (Show, Eq, Enum)


-- | A register of some class
data Reg
        -- a register of some class
        = Reg RegClass Int

        -- a sub-component of one of the other regs
        | RegSub RegSub Reg
        deriving (Show, Eq)


-- | so we can put regs in UniqSets
instance Uniquable Reg where
        getUnique (Reg c i)
         = mkRegSingleUnique
         $ fromEnum c * 1000 + i

        getUnique (RegSub s (Reg c i))
         = mkRegSubUnique
         $ fromEnum s * 10000 + fromEnum c * 1000 + i

        getUnique (RegSub _ (RegSub _ _))
          = error "RegArchBase.getUnique: can't have a sub-reg of a sub-reg."


-- | A subcomponent of another register
data RegSub
        = SubL16        -- lowest 16 bits
        | SubL8         -- lowest  8 bits
        | SubL8H        -- second lowest 8 bits
        deriving (Show, Enum, Ord, Eq)


-- | Worst case displacement
--
--      a node N of classN has some number of neighbors,
--      all of which are from classC.
--
--      (worst neighbors classN classC) is the maximum number of potential
--      colors for N that can be lost by coloring its neighbors.
--
-- This should be hand coded/cached for each particular architecture,
--      because the compute time is very long..
worst   :: (RegClass    -> UniqSet Reg)
        -> (Reg         -> UniqSet Reg)
        -> Int -> RegClass -> RegClass -> Int

worst regsOfClass regAlias neighbors classN classC
 = let  regAliasS regs  = unionManyUniqSets
                        $ map regAlias
                        $ nonDetEltsUniqSet regs
                        -- This is non-deterministic but we do not
                        -- currently support deterministic code-generation.
                        -- See Note [Unique Determinism and code generation]

        -- all the regs in classes N, C
        regsN           = regsOfClass classN
        regsC           = regsOfClass classC

        -- all the possible subsets of c which have size < m
        regsS           = filter (\s -> not (isEmptyUniqSet s)
                                     && sizeUniqSet s <= neighbors)
                        $ powersetLS regsC

        -- for each of the subsets of C, the regs which conflict
        -- with possibilities for N
        regsS_conflict
                = map (\s -> intersectUniqSets regsN (regAliasS s)) regsS

  in    maximum $ 0 :| map sizeUniqSet regsS_conflict


-- | For a node N of classN and neighbors of classesC
--      (bound classN classesC) is the maximum number of potential
--      colors for N that can be lost by coloring its neighbors.
bound   :: (RegClass    -> UniqSet Reg)
        -> (Reg         -> UniqSet Reg)
        -> RegClass -> [RegClass] -> Int

bound regsOfClass regAlias classN classesC
 = let  regAliasS regs  = unionManyUniqSets
                        $ map regAlias
                        $ nonDetEltsUFM regs
                        -- See Note [Unique Determinism and code generation]

        regsC_aliases
                = unionManyUniqSets
                $ map (regAliasS . getUniqSet . regsOfClass) classesC

        overlap = intersectUniqSets (regsOfClass classN) regsC_aliases

   in   sizeUniqSet overlap


-- | The total squeese on a particular node with a list of neighbors.
--
--   A version of this should be constructed for each particular architecture,
--   possibly including uses of bound, so that aliased registers don't get
--   counted twice, as per the paper.
squeese :: (RegClass    -> UniqSet Reg)
        -> (Reg         -> UniqSet Reg)
        -> RegClass -> [(Int, RegClass)] -> Int

squeese regsOfClass regAlias classN countCs
        = sum
        $ map (\(i, classC) -> worst regsOfClass regAlias i classN classC)
        $ countCs


-- | powerset (for lists)
powersetL :: [a] -> [[a]]
powersetL       = concatMapM (\x -> [[],[x]])


-- | powersetLS (list of sets)
powersetLS :: Uniquable a => UniqSet a -> [UniqSet a]
powersetLS s    = map mkUniqSet $ powersetL $ nonDetEltsUniqSet s
  -- See Note [Unique Determinism and code generation]
