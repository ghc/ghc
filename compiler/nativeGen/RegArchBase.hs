
-- | Utils for calculating general worst, bound, squeese and free, functions.
--
--	as per: "A Generalized Algorithm for Graph-Coloring Register Allocation"
--		Michael Smith, Normal Ramsey, Glenn Holloway.
--		PLDI 2004
--	
--	These general versions are not used in GHC proper because they are too slow.
--	Instead, hand written optimised versions are provided for each architecture
--	in MachRegs*.hs 
--
--	This code is here because we can test the architecture specific code against it.
--
--
module RegArchBase (
	RegClass(..),
	Reg(..),
	RegSub(..),
	
	worst,
	bound,
	squeese
)
	
where


-----
import qualified Data.Set	as Set
import Data.Set			(Set)

-- import qualified Data.Map	as Map
-- import Data.Map			(Map)


-- Some basic register classes.
--	These aren't nessesarally in 1-to-1 correspondance with the allocatable
--	RegClasses in MachRegs.hs
--
data RegClass
	-- general purpose regs
	= ClassG32	-- 32 bit GPRs
	| ClassG16	-- 16 bit GPRs
	| ClassG8	-- 8  bit GPRs
	
	-- floating point regs
	| ClassF64	-- 64 bit FPRs
	deriving (Show, Ord, Eq)


-- | A register of some class
data Reg
	-- a register of some class
	= Reg RegClass Int
	
	-- a sub-component of one of the other regs
	| RegSub RegSub Reg
	deriving (Show, Ord, Eq)


-- | A subcomponent of another register
data RegSub
	= SubL16	-- lowest 16 bits
	| SubL8		-- lowest  8 bits
	| SubL8H	-- second lowest 8 bits
	deriving (Show, Enum, Ord, Eq)
	


-- | Worst case displacement
--
--	a node N of classN has some number of neighbors, 
--	all of which are from classC.
--
--	(worst neighbors classN classC) is the maximum number of potential
--	colors for N that can be lost by coloring its neighbors.

-- This should be hand coded/cached for each particular architecture,
--	because the compute time is very long..

worst 
	:: (RegClass 	-> Set Reg)
	-> (Reg 	-> Set Reg)
	-> Int -> RegClass -> RegClass -> Int

worst regsOfClass regAlias neighbors classN classC
 = let	regAliasS regs	= unionsS $ Set.map regAlias regs

	-- all the regs in classes N, C
 	regsN		= regsOfClass classN
	regsC		= regsOfClass classC
	
	-- all the possible subsets of c which have size < m
	regsS		= Set.filter (\s -> Set.size s >= 1 && Set.size s <= neighbors)
			$ powerset regsC

	-- for each of the subsets of C, the regs which conflict with posiblities for N
	regsS_conflict 
		= Set.map (\s -> Set.intersection regsN (regAliasS s)) regsS

  in	Set.findMax $ Set.map Set.size $ regsS_conflict


-- | For a node N of classN and neighbors of classesC
--	(bound classN classesC) is the maximum number of potential 
--	colors for N that can be lost by coloring its neighbors.
--

bound 
	:: (RegClass 	-> Set Reg)
	-> (Reg		-> Set Reg)
	-> RegClass -> [RegClass] -> Int

bound regsOfClass regAlias classN classesC
 = let	regAliasS regs	= unionsS $ Set.map regAlias regs
 
 	regsC_aliases
		= Set.unions 
		$ map (regAliasS . regsOfClass) classesC

	overlap	= Set.intersection (regsOfClass classN) regsC_aliases
   
   in	Set.size overlap


-- | The total squeese on a particular node with a list of neighbors.
--
--	A version of this should be constructed for each particular architecture,
--	possibly including uses of bound, so that alised registers don't get counted
--	twice, as per the paper.
--	
squeese 
	:: (RegClass	-> Set Reg)
	-> (Reg		-> Set Reg)
	-> RegClass -> [(Int, RegClass)] -> Int

squeese regsOfClass regAlias classN countCs
	= sum (map (\(i, classC) -> worst regsOfClass regAlias i classN classC) countCs)
	

-- | powerset (for lists)
powersetL :: Ord a => [a] -> [[a]]
powersetL 	= map concat . mapM (\x -> [[],[x]])
	
-- | powerset (for sets)
powerset :: Ord a => Set a -> Set (Set a)
powerset s	= Set.fromList $ map Set.fromList $ powersetL $ Set.toList s

-- | unions (for sets)
unionsS :: Ord a => Set (Set a) -> Set a
unionsS	ss 	= Set.unions $ Set.toList ss


