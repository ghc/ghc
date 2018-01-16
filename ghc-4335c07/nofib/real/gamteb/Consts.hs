-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module Consts (cylLen,cylLen2,cylRad,cylRad2,wgtCut,wgtCut2,ergCut,
	       big,small,nothing,
	       scatter,escape,transit,numExit,numLev,numStat,
	       ne,nt,ns,nek,nwk,nr,nsp,nc,nnc,nrk,wrl,wrg) where


cylLen,cylLen2,cylRad,cylRad2,wgtCut,wgtCut2,ergCut,big,small,nothing :: Double
cylLen		= 20.0			-- cylinder length
cylLen2		= cylLen + 10.0		-- cylinder length squared
cylRad		= 1.0			-- cylinder radius
cylRad2		= cylRad * cylRad	-- cylinder radius squared
big		= 100.0
small		= 0.0000001
nothing		= -30.0			-- bad value in xsect table
wgtCut		= 0.5			-- weight cutoff of a particle
wgtCut2		= wgtCut * wgtCut	-- weight cutoff squared
ergCut		= 0.001			-- energy cutoff in pair

scatter,escape,transit,numExit,numLev,numStat :: Int
ne,nt,ns,nek,nwk,nr,nsp,nc,nnc,nrk,wrl,wrg :: Int
scatter		= 1
escape		= 2
transit		= 3
numExit		= 3
numLev		= 35
numStat         = 12                    -- number of statistics
ne              = 1                     -- number of escapes
nt              = 2                     -- number of transits
ns              = 3                     -- number of scatters
nek             = 4                     -- number of energy kills
nwk             = 5                     -- number of weight kills
nr              = 6                     -- number of roulettes
nsp             = 7                     -- number of splits
nc              = 8                     -- number of collisions
nnc             = 9                     -- number of noncollisions
nrk             = 10                    -- number of roulette kills
wrl             = 11                    -- weight in roulette lost
wrg             = 12                    -- weight in roulette gained
