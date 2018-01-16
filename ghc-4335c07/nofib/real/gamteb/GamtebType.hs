-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--	GAMTEB: Monte Carlo
--      1990 August
--
module GamtebType (Angle, Coord, Energy, Indx, PartType, Prob, 
		 Random, StatType, Weight, Value, Result, Stat,
		 Point, Particle(..), Probability) where

type	Angle		= Double
type	Coord		= Double
type	Energy		= Double
type	Indx		= Int		-- index into energy and xsect tables
type	PartType	= Int		-- particle scatter, escape, transmit
type	Prob		= Double
type	Random		= Double
type	StatType	= Int		-- which statistic is incremented
type	Weight		= Double	-- final weight of the particle
type	Value		= Double
type	Result		= ((PartType, Indx), Weight)
type	Stat		= (StatType, Value)
type	Point		= (Coord, Coord, Coord)
type	Probability	= (Prob, Prob, Prob, Prob)

data Particle = 
	Part
	Point			-- position of particle
	Point			-- direction particle is moving in
	Weight			-- weight of particle
	Energy			-- energy of particle
	Indx			-- energy index of particle in table
	Int			-- cell indicating split or roulette
	Random			-- seed of random number generator
