-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module

#define Angle		Float
#define	Count		Int
#define Energy		Float
#define	Indx		Int
#define	PartType	Int
#define	Random		Float
#define	StatType	Int
#define Weight		Float
#define Value		Float

export Point, Particle, Prob, Result, Stat;

rec
type Point = Pt Float Float Float

and
type Particle =
	Part
	Point			-- position	x,y,z
	Point			-- dir_cosine	u,v,w
	Weight			-- weight	wt
	Energy			-- energy	erg
	Indx			-- e_indx	
	Int			-- cell		ia
	Random			-- seed		ps

and
type Prob =
	Pr
	Float			-- pcompton
	Float			-- ppair
	Float			-- pphoto
	Float			-- ptotal

and
type Result =
	Res
	PartType		-- did particle scatter, escape or transit
	Indx			-- index of final energy in energy table
	Weight			-- final weight of particle
	
and
type Stat =
	St
	StatType		-- which statistic is incremented
	Value			-- increment value

end
