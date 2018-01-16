-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module

export cl,cl2,crad,crad2,big,small,wcp1,wcp2,energyCutoff,nothing,
       scatter,escape,transit, numexit,numlev,numstat,
       ne,nt,ns,nek,nwk,nr,nsp,nc,nnc,nrk,wrl,wrg;

rec	cl		= 20.0			-- cylinder length
and	cl2		= cl +. 10.0		-- cylinder length squared
and	crad		= 1.0			-- cylinder radius
and	crad2		= crad *. crad		-- cylinder radius squared
and	big		= 100.0
and	small		= 0.0000001
and	nothing		= (0.0 -. 30.0)		-- bad value for xsect table
and	wcp1		= 0.5			-- weight cutoff of a particle
and	wcp2		= wcp1 *. wcp1		-- weight cutoff squared
and	energyCutoff	= 0.001			-- energy cutoff in pair

and	scatter		= 1
and	escape		= 2
and	transit		= 3
and	numexit		= 3			-- number of possible exits
and	numlev		= 35			-- number of energy levels

and	ne		= 1			-- number of escapes
and	nt		= 2			-- number of transits
and	ns		= 3			-- number of scatters
and	nek		= 4			-- number of energy kills
and	nwk		= 5			-- number of weight kills
and	nr		= 6			-- number of roulettes
and	nsp		= 7			-- number of splits
and	nc		= 8			-- number of collisions
and	nnc		= 9			-- number of noncollisions
and	nrk		= 10			-- number of roulette kills
and	wrl		= 11			-- weight in roulette lost
and	wrg		= 12			-- weight in roulette gained
and	numstat		= 12			-- number of statistics

end
