{-	A kernel fragment from a program written by 
		Ron Legere  -- http://www.its.caltech.edu/~legere
		Caltech Quantum Optics

	It has the interesting property that Classic Hugs 
	runs it 20x faster than GHC!
	Reason: runExperiment calls itself with identical parameters,
		and Hugs commons that up for some reason.

	(Even with that fixed, STG Hugs ran the program a lot
	slower than Classic Hugs.)

	So it seems like an interesting program.  It appears here
	in the form with the silly self-call, because that makes
	it run a nice long time.  It thrashes floating point
	multiplication and lists.
-}

module Main where

import System.Environment

infixl 9 .*
infix 9 <*>

main = do
 [arg] <- getArgs
 let n = read arg :: Int
 putStr (show (take n test))

test :: StateStream 
test = runExperiment testforce 0.02 [1.0] (State [1.0] [0.0]) 


testforce :: ForceLaw [Float]
testforce k [] = [] 
testforce k ( (State pos vel):atoms) = (-1.0) .* k * pos: 
		                       testforce k atoms

{-
The test force: K is a list of spring 
constants. (But here I am only doing one dimension for the purposes
of demonstrating the approach)
-}
 


{-
******************
******************
Module: Numerical classical atom (atom.lhs)
******************
******************
-}


-- We will want types for the whole simulation (where we can configure
-- the dimensions, etc), for the results (a state stream), and the force laws.

data AtomState = State Position Velocity
type Position   = [Float]
type Velocity   = [Float]
type Force  = [Float]


type StateStream = [AtomState]



{-
I made AtomState a data type, just so I could play with them. I think
I would prefer to keep it just a synonym, because that would
be simpler!

Now we need a function to write out the results to a file in a nice format.
I think I would prefer a simple x y z /n x y z /n etc

NOTE that show AtomState only shows the position! 
-}

instance Show AtomState where
  show  (State pos vel) = concat [ (show component) ++ "\t"  | component <- pos ]    
  showList states = showString (concat [(show state) ++ "\n" | state <- states])


{-
Note that I used lists for the position and velocity to allow for
unknown number of dimensions. I suspect this will have to
be optimized into tuples at some point!



Ok, so how shall we define the ForceLaw type?
-}

type ForceLaw a = a -> StateStream -> [Force]



{-
The force law maps a stream of states to a stream of forces so that time
dependant forces can be used. The parametric type 'a' is to allow the force law
to depend on some parameter, for example (the common case!) a seed for a random number
generater, and/or the timestep, or the spring constant 
-}
 
runExperiment :: ForceLaw a -> Float -> a -> AtomState -> StateStream 

{-
	In this form this program takes 1 min when compiled under ghc-4.05,
	but takes 3 seconds under hugs....
-}
runExperiment law dt param init = init : zipWith (propagate dt)
              				     (law param stream)
              				     stream
                   where  stream =
                             runExperiment law dt param init

{-
-- In this form GHC is (as expected) much faster
runExperiment law dt param init = stream
	where
	  stream = init : zipWith (propagate dt)
              		  (law param stream)
               		  stream
-}

{-
runExperiment forces timestep param initialcondition :: [AtomState] 
is an infinite stream of atom states. We can then use this to 
generate necessary averages, temperatures, allen variences , or wtf 
you want. 
 
We could for example, start the random number generator with seed param, if
the type is int . 

It is an error to have the initial 
atom state not have the correct number of dimensions. 
-}

propagate :: Float -> Force -> AtomState -> AtomState

{-
Ok, I see one problem with this, not general enough! Some better propagators
exist that can use previous atom states. Actually, by using previous atom states,
we will not even need to seperately track the velocities either.  Oh well, for now
I will stick with that.
-}

propagate dt aforce (State pos vel)  = State newpos newvel
        where newpos = pos  + (dt .* vel)
              newvel = vel  + (dt .* aforce)

-- Note assumes mass =1



{-
********************************************************
********************************************************

Numerical Lists

********************************************************
********************************************************
-}

instance Num a => Num [a] where
	negate (f:fs) = (negate f):(negate fs)
	l + []        = l
	[] + l	  = l
	(f:fs) + (g:gs) = (f+g):fs+gs
	_ * []     = []
	[] * _	   = []
	(f:fs) * (g:gs) = (f*g):(gs*gs)
	fromInteger c = fromInteger c : [0]


(.*):: Num a => a-> [a] -> [a]
c .* []     = []


c .* (f:fs) = c*f : c .* fs

(<*>):: Num a => [a] -> [a] -> a
f <*> g = sum (f*g)


