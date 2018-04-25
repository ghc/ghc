{-
From: David J King <gnik@dcs.gla.ac.uk>
Sent: Friday, October 02, 1998 3:38 PM
Subject: Challenge application 2 -- A circuit simulator


Hi again SCOFPIGgers,

Here's the circuit simulator (uuencoded literate Haskell) as
promised yesterday.  Again this is sequential code and you're
left to your own devices to parallelise it.

It's worth mentioning that the algorithm has been written
with data parallelism in mind.  The main simulation routine
uses paralleliseable combinators i.e. maps, folds, scans etc.
Therefore the key to getting good parallelism is to implement
these combinators efficiently.  We also have a parallel C+MPI
implementation of this algorithm to compare with.

Right now I'm putting together a repository of algorithms
like these for Glasgow Parallel Haskell (GpH).  The more
interesting examples we have the better.  So if you have
an algorithm, please do send it on.  It doesn't matter if
it's not in Haskell.

Cheers,

David and John O'D.
-}

The Circuit Simulator example
David J. King & John O'Donnell
January, 1998

> import System.Environment
> import Data.List

> data BinTree a b = Cell a
>		   | Node b (BinTree a b) (BinTree a b)
>		   deriving Show

Given a list xs where length xs == 2^n, put builds a tree with the
list elements at the leaves and the node elements empty

> put :: [a] -> BinTree a ()
> put [x] = Cell x
> put xs  = Node () (put fstHalf) (put sndHalf)
>	where
>	  (fstHalf, sndHalf) = splitAt (length xs `div` 2) xs

The function get takes a tree and returns a list of the cells, so
we have the identity	(get . put) xs == xs

> get :: BinTree a b -> [a]
> get (Cell x) = [x]
> get (Node x l r) = get l ++ get r


> upsweep :: (a -> a -> a)		-- node function
>	  -> BinTree a b
>	  -> (a, BinTree a (a,a))
> upsweep f (Cell a)     = (a, Cell a)
> upsweep f (Node x l r) = (f lv rv, Node (lv, rv) l' r')
>     where
>	(lv, l') = upsweep f l
>	(rv, r') = upsweep f r

> downsweep :: (a -> b -> c -> (c,c))	-- downsweep function
>	    -> c			-- starting value
>	    -> BinTree d (a,b)
>	    -> BinTree c ()
> downsweep g d (Cell x) 	   = Cell d
> downsweep g d (Node (lv,rv) l r) = Node () l' r'
>	where
>	(dl, dr) = g lv rv d
>	(l', r') = (downsweep g dl l, downsweep g dr r)

An up-sweep followed by a down-sweep

> sweep_ud :: (a -> a -> a)			-- upsweep node function
>	   -> (a -> a -> b -> (b,b))		-- downsweep function
>	   -> b					-- root input
>	   -> BinTree a c
>	   -> (a, BinTree b ())
> sweep_ud up down u t
>	= (ans, downsweep down u t')
>	where (ans, t') = upsweep up t


A left to right scan

> scanL :: (a -> a -> a) -> a -> [a] -> (a,[a])
> scanL f u xs = (up_ans, get t')
>	where
>	(up_ans, t') = sweep_ud f down u (put xs)
>	down l r x = (x, f x l)

A right to left scan

> scanR :: (a -> a -> a) -> a -> [a] -> (a,[a])
> scanR f u xs = (up_ans, get t')
>	where
>	(up_ans, t') = sweep_ud f down u (put xs)
>	down l r x = (f r x, x)

Scans f left to right, and g right to left

> scanlr :: (a -> a -> a)			-- left to right function
>	 -> (a -> a -> a)			-- right to left function
>	 -> a					-- left input
>	 -> a					-- right input
>	 -> [a]
>	 -> ((a,a), [(a,a)])
> scanlr f g lu ru xs = (ans, get t)
>     where
>       ((l_ans,r_ans), t) = sweep_ud up down (lu,ru) (put xs')
>       ans = (g r_ans ru, f lu l_ans)
>       xs' = map (\x -> (x,x)) xs
>       up (lx,ly) (rx,ry) = (f lx rx, g ly ry)
>       down (lx,ly) (rx,ry) (a,b) = ((a, g ry b), (f a lx, b))


--------------------------------------------------------------------------------
A circuit is a list of processor states

> type Circuit a = 	(Int,		-- circuit size
>			[Label],	-- location of inputs
>			[Label],	-- location of outputs
>			[State a])

Names and pids

> type Label = (String, Pid)

> type Pid = Int

Basic circuit components

> data Component
>	= None	-- no component
>	| Inp	-- input to the entire circuit
>	| Outp	-- output from the entire circuit
>	| Dff	-- delay flip flop  
>	| Inv	-- inverter         
>	| And2	-- 2-input and gate 
>	| Or2	-- 2-input or gate  
>	| Xor	-- exclusive or gate
>	deriving (Eq, Show)

a is the type of a signal (the value on a wire)

> data State a = PS
>   {pid        :: Int,		-- site identifier
>    compType   :: Component,	-- component represented in the site
>    pathDepth  :: Int,		-- path depth at which outputs become valid
>    inports    :: [InPort a],	-- tags and latches for the inputs
>    outports   :: [OutPort a]	-- tags and latches for the outputs
>   }



> type InPort a =
>   (Pid,	-- identifies processor that will supply the input signal
>    Int,	-- the output port number of the signal
>    a)		-- latch to hold the input signal value

> type OutPort a =
>	(Int,	-- output port number for the signal value      
>	a,	-- latch to hold the signal value               
>	Bool,	-- need to send it to the left?                 
>	Int,	-- distance to send to the left                 
>	Bool,	-- need to send it to the right?                
>	Int)	-- distance to send to the right                



--------------------------------------------------------------------------------
Outputing circuits

> {-

> instance Show a => Show (State a) where
>	showsPrec p (PS {pid=x, compType=c, pathDepth=d,
>			 inports=ins, outports=outs})
>		= showString "\npid       = " . shows x .
>		  showString "\ncompType  = " . shows c .
>		  showString "\npathDepth = " . shows d .
>		  showString "\ninports   = " . shows ins .
>		  showString "\noutports  = " . shows outs . showChar '\n'
> -}

> instance Show a => Show (State a) where
>	showsPrec p (PS {pid=x, compType=c, pathDepth=d,
>			 inports=ins, outports=outs})
>		= shows (x,c,d,ins,outs) . showChar '\n'

> {-
> instance (Show a, Show b, Show c, Show d, Show e, Show f)
>		=> Show (a, b, c, d, e, f) where
>     showsPrec p (a,b,c,d,e,f) = showChar '(' . shows a . showChar ',' .
>				      		 shows b . showChar ',' .
>						 shows c . showChar ',' .
>						 shows d . showChar ',' .
>						 shows e . showChar ',' .
>						 shows f . showChar ')'


> instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h)
>		=> Show (a, b, c, d, e, f, g, h) where
>     showsPrec p (a,b,c,d,e,f,g,h) = showChar '(' . shows a . showChar ',' .
>				      		 shows b . showChar ',' .
>						 shows c . showChar ',' .
>						 shows d . showChar ',' .
>						 shows e . showChar ',' .
>						 shows f . showChar ',' .
>						 shows g . showChar ',' .
>						 shows h . showChar ')'
> -}

--------------------------------------------------------------------------------
Find the nearest power of two >= x

> nearest_power_of_two :: Int -> Int
> nearest_power_of_two x = until (>=x) (*2) 1


Pad a circuit with empty components to make the state length
a power of two

> pad_circuit :: Signal a => Circuit a -> Circuit a
> pad_circuit (size, ins, outs, states)
>	= (p2, ins, outs, take p2 (states ++ repeat emptyState))
>     where
>	p2 = nearest_power_of_two size


> emptyState :: Signal a => State a
> emptyState = PS {	pid = -1, 
>			compType = None, 
>			pathDepth = -1, 
>			inports = [],
>			outports = []}



--------------------------------------------------------------------------------

> class (Eq a, Show a) => Signal a where
>	zeroS, one :: a
>	inv :: a -> a
>	and2, or2, xor :: a -> a -> a
>
>	inv x = if (x==one) then zeroS  else one
>
>	and2 x y = if (x==one) && (y==one) then one else zeroS
>
>	or2 x y = if (x==one) || (y==one) then one else zeroS
>
>	xor x y = if (x==y) then one else zeroS


> data Boolean = F | T
>	deriving (Eq, Show)

> instance Signal Boolean where
>	zeroS = F
>	one   = T

--------------------------------------------------------------------------------
Packets are the objects that get send around.  Then are just outports
with pids, and extents.

Packets are just OutPorts with a pid

> type Packet a =
>	(Pid,	-- id of this packet
>	Int,	-- output port number for the signal value      
>	a,	-- latch to hold the signal value               
>	Bool,	-- need to send it to the left?                 
>	Int,	-- distance to send to the left                 
>	Bool,	-- need to send it to the right?                
>	Int,	-- distance to send to the right  
>	Int)	-- extent


Note: all extents should have an initial value of 1

> emptyPacket :: Signal a => Packet a
> emptyPacket = (-1, -1, zeroS, False, 0, False, 0, 1)


--------------------------------------------------------------------------------
Showing packets

> showPacket :: Signal a => Packet a -> IO ()
> showPacket (pid, reqL, distL, reqR, distR, sender, msg, ext)
>	= putStr (show pid) >> putStr "\t" >>
>	  putStr (show reqL) >> putStr "\t" >>
>	  putStr (show distL) >> putStr "\t" >>
>	  putStr (show reqR) >> putStr "\t" >>
>	  putStr (show distR) >> putStr "\t" >>
>	  putStr (show sender) >> putStr "\t" >>
>	  putStr (show msg) >> putStr "\t" >>
>	  putStr (show ext) >> putStr "\n"


> showPackets :: Signal a => [Packet a] -> IO ()
> showPackets []     = putStr ""
> showPackets (x:xs) = showPacket x >> showPackets xs

--------------------------------------------------------------------------------
Sending packets

Sends messages from left to right, and is scanned with scan-left

If two messages are send to the right, and there is an overlap, then
the leftmost one gets sent onward

> send_right :: Packet a -> Packet a -> Packet a
> send_right (ia,sa,ma,qla,dla,qra,dra,ea) (ib,sb,mb,qlb,dlb,qrb,drb,eb) =
>	if qra && dra>eb
>	  then (ia,sa,ma,qla,dla,qra,dra-eb,ea+eb)
>	  else (ib,sb,mb,qlb,dlb,qrb,drb,ea+eb)


Sends messages from right to left, and is scanned with scan-right

> send_left :: Packet a -> Packet a -> Packet a
> send_left (ia,sa,ma,qla,dla,qra,dra,ea) (ib,sb,mb,qlb,dlb,qrb,drb,eb) =
>	if qlb && dlb>ea
>	  then (ib,sb,mb,qlb,dlb-ea,qrb,drb,ea+eb)
>	  else (ia,sa,ma,qla,dla,qra,dra,ea+eb)

> send :: Signal a => [Packet a]
>	  -> ((Packet a, Packet a), [(Packet a, Packet a)])
> send xs = scanlr send_right send_left emptyPacket emptyPacket xs


--------------------------------------------------------------------------------
Circuit simulator

"inputs_list" contains the inputs for each cycle.  Hence, the
length of "inputs_list" corresponds to the number of cycles that we
are simulating.


> circuit_simulate :: Signal a => [[a]] -> Circuit a -> [[a]]
> circuit_simulate inputs_list circuit
>	= map collect_outputs (simulate inputs_list circuit)


Pick up the values held in the output components for a circuit

> collect_outputs :: Circuit a -> [a]
> collect_outputs (size, ins, outs, states) = map get_output outs
>    where
>	get_output (label, p)
>		 = third (head [ head (inports s) | s<-states, p==pid s])
>	third (_,_,v) = v


> simulate :: Signal a => [[a]] -> Circuit a -> [Circuit a]
> simulate inputs_list circuit@(size, ins, outs, states)
>	= tail (scanl (do_cycle cpd) circuit' inputs_list)
>     where
>	circuit' = (size, ins, outs, map init_dffs states)
>	cpd = critical_path_depth circuit

"do_cycle" will simulate the circuit for one cycle

"inputs" is a list of signals.  The order is important because it
relates the input values with input labels.

> do_cycle :: Signal a => Int -> Circuit a -> [a] -> Circuit a
> do_cycle cpd (size, ins, outs, states) inputs = (size, ins, outs, states4)
>     where
>	states1 = map (store_inputs (zip ins inputs)) states
>	states2 = do_sends 0 states1
>	states3 = foldl sim_then_send states2 [1..cpd]
>	sim_then_send state d = do_sends d (simulate_components d state)
>	states4 = restore_requests states states3


After doing a cycle the request-to-send flags will all be False, so we
need to restore them to there original values.

> restore_requests :: Signal a => [State a] -> [State a] -> [State a]
> restore_requests old_states new_states
>		= zipWith restore old_states new_states
>      where
>	restore os ns = ns { outports = zipWith restore_outport (outports os) 
>						    (outports ns) }
>	restore_outport (p,_,ql,dl,qr,dq) (_,m,_,_,_,_) = (p,m,ql,dl,qr,dq)


--------------------------------------------------------------------------------
Send outputs to destinations

Repeatedly send until all the request-to-send flags at depth d are False

> do_sends :: Signal a => Int -> [State a] -> [State a]
> do_sends d states = until (acknowledge d) (do_send d) states


> acknowledge :: Signal a => Int -> [State a] -> Bool
> acknowledge d states = not (or (map (check_requests . outports) states1))
>     where
>	check_requests xs = or (map check_lr_requests xs)
>	check_lr_requests (p,m,ql,dl,qr,dr) = ql || qr
>	states1 = map (check_depth d) states

Convert outports to packets, do a send, and then integrate packets
back into the inports and outports

> do_send :: Signal a => Int -> [State a] -> [State a]
> do_send d states = zipWith (update_io d) pss' states
>     where
>	states1 = map (check_depth d) states
>	pss = (transpose . pad_packets) (map make_packet states1)
>	send_results = map (snd . send) pss
>	pss' = transpose send_results


--------------------------------------------------------------------------------
Update both the inports and outports of a state

> update_io :: Signal a => Int -> [(Packet a,Packet a)] -> State a -> State a
> update_io d lrps state = update_os (update_is state)
>     where
>	update_is state = state { inports = foldr update_i 
>						       (inports state) lrps }
>	update_os state = if pathDepth state == d
>			    then state { outports = zipWith update_o 
>							lrps (outports state) }
>			    else state

--------------------------------------------------------------------------------
Updating outports

> update_o :: Signal a => (Packet a, Packet a) -> OutPort a -> OutPort a
> update_o (lp, rp) out = check_left lp (check_right rp out)

> check_left (pid, port, pm, pql, pdl, pqr, pdr, e) (p, m, ql, dl, qr, dr)
>	= if pqr && pdr>0
>		then (p, m, ql, dl, qr, dr)		-- message blocked
>		else (p, m, ql, dl, False, dr)		-- send right succeeded

> check_right (pid, port, pm, pql, pdl, pqr, pdr, e) (p, m, ql, dl, qr, dr)
>	= if pql && pdl>0
>	     then (p, m, ql, dl, qr, dr)		-- message blocked
>	     else (p, m, False, dl, qr, dr)		-- send left succeeded


--------------------------------------------------------------------------------
Updating inports

> update_i :: Signal a => (Packet a, Packet a) -> [InPort a] -> [InPort a]
> update_i (l,r) ins = up_i l (up_i r ins)

> up_i :: Signal a => Packet a -> [InPort a] -> [InPort a]
> up_i (i, p, m', _, _, _, _, _) ins
>	= map (compare_and_update (i,p,m')) ins

> compare_and_update :: Signal a => InPort a -> InPort a -> InPort a
> compare_and_update (i, p, m') (pid, port, m)
>		= if (i, p) == (pid, port)
>		    then (pid, port, m')
>		    else (pid, port, m)

--------------------------------------------------------------------------------
Make a packet list from an outport list, i.e. just add in the pid and
an initial extent of "1"

> make_packet :: Signal a => State a -> [Packet a]
> make_packet state = [ (pid state, p, m, ql, dl, qr, dr, 1)
>			| (p, m, ql, dl, qr, dr) <- outports state ]


> pad_packets :: Signal a => [[Packet a]] -> [[Packet a]]
> pad_packets pss = map pad pss
>     where
>	pad xs = take max_ps (xs ++ repeat emptyPacket)
>	max_ps = maximum (map length pss)

Make all requests False that are not at depth "d"

> check_depth :: Signal a => Int -> State a -> State a
> check_depth d state
>	= if pathDepth state == d
>	    then state
>	    else update_requests False state

> update_requests :: Signal a => Bool -> State a -> State a
> update_requests b state
>	= state { outports = [ (p, m, b, dl, b, dr)
>			       | (p, m, ql, dl, qr, dr) <- outports state ] }

--------------------------------------------------------------------------------
Simulate all components at path depth "depth"

> simulate_components :: Signal a => Int -> [State a] -> [State a]
> simulate_components depth states
>	= map (simulate_component depth) states


> simulate_component :: Signal a => Int -> State a -> State a
> simulate_component d state = if d == pathDepth state && new_value/=Nothing
>		  		 then let Just v = new_value
>				      in update_outports state v
>				 else state
>     where
>	out_signals = [ sig | (_,_,sig) <- inports state]
>	new_value = apply_component (compType state) out_signals

> apply_component :: Signal a => Component -> [a] -> Maybe a
> apply_component Inp _      = Nothing
> apply_component Outp [x]   = Just x
> apply_component Dff [x]    = Just x
> apply_component Inv [x]    = Just (inv x)
> apply_component And2 [x,y] = Just (and2 x y)
> apply_component Or2 [x,y]  = Just (or2 x y)
> apply_component Xor [x,y]  = Just (xor x y)
> apply_component _ _        = error "Error: apply_component\n"


--------------------------------------------------------------------------------
Place input values into circuit's input components

> store_inputs :: Signal a => [(Label,a)] -> State a -> State a
> store_inputs label_inputs state@(PS {compType=Inp})
>	= head [ update_outports state value
>			| ((label, input_pid), value) <- label_inputs,  
>						pid state == input_pid ]
> store_inputs label_inputs state = state


--------------------------------------------------------------------------------
set all flip-flips to zero

> init_dffs :: Signal a => State a -> State a
> init_dffs state = if compType state == Dff
>	    	      then update_outports state zeroS
>		      else state

--------------------------------------------------------------------------------
Critical path depth

Returns the critical path length of a given circuit

> critical_path_depth :: Signal a => Circuit a -> Int
> critical_path_depth (size, ins, outs, states)
>	= maximum (map pathDepth states)

--------------------------------------------------------------------------------
Generate all possible input values

> input_values :: Signal a => Int -> [[a]]
> input_values nbits = map binary [0..2^nbits-1]
>     where
>	binary n = map int2sig (reverse (take nbits (bin n ++ repeat 0)))
>	int2sig s = if (s==0) then zeroS else one
>	bin 0 = []
>	bin n = r:bin q
>	      where
>		(q,r) = n `quotRem` 2


--------------------------------------------------------------------------------
Utility functions

> update_outports :: Signal a => State a -> a -> State a
> update_outports state value
>		= state { outports = [ (p, value, ql, dl, qr, dr)
>					| (p, m, ql, dl, qr, dr) <- outs ] }
>     where
>	outs = outports state


--------------------------------------------------------------------------------
Circuit example -- a register


> regs :: Signal a => Int -> Circuit a
> regs bits = (size, is, os, states)
>     where
>	size = 1+7*bits
>	is = ("sto",0): zipWith ilabel [0..] [ 7*x+1 | x <- [0..bits-1]]
>	ilabel n pid = ("x" ++ show n, pid)
>	os = zipWith olabel [0..] [ 7*x+7 | x <- [0..bits-1]]
>	olabel n pid = ("y" ++ show n, pid)
>	states = sto:concat (map (reg 0) [ 7*x+1 | x <- [0..bits-1]])
>	sto = PS {	pid = 0,
>			compType  = Inp,
>			pathDepth = 0,
>			inports    = [],
>			outports   = [(0, zeroS, False, 
>				       0, True, 8*(bits-1)+5)]
>		}

A one bit register with seven components

> reg :: Signal a => Pid -> Pid -> [State a]
> reg sto n
>   = [ PS { pid       = n,	-- x input -------------------------------------
>	     compType  = Inp,
>	     pathDepth = 0,
>	     inports    = [],
>	     outports   = [(0, zeroS, False, 0, True, 4)]	-- lower and
>	   },
>	PS { pid       = n+1,	-- dff -----------------------------------------
>	     compType  = Dff,
>	     pathDepth = 1,
>	     inports    = [(n+5, 0, zeroS)],
>	     outports   = [(0, zeroS, False, 0, True, 5)]	-- y output
>	   },
>	PS { pid       = n+2,	-- inv -----------------------------------------
>	     compType  = Inv,
>	     pathDepth = 1,
>	     inports    = [(sto, 0, zeroS)],			-- sto
>	     outports   = [(0, zeroS, False, 0, True, 1)]	-- upper and
>	   },
>	PS { pid       = n+3, -- upper and -------------------------------------
>	     compType  = And2,
>	     pathDepth = 2,
>	     inports    = [(n+1, 0, zeroS),		-- dff
>			  (n+2, 0, zeroS)],		-- inv
>	     outports   = [(0, zeroS, False, 0, True, 2)]	-- or
>	   },
>	PS { pid       = n+4,	-- lower and -----------------------------------
>	     compType  = And2,
>	     pathDepth = 1,
>	     inports    = [(sto, 0, zeroS),		-- sto input
>			  (n, 0, zeroS)],		-- x input
>	     outports   = [(0, zeroS, False, 0, True, 1)]	-- or
>	   },
>	PS { pid       = n+5,	-- or ------------------------------------------
>	     compType  = Or2,
>	     pathDepth = 3,
>	     inports    = [(n+3, 0, zeroS),		-- upper and
>			  (n+4, 0, zeroS)],		-- lower and
>	     outports   = [(0, zeroS, True, 4, False, 0)]	-- dff
>	   },
>	PS { pid	= n+6,	-- output y ------------------------------------
>	     compType   = Outp,
>	     pathDepth  = 4,
>	     inports	= [(n+1, 0, zeroS)],		-- dff
>	     outports	= []
>	   }
>	]


--------------------------------------------------------------------------------
To run (with ghc) for a (8 bit register) circuit over 1000 cycles

% ghc -o circ_sim circ_sim.lhs
% circ_sim 8 1000

> main :: IO ()
> main = getArgs >>= \[num_bits, num_cycles] ->
>	 print (run (read num_bits) (read num_cycles))


> run :: Int -> Int -> [[Boolean]]
> run num_bits num_cycles = circuit_simulate cycles example
>	where
>	example = pad_circuit (regs num_bits)
>	cycles = take num_cycles (repeat inputs)
>	inputs = take (num_bits + 1) (repeat T)
