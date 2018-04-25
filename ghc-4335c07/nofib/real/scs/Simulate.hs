module Simulate where

import Data.Array
import Data.List
import Data.Ratio

import RandomFix
import LinearAlgebra
import Types

simulate :: Circuit -> Seed -> Time -> Time -> Temperature -> RBC -> [(Time, Output)]
simulate circuit seed dt end_time temp rbc	= select trace
  where
  	select					= takeWhile (\(t, _) -> t <= end_time) . map (\(t, s, o, r) -> (t, o))
	trace					= (concat . iterate step) ((tunnel . correct . solve . sources) (0, initial', initial', randoms))
	step					= tunnel . correct . solve . sources . integrate . last
	randoms					= map ((/ (2 ^ 30)) . fromInteger) (random (0, 2 ^ 30 - 1) seed)
	
	e					= 1.6021773e-19
	k					= 1.38066e-23
	dt'					= circa dt
	
	(initial', solve', integrate', correct_state', backdate')	= prepare circuit dt
	
	solve     (t, s, o, r) 			= (t, s, apply solve' s, r)
	integrate (t, s, o, r) 			= (t + dt, apply integrate' s, o, r)
	correct   (t, s, o, r)			= (t, apply correct_state' s, v_sub o (apply backdate' s), r)

	sources   (t, s, o, r)			= (t, random_offsets // map elements circuit, o, drop size r)
	  where
	 	elements (i, p, m, Conductor  _    )	= (i, 0)
		elements (i, p, m, Resistor   _    )	= (i, 0)
		elements (i, p, m, Capacitor  _    )	= (i, s ! i)
		elements (i, p, m, Inductor   _    )	= (i, s ! i)
		elements (i, p, m, Vsource    list )	= (i, source list)
		elements (i, p, m, Isource    list )	= (i, source list)
		elements (i, p, m, Junction   _    )	= (i, s ! i)
	
		random_offsets				= listArray (1, size) (map ((/ dt') . (* e). (* rbc) . (* 2) . (+ (-0.5))) r)
		(1, size)				= bounds s
	
		t'					= circa t

		source [(t0, v0)]					= v0
		source ((t0, v0) : _)			| t' < t0	= v0
		source ((t0, v0) : (t1, v1) : rest)	| t0 >= t1	= error "sources must have increasing times"
							| t' < t1	= v0 + (v1 - v0) * (t' - t0) / (t1 - t0)
							| otherwise	= source ((t1, v1) : rest)
						
	tunnel    (t, s, o, r)			= select (zip cumulative_probabilities new_states)
	  where
		e2					= e * e
		kT					= k * temp
	
		e0					= energy_state s
		energy_state  s				= sum (map (energy_state'  s  ) circuit)
		energy_source s o			= sum (map (energy_source' s o) circuit)
		
		events					= (concat . map events') circuit
		new_states				= map  (\(_,s,_,_) -> s) events
		delta_energies				= map ((\(r,s,o,_) -> (r, energy_state s - e0 + energy_source s o)) . correct) events
		probabilities				= limit_total (map (no_negative . probability) delta_energies)
		probability (r, de)	| r    == 0	= error "tunnel resistance must be nonzero (and should be >> 26k anyway)"
				 	| temp == 0	= dt' * (-de) / (e2 * circa r)
					| de   == 0	= dt' * kT / (e2 * circa r)
					| otherwise	= dt' * (-de) / (e2 * circa r * (1 - exp(de / kT)))
		no_negative p				= if p < 0 then 0 else p
		limit_total ps				= let total = sum ps in if total > 1 then map (/ total) ps else ps
		cumulative_probabilities		= tail (scanl (+) 0 probabilities)

		select []				= [(t, s, o, r')]
		select ((p', s') : zs)	| p < p'	= [(t, s, o, r'), (correct . solve) (t, s', o, r')]
					| otherwise	= select zs

		(p : r')				= r

		energy_state'  s   (i, p, m, Capacitor (c, _)   )	| c == 0	= error "capacitance must be nonzero"
									| otherwise	= 1/2 * (s ! i) ^ 2 / circa c
		energy_state'  s   (i, p, m, Junction  (c, _, _))	| c == 0	= error "junction capacitance must be nonzero"
									| otherwise	= 1/2 * (s ! i) ^ 2 / circa c
		energy_state'  _   _							= 0
		
		energy_source' s o (i, p, m, Vsource   _        )	= (s ! i) * (o ! i) * dt'
		energy_source' _ _ _					= 0

		events'        (i, p, m, Junction  (c, r, _))		= [(r, s // [(i, s ! i + e)], v_map (const 0) o, []),
									   (r, s // [(i, s ! i - e)], v_map (const 0) o, [])]             
		events'        _					= []

prepare :: Circuit -> Time -> (Vector Approx, Matrix Approx, Matrix Approx, Matrix Approx, Matrix Approx)
prepare circuit dt = (v_map circa initial, m_map circa solve, m_map circa integrate, m_map circa correct_state, m_map circa backdate)
  where
	elements				= matrix element
	derivative				= matrix deriv
	initial					= vector init

	(l1, u1, i1)				= gauss_jordan               elements
	(l2, u2, i2)				= gauss_jordan (m_transpose  elements)
	constraint				=                            m_select_rows l1 i1
	freedom					=               m_transpose (m_select_rows l2 i2)

	(l3, u3, i3)				= gauss_jordan (m_mul constraint (m_mul derivative freedom))
	inverse3		| l3 == []	= i3
				| otherwise	= error "illegal circuit"
	correct			| l1 == []	= m_zero size
				| otherwise	= m_mul freedom (m_mul inverse3 constraint)
	correct_state				= m_sub (m_unit size) (m_mul derivative correct   )
	correct_output				= m_sub (m_unit size) (m_mul correct    derivative)
	
	solve					= m_mul correct_output (m_mul i1 correct_state)

	scaled_derivative			= m_map (* (dt / 2)) (m_mul derivative solve)
	(l4, u4, i4)				= gauss_jordan (m_sub (m_unit size) scaled_derivative)
	integrate		| l4 == []	= m_mul i4     (m_add (m_unit size) scaled_derivative)
				| otherwise	= error "unlucky choice of dt - try a smaller one"

	backdate				= m_map (/ dt) correct
				
	matrix f				= accumArray (+) (0 % 1) ((1, 1), (size, size)) ((filter no_ground . concat . map f) circuit)
	vector f				= accumArray (+) (0 % 1) ( 1    ,  size       ) ((                   concat . map f) circuit)
	
  	element	(i, p, m, Conductor  g       )	= [((p, i), -1 % 1), ((m, i),  1 % 1), ((i, m), -g    ), ((i, p),  g    ), ((i, i), -1 % 1)]
  	element	(i, p, m, Resistor   r       )	= [((p, i), -1 % 1), ((m, i),  1 % 1), ((i, m), -1 % 1), ((i, p),  1 % 1), ((i, i), -r    )]
	element	(i, p, m, Capacitor (c, _)   )	= [((p, i), -1 % 1), ((m, i),  1 % 1), ((i, m), -c    ), ((i, p),  c    )]
	element	(i, p, m, Inductor  (l, _)   )	= [((p, i), -1 % 1), ((m, i),  1 % 1), ((i, i),  l    )]
	element	(i, p, m, Vsource    _       )	= [((p, i), -1 % 1), ((m, i),  1 % 1), ((i, m), -1 % 1), ((i, p),  1 % 1)]
	element	(i, p, m, Isource    _       )	= [((p, i), -1 % 1), ((m, i),  1 % 1), ((i, i),  1 % 1)]
	element	(i, p, m, Junction  (c, _, _))	= [((p, i), -1 % 1), ((m, i),  1 % 1), ((i, m), -c    ), ((i, p),  c    )]
	
  	deriv 	(i, p, m, Conductor  _       )	= []
  	deriv 	(i, p, m, Resistor   _       )	= []
	deriv 	(i, p, m, Capacitor (_, _)   )	= [((i, i),  1)]
	deriv 	(i, p, m, Inductor  (_, _)   )	= [((i, p),  1), ((i, m), -1)]
	deriv 	(i, p, m, Vsource    _       )	= []
	deriv 	(i, p, m, Isource    _       )	= []
	deriv 	(i, p, m, Junction  (_, _, _))	= [((i, i),  1)]
	
  	init	(i, p, m, Conductor  _       )	= []
  	init   	(i, p, m, Resistor   _       )	= []
	init    (i, p, m, Capacitor (_, q)   )	= [(i, q)]
	init   	(i, p, m, Inductor  (_, f)   )	= [(i, f)]
	init   	(i, p, m, Vsource    _       )	= []
	init   	(i, p, m, Isource    _       )	= []
	init   	(i, p, m, Junction  (_, _, q))	= [(i, q)]
	
	no_ground ((r, c), _)			= r /= 0 && c /= 0
	size					= maximum (concat (map (\(p, m, i, _) -> [p, m, i]) circuit))
