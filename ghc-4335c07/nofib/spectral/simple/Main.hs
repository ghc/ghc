--simple.hs--
-- The Simple code:
--  translated to haskell by an id to ph translator, hand annotated for toplevel
--  types, constant types and array strictness.  Toplevel comments were readded 
--  by hand.

-- ORIGINAL COMMENTS
--
-- %%% Id Example Program
-- %%% Title: SIMPLE (with higher order functions)
-- %%% Original Author: Kattamuri Ekanadham <EKNATH@ibm.com>

-- %%% Author's Comments:
-- % Date: 1 Nov 89 13:33:44 EST
-- % From: Kattamuri Ekanadham <EKNATH@ibm.com>
-- % Subject: simple89
-- % To:   jamey%au-bon-pain.lcs.mit.edu@relay.cs.net

-- % RPaul:
-- % I compared the equations in CSG 273 with this code for the following
-- % computations: velocity, position, area/volume/density , artificial-viscosity
-- %   and heat-conduction.
-- %  - I fixed the area computation in zone_area. (equation 24)
-- %  - both equation 24 and the code are still missing the constant 
-- %   factor of 2pi in the volume computation
-- %  - the code for artifical viscocity (equation 27) were missing upper_disc^2
-- %   and lower_disc^2
-- %  - the code for interior_cc (equation 37) was missing theta_hat^5/2
-- %  - the initial condition for heat is now 10.0 (was .0001)
-- %  
-- % --------------
-- % Jamey:
-- % I finally re-constructed the scenario that worked some time ago.
-- % The following is a program with all defs and built in constant of
-- % size = 10. Try this. This should run well.
-- % If so, you may try replacing defs by defsubsts and try.
-- % -eknath.
-- % --------------
-- % eknath : 9/15/89 : the following code is taken directly from the simple paper
-- % SIMPLE89-DEFS-OPEN : This has no defsubsts and all constants are in line.
-- % The last function simple is invoked with just the number of iterations.
-- % Ideally what we want is to wrap this whole thing into the function simple and
-- % make grid-max as the second argument for it.


module Main where
import Data.Array
import Data.Ix
infixr 1 =:
type Assoc a b = (a,b)
(=:) = (,)

main = getContents >>= \ userInput -> runSimple (lines userInput)

runSimple :: [String] -> IO ()
runSimple inputLines = 
  readInt "Run_simple :  " inputLines >>= \ num1 ->
  if num1 >= 0  then reportSimple num1 else reportSimple2 (- num1)
	  
readInt:: String -> [String] -> IO Int
readInt prompt inputLines =
      putStr prompt >>
      (case inputLines of 
	 (l1:rest) -> case (reads l1) of
		       [(x,"")] -> return x
		       _ -> error "Error"
	 _ -> error "Eof Error")

reportSimple :: Int -> IO ()
reportSimple iterations =
  putStrLn
    (mix "\n" (let {(u,v,r,z,alpha,s,rho,p,q,epsilon,theta,deltat,err) = simple_total iterations}
			in [show "RESULT u,v,r,z,alpha,s,rho,p,q,epsilon,theta,deltat err",
			    show "<" ++ show u ++ "," ++ show v ++ "," ++ show r ++ "," ++ show z ++ ","
			    ++ show alpha ++ "," ++ show s ++ "," ++ show rho ++ "," ++ show p ++ ","
			    ++ show q ++ ","++ show epsilon ++ ","++ show theta ++ ","
			    ++ show deltat ++ ","++ show err ++ ">" ])
		     ++ "\n" ++ "done")


reportSimple2 :: Int -> IO ()
reportSimple2 iterations =
  putStrLn
    (mix "\n" (let {((mat0,mat1),(mat2,mat3),mat4,mat5,mat6,mat7,mat8,mat9,mat10,a,b) = simple iterations}
			in [show "RESULT ",show "U",show mat0,show "V",show mat1,show "R",
			    show mat2,show "Z", show mat3,show "Alpha", show mat4,show "S",
			    show mat5,show "Rho", show mat6,show "P",
			    show mat7,show "Q", show mat8,show "epsilon", show mat9,"theta",
			    show mat10,show a, show b])
		     ++ "\n" ++ "done")



mix s [] = []
mix s [x] = x
mix s (x:xs) = x ++ s ++ mix s xs



-- pH prelude functions
type Zone = (Int,Int)
type DblArray = (Array Zone Double)
type DblVector = (Array (Int) Double)
type ArrayDim = (Zone,Zone)
type VectorDim = Zone
type State = ((DblArray, DblArray), (DblArray, DblArray), DblArray, DblArray, DblArray, DblArray, DblArray, DblArray, DblArray, Double, Double)
type Totals = (Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)


arrays_2 :: (Ix a) => (a, a) -> [Assoc a (b, c)] -> (Array a b, Array a c)
arrays_2 b ivs = ((array b (array_unzip ivs fst)),
                  (array b (array_unzip ivs snd)))

arrays_3 :: (Ix a) => (a, a) -> [Assoc a (b, c, d)] -> (Array a b, Array a c, Array a d)
arrays_3 b ivs = let {first (x,y,z) = x;
		      second (x,y,z) = y;
		      third  (x,y,z) = z}
  		   in (array b (array_unzip ivs first),
	               array b (array_unzip ivs second),	
                       array b (array_unzip ivs third))

-- Strict Arrays, force evaluation of the elements.
strictArray resultArray = seq (map evalValue (elems resultArray)) resultArray
	where evalValue v = seq v v

strictArrays_2 (a1, a2) = (strictArray a1, strictArray a2)
strictArrays_3 (a1, a2, a3) = (strictArray a1, strictArray a2, strictArray a3)




array_unzip :: [Assoc a b] -> (b -> c) -> [Assoc a c]
array_unzip ivs sel = [ i =: (sel vs) | (i, vs) <- ivs]

pHbounds :: ((a, b), (c, d)) -> ((a, c), (b, d))
pHbounds ((x_low,x_high),(y_low,y_high)) = ((x_low,y_low),(x_high,y_high))
--- end pH prelude
--- end of driver
---
---
--- TRANSLATED CODE
---
{- # INCLUDE "list-library"# -}

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %% Simple (toplevel function)
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simple :: Int -> State
simple step_count = let {
 			 (totals,state) = simple_loop (compute_initial_state ()) step_count
			} in state

simple_total :: Int -> Totals
simple_total step_count = 
			let {
			     (totals,state) = simple_loop (compute_initial_state ()) step_count
			    } in totals

simple_loop ::  (Totals,State) -> Int -> (Totals,State)
simple_loop result@(totals,state) step = 
				if step < 1 then result else 
	                           seq (total_total totals) (simple_loop (compute_next_state state))
	                              (step - (1::Int))

total_total (v1, v2, x1, x2, alpha, s, rho, 
                p, q, epsilon, theta, deltat, c) =
   v1 + v2 + x1 + x2 + alpha + s + rho + 
                p + q + epsilon + theta + deltat + c
	
total :: (Ix a) => (Array a Double) -> Double
total a = foldl (+) (0.0 :: Double) (elems a)

total_state :: State -> Totals
total_state state = 
   let {
       ((v1, v2), (x1, x2), alpha, s, rho, p, q, epsilon, theta, deltat, c) = 
          state ;
       v1' = total v1 ;
       v2' = total v2 ;
       x1' = total x1 ;
       x2' = total x2 ;
       alpha' = total alpha ;
       s' = total s ;
       rho' = total rho ;
       p' = total p ;
       q' = total q ;
       epsilon' = total epsilon ;
       theta' = total theta ;
       state' = (v1', v2', x1', x2', alpha', s', rho', 
                p', q', epsilon', theta', deltat, c)
       } in state'

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %% Compute_next_state
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compute_next_state :: State -> (Totals,State)
compute_next_state state = 
   let {
       state' = (v', x', alpha', s', rho', p', q', 
                epsilon', theta', deltat', c') ;
       (v, x, alpha, s, rho, p, q, epsilon, theta, deltat, c) = 
          state ;
       v' = make_velocity v x p q alpha rho deltat ;
       x' = make_position x deltat v' ;
       (alpha', s', rho') = make_area_density_volume 
                               rho s x' ;
       (q', d) = make_viscosity p v' x' alpha' rho' ;
       theta_hat = make_temperature p epsilon rho theta 
                      rho' q' ;
       p' = make_pressure rho' theta' ;
       epsilon' = make_energy rho' theta' ;
       (theta', gamma_k, gamma_l) = 
          compute_heat_conduction theta_hat deltat x' alpha' 
                                       rho' ;
       c' = compute_energy_error v' x' p' q' epsilon' 
               theta' rho' alpha' gamma_k gamma_l deltat ;
       (deltat', delta_t_c, delta_t_h) = 
          compute_time_step d theta_hat theta'
       } in (total_state state', state')

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %% Compute_initial_state
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compute_initial_state :: a -> (Totals,State)
compute_initial_state _ = 
   let {
       state = (v, x, alpha, s, rho, p, q, epsilon, 
               theta, deltat, c) ;
       v = (all_zero_nodes (), all_zero_nodes ()) ;
       x = 
          let {
              interior_position (k, l) = 
                 let {
                     rp = fromIntegral (lmax - lmin) ;
                     z1 = fromIntegral ((10 + k) - kmin) ;
                     zz = ((- (0.5::Double)) + (fromIntegral (l - lmin)) 
                          / rp) * (pi::Double)
                     } in ((z1::Double) * (cos (zz::Double)), z1 * (sin zz))
              } in make_position_matrix interior_position ;
       (alpha, s) = 
          let {
              reflect_area_vol reflect_function = 
                 (reflect_function alpha', reflect_function s') ;
              (alpha', s') = (arrays_2 (pHbounds dimension_all_zones)
                              ([zone=: zone_area_vol x zone
                                   | zone <- interior_zones]++
                               [zone=: reflect_area_vol (reflect_south zone)
                                   | zone <- north_zones]++
                               [zone=: reflect_area_vol (reflect_north zone)
                                   | zone <- south_zones]++
                               [zone=: reflect_area_vol (reflect_west zone)
                                   | zone <- east_zones]++
                               [zone=: reflect_area_vol (reflect_east zone)
                                   | zone <- west_zones]))
              } in strictArrays_2 (alpha', s') ;
       rho = strictArray (array (pHbounds dimension_all_zones)
              ([zone=: (1.4::Double) | zone <- all_zones])) ;
       p = make_pressure rho theta ;
       q = all_zero_zones () ;
       epsilon = make_energy rho theta ;
       theta = strictArray (array (pHbounds dimension_all_zones)
                ([zone=: (10.0::Double) | zone <- interior_zones]++
                 [zone=: constant_heat_source
                     | zone <- north_zones]++
                 [zone=: constant_heat_source
                     | zone <- south_zones]++
                 [zone=: constant_heat_source
                     | zone <- east_zones]++
                 [zone=: constant_heat_source
                     | zone <- west_zones])) ;
       deltat = (0.01::Double) ;
       c = (0.0 :: Double)
       } in (total_state state, state)

line_integral :: DblArray -> DblArray -> (Int,Int) -> Double
line_integral p z node = 
          (((p!zone_a node) * ((z!west node) - (z!north node)) 
                                 + (p!zone_b node) * ((z!south node) 
                                 - (z!west node))) + (p!zone_c node) 
                                * ((z!east node) - (z!south node))) 
                                + (p!zone_d node) * ((z!north node) 
                                - (z!east node)) 

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %% Make_velocity
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


make_velocity :: (DblArray, DblArray) -> (DblArray, DblArray) -> DblArray -> DblArray -> (DblArray) -> (DblArray) -> Double -> (DblArray, DblArray)
make_velocity (u, w) (r, z) p q alpha rho deltat = 
   
   let {
       regional_mass node = (0.5::Double) * ((((rho!zone_a node) * (alpha!zone_a node) 
                                    + (rho!zone_b node) * (alpha!zone_b node)) 
                                   + (rho!zone_c node) * (alpha!zone_c node)) 
                            + (rho!zone_d node) * (alpha!zone_d node)) ;
       velocity node = 
          let {
              d = regional_mass node ;
              n1 = - (line_integral (p :: DblArray) (z :: DblArray) (node :: (Int,Int)))
                   - line_integral q z node ;
              n2 = line_integral (p :: DblArray) (r :: DblArray) (node :: (Int,Int)) 
                   + line_integral (q :: DblArray) r node ;
              u_dot = (n1 :: Double) / (d :: Double) ;
              w_dot = (n2 :: Double) / (d :: Double)
              } in ((u!node) + deltat * u_dot, 
                   (w!node) + deltat * w_dot)
       } in strictArrays_2 (arrays_2 (pHbounds dimension_interior_nodes)
             ([node=: velocity node | node <- interior_nodes]))

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %% Make_position
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_position :: (DblArray, DblArray) -> Double -> (Array (Int,Int) Double, DblArray) -> (DblArray, DblArray)
make_position (r, z) deltat (u', w') = 
   let {
       interior_position node = 
          ((r!node) + deltat * (u'!node), (z!node) + deltat * (w'!node))
       } in make_position_matrix interior_position

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %% Make_area_density_volume
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_area_density_volume :: (DblArray) -> (DblArray) -> (DblArray, DblArray) -> (DblArray, DblArray, DblArray)
make_area_density_volume rho s x' = 
   let {
       interior_area zone = 
          let {
              (area, vol) = zone_area_vol x' zone ;
              density = ((rho!zone) * (s!zone)) / vol
              } in (area, vol, density) ;
       reflect_area_vol_density reflect_function = 
          (reflect_function alpha', 
          reflect_function s', reflect_function rho') ;
       (alpha', s', rho') = (arrays_3 (pHbounds dimension_all_zones)
                             ([zone=: interior_area zone
                                  | zone <- interior_zones]++
                              [zone=: reflect_area_vol_density 
                                         (reflect_south zone)
                                  | zone <- north_zones]++
                              [zone=: reflect_area_vol_density 
                                         (reflect_north zone)
                                  | zone <- south_zones]++
                              [zone=: reflect_area_vol_density 
                                         (reflect_west zone)
                                  | zone <- east_zones]++
                              [zone=: reflect_area_vol_density 
                                         (reflect_east zone)
                                  | zone <- west_zones]))
       } in strictArrays_3 (alpha', s', rho')

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %% Make_viscosity
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


c0 = 1.22474487

c1 = 0.50

make_viscosity :: (DblArray) -> (DblArray, DblArray) -> (DblArray, DblArray) -> (DblArray) -> (DblArray) -> (DblArray, DblArray)
make_viscosity p (u', w') (r', z') alpha' rho' = 
   let {
       interior_viscosity zone = 
          let {
              upper_del f = (0.5::Double) * (((f!zone_corner_southeast zone) 
                                   - (f!zone_corner_northeast zone)) 
                            + (f!zone_corner_southwest zone) 
                            - (f!zone_corner_northwest zone)) ;
              lower_del f = (0.5::Double) * (((f!zone_corner_southeast zone) 
                                   - (f!zone_corner_southwest zone)) 
                            + (f!zone_corner_northeast zone) 
                            - (f!zone_corner_northwest zone)) ;
              xi = (upper_del r') ^ (2::Int) + (upper_del z') 
                   ^ (2::Int) ;
              eta = (lower_del r') ^ (2::Int) + (lower_del z') 
                    ^ (2::Int) ;
              upper_disc = (upper_del r') * (lower_del w') 
                           - (upper_del z') * (lower_del u') ;
              lower_disc = (upper_del u') * (lower_del z') 
                           - (upper_del w') * (lower_del r') ;
              upper_ubar = if upper_disc < (0.0 :: Double) then 
                             upper_disc ^ (2::Int) / xi else 
                             (0.0 :: Double) ;
              lower_ubar = if lower_disc < (0.0 :: Double) then 
                             lower_disc ^ (2::Int) / eta else 
                             (0.0 :: Double) ;
              gamma = 1.6 ;
              speed_of_sound = (gamma * (p!zone)) / (rho'!zone) ;
              ubar = upper_ubar + lower_ubar ;
              viscosity = (rho'!zone) * ((c0 ^ (2::Int) / 4.0) * ubar 
                          + ((c1 / (2.0::Double)) * speed_of_sound) 
                          * (sqrt ubar)) ;
              length = sqrt ((upper_del r') ^ (2::Int) + (lower_del r') 
                            ^ (2::Int)) ;
              courant_delta = ((0.5::Double) * (alpha'!zone)) 
                              / speed_of_sound * length
              } in (viscosity, courant_delta) ;
       reflect_viscosity_cdelta direction zone = 
          ((q'!direction zone) * (qb!(nbc!zone)), (0.0 :: Double)) ;
       (q', d) = (arrays_2 (pHbounds dimension_all_zones)
                  ([zone=: interior_viscosity zone
                       | zone <- interior_zones]++
                   [zone=: reflect_viscosity_cdelta 
                              south zone | zone <- north_zones]++
                   [zone=: reflect_viscosity_cdelta 
                              north zone | zone <- south_zones]++
                   [zone=: reflect_viscosity_cdelta 
                              west zone | zone <- east_zones]++
                   [zone=: reflect_viscosity_cdelta 
                              east zone | zone <- west_zones]))
       } in strictArrays_2 (q', d)

polynomial ::  (Array (Int, Int) (DblArray)) -> Int -> (Array Int Double) -> (Array Int Double) -> Double -> Double -> Double
polynomial g degree rho_table theta_table rho_value theta_value = 
   
   let {
       table_search table value = 
          let {
              (low, high) = bounds table ;
              search_down i = if value > (table!(i - 1)) then 
                                i else search_down (i - 1)
              } in if value > (table!high) then 
                     high + 1 else if value <= (table!low) then 
                                     low else search_down high ;
       rho_index = table_search rho_table rho_value ;
       theta_index = table_search theta_table theta_value ;
       a = g!(rho_index, theta_index)
       } in sum_list [ ((a!(i, j)) * rho_value ^ (i::Int)) 
                       * theta_value ^ j | i <- [0..degree],j <- 
                                           [0..degree] ]

zonal_pressure :: Double -> Double -> Double
zonal_pressure rho_value theta_value = 
   let {
       (g, degree, rho_table, theta_table) = extract_pressure_tables_from_constants
       } in polynomial g degree rho_table theta_table 
               rho_value theta_value

zonal_energy :: Double -> Double -> Double
zonal_energy rho_value theta_value = 
   let {
       (g, degree, rho_table, theta_table) = extract_energy_tables_from_constants
       } in polynomial g degree rho_table theta_table 
               rho_value theta_value

dx :: Double
dx = 1.0E-6 ::Double

tiny :: Double
tiny = 1.0E-6 ::Double

newton_raphson :: (Double -> Double) -> Double -> Double
newton_raphson f x = newton_raphson_loop f x (f x)

newton_raphson_loop :: (Double -> Double) -> Double -> Double -> Double
newton_raphson_loop f x fx = if fx > tiny then 
                               let {
                                   fxdx = f (x + dx) ;
                                   denom = fxdx - fx ;
                                   (next_x, next_fx) = if denom < tiny then (x,
 tiny) 
                                                       else (x - (fx * dx) / denom, 
                                                            fxdx)
                                   } in newton_raphson_loop f next_x 
                                           next_fx 
                             else x

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %% Make_temperature
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_temperature :: (DblArray) -> (DblArray) -> (DblArray) -> (DblArray) -> (DblArray) -> (DblArray) -> DblArray
make_temperature p epsilon rho theta rho' q' = 
   let {
       interior_temperature zone = 
          let {
              qkl = q'!zone ;
              rho_kl = rho!zone ;
              rho'_kl = rho'!zone ;
              tau_kl = 1.0 / rho'_kl - 1.0 / rho_kl ;
              energy_equation epsilon_kl theta_kl = 
                 epsilon_kl - zonal_energy rho_kl theta_kl ;
              revised_energy pkl = epsilon_0 - (pkl + qkl) 
                                   * tau_kl ;
              revised_temperature epsilon_kl theta_kl = 
                 newton_raphson (energy_equation epsilon_kl) 
                    theta_kl ;
              revised_pressure theta_kl = 
                 zonal_pressure rho_kl theta_kl ;
              epsilon_0 = epsilon!zone ;
              p_0 = p!zone ;
              theta_0 = theta!zone ;
              epsilon_1 = revised_energy p_0 ;
              theta_1 = revised_temperature epsilon_1 
                           theta_0 ;
              p_1 = revised_pressure theta_1 ;
              epsilon_2 = revised_energy p_1 ;
              theta_2 = revised_temperature epsilon_2 
                           theta_1
              } in theta_2
       } in strictArray (array (pHbounds dimension_all_zones)
             ([zone=: interior_temperature zone
                  | zone <- interior_zones]++
              [zone=: constant_heat_source
                  | zone <- north_zones]++
              [zone=: constant_heat_source
                  | zone <- south_zones]++
              [zone=: constant_heat_source
                  | zone <- east_zones]++
              [zone=: constant_heat_source
                  | zone <- west_zones]))

make_cc :: (DblArray) -> (DblArray) -> DblArray
make_cc alpha' theta_hat = 
   let {
       interior_cc zone = ((0.0001::Double) * ((**) (theta_hat!zone) (2.5::Double))) 
                          / (alpha'!zone) ;
       cc = (array (pHbounds dimension_all_zones)
             ([zone=: interior_cc zone | zone <- interior_zones]++
              [zone=: reflect_south zone cc
                  | zone <- north_zones]++
              [zone=: reflect_north zone cc
                  | zone <- south_zones]++
              [zone=: reflect_west zone cc
                  | zone <- east_zones]++
              [zone=: reflect_east zone cc
                  | zone <- west_zones]))
       } in strictArray cc

make_sigma :: Double -> (DblArray) -> (DblArray) -> Array (Int,Int) Double
make_sigma deltat rho' alpha' = 
   let {
       interior_sigma zone = (((rho'!zone) * (alpha'!zone)) 
                             * specific_heat) / deltat
       } in strictArray (array (pHbounds dimension_interior_zones)
             ([zone=: interior_sigma zone
                  | zone <- interior_zones]))

make_gamma :: (DblArray, DblArray) -> (DblArray)-> ((Int, Int) -> (Int, Int)) -> ((Int, Int) -> (Int, Int)) -> DblArray
make_gamma (r', z') cc succeeding adjacent = 
   let {
       interior_gamma zone = 
          let {
              r1 = r'!zone_corner_southeast zone ;
              z1 = z'!zone_corner_southeast zone ;
              r2 = r'!zone_corner_southeast (adjacent zone) ;
              z2 = z'!zone_corner_southeast (adjacent zone) ;
              cross_section = ((0.5::Double) * (r1 + r2)) * ((r1 - r2) ^ (2::Int) 
                              + (z1 - z2) ^ (2::Int)) ;
              (c1, c2) = (cc!zone, cc!succeeding zone) ;
              specific_conductivity = 
                 (((2.0::Double) * c1) * c2) / (c1 + c2)
              } in cross_section * specific_conductivity
       } in strictArray (array (pHbounds dimension_all_zones)
             ([zone=: interior_gamma zone
                  | zone <- interior_zones]++
              [zone=: (0.0 :: Double) | zone <- north_zones]++
              [zone=: (0.0 :: Double) | zone <- south_zones]++
              [zone=: (0.0 :: Double) | zone <- east_zones]++
              [zone=: (0.0 :: Double) | zone <- west_zones]))

make_ab :: (DblArray) -> (DblArray) -> (DblArray) -> ((Int, Int) -> (Int, Int))
 -> (DblArray, DblArray)
make_ab theta sigma gamma preceding = 
   let {
       interior_ab zone = 
          let {
              denom = ((sigma!zone) + (gamma!zone)) 
                      + (gamma!preceding zone) * ((1.0 ::Double)
                      - (a!preceding zone)) ;
              nume1 = gamma!zone ;
              nume2 = (gamma!preceding zone) 
                      * (b!preceding zone) + (sigma!zone) 
                      * (theta!zone);
	      result1 = nume1 / denom;
              result2 = nume2 / denom
              } in seq (result1 + result2) (result1,result2) ;
       (a, b) = (arrays_2 (pHbounds dimension_all_zones)
                 ([zone=: interior_ab zone
                      | zone <- interior_zones]++
                  [zone=: ((0.0 :: Double), theta!zone)
                      | zone <- north_zones]++
                  [zone=: ((0.0 :: Double), theta!zone)
                      | zone <- south_zones]++
                  [zone=: ((0.0 :: Double), theta!zone)
                      | zone <- east_zones]++
                  [zone=: ((0.0 :: Double), theta!zone)
                      | zone <- west_zones]))
       } in strictArrays_2 (a, b)

make_theta :: (DblArray) -> (DblArray) -> ((Int, Int) -> (Int, Int)) -> [(Int, Int)] -> DblArray
make_theta a b succeeding int_zones = 
   let {
       interior_theta zone = (a!zone) * (theta!succeeding zone) 
                             + (b!zone) ;
       theta = (array (pHbounds dimension_all_zones)
                ([zone=: interior_theta zone
                     | zone <- int_zones]++
                 [zone=: constant_heat_source
                     | zone <- north_zones]++
                 [zone=: constant_heat_source
                     | zone <- south_zones]++
                 [zone=: constant_heat_source
                     | zone <- east_zones]++
                 [zone=: constant_heat_source
                     | zone <- west_zones]))
       } in strictArray theta

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %% Make_pressure
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_pressure :: (DblArray) -> (DblArray) -> DblArray
make_pressure rho' theta' = 
   let {
       boundary_p direction zone = 
          (pbb!(nbc!zone)) + (pb!(nbc!zone)) * (p!direction zone) ;
       p = (array (pHbounds dimension_all_zones)
            ([zone=: zonal_pressure (rho'!zone) (theta'!zone)
                 | zone <- interior_zones]++
             [zone=: boundary_p south zone
                 | zone <- north_zones]++
             [zone=: boundary_p north zone
                 | zone <- south_zones]++
             [zone=: boundary_p west zone
                 | zone <- east_zones]++
             [zone=: boundary_p east zone
                 | zone <- west_zones]))
       } in strictArray p

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %% Make_energy
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_energy :: (DblArray) -> (DblArray) -> DblArray
make_energy rho' theta' = 
   let {
       epsilon' = (array (pHbounds dimension_all_zones)
                   ([zone=: zonal_energy (rho'!zone) 
                               (theta'!zone)
                        | zone <- interior_zones]++
                    [zone=: reflect_south zone epsilon'
                        | zone <- north_zones]++
                    [zone=: reflect_north zone epsilon'
                        | zone <- south_zones]++
                    [zone=: reflect_west zone epsilon'
                        | zone <- east_zones]++
                    [zone=: reflect_east zone epsilon'
                        | zone <- west_zones]))
       } in strictArray epsilon'

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %% Compute_heat_conduction
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compute_heat_conduction :: (DblArray) -> Double -> (DblArray, DblArray) -> (DblArray) -> (DblArray) -> (DblArray, DblArray, DblArray)
compute_heat_conduction theta_hat deltat x' alpha' rho' = 
   
   let {
       sigma = make_sigma deltat rho' alpha' ;
       cc = make_cc alpha' theta_hat ;
       gamma_k = make_gamma x' cc north east ;
       (a_k, b_k) = make_ab theta_hat sigma gamma_k 
                       north ;
       theta_k = make_theta a_k b_k south north_ward_interior_zones ;
       gamma_l = make_gamma x' cc west south ;
       (a_l, b_l) = make_ab theta_k sigma gamma_l west ;
       theta_l = make_theta a_l b_l east west_ward_interior_zones
       } in (theta_l, gamma_k, gamma_l)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %% Compute_energy_error
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compute_energy_error :: (DblArray, DblArray) -> (DblArray, DblArray) -> (DblArray) -> (DblArray) -> (DblArray) -> (DblArray) -> (DblArray) -> (DblArray) -> (DblArray) -> (DblArray) -> Double -> Double
compute_energy_error (u', w') (r', z') p' q' epsilon' theta' rho' alpha' gamma_k gamma_l deltat = 
   
   let {
       mass zone = (rho'!zone) * (alpha'!zone) ;
       internal_energy = sum_list [ (epsilon'!zone) * (mass zone) 
                                     | zone <- interior_zones ] ;
       kinetic node = 
          let {
              average_mass = (0.25::Double) * (((mass (zone_a node) + mass (zone_b node)) 
                                     + mass (zone_c node)) 
                             + mass (zone_d node)) ;
              v_square = (u'!node) ^ (2::Int) + (w'!node) 
                         ^ (2::Int)
              } in ((0.5::Double) * average_mass) * v_square ;
       kinetic_energy = sum_list [ kinetic node | node <- interior_nodes ] ;
       work_done (node1, node2) = 
          let {
              (r1, r2) = (r'!node1, r'!node2) ;
              (z1, z2) = (z'!node1, z'!node2) ;
              (u1, u2) = (p'!node1, p'!node2) ;
              (w1, w2) = (z'!node1, z'!node2) ;
              (p1, p2) = (p'!node1, p'!node2) ;
              (q1, q2) = (q'!node1, q'!node2) ;
              force = (0.5::Double) * (((p1 + p2) + q1) + q2) ;
              radius = (0.5::Double) * (r1 + r2) ;
              area = (0.5::Double) * ((r1 - r2) * (u1 - u2) 
                     - (z1 - z2) * (w1 - w2))
              } in ((force * radius) * area) * deltat ;
       north_line = [ ((i, j), (k, l)) | k <- [kmin],l <- [lmin + 1..lmax],
                                         (i, j) <- [west (k, l)] ] ;
       south_line = [ ((i, j), (k, l)) | k <- [kmax],l <- [lmin + 1..lmax],
                                         (i, j) <- [west (k, l)] ] ;
       east_line = [ ((i, j), (k, l)) | l <- [lmax],k <- [kmin + 1..kmax],
                                        (i, j) <- [south (k, l)] ] ;
       west_line = [ ((i, j), (k, l)) | l <- [lmin + 1],k <- 
                                        [kmin + 1..kmax],(i, j) <- 
                                        [south (k, l)] ] ;
       w1 = sum_list [ work_done segment | segment <- north_line ] ;
       w2 = sum_list [ work_done segment | segment <- south_line ] ;
       w3 = sum_list [ work_done segment | segment <- east_line ] ;
       w4 = sum_list [ work_done segment | segment <- west_line ] ;
       boundary_work = ((w1 + w2) + w3) + w4 ;
       heat_flow gamma (zone1, zone2) = 
          (deltat * (gamma!zone1)) * ((theta'!zone1) - (theta'!zone2)) ;
       north_flow = [ ((i, j), (k, l)) | k <- [kmin + 1],l <- 
                                         [lmin + 1..lmax],(i, j) <- 
                                         [north (k, l)] ] ;
       south_flow = [ ((i, j), (k, l)) | k <- [kmax],l <- [lmin + 2..lmax - 1],
                                         (i, j) <- [south (k, l)] ] ;
       east_flow = [ ((i, j), (k, l)) | l <- [lmax],k <- [kmin + 2..kmax],
                                        (i, j) <- [east (k, l)] ] ;
       west_flow = [ ((i, j), (k, l)) | l <- [lmin + 1],k <- 
                                        [kmin + 2..kmax],(i, j) <- 
                                        [west (k, l)] ] ;
       h1 = sum_list [ heat_flow gamma_k segment 
                        | segment <- north_flow ] ;
       h2 = sum_list [ heat_flow gamma_k segment 
                        | segment <- south_flow ] ;
       h3 = sum_list [ heat_flow gamma_l segment 
                        | segment <- east_flow ] ;
       h4 = sum_list [ heat_flow gamma_l segment 
                        | segment <- west_flow ] ;
       boundary_heat = ((h1 + h2) + h3) + h4
       } in ((internal_energy + kinetic_energy) - boundary_heat) 
            - boundary_work

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %% Compute_time_step
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compute_time_step :: (DblArray) -> (DblArray) -> (DblArray) -> (Double, Double, Double)
compute_time_step d theta_hat theta' = 
   let {
       deltat_courant = min_list [ d!zone | zone <- interior_zones ] ;
       deltat_conduct = max_list [ (abs ((theta_hat!zone) - (theta'!zone))) 
                                   / (theta_hat!zone) | zone <- interior_zones ] ;
       deltat_minimum = min deltat_courant deltat_conduct
       } in (min deltat_maximum deltat_minimum, deltat_courant, 
            deltat_conduct)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %% Geometry
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

north :: Zone -> Zone
north (k, l) = (k - 1, l)

south :: Zone -> Zone
south (k, l) = (k + 1, l)

east :: Zone -> Zone
east (k, l) = (k, l + 1)

west :: Zone -> Zone
west (k, l) = (k, l - 1)

northeast :: Zone -> Zone
northeast node = north (east node)

southeast :: Zone -> Zone
southeast node = south (east node)

northwest :: Zone -> Zone
northwest node = north (west node)

southwest :: Zone -> Zone
southwest node = south (west node)

farnorth :: Zone -> Zone
farnorth node = north (north node)

farsouth :: Zone -> Zone
farsouth node = south (south node)

fareast :: Zone -> Zone
fareast node = east (east node)

farwest :: Zone -> Zone
farwest node = west (west node)

zone_a :: Zone -> Zone
zone_a (k, l) = (k, l)

zone_b :: Zone -> Zone
zone_b (k, l) = (k + 1, l)

zone_c :: Zone -> Zone
zone_c (k, l) = (k + 1, l + 1)

zone_d :: Zone -> Zone
zone_d (k, l) = (k, l + 1)

zone_corner_northeast :: Zone -> Zone
zone_corner_northeast zone = north zone

zone_corner_northwest :: Zone -> Zone
zone_corner_northwest zone = northwest zone

zone_corner_southeast :: Zone -> Zone
zone_corner_southeast zone = zone

zone_corner_southwest :: Zone -> Zone
zone_corner_southwest zone = west zone

((kmin, kmax), (lmin, lmax)) = grid_size :: ArrayDim

dimension_all_nodes = ((kmin - 1, kmax + 1), 
                      (lmin - 1, lmax + 1)) :: ArrayDim

all_nodes :: [(Int, Int)]
all_nodes = [ (k, l) | k <- [kmin - 1..kmax + 1],l <- 
                       [lmin - 1..lmax + 1] ]

dimension_interior_nodes :: ArrayDim
dimension_interior_nodes = ((kmin, kmax), (lmin, lmax))

interior_nodes :: [(Int, Int)]
interior_nodes = [ (k, l) | k <- [kmin..kmax],l <- 
                            [lmin..lmax] ]

dimension_all_zones :: ArrayDim
dimension_all_zones = ((kmin, kmax + 1), (lmin, lmax + 1))

all_zones :: [(Int, Int)]
all_zones = [ (k, l) | k <- [kmin..kmax + 1],l <- 
                       [lmin..lmax + 1] ]

dimension_interior_zones :: ArrayDim
dimension_interior_zones = 
   ((kmin + 1, kmax), (lmin + 1, lmax))

interior_zones :: [(Int, Int)]
interior_zones = [ (k, l) | k <- [kmin + 1..kmax],l <- 
                            [lmin + 1..lmax] ]

north_ward_interior_zones :: [(Int, Int)]
north_ward_interior_zones = 
   [ (k, l) | k <- [kmax,(kmax-1)..kmin + 1],l <- [lmin + 1..lmax] ]

west_ward_interior_zones :: [(Int, Int)]
west_ward_interior_zones = 
   [ (k, l) | k <- [kmin + 1..kmax],l <- [lmax,(lmax-1)..lmin + 1] ]

north_zones :: [(Int, Int)]
north_zones = [ (k, l) | k <- [kmin],l <- [lmin..lmax + 1] ]

south_zones :: [(Int, Int)]
south_zones = [ (k, l) | k <- [kmax + 1],l <- 
                         [lmin + 1..lmax] ]

east_zones :: [(Int, Int)]
east_zones = [ (k, l) | k <- [kmin + 1..kmax + 1],l <- 
                        [lmax + 1] ]

west_zones :: [(Int, Int)]
west_zones = [ (k, l) | k <- [kmin + 1..kmax + 1],l <- 
                        [lmin] ]

reflect :: ((Int, Int) -> (Int, Int)) -> (Int, Int) -> DblArray -> Double
reflect dir node a = a!dir node

reflect_north :: (Int, Int) -> (Array (Int, Int) Double) -> Double
reflect_north = reflect north

reflect_south :: (Int, Int) -> (Array (Int, Int) Double) -> Double
reflect_south = reflect south

reflect_east :: (Int, Int) -> (Array (Int, Int) Double) -> Double
reflect_east = reflect east

reflect_west :: (Int, Int) -> (Array (Int, Int) Double) -> Double
reflect_west = reflect west

north_nodes :: [(Int, Int)]
north_nodes = [ (k, l) | k <- [kmin - 1],l <- 
                         [lmin..lmax - 1] ]

south_nodes :: [(Int, Int)]
south_nodes = [ (k, l) | k <- [kmax + 1],l <- 
                         [lmin..lmax - 1] ]

east_nodes :: [(Int, Int)]
east_nodes = [ (k, l) | k <- [kmin..kmax - 1],l <- 
                        [lmax + 1] ]

west_nodes :: [(Int, Int)]
west_nodes = [ (k, l) | k <- [kmin..kmax - 1],l <- 
                        [lmin - 1] ]

north_east_corner :: [(Int, Int)]
north_east_corner = [ (k, l) | k <- [kmin - 1],l <- 
                               [lmax + 1] ]

north_west_corner :: [(Int, Int)]
north_west_corner = [ (k, l) | k <- [kmin - 1],l <- 
                               [lmin - 1] ]

south_east_corner :: [(Int, Int)]
south_east_corner = [ (k, l) | k <- [kmax + 1],l <- 
                               [lmax + 1] ]

south_west_corner :: [(Int, Int)]
south_west_corner = [ (k, l) | k <- [kmax + 1],l <- 
                               [lmin - 1] ]

west_of_north_east :: [(Int, Int)]
west_of_north_east = [ (k, l) | k <- [kmin - 1],l <- 
                                [lmax] ]

west_of_south_east :: [(Int, Int)]
west_of_south_east = [ (k, l) | k <- [kmax + 1],l <- 
                                [lmax] ]

north_of_south_east :: [(Int, Int)]
north_of_south_east = [ (k, l) | k <- [kmax],l <- [lmax + 1] ]

north_of_south_west :: [(Int, Int)]
north_of_south_west = [ (k, l) | k <- [kmax],l <- [lmin - 1] ]

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %% Constants
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- % eknath : initializations and other additions follow here

grid_max = 100 :: Int

grid_size = ((2, grid_max), (2, grid_max)) ::ArrayDim

constant_heat_source = (0.0 :: Double)

deltat_maximum = (0.01 :: Double)

specific_heat = 0.1 ::Double

p_coeffs :: DblArray
p_coeffs = strictArray (array ((0, 0),(2, 2))
            ([(1, 1) =: 0.06698]++
             [(i, j)=: 0.0 | i <- [0..2],j <- [0..2], not (i == 1 && j == 1)]))

e_coeffs:: DblArray
e_coeffs = strictArray (array ((0, 0),(2, 2))
            ([(0, 1) =: 0.1]++
             [(i, j)=: 0.0 | i <- [0..2],j <- [0..2], not (i == 0 && j == 1)]))

p_poly = strictArray (array ((1, 1),(4, 5))
          ([(i, j)=: p_coeffs | i <- [1..4],j <- [1..5]]))

e_poly = strictArray (array ((1, 1),(4, 5))
          ([(i, j)=: e_coeffs | i <- [1..4],j <- [1..5]]))

rho_table:: DblVector
rho_table = strictArray (array (1, 3)
             ([1 =: 0.0]++
              [2 =: 1.0]++
              [3 =: 100.0]))

theta_table:: DblVector
theta_table = strictArray (array (1, 4)
               ([1 =: 0.0]++
                [2 =: 3.0]++
                [3 =: 300.0]++
                [4 =: 3000.0]))

extract_energy_tables_from_constants = 
   (e_poly, 2, rho_table, theta_table)

extract_pressure_tables_from_constants = 
   (p_poly, 2, rho_table, theta_table)

nbc :: Array (Int, Int) Int
nbc = strictArray (array (pHbounds dimension_all_zones)
       ([(i, j)=: 1 | i <- [kmin],j <- [lmin + 1..lmax]]++
        [(i, j)=: 2 | i <- [kmax + 1],j <- [lmin + 1..lmax]]++
        [(i, j)=: 1 | j <- [lmin],i <- [kmin + 1..kmax]]++
        [(i, j)=: 1 | j <- [lmax + 1],i <- [lmin + 1..lmax]]++
        [(i, j)=: 4 | i <- [kmin],j <- [lmin]]++
        [(i, j)=: 4 | i <- [kmin],j <- [lmax + 1]]++
        [(i, j)=: 4 | i <- [kmax + 1],j <- [lmin]]++
        [(i, j)=: 4 | i <- [kmax + 1],j <- [lmax + 1]]))

pbb :: DblVector
pbb = strictArray (array (1, 4)
       ([2 =: 6.0]++
        [i=: 0.0 | i <- [1..4], not (i == 2)]))

pb:: DblVector
pb = strictArray (array (1, 4)
      ([i=: 1.0 | i <- [1..4], not (i == 2 || i == 3)]++
       [i=: 0.0 | i <- [2..3]]))

qb:: DblVector
qb = pb

all_zero_nodes :: a -> DblArray
all_zero_nodes _ = strictArray (array (pHbounds dimension_all_nodes)
                    ([node=: 0.0 | node <- all_nodes]))

all_zero_zones :: a -> DblArray
all_zero_zones _ = strictArray (array (pHbounds dimension_all_zones)
                    ([zone=: 0.0 | zone <- all_zones]))

make_position_matrix :: ((Int, Int) -> (Double, Double)) -> (DblArray, DblArray)
make_position_matrix interior_function = 
   let {
       boundary_position rx zx ry zy ra za = 
          let {
              (rax, zax) = (ra - rx, za - zx) ;
              (ryx, zyx) = (ry - rx, zy - zx) ;
              omega = ((2.0::Double) * (rax * ryx + zax * zyx)) 
                      / (ryx ^ (2::Int) + zyx ^ (2::Int)) ;
              rb = (rx - rax) + omega * ryx ;
              zb = (zx - zax) + omega * zyx
              } in (rb, zb) ;
       reflect_node x_dir y_dir a_dir node = 
          let {
              rx = reflect x_dir node r' ;
              zx = reflect x_dir node z' ;
              ry = reflect y_dir node r' ;
              zy = reflect y_dir node z' ;
              ra = reflect a_dir node r' ;
              za = reflect a_dir node z'
              } in boundary_position rx zx ry zy ra 
                      za ;
       (r', z') = (arrays_2 (pHbounds dimension_all_nodes)
                   ([node=: interior_function node
                        | node <- interior_nodes]++
                    [node=: reflect_node south southeast 
                               farsouth node
                        | node <- north_nodes]++
                    [node=: reflect_node north northeast 
                               farnorth node
                        | node <- south_nodes]++
                    [node=: reflect_node west southwest 
                               farwest node | node <- east_nodes]++
                    [node=: reflect_node east southeast 
                               fareast node | node <- west_nodes]++
                    [node=: reflect_node southwest west 
                               farwest node | node <- north_east_corner]++
                    [node=: reflect_node northwest west 
                               farwest node | node <- south_east_corner]++
                    [node=: reflect_node southeast south 
                               farsouth node
                        | node <- north_west_corner]++
                    [node=: reflect_node northeast east 
                               fareast node | node <- south_west_corner]++
                    [node=: reflect_node south southwest 
                               farsouth node
                        | node <- west_of_north_east]++
                    [node=: reflect_node north northwest 
                               farnorth node
                        | node <- west_of_south_east]++
                    [node=: reflect_node west northwest 
                               farwest node | node <- north_of_south_east]++
                    [node=: reflect_node east northeast 
                               fareast node | node <- north_of_south_west]))
       } in strictArrays_2 (r', z')

zone_area_vol :: (DblArray, DblArray) -> Zone -> (Double, Double)
zone_area_vol (r, z) zone = 
   let {
       (rsw, zsw) = (r!zone_corner_southwest zone, 
                    z!zone_corner_southwest zone) ;
       (rse, zse) = (r!zone_corner_southeast zone, 
                    z!zone_corner_southeast zone) ;
       (rne, zne) = (r!zone_corner_northeast zone, 
                    z!zone_corner_northeast zone) ;
       (rnw, znw) = (r!zone_corner_northwest zone, 
                    z!zone_corner_northwest zone) ;
       area1 = (rse * (zne - zsw) + rsw * (zse - zne)) 
               + rne * (zsw - zse) ;
       area2 = (rnw * (zsw - zne) + rne * (znw - zsw)) 
               + rsw * (zne - znw) ;
       average1 = (rsw + rse) + rne ;
       volume1 = (area1 * average1) / (3.0::Double) ;
       average2 = (rsw + rne) + rnw ;
       volume2 = (area2 * average2) / (3.0::Double) ;
       zone_area = (area1 + area2) / (2.0::Double) ;
       zone_volume = (volume1 + volume2) * (pi::Double)
       } in (zone_area, zone_volume)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %% Utilities
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reduce_list :: (a -> b -> a) -> a -> [b] -> a
reduce_list fcn z list = foldl fcn z list

max_list:: [Double] -> Double
max_list list = reduce_list max (head (list :: [ Double])) list

min_list:: [Double] -> Double
min_list list = reduce_list min (head (list :: [ Double])) list

sum_list:: [Double] -> Double
sum_list list = reduce_list (+) (0.0 :: Double) (list :: [ Double])

-- %%%% Edit History: /home/prj2/rpaul/ph/tests/ek-simple/003/ek-simple.id

-- %%%+/tmp_mnt/home/prj2/rpaul/ph/tests/ek-simple-93.id 
-- %%% [rpaul]  4/13/94 17:28  verified computations: velocity, position, area/volume/density , artificial-viscosity   and heat-conduction. 
-- %%%    - fixed the area computation in zone_area. (equation 24)
-- %%%    - both equation 24 and the code are still missing the constant 
-- %%%     factor of 2pi in the volume computation
-- %%%    - the code for artifical viscocity (equation 27) were missing upper_disc^2
-- %%%     and lower_disc^2
-- %%%    - the code for interior_cc (equation 37) was missing theta_hat^5/2
-- %%%    - the initial condition for heat is now 10.0 (was .0001)
