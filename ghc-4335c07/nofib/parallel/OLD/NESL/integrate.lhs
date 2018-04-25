Time-stamp: <2010-10-13 10:22:35 simonmar>

An adaptive algorithm for numeric integration.

Based on the NESL code presented in:
   Programming Parallel Algorithms
   by Guy E. Blelloch 
   in CACM 39(3), March 1996
   URL: http://www.cs.cmu.edu/afs/cs.cmu.edu/project/scandal/public/www/nesl/alg-numerical.html


This example code does adaptive integration of single variable functions.

\begin{code}
#if defined(GRAN)
import Strategies
#endif

import System.Environment  -- for getArgs
\end{code}

% ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; %
%   TRAPEZOIDAL INTEGRATOR        %
% ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; %

\section{Trapezoidal Integrator}

Used as a step in the adaptive method 

\begin{code}
trapezoidal :: (Floating a, NFData a, Integral b) => (a->a) -> (a, a) -> b -> a
trapezoidal f (a,b) n =
  let
      -- delta :: Float  
      delta = (b-a)/(fromIntegral n)
  in
#if defined(GRAN) && defined(DATA_PARALLEL)
      delta * sum (parMap rnf (\ i -> f (delta * (fromIntegral i) + a)) [0..n-1])
#else
      delta * sum [f (delta * (fromIntegral i) + a) | i <- [0..n-1]]
#endif
\end{code}

% ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; %
%   SIMPLE ADAPTIVE INTEGRATOR    %
% ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; %

\section{Simple Adaptive Integrator}

Some constants.

\begin{code}
accuracy1 :: Float
accuracy1 = 0.1 --1e-3   -- Error as a fraction of the value of the integral
                         --   (approximate) 
abs_accuracy1 :: Float
abs_accuracy1 = 0.01 --1e-3   -- Error as absolute value 

points1 :: Int
points1 = 1000     -- Number of points used in trapezoidal integral 

{-
% Simple adaptive integrator. 
    func -- a function, must have type (float -> float)
    (a,b) -- an interval, must both be floats
%
-}

-- adaptive1 :: (Floating a) => (a->a) -> (a, a) -> a
adaptive1 f ab@(a, b) abs_accuracy =
  let
      integral = trapezoidal f ab points1
      integral2 =  trapezoidal f ab points1*2
      mid = (a+b)/2.0
      left = adaptive1 f (a, mid) abs_accuracy
      right = adaptive1 f (a, mid) abs_accuracy
  in
      if 
#if defined(GRAN) && defined(DC_PARALLEL)
         let
          -- _parGlobal_ 25# 25# 0# 0# integral $
 	  -- _parGlobal_ 26# 26# 0# 0# integral2 $ 
          strategy x = rnf integral `par`
                       rnf integral2 `par`
                       x	   
         in
         strategy 
#endif
         ( (abs(integral2 - integral) > abs(accuracy1*integral)) &&
           (abs(integral2 - integral) > abs_accuracy) )
      then 
	  -- Parallel recursive calls on the two halves of the interval
          {-	  
            let (trace1, result1) = adaptive1 f (a, mid)
                (trace2, result2) = adaptive1 f (mid, b)
	    in  ( (abs(integral2 - integral) - abs(accuracy1*integral)) :
	         (concat (zipWith (\ x y -> [x,y]) trace1 trace2))
		 , result1+result2) 
          -}
#if defined(GRAN) && defined(DC_PARALLEL)
         let
          -- _parGlobal_ 21# 21# 0# 0# left $
 	  -- _parGlobal_ 22# 22# 0# 0# right $ 
          strategy x = rnf left `par`
                       rnf right `par`
                       x	   
         in
          strategy 
#endif
	  (left + right)
      else 
	  integral2
    
-- integrate1 :: (Floating a) => (a->a) -> (a, a) -> a
integrate1 f ab abs_accuracy  = adaptive1 f ab abs_accuracy
\end{code}

The main function as a test wrapper for the code:
\begin{code}
#ifdef ARGS
main = 	getArgs >>=  \[a1, a2, a3] ->
	                  let x      = fst (head (readFloat a1))
			      y      = fst (head (readFloat a2))
			      acc    = fst (head (readFloat a3))
			  in
                          print (integrate1 sinh (x, y) acc) 
#else
main = print (integrate1 sinh (1.0, 1.2) abs_accuracy1)
#endif
\end{code}

% ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; %
\section{Improved Adaptive Integrator}
% ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; %

\begin{code}
-- Constants %
accuracy2 :: Float
accuracy2 = 0.0001  -- 1e-4; 

points2 = 1000      -- Number of points used in trapezoidal integral %
conv_depth = 7      -- Depth of recursion at which start making serial calls %
max_depth = 9       -- Depth of recursion at which it quits %

{-
% This version has two improvements over the first version.
  1) It has a bound on how deep the recursion can go.
     This can prevent infinite loops at singularities.
  2) It starts making serial calls when recursion reaches a certain depth.
     This reduces parallelism and can prevent memory overflows.
% 
-}
     
-- adaptive1 :: (Floating a) => (a->a) -> Int -> (a, a) -> a
adaptive2 func depth (a,b) =
  let
      integral = trapezoidal func (a,b) points2
      integral2 =  trapezoidal func (a,b) points2*2
      mid = (a+b)/2.0
  in
      if ( abs (integral2 - integral)  > abs (accuracy2*integral) )
	 && (depth < max_depth)
      then 
	  if depth < conv_depth 
	  then
	      sum [ adaptive2 func (depth+1) interval  
		  | interval <- [(a,mid),(mid,b)] ]
	  else
	      adaptive2 func (depth+1) (a,mid) +
	      adaptive2 func (depth+1) (mid,b)
      else integral2

integrate2 func (a,b) = adaptive2 func 0 (a,b)
\end{code}

-----------------------------------------------------------------------------

\begin{pseudocode}
This is the original NESL code:

%
This example code does adaptive integration of single variable functions.
%

% ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; %
%   TRAPEZOIDAL INTEGRATOR        %
% ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; %

% Used as a step in the adaptive method %
function trapezoidal(func,a,b,n) =
let
    delta = (b-a)/float(n)
in
    delta * sum({func(delta * float(i) + a) : i in index(n)}) $

% ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; %
%   SIMPLE ADAPTIVE INTEGRATOR    %
% ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; %

% Some constants %
accuracy1 = 1e-3; % Error as a fraction of the value of the integral
                    (approximate) %
points1 = 1000;   % Number of points used in trapezoidal integral %

% Simple adaptive integrator. 
    func -- a function, must have type (float -> float)
    (a,b) -- an interval, must both be floats
%
function adaptive1(func,(a,b)) =
let
    integral = trapezoidal(func,a,b,points1);
    integral2 =  trapezoidal(func,a,b,points1*2);
    mid = (a+b)/2.0
in
    if abs(integral2 - integral) > abs(accuracy1*integral)
    then 
	% Parallel recursive calls on the two halves of the interval %
	sum({adaptive1(func,interval): interval in [(a,mid),(mid,b)]})
    else 
	integral2 $

function integrate1(func,a,b) = adaptive1(func,a,b);

% ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; %
%   IMPROVED ADAPTIVE INTEGRATOR  %
% ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; %

% Constants %
accuracy2 = 1e-4; 
points2 = 1000;  % Number of points used in trapezoidal integral %
conv_depth = 7;  % Depth of recursion at which start making serial calls %
max_depth = 9;  % Depth of recursion at which it quits %

% This version has two improvements over the first version.
  1) It has a bound on how deep the recursion can go.
     This can prevent infinite loops at singularities.
  2) It starts making serial calls when recursion reaches a certain depth.
     This reduces parallelism and can prevent memory overflows.
%      
function adaptive2(func,depth,a,b) =
let
    integral = trapezoidal(func,a,b,points2);
    integral2 =  trapezoidal(func,a,b,points2*2);
    mid = (a+b)/2.0
in
    if (abs(integral2 - integral) > abs(accuracy2*integral))
       and (depth < max_depth)
    then 
	if depth < conv_depth 
	then
	    sum({adaptive2(func,depth+1,interval): 
		 interval in [(a,mid),(mid,b)]})
	else
	    adaptive2(func,depth+1,a,mid) +
	    adaptive2(func,depth+1,mid,b)
    else integral2 $

function integrate2(func,a,b) = adaptive2(func,0,a,b);
\end{pseudocode}
