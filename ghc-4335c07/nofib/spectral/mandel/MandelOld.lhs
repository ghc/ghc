\section{The Mandelbrot set: Serial}

\begin{code}
module Mandel where
import PortablePixmap
import Data.Complex
default ()

\end{code}

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
The Mandelbrot set is the set of complex numbers $c$ for which the values 
of the polynomial
\begin{equation}
\label{poly}
f_c(z) = z^2 + c
\end{equation}
are connected\cite{falconer}. A graphical representation of this set 
can be rendered by plotting for different points in the complex plane 
an intensity that is proportional to either the rate of convergence of 
the above polynomial, or a constant if the polynomial is found to diverge.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{pseudocode}
mandel::Complex Double -> [Complex Double]
mandel c = infiniteMandel
           where
		infiniteMandel = c : (map (\z -> z*z +c) infiniteMandel)
\end{pseudocode}

\begin{code}
mandel::Complex Double -> [Complex Double]
mandel c@(cr :+ ci)  = infiniteMandel
           where
		infiniteMandel = c : (map fn infiniteMandel)
		fn::Complex Double -> Complex Double
		fn (r :+ i) = ((r*r) - (i*i) + cr ) :+ ((2*(r*i)) + ci)
\end{code}
The function @mandel@ is the functional equivalent of the mandelbrot
polynomial(\ref{poly}); operationally it generates an infinite list of 
complex numbers caused by iterating a starting value @c@ in the complex 
plane with the the mandelbrot polynomial.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}			
whenConverge::[Complex Double] -> Int -> Double -> Int
whenConverge (x:xs) limit radiusSq
   | (converged x radiusSq)  || 
     (limit == 0) 	   = 0				 
   |  otherwise            = 1 + (whenConverge xs (limit-1) radiusSq )
   
converged::Complex Double -> Double -> Bool
converged (x:+y) r =  r <= (x*x) + (y*y)

\end{code}
``@whenconverge@'' determines the rate of convergence of a list of complex
numbers. The rate of convergence of a the mandelbrot polynomial when applied 
to a complex number can therefore determined by the composition of the 
@mandel@ and @whenConverge@ functions thus: ``@whenConverge.mandel@''.
Care needs to be taken in the above composition that overflow does not
occur in any of the real number calculations performed in the @mandel@
function (see Section \ref{overflow} for a hacked solution). 


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsection{Testing for convergence}

\begin{equation}
\label{norm}
\|x\|_{p} = (\sum x^p)^\frac{1}{p}
\end{equation}

Convergence of two complex numbers is calculated by checking that the $p$
norm ($\|x\|_{p}$) of the numbers are equivalent. In 
the mandelbrot set, where we are calculating the norm of complex numbers
we could use the $\|x\| _{1}$ , $\|x\|_{2}$ 
or $\|x\| _{\infty} $.

\begin{eqnarray}
\label{normone}
\|x + iy\|_{1}      &=& |x| + |y| 		\\
\label{normtwo}
\|x + iy\|_{2}      &=& \sqrt{x^2 + y^2} 	\\ 
\label{norminf}
\|x + iy\|_{\infty} &=& max |x| |y|		\\ 
\end{eqnarray}

\begin{pseudocode}
converge'::Complex Double -> Complex Double -> Bool
converge' x y = (magnitude x) == (magnitude y)
\end{pseudocode}

The function @converge'@ uses the euclidean norm ($\|x\|_{2}$) defined as 
@magnitude@ in the standard prelude. The use of this norm places a large
overhead in the mandelbrot calculation due to the square root 
and scaling of the real numbers to prevent overflow in the preludes definition
of magnitude (we test and therefore prevent overflow elsewhere \smiley). 


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{The main engine - ``a place for parallelism''}

The function @mandelset@ generates a list of integers that represent the
rate of convergence of the mandelbrot polynomial at locations in a clipping
region of the complex plane.

\subsubsection{A sequential version}
\begin{code}[sequential]

mandelset::Double -> Double -> Double -> Double -> 
	   Integer -> Integer -> Int -> PixMap
mandelset x y x' y' screenX screenY lIMIT
   = render screenX screenY lIMIT results	
     where

	results::[Int]
	results = [colour b a | a <- [1..screenY] , b <- [1..screenX]]

	colour::Integer -> Integer -> Int
	colour  s t = whenConverge (mandel (initial s t)) lIMIT radiusSq

	radiusSq::Double
	radiusSq = max (x'-x) (y'-y)

	initial::Integer -> Integer -> Complex Double
	initial s t = (x + (((coerce s) * (x' - x)) / (fromInteger screenX)) :+
		       y + (((coerce t) * (y' - y)) / (fromInteger screenY)))

	coerce::Integer -> Double
	coerce  s   = encodeFloat (toInteger s) 0

\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Graphical rendering \smiley}
We generate high quality renderings of the mandelbrot set by printing 
ASCII characters to a terminal. The heuristic we use for printing is
arbitrary, and defined in @blackwhite@ :
\begin{code}

render::Integer -> Integer -> Int -> [Int] -> PixMap
render width height maxColour intensities
   = createPixmap (fromInteger width) (fromInteger height) maxColour
		  (map fn intensities)
     where
	fn::Int -> (Int,Int,Int)
        fn x = let y = (maxColour - x) in (x,y,y)
\end{code}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
