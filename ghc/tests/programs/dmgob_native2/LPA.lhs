\begin{comment}

> module LPA where
>#ifndef __GLASGOW_HASKELL__
> import Trace
>#endif

\end{comment}


%----------------------------------------------------------------------
\section {A Brief Review of Discrete-Time Signal Processing}
%----------------------------------------------------------------------

%%------------------------------------------------------------
\subsection {Discrete-Time Signals}
%%------------------------------------------------------------

        This section only provides some background terminology;
see~\cite{OppeScha75} or \cite{ProaMano92} for motivation and more
details.


        A {\em discrete-time signal\/} is simply a sequence of
numbers.  That is, a discrete-time signal is a function whose domain
is the set of integers and whose range is some set of numbers, e.g.,
the real or complex numbers.  The elements of the sequence are called
{\em samples}.  The $n$th sample of the signal $x$ is denoted by
$x_n$.  For the rest of this paper, we will refer to discrete-time
signals as ``signals'' or ``sequences'' interchangeably.


        It is mathematically convenient to consider signals to be
defined for all integers, with samples taking the value zero if not
otherwise specified.  A signal $x$ is {\em finite-duration\/} if there
are integers $n_1$ and $n_2$ such that $x_n = 0$ whenever $n<n_1$ or
$n>n_2$.  A {\em right-sided sequence\/} $x$ is one for which there is
an integer $n_1$ such that $x_n = 0$ for all $n < n_1$; if $n_1 \geq
0$ then the signal is also said to be {\em causal}.  {\em
Left-sided\/} and {\em two-sided\/} sequences are defined similarly.
All signals in this paper are assumed to be causal with $n_1 = 0$.


        Since signals are sequences of numbers, they are naturally
represented in Haskell using lists, particularly since we are assuming
causal signals indexed from zero and Haskell lists are indexed from
zero.


        We will often use the notation $x^n_k$ to denote the list of
samples $[x_k,x_{k+1},\ldots,x_n]$ of the signal $x$.  If $x$ is
potentially infinite in duration or if knowing its duration is not
important, we will replace $n$ by $\infty$.  For a causal signal, this
convention means that $x^\infty_0$ represents the entire signal.


        For convenience, we introduce the following type synonym:
        \begin{verbatim}

> type Signal a = [a]

\end{verbatim}
        The element type is unspecified because signals can be
sequences of integers, floating point numbers, or complex numbers.
Thus, \verb~Signal Int~ represents a signal whose samples are
finite-precision integers, \verb~Signal Float~ represents a signal
whose samples are floating point numbers, etc.


%%------------------------------------------------------------
\subsection {The $Z$ Transform}
%%------------------------------------------------------------

        Given a signal $x$, the {\em Z transform\/} of $x$ is the
complex-valued function $X(z)$ of the complex variable $z$ defined by
        \begin{equation}
        X(z) \isDefined \sum_{k=-\infty}^\infty x_k z^{-k}
        \label{eq:Z-transform}
        \end{equation}
        for all $z$ for which the power series converges.  If the
signal $x$ has $Z$ transform $X(z)$, we will write
        \[
        x \transformPair X(z)
        \]
        The $Z$ transform is useful because it allows us to study
discrete-time systems (Section~\ref{sc:discrete-time-systems}) using
algebraic manipulations.  This is because the $Z$ transform has a
number of useful properties, only three of which are needed here.
        \begin{enumerate}

        \item {\em scaling}

        If we multiply each sample of the signal $x$ by a scalar $a$
and call the resulting sequence $y$, then
        \[
        y \transformPair Y(z) = aX(z) \transformPair map\ (a*)\ x
        \]

        \item {\em superposition}

        Given two signals $x$ and $y$, if we add them
sample-by-sample to get a new signal $w$, then
        \[
        w \transformPair W(z)  =  X(z) + Y(z)
        \]

        \item {\em shift}

        Given a signal $x$ and an integer $k$, define the signal $y$
by
        \[
        y_n = x_{n-k}
        \]
        we have
        \[
        y \transformPair Y(z) = z^{-k}X(z)
        \]
        

        \end{enumerate}


%%------------------------------------------------------------
\subsection {Discrete-Time Systems}
\label{sc:discrete-time-systems}
%%------------------------------------------------------------

        A {\em discrete-time system\/} is an operator that transforms
an {\em input\/} signal into an {\em output\/} signal.  These systems
are commonly called {\em filters\/} when their function is to suppress
or enhance some feature of the input signal.  In Section~\ref{sc:fir}
we will define a function for performing a particular type of
filtering.
        \begin{verbatim}

> type Filter a = Signal a -> Signal a

\end{verbatim}


%%------------------------------------------------------------
\subsection {Finite Impulse Response Filtering}
\label{sc:fir}
%%------------------------------------------------------------

        A (causal) {\em finite impulse response\/} (FIR) filter is
characterized by a finite-duration signal $h$ called the {\em impulse
response\/} of the filter.  For an arbitrary input signal $x$, the
samples of the output signal $y$ are defined by the {\em convolution
sum\/}
        \begin{equation}
        y_n = \sum^{M-1}_{k=0} h_k x_{n-k}
        \label{eq:convolution}
        \end{equation}
        where $M$ is the length of $h$. We will use FIR filtering
twice, once in the definition of the function \verb~preemph~ in
Section~\ref{sc:preemph} and again in the definition of the function
\verb~cepstra~ in Section~\ref{sc:cepstra}.  Hence we need to define a
Haskell function that implements FIR filtering.  In doing so, we will
demonstrate how it is often possible to derive an executable
definition from a mathematical specification.


        $Z$ transforms can be used to express the filtering operation.
Suppose signals $x$, $h$, and $y$ are related by
(\ref{eq:convolution}).  Denoting the $Z$ transforms of these signals
by $X(z), Y(z)$, and $H(z)$, respectively, it is easy to show that
        \begin{equation}
        Y(z) = H(z)X(z)
        \end{equation}
        We now derive a Haskell function for performing FIR filtering.
        \begin{eqnarray*}
        Y(z) & = & H(z)X(z) \\
             & = & \sum^{M-1}_{k=0} h_k z^{-k} X(z)\\
             & = & \sum^{M-1}_{k=0} z^{-k} (h_kX(z)) \\
             & = & \sum^{M-1}_{k=0} z^{-k} W^{(k)}(z)
        \end{eqnarray*}
        where we have set $W^{(k)}(z) = h_kX(z)$.  Using Horner's rule,
        \begin{eqnarray*}
        Y(z) & = & W^{(0)}(z)\ +\ z^{-1}(W^{(1)}(z)
                               +\ z^{-1}(W^{(2)}(z) 
                               +\ \ldots \nonumber \\
             &   & \hspace{1.2in}
                               +\ z^{-1}(W^{(M-2)}(z)
                               +\ z^{-1} W^{(M-1)}(z)) \ldots\ ))
        \end{eqnarray*}
        %
        %
        Defining the transform relationships $w^{(k)} \transformPair
W^{(k)}(z)$, we get
        \begin{equation}
        y  =  w^{(0)} \oplus (w^{(1)} \oplus (w^{(2)} \oplus \ldots
                   \oplus (w^{(M-2)} \oplus w^{(M-1)}) \ldots\ ))
        \label{eq:ws}
        \end{equation}
        where the operator $\oplus$ is such that, for two arbitrary
signals $u$ and $v$ having $Z$ transforms $U(z)$ and $V(z)$ we have
        \[
        u \oplus v \ \transformPair\  U(z) + z^{-1}V(z).
        \]
        From the superposition and shift properties of the $Z$
transform, it is clear that the operator $\oplus$ is implemented by
delaying the second operand by one sample and then performing
sample-by-sample addition.  Let's call this operation
``\verb~delayAdd~'':
        \begin{verbatim}

> delayAdd :: (Num a) => Signal a -> Signal a -> Signal a
> (x:rxs) `delayAdd` y  = x : combine rxs y
> []      `delayAdd` y  = 0 : y

\end{verbatim}
        (The second equation treats the empty list as the all-zero
signal.)  This definition uses a function \verb~combine~ which is
similar to the Standard Prelude function \verb~zipWith~, except that
\verb~combine~ returns a list as long as the {\em longer\/} of the two
lists, with the ``missing'' values from the shorter list assumed to be
zero.
        \begin{verbatim}

> combine :: (Num a) => Signal a -> Signal a -> Signal a
> combine (x:rxs) (y:rys) = x+y : combine rxs rys
> combine  []       ys    = ys
> combine  xs       []    = xs

\end{verbatim}


        Now we observe that (\ref{eq:ws}) can be rewritten using
$foldr1$:
        \begin{eqnarray*}
         y    & = & foldr1\ \ (\oplus)\ \ [\: w^{(0)},\ 
                                 w^{(1)},\ \ldots,\ w^{(M-1)} \:] \\
              & = & foldr1\ \ (\oplus)\ \ [\: map\ \ (h_k *)\ \ x \:|\:
                                           k \leftarrow [0..M-1] \:]
        \end{eqnarray*}


        Thus, we can implement an FIR filter having impulse response
\verb~hs~ on an input signal \verb~x~ as follows:\footnote{My thanks
to Mark Jones of Yale University who first suggested this form of the
definition.}
        \begin{verbatim}

> fir :: Signal Float -> Filter Float
> fir hs x = foldr1 delayAdd [map (h*) x | h <- hs]

\end{verbatim}


%----------------------------------------------------------------------
\section {Linear Predictive Analysis}
%----------------------------------------------------------------------

        The processor described here is rather conventional; we just
implement most of the blocks described in~\cite[pp.\ 
112--117]{RabiJuan93}.


%--------------------------------------------------
\subsection {Preemphasis}
\label{sc:preemph}
%--------------------------------------------------

        The speech signal is first passed through a simple high-pass
filter to reduce the degree of spectral tilt.  The system
function is
        \[
        P(z) = 1 - a z^{-1}
        \]
        where $a$ is a real number less than but close to 1; a typical
value is 0.95.  Hence, the preemphasis filter is an FIR filter with
the impulse response $[1,\ -a\ ]$.
        \begin{verbatim}

> preemph :: Float -> Filter Float
> preemph a =  fir [1,-a]

\end{verbatim}


%--------------------------------------------------
\subsection {Framing}
%--------------------------------------------------

        Because of the time-varying nature of speech, the signal is
broken into a list of equal-length {\em frames}, each offset from its
predecessor by a fixed number of samples.  Typically, the frame length
is about $20$ ms and the offset is about $10$ ms.  We need a function
\verb~frames~ that takes three arguments: a frame length, an offset,
and the signal.  We can derive the definition of \verb~frames~ as
follows:
        \begin{eqnarray*}
      frames\ n\ m\ x^\infty_0
      & = & [
          x_0^{n-1},\;  
          x_m^{m+n-1},\;   
          x_{2m}^{2m+n-1},\   
          \ldots\ ] \\
      & = & map\ (take\ n)\ 
          [
          x_0^\infty,\;
          x_m^\infty,\;
          x_{2m}^\infty,\;
          \ldots\ ] \\ 
      & = & map\ (take\ n)\ (iterate\ (drop\ m)\ x_0^\infty).
\end{eqnarray*}
and thus
        \begin{equation}
        frame\ n\ m\ = map\ (take\ n)\ \circ\ iterate\ (drop\ m)
        \end{equation}
        But the signal may be finite, so we define and use a function
\verb~check_for_end~ to avoid processing an infinite stream of empty
lists.
        \begin{verbatim}

> type Frame a =  [a]
> frames      :: Int -> Int -> Signal a -> [Frame a]
> frames n m  =  check_for_end . map (take n) . iterate (drop m)

\end{verbatim}
        Here is the definition of \verb~check_for_end~:
        \begin{verbatim}

> check_for_end = takeWhile (not . null)

\end{verbatim}


%--------------------------------------------------
\subsection {Windowing}
%--------------------------------------------------

        A {\em window\/} is used to smoothly attenuate samples at the
edges of a frame prior to analysis.  A window is applied by
sample-by-sample multiplication. It is tempting to define the
windowing operation using the Standard Prelude function
\verb~zipWith~, however, we don't want to analyze a frame of data that
is too short, as would happen if the length of the entire signal
exceeded that of an integral number of frames by only a few samples.
Hence we define the function \verb~window~ using a utility function
\verb~window'~ that makes sure that the segment being windowed is at
least as long as the window itself; otherwise an empty frame is
returned.  Note that \verb~window'~ is not a stream function; the
output list is accumulated in the third argument so that it can be
forgotten if the signal turns out to be shorter than the window.
        \begin{verbatim}

> type Window = [Float]

> window      :: Window -> Signal Float -> Frame Float
> window w x  =  window' w x []

> window' (w:rws) (x:rxs) buf = window' rws rxs (w*x : buf)
> window' (_:_)    []     _   = []
> window'  []      _      buf = buf

\end{verbatim}


        A popular window is the {\em Hamming window}.  If the window
is to have $N$ samples, the formula for the $n$th sample is:
        \[
        w(n) = 0.54 - 0.46 \cos( \frac{2\pi}{N-1} n )
        \]
        Here is the function for building a Hamming window of \verb~n~
samples:
        \begin{verbatim}

> hamming_window :: Int -> Window
> hamming_window npts =
>       let angle = (2*pi)/fromInt (npts-1)
>       in  [0.54 - 0.46*cos (angle*fromInt n) | n <- [0..npts-1]]

\end{verbatim}


        One advantage of lazy evaluation is that by defining the
windowing function the way we have we can do the windowing and the
framing simultaneously. Thus, it turns out that we don't need the
function \verb~frames~.  Let the first argument be the window width
and the second argument be the offset between frames.
        \begin{verbatim}

> windows :: Int -> Int -> Signal Float -> [Frame Float]
> windows n m =
>       let
>         hw           = hamming_window n
>         apply_window = window hw
>       in 
>         check_for_end . map apply_window . iterate (drop m)

\end{verbatim}


%--------------------------------------------------
\subsection {The Autocorrelation Sequence}
%--------------------------------------------------

        The function \verb~autocorr~ computes the autocorrelation
sequence for the signal $x$ as a prelude to computing the coefficients
of the optimal $p$th-order linear prediction filter
(Section~\ref{sc:Durbin}). The $i$th sample of the autocorrelation
sequence for an $N$-point signal is defined by the equation
        \[
        r_i = \left\{
              \begin{array}{ll}
              \sum^{N-1-i}_{n=0} x_n x_{n+i} & 0 \leq i < N \\
               0                             & i \geq N
              \end{array}
              \right.
        \]
        The summation operation is just the familiar dot product of
linear algebra.  The dot product can be coded in Haskell as follows:
        \begin{verbatim}

> x `dot` y = sum (zipWith (*) x y)

\end{verbatim}
        However, the operator \verb~`dot`~ is actually more general
than the familiar dot product because the two argument lists can have
different length; the longer list is essentially truncated to the
length of the shorter list.  For derivation purposes, let `$\odot$'
denote this more general operation, and let `$\cdot$' denote the
standard dot product.  In general,
        \[
        x^n_0 \cdot y^n_0 = x_0^{n+k} \odot y^n_0 =
        x^n_0 \odot y^{n+k}_0
        \]
        for all $k \geq 0$, whereas $x^{n+k}_0 \cdot y^n_0$ would be
undefined.  We can now formally derive the Haskell function for
computing the non-zero portion of the autocorrelation sequence as
follows:
        \begin{eqnarray*}
        [r_0,\ r_1,\ \ldots,\ r_{N-1} ]
        & = & [ \sum^{N-1-i}_{n=0} x_n x_{n+i} \ |\ i \leftarrow [0..N-1]\:]\\
        & = & [ x^{N-1-i}_0 \cdot x^{N-1}_i    \ |\ i \leftarrow [0..N-1]\:]\\
        & = & [ x^{N-1}_0 \odot x^{N-1}_i      \ |\ i \leftarrow [0..N-1]\:]\\
        & = & map\ \ (x^{N-1}_0\ \odot)\ \ [ x^{N-1}_i \ |\ i \leftarrow
                                                         [0..N-1]\:]\\
        & = & map\ \ (x^{N-1}_0\ \odot)\ \ (tails\ x^{N-1}_0)
        \end{eqnarray*}
        where the function $tails$ is such that
        \[
        tails\ x^N_0 = [\ x^N_0,\ x^N_1,\ x^N_2,\ \ldots,\ x^N_N\ ]
        \]
        To get the complete autocorrelation sequence, we just append
an infinite list of zeros:
        \begin{verbatim}

> autocorr x = map (x `dot`) (tails x) ++ repeat 0.0

\end{verbatim}
        The function \verb~tails~ is defined as follows:\footnote{This
function is also provided in the Chalmer's hbc library module {tt
ListUtil}.}
        \begin{verbatim}

> tails xs@(_:rxs) | null rxs   = [xs]
>                  | otherwise  = xs : tails rxs

\end{verbatim}


        In a conventional imperative language we would need to pass
{\em two\/} arguments to this function, one specifying how many
autocorrelation values are to be computed.  The function would then
return an array containing precisely that many values.  However, in a
lazy language the autocorrelation values will only be computed as
needed, so we do not need to specify such a value for this function.


        The value $r_0$ is the {\em energy} of the signal.  The
logarithm of the energy is often included as a parameter in feature
vectors used in recognizers.


%--------------------------------------------------
\subsection {The Durbin Algorithm}
\label{sc:Durbin}
%--------------------------------------------------

        The next step is to solve for the optimum linear predictor
filter coefficients for a given order $p$.  This can be done
efficiently using the {\em Durbin Algorithm}.  The steps are listed in
Figure~\ref{fg:durbin} as they appear in~\cite[p.\ 411]{RabiScha78}
but with a few changes in notation.  Basically, we successively
compute the optimum prediction filters for each order $i$ starting
with $i=1$.  The optimum $i$th-order filter coefficients, $a^{(i)} =
[a^{(i)}_1,\ldots,a^{(i)}_i]$, are calculated from the optimum
$(i-1)$th-order filter coefficients, $a^{(i-1)} = [
a^{(i-1)}_1,\ldots, a^{(i-1)}_{i-1}]$, the mean squared error of that
filter, $e_{i-1}$, the autocorrelation value $r_i$, and the
autocorrelation values $r_1, r_2,\ldots, r_{i-1}$.
        \begin{figure}
        \fbox{
        \begin{tabular}{cl}
Initialization: & $e_0 = r_0$ \\[0.10in]
%
%  Durbin algorithm
%
\shortstack{ Iteration: \\ $(i=1,2,\ldots,p)$} &
   \begin{minipage}{2.850in}
        \begin{eqnarray*}
%
      k_i & = & \left( r_i - \sum^{i-1}_{j=1} a_j^{(i-1)} r_{i-j}
                \right) \left/ \: e_{i-1} \right. \\
%
      a_j^{(i)} & = & \left\{ \begin{array}{ll}
                              a_j^{(i-1)} - k_i a_{i-j}^{(i-1)}, &
                                    j = 1, \ldots, i-1 \\
                              k_i, & j=i
                             \end{array}
                       \right.\\
%
      e_i   & = & (1 - k_i^2) e_{i-1}
%
        \end{eqnarray*}
   \end{minipage}
%
\end{tabular}
  }
\caption[]{The Durbin Algorithm.}
\label{fg:durbin}
\end{figure}


        This description can be straightforwardly realized in a
conventional language using arrays; we will implement the algorithm in
Haskell without using arrays.
        \begin{verbatim}

> type LPA_Filter = (Float, [Float])

\end{verbatim}


%%------------------------------------------------------------
        \subsubsection {Updating the Predictor Polynomial}
%%------------------------------------------------------------

        First, we derive a function \verb~new_as~ that takes the
optimal $(i-1)$th-order linear prediction filter coefficients and the
$i$th reflection coefficient and returns the optimal $i$th-order
linear prediction filter coefficients.  From Figure~\ref{fg:durbin},
the optimal $i$th-order linear prediction filter is given by
        \begin{eqnarray*}
        a^{(i)} & \isDefined &
                      [ a^{(i)}_1,\ a^{(i)}_2,\ \ldots,\ a^{(i)}_i ] \\
                & = & [ a^{(i-1)}_1 - k_i a^{(i-1)}_{i-1},\ \ 
                        a^{(i-1)}_2 - k_i a^{(i-1)}_{i-2},\ \ 
                        \ldots,\ \ 
                        a^{(i-1)}_{i-1} - k_i a^{(i-1)}_1,\ \ 
                        k_i ]
        \end{eqnarray*}
        Note that the subtraction operation involves the first and
last coefficients, the second and second-to-last coefficients, etc.
If we place two copies of $a^{(i-1)}$ side by side,
        \[
        [ a^{(i-1)}_1,\ a^{(i-1)}_2,\ \ldots,\ 
          a^{(i-1)}_{i-2},\ a^{(i-1)}_{i-1} ]\ \ \  
        [ a^{(i-1)}_1,\ a^{(i-1)}_2,\ \ldots,\   
          a^{(i-1)}_{i-2},\ a^{(i-1)}_{i-1} ]
        \]
        it is easy to see that what is called for is some type
of \verb~foldr~ operation that consumes the list on the right as it
moves through the list on the left.  Indeed, if we define the binary
operator $\oplus$ by (assuming that $k_i$ is in scope)
        \begin{equation}
        a \oplus (p,\ b:bs)  =  ((a-k_i*b):p,\ bs)
        \label{eq:oplus}
        \end{equation}
        then we have
        \begin{equation}
        a^{(i)} = fst\ \ (foldr\ \ (\oplus)\ \ ([k_i], a^{(i-1)})\ \ 
                        a^{(i-1)})
        \label{eq:new-as}
        \end{equation}
        Combining (\ref{eq:oplus}) and (\ref{eq:new-as}) into a
Haskell definition:
        \begin{verbatim}

> new_as as k = fst (foldr op ([k],as) as)
>               where  a `op` (p,b:bs) = ((a-k*b):p,bs)

\end{verbatim}


%%------------------------------------------------------------
        \subsubsection {Computing the Reflection Coefficient}
%%------------------------------------------------------------

        Next we consider the calculation of the $i$th reflection
coefficient.  Ignoring the division by $e_{i-1}$ for the moment,
consider just the expression
        \begin{equation}
        r_i - \sum^{i-1}_{j=1} a^{(i-1)}_j r_{i-j}
        \label{eq:k-numerator}
        \end{equation}
        For the summation term we can use the same approach we used
for calculating the new prediction filter coefficients: a
\verb~foldr~ operation that consumes one list while moving through
another.  Defining the binary operator $\otimes$ by
        \begin{equation}
        a \otimes (s,\ b:bs) = (s + a*b,\ bs)
        \label{eq:otimes}
        \end{equation}
        the summation is the first component of the pair returned by
the expression
        \begin{equation}
        foldr\ \ (\otimes)\ \ (0,rs)\ \ as
        \label{eq:k-summation}
        \end{equation}
        where it is assumed that the length of $rs$ is at least as
great as that of $as$ (it is in practice).  The second component of
the pair returned by (\ref{eq:k-summation}) is a list having $r_i$ as
its head, which, as we see from (\ref{eq:k-numerator}), is what we are
subtracting the summation from.  Combining (\ref{eq:otimes}) and
(\ref{eq:k-summation}) with the division by $e_{i-1}$ yields the
following definition.
        \begin{verbatim}

> new_k rs (e,as) =
>       let (summation,r:_) = foldr op (0,rs) as
>                             where  a `op` (s,b:bs) = (s+a*b,bs)
>       in (r - summation)/e

\end{verbatim}


%%------------------------------------------------------------
        \subsubsection {One Step in the Durbin Algorithm}
%%------------------------------------------------------------

        We can now define the function that produces the optimal
$i$th-order linear prediction filter from the autocorrelation sequence
and the optimal $(i-1)$th-order filter.
        \begin{verbatim}

> durbin_step rs (e,as) = let k = new_k rs (e,as)
>                         in  ((1-k*k)*e, new_as as k)

\end{verbatim}


%%------------------------------------------------------------
        \subsubsection {The Durbin Algorithm}
%%------------------------------------------------------------

        To get the optimal $p$th-order linear predictor given the
autocorrelation sequence, we just iteratively apply the function
\verb~durbin_step~ to the appropriate initial condition and select the
$p$th element of the resulting sequence.  If $(r : rrs)$ is the
autocorrelation sequence for a frame, then the expression
        \[
        iterate\ \ (durbin\_step\ rrs)\ \ (r,[\ ])
        \]
        produces the list
        \[
        [\: (e_0,[\ ]),\ (e_1,a^{(1)}),\ (e_2,a^{(2)}),\ \ldots \:]
        \]
from which we want the element at position $p$.
        \begin{verbatim}

> durbin :: Int -> Signal Float -> LPA_Filter
> durbin p (r:rrs) = (iterate (durbin_step rrs) (r,[]))!!p

\end{verbatim}


%%------------------------------------------------------------
\subsection {Conversion to Cepstral Coefficients}
\label{sc:cepstra}
%%------------------------------------------------------------

        Given a discrete-time linear system with the system function
        \[
        \frac{G}{1 - \sum^p_{i=1} a_i z^{-i}}
        \]
        the sequence of {\em cepstral coefficients\/} $c^\infty_0$ for
this system is defined by
        \begin{equation}
        c_n = \left\{
              \begin{array}{ll}
              \ln G     & n=0 \\[0.10in]
              a_n + \frac{1}{n} \sum^{n-1}_{k=1} a_k \cdot (n-k)
                                                     \cdot c_{n-k} &
                        1 \leq n \leq p \\[0.10in]
        \frac{1}{n} \sum^p_{k=1} a_k \cdot (n-k) \cdot c_{n-k} & n > p
                \end{array}
              \right.
        \label{eq:cepstra}
        \end{equation}
        Note that the summation terms of (\ref{eq:cepstra}) are just
convolution sums for the FIR filter with $a^{(p)}$ as its impulse
response and the scaled cepstral coefficient signal $[c_1, 2c_2, 3c_3,
4c_4, \ldots]$.  Also, the second and third formulas are the same
except for the adding of the $a_i$'s to the scaled and filtered
cepstral coefficients.  Hence, we can use the function \verb~delayAdd~
that we defined when developing the FIR filtering function \verb~fir~
(Section~\ref{sc:fir}).  Also, the gain term is usually ignored in
forming the feature vector, so we just compute the sequence
$[c_1,c_2,\ldots]$.
        \begin{verbatim}

> cepstra :: LPA_Filter -> Signal Float
> cepstra (_,as) = cs
>       where
>       cs  = as `delayAdd` xs
>       xs  = zipWith (/) (fir as (zipWith (*) [1..] cs)) [2..]

\end{verbatim}
        Because there is no terminating condition and because of the
recursion---the sequence \verb~cs~ appears in the definition of the
sequence \verb~xs~---this definition relies on lazy evaluation in a
critical way.


        \subsection {Putting it all together}

        The function \verb~analyze~ transforms a windowed frame of
samples to a pair of values, a log gain term and a list of cepstral
coeficients.  The first argument is the order of the linear prediction
analysis.  The second argument is the number of cepstral coefficients.
        \begin{verbatim}

> analyze :: Int -> Int -> Frame Float -> (Float, [Float])

> analyze p q wxs = let
>                     rs         = autocorr wxs
>                     log_energy = log10 (head rs)
>                     cep        = take q (cepstra (durbin p rs))
>                   in
>                     (log_energy, cep)

> log10 :: Float -> Float
> log10 x = log x / log 10
> --log10 x = let result = log x / log 10 in
> --  trace ("log10:"++(shows x ":")++(shows (log x) "/")++(shows (log 10) "=")++(show result)) result

\end{verbatim}



        \subsection {The Main Program}

        Figure~\ref{fg:complete} shows the main Haskell program,
including the speech/feature I/O functions.
        \begin{figure}[p]
        \input{Main.lhs}
        \caption[]{The linear predictive speech analysis main program}
        \label{fg:complete}
        \end{figure}

