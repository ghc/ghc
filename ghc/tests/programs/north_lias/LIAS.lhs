% lias.lhs - Language Independent Arithmetic Standard in Haskell

% @(#)LIAS.lhs	1.11 dated 92/12/07 at 15:01:23

\documentstyle[a4wide,11pt,times]{article}

\title{Haskell and the Language Independent Arithmetic Standard}
\author{N D North\\
	National Physical Laboratory\\
	Teddington, TW11 0LW, UK.\\
	{\tt ndn@seg.npl.co.uk}}

% Some macros lifted from elsewhere to make this more standalone.
\makeatletter
% INLINE PROGRAM CODE
%
% \prog{foo} sets its argument in typewriter font.
\def\prog#1{\ifmmode\mbox{\tt #1}\else{\tt #1}\fi}

% NEWVERBATIM (from iso.sty)
%
% \newverbatim{foo} creates a new environment, foo, which behaves exactly
% like the verbatim environment except that it is delimited by
% \begin{foo} ... \end{foo}.
% See the VERBATIM section of latex.tex for the inspiration behind this.
%
\def\newverbatim#1{\expandafter\def\csname #1\endcsname{%
\@verbatim \frenchspacing\@vobeyspaces \csname @x#1verbatim\endcsname}
\expandafter\let\csname end#1\endcsname=\endtrivlist
\new@xverbatim{#1}}

\begingroup \catcode `|=0 \catcode `[= 1
\catcode`]=2 \catcode `\{=12 \catcode `\}=12
\catcode`\\=12
|gdef|new@xverbatim#1[
|expandafter|def|csname @x#1verbatim|endcsname##1\end{#1}[##1|end[#1]]]
|endgroup
\makeatother

\newverbatim{haskell}

% \lias{id} sets an identifier in LIAS font (italic)
\def\lias#1{\mbox{\it #1}}

% \liass{id}{sub} sets the identifier in LIAS font, with the given
%   subscript.
\def\liass#1#2{\mbox{$\lias{#1}_{#2}$}}

% \liasss{id}{sub}{sup} sets the identifier in LIAS font, with the
%   subscript and superscript.
\def\liasss#1#2#3{\mbox{$\lias{#1}_{#2}^{#3}$}}

\begin{document}
\maketitle

\section*{Introduction}

Haskell~\cite{hudak} is intended as an ``industrial strength'' 
functional programming language and, in partial fulfillment of that
aim, includes a rich set of numeric types and operators.
However, the semantics of numeric operations are rather imprecise, so
that determining the accuracy of numerical analysis programs is impossible
in Haskell, limiting its applicability.
The Language Independent Arithmetic Standard (LIAS)~\cite{lias}
defines the behaviour of numerical operations precisely, yet flexibly
enough that it is compatible with virtually all major arithmetic
implementations including, for example IEEE~754~\cite{ieee754}.

This report examines the extent to which Haskell and LIAS are compatible,
provides a model implementation of LIAS in Haskell, and recommends a
small addition to Haskell to improve compatibility.
The intention is to improve the portability of programs, both between
Haskell implementations and between Haskell and other LIAS-compliant
languages.


\section{Compatibility between Haskell and LIAS}

\subsection{Denormalised numbers}
\label{denorm}
Parameters for LIAS are all available in Haskell, with the exception of
\lias{denorm}, so few problems in principle.

\subsection{Accuracy}
Haskell implementations tend to use arithmetic of underlying system, so
extent to which accuracy complies depends on that of underlying system.

\subsection{Notification}
\label{notification}
Semantics of overflow etc are ``undefined'' so demanding notification is
impossible.
{\em Check what systems actually do.}

\subsection{Integers}
Haskell provides a class \prog{Integral}, whose members are integer
types.
In particular, \prog{Integer} is the type of arbitrary-precision
integers, and \prog{Int} is a type of fixed-precision integers with
range at least $[-2^{29} + 1, 2^{29} - 1]$ and closed under
negation.
Implementations are at liberty provide other integer types.

Both \prog{Integer} and \prog{Int} should comply with LIAS, with the
exception of notifications, as described in Section~\ref{notification}.

\subsection{Floating point}
Haskell provides a class \prog{RealFloat}, whose members are real (as
opposed to complex) floating point numbers.
In particular, \prog{Float} and \prog{Double} are supposed to
be at least equal in range and precision to IEEE single and double
precision respectively.
Implementations are at liberty provide other floating point types.

Both \prog{Float} and \prog{Double} should comply with LIAS, with the
exceptions of notifications, as described in Section~\ref{notification},
and \lias{denorm}, as described in Section~\ref{denorm}.


\section{LIAS in Haskell}

This section provides a model implementation of LIAS in Haskell.
Many of the required parameters and functions already exist, in which
case this section just describes how to obtain them in Haskell.
Others are not part of the standard language, and code is given to
implement these.

The section begins with the module header giving the module name and
exported identifiers.

\begin{haskell}

> module LIAS (
>     emax, emin, denorm,
>     fmax, fminN, fmin, epsilon,
>     signf, succf, predf, ulpf, truncf, roundf, fractpart
>     ) where

\end{haskell}


\subsection{Integers}
The LIAS parameters are: \lias{minint}, \lias{maxint} and \lias{bounded}.
Whether an integer type is bounded or not is part of the language
definition, so this parameter is not available to the user in Haskell.
The minimum and maximum parameters are available for the \prog{Int}
type, and are accessed as follows:
\begin{tabbing}
\lias{minint} \= \prog{minInt} \kill
\lias{minint} \> \prog{minInt} \\
\lias{maxint} \> \prog{maxInt} (\prog{= -minInt})
\end{tabbing}
Note that the Haskell Report (Section 6.8.2) states that
\prog{maxInt = -minInt}, which is compatible with LIAS.

All the integer operations required by LIAS are available in Haskell,
and are accessed as described in the table below:
\begin{tabbing}
mmmmmmmmmmmmmmm \= mmmmmmmmmm \= \kill
\liass{add}{I}     \>  \prog{x + y} \\
\liass{sub}{I}     \>  \prog{x - y} \\
\liass{mul}{I}     \>  \prog{x * y} \\
\liass{div}{I}     \>  \prog{x `div` y} (round to $-\infty$) \\
                   \>  \prog{x `quot` y} (round to 0) \\
\liass{rem}{I}     \>  \prog{x `mod` y} (round to $-\infty$) \\
                   \>  \prog{x `rem` y} (round to 0) \\
\liass{mod}{I}     \>  \prog{x `mod` y} (this is \liasss{mod}{I}{1}) \\
\liass{neg}{I}     \>  \prog{negate x} \\
\liass{abs}{I}     \>  \prog{abs x} \\
\liass{eq}{I}      \>  \prog{x == y} \\
\liass{neq}{I}     \>  \prog{x /= y} \\
\liass{lss}{I}     \>  \prog{x < y} \\
\liass{leq}{I}     \>  \prog{x <= y} \\
\liass{gtr}{I}     \>  \prog{x > y} \\
\liass{geq}{I}     \>  \prog{x >= y}
\end{tabbing}
The table shows that Haskell provides integer division with rounding
towards $-\infty$ and with rounding towards 0.

\subsection{Floating point}
Haskell provides all the parameters for floating point numbers, except
for \lias{denorm}.
The available parameters are determined as follows:
\begin{tabbing}
\lias{emax}  \=  fst (floatRange x) \= \prog{Int} \kill
\lias{r}     \>  floatRadix x       \> \prog{Integer} \\
\lias{p}     \>  floatDigits x      \> \prog{Int} \\
\lias{emax}  \>  fst (floatRange x) \> \prog{Int} \\
\lias{emin}  \>  snd (floatRange x) \> \prog{Int}
\end{tabbing}
In the table, \prog{x} is an expression of the type for which the
parameter is required.
For example, \prog{floatRadix (1.0 :: Float)} would give the radix of
the \prog{Float} type.
The alternative to this mechanism is to provide a separate set of
identifiers for each floating point type.

For convenience, we provide Haskell identifiers for \lias{emax} and
\lias{emin}.
\begin{haskell}

> emax, emin :: (RealFloat a) => a -> Int
> emax x  =  snd (floatRange x)
> emin x  =  fst (floatRange x)

\end{haskell}

The derived constants require some coding as follows:
\begin{haskell}

> fmax, fminN, fminD, fmin, epsilon :: (RealFloat a) => a -> a

> fmax x  =  encodeFloat (floatRadix x ^ floatDigits x - 1)
>                        (emax x - floatDigits x)

> fminN x  =  encodeFloat 1 (emin x - 1)

> fminD x  =  encodeFloat 1 (emin x - floatDigits x)

> fmin x  =  if denorm x then fminD x else fminN x

> epsilon x  =  encodeFloat 1 (1 - floatDigits x)

\end{haskell}

The definition of \lias{denorm} assumes that the implementation gives
zero on underflow.
The Haskell Report leaves behaviour on underflow undefined, which
makes this definition less than satisfactory and suggests that
\prog{denorm} should be part of the language.
\begin{haskell}

> denorm :: (RealFloat a) => a -> Bool
> denorm x  =  fminN x / fromInteger (floatRadix x) /= 0

\end{haskell}

The floating point operations are listed below, with the syntax for
invoking them.
The operations marked ``$\dagger$'' are not part of Haskell and are
defined later in the LIAS module.
\begin{tabbing}
mmmmmmmmmmmmmmm \= mmmmmmmmmm \= \kill
\liass{add}{F}       \>  \prog{x + y} \\
\liass{sub}{F}       \>  \prog{x - y} \\
\liass{mul}{F}       \>  \prog{x * y} \\
\liass{div}{F}       \>  \prog{x / y} \\
\liass{neg}{F}       \>  \prog{negate x} \\
\liass{abs}{F}       \>  \prog{abs x} \\
\liass{sqrt}{F}      \>  \prog{sqrt x} \\
\liass{sign}{F}      \>  \prog{signf x} \> $\dagger$ \\
\liass{exponent}{F}  \>  \prog{exponent x} \\
\liass{fraction}{F}  \>  \prog{significand x} \\
\liass{scale}{F}     \>  \prog{scaleFloat n x} \\
\liass{succ}{F}      \>  \prog{succf x} \> $\dagger$ \\
\liass{pred}{F}      \>  \prog{predf x} \> $\dagger$ \\
\liass{ulp}{F}       \>  \prog{ulpf x} \> $\dagger$ \\
\liass{trunc}{F}     \>  \prog{truncf x n} \> $\dagger$ \\
\liass{round}{F}     \>  \prog{roundf x n} \> $\dagger$ \\
\liass{intpart}{F}   \>  \prog{truncate x} \\
\liass{fractpart}{F} \>  \prog{snd (properFraction x)} \\
\liass{eq}{F}        \>  \prog{x == y} \\
\liass{neq}{F}       \>  \prog{x /= y} \\
\liass{lss}{F}       \>  \prog{x < y} \\
\liass{leq}{F}       \>  \prog{x <= y} \\
\liass{gtr}{F}       \>  \prog{x > y} \\
\liass{geq}{F}       \>  \prog{x >= y}
\end{tabbing}

The code below provides definitions of the operations marked ``$\dagger$''
and, for convenience, a definition of \liass{fractpart}{F}.

\begin{haskell}

> signf :: (RealFloat a) => a -> a
> signf x | x >= 0  =  1
>         | x <  0  =  -1

\end{haskell}

\prog{floatRadixf} is a useful utility function which gives the floating
point radix as a member of the class \prog{RealFloat}.
\begin{haskell}

> floatRadixf :: (RealFloat a) => a -> a
> floatRadixf x  =  fromInteger (floatRadix x)

\end{haskell}

\begin{haskell}

> succf, predf :: (RealFloat a) => a -> a
> succf x | x == 0          =  fmin x
>         | x == -(fmin x)  =  0
>         | True            =  encodeFloat (m + 1) n
>                              where
>                              (m, n)  =  decodeFloat x

> predf x  =  - succf (- x)

\end{haskell}

\begin{haskell}

> ulpf :: (RealFloat a) => a -> a
> ulpf x | x == 0  =  error "ulpf of 0"
>        | True    =  res (encodeFloat 1 (expf x - floatDigits x))
>                     where
>                     res 0  =  error "ulpf underflow"
>                     res x  =  x

\end{haskell}

\begin{haskell}

> floorf :: (RealFloat a) => a -> a
> floorf x  =  fromInteger (floor x)

> expf :: (RealFloat a) => a -> Int
> expf x  =  if abs x >= fminN x then exponent x else emin x

> truncf :: (RealFloat a) => a -> Int -> a
> truncf x n | n <= 0          =  error "truncf with n <= 0"
>            | j >= eemin - p  =  encodeFloat (i `quot` (r ^ (p - n)))
>                                             (j + p - n)
>            | True            =  encodeFloat (i `quot` (r ^ (eemin - j - n)))
>                                             (eemin -n)
>                                 where
>                                 (i, j)  =  decodeFloat x
>                                 eemin   =  emin x
>                                 r       =  floatRadix x
>                                 p       =  floatDigits x

\end{haskell}

\begin{haskell}

> roundf ::  (RealFloat a) => a -> Int -> a
> roundf x n | n <= 0              =  error "roundf with n <= 0"
>            | n >= floatDigits x  =  x
>            | True                =  signf x * floorf (abs x / y + 0.5) * y
>                                     where
>                                     y  =  encodeFloat 1 (expf x - n)

\end{haskell}

\begin{haskell}

> fractpart :: (RealFloat a) => a -> a
> fractpart x  =  snd (properFraction x)

\end{haskell}

\section{Recommendations}


\begin{thebibliography}{9}
\bibitem{hudak} P Hudak, S Peyton Jones, P Wadler et al.
{\it Report on the Functional Programming Language Haskell, Version 1.1.}
Department of Computing Science, University of Glasgow, August 1991.
\bibitem{ieee754} IEEE Standard for Binary Floating-Point Arithmetic.
    ANSI/IEEE Std 754-1985, 1985.
\bibitem{lias} M~Payne, C~Schaffert, and B~A~Wichmann.
{\em The Language Compatible Arithmetic Standard}.
 January 1990. ACM SIGPLAN Notices, Vol 25,
  pp59-86, and ACM SIGNUM Newsletter, Vol 25, No 1, pp2-43.
\end{thebibliography}
\end{document}
