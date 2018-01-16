
% 	A program by Oege de Moor and Jeremy Gibbons
%		Sept 99
%
%
%                     | lines | chars | size(KB) | time(s)  |
% ---------------------+-------+-------+----------+----------+
% Haskell ghc 4.01     |   183 |  4676 |      453 |     4.72 |
% Haskell hbc 0.9999.4 |   183 |  4676 |      196 |    10.34 |
% C++ g++ 2.90.27      |   416 |  6310 |        8 |     0.43 |

% As one of the authors of this paper, I'd like to echo Manuel's warning
% about quoting out of context. The paper is about Haskell as a tool in
% designing and presenting algorithms, not about performance. The Haskell
% program was written for clarity, to explain a fairly tricky algorithm. The
% figures are there to show that in spite of that clarity-first style, the
% program is still suitable for sizable experiments.
% 
% The sources (both Haskell and C++) are on my home page at
% 
% http://www.comlab.ox.ac.uk/oucl/users/oege.demoor/homepage.htm

%% created by ODM@15:00, August 5, 1995
%% modified by ODM@15:00, April 16, 1996
%% more modifications JG, 10/6/97, 13/6/97, 16/6/97, ..., 2/7/97
%% rearranged JG 14/7/98, 16/11/98, 3/12/98
%% minor changes, make suitable for compilation by ghc/hbc, new test results ODM, 3/2/99


\documentclass[12pt]{article}
\usepackage{a4}


\begin{document}

\newif\ifskipspaces
\def\marginnote#1{%
  \ifvmode
    \leavevmode
    \skipspacestrue
  \else
    \skipspacesfalse
  \fi
  \domarginnote{#1}%
  \ifskipspaces \ignorespaces \fi}
\iftrue
  \def\domarginnote#1{\marginpar{\scriptsize\raggedright\hspace{0pt}#1}}
\else
  \def\domarginnote#1{}
\fi

\makeatletter
% "code" environment is like verbatim, but with negative vertical space
% before and after (so the verbatim material can start and end with a blank
% line, which doesn't appear in the printed result).
\begingroup \catcode `|=0 \catcode `[= 1
\catcode`]=2 \catcode `\{=12 \catcode `\}=12
\catcode`\\=12 |gdef|@xcode#1\end{mcode}[#1|end[mcode]]
|endgroup
\def\mcode{\unskip \vspace{-\baselineskip}%
          \@verbatim \frenchspacing\@vobeyspaces \@xcode}
\def\endmcode{\if@newlist \leavevmode\fi\endtrivlist\vspace{-\baselineskip}}
\makeatother

\def\implies{\Rightarrow}
\def\iff{\Leftrightarrow}


\title{Bridging the Algorithm Gap: \\
  A Linear-time Functional Program \\ 
  for Paragraph Formatting}
\author{
  \small\begin{tabular}[t]{l}
  {\large Oege de Moor}\\
  Programming Research Group \\ 
  Oxford University\\
  % Wolfson Building \\ 
  % Parks Road \\ 
  % Oxford OX1 3QD, UK
  \texttt{oege@comlab.ox.ac.uk}
  \end{tabular}
  \and
  \small\begin{tabular}[t]{l}
  {\large Jeremy Gibbons}\\
  School of Computing \& Math.\ Sciences \\ 
  Oxford Brookes University\\
  % Gipsy Lane \\ 
  % Headington \\ 
  % Oxford OX3 0BP, UK
  \texttt{jgibbons@brookes.ac.uk}
  \end{tabular}
}
\date{\today}
\maketitle

\begin{abstract} \noindent
In the constructive programming community it is commonplace to see
formal developments of textbook algorithms. In the algorithm design
community, on the other hand, it may be well known that the textbook
solution to a problem is not the most efficient possible. However, in
presenting the more efficient solution, the algorithm designer will
usually omit some of the implementation details, thus creating an
\emph{algorithm gap} between the abstract algorithm and its concrete
implementation. This is in contrast to the formal development, which
usually proceeds all the way to the complete concrete implementation of the less
efficient solution.

We claim that the algorithm designer is forced to omit some of the
details by the relative expressive poverty of the Pascal-like
languages typically used to present the solution. The greater
expressiveness provided by a functional language would allow the whole story
to be told in a reasonable amount of space. In this paper we use a
functional language to present the development of a sophisticated algorithm
all the way to the final code. We hope to bridge
the algorithm gap between abstract and concrete implementations, 
and thereby facilitate communication between the two communities.
\end{abstract}

{\renewcommand{\thefootnote}{\fnsymbol{footnote}}
\footnotetext{This paper is a revision of
Technical Report CMS-TR-97-03 from the School
of Computing and Mathematical Sciences, Oxford Brookes University.
%It is currently being submitted for publication.
}}

\section{Introduction}

The paragraph formatting problem \cite{knuth81} is a favourite 
example for demonstrating the effectiveness of formal methods.
Two particularly convincing derivations can be found in \cite{bird86} and \cite{morgan94}.
The algorithms derived in these references are applications of dynamic 
programming, and their time complexity is 
$O(min(w n,n^2))$, where $w$ is the 
maximum number of words on a line, and $n$ is the number of words to 
be formatted.  

Among algorithm designers it is well-known that one can solve the 
paragraph problem in $O(n)$ time, independent of $w$ 
\cite{eppstein92b,galil89,hirschberg87a,hirschberg87b}. 
Typical presentations of these linear
algorithms do however ignore some details (for instance, that the white
space on the last line does not count), thus creating an \emph{algorithm
gap} between the abstract algorithm and its concrete implementation.
This contrasts with the formal developments cited above, which
present concrete code, albeit for a less efficient solution.

This paper is an experiment in bringing formal methods and algorithm
design closer together, through the medium of functional programming. 
It presents a linear-time algorithm for paragraph formatting in a semi-formal
style. The algorithm is given as executable code; in fact, the \LaTeX\ file
used to produce this paper is also an executable 
\emph{Haskell} program.
In writing this paper we
hope to convince the reader that 
a functional style can be suitable for communicating non-trivial 
algorithms, without sacrificing rigour or clarity or introducing an algorithm gap.

Of course, there exist algorithms that are difficult to express
functionally. In fact, it came as a surprise to us that the algorithm 
presented here can indeed be implemented without resorting to any 
imperative language features. One of us (OdM) first attempted to
explain the algorithm (which is very similar to that in \cite{galil89})
in 1992, and then implemented it in Modula-2; when 
that program was discussed at the Oxford \emph{Problem Solving Club}, it met with
a lot of disappointment because of the need for destructive updates.
Only recently we realised how the algorithm could be expressed functionally.
This paper is therefore also a contribution to the ongoing effort
of determining what can be done efficiently in a purely functional
style. 

\iffalse
\begin{mcode}

>module Main where

>import Data.Char
>import System.IO
>import System.Environment
>import Prelude hiding (Word)

\end{mcode}
\fi


\subsection{Preliminaries}

We do not assume that the reader is an expert in functional programming;
we explain the necessary syntax and standard functions of Haskell as we go. (Of
course, some familiarity with a modern lazy functional language
such as Haskell, Miranda\footnote{Miranda is a trademark
of Research Software Ltd.} or Hope would be helpful; 
but we trust that the notation is fairly self-evident.)
In addition to the
standard functions of Haskell, we use a number of other
primitives, which are explained in this section.

\begin{description}

\item[fold1:]
The function \texttt{fold1} is related to the standard function \texttt{foldr}, 
but it operates on non-empty lists. Informally, we have
\begin{eqnarray*}
   \hbox to 0.5in{$\verb"fold1 step start [a0,a1,...,an]"$\hss} \\
 &=& 
   \verb"a0 `step` (a1 `step` (... `step` start an))"
\end{eqnarray*}
(Here, `\verb"[a0,a1,...,an]"' denotes a list, and
writing the binary function \texttt{f} inside backwards quotes, \verb"`f`",
allows it to be used as an infix operator.)
In words, \texttt{fold1} traverses a list from right to left, applying 
\texttt{start} to the last element, and `adding in' the next element at each 
stage using the function \texttt{step}.
The formal definition of \texttt{fold1} is
\begin{mcode}

>fold1 :: (a->b->b) -> (a->b) -> [a] -> b
>fold1 f g [a]   = g a
>fold1 f g (a:x) = f a (fold1 f g x)

\end{mcode}
(The first line is a \emph{type declaration}, stating that \verb"fold1" 
takes three arguments~--- a binary function of type \verb"a->b->b", 
a unary function of type \verb"a->b", and a list of type \verb"[a]"~---
and returns a value of type \verb"b".
The binary operator `\verb":"' is `cons', prepending an element onto a
list. The definition of \verb"fold1" is by \emph{pattern matching}:
the first equation that matches the argument applies.
Because neither equation matches the empty list, \verb"fold1" is undefined there.)

\item[scan1:]
The function \texttt{scan1} records all intermediate results of the
computation of \texttt{fold1} in a list:
\begin{mcode}

>scan1 :: (a->b->b) -> (a->b) -> [a] -> [b]
>scan1 f g = fold1 f' g'
>            where g' a   = [g a]
>                  f' a s = f a (head s) : s

\end{mcode}
(Here, the function \texttt{head} returns the first element of a non-empty
list.)
For example, the function \texttt{tails}, which returns all non-empty suffixes
of its argument, can be defined
\begin{mcode}

>tails :: [a] -> [[a]]
>tails = scan1 (:) (:[])

\end{mcode}
(The second argument \verb"(:[])" to \texttt{scan1} is the binary operator
`\texttt{:}' already supplied with its second argument, in this case the
empty list; the function \verb"(:[])" that results is the function
taking an element into a singleton list containing just that element.)

The relationship between \texttt{fold1} and \texttt{scan1} is succinctly
expressed in the so-called \emph{scan lemma}:
\begin{eqnarray*}
 \verb"scan1 f g" &=& \verb"map (fold1 f g) . tails"
\end{eqnarray*}
(Here, the higher-order function \texttt{map} applies the function which is
its first argument to every element of the list which is its second
argument; the binary operator `\verb"."' is function composition.)
The scan lemma will be useful towards the end of this paper.

\item[single:]
The operator \texttt{single} tests whether its
argument is a singleton list:
\begin{mcode}

>single :: [a] -> Bool
>single [a] = True
>single _   = False

\end{mcode}
(The pattern `\verb"_"' matches any argument, thereby acting as a kind
of `else' clause.)
It is thus similar to the standard Haskell function \texttt{null},
which tests for emptiness.

\item[minWith:]
The function \texttt{minWith f} takes a list \texttt{x}
and returns an element of \texttt{x} whose \texttt{f}-value is minimal.
Formally, \texttt{minWith} can be defined in terms of \texttt{fold1}:
\begin{mcode}

>minWith :: (a->Int) -> [a] -> a
>minWith f = fold1 choice id
>            where choice a b | f a <  f b = a
>                             | otherwise  = b

\end{mcode}
(The expression `\verb"f a <= f b"' here is a \emph{guard}.
The first equation for \verb"choice" applies if the guard holds,
and the second equation applies if it does not.)

\end{description}

\section{Specifying the problem} \label{sec:spec}

In the paragraph problem, the aim is to lay out a given text 
as a paragraph in a visually pleasing way. A text is given 
as a list of words, each of which is a string, that is, a sequence of
characters: 
\begin{mcode}
 
>type Txt = [Word] 
>type Word = String

\end{mcode}
A paragraph is a sequence of lines, each of which is a sequence of words: 
\begin{mcode}

>type Paragraph = [Line]
>type Line = [Word] 

\end{mcode}
The problem can be specified as
\begin{mcode}

>par0 :: Txt -> Paragraph
>par0 = minWith cost . filter feasible . formats

\end{mcode}
or informally, to compute the minimum-cost format among all 
feasible formats.
(The function \verb"filter p" takes a list \texttt{x} and returns exactly
those elements of \texttt{x} that satisfy the predicate \texttt{p}.)
In the remainder of this section we formalise the three components of
this specification. The result will be an executable program,
but one whose execution takes exponential time.


\subsection{Paragraph formats}

The function \texttt{formats} takes a text and returns all possible formats
as a list of paragraphs:
\begin{mcode}

>formats :: Txt -> [Paragraph]
>formats = fold1 next_word last_word
>          where last_word w = [ [[w]] ]
>                next_word w ps = map (new w) ps ++ map (glue w) ps 

>new w ls      = [w]:ls
>glue w (l:ls) = (w:l):ls

\end{mcode}
(Here, the binary operator \verb"++" is list concatenation.)
That is, for the last word alone there is just one possible format, and for
each remaining word we have the option either of putting it on a new line
at the beginning of an existing paragraph, or of gluing it onto the front of
the first line of an existing paragraph.


\subsection{Feasible paragraph formats}

A paragraph format is feasible if every line fits:
\begin{mcode}

>feasible :: Paragraph -> Bool
>feasible = all fits

\end{mcode}
(The predicate \verb"all p" holds of a list precisely 
when all elements of the list satisfy the predicate \texttt{p}.)

We define a global constant \texttt{maxw} for the maximum line width:
\begin{mcode}

>maxw :: Int
>maxw = 70

\end{mcode}
Of course, for flexibility many of the functions we develop should be
parameterized by \texttt{maxw}; however, a global constant makes the
presentation of the algorithm less cluttered. It is straightforward to
make \texttt{maxw} a parameter throughout.

A line \texttt{fits} if its width is at most the maximum line width:
\begin{mcode}

>fits :: Line -> Bool
>fits xs = (width xs <= maxw)

\end{mcode}
In formatting a text as a paragraph, the maximum line width should
never be exceeded. Our programs will halt with a run-time error
if an individual word exceeds the maximum line width; in practical
applications one would need to deal with such pathological inputs
more graciously.

The \texttt{width} of a line is defined to be its total length when the words
are printed next to each other, with one space character between each
consecutive pair of words:
\begin{mcode}

>width :: Line -> Int
>width = fold1 plus length
>        where plus w n = length w + 1 + n

\end{mcode} 
The function \texttt{length} returns the number of elements in a list.
This notion of width is appropriate for displaying paragraphs on a device
where every character has the same width.
The programs presented below can easily be modified to deal with
proportional spacing, where different characters may have different
widths.
In that case, however, one will need constant-time access arrays,
which are not available in all functional languages.


\subsection{The cost of a paragraph format}

In addition to the maximum line width \texttt{maxw}, the problem also
depends upon an optimum line width \texttt{optw}, another global constant:
\begin{mcode}

>optw :: Int
>optw = 63

\end{mcode}
The optimum line width should of course be
at most the maximum line width. 

A \emph{visually pleasing} paragraph is one in which the width of each
line in the paragraph is as close to the optimum line width
as possible. More precisely, we wish to minimise the total \emph{cost},
the sum of the squares of the deviations of the line widths from the
optimum line width: 
\begin{mcode}

>cost :: Paragraph -> Int
>cost = fold1 plus (const 0)
>       where plus l n = linc l + n
>             linc l = (optw - width l)^2

\end{mcode}
Note that the last line gets treated as a special case: it does not
count towards the total cost of the paragraph. This is achieved by the
function \texttt{const}, which satisfies the equation \verb"const a b = a" for
any \texttt{b}.


\section{The standard algorithm} \label{sec:standard}

The specification in Section~\ref{sec:spec} makes an inefficient program
because it maintains too many candidate solutions. It is a \emph{generate and
test} algorithm, of the form
\begin{verbatim}
  best . filter ok . generate
\end{verbatim}
The standard technique for improving algorithms of this form is to \emph{promote}
the test \verb"filter ok" and the selection \verb"best" inside the
generator, so as to avoid generating unnecessary candidates.
Standard theory \cite{bird86,bird90,bird93d} tells us what properties of \verb"generate",
\verb"ok" and \verb"best" this requires. In our particular case it is
sufficient that
\texttt{new} is monotonic in its second argument:
\begin{eqnarray}
   \verb"cost ls" \le \verb"cost ls'"
   &\implies&
   \verb"cost (new w ls)" \le \verb"cost (new w ls')"
   \label{eqn:new-monotonic}
\end{eqnarray}
and furthermore, that \texttt{glue} is monotonic in its second argument in
a slightly
weaker sense~--- namely, for paragraphs with the same first line:
\begin{eqnarray}
  \hbox to 0.5in{$\verb"cost (l:ls)" \le \verb"cost (l:ls')"$\hss} \nonumber\\
  &\implies&
  \verb"cost (glue w (l:ls))" \le \verb"cost (glue w (l:ls'))"
  \label{eqn:glue-monotonic}
\end{eqnarray}
Note that neither of these two properties depends on the precise definition
of the cost of individual lines, as returned by the function \texttt{linc};
all that matters is that the total cost
is the sum of the costs of the individual lines.

Using the above two monotonicity properties, the standard theory alluded to above
concludes that the 
following dynamic programming algorithm is a valid solution 
to the specification \texttt{par0}. 
This is a well-rehearsed development, so we do
not repeat it here.
\begin{mcode}

>par1 
> = minWith cost . fold1 step start
>   where 
>     step w ps = filter fitH (new w (minWith cost ps):map (glue w) ps)
>     start w   = filter fitH [ [[w]] ]
>fitH = fits . head

\end{mcode}
Note that \texttt{par1} is not equal to \texttt{par0}; in particular, if there
is more than one optimal paragraph, \texttt{par0} and \texttt{par1} may return
different (but still optimal) paragraphs. To express this refinement
property formally we would have to generalize from functional
programming to relational programming~\cite{bdm96}, a step that is
beyond the scope of this paper.

For efficiency, we need to perform a \emph{tupling transformation} 
\cite{pettorossi84,chin90}
to avoid recomputing the width of the first line and the cost of the
remaining candidate solutions. We represent the paragraph \texttt{(l:ls)} 
by the triple 
\begin{verbatim}
  (l:ls, width l, cost ls)
\end{verbatim}
(Because \texttt{ls} may be empty,
we stipulate also that \verb"cost [] = 0".)
The program resulting from this data refinement is as follows.
\begin{mcode}

>par1' :: [[a]] -> [[[a]]]
>par1'
> = the . minWith cost . fold1 step start
>   where 
>     step w ps = filter fitH (new w (minWith cost ps):map (glue w) ps)
>     start w   = filter fitH [([[w]], length w,0)]
>     new w ([l],n,0)   = ([w]:[l], length w, 0)
>     new w p           = ([w]:ls , length w, cost p) where (ls,n,m) = p
>     glue w (l:ls,n,m) = ((w:l):ls, length w + 1 + n, m)
>     the (ls,n,m)      = ls
>     width_hd (ls,n,m) = n
>     cost_tl (ls,n,m)  = m
>     linc_hd p         = (optw - width_hd p)^2
>     cost ([_],_,_)    = 0
>     cost p            = linc_hd p + cost_tl p
>     fitH p            = width_hd p <= maxw

\end{mcode}

\section{Improving the standard algorithm}

The algorithm at the end of Section~\ref{sec:standard}
is the standard dynamic programming solution to the paragraph
problem, and its time complexity is 
$O(min(w n, n^2))$ where $w$ is the maximum number of words on a line and
$n$ is the number of words in the paragraph.
Computational experiments confirm that this is an accurate estimate of
the program's behaviour.
This algorithm is the final product of the typical textbook
derivation. It does not make use of any special properties of
\texttt{linc}, the function that returns the cost on an individual line.
Indeed, if nothing more is known about \texttt{linc}, it is not possible
to improve upon this algorithm,
as noted in \cite{hirschberg87a}.  
However, as we shall show in Sections~\ref{sec:dominance}
to~\ref{sec:differencing}, for our particular choice 
of \texttt{linc}, namely
\begin{verbatim}
 linc l = (optw - width l)^2
\end{verbatim}
substantial improvements are possible; in fact, we will
end up with a program linear in $n$ and independent of $w$.

In Section~\ref{sec:dominance} we determine a \emph{dominance criterion},
whereby some candidate solutions can be discarded because they are dominated
by other `better' solutions. Therefore, after processing each word of
the paragraph we may `trim' the collection of candidate solutions to
remove the dominated ones.

To obtain a linear-time solution we can afford to do at most an
amortized constant amount of work for each word in the paragraph.
Precisely one new candidate solution is added for each word, so it
suffices that the amount of work performed with each word is
proportional to the number of candidate solutions discarded.
The obvious implementation of the trimming operation introduced in
Section~\ref{sec:dominance} involves reconsidering every candidate
solution for each word in the paragraph. In Section~\ref{sec:forecasting}
we show that this is not necessary: trimming in fact results in removing
some candidate solutions from the beginning of the list and some others from
the end. Crucially, the solutions in the middle of the list need
not be considered; at the beginning and at the end of the list, as soon
as one undominated solution is found the trimming can stop.

That still leaves the \verb"filter" and the \verb"map" in the definition
of the function \verb"step", each of which traverses all candidate
solutions for each word of the paragraph. In Section~\ref{sec:filtering} we
replace the \verb"filter" with a function that performs work proportional
to the number of solutions discarded, independently of the number of
solutions retained.
In Section~\ref{sec:differencing} we eliminate the \verb"map (glue w)", by making a
change of representation under which \verb"glue w" is the identity
function. The resulting algorithm is linear in the paragraph length and
independent of the line width.


\section{Dominance criteria} \label{sec:dominance}

In this section we determine a \emph{dominance criterion} whereby some
paragraph formats can be discarded because they are dominated by another;
thus, fewer candidate solutions need be maintained at each step. 
Dominance criteria are the basis for most improvements over straightforward
dynamic programming. 
In our case, the
dominance criterion is a consequence of the fact that
the function \texttt{linc} is \emph{concave},
in the sense that
\marginnote{label more equations, and use in cross-refs}
\begin{eqnarray*}
   \verb"linc (l++m) - linc l" &\le& \verb"linc (k++l++m) - linc (k++l)"
\end{eqnarray*}
Consequently, the monotonicity property of \texttt{glue} 
(Equation~\ref{eqn:glue-monotonic}) can be strengthened to:
\begin{eqnarray}
   \hbox to 0.5in{$\verb"cost (l:ls)" \le \verb"cost ((l++m):ms)"$\hss} \nonumber\\
   &\implies&
   \verb"cost (glue w (l:ls))" \le \verb"cost (glue w ((l++m):ls))"
   \label{eqn:glue-monotonic-stronger}
\end{eqnarray}
In words, the concavity property says that appending a line \verb"m" onto
a line \verb"l" has no worse an effect than appending \verb"m" onto a
longer line \verb"k++l", and the monotonicity property
says that if a paragraph with a shorter
first line is better than another paragraph, it will remain better
when more words are glued to the first lines of both paragraphs~---
a cheaper paragraph with a shorter first line dominates a costlier
paragraph with a longer first line.


\subsection{Exploiting concavity} \label{sec:concavity}

We can exploit the dominance criterion to arrive at an improved definition
of \texttt{step}. 
Note that \verb"step w" maintains the property `is in
strictly increasing order of length of first line' of the list of candidate 
solutions. Now suppose that we have two formats \texttt{p} and \texttt{q}, in that
order, in the list of candidate solutions; 
the first line of \texttt{q} is longer than the
first line of~\texttt{p}. Suppose also that $\verb"cost p" \le \verb"cost q"$. 
Then \verb"p" dominates \verb"q": 
by the monotonicity property of \texttt{new} (Equation~\ref{eqn:new-monotonic})
and the stronger property of \texttt{glue} (Equation~\ref{eqn:glue-monotonic-stronger}),
it follows that \texttt{q} may be safely discarded, because any
candidate solution generated from \texttt{q} will always be beaten by the
candidate solution generated in the same way from \texttt{p}. So we may
improve the definition of \texttt{step} to
\begin{verbatim}
 step w ps = trim (filter fitH (new w (minWith cost ps):map (glue w) ps))
\end{verbatim}
where the function \texttt{trim} discards the dominated
candidate solutions (namely, the formats~\verb"q" for which there is a
format~\verb"p" appearing earlier in the collection with $\verb"cost p"
\le \verb"cost q"$):
\begin{verbatim}
 trim []                             = [] 
 trim [p]                            = [p]
 trim (ps++[p,q]) | cost p <= cost q = trim (ps++[p])
                  | otherwise        = trim (ps++[p]) ++ [q]
\end{verbatim}
This is not a valid definition in Haskell, because patterns
involving \verb"++" are not allowed. However, the pattern-matching is easily
re-expressed in terms of the standard functions \texttt{last} and
\texttt{init}, which return the last element and the remainder of a list,
respectively; we omit the details.
\iffalse
\begin{mcode}

>par2'
> = minWith cost . fold1 step start
>   where 
>     step w ps = trim (filter fitH (new w (minWith cost ps):map (glue w) ps))
>     start w   = filter fitH [ [[w]] ]
>     trim []   = []
>     trim [p]  = [p]
>     trim pspq 
>       | cost p <= cost q = trim psp
>       | otherwise        = trim psp ++ [q]
>       where q   = last pspq
>             psp = init pspq
>             p   = last psp
>             ps  = init psp

\end{mcode}
\fi

Note that we could trim from the back, as here, or from the front.
In Section~\ref{sec:forecasting} we will see that trimming from the back is
the better choice, because we will develop a criterion for stopping trimming early.

\subsection{Trimming introduces order} \label{sec:trim-order}

Now note further that the result of a \texttt{trim} is in strictly
decreasing order of cost, 
so the cheapest candidate solution is the last in the
list. We can therefore improve the definition of \texttt{step} further, by
using \verb"last" instead of \verb"minWith cost":
\begin{verbatim}
 step w ps = trim (filter fitH (new w (last ps):map (glue w) ps))
\end{verbatim}
The resulting algorithm is an improvement over the standard solution,
but it is still not linear because at each step the whole list of
intermediate solutions is traversed.

\section{Forecasting the future} \label{sec:forecasting}

To remedy this inefficiency we will develop a criterion for stopping
trimming before having traversed the whole list of candidate
solutions. Observe that we maintain
a list of paragraph formats with strictly increasing
first line lengths, and strictly decreasing costs. 

Say that a candidate solution
element is \emph{bumped} by its predecessor when it is eliminated
in the computation of \texttt{trim}. Can we forecast how much gluing is needed
before a particular format \texttt{p} is bumped by its predecessor? If so, we
may be able to stop trimming early: if a word shorter than the length
forecasted for \texttt{p} has been glued, then \texttt{p} need not be considered
for trimming.

\subsection{The bump factor}

We introduce a function \verb"cg :: Paragraph -> Int -> Int" (for
`cost-glue') such that
\marginnote{wouldn't it be simpler with a \texttt{cg'} such that 
\texttt{cg'~p~n = cg~p~(n+1)}?}
\begin{verbatim}
 cg p (length w + 1) = cost (glue w p)
\end{verbatim}
One suitable definition of \texttt{cg} is
\begin{verbatim}
 cg [l] n    = 0
 cg (l:ls) n = (optw - (n + width l))^2 + cost ls
\end{verbatim}
In words, \texttt{cg p n} is the total cost of the paragraph formed after a
sequence of words whose width is \texttt{n} has been glued to the first
line of paragraph \texttt{p}. Note that we do not check whether the maximum line
width is exceeded, and so the notion of cost may be meaningless
in terms of paragraphs. 
We allow negative values of \texttt{n} as
arguments of \verb|cg|. 

Using the function \texttt{cg}, we can forecast when a paragraph \texttt{p}
will bump a paragraph \texttt{q} in the process of
gluing more 
words to both paragraphs. Define 
the function \texttt{bf} (for `bump factor') by
\begin{verbatim}
   bf p q
 =  
   setmin {n | cg p n <= cg q n}  `min` (maxw - width (head q) + 1)
\end{verbatim}
(This is not a Haskell definition ---~Haskell does not have sets, and
besides, the quantification is over a potentially infinite set~---
but it does completely determine \texttt{bf}.)
After gluing a width of \texttt{bf p q} to both \texttt{p} and \texttt{q},
paragraph \texttt{p} will be at least as good as \texttt{q}; furthermore, if we glue
more than \texttt{bf p q}, paragraph \texttt{p} will still be as good as \texttt{q}
(by concavity), so \texttt{q} can be discarded. The second term in
the definition of \verb|bf| reflects the fact that after gluing
\verb|maxw - width (head q) + 1|, paragraph \verb|p| is always better
than paragraph \verb|q|, because the first line of \verb|q| exceeds
the maximum line width. It can happen that \verb|bf| returns a negative
number, namely when \verb|p| is better than \verb|q| to start
with. 

\subsection{Using the bump factor} \label{sec:using-bump-factor}

Let us now formalise these observations as properties of
\texttt{glue}. Starting with \texttt{cg}, we have that
\begin{verbatim}
 cg (glue w p) n = cg p (n + 1 + length w)
\end{verbatim}
Consequently, \texttt{bf} satisfies
\begin{verbatim}
 bf (glue w p) (glue w q) = bf p q - (1 + length w)
\end{verbatim}
and therefore
\begin{eqnarray}
  \hbox to 0.5in{$\verb"bf p q" \;<\; \verb"bf r s"$\hss}  \nonumber\\
  &\iff&
  \verb"bf (glue w p) (glue w q)" \;<\; \verb"bf (glue w r) (glue w s)"
  \label{eqn:glue-respects-bf}
\end{eqnarray}
Finally, define the predicate \texttt{better} by
\marginnote{`$\mathtt{better\;w\;p\;q}$' is equivalent to `$\mathtt{1{+}\#w \ge
bf\,p\,q}$'}
\begin{verbatim}
 better w p q  =  cost (glue w p) <= cost (glue w q) || 
                  not (fitH (glue w q))
\end{verbatim}
(The binary operator \verb"||" is boolean disjunction; later on we use
\verb"&&", which is boolean conjunction.)
In words, \verb"better w p q" states that, after gluing a word \texttt{w},
paragraph \texttt{p} will be better than paragraph \texttt{q}, either on grounds
of cost or because the first line of \texttt{q} has become too long.
Suppose \verb|p = l0:ls0|, \verb|q = (l0++l1):ls1|, 
\verb|r = (l0++l1++l2):ls2|, and  $\verb"bf p q" \le \verb"bf q r"$.
We have the following property:
\begin{eqnarray*}
  \verb"better w q r"
  &\implies&
  \verb"better w p q" \;\land\; \verb"better w p r"
\end{eqnarray*}
In words, this property says that whenever \verb|q| gets better than
\verb|r| by gluing a word \verb|w|, 
then \verb|p| is better than both \verb|q| and \verb|r| after
gluing the same word. 
It follows that \verb|q| can never be useful, and therefore, if we
had a triple like \verb|p|, \verb|q| and \verb|r| in the list of
candidate solutions, \verb|q| could be safely discarded. 

\subsection{Tight lists of solutions}

We shall exploit this observation by maintaining the list of candidate
solutions so that, as well as
\begin{enumerate}

\item \label{property:width}
the lengths of the first lines are in strictly increasing order, and

\item \label{property:cost}
the costs of the candidate solutions are in strictly decreasing order,

\end{enumerate}
as before, also
\begin{enumerate} \setcounter{enumi}{2}

\item \label{property:bf}
the \texttt{bf}-values of consecutive adjacent pairs of candidate solutions are
in strictly decreasing order.

\end{enumerate}
Such a list of candidate solutions
we call \emph{tight}.

To see how we can keep the list of candidate solutions tight,
recall the definition of \verb|step| from Section~\ref{sec:trim-order}:
\begin{verbatim}
 step w ps = trim (filter fitH (new w (last ps):map (glue w) ps))
\end{verbatim}
We argued in Section~\ref{sec:concavity} that \verb"step w" maintains
properties~\ref{property:width} and~\ref{property:cost}. We now show how
the third property is maintained.

Notice that all three properties are preserved when taking a subsequence
of (that is, deleting some elements from) the list of candidate
solutions. For properties~\ref{property:width} and~\ref{property:cost}
this is obvious. For property~\ref{property:bf} it follows from the fact
that $\verb"bf p q" \ge \verb"bf p r" \ge \verb"bf q r"$, provided that
$\verb"bf p q" > \verb"bf q r"$ and \texttt{p}, \texttt{q} and \texttt{r} are in
strictly increasing order of length of first line; 
\marginnote{I have a marvellous demonstration that this margin is too
narrow to hold\ldots}
this fact is not too hard to establish.

Suppose that \verb|ps| satisfies property~\ref{property:bf}.
Because \verb|glue| respects the \verb|bf|
order (Equation~\ref{eqn:glue-respects-bf}), the list \verb|map (glue w) ps| also
satisfies property~\ref{property:bf}. 
It is however not necessarily the case that 
\verb"new w (last ps) : map (glue w) ps"
satisfies property~\ref{property:bf}:
the presence of the new element at the beginning may require some of the
initial elements of \verb|map (glue w) ps| to be removed. So, the cons operator
\verb|(:)| in the definition of \verb"step" is replaced by a \emph{smart constructor}
\verb|add|, 
which does the required pruning~--- 
by the argument at the end of Section~\ref{sec:using-bump-factor}, if we
have three consecutive candidate solutions \texttt{p}, \texttt{q} and \texttt{r}
with $\verb"bf p q" \le \verb"bf q r"$, solution \texttt{q} can be discarded.
\begin{verbatim}
 add p []                             = [p]
 add p [q]                            = [p,q]
 add p ([q,r]++rs) | bf p q <= bf q r = add p ([r]++rs)
                   | otherwise        = [p,q,r]++rs
\end{verbatim}
Now the list of candidate solutions \verb"new w (last ps) `add` map (glue w) ps"
satisfies property~\ref{property:bf}. 
Note that \verb"p `add` ps" is a subsequence of \verb"p:ps", so
properties~\ref{property:width} and~\ref{property:cost} are still
maintained; for the same reason, all three
properties are maintained
by the \verb"filter fitH" too.

Now, however, property~\ref{property:bf} permits an optimization of
\texttt{trim}, whereby we can stop trimming early. This was the reason for
introducing bump factors in the first place.
Suppose that \texttt{ps} satisfies property~\ref{property:bf}; we claim that
\verb"trim ps" can be written
\begin{verbatim}
 trim []  = []
 trim [p] = [p]
 trim (ps++[p,q]) | cost p <= cost q = trim (ps++[p])
                  | otherwise        = ps++[p,q]
\end{verbatim}
without a recursive call in the last clause.
Here is the justification.
Suppose that \texttt{r},~\texttt{s} are adjacent
candidate solutions in \texttt{ps}, and \texttt{p},~\texttt{q} are adjacent
candidate solutions in \texttt{ps}, and \texttt{r} is before \texttt{p} in the
list. Then $\verb"bf r s" > \verb"bf p q"$, by property~\ref{property:bf}.
Suppose also that $\verb"cost p" > \verb"cost q"$. Then the word \texttt{w} that
has just been processed was too short for \texttt{p} to bump \texttt{q}, and
so was also too short for \texttt{r} to bump \texttt{s} (because of the
\texttt{bf} ordering); that is, $\verb"cost r" > \verb"cost s"$ too, and
the initial segment of the list of candidate solutions ending
with solution \texttt{q} already satisfies property~\ref{property:cost}.
Thus, we have
\verb"trim (ps++[p,q]) = ps++[p,q]"~--- the second recursive call to
\texttt{trim} can be omitted 
when \verb"ps++[p,q]" satisfies property~\ref{property:bf} and
$\verb"cost p" > \verb"cost q"$.

Because we are now manipulating the list of candidate solutions at both ends, 
it will be profitable
to use a symmetric set of list operations, where \texttt{head} and \texttt{last}
are equally efficient. Such an implementation of lists is
summarized in an appendix to this paper. Below, whenever we use symmetric
lists, the familiar list operations are written using a dash~---
\texttt{head'}, \texttt{init'}, and so on~--- and the type of symmetric lists over
\texttt{a} is written \verb"SymList a".

In outline, the program now is
\begin{verbatim}
 par2 = last . fold1 step start
 step w ps = trim (filter fitH (new w (last ps) `add` map (glue w) ps))
\end{verbatim}
\iffalse
\begin{mcode}

>par2
> = last . fold1 step start
>   where 
>     step w ps = trim(filter fitH (new w (last ps) `add` map (glue w) ps))
>     start w   = filter fitH [ [[w]] ]
>     add p []                          = [p]
>     add p [q]                         = [p,q]
>     add p (q:r:rs) | bf p q <= bf q r = add p (r:rs)
>                    | otherwise        = p:q:r:rs
>     bf p q
>       | single q && cost pt == 0 
>                   = (optw - wph) `min` rqh
>       | single q  = rqh
>       | otherwise = ceildiv (cost p - cost q) (2*(wqh-wph)) `min` rqh
>         where ph:pt = p
>               qh:qt = q
>               wph   = width ph
>               wqh   = width qh
>               rqh   = maxw - wqh + 1
>               ceildiv n m = (n+m-1) `div` m
>     trim []                      = []
>     trim [p]                     = [p]
>     trim pspq | cost p <= cost q = trim psp
>               | otherwise        = pspq
>       where q   = last pspq
>             psp = init pspq
>             p   = last psp
>             ps  = init psp

\end{mcode}
\fi
(Note that we have made use again of the fact that a tight list of
paragraphs is in strictly decreasing order of cost, replacing the
\verb"minWith cost" in \verb"par2" by \verb"last".)
This new  program is in fact quite efficient:
computational experiments 
show that at each step of the computation, only a very small 
number of candidate solutions
are kept. Still, all candidate solutions get inspected each time \texttt{step}
is evaluated, and this remains a source of inefficiency.
To make progress, we shall have to remove the subexpressions
\texttt{filter fitH} and \texttt{map (glue w)} from the definition of \texttt{step};
we do this in Sections~\ref{sec:filtering} and~\ref{sec:differencing}.

\subsection{Computing the bump factor}

One point that we have not touched upon is how \texttt{bf} can be efficiently
implemented. This is an exercise in high-school algebra. 
Note that, when \texttt{p} and \texttt{q} appear in that order in the list of
candidate solutions, \texttt{p} cannot be a singleton: 
there is just one
way of formatting a paragraph into a single line, and if that line fits
it will be the last candidate solution because it has the longest first line.
Therefore there are just two cases to consider in computing \verb"bf p q": 
when \texttt{q} is a singleton, and when \texttt{q} is not.
Recall the definition of \verb"bf":
\begin{eqnarray*}
  \hbox to 2em{\rlap{\texttt{bf p q}}} \\
  &=&
  \verb"setmin {n | cg p n <= cg q n} `min` (maxw - width (head q) + 1)"
\end{eqnarray*}
Note that the second term 
$\verb"rqh" = \verb"maxw - width (head q) + 1"$ is always greater than zero.
\begin{description}

\item[Case \texttt{q} is a singleton:]
so \verb"cg q n" is zero for any \texttt{n}. Thus, the only value of \texttt{n} for
which $\verb"cg p n" \le \verb"cg q n"$ would be one for which 
\verb"cg p n" is zero; this can only happen when
$\verb"n" = \verb"optw - width (head p)"$ and $\verb"cost (tail p)" = \verb"0"$.
This case therefore splits into two subcases: 
if \verb"cost (tail p)" is zero, then \verb"bf p q" is the smaller of
\texttt{rqh} and \verb"optw - width (head p)";
otherwise, there are no suitable values of \texttt{n}, and \verb"bf p q" is
simply \texttt{rqh}.

\item[Case \texttt{q} is not a singleton:]
Note that, for non-singleton \texttt{p},
\begin{eqnarray*}
  \verb"cg p n"  &=&  \verb"cost p + n^2 - 2*n*(optw - width (head p))"
\end{eqnarray*}
and so $\verb"cg p n" \le \verb"cg q n"$ precisely when
\begin{eqnarray*}
  \texttt{n} &\ge& \verb"(cost p-cost q)/(2*(width (head q)-width (head p)))"
\end{eqnarray*}
(the divisor is non-zero, because of the ordering on lengths
of first lines).
Therefore, the first term in \verb"bf p q" is the ceiling of the fraction
on the right-hand side of this inequation, and \verb"bf p q" itself is
the smaller of this first term and \texttt{rqh}.

\end{description}
Thus, we can implement \texttt{bf} as follows:
\begin{verbatim}
 bf p q 
   | single q && cost pt == 0 = (optw - wph) `min` rqh
   | single q                 = rqh
   | otherwise                = ceildiv (cost p - cost q) 
                                        (2*(wqh - wph)) `min` rqh
      where
        ph:pt = p
        qh:qt = q
        wph   = width ph
        wqh   = width qh
        rqh   = maxw - wqh + 1
\end{verbatim}
where \verb"ceildiv x y" rounds the fraction \verb"x/y" up to the next integer.

\section{Filtering} \label{sec:filtering}

Because the list of candidate paragraphs is kept in increasing order of
length of the first line, the \texttt{filter} is easily dealt with. The net
effect of filtering is that the last few formats (namely, those with the
longest first lines) are discarded, and the remainder are
retained. Therefore, instead of \verb"filter fitH" we can use 
\verb"droptail (not . fitH)", where
\begin{verbatim}
 droptail :: (a->Bool) -> [a] -> [a]
 droptail p []                    = []
 droptail p (xs++[x]) | p x       = droptail p xs
                      | otherwise = xs ++ [x]
\end{verbatim}
Informally, \verb"droptail p x" discards elements from the end of the
list~\verb"x", stopping when the list is empty or the last element does not
satisfy predicate~\verb"p".
\iffalse
\begin{mcode}

>par2''
> = last . fold1 step start
>   where
>     step w ps = trim(droptail (not.fitH) (new w (last ps) `add` map (glue w) ps))
>     start w   = droptail (not.fitH) [ [[w]] ]
>     droptail p []              = []
>     droptail p xsx | p x       = droptail p xs
>                    | otherwise = xsx
>       where x  = last xsx
>             xs = init xsx
>     add p []                          = [p]
>     add p [q]                         = [p,q]
>     add p (q:r:rs) | bf p q <= bf q r = add p (r:rs)
>                    | otherwise        = p:q:r:rs
>     bf p q
>       | single q && cost pt == 0 
>                   = (optw - wph) `min` rqh
>       | single q  = rqh
>       | otherwise = ceildiv (cost p - cost q) (2*(wqh-wph)) `min` rqh
>         where ph:pt = p
>               qh:qt = q
>               wph   = width ph
>               wqh   = width qh
>               rqh   = maxw - wqh + 1
>               ceildiv n m = (n+m-1) `div` m
>     trim []                      = []
>     trim [p]                     = [p]
>     trim pspq | cost p <= cost q = trim psp
>               | otherwise        = pspq
>       where q   = last pspq
>             psp = init pspq
>             p   = last psp
>             ps  = init psp

\end{mcode}
\fi

\section{Differencing} \label{sec:differencing}

In this section we will get rid of the \verb"map" from the
definition of \verb"step", by making a change of representation under
which \verb"glue w" is the identity function. 
If we assume that the list
operations \verb"head", \verb"tail", \verb"init" and \verb"last" take
amortized constant time, then this gives an amortized linear-time
algorithm for paragraph formatting. Every word of the paragraph
contributes at most one new candidate solution, and the amount of work
performed (by \verb"add" and \verb"trimf") on the list of
candidate solutions is proportional to the number of candidate solutions
discarded.

\subsection{A data refinement}

Elimination of \texttt{glue} can be achieved by computing only the tail
of each paragraph. As long as we have the original text available
(which is the concatenation of the paragraph), 
all necessary quantities can be computed in terms of the tail alone:
\begin{verbatim}
 length (head p) = length (concat p) - length (concat (tail p))
 width (head p)
   | single p    = width (concat p)
   | otherwise   = width (concat p) - width (concat (tail p)) - 1
 cost p
   | single p    = 0
   | otherwise   = (optw - width (head p))^2 + cost (tail p)
\end{verbatim}
(Recall that we stipulated that \texttt{cost [] = 0}.)
The exploitation of this type of equation is known as \emph{differencing}.
We shall represent a
paragraph \texttt{p} by the triple \verb"rep p" where
\begin{verbatim}
  rep p = ((width.concat.tail) p, (cost.tail) p, (length.concat.tail) p)
\end{verbatim}
It will be useful to have a type synonym
for the new representation of paragraphs:
\begin{mcode}

>type Par    = (Width,Cost,Length) 
>type Width  = Int
>type Cost   = Int
>type Length = Int

>width_tl = fst3
>cost_tl  = snd3
>len_tl   = thd3

\end{mcode}
Here, the functions \texttt{fst3}, \texttt{snd3} and \texttt{thd3} return the first,
second and third components of a triple, respectively.
\begin{mcode}

>fst3 (a,b,c) = a
>snd3 (a,b,c) = b
>thd3 (a,b,c) = c

\end{mcode}
On this representation, the function \texttt{glue w} is the identity function,
as required. 

\subsection{The overall structure}

Before we go into the details of the implementation of other
operators on paragraphs, we outline the structure of the final
program.

The program presented below is based on the fact that
a solution to \texttt{par0} is returned by \texttt{par3}, where
\begin{verbatim}
 par3 :: Txt -> Paragraph
 par3 ws 
   = tile ws (map (length.concat.tail.par0) (tails ws), length ws)
\end{verbatim}
The function \texttt{tile xs} produces the required solution by
exploiting the differencing equation for \verb"length . head":
\begin{mcode}

>tile :: Txt -> ([Length],Length) -> Paragraph
>tile ws ([],n)   = []
>tile ws (m:ms,n) = ws1 : tile ws2 (drop l (m:ms),m)
>                   where l = n - m
>                         (ws1,ws2) = splitAt l ws

\end{mcode}
(Here, 
\verb"splitAt l x" is a pair of lists, the first element of
the pair being the first \texttt{l} elements of \texttt{x} and the second element
being the remainder;
\verb"drop l x" is the second component of \verb"splitAt l x".)
The proof that this works is an induction over all tails of the argument,
and a detailed exposition can be found in \cite{bird93d}. It is perhaps
interesting to note that a program involving \texttt{tile} 
is the starting point
for the paper by Hirschberg and Larmore \cite{hirschberg87a}; 
for us, it is part of a
final optimisation.

Adapting the algorithm developed in previous sections to the new
representation of paragraphs, one
can find functions 
\texttt{stepr} and \texttt{startr} ---~data refinements of 
\texttt{step} and \texttt{start}~--- such that
\begin{verbatim}
   fold1 stepr startr (map length ws)
 =
   (map rep (fold1 step start ws), width ws, length ws)
\end{verbatim}
and so, by the scan lemma of the introductory section,
which showed how the computation of \verb|fold1 f g| on all tails
can be written in terms of \verb|scan1|,
\begin{verbatim}
   scan1 stepr startr (map length ws)
 =
   zip3 (map (map rep . fold1 step start) (tails ws), 
         map width (tails ws), 
         map length (tails ws))
\end{verbatim}
(The function \texttt{zip3} `zips' in the obvious way a triple of lists, all of the
same length, into a list of triples.)

Let \verb"zs = scan1 stepr startr (map length ws)". Then
\begin{verbatim}
   length ws = thd3 (head zs)
\end{verbatim}
and
\begin{verbatim}
   map (length . concat . tail . par0) (tails ws)
 =   { rep }
   map (len_tl . rep . par0) (tails ws)
 =   { par0 = last . fold1 step start }
   map (len_tl . last . map rep . fold1 step start) (tails ws)
 =   { above }
   map (len_tl . last . fst3) zs
\end{verbatim}   
The resulting program is below.
(Recall that the dashed list operations are the operations on symmetric
lists, defined in the appendix.)
\begin{mcode}

>par3 :: Txt -> Paragraph 
>par3 ws
> = tile ws (map (len_tl.last'.fst3) zs, thd3 (head zs))
>   where zs = scan1 stepr startr (map length ws)

\end{mcode}




\subsection{Implementing the data refinement}

It remains to give appropriate definitions of \texttt{stepr} and \texttt{startr}.
The definition of \texttt{startr} is 
\begin{mcode}

>startr :: Length -> (SymList Par, Width, Length)
>startr a | a <= maxw = (cons' (0,0,0) nil',a,1)

\end{mcode}

The definition of \texttt{stepr} mirrors that in the preceding section,
except that all operations on paragraphs have been data-refined  
to the new representation of paragraphs.
Those modifications are justified by
the differencing equations stated above,
and the following definitions are 
immediate consequences of those identities:
\begin{mcode}

>stepr :: Length -> 
>        (SymList Par, Cost, Length) -> 
>        (SymList Par, Cost, Length)
>stepr w (ps,tw,tl)  
> = (trim (drop_nofit (new (last' ps) `add` ps)), tot_width, tot_len)
>   where 
>     single p      = len_tl p == 0
>     cost p 
>       | single p  = 0
>       | otherwise = cost_tl p + (optw - width_hd p)^2
>     width_hd p
>       | single p  = tot_width
>       | otherwise = tot_width - width_tl p - 1
>     tot_width     = w + 1 + tw
>     tot_len       = 1 + tl

\end{mcode}
The operator \texttt{new} adds a new line to the front of a paragraph.
It is important that, in computing the cost of the tail of the 
newly created paragraph, we use the old width of the head, that is,
without taking the new word \texttt{w} into account:
\begin{mcode}

>     new p | single p  = (tw,0,tl)
>           | otherwise = (tw,cost_tl p + (optw-old_width_hd p)^2,tl)
>     old_width_hd p | single p  = tw
>                    | otherwise = tw - width_tl p - 1

\end{mcode}
The definition of \texttt{trim} is not changed at all:
\begin{mcode}

>     trim ps_pq | null' ps_pq      = ps_pq
>                | single' ps_pq    = ps_pq
>                | cost p <= cost q = trim ps_p
>                | otherwise        = ps_pq
>                  where ps_p = init' ps_pq
>                        q    = last' ps_pq
>                        p    = last' ps_p

\end{mcode}
whereas \verb"drop_nofit" is an implementation of 
\verb"droptail (not . fitH)", using the new implementation \verb"width_hd"
of \verb"width . head". 
\begin{mcode}

>     drop_nofit ps_p | null' ps_p        = ps_p
>                     | width_hd p > maxw = drop_nofit ps
>                     | otherwise         = ps_p
>                       where ps = init' ps_p
>                             p  = last' ps_p

\end{mcode}
The definition of \texttt{add} is similarly unaffected.
On an intuitive level, there seems to be a duality between
the ways \texttt{trimf} and \texttt{add} operate, but we have been unable
to bring this out in the code, 
partly because \texttt{trimf} also performs
the filtering operation.
\marginnote{also because \texttt{add} compares \texttt{f p} with \texttt{f q},
whereas \texttt{trim} compares \texttt{f p q} with \texttt{f q r}}
\begin{mcode}

>     add p qr_rs | single' qr_rs || null' qr_rs = cons' p qr_rs
>                 | bf p q <= bf q r             = add p r_rs
>                 | otherwise                    = cons' p qr_rs
>                   where r_rs = tail' qr_rs
>                         q  = head' qr_rs
>                         r  = head' r_rs

\end{mcode}
Finally, the data-refined version of \texttt{bf} becomes
\begin{mcode}

>     bf p q 
>       | single q && cost_tl p == 0 = (optw - wph) `min` rqh 
>       | single q                   = rqh
>       | otherwise                  = ceildiv (cost p-cost q) 
>                                              (2*(wqh-wph)) `min` rqh
>          where
>            wph = width_hd p
>            wqh = width_hd q
>            rqh = maxw - wqh + 1

>ceildiv n m = (n+m-1) `div` m

\end{mcode}

It is not hard to check that program \texttt{par3} does indeed
have (amortised) linear time complexity. This theoretical bound
is confirmed in computational experiments, and for all but the
smallest inputs, \texttt{par3} outperforms the standard algorithm
\texttt{par1}.

\section{Haskell vs C++}

We now have the ingredients for writing a program that has the
same functionality as the Unix utility \emph{fmt},
although its output will be far superior (\emph{fmt}
uses a naive greedy strategy, and the resulting
paragraphs are \emph{not} visually pleasing). 
We shall make use of the functions 
\begin{verbatim}
 parse :: String -> [Paragraph]
 unparse :: [Paragraph] -> String
\end{verbatim}
which are well-known text-processing primitives in
functional programming \cite{bird88}. Their
definitions are included in an appendix to this paper.
Using these primitives, our implementation of \texttt{fmt}
takes a single line:  
\begin{mcode} 

>fmt = unparse . map (par3 . concat) . parse

\end{mcode}
\iffalse
\begin{mcode}

>fmtWith par = unparse . map (par . concat) . parse

\end{mcode}
\fi
Joe Programmer 
may not be happy about this implementation of a high-quality \texttt{fmt}.
Although there is
no algorithm gap, one might expect a \emph{performance gap} between
the Haskell program and an implementation of the same algorithm
in a more conventional language.
To measure the performance gap we compared the
Haskell program for \texttt{fmt} to a hand-coded C++
implementation that is in close correspondence to the program
presented here. 
The conventional program in C++ does make extensive use
of destructive updates, however, 
and the implementation of symmetric lists is replaced by an array implementation.
Because the program only makes use of {\em cons} (and not of {\em snoc}), we can
implement it by declaring an array whose size is an upperbound on the number of
words in a paragraph, with two pointers that indicate the beginning and end of
the symmetric list. (If we also had a {\em snoc} operation, we would need to
use the folklore circular array code for queues.) All data structures in the conventional
program are therefore of fixed size. Appropriate size bounds were determined
by experimentation.
The conventional program is of course longer than 
the Haskell program, but this is mostly due to the unwieldy syntax,
as the difference is only a factor of one third. Personally we
found the 
conventional code much harder to write because it uses a lot of indexing
in arrays, as opposed to the standard list processing functions in 
Haskell. 

In writing the C++ code, we attempted to apply all the standard tricks that good C++ programmers
employ to speed up their programs. For example, index calculations were avoided through use
of pointers, and we provided ample hints to the compiler through {\em const} 
declarations and {\em inline} directives. To check that we did indeed conform to
good practice in writing the C++ program, we compared its performance to that of
the \verb|fmt| utility: our code for the sophisticated algorithm is only 18% 
slower than \verb|fmt|. 
By contrast, the Haskell program in this paper
has {\em not} been fine-tuned for performance at all, and we directly compiled the
\LaTeX source of this paper.
It follows that the performance measurements reported give an edge to C++. 

All three programs were compiled on a Pentium II processor, running RedHat Linux.
For Haskell we used version 4.01 of the Glasgow compiler \texttt{ghc}, 
because it
produces the best code of all Haskell compilers available. 
The same code was also compiled with \texttt{hbc}, which also has a good
reputation for speed and reliability. 
For C++ we used the Gnu compiler.
All three executables were reduced in size using the utility \texttt{strip}.
The Haskell executables are, as expected, vastly larger than the
C++ code ---~they differ by about a factor of 25.
In all cases we switched on all optimizers. This has a spectacular
effect for the \texttt{ghc} program: it ran more than four times faster
than without the optimisation switch. Indeed, this is were we claw back some
of the gains obtained by hand-coded optimisations in the C++ code: the \texttt{ghc}
compiler aggressively applies optimising program transformations \cite{peyton-jones98}. 


To compare the performance of the two executables, we 
formatted the full text of Thomas Hardy's
\emph{Far from the madding crowd}, an ASCII file of approximately
780Kb \cite{gutenberg}. The three programs were run to format this file 
for a maximum line width of 70 characters and an optimum width of 63.
The CPU time was measured
using the \texttt{time} command provided by the Linux \texttt{bash} shell. 
The \texttt{ghc} executable is about twice as fast as the \texttt{hbc} program,
 which is shows how much can be achieved by automatic transformation of
Haskell programs. The C++ program is eleven times faster
again, which reflects the effort put into the respective compilers, and
the fact that we did not bother to fine-tune the Haskell code.

The table below summarises the above comparison: the first two columns
compare
the programs with respect to their textual length (lines and characters),
the third column is the
size of the executable (in Kbytes), and the last column shows 
their execution time (in CPU seconds).
\begin{center}
\begin{tabular}{l|rrrrr}
                 & lines & chars & size (Kb) & time (s) \\\hline
Haskell (hbc)    &   183 &  4676 &    196~~~ &  10.34~~~ \\
Haskell (ghc)    &   183 &  4676 &    453~~~ &  4.72~~~  \\
C++              &   416 &  6310 &      8~~~ &  0.43~~~~ \\\hline
Haskell (hbc)/(ghc) &  1.00 &  1.00 &   2.31~~~ &   2.19~~~  \\
Haskell (ghc)/C++   &  0.44 &  0.74 &   24.50~~~ &  11.27~~~
\end{tabular}
\end{center}
In summary, the performance gap is not all that great;
it furthermore seems likely that advances in compiler technology
(illustrated by the difference between \texttt{hbc} and \texttt{ghc})
will cancel the remaining advantages of languages like 
C++ over Haskell in the next few years. 

\section{Discussion}

This paper was an experiment in using a functional
language for presenting a non-trivial algorithm in a semi-formal
style. We personally believe that for a large class of problems,
this style of presentation is adequate, at once closing the
algorithm gap and reconciling algorithm design with formal methods.
The comparison with the hand-coded conventional implementations indicates that
for non-trivial algorithms like the one presented here, the performance 
gap is rather small too.
There are, however, two unsatisfactory aspects of the material
presented here:

\begin{itemize}
\item First, we are not entirely satisfied with the semi-formal style of
this paper. Up to the introduction of \texttt{trim}, 
the program derivation is absolutely
standard, and no invention is involved in 
synthesizing the program. That part of the paper could easily be
cast in calculational form, given the right machinery.
The invention of the `bump factor', and its role
in `forecasting the future', is however rather \emph{ad hoc}, and
escapes, at present, an elegant calculational treatment. This is
unsatisfactory, especially since
the technique seems more generally applicable.

\item Second, we are very dissatisfied with the way one has to program 
differencing in
a functional language.
In a sense this is the least interesting part of the programming process,
and yet it is quite error-prone. Moreover, differencing destroys some of the
delightful elegance that characterises the functional expression of
the standard algorithm. Meta-programming features in the spirit of 
Paige's \texttt{invariant} construct \cite{paige86} 
such as those espoused by Smith~\cite{smith90} and Liu~\cite{liu95}
might be used to 
circumvent this problem, but
unfortunately we do not know of any modern functional language that
supports those ideas.
\end{itemize}


Finally, the algorithm presented here is representative of a large
class of ingenious algorithms, collectively known under the name
\emph{sparse dynamic programming}
\cite{eppstein92b}. It would be nice to see whether 
a generic treatment of this class of algorithms is possible, in
the style of \cite{demoor95}. It seems that such a generic approach
is within reach, but we have not investigated this in any depth. 

\iffalse
  \bibliographystyle{plain}
  \bibliography{new}
\else
  \begin{thebibliography}{10} \raggedright

  \bibitem{bird86}
  R.~S. Bird.
  \newblock Transformational programming and the paragraph problem.
  \newblock {\em Science of Computer Programming}, 6(2):159--189, 1986.

  \bibitem{bird90}
  R.~S. Bird.
  \newblock A calculus of functions for program derivation.
  \newblock In D.~A. Turner, editor, {\em Research Topics in Functional
    Programming}, University of Texas at Austin Year of Programming Series, pages
    287--308. Addison--Wesley, 1990.

  \bibitem{bird93d}
  R.~S. Bird and O.~De~Moor.
  \newblock List partitions.
  \newblock {\em Formal Aspects of Computing}, 5(1):61--78, 1993.

  \bibitem{bdm96}
  R.~S. Bird and O.~De~Moor.
  \newblock \emph{Algebra of Programming}.
  \newblock International Series in Computer Science. Prentice--Hall, 1996.

  \bibitem{bird88}
  R.~S. Bird and P.~Wadler.
  \newblock {\em Introduction to Functional Programming}.
  \newblock International Series in Computer Science. Prentice--Hall, 1988.

  \bibitem{chin90}
  W.~N. Chin.
  \newblock {\em Automatic Methods for Program Transformation}.
  \newblock Ph.\,D. thesis, Imperial College, London, 1990.

  \bibitem{demoor95}
  O.~De~Moor.
  \newblock A generic program for sequential decision processes.
  \newblock In M.~Hermenegildo and D.~S. Swierstra, editors, {\em Programming
    Languages: Implementations, Logics, and Programs}, volume 982 of {\em Lecture
    Notes in Computer Science}. Springer--Verlag, 1995.

  \bibitem{eppstein92b}
  D.~Eppstein, Z.~Galil, R.~Giancarlo, and G.~F. Italiano.
  \newblock Sparse dynamic programming \uppercase{II}: Convex and concave cost
    functions.
  \newblock {\em Journal of the ACM}, 39(3):546--567, 1992.

  \bibitem{galil89}
  Z.~Galil and R.~Giancarlo.
  \newblock Speeding up dynamic programming with applications to molecular
    biology.
  \newblock {\em Theoretical Computer Science}, 64:107--118, 1989.

  \bibitem{hirschberg87a}
  D.~S. Hirschberg and L.~L. Larmore.
  \newblock The least weight subsequence problem.
  \newblock {\em SIAM Journal on Computing}, 16(4):628--638, 1987.

  \bibitem{hirschberg87b}
  D.~S. Hirschberg and L.~L. Larmore.
  \newblock New applications of failure functions.
  \newblock {\em Journal of the Association for Computing Machinery},
    34(3):616--625, 1987.

  \bibitem{hoogerwoord92}
  R.~R. Hoogerwoord.
  \newblock A symmetric set of efficient list operations.
  \newblock {\em Journal of Functional Programming}, 2(4):505--513, 1992.

  \bibitem{knuth81}
  D.~E. Knuth and M.~F. Plass.
  \newblock Breaking paragraphs into lines.
  \newblock {\em Software: Practice and Experience}, 11:1119--1184, 1981.

  \bibitem{liu95}
  Y.~A.~Liu and T.~Teitelbaum.
  \newblock Systematic derivation of incremental programs.
  \newblock {\em Science of Computer Programming},
    24(1):1--39, 1995.

  \bibitem{morgan94}
  C.~C. Morgan.
  \newblock {\em Programming from Specifications}.
  \newblock International Series in Computer Science. 2nd edition,
    Prentice--Hall, 1994.

  \bibitem{paige86}
  R.~Paige.
  \newblock Programming with invariants.
  \newblock {\em IEEE Software}, 3(1):56--69, 1986.

  \bibitem{pettorossi84}
  A.~Pettorossi.
  \newblock {\em Methodologies for Transformations and Memoing in Applicative
        Languages}.
  \newblock Ph.\,D.\ thesis, Department of Computer Science, Edinburgh, 1984.

  \bibitem{smith90}
  D.~R.~Smith.
  \newblock KIDS: A Semi-Automatic Program Development System.
  \newblock {\em IEEE Transactions on Software Engineering},
    16(9):1024--1043, 1990.

  \bibitem{gutenberg}
  Gutenberg Project.
  \newblock Available by ftp from: {\tt mrcnext.cso.uiuc.edu}.
  \newblock Many original texts as ASCII files, 1994.

  \end{thebibliography}
\fi

\section*{Appendix: Symmetric lists}

The implementation of symmetric lists given below is explained in some
depth in \cite{hoogerwoord92}. Briefly, a 
list \texttt{x} is represented as a pair
of lists \texttt{(y,z)} such that \verb"abs (y,z)"~=~\verb"x", where the
\emph{abstraction function} \texttt{abs} is defined by
\begin{verbatim}
 abs (y,z) = y ++ reverse z
\end{verbatim}
Moreover, the following invariant is maintained:
if either of the two lists is empty, the other is
empty or a singleton.

The operations below implement their non-symmetric counterparts
in the sense that
\begin{verbatim}
 head' = head . abs
 abs . tail' = tail . abs
\end{verbatim}
and so on. The implementation is such that each operation takes
amortised constant time.

\begin{mcode}

>type SymList a = ([a],[a])

>single' (x,y) = (null x && single y) || (single x && null y)

>null' ([],[]) = True
>null' _       = False

>nil' = ([],[])

>head' (x,y) | not (null x) = head x
>            | otherwise = head y

>last' (y,x) | not (null x) = head x
>            | otherwise = head y

>cons' a (x,y) | not (null y) = (a:x,y)
>              | otherwise = ([a],x)

>snoc' a (y,x) | not (null y) = (y,a:x)
>              | otherwise = (x,[a])

>tail' (x,y) | null x    = ([],[])
>            | single x  = (reverse y1, y0)
>            | otherwise = (tail x, y)
>              where (y0,y1) = splitAt (length y `div` 2) y

>init' (y,x) | null x    = ([],[])
>            | single x  = (y0, reverse y1)
>            | otherwise = (y, tail x)
>              where (y0,y1) = splitAt (length y `div` 2) y

\end{mcode}

\section*{Appendix: Text processing}

The text processing package given below 
is explained in \cite{bird88}.
It provides primitives for converting between strings and lines,
lines and words, and paragraphs and lines. In each case, the forward
direction can be programmed using the generic solution \texttt{format},
and the backward conversion using \texttt{unformat}. The definitions of
\texttt{unlines}, \texttt{lines}, \texttt{unwords} and \texttt{words} have been
commented out because they are already defined in the standard
Haskell prelude. The function \texttt{id} is the identity function.
\begin{mcode}

>unformat :: a -> [[a]] -> [a]
>unformat a = fold1 insert id
>         where insert xs ys = xs ++ [a] ++ ys

>format :: Eq a => a -> [a] -> [[a]]
>format a [] = [[]]
>format a x  = fold1 (break a) (start a) x
>       where break a b xs | a == b    = []:xs
>                          | otherwise = (b:head xs):tail xs
>             start a b = break a b [[]]

*unlines = unformat '\n'
*lines = format '\n'

*unwords = unformat ' '
*words = filter (/=[]) . format ' '

>unparas :: [[[String]]] -> [[String]]
>unparas = unformat []

>paras :: [[String]] -> [[[String]]]
>paras   = filter (/=[]) . format []

>parse    = paras . map words . lines
>unparse  = unlines . map unwords . unparas

\end{mcode}

\end{document} 

The simple greedy algorithm:

>parg :: Txt -> Paragraph
>parg = foldl nextword [[]]
>  where
>    nextword p w | fits (last p++[w]) = init p ++ [last p ++ [w]]
>                 | otherwise = p ++ [[w]]
>fmtg = fmtWith parg



For comparison, the quadratic algorithm:

>fmt1 = fmtWith par1


Some test data:

>test = 
>  "In the constructive programming community it is commonplace to see " ++
>  "formal developments of textbook algorithms. In the algorithm design " ++
>  "community, on the other hand, it may be well known that the textbook " ++
>  "solution to a problem is not the most efficient possible. However, in " ++
>  "presenting the more efficient solution, the algorithm designer will " ++
>  "usually omit some of the implementation details, this creating an " ++
>  "algorithm gap between the abstract algorithm and its concrete " ++
>  "implementation. This is in contrast to the formal development, which " ++
>  "usually presents the complete concrete implementation of the less " ++
>  "efficient solution. \n \n"

>tests = concat (repeat test)

>main = getArgs >>= (\as ->
>       if length as /= 1
>       then putStr "usage: para <file name>"
>       else openFile (head as) ReadMode >>= (\h ->
>            hGetContents h >>= (\ws ->
>            putStr (if null ws then [] else (fmt ws)))))




