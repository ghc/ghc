{-

This test runs for a Long Time (10mins for the registerised version)
and allocates 3.4Gbytes.  It also hammers the GC; with -H16M it spend
40% of the time in the GC.



Date: Sun, 25 Oct 92 16:38:12 GMT
From: Julian Seward (DRL PhD) <sewardj@uk.ac.man.cs>
Message-Id: <9210251638.AA21153@r6b.cs.man.ac.uk>
To: partain@uk.ac.glasgow.dcs
Subject: Space consumption in 0.09 produced binary
Cc: sewardj@uk.ac.man.cs, simonpj@uk.ac.glasgow.dcs

Folks,

At the risk of wasting even more of your valuable time, here is
a small problem I ran into:

The program (XXXX.lhs) listed below runs in constant space (about 4k)
in both Gofer and hbc 0.998.5.  When compiled with 0.09, it runs out
of heap in seconds (4 meg heap).

The program builds a gigantic list of things (CDSs, in fact), I believe
at least 100,000 long, and searches to find out if a particular CDS is
present.  The CDS list is generated lazily, and should be thrown away
as it goes, until apply_cds is found (see the bottom of the listing).
Gofer and hbc behave as expected, but I suspect ghc is holding onto 
the complete list unnecessarily.

I include XXXX.stat as supporting evidence.

Jules

(compiled hence:
ghc9 -v -O -gc-ap -flet-float -Rgc-stats -Rmax-heapsize 14M -o XXXX XXXX.lhs
)

-----------------------------------------------------------------------

XXXX +RTS -S 

Collector: APPEL  HeapSize: 4,194,304 (bytes)

  Alloc   Live   Live   Astk   Bstk OldGen   GC    GC     TOT     TOT  Page Flts  Collec  Resid
  bytes   bytes    %   bytes  bytes  roots  user  elap    user    elap   GC  TOT   tion   %heap
2097108 1119672  53.4     52    132 1119616  0.33  0.35    1.01    1.15    0    0   Minor
1537300  918200  59.7     48    128 918188  0.26  0.31    1.76    1.95    0    0   Minor
1078216  654212  60.7     56    160 652612  0.19  0.18    2.29    2.46    0    0   Minor
 751108  442140  58.9     52    108 442140  0.12  0.12    2.64    2.84    0    0   Minor
3134224 2935044  93.6     52    108         1.49  1.50    4.13    4.34    0    0  *MAJOR* 70.0%
 629612  376848  59.9     52    132 376836  0.11  0.11    4.44    4.64    0    0   Minor
 441184  265100  60.1     96    200 264416  0.08  0.07    4.66    4.86    0    0   Minor
 308640  204072  66.1     56    160 199476  0.06  0.05    4.81    5.01    0    0   Minor
3781064 3687092  97.5     56    160         1.81  1.85    6.62    6.86    0    0  *MAJOR* 87.9%
 253600  160584  63.3     52    108 160584  0.05  0.04    6.75    6.98    0    0   Minor
 173312  112344  64.8     56    160 110304  0.03  0.03    6.83    7.07    0    0   Minor
 117128   77260  66.0     36    140  74112  0.01  0.02    6.88    7.13    0    0   Minor
4037280 3985284  98.7     36    140         1.96  1.98    8.85    9.11    0    0  *MAJOR* 95.0%

-------------------------------------------------------------------------
-}

> module Main where

%============================================================
%============================================================

\section{A CDS interpreter}

\subsection{Declarations}

Second attempt at a CDS interpreter.  Should do
loop detection correctly in the presence of higher order functions.

The types allowed are very restrictive at the mo.

> data Type = Two
>           | Fn [Type]

Now, we also have to define CDSs and selectors.
\begin{itemize}
\item
@Empty@ is a non-legitimate CDS, denoting no value at all.  We use
it as an argument in calls to other CDSs denoting that 
the particular argument is not really supplied.
\item
@Par@ is similarly a non-legit CDS, but useful for constructing
selectors.  It simply denotes the parameter specified (note
parameter numbering starts at 1).
\item
@Zero@ and @One@ are constant valued CDSs.
\item 
@Call@.
Calls to other functions are done with @Call@, which expects
the callee to return @Zero@ or @One@, and selects the relevant
branch.  The @Tag@s identify calls in the dependancy list.
Although a @Call@ is a glorified @Case@ statement, the only allowed
return values are @Zero@ and @One@.  Hence the @CDS CDS@ continuations
rather than the more comprehensive @(AList Return CDS)@.
We require arguments to be fully disassembled.
\item @Case@
Case selectors can only be of the following form:
\begin{itemize}
\item
   @[Par n]@  if the n'th parameter is not a function space.
\item
   @[Par n, v1 ... vn]@ if the n'th parameter is a function space of
                      arity n.  The v's may be only @Empty@, @Zero@,
                      @One@, or @Par n@.
\end{itemize}
\end{itemize}
We also have a @Magic@ CDS which is a load of mumbo-jumbo for use
in enumeration of and compilation to CDSs.  Of no significance 
whatever here.

> data CDS = Empty
>          | Par Int
>          | Zero
>          | One
>          | Case [CDS] (AList Return CDS)
>          | Call String Tag [CDS] CDS CDS
>          | Magic
>
> type AList a b = [(a, b)]
>
> type Tag = Int

> instance Eq CDS where
>    (Par n1) == (Par n2) = n1 == n2
>    Zero == Zero = True
>    One == One = True
>    (Case sels1 rets1) == (Case sels2 rets2) = sels1 == sels2 && 
>                                               rets1 == rets2
>    (Call f1 t1 sels1 a1 b1) == (Call f2 t2 sels2 a2 b2)
>       = f1 == f2 && t1 == t2 && sels1 == sels2 && a1 == a2 && b1 == b2
>    Magic == Magic = True
>    _ == _ = False


A @Return@ is a temporary thing used to decide which way to go at
a @Case@ statement.

> data Return = RZero
>             | ROne
>             | RP Int

> instance Eq Return where
>    RZero == RZero  = True
>    ROne == ROne = True
>    (RP p1) == (RP p2) = p1 == p2
>    _ == _ = False

We need a code store, which gives out a fresh instance of a CDS
as necessary.  ToDo: Need to rename call sites?  I don't think so.

> type Code = AList String CDS

%============================================================
%============================================================

\subsection{The evaluator}
Main CDS evaluator takes
\begin{itemize}
\item the code store
\item the dependancy list, a list of @Tag@s of calls which are
      currently in progress
\item the current arguments
\item the CDS fragment currently being worked on
\end{itemize}

> type Depends = [Tag]
>
> eval :: Code -> Depends -> [CDS] -> CDS -> CDS

Evaluating a constant valued CDS is trivial.  There may be arguments
present -- this is not a mistake.

> eval co de args Zero = Zero
> eval co de args One  = One

Making a call is also pretty simple, because we assume
that all non-functional arguments are presented as literals,
and all functional values have already been dismantled (unless
they are being passed unchanged in the same position in a recursive call
to the same function, something for the compiler to detect).

Two other issues are at work here.  Guided by the selectors,
we copy the args to make a set of args for the call.  However, if an
copied arg is Empty, the call cannot proceed, so we return the CDS as-is.
Note that an Empty *selector* is not allowed in a Call (although it is
in a Case).

The second issue arises if the call can go ahead.  We need to check the
tag on the call just about to be made with the tags of calls already in 
progress (in de) to see if we are looping.  If the tag has already been
encountered, the result of the call is Zero, so the Zero alternative is
immediately selected.

> eval co de args cds@(Call fname tag params alt0 alt1)
>   = let (copied_an_empty, callee_args) = copy_args args params
>         augmented_de      = tag : de
>         callee_code       = lkup co fname
>         callee_result     = eval co augmented_de callee_args callee_code
>         been_here_before  = tag `elem` de
>     in
>         if    copied_an_empty
>         then  cds
>         else
>         if    been_here_before
>         then  eval co augmented_de args alt0
>         else  case callee_result of
>                  Zero -> eval co de args alt0
>                  One  -> eval co de args alt1
>                  _    -> error "Bad callee result"

Case really means "evaluate".  

   - make sure first selector is non-Empty.  If so, return CDS as-is.

   - Copy other args.  If Empty is *copied*, return CDS as-is.
     Otherwise, call evaluator and switch on head of result.

Note about switching on the head of the result.  We expect to see
*only* the following as results:

   Zero
   One
   Case [Param m, rest]

in which case switching is performed on

   Zero
   One
   Case (Param m)

ToDo: what happens if a Call turns up ???

> eval co de args cds@(Case ((Par n):ps) alts)
>   = let (copied_an_empty, new_args) = copy_args args ps
>         functional_param = args !! (n-1)
>     in  if    functional_param == Empty || 
>               copied_an_empty
>         then  cds
>         else  eval co de args 
>                    (lkup alts (get_head 
>                                    (eval co de new_args functional_param)))

Auxiliary for evaluating Case expressions.

> get_head Zero                  = RZero
> get_head One                   = ROne
> get_head (Case ((Par n):_) _)  = RP n

Copy args based on directions in a list of selectors.
Also returns a boolean which is True if an Empty has been
*copied*.  An Empty *selector* simply produces Empty in the
corresponding output position.

> copy_args :: [CDS] -> [CDS] -> (Bool, [CDS])
>
> copy_args args params
>   = case cax False params [] of
>        (empty_copied, res) -> (empty_copied, reverse res)
>     where
>        cax empty [] res = (empty, res)
>        cax empty (Zero:ps) res = cax empty ps (Zero:res)
>        cax empty (One:ps) res = cax empty ps (One:res)
>        cax empty (Empty:ps) res = cax empty ps (Empty:res)
>        cax empty ((Par n):ps) res
>           = case args !! (n-1) of
>                Empty -> cax True ps (Empty:res)
>                other -> cax empty ps (other:res)

> lkup env k = head ( [v | (kk,v) <- env, kk == k] ++ 
>                       [error ( "Can't look up " ) ] )

%============================================================
%============================================================

%============================================================
%============================================================

Something to make running tests easier ...

> eval0 fname args = eval test [] args (lkup test fname)
>
> two = [Zero, One]

Now for some test data ...

> test
>  =
>  [
>    ("add",     add_cds),
>    ("apply",   apply_cds),
>    ("k0",      k0_cds),
>    ("id",      id_cds),
>    ("k1",      k1_cds),
>    ("kkkr",    kkkr_cds),
>    ("kkkl",    kkkl_cds),
>    ("apply2",  apply2_cds)
>  ]
>

Constant Zero function.

> k0_cds
>   = Case [Par 1]
>         [(RZero, Zero),
>          (ROne,  Zero)]
>

Identity.

> id_cds
>   = Case [Par 1]
>         [(RZero, Zero),
>          (ROne,  One)]

Constant One function.

> k1_cds
>   = Case [Par 1]
>         [(RZero, One),
>          (ROne,  One)]

Strict in both of two arguments, for example (+).

> add_cds
>  =    Case [Par 1]
>          [(RZero, Case [Par 2]
>                        [(RZero, Zero),
>                         (ROne,  Zero)
>                        ]),
>           (ROne, Case [Par 2]
>                       [(RZero, Zero),
>                        (ROne, One)
>                       ])
>          ]

The (in)famous apply function.

> apply_cds
>  = Case [Par 1, Empty]
>        [(RZero, Zero),
>         (ROne, One),
>         (RP 1, Case [Par 2]
>                    [(RZero, Case [Par 1, Zero]
>                                 [(RZero, Zero),
>                                  (ROne, One)]),
>                     (ROne,  Case [Par 1, One]
>                                 [(RZero, Zero),
>                                  (ROne, One)])
>                    ])
>        ]

The inverse K-combinator: K x y = y

> kkkr_cds
>  = Case [Par 2]
>        [(RZero, Zero),
>         (ROne, One)
>        ]

The standard K-combinator, defined thus: K x y = K-inverse y x.
Purpose of this is to test function calling.

> kkkl_cds
>  = Case [Par 1]
>        [(RZero, Case [Par 2]
>                     [(RZero, Call "kkkr" 101 [Zero, Zero] Zero One),
>                      (ROne,  Call "kkkr" 102 [One, Zero]  Zero One)
>                     ]),
>         (ROne,  Case [Par 2]
>                     [(RZero, Call "kkkr" 103 [Zero, One]  Zero One),
>                      (ROne,  Call "kkkr" 104 [One, One]   Zero One)
>                     ])
>        ]

Apply a 2-argument function (apply2 f x y = f x y).

> apply2_cds
>  = Case [Par 1, Empty, Empty]
>        [(RZero, Zero),
>         (ROne, One),
>         (RP 1, Case [Par 2]
>               [(RZero, Case [Par 1, Zero, Empty]
>                            [(RZero, Zero),
>                             (ROne, One),
>                             (RP 2, Case [Par 3]
>                                        [(RZero, Case [Par 1, Zero, Zero]
>                                                [(RZero, Zero),
>                                                 (ROne, One)]),
>                                         (ROne, Case [Par 1, Zero, One]
>                                                [(RZero, Zero),
>                                                 (ROne, One)])
>                                        ])
>                            ]),
>                (ROne,  Case [Par 1, One, Empty]
>                            [(RZero, Zero),
>                             (ROne, One),
>                             (RP 2, Case [Par 3]
>                                        [(RZero, Case [Par 1, One, Zero]
>                                                [(RZero, Zero),
>                                                 (ROne, One)]),
>                                         (ROne, Case [Par 1, One, One]
>                                                [(RZero, Zero),
>                                                 (ROne, One)])
>                                        ])
>                            ])
>               ]),
>         (RP 2, Case [Par 3]
>               [(RZero, Case [Par 1, Empty, Zero]
>                            [(RZero, Zero),
>                             (ROne, One),
>                             (RP 1, Case [Par 2]
>                                        [(RZero, Case [Par 1, Zero, Zero]
>                                                [(RZero, Zero),
>                                                 (ROne, One)]),
>                                         (ROne, Case [Par 1, One, Zero]
>                                                [(RZero, Zero),
>                                                 (ROne, One)])
>                                        ])
>                            ]),
>                (ROne,  Case [Par 1, Empty, One]
>                            [(RZero, Zero),
>                             (ROne, One),
>                             (RP 1, Case [Par 2]
>                                        [(RZero, Case [Par 1, Zero, One]
>                                                [(RZero, Zero),
>                                                 (ROne, One)]),
>                                         (ROne, Case [Par 1, One, One]
>                                                [(RZero, Zero),
>                                                 (ROne, One)])
>                                        ])
>                            ])
>               ])
>           ]

Simple, isn't it!

%============================================================
%============================================================

%============================================================
%============================================================

Enumeration of all CDSs of a given type.

Define n-ary branched trees.  These are used to hold the 
possible prefixes of function arguments, something essential
when enumerating higher-order CDSs. ToDo: translate to English

> data NTree a = NLeaf
>              | NBranch a [NTree a]

The enumeration enterprise involves some mutual recursion
when it comes to higher-order functions.  We define the
top-level enumerator function, for trivial cases, hence:

> enumerate :: Type -> [CDS]
>
> enumerate Two = [Zero, One]
> enumerate (Fn ats) = 
>    expand_templates (traverse (length ats) (gen_pfx_trees ats))

Enumerating a function space is tricky.  In summary:

   - Generate the prefix trees for each argument.  
     For non-function arguments this trivial, but for
     function-valued arguments this means a call to the
     enumerator to get all the possible values of the
     (argument) function space.

   - Traverse the prefix trees, generating a series of
     "templates" for functions.

   - Expand each template thus generated into a genuine CDS.
     Each template denotes a group of CDSs, all of
     the same "shape" and differing only in the constants
     they return.  The Magic and RMagic constructors are
     used for these purposes.

Generating prefix trees.  For a Two-argument, is easy:

> gen_pfx_trees :: [Type] -> [NTree [CDS]]
> 
> gen_pfx_trees ts = zipWith gen_pfx_tree ts [1 .. length ts]
>
> gen_pfx_tree :: Type -> Int -> NTree [CDS]
>
> gen_pfx_tree Two n = NBranch [Par n] []

Note all prefixes are missing the initial (Par n) selector ...

For a function arg

   - enumerate each of the *function's* args

   - starting with a selector [Empty, ...., Empty],
     make a tree wherein at each level, branching is 
     achieved by filling in every Empty with every value
     of that argument type.  ToDo: fix this

> gen_pfx_tree (Fn arg_types) n
>   = let number_args = length arg_types
>         enumed_args = map enumerate arg_types
>         initial_sel = take number_args (repeat Empty)
>         init_tree   = NBranch ((Par n):initial_sel) []
>     in
>         expand_pfx_tree number_args number_args n enumed_args init_tree

@expand_pfx_tree@ expands a tree until there are no Emptys
at the leaves.  Its first parameter is the number of Emptys
in the tree it has been given; when zero, expansion is complete.
The second parameter is the number of Emptys in the original
tree (equal to the arity of the function being enumerated).
Third number is the argument number in the top-level function,
needed to make the initial "Par n" selector.
Also needs to carry around the enumeration of the function's
arguments.

> expand_pfx_tree :: Int -> Int -> Int -> [[CDS]] -> NTree [CDS] -> NTree [CDS]
>
> expand_pfx_tree 0 w i enums tree = tree
>
> expand_pfx_tree n w i enums (NBranch sels [])
>   = let indices = [0 .. w - 1]
>         n_minus_1 = n - 1
>         new_sels = concat (map expand_sel indices)
>         expand_sel n
>           = case sels !! (n+1) of
>                Empty -> map (upd (n+1) sels) (enums !! n)
>                other -> []
>         mk_trivial_tree sel = NBranch sel []
>     in
>         NBranch sels (map (expand_pfx_tree n_minus_1 w i enums . mk_trivial_tree) 
>                        new_sels)

> upd :: Int -> [a] -> a -> [a]
> upd 0 (y:ys) x = x:ys
> upd n (y:ys) x = y:upd (n-1) ys x

In the second phase, the prefix trees are traversed to generate
CDS templates (full of Magic, but no Zero or One).
The first arg is the number of arguments, and the
second the prefix trees for each argument.

> traverse :: Int -> [NTree [CDS]] -> [CDS]

Each pfxtree denotes a selector, one for each argument, plus a load
of more specific selectors.  So for each argument, one manufactures
all possible sub-cds using the sub-selectors as the set Z.
You then take this arg's selector, and manufacture a load of CDSs
like this:
\begin{verbatim}
   Case this_selector
      0 -> z | z <- Z
      1 -> z | z <- Z
      Par n -> z | z <- Z for each n in [1 .. length this_selector]
                          satisfying this_selector !! n == Empty
\end{verbatim}


> traverse n pfxtrees
>   = Magic : concat (map doOne [0 .. n - 1])
>     where
>        doOne i = traverse_arg n i pfxtrees (pfxtrees !! i) 

@traverse_arg@ makes the CDSs corresponding to descending a
particular argument, the number of which is given as its second
parameter.  It also gets the complete set of pfxtrees and the one
to descend.  Note that having descended in the given argument, we
check its sub-selectors.  If none, (an empty list), this replaced
by [NLeaf] to make everything work out.  A NLeaf selector
is a dummy which generates no CDSs.

> traverse_arg n i pfxtrees NLeaf
>   = []

> traverse_arg n i pfxtrees (NBranch this_selector subsidiary_selectors_init)
>   = let subsidiary_selectors 
>            = case subsidiary_selectors_init of 
>                 [] -> [NLeaf]; (_:_) -> subsidiary_selectors_init
>         subsidiary_pfxtrees = map (upd i pfxtrees) subsidiary_selectors
>         par_requests = preq 1 [] this_selector
>         preq n acc [] = acc
>         preq n acc (Empty:rest) = preq (n+1) ((RP n):acc) rest
>         preq n acc (other:rest) = preq (n+1)         acc  rest
>         subsidiary_cdss = concat (map (traverse n) subsidiary_pfxtrees)
>         all_poss_rhss = splat (2 + length par_requests) subsidiary_cdss
>         all_poss_returns = [RZero,  ROne] ++ par_requests
>     in
>         [Case this_selector (zip all_poss_returns rhs)
>         | rhs <- all_poss_rhss]
>
> splat :: Int -> [a] -> [[a]]
> splat 0 set = [[]]
> splat n set = [x:xs | x <- set, xs <- splat (n-1) set]

The final stage in the game is to fill in all the @Magic@s
with constants.  A template with $n$ @Magic@s presently generates
@2^n@ CDSs, obtained by all possible combinations of
filling each @Magic@ in with @Zero@ or @One@.  To do this we
first need to count the @Magic@s.

> count_magic :: CDS -> Int
> 
> count_magic Magic             = 1
> count_magic (Case sels alts)  = sum (map (count_magic.snd) alts)

We don't expect to see anything else at this stage.
Now make $2^n$ lists, each of length $n$, each with a different
sequence of @Zero@s and @One@s.  Use these to label the 
@Magic@s in the template.

> label_cds :: CDS -> [CDS] -> ([CDS], CDS)
>
> label_cds Magic (l:ls) = (ls, l)
> label_cds (Case sels alts) ls
>   = case f ls alts of (l9, alts_done) -> (l9, Case sels alts_done)
>     where
>        f l0 []     = (l0, [])
>        f l0 (a:as) = let (l1, a_done)  = lalt l0 a
>                          (l2, as_done) = f l1 as
>                      in  (l2, a_done:as_done)
>        lalt l0 (ret, cds) = case label_cds cds l0 of 
>                               (l1, cds_done) -> (l1, (ret, cds_done))

Finally:

> expand_templates :: [CDS] -> [CDS]
>
> expand_templates ts
>    = concat (map f ts)
>      where
>         f tem = map (snd . label_cds tem) 
>                     (splat (count_magic tem) [Zero, One])

--> testq tt = (layn . map show' . nub) (enumerate tt)

> main = putStrLn (show (apply_cds `myElem` (enumerate (Fn [Fn [Two], Two]))))
>
> i `myElem` [] = False
> i `myElem` (x:xs) = if i == x then True else i `myElem` xs 

%============================================================
%============================================================
