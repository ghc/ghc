A Simple Country Boy's Guide to Monadic-Style Programming
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Forget the category theory, forget all the fancy talk, forget "monadic
I/O", forget Phil Wadler's papers!  Let's just do a little *plumbing*
in the monadic style, in plain-vanilla Haskell.

You can compile this guide as a Haskell module; I haven't put in
enough code to make it run or do anytning interesting.  Excuse me for
a moment, while I get some preliminaries out of the way...
\begin{code}
module Foo where

infixr 9 `thenFoo`, `thenFoo_` -- ignore me

data Foo = NullFoo | ConsFoo Int Foo -- assorted types, of little interest
type SwitchChecker = String -> Bool
type EnvA = [(String, Float)]
type NameSupply = Int
\end{code}

*** MOTIVATION *********

If you find that your Haskell functions are starting to carry around a
lot of baggage ..., e.g.,
\begin{code}
f :: EnvA -> SwitchChecker -> NameSupply -> Foo -> (Int, NameSupply)

f env sw_chkr names NullFoo = (0, names)

f env sw_chkr names (ConsFoo x xs)
  = let
	(x', names')  = f env sw_chkr names  xs
    in
    (x + x', names')
{-
    `env' is some kind of environment;
	what most people call "lookup tables".
    `sw_chkr' is a function which, when presented with a
  	String, will tell you if that string was present
  	on the command line.
    `names' is some kind of "name supply"; `f'
    `f' returns a depleted name supply (2nd component of result).
-}
\end{code}

...then it may be time to use monadic code to hide some of the mess!!

GRATUITOUS PLUMBING OF STATE MUST DIE.


*** SETTING UP THE MONAD MACHINERY *******

First, divide the things to be plumbed into:

    * things that are only passed "downwards" through the function;
      in the example above, the `env' and `sw_chkr' are such things;

    * things that are "threaded" through the function; you want the
      changed whatsit back from "down below"; `names' is such a thing.

Now, implement your monad; let's call it `FooM'; think of a `FooM
Wibble' as an *action* that, when performed, produces a `Wibble'.

\begin{code}
type FooM a =  EnvA		-- type of lookup-tbl being plumbed
	    -> SwitchChecker	-- type of switch looker-upper...
	    -> NameSupply	-- NameSupply going down...
	    -> (a,		-- result of the `FooM a' action
		NameSupply)	-- NameSupply that comes back...
\end{code}

(Note: in reality, it would be good practice to hide all this stuff
behind a clean interface, in another module.)

Let's write the basic operations on these `FooM a' guys:

\begin{code}
returnFoo :: a -> FooM a
    -- make a `FooM thing' action from a `thing' value
    -- [Phil W would call this `unitFoo']

thenFoo :: FooM a -> (a -> FooM b) -> FooM b
    -- sequence two actions; the second uses the
    -- result of the first
    -- [Phil W would call this `bindFoo', or semi-colon :-]

thenFoo_ :: FooM a -> FooM b -> FooM b
    -- sequence two actions; don't care about the
    -- result of the first
    -- [the name is a mnemonic for "... thenFoo \ _ -> ...]
\end{code}

They're implemented in the obvious way:
\begin{code}
returnFoo thing env sw_chkr ns = (thing, ns)

thenFoo action1 action2 env sw_chkr ns
  = case (action1 env sw_chkr ns) of
      (result1, ns1) -> action2 result1 env sw_chkr ns1

thenFoo_ action1 action2 env sw_chkr ns
  = case (action1 env sw_chkr ns) of
      (_{-boring result-}, ns1) -> action2 env sw_chkr ns1
\end{code}

All those are "pure plumbing".  We need a few "monadic functions" that
do something useful.

For example, you need to be able to "do a `FooM' action" and get the
answer back (along with the depleted NameSupply); for that, use...
\begin{code}
initFoo :: FooM a -> SwitchChecker -> NameSupply -> (NameSupply, a)

initFoo action sw_chkr ns
  = case (action [] sw_chkr ns) of
      (result, new_ns) -> (new_ns, result)
	-- gratuitous order-swapping
\end{code}

You would then have a this-monad-specific set of functions to ``reach
down'' in the plumbing and use the env, switch-checker, etc., that are
being carried around.  Some examples might be:
\begin{code}
getNewName :: FooM Int

getNewName env sw_chkr ns = (ns, ns+1)

------------

ifSwitchSet :: String -> FooM a -> FooM a -> FooM a

ifSwitchSet sw_to_chk then_ else_ env sw_chkr ns
  = (if (sw_chkr sw_to_chk) then then_ else else_) env sw_chkr ns

------------

lookupInEnv :: String -> FooM Float

lookupInEnv key env sw_chkr ns
  = case [ v | (k, v) <- env, k == key ] of
      []      -> error "lookupInEnv: no match"
      (val:_) -> (val, ns)
\end{code}

*** USING THE MONAD MACHINERY *******

We now have everything needed to write beautiful (!) monadic code.  To
remind you of the basic "monadic" functions at our disposal:

\begin{verbatim}
returnFoo :: a -> FooM a
thenFoo :: FooM a -> (a -> FooM b) -> FooM b
thenFoo_ :: FooM a -> FooM b -> FooM b
initFoo :: FooM a -> SwitchChecker -> NameSupply -> (NameSupply, a)

getNewName :: FooM Int
ifSwitchSet :: String -> FooM a -> FooM a -> FooM a
lookupInEnv :: String -> FooM Float
\end{verbatim}

Before going on: there are a few plumbing things that aren't
essential, but tend to be useful.  They needn't be written at the
"bare-bones" level; they show the use of `returnFoo' and `thenFoo'.
\begin{code}
mapFoo :: (a -> FooM b) -> [a] -> FooM [b]

mapFoo f []     = returnFoo []
mapFoo f (x:xs)
  = f x         `thenFoo` \ r  ->
    mapFoo f xs `thenFoo` \ rs ->
    returnFoo (r:rs)

mapAndUnzipFoo  :: (a -> FooM (b,c))   -> [a] -> FooM ([b],[c])

mapAndUnzipFoo f [] = returnFoo ([],[])
mapAndUnzipFoo f (x:xs)
  = f x		    	`thenFoo` \ (r1,  r2)  ->
    mapAndUnzipFoo f xs	`thenFoo` \ (rs1, rs2) ->
    returnFoo (r1:rs1, r2:rs2)
\end{code}

You should read

    f x `thenFoo` \ r -> ...

as

    "do `f' with argument `x', giving result `r'".

If you wanted, you could do really horrible things with the C
pre-processor (GHC and HBC let you do this...):
\begin{verbatim}
#define RETN returnFoo
#define BIND {--}
#define _TO_ `thenFoo` \ {--}

mapFoo f [] = RETN []
mapFoo f (x:xs)
  = BIND (f x)         _TO_ r  ->
    BIND (mapFoo f xs) _TO_ rs ->
    RETN (r:rs)
\end{verbatim}

*** USING THE MONAD MACHINERY, FOR REAL *******

We can finally re-write our `f' function in a "monadic style" (except
I'll call it `g'), using the functions above.
\begin{code}
g :: Foo -> FooM Int
    -- `g' has the same arguments as `f' (really), but in a different
    -- order: just unravel the type synonyms

g NullFoo = returnFoo 0

g (ConsFoo x xs)
  = g xs    `thenFoo` \ x' ->
    returnFoo (x + x')
\end{code}

LOOK, MOM, NO GRATUITOUS PLUMBING OF STATE!

OK, `g' shows how much the monadic style tidies up the plumbing, but
it is really boring---it doesn't use any of the functions we defined
earlier.  Here's a function that does:
\begin{code}
h :: Int -> FooM Integer

h x
  = getNewName	`thenFoo_` -- ignore that one...
    getNewName	`thenFoo`  \ int_name ->

    mapAndUnzipFoo zwonk [int_name ..]
		`thenFoo` \ (some_nums, more_nums) ->

    ifSwitchSet "-beat-hbc" (
	returnFoo (toInteger (some_nums !! 6) + 42)

    ) {-else-} (
	lookupInEnv "-ghc-is-cool"  `thenFoo` \ ghc_float ->
	returnFoo (toInteger (truncate ghc_float))
    )
  where
    zwonk :: Int -> FooM (Int, Int)
    zwonk i = returnFoo (i, x*i)
\end{code}

*** CONCLUSION *******

Ordinary Haskell programming, but in a "monadic style", is a good way
to control the plumbing of state through your code.

I have left out lots and lots of Neat Things you can do with monads --
see the papers for more interesting stuff.  But 99% of the monadic
code you're likely to write or see will look like the stuff in here.

Comments, suggestions, etc., to me, please.

Will Partain
partain@dcs.glasgow.ac.uk

% compile with:
%   ghc -cpp <other-flags> Foo.lhs
%   hbc -M <other-flags> Foo.lhs
