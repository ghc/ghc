\input{LiterateUtils}
{
\DownLevel
\filetitle{JobApp.lgs --- Convenience functions for the I/O / state monad}
\author{Andy Gordon and Ian Poole}
\maybemaketitle

% 
%               Copyright (C) 1993  Medical Research Council
% 
% SCCS: %W% %G%
%
%  MODIFICATIONS
%  -------------
%     02-09-93  ipoole  Job now carries application state.
%                       Note addition of >>>, >>>=
%     20-08-93  ipoole  infixl 0 >> (was infixr 0)
%     16-08-93  ipoole  JobS s
%     20-06-93  ipoole  readRemainingInputJ :: Job String
%     11-03-93  derekc  strToInt, isNat, isInt -> Lib.lgs,
%                       trace <- Lib.lgs
%     02-03-93  derekc  newStyleIdentifiersUsed, put under sccs
%     23-02-93  ipoole  Moved some definition in from Lib, to make IO
%                       more self contained.
%     21-02-93  ipoole  Added clearScreen
%     21-02-93  ipoole  Mods to match those in IOimp.lgs,
%                       ie to use outputReq and inputResp.
%                       Added getEnvJ, getArgsJ, getProgNameJ,
%                       writeFileJ, appendFileJ, readFileJ.
%                       (The postfixed "j" is to avoid conflict with
%                       standard continuation versions)
%     11-11-92  ipoole  added getWord, getInt and putInt
%     08-11-92  ipoole  infixl 0 ##= (was infix 0 ##=)
%     08-11-92  ipoole  'gather' made polymorphic
%     25-10-92  ipoole  priority of >> set to 1 (was "infixr >>" ??)


%==========================================================================
\iftopdocument{\tableofcontents}

This script builds on top of the facilities provided in
\verb@IO.lgs@ and \verb@JobImp.lgs@ without requiring access to the
implementation of the \verb@IO@ or \verb@Job@  datatypes.  In particular,
it operator synonyms for the most-used combinators.  


\begin{vb}

> module JobApp where
> import Lib
> import Io
> import JobImp

> infixl 1 >>=          -- bindJobh
> infixl 1 >>>=         -- bindJob

> infixl 0 >>           
> infixl 0 >>>          

> infixr 0 ?            
> infixl 0 ##=

\end{verbatim}\end{vb}



\sectionH{Synonyms for basic types and combinators}

\begin{Dec}{Task}
Things of type \verb@Task s1 s2@ may perform I/O, but returns no result.
It may read/modify state if the state type parameters are instantiated.
\begin{vb}

> type Task s1 s2 = Job s1 s2 ()

\end{verbatim}\end{vb}\end{Dec}

\begin{Def}{arrow-arrow-equal}

Synonyms for various combinators.

\begin{vb}

> (>>=) = bindJobh
> (>>>=) = bindJob
> val = unitJob

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{arrow-arrow}
Perform the first Job, discard any result, then perform the second job.
\begin{vb}

> (>>>)     :: Job s1 s2 a -> Job s2 s3 b -> Job s1 s3 b
> a >>> b   = a >>>= (\_ -> b)

> (>>)      :: Job s1 s2 a -> Job s2 s2 b -> Job s1 s2 b
> a >> b    = a >>= (\_ -> b)

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{questionmark}

So-called "biased choice";  Perform the first Job and iff it raises
an unhandled error, perform the second job.
\begin{vb}


> (?)       :: Job s s a -> Job s s a -> Job s s a   
> a ? b     = a `handle` (\x -> b)

\end{verbatim}\end{vb}\end{Def}


\begin{Def}{zeroT} The null Task --- does nothing.
\begin{vb}

> zeroT      :: Task s s
> zeroT      = val ()

\end{verbatim}\end{vb}\end{Def}



%==========================================================================

\sectionH{Useful imperative combinators}

\begin{Def}{FoldT} 
Sequentially perform a list of tasks.
\begin{vb}

> foldT     :: [Task s s] -> Task s s
> foldT     = foldr (>>) zeroT

\end{verbatim}\end{vb}\end{Def}


\begin{Def}{while}
Iteratively perform a continuation Job, while a condition on the
result holds true.
\begin{vb}

> while :: (a -> Bool) -> (a -> Job s s a) -> (a -> Job s s a)
> while f p a | f a       = p a >>= while f p
>             | otherwise = val a

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{gather}
Gather input items into list a by repeatedly performing the given Job, as
long as the given continuation condition holds true.
\begin{vb}

> gather :: Job s s a -> (a -> Bool) -> Job s s [a]
> gather inputFun cond =
>     inputFun >>= (\c ->
>     while
>         (\(c,cs) -> cond c)
>         (\(c,cs) -> inputFun >>= (\c' -> val (c', c:cs)))
>         (c,[]) >>=
>     val . reverse . snd)

\end{verbatim}\end{vb}\end{Def}


\begin{Def}{hash--hash--equals}
Perform the first job followed by the second, combining their results
into a 2-tuple.  Note that no change of state type is permitted.
\begin{vb}


> (##=) :: Job s s a -> Job s s b -> Job s s (a,b)
> p ##= q =
>     p >>= (\a ->
>     q >>= (\b ->
>     val (a,b)))

\end{verbatim}\end{vb}\end{Def}


%==========================================================================
\sectionH{Teletype I/O}

Some higher-level IO operations on stdin and stdout.  Note that the
low-level functions \verb@getChar, getRest, ungetChar@ and \verb@ungetStr@
are defined in \verb@JobImp.lgs@.

\begin{Def}{getWord,getLine}

Read the next word from stdin.  A word is defined as
a sequence of non-space characters,  a space being any of
\verb@<space>,\t,\n,\r,\f,\v@.
\begin{vb}

> getWord :: Job s s String
> getWord = gather getChar (not . isSpace) >>=
>           (\str -> if null str  || isSpace (head str) then
>                        getWord
>                    else
>                        val str
>           )

> getLine :: Job s s String
> getLine = gather getChar (\x -> x /= '\n')

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{getInt}
Read the next word of input from stdin and interpret as
a integer.  If the word is not a valid integer, then raise a
(handleable) error.
\begin{vb}

> getInt  :: Job s s Int
> getInt = getWord >>= (\str ->
>       if isInt str then(val . strToInt) str
>       else raise "getInt: invalid string")

\end{verbatim}\end{vb}\end{Def}


\begin{Def}{putChar,putLine,etc}~~\begin{vb}

> putChar :: Char -> Task s s
> putLine :: String -> Task s s
> putStr  :: String -> Task s s
> putInt  :: Int -> Task s s
> putStrStderr :: String -> Task s s
> askFor  :: String -> Job s s String

> putChar c = putStr [c]
> putLine xs = putStr xs >> putStr "\n"
> putStr = appendChanJ stdout 
> putStrStderr = appendChanJ stderr
> putInt = putStr . show
> askFor xs = putStr xs >> getLine

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{clearScreen}
On an xterm at least, clear the screen, leaving the cursor at the top left
position.

\begin{vb}

> clearScreen :: Task s s
> clearScreen = putStr (map toEnum [27, 91, 72, 27, 91, 50, 74])


\end{verbatim}\end{vb}\end{Def}


\sectionH{Job equivalents of Dialogue IO}

Note that these functions raise exceptions upon encountering an
error condition.

\begin{Def}{appendChanJ}
Append to the given channel (stdout, or stdin).

\begin{vb}

> appendChanJ :: String -> String -> Task s s
> appendChanJ chan str
>       = processRequestJ (AppendChan chan str) >>=
>       (\resp -> case resp of
>               Success -> zeroT
>               Failure (SearchError estr) -> 
>                       raise ("Error in appendChanJ: " ++ estr))

\end{verbatim}\end{vb}\end{Def}


\begin{Def}{writeFileJ}~~\begin{vb}

> writeFileJ :: String -> String -> Task s s
> writeFileJ fname str 
>       = processRequestJ (WriteFile fname str) >>=
>         (\resp -> case resp of
>               Success -> zeroT
>               Failure err -> raise ("writeFileJ " ++ fname) )

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{appendFileJ}~~\begin{vb}

> appendFileJ :: String -> String -> Task s s
> appendFileJ fname str 
>       = processRequestJ (AppendFile fname str) >>=
>         (\resp -> case resp of
>               Success -> zeroT
>               Failure err -> 
>                       raise ("Error in appendFileJ " ++ fname) )

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{readFileJ}
\begin{vb}

> readFileJ :: String -> Job s s String
> readFileJ fname 
>       = processRequestJ (ReadFile fname) >>=
>         (\resp -> case resp of
>               Str l -> val l
>               Failure (SearchError estr) -> 
>                       raise ("Search error in readFileJ: " ++ estr) 
>               Failure err -> 
>                       raise ("Error in readFileJ: " ++ fname))

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{readChanJ}
\begin{vb}

> readChanJ :: String -> Job s s String
> readChanJ fname 
>       = processRequestJ (ReadChan fname) >>=
>         (\resp -> case resp of
>               Str l -> val l
>               Failure (SearchError estr) -> 
>                       raise ("Search error in readChanJ: " ++ estr) 
>               Failure err -> 
>                       raise ("Error in readChanJ: " ++ fname))

\end{verbatim}\end{vb}\end{Def}



\sectionHH{Reading from a Pipe (HGU extension)}

\begin{Def}{readPipeJ}
The argument is any shell command, the return is the result from
stdout after executing the command under sh.
\begin{vb}

#ifdef Gofer	-- the ReadPipe request is a HGU extension to Gofer.

> readPipeJ :: String -> Job s s String

> readPipeJ fname 
>       = processRequestJ (ReadPipe fname) >>=
>         (\resp -> case resp of
>               Str l -> val l
>               Failure (SearchError estr) -> 
>                       raise ("Search error in readPipeJ: " ++ estr) 
>               Failure err -> 
>                       raise ("Error in readPipeJ: " ++ fname))

#endif Gofer
\end{verbatim}\end{vb}\end{Def}



\sectionH{Environment I/O}

These Jobs use the the new I/O requests available in Gofer 2.28.

\begin{Def}{getEnvJ}
Return the value of the given Unix environment variable.
\begin{vb}

> getEnvJ :: String -> Job s s String
> getEnvJ var = processRequestJ (GetEnv var) >>=
>       (\resp -> case resp of
>               Str l -> val l
>               Failure (SearchError estr) -> 
>                       raise ("Error in getEnvJ: " ++ estr))

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{getArgsJ}
Return command-line arguments.  Returns [] when used from the interpreter.
\begin{vb}



> getArgsJ :: Job s s [String]
> getArgsJ = processRequestJ GetArgs >>=
>       (\resp -> case resp of
>               StrList ll -> val ll)

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{getProgNameJ}
Reurn program name.  Returns "" when used from the interpreter.
\begin{vb}

> getProgNameJ :: Job s s String
> getProgNameJ = processRequestJ GetProgName >>=
>       (\resp -> case resp of
>               Str l   -> val l
>               Failure (OtherError l) -> val "")

\end{verbatim}\end{vb}\end{Def}


\sectionH{Some higher level functions operating on state}

\sectionHH{A dynamic `state stack'}
\label{sec-StateStack}
Some functions, inspired by Simon Thompson's work, to allow stacking
of state values.  Values in the 'state stack' can be of differing types.

Note that we have:
\begin{vb}

        pushState s `bindJob` (\_ -> popState) == unitJob s

\end{verbatim}\end{vb}
The stack is in fact implemented by nested 2-tuples, so that, for example,
\newline \verb@pushState 1 >>> pushState 2.0 >>> pusState "Fred"@ has type
\newline \verb@Task s ([Char],(Float,(Int,s)))@.

\begin{Def}{pushState,etc}~~\begin{vb}

> pushState :: st -> Job s (st,s) ()
> pushState st = getState `bindJob` (\s ->
>                setState (st,s))

> popState :: Job (st,s) s st
> popState = getState `bindJob` (\(st,s) ->
>            setState s `bindJob` (\_-> unitJob st))

> getTopState :: Job (st,s) (st,s) st
> getTopState = getState `bindJob` (unitJob . fst)

> setTopState :: st2 -> Job (st1,s) (st2,s) ()
> setTopState st2 = popState `bindJob` (\_ -> pushState st2)

\end{verbatim}\end{vb}\end{Def}

The point of these functions is that they provide a form of dynamic
scoping of state, whilst being fully visible to the type
system\footnote{expect this could be said better, or even
correctly...}.  Thus, a package may create and use 
state internally whilst remaining transparent to any state used by the
calling program; the package will have type \verb@Job s s a@.

\sectionHH{SJob and STask}

[NOT YET FULLY WORKED OUT]

It seems desirable to write programs in units which conform to the
above conventions --- ie, which at worst modify the top of
the state stack, but are transparent to any other state in the stack.

The types \verb@SJob@ and \verb@STask@ enforce the above,
providing the first parameter remains un-instantiated.

\begin{vb}

> type SJob s t1 t2 a = Job (t1,s) (t2,s) a
> type STask s t1 t2 = SJob s t1 t2 ()

\end{verbatim}\end{vb}

Thus, a function of type \verb@STask s T1 T2@ expects to find state
of type \verb@T1@ on top of the state stack, and may transform it
to type \verb@T2@, but neither reads, nor modifies the remainder of the 
rest of the state stack. 

Here is a very silly example --- one would never use state for 
such programming in the small.

\begin{vb}

> greet_ :: STask s t t
> greet_ 
>    = putLine "Hello, who's there?" >>>
>      pushState "Ian" >>    
>      greetme_ >>>
>      popState >>= (\name -> putLine ("Oh, hi " ++ name))

> greetme_ :: STask s String String
> greetme_ 
>    = getTopState >>= (\name->
>      putLine ("Hi " ++ name ++ ", it's only me")) >>
>      setTopState "John" 

\end{verbatim}\end{vb}

Evaluating \verb@go greet_@ produces,

\begin{vb}

Hello, who's there?
Hi Ian, it's only me
Oh, hi John

\end{verbatim}\end{vb}
Security is gained, since type errors will be generated if any
of the push/pops are not balanced and of appropriate type.

Ideally, we would like to {\em enforce\/} this style by making
\verb@SJob@ fully abstract, and disallowing use of \verb@getState@
and \verb@setState@,  but this is still TO-DO.

\sectionHH{Mutate state by a given function}

\begin{Def}{applyToState,applyToTopState}~~\begin{vb}
        
> applyToState :: (s1->s2) -> Task s1 s2
> applyToState sf = getState `bindJob` (setState . sf)

> applyToTopState :: (t1->t2) -> STask s t1 t2
> applyToTopState sf = getTopState `bindJob` (setTopState . sf)

\end{verbatim}\end{vb}\end{Def}

}
\EndFile

