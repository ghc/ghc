\input{LiterateUtils}
{
\DownLevel

\filetitle{JobImp.lgs --- I/O monad with state}

% SCCS: %W% %G%
% MODIFICATIONS
% =============
% 15-01-94	ipoole	       IO --> Io  (to keep ghc happy)
% 06-01-94	ipoole	added performJob (type sig. only)

\author{Ian Poole (editor), Andy Gordon and Andy Gill}
\maybemaketitle
\iftopdocument{\tableofcontents}
\begin{vb}

> module JobImp where
> import Io
> infixl 1 `bindJob` 
> infixl 1 `bindJobh`

\end{verbatim}\end{vb}
\sectionH{Introduction}

Here we implement a monad which can carry application defined state.
Facilities for character-by-character reading of stdin are also 
implemented here.

A value of type Job s1 s2 a is understood to potentially perform I/O
returning a result of type a and transforming its internal state to
type s2.  Both the I/O, the result and the final state may depend
on the input state.

Note that the typing of a particular Job tells us quite alot.
In the following table, ``Type'' is a values most general type.
\verb@S@ is some concrete application-specific type.

\begin{tabular}{ll}
        Type    &       Properties \\~~\\

        Job s s a       & can neither modify nor read the application state \\
        Job s S a       & the state is overwritten but never read\\
        Job S S a       & state is read and modified\\
\end{tabular}
\footnote{Unfortunately, a Job which only reads the state 
has type \verb@Job S S a@,
not \verb@Job S s a@ as one might at first think}.
Note that a Job which is defined in terms of other Jobs, inherits
the most general type which which is able to include all the nested Jobs.

The implementation is presented here, for brevity, as a single layer,
on top of the basic \verb@IO@ monad.  In fact several layers could be
made explicit as follows:

\begin{vb}

    type IOS s1 s2 a = s1 -> IO (a,s2)	   -- IO monad with state
    type IOSE s1 s2 a = IOS s1 s2 (E a)    -- ... with exceptions
    type Job s1 s2 a = IOSE (s1,[Char]) (s2,[Char]) a  -- with stdin stream

\end{verbatim}\end{vb}

\sectionH{The \protect\verb@Job@ monad}

\begin{Dec}{Job} The state monad, parameterised by initial state type 
(\verb@s1@), final state type (\verb@s2@), and result type, (\verb@a@).
\begin{vb}

> type Job s1 s2 a = (s1, MS) -> Io (E a, (s2, MS)) 

#ifdef Gofer	-- (Gofer doesn't run cpp, so will ignore this!)

>              in
>              unitJob, bindJob, bindJobh,
>              handle,
>              setState, getState,
>              getChar, getRest,
>              ungetChar, ungetStr,
>              iOtoJob, jobtoIo,
>              raise, handle

#endif

> data E a = Ret a | Fail Exn
> type Exn = String

\end{verbatim}\end{vb}\end{Dec}

\begin{Dec}{MS}

Type MS holds any fixed-type state we want always (and invisibly)
to carry in the Job monad.   
At present, we carry only the input stream, but we'll make
it abstract so that other things can easily be added if desired.
\begin{vb}

> type MS = String 

#ifdef Gofer

>           in getinputMS, setinputMS, initMS

#endif

> initMS :: MS
> initMS = []
> getinputMS :: MS -> String
> getinputMS ms = ms
> setinputMS :: MS -> String -> MS
> setinputMS ms inp = inp

\end{verbatim}\end{vb}\end{Dec}


\sectionH{Basic combinators}

\begin{Def}{unitJob}

Return a value without I/O.

\begin{vb}

> unitJob :: a -> Job s s a    
> unitJob a = \ss -> unitIo (Ret a ,ss)

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{bindJob}

Connect a Job to a continuation.  Notice the chaining of the state types.
Note that if the first Job generates a failure, then the application
state becomes undefined, since there is no way we can then obtain
state of type s3.

\begin{vb}


> bindJob :: Job s1 s2 a -> (a -> Job s2 s3 b) -> Job s1 s3 b
> bindJob m k (s1, ms) =
>       m (s1, ms)             `bindIo` \ (a,(s3, ms3)) ->
>       case a of
>       Ret v -> k v (s3, ms3)
>       Fail str -> unitIo (Fail str, (nostate, ms3))
>           where
>               nostate = error "State not defined due to failure"

\end{verbatim}\end{vb}\end{Def}


Because of the above noted problem with raising exceptions,  we provide
a variant of \verb@bindJob@ which does not allow its right-hand argument to 
modify the state type. Thus we are able to perform the error
continuation with the state as it was before the failure was raised.  
Programs should use this function whenever possible, ie, 
when binding to a Job which does
not change the state type.

\begin{Def}{bindJobh} ~~ \begin{vb}


> bindJobh :: Job s1 s2 a -> (a -> Job s2 s2 b) -> Job s1 s2 b
> bindJobh m k (s1, ms) =
>       m (s1, ms)             `bindIo` \ (a,(s2, ms2)) ->
>       case a of
>       Ret v -> k v (s2, ms2)
>       Fail str -> unitIo (Fail str, (s2, ms2))

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{doneJ}

Terminate the program.

\begin{vb}

> doneJ :: Job s s a
> doneJ = iOtoJob (doneIo)

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{raise}

Raise an exception which can later be trapped by \verb@handle@.
\begin{vb}

> raise :: Exn -> Job s s a
> raise exn ss = unitIo (Fail exn, ss)

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{handle}~~\begin{vb}

> handle :: Job s s a -> (Exn -> Job s s a) -> Job s s a
> handle p h ss
>       = p ss `bindIo` (\(a, ss2) ->
>         case a of
>         Ret v -> unitIo (a, ss2)
>         Fail str -> h str ss2)

\end{verbatim}\end{vb}\end{Def}

\sectionH{Access to application specific state}

\begin{Def}{setState}
Overwrite state, potentially changing its type.

\begin{vb}

> setState :: s2 -> Job s1 s2 ()      
> setState s2 = \(_,ms) -> unitIo (Ret (),(s2,ms))

\end{verbatim}\end{vb}\end{Def}


\begin{Def}{getState}

Return state as result.

\begin{vb}

> getState :: Job s s s 
> getState = \(s,ms) -> unitIo (Ret s,(s,ms))

\end{verbatim}\end{vb}\end{Def}


\sectionH{Reading from stdin}

\begin{Def}{getChar}.
Read a single character from stdin.  This is done by reading from the string 
that we carry around in the \verb@MS@ data.  If there is no more data
to be read, an exception is raised.
\begin{vb}

> getChar :: Job s s Char
> getChar = \(s,ms) -> 
>               case (getinputMS ms) of
>               [] -> unitIo (Fail "Attempt to read past end of stdin", (s,ms))
>               (x:xs) ->  unitIo (Ret x, (s, setinputMS ms xs))

\end{verbatim}\end{vb}\end{Def}


\begin{Def}{getRest}
Read (lazily) all remaining characters from stdin.
\begin{vb}

> getRest :: Job s s String
> getRest = \(s,ms) -> unitIo (Ret (getinputMS ms), (s, setinputMS ms []))

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{ungetChar}
Place a character onto the input stream, so that it will be read next.
\begin{vb}

> ungetChar :: Char -> Job s s ()
> ungetChar c = \(s,ms) -> 
>           unitIo (Ret (), (s, setinputMS ms (c:getinputMS ms)))

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{ungetStr}
Place a string onto the input stream, so that it will be read next.
\begin{vb}

> ungetStr :: String -> Job s s ()
> ungetStr str = \(s,ms) -> 
>           unitIo (Ret (), (s, setinputMS ms (str++(getinputMS ms))))

\end{verbatim}\end{vb}\end{Def}



\sectionH{Conversions}

\begin{Def}{iOtoJob}
Conversion to allow access to facilities of the basic Io monad.

\begin{vb}

> iOtoJob :: Io a -> Job s s a         
> iOtoJob m = \ss ->                   
>       m               `bindIo` \v ->
>       unitIo (Ret v,ss)

\end{verbatim}\end{vb}\end{Def}
\begin{Def}{jobtoIo}

Conversion to allow a Job to be run as an Io.

\begin{vb}

> jobtoIo :: s1 -> MS -> Job s1 s2 a -> Io a  -- strip out state so we can run
> jobtoIo s1 ms m =
>       m (s1, ms)           `bindIo` \ (a,_) ->
>       case a of
>       Ret v -> unitIo v
>       Fail str -> error ("iOtoJob:  Failed with: " ++ str)

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{processRequestJ}

Perform a Haskell \verb@Request->Response@ interaction.
\begin{vb}

> processRequestJ :: Request -> Job s s Response
> processRequestJ req = iOtoJob (processRequestIo req)

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{go}

Execute a Job as a Haskell Dialogue.  Note that we must first
(lazily) read input from stdin so that we can then allow 
character-by-character reading.

\begin{vb}

> go :: Job s1 s2 a -> Dialogue
> go t = ioToDialogue (processRequestIo (ReadChan stdin) `bindIo` (\resp ->
>         case resp of
>               Str inp-> let nostate = error "Application state not set"
>                         in jobtoIo nostate (setinputMS initMS inp) t
>               Failure (SearchError estr) -> 
>                       error ("Search error by ReadChan in goS: " ++ estr) 
>               Failure err -> 
>                       error ("Error by readChan in goS")))

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{performJob}
This is an UNSAFE function to perform a job and return (as a simple value)
the result.   If the job has any observable side-effects then the behvior
will be unpredictable.  

Having said all that, I don't think it can be implemented under our
definition of the I/O monad!   It is provided here simply to make it possible
for jobs to be defined implicitly, in terms of an abstraction which
is assumed RT.

> performJob :: Job s s a -> a
> performJob = error "performJob: not implemented!"

\end{verbatim}\end{vb}\end{Def}
}
\EndFile
