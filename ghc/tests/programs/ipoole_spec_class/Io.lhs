\input{LiterateUtils}
{
\DownLevel
\filetitle{IO.lgs --- Implementation of the basic I/O monad}
\author{Andy Gill \\ University of Glasgow.\\(edited by IP)}
\maybemaketitle
%
%
% SCCS: %W% %G%
%
% Modifications
% -------------
% 15-01-94	ipoole	       IO --> Io  (to keep ghc happy)
% 04-09-93	ipoole	       #ifdef Gofer, so we can compile with hbc
%                              (and maybe ghc)
% 02-09-93	ipoole         extracted from Andy's prelude, name changes:
%                              returnIO --> unitIO, thenIO --> bindIO.

\begin{vb}

> module Io where

\end{verbatim}\end{vb}

This is the basic monad upon which the \verb@Job s1 s2 a@ monad is defined.

\begin{Dec}{Io}         
The Io monad, defined in terms of a Haskell Dialogue.
\begin{vb}

> type Io a = (a -> Dialogue) -> Dialogue

#ifdef Gofer

>       in unitIo,  bindIo,
>          ioToDialogue, processRequestIo, doneIo
>

#endif

\end{verbatim}\end{vb}\end{Dec}

\begin{Def}{unitIo}
The operation which returns a result without performing I/O.

\begin{vb}

> unitIo :: x -> Io x
> unitIo x cont = cont x

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{bindIo}
Connect an Io operation to a continuation.
\begin{vb}

> bindIo :: Io a -> (a -> Io b) -> Io b
> bindIo m k cont = m (\ a -> k a cont)

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{ioToDialogue}
Convert an Io to a runable Haskell Dialogue
\begin{vb}

> ioToDialogue :: Io a -> Dialogue
> ioToDialogue io = io (const (const []))

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{processRequestIo}
Output a Haskell Request and get back the response.
\begin{vb}

> processRequestIo   :: Request -> Io Response
> processRequestIo req cont ~(resp:resps) = req : cont resp resps

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{doneIo}
Terminate the Io.
\begin{vb}

> doneIo :: Io a
> doneIo cont = \ _ -> []

\end{verbatim}\end{vb}\end{Def}
}
\EndFile


