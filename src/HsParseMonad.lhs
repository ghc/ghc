-----------------------------------------------------------------------------
-- $Id: HsParseMonad.lhs,v 1.1 2002/04/04 16:23:43 simonmar Exp $
--
-- (c) The GHC Team 1997-2000
--
-- Monad for the Haskell parser.
--
-----------------------------------------------------------------------------

\begin{code}
module HsParseMonad where

import HsSyn
\end{code}

\begin{code}
data ParseResult a = Ok ParseState a | Failed String
	deriving Show

data LexContext = NoLayout | Layout Int
	deriving (Eq,Ord,Show)

type ParseState = [LexContext]

type P a
     =  String			-- input string
     -> SrcLoc			-- location of last token read
     -> Int			-- current line
     -> Int			-- current column
     -> ParseState		-- layout info.
     -> ParseResult a

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \i l n c s -> 
	case m i l n c s of 
	    Failed s -> Failed s
	    Ok s' a -> case k a of k' -> k' i l n c s'

m `thenP_` k = m `thenP` \_ -> k

mapP :: (a -> P b) -> [a] -> P [b]
mapP f [] = returnP []
mapP f (a:as) = 
     f a `thenP` \b ->
     mapP f as `thenP` \bs ->
     returnP (b:bs)

returnP a = \i l n c s -> Ok s a

failP :: String -> P a
failP err = \i l n c s -> Failed err

getSrcLoc :: P SrcLoc
getSrcLoc = \i l n c s -> Ok s l

getContext :: P [LexContext]
getContext = \i l n c s -> Ok s s

pushContext :: LexContext -> P ()
pushContext ctxt = 
--trace ("pushing lexical scope: " ++ show ctxt ++"\n") $
	\i l n c s -> Ok (ctxt:s) ()

popContext :: P ()
popContext = \i l n c stk ->
      case stk of
   	(_:s) -> --trace ("popping lexical scope, context now "++show s ++ "\n") $ 
            Ok s ()
        []    -> error "Internal error: empty context in popContext"
\end{code}
