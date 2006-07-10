-----------------------------------------------------------------------------
-- $Id: HsParseMonad.lhs,v 1.2 2002/07/24 09:42:18 simonmar Exp $
--
-- (c) The GHC Team 1997-2000
--
-- Monad for the Haskell parser.
--
-----------------------------------------------------------------------------

\begin{code}
module HsParseMonad where

import HsSyn2
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
     -> FilePath		-- current original filename
     -> ParseState		-- layout info.
     -> ParseResult a

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \i l n c f s0 -> 
	case m i l n c f s0 of 
	    Failed s -> Failed s
	    Ok s' a -> case k a of k' -> k' i l n c f s'

thenP_ :: P a -> P b -> P b
m `thenP_` k = m `thenP` \_ -> k

mapP :: (a -> P b) -> [a] -> P [b]
mapP _ [] = returnP []
mapP f (a:as) = 
     f a `thenP` \b ->
     mapP f as `thenP` \bs ->
     returnP (b:bs)

returnP :: a -> P a
returnP a = \_ _ _ _ _ s -> Ok s a

failP :: String -> P a
failP err = \_ _ _ _ _ _ -> Failed err

getSrcLoc :: P SrcLoc
getSrcLoc = \_ l _ _ _ s -> Ok s l

getContext :: P [LexContext]
getContext = \_ _ _ _ _ s -> Ok s s

pushContext :: LexContext -> P ()
pushContext ctxt = 
--trace ("pushing lexical scope: " ++ show ctxt ++"\n") $
	\_ _ _ _ _ s -> Ok (ctxt:s) ()

popContext :: P ()
popContext = \_ _ _ _ _ stk ->
      case stk of
   	(_:s) -> --trace ("popping lexical scope, context now "++show s ++ "\n") $ 
            Ok s ()
        []    -> error "Internal error: empty context in popContext"
\end{code}
