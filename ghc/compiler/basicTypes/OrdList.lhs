%
% (c) The AQUA Project, Glasgow University, 1993-1994
%

%
% This is useful, general stuff for the Native Code Generator.
%

\begin{code}

module OrdList (
	OrdList, 

    	mkParList, mkSeqList, mkEmptyList, mkUnitList,
	
    	flattenOrdList
-- UNUSED:
--	concatOrdList, fnOrdList, foldOrdList,
--	mapAccumBOrdList, mapAccumLOrdList, mapAccumROrdList,
--	mapOrdList, reverseOrdList, simplOrdList
    ) where

import Util	( mapAccumB, mapAccumL, mapAccumR )

\end{code}

This section provides an ordering list that allows fine grain
parallelism to be expressed.  This is used (ultimately) for scheduling
of assembly language instructions.

\begin{code}

data OrdList a = SeqList (OrdList a) (OrdList a) 
	       | ParList (OrdList a) (OrdList a) 
	       | OrdObj a
	       | NoObj 
	       deriving ()

mkSeqList a b = SeqList a b
mkParList a b = ParList a b
mkEmptyList   = NoObj
mkUnitList    = OrdObj

\end{code}

%------------------------------------------------------------------------

This simplifies an ordering list, using correctness preserving transformations.
Notice the duality between @Seq@ and @Par@.

\begin{code}
{- UNUSED:
simplOrdList :: OrdList a -> OrdList a
simplOrdList (SeqList vs)  = 
      case (concat [ 
	      (case simplOrdList v of
		 SeqList xs	-> xs
		 OrdObj a	-> [OrdObj a]
		 NoObj		-> []
		 xs		-> [xs]) | v <- vs]) of
	[]  -> NoObj
	[x] -> x
	v   -> SeqList v
simplOrdList (ParList vs)  = 
      case (concat [ 
	      (case simplOrdList v of
		 ParList xs	-> xs
		 OrdObj a	-> [OrdObj a]
		 NoObj		-> []
		 xs		-> [xs]) | v <- vs]) of
	[]  -> NoObj
	[x] -> x
	v   -> ParList v
simplOrdList v = v
-}
\end{code}

%------------------------------------------------------------------------

First the foldr !

\begin{code}
{- UNUSED:

foldOrdList 
      :: ([b] -> b) 
      -> ([b] -> b)
      -> (a -> b)
      -> b 
      -> (b -> b -> b)
      -> OrdList a
      -> b
foldOrdList s p o n c (SeqList vs)   = s (map (foldOrdList s p o n c) vs)
foldOrdList s p o n c (ParList vs)   = p (map (foldOrdList s p o n c) vs)
foldOrdList s p o n c (OrdObj a)     = o a
foldOrdList s p o n c  NoObj	     = n

fnOrdList :: (a -> OrdList b) -> OrdList a -> OrdList b
fnOrdList f (SeqList vs)   = SeqList (map (fnOrdList f) vs)
fnOrdList f (ParList vs)   = ParList (map (fnOrdList f) vs)
fnOrdList f (OrdObj a)	   = f a
fnOrdList f  NoObj	   = NoObj
-}
\end{code}

This does a concat on an ordering list of ordering lists.

\begin{code}
{- UNUSED:
concatOrdList :: OrdList (OrdList a) -> OrdList a
concatOrdList = fnOrdList id
-}
\end{code}

This performs a map over an ordering list.

\begin{code}
{- UNUSED:
mapOrdList :: (a -> b) -> OrdList a -> OrdList b
mapOrdList f = fnOrdList (OrdObj.f)
-}
\end{code}

Here is the reverse over the OrdList.

\begin{code}
{- UNUSED:
reverseOrdList :: OrdList a -> OrdList a
reverseOrdList NoObj	    = NoObj
reverseOrdList (OrdObj a)   = OrdObj a
reverseOrdList (ParList vs) = ParList (reverse (map reverseOrdList vs))
reverseOrdList (SeqList vs) = SeqList (reverse (map reverseOrdList vs))
-}
\end{code}

Notice this this throws away all potential expression of parrallism.

\begin{code}
flattenOrdList :: OrdList a -> [a]

flattenOrdList ol
  = -- trace (shows ol "\n") (
    flat ol []
    -- )
  where
    flat :: OrdList a -> [a] -> [a]
    flat NoObj         rest = rest
    flat (OrdObj x)    rest = x:rest
    flat (ParList a b) rest = flat a (flat b rest)
    flat (SeqList a b) rest = flat a (flat b rest)

{- DEBUGGING ONLY:
instance Text (OrdList a) where
    showsPrec _ NoObj	= showString "_N_"
    showsPrec _ (OrdObj _) = showString "_O_"
    showsPrec _ (ParList a b) = showString "(PAR " . shows a . showChar ')'
    showsPrec _ (SeqList a b) = showString "(SEQ " . shows a . showChar ')'
-}
\end{code}

This is like mapAccumR, but over OrdList's.

\begin{code}
{- UNUSED:
mapAccumROrdList :: (s -> a -> (s,b)) -> s -> OrdList a -> (s,OrdList b)
mapAccumROrdList f s NoObj	  = (s,NoObj)
mapAccumROrdList f s (OrdObj a)	  = 
   case f s a of
      (s',b) -> (s',OrdObj b)
mapAccumROrdList f s (SeqList vs) =
   case mapAccumR (mapAccumROrdList f) s vs of
      (s',b) -> (s',SeqList b)
mapAccumROrdList f s (ParList vs) =
   case mapAccumR (mapAccumROrdList f) s vs of
      (s',b) -> (s',ParList b)

mapAccumLOrdList :: (s -> a -> (s,b)) -> s -> OrdList a -> (s,OrdList b)
mapAccumLOrdList f s NoObj	  = (s,NoObj)
mapAccumLOrdList f s (OrdObj a)	  = 
   case f s a of
      (s',b) -> (s',OrdObj b)
mapAccumLOrdList f s (SeqList vs) =
   case mapAccumL (mapAccumLOrdList f) s vs of
      (s',b) -> (s',SeqList b)
mapAccumLOrdList f s (ParList vs) =
   case mapAccumL (mapAccumLOrdList f) s vs of
      (s',b) -> (s',ParList b)

mapAccumBOrdList :: (accl -> accr -> x -> (accl, accr, y))
	  -> accl -> accr -> OrdList x -> (accl, accr, OrdList y)

mapAccumBOrdList f a b NoObj  = (a,b,NoObj)
mapAccumBOrdList f a b (OrdObj x) = 
   case f a b x of
      (a',b',y) -> (a',b',OrdObj y)
mapAccumBOrdList f a b (SeqList xs) = 
   case mapAccumB (mapAccumBOrdList f) a b xs of
      (a',b',ys) -> (a',b',SeqList ys)
mapAccumBOrdList f a b (ParList xs) = 
   case mapAccumB (mapAccumBOrdList f) a b xs of
      (a',b',ys) -> (a',b',ParList ys)
-}
\end{code}

%------------------------------------------------------------------------

In our printing schema, we use @||@ for parallel operations,
and @;@ for sequential ones.

\begin{code}

#ifdef _GOFER_

instance (Text a) => Text (OrdList a) where
      showsPrec _ (ParList [a]) = shows a
      showsPrec _ (ParList as ) = showString "( " .
				      showOurList as " || " . 
				  showString " )"
      showsPrec _ (SeqList [a]) = shows a
      showsPrec _ (SeqList as ) = showString "( " .
				      showOurList as " ; " . 
				  showString " )"
      showsPrec _ (OrdObj a)	= shows a
      showsPrec _ (NoObj)	= showString "$"

showOurList :: (Text a) => [a] -> String -> ShowS
showOurList []	   s = showString ""
showOurList [a]	   s = shows a
showOurList (a:as) s = shows a .
		       showString s .
		       showOurList as s

#endif

\end{code}

