%
% (c) The AQUA Project, Glasgow University, 1993-1996
%

This is useful, general stuff for the Native Code Generator.

\begin{code}
module OrdList (
	OrdList,

    	mkParList, mkSeqList, mkEmptyList, mkUnitList,

    	flattenOrdList
    ) where
\end{code}

This section provides an ordering list that allows fine grain
parallelism to be expressed.  This is used (ultimately) for scheduling
of assembly language instructions.

\begin{code}
data OrdList a
  = SeqList (OrdList a) (OrdList a)
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

Notice this this throws away all potential expression of parallelism.

\begin{code}
flattenOrdList :: OrdList a -> [a]

flattenOrdList ol
  = flat ol []
  where
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
