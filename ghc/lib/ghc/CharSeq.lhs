%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[CharSeq]{Characters sequences: the @CSeq@ type}

\begin{code}
#if defined(COMPILING_GHC)
# include "HsVersions.h"
#else
# define FAST_STRING String
# define FAST_INT    Int
# define ILIT(x)     (x)
# define IBOX(x)     (x)
# define _GE_	     >=
# define _ADD_	     +
# define _SUB_	     -
# define FAST_BOOL   Bool
# define _TRUE_	     True
# define _FALSE_     False
#endif

module CharSeq (
	CSeq,
	cNil, cAppend, cIndent, cNL, cStr, cPStr, cCh, cInt,
#if ! defined(COMPILING_GHC)
	cLength,
	cShows,
#endif
	cShow

#if defined(COMPILING_GHC) && __GLASGOW_HASKELL__ >= 22
	, cAppendFile
   ) where

#if __GLASGOW_HASKELL__ < 26
import PreludePrimIO
#endif
import PreludeGlaST

#else
   ) where
#endif
\end{code}

%************************************************
%*						*
	\subsection{The interface}
%*						*
%************************************************

\begin{code}
cShow	:: CSeq -> [Char]

#if ! defined(COMPILING_GHC)
-- not used in GHC
cShows	:: CSeq -> ShowS
cLength	:: CSeq -> Int
#endif

cNil    :: CSeq
cAppend :: CSeq -> CSeq -> CSeq
cIndent :: Int -> CSeq -> CSeq
cNL 	:: CSeq
cStr 	:: [Char] -> CSeq
cPStr	:: FAST_STRING -> CSeq
cCh 	:: Char -> CSeq
cInt	:: Int -> CSeq

#if defined(COMPILING_GHC) && __GLASGOW_HASKELL__ >= 22

# if __GLASGOW_HASKELL__ < 23
#  define _FILE _Addr
# endif

cAppendFile :: _FILE -> CSeq -> PrimIO ()
#endif
\end{code}

%************************************************
%*						*
	\subsection{The representation}
%*						*
%************************************************

\begin{code}
data CSeq
  = CNil
  | CAppend	CSeq CSeq
  | CIndent	Int  CSeq
  | CNewline			-- Move to start of next line, unless we're
				-- already at the start of a line.
  | CStr	[Char]
  | CCh		Char
  | CInt	Int	-- equiv to "CStr (show the_int)"
#if defined(COMPILING_GHC) && __GLASGOW_HASKELL__ >= 23
  | CPStr	_PackedString
#endif
\end{code}

The construction functions do pattern matching, to ensure that
redundant CNils are eliminated.  This is bound to have some effect on
evaluation order, but quite what I don't know.

\begin{code}
cNil = CNil
\end{code}

The following special cases were eating our lunch! They make the whole
thing too strict.  A classic strictness bug!
\begin{code}
-- cAppend CNil cs2  = cs2
-- cAppend cs1  CNil = cs1

cAppend cs1 cs2 = CAppend cs1 cs2

cIndent n cs = CIndent n cs

cNL	= CNewline
cStr	= CStr
cCh	= CCh
cInt	= CInt

#if defined(COMPILING_GHC) && __GLASGOW_HASKELL__ >= 23
cPStr	= CPStr
#else
cPStr	= CStr
#endif

cShow  seq	= flatten ILIT(0) _TRUE_ seq []

#if ! defined(COMPILING_GHC)
cShows seq rest = cShow seq ++ rest
cLength seq = length (cShow seq) -- *not* the best way to do this!
#endif

#if defined(COMPILING_GHC) && __GLASGOW_HASKELL__ >= 22
cAppendFile file_star seq
  = flattenIO file_star seq
#endif
\end{code}

This code is {\em hammered}.  We are not above doing sleazy
non-standard things.  (WDP 94/10)

\begin{code}
data WorkItem = WI FAST_INT CSeq -- indentation, and sequence

flatten :: FAST_INT	-- Indentation
	-> FAST_BOOL	-- True => just had a newline
	-> CSeq		-- Current seq to flatten
	-> [WorkItem]	-- Work list with indentation
	-> String

flatten n nlp CNil seqs = flattenS nlp seqs

flatten n nlp (CAppend seq1 seq2) seqs = flatten n nlp seq1 ((WI n seq2) : seqs)
flatten n nlp (CIndent IBOX(n2) seq) seqs = flatten (n2 _ADD_ n) nlp seq seqs

flatten n _FALSE_ CNewline seqs = '\n' : flattenS _TRUE_ seqs
flatten n _TRUE_  CNewline seqs = flattenS _TRUE_ seqs	-- Already at start of line

flatten n _FALSE_ (CStr s) seqs = s ++ flattenS _FALSE_ seqs
flatten n _FALSE_ (CCh  c) seqs = c :  flattenS _FALSE_ seqs
flatten n _FALSE_ (CInt i) seqs = show i ++ flattenS _FALSE_ seqs
#if defined(COMPILING_GHC) && __GLASGOW_HASKELL__ >= 23
flatten n _FALSE_ (CPStr s) seqs = _unpackPS s ++ flattenS _FALSE_ seqs
#endif

flatten n _TRUE_  (CStr s) seqs = mkIndent n (s ++ flattenS _FALSE_ seqs)
flatten n _TRUE_  (CCh  c) seqs = mkIndent n (c :  flattenS _FALSE_ seqs)
flatten n _TRUE_  (CInt i) seqs = mkIndent n (show i ++ flattenS _FALSE_ seqs)
#if defined(COMPILING_GHC) && __GLASGOW_HASKELL__ >= 23
flatten n _TRUE_ (CPStr s) seqs = mkIndent n (_unpackPS s ++ flattenS _FALSE_ seqs)
#endif
\end{code}

\begin{code}
flattenS :: FAST_BOOL -> [WorkItem] -> String
flattenS nlp [] = ""
flattenS nlp ((WI col seq):seqs) = flatten col nlp seq seqs
\end{code}

\begin{code}
mkIndent :: FAST_INT -> String -> String
mkIndent ILIT(0) s = s
mkIndent n       s
  = if (n _GE_ ILIT(8))
    then '\t' : mkIndent (n _SUB_ ILIT(8)) s
    else ' '  : mkIndent (n _SUB_ ILIT(1)) s
    -- Hmm.. a little Unix-y.
\end{code}

Now the I/O version.
This code is massively {\em hammered}.
It {\em ignores} indentation.

\begin{code}
#if defined(COMPILING_GHC) && __GLASGOW_HASKELL__ >= 22

flattenIO :: _FILE	-- file we are writing to
	  -> CSeq	-- Seq to print
	  -> PrimIO ()

flattenIO file sq
# if __GLASGOW_HASKELL__ >= 23
  | file == ``NULL'' = error "panic:flattenIO" -- really just to force eval :-)
  | otherwise
# endif
  = flat sq
  where
    flat CNil = BSCC("flatCNil") returnPrimIO () ESCC

    flat (CIndent n2 seq) = BSCC("flatCIndent") flat seq ESCC

    flat (CAppend seq1 seq2)
      = BSCC("flatCAppend")
	flat seq1 `seqPrimIO` flat seq2
	ESCC

    flat CNewline = BSCC("flatCNL") _ccall_ stg_putc '\n' file ESCC

    flat (CCh c) = BSCC("flatCCh") _ccall_ stg_putc c file ESCC

    flat (CInt i) = BSCC("flatCInt") _ccall_ fprintf file percent_d i ESCC

    flat (CStr s) = BSCC("flatCStr") put_str s ESCC

# if defined(COMPILING_GHC) && __GLASGOW_HASKELL__ >= 23
    flat (CPStr s) = BSCC("flatCPStr") put_pstr s ESCC
# endif

    -----
    put_str, put_str2 :: String -> PrimIO ()

    put_str str
      = --put_str2 ``stderr'' (str ++ "\n") `seqPrimIO`
	put_str2		str

    put_str2 [] = BSCC("putNil") returnPrimIO () ESCC

    put_str2 (c1@(C# _) : c2@(C# _) : c3@(C# _) : c4@(C# _) : cs)
      = BSCC("put4")
	_ccall_ stg_putc  c1 file	`seqPrimIO`
	_ccall_ stg_putc  c2 file	`seqPrimIO`
	_ccall_ stg_putc  c3 file	`seqPrimIO`
	_ccall_ stg_putc  c4 file	`seqPrimIO`
	put_str2 cs	-- efficiency hack?  who knows... (WDP 94/10)
	ESCC

    put_str2 (c1@(C# _) : c2@(C# _) : c3@(C# _) : cs)
      = BSCC("put3")
	_ccall_ stg_putc  c1 file	`seqPrimIO`
	_ccall_ stg_putc  c2 file	`seqPrimIO`
	_ccall_ stg_putc  c3 file	`seqPrimIO`
	put_str2 cs	-- efficiency hack?  who knows... (WDP 94/10)
	ESCC

    put_str2 (c1@(C# _) : c2@(C# _) : cs)
      = BSCC("put2")
	_ccall_ stg_putc  c1 file	`seqPrimIO`
	_ccall_ stg_putc  c2 file	`seqPrimIO`
	put_str2 cs	-- efficiency hack?  who knows... (WDP 94/10)
	ESCC

    put_str2 (c1@(C# _) : cs)
      = BSCC("put1")
	_ccall_ stg_putc  c1 file	`seqPrimIO`
	put_str2 cs	-- efficiency hack?  who knows... (WDP 94/10)
	ESCC

# if __GLASGOW_HASKELL__ >= 23
    put_pstr ps = _putPS file ps
# endif

# if __GLASGOW_HASKELL__ >= 23
percent_d = _psToByteArray SLIT("%d")
# else
percent_d = "%d"
# endif

#endif {- __GLASGOW_HASKELL__ >= 22 -}
\end{code}
