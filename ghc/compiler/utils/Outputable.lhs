%
% (c) The GRASP Project, Glasgow University, 1992-1996
%
\section[Outputable]{Classes for pretty-printing}

Defines classes for pretty-printing and forcing, both forms of
``output.''

\begin{code}
#include "HsVersions.h"

module Outputable (
	Outputable(..), 	-- class

	PprStyle(..),
	codeStyle, ifaceStyle, userStyle,
	ifPprDebug,
	ifnotPprForUser,
	ifPprShowAll, ifnotPprShowAll,
	ifPprInterface,
	pprQuote, 

	printDoc, interppSP, interpp'SP,

	speakNth
	
#if __GLASGOW_HASKELL__ <= 200
	, Mode
#endif

    ) where

#if __GLASGOW_HASKELL__ >= 202
import IO
import GlaExts
#else
import Ubiq		( Uniquable(..), Unique, Name )	-- FastString mentions it; todo: rm

#endif

import FastString
import Pretty
import Util		( cmpPString )
\end{code}


%************************************************************************
%*									*
\subsection{The @PprStyle@ data type}
%*									*
%************************************************************************

\begin{code}
data PprStyle
  = PprForUser Int 		-- Pretty-print in a way that will
				-- make sense to the ordinary user;
				-- must be very close to Haskell
				-- syntax, etc.
				-- Parameterised over how much to expand
				-- a pretty-printed value (<= 0 => stop pp).
  | PprQuote			-- Like PprForUser, but also quote the whole thing

  | PprDebug			-- Standard debugging output
  | PprShowAll			-- Debugging output which leaves
				-- nothing to the imagination

  | PprInterface		-- Interface generation

  | PprForC			-- must print out C-acceptable names

  | PprForAsm			-- must print out assembler-acceptable names
	Bool	        	-- prefix CLabel with underscore?
	(String -> String)    	-- format AsmTempLabel

\end{code}

Orthogonal to the above printing styles are (possibly) some
command-line flags that affect printing (often carried with the
style).  The most likely ones are variations on how much type info is
shown.

The following test decides whether or not we are actually generating
code (either C or assembly), or generating interface files.
\begin{code}
codeStyle :: PprStyle -> Bool
codeStyle PprForC	  = True
codeStyle (PprForAsm _ _) = True
codeStyle _		  = False

ifaceStyle :: PprStyle -> Bool
ifaceStyle PprInterface	  = True
ifaceStyle other	  = False

userStyle ::  PprStyle -> Bool
userStyle PprQuote   = True
userStyle (PprForUser _) = True
userStyle other      = False
\end{code}

\begin{code}
ifPprDebug	sty p = case sty of PprDebug	 -> p ; _ -> empty
ifPprShowAll	sty p = case sty of PprShowAll	 -> p ; _ -> empty
ifPprInterface  sty p = case sty of PprInterface -> p ; _ -> empty

ifnotPprForUser	  sty p = case sty of { PprForUser _ -> empty ; PprQuote -> empty; _ -> p }
ifnotPprShowAll	  sty p = case sty of { PprShowAll -> empty ; _ -> p }
\end{code}

\begin{code}
pprQuote :: PprStyle -> (PprStyle -> Doc) -> Doc
pprQuote PprQuote fn = quotes (fn (PprForUser 5{-opt_PprUserLength-}))
pprQuote sty	  fn = fn sty
\end{code}



%************************************************************************
%*									*
\subsection[Outputable-class]{The @Outputable@ class}
%*									*
%************************************************************************

\begin{code}
class Outputable a where
	ppr :: PprStyle -> a -> Doc
\end{code}

\begin{code}
instance Outputable Bool where
    ppr sty True = ptext SLIT("True")
    ppr sty False = ptext SLIT("False")

instance Outputable Int where
   ppr sty n = int n

instance (Outputable a) => Outputable [a] where
    ppr sty xs = brackets (fsep (punctuate comma (map (ppr sty) xs)))

instance (Outputable a, Outputable b) => Outputable (a, b) where
    ppr sty (x,y) =
      hang (hcat [lparen, ppr sty x, comma]) 4 ((<>) (ppr sty y) rparen)

-- ToDo: may not be used
instance (Outputable a, Outputable b, Outputable c) => Outputable (a, b, c) where
    ppr sty (x,y,z) =
      parens (sep [ (<>) (ppr sty x) comma,
		      (<>) (ppr sty y) comma,
		      ppr sty z ])
\end{code}


%************************************************************************
%*									*
\subsection{Other helper functions}
%*									*
%************************************************************************

\begin{code}
printDoc :: Mode -> Handle -> Doc -> IO ()
printDoc mode hdl doc
  = fullRender mode 100 1.5 put done doc
  where
    put (Chr c)  next = hPutChar hdl c >> next 
    put (Str s)  next = hPutStr  hdl s >> next 
    put (PStr s) next = hPutFS   hdl s >> next 

    done = hPutChar hdl '\n'
\end{code}


\begin{code}
interppSP  :: Outputable a => PprStyle -> [a] -> Doc
interppSP  sty xs = hsep (map (ppr sty) xs)

interpp'SP :: Outputable a => PprStyle -> [a] -> Doc
interpp'SP sty xs
  = hsep (punctuate comma (map (ppr sty) xs))
\end{code}




%************************************************************************
%*									*
\subsection{Printing numbers verbally}
%*									*
%************************************************************************

@speakNth@ converts an integer to a verbal index; eg 1 maps to
``first'' etc.

\begin{code}
speakNth :: Int -> Doc

speakNth 1 = ptext SLIT("first")
speakNth 2 = ptext SLIT("second")
speakNth 3 = ptext SLIT("third")
speakNth 4 = ptext SLIT("fourth")
speakNth 5 = ptext SLIT("fifth")
speakNth 6 = ptext SLIT("sixth")
speakNth n = hcat [ int n, text st_nd_rd_th ]
  where
    st_nd_rd_th | n_rem_10 == 1 = "st"
		| n_rem_10 == 2 = "nd"
		| n_rem_10 == 3 = "rd"
		| otherwise     = "th"

    n_rem_10 = n `rem` 10
\end{code}
