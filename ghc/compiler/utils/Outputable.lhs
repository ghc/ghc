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

	printDoc,

	interppSP, interpp'SP,
	ifnotPprForUser,
	ifPprDebug,
	ifPprShowAll, ifnotPprShowAll,
	ifPprInterface,
	pprQuote,

	speakNth
	
#if __GLASGOW_HASKELL__ <= 200
	, Mode
#endif

    ) where

IMP_Ubiq(){-uitous-}

#if __GLASGOW_HASKELL__ >= 202
import IO
#endif

import PprStyle		( PprStyle(..) )
import Pretty
import Util		( cmpPString )
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

\begin{code}
ifPprDebug	sty p = case sty of PprDebug	 -> p ; _ -> empty
ifPprShowAll	sty p = case sty of PprShowAll	 -> p ; _ -> empty
ifPprInterface  sty p = case sty of PprInterface -> p ; _ -> empty

ifnotPprForUser	  sty p = case sty of { PprForUser -> empty ; PprQuote -> empty; _ -> p }
ifnotPprShowAll	  sty p = case sty of { PprShowAll -> empty ; _ -> p }
\end{code}

\begin{code}
pprQuote :: PprStyle -> (PprStyle -> Doc) -> Doc
pprQuote PprQuote fn = quotes (fn PprForUser)
pprQuote sty	  fn = fn sty
\end{code}


\begin{code}
instance Outputable Bool where
    ppr sty True = ptext SLIT("True")
    ppr sty False = ptext SLIT("False")

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
