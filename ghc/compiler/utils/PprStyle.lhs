%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[PprStyle]{Pretty-printing `styles'}

\begin{code}
#include "HsVersions.h"

module PprStyle (
	PprStyle(..),
	codeStyle,
	showUserishTypes
    ) where

CHK_Ubiq() -- debugging consistency check

data PprStyle
  = PprForUser	 		-- Pretty-print in a way that will
				-- make sense to the ordinary user;
				-- must be very close to Haskell
				-- syntax, etc.  ToDo: how diff is
				-- this from what pprInterface must
				-- do?
  | PprDebug			-- Standard debugging output
  | PprShowAll			-- Debugging output which leaves
				-- nothing to the imagination
  | PprInterface		-- Interface generation
  | PprForC			-- must print out C-acceptable names
  | PprUnfolding		-- for non-interface intermodule info
				-- the compiler writes/reads
  | PprForAsm			-- must print out assembler-acceptable names
	Bool	        	-- prefix CLabel with underscore?
	(String -> String)    	-- format AsmTempLabel
\end{code}

Orthogonal to the above printing styles are (possibly) some
command-line flags that affect printing (often carried with the
style).  The most likely ones are variations on how much type info is
shown.

The following test decides whether or not we are actually generating
code (either C or assembly).
\begin{code}
codeStyle :: PprStyle -> Bool

codeStyle PprForC	  = True
codeStyle (PprForAsm _ _) = True
codeStyle _		  = False
\end{code}

\begin{code}
-- True means types like   (Eq a, Text b) => a -> b
-- False means types like  _forall_ a b => Eq a -> Text b -> a -> b
showUserishTypes PprForUser   = True	
showUserishTypes PprInterface = True
showUserishTypes other	      = False
\end{code}
