%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[PprStyle]{Pretty-printing `styles'}

\begin{code}
#include "HsVersions.h"

module PprStyle (
	PprStyle(..),
	codeStyle, ifaceStyle, userStyle
    ) where

CHK_Ubiq() -- debugging consistency check

import FastString

data PprStyle
  = PprForUser	 		-- Pretty-print in a way that will
				-- make sense to the ordinary user;
				-- must be very close to Haskell
				-- syntax, etc.
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
userStyle PprForUser = True
userStyle other      = False
\end{code}
