%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrefixSyn]{``Prefix-form'' syntax}

This module contains an algebraic data type into which a prefix form
string from the current Haskell parser is converted.  Given in an
order that follows the \tr{Prefix_Form} document.

\begin{code}
#include "HsVersions.h"

module PrefixSyn (
	RdrBinding(..),
	RdrId(..),
	RdrMatch(..),
	RdrTySigPragmas(..),
	SigConverter(..),
	SrcFile(..),
	SrcFun(..),
	SrcLine(..),

	readInteger
    ) where

import Ubiq{-uitous-}

import HsSyn
import RdrHsSyn
import Util		( panic )

type RdrId   = ProtoName
type SrcLine = Int
type SrcFile = FAST_STRING
type SrcFun  = ProtoName
\end{code}

\begin{code}
data RdrBinding
  = RdrNullBind
  | RdrAndBindings	RdrBinding RdrBinding

  | RdrTyDecl		ProtoNameTyDecl
  | RdrFunctionBinding	SrcLine [RdrMatch]
  | RdrPatternBinding	SrcLine [RdrMatch]
  | RdrClassDecl 	ProtoNameClassDecl
  | RdrInstDecl 	ProtoNameInstDecl
  | RdrDefaultDecl	ProtoNameDefaultDecl
  | RdrIfaceImportDecl	(IfaceImportDecl ProtoName)
  | RdrIfaceFixities	[ProtoNameFixityDecl]

			-- signatures are mysterious; we can't
			-- tell if its a Sig or a ClassOpSig,
			-- so we just save the pieces:
  | RdrTySig		[ProtoName]	    -- vars getting sigs
			ProtoNamePolyType   -- the type
			RdrTySigPragmas	    -- val/class-op pragmas
			SrcLoc

  -- user pragmas come in in a Sig-ish way/form...
  | RdrSpecValSig   	[ProtoNameSig]
  | RdrInlineValSig 	ProtoNameSig
  | RdrDeforestSig 	ProtoNameSig
  | RdrMagicUnfoldingSig ProtoNameSig
  | RdrSpecInstSig  	ProtoNameSpecInstSig
  | RdrSpecDataSig   	ProtoNameSpecDataSig

data RdrTySigPragmas
  = RdrNoPragma
  | RdrGenPragmas	ProtoNameGenPragmas
  | RdrClassOpPragmas	ProtoNameClassOpPragmas

type SigConverter = RdrBinding {- a RdrTySig... -} -> [ProtoNameSig]
\end{code}

\begin{code}
data RdrMatch
  = RdrMatch_NoGuard
	     SrcLine SrcFun
	     ProtoNamePat
	     ProtoNameHsExpr
	     RdrBinding

  | RdrMatch_Guards
	     SrcLine SrcFun
	     ProtoNamePat
	     [(ProtoNameHsExpr, ProtoNameHsExpr)]
	     -- (guard,         expr)
	     RdrBinding
\end{code}

Unscramble strings representing oct/dec/hex integer literals:
\begin{code}
readInteger :: String -> Integer

readInteger ('-' : xs)	     = - (readInteger xs)
readInteger ('0' : 'o' : xs) = chk (stoo 0 xs)
readInteger ('0' : 'x' : xs) = chk (stox 0 xs)
readInteger ['0']	     = 0    -- efficiency shortcut?
readInteger ['1']	     = 1    -- ditto?
readInteger xs		     = chk (stoi 0 xs)

chk (i, "")   = i
chk (i, junk) = panic ("readInteger: junk after reading:"++junk)

stoo, stoi, stox :: Integer -> String -> (Integer, String)

stoo a (c:cs) | is_oct c  = stoo (a*8 + ord_ c - ord_0) cs
stoo a cs                 = (a, cs)

stoi a (c:cs) | isDigit c = stoi (a*10 + ord_ c - ord_0) cs
stoi a cs                 = (a, cs)

stox a (c:cs) | isDigit c = stox (a_16_ord_c - ord_0)      cs
	      | is_hex  c = stox (a_16_ord_c - ord_a + 10) cs
	      | is_Hex  c = stox (a_16_ord_c - ord_A + 10) cs
	      where a_16_ord_c = a*16 + ord_ c
stox a cs = (a, cs)

is_oct c = c >= '0' && c <= '7'
is_hex c = c >= 'a' && c <= 'f'
is_Hex c = c >= 'A' && c <= 'F'

ord_ c = toInteger (ord c)

ord_0, ord_a, ord_A :: Integer
ord_0 = ord_ '0'; ord_a = ord_ 'a'; ord_A = ord_ 'A'
\end{code}
