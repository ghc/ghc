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

	interppSP, interpp'SP,
	ifnotPprForUser,
	ifPprDebug,
	ifPprShowAll, ifnotPprShowAll,
	ifPprInterface
    ) where

import Ubiq{-uitous-}

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
	ppr :: PprStyle -> a -> Pretty
\end{code}

\begin{code}
-- the ppSep in the ppInterleave puts in the spaces
-- Death to ppSep! (WDP 94/11)

interppSP  :: Outputable a => PprStyle -> [a] -> Pretty
interppSP  sty xs = ppIntersperse ppSP (map (ppr sty) xs)

interpp'SP :: Outputable a => PprStyle -> [a] -> Pretty
interpp'SP sty xs
  = ppIntersperse sep (map (ppr sty) xs)
  where
    sep = ppBeside ppComma ppSP

#ifdef USE_ATTACK_PRAGMAS
{-# SPECIALIZE interppSP :: PprStyle -> [Id] -> Pretty #-}
{-# SPECIALIZE interppSP :: PprStyle -> [TyVar] -> Pretty #-}

{-# SPECIALIZE interpp'SP :: PprStyle -> [(Id, Id)] -> Pretty #-}
{-# SPECIALIZE interpp'SP :: PprStyle -> [Id] -> Pretty #-}
{-# SPECIALIZE interpp'SP :: PprStyle -> [TyVarTemplate] -> Pretty #-}
{-# SPECIALIZE interpp'SP :: PprStyle -> [TyVar] -> Pretty #-}
{-# SPECIALIZE interpp'SP :: PprStyle -> [Type] -> Pretty #-}
#endif
\end{code}

\begin{code}
ifPprDebug	sty p = case sty of PprDebug	 -> p ; _ -> ppNil
ifPprShowAll	sty p = case sty of PprShowAll	 -> p ; _ -> ppNil
ifPprInterface  sty p = case sty of PprInterface -> p ; _ -> ppNil

ifnotPprForUser	  sty p = case sty of PprForUser -> ppNil ; _ -> p
ifnotPprShowAll	  sty p = case sty of PprShowAll -> ppNil ; _ -> p
\end{code}

\begin{code}
instance Outputable Bool where
    ppr sty True = ppPStr SLIT("True")
    ppr sty False = ppPStr SLIT("False")

instance (Outputable a) => Outputable [a] where
    ppr sty xs =
      ppBesides [ ppLbrack, ppInterleave ppComma (map (ppr sty) xs), ppRbrack ]

instance (Outputable a, Outputable b) => Outputable (a, b) where
    ppr sty (x,y) =
      ppHang (ppBesides [ppLparen, ppr sty x, ppComma]) 4 (ppBeside (ppr sty y) ppRparen)

-- ToDo: may not be used
instance (Outputable a, Outputable b, Outputable c) => Outputable (a, b, c) where
    ppr sty (x,y,z) =
      ppSep [ ppBesides [ppLparen, ppr sty x, ppComma],
	      ppBeside (ppr sty y) ppComma,
	      ppBeside (ppr sty z) ppRparen ]
\end{code}
