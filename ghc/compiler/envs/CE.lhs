%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[CE]{Class environment}

\begin{code}
#include "HsVersions.h"

module CE (
	CE(..),
	nullCE, unitCE, rngCE,
	plusCE, lookupCE,
	checkClassCycles,

	-- imported things so we're self-contained...
	Unique, UniqFM,
	Class, MaybeErr, Name, Pretty(..), PprStyle,
	PrettyRep, Error(..)
	
	IF_ATTACK_PRAGMAS(COMMA emptyUFM COMMA plusUFM)
	IF_ATTACK_PRAGMAS(COMMA eltsUFM  COMMA singletonDirectlyUFM)
	IF_ATTACK_PRAGMAS(COMMA u2i)
    ) where

import AbsUniType	( getClassSig, Class, ClassOp, TyCon, FullName, Arity(..)
			  IF_ATTACK_PRAGMAS(COMMA cmpTyCon COMMA cmpClass)
			)
import Digraph		( topologicalSort )
import Errors		-- notably classCycleErr
import UniqFM		-- basic environment handling
import Maybes		( Maybe(..), MaybeErr(..) )
import Name		-- Name(..), etc.
import Pretty
import Outputable	-- def of ppr
import Unique		-- for ClassKey uniques
import Util
\end{code}

%************************************************************************
%*									*
%*		The main representation					*
%*									*
%************************************************************************

\begin{code}
--data CE = MkCE (FiniteMap Unique Class) -- keyed off Class's Uniques
type CE = UniqFM Class
#define MkCE {--}
-- also killed instance CE, exported non-abstractly

nullCE :: CE
nullCE = MkCE emptyUFM

rngCE :: CE -> [Class]
rngCE (MkCE env) = eltsUFM env

unitCE :: Unique{-ClassKey-} -> Class -> CE
unitCE u c = MkCE (singletonDirectlyUFM u c)

plusCE :: CE -> CE -> CE
plusCE (MkCE ce1) (MkCE ce2) = MkCE (plusUFM ce1 ce2)

lookupCE :: CE -> Name -> Class
lookupCE (MkCE ce) name
  = case name of
      PreludeClass key _  -> case (lookupDirectlyUFM ce key) of
				Just clas -> clas
				Nothing	-> err_msg
      OtherClass uniq _	_ -> case (lookupDirectlyUFM ce uniq) of
				Just clas -> clas
				Nothing	-> panic "lookupCE! (non-prelude)"
  where
    err_msg = error ("ERROR: in looking up a Prelude class! "++(ppShow 80 (ppr PprDebug name))++"\n(This can happen if you use `-fno-implicit-prelude'\nor you hide the system's Prelude.hi in some way.)\n")

checkClassCycles :: CE -> MaybeErr () Error
checkClassCycles (MkCE stuff)
  = case (topologicalSort (==) edges classes) of
      Succeeded _ -> Succeeded ()
      Failed cycles
	   -> Failed (classCycleErr [ map fmt_tycon c | c <- cycles ])
		where
		  fmt_tycon c = (ppr PprForUser c, getSrcLoc c)
  where
    classes = eltsUFM stuff	-- the "vertices"
    edges   = concat (map get_edges classes)

    get_edges clas
      = let  (_, super_classes, _) = getClassSig clas  in
	[ (clas, super_class) | super_class <- super_classes ]
\end{code}
