%************************************************************************
%*									*
\section[ProtoName]{@ProtoName@: name type used early in the compiler}
%*									*
%************************************************************************

\begin{code}
#include "HsVersions.h"

module ProtoName (
	ProtoName(..),

	mkPreludeProtoName,

	cmpProtoName, eqProtoName, elemProtoNames,
	cmpByLocalName, eqByLocalName, elemByLocalNames,
	
	isConopPN,

	-- and to make the module self-sufficient...
	Name, Maybe
#ifndef __GLASGOW_HASKELL__
	,TAG_
#endif
    ) where

IMPORT_Trace		-- ToDo: rm (debugging)

import Name		( cmpName, Name
			  IF_ATTACK_PRAGMAS(COMMA eqName)
			)
import Outputable
import Pretty
import Util
\end{code}

%************************************************************************
%*									*
\subsection{The main type declaration}
%*									*
%************************************************************************

\begin{code}
data ProtoName
  = Unk		FAST_STRING	-- local name in module

  | Imp		FAST_STRING	-- name of defining module 
		FAST_STRING	-- name used in defining name
		[FAST_STRING]	-- name of the module whose interfaces
				-- told me about this thing
		FAST_STRING	-- occurrence name; Nothing => same as field 2
  | Prel	Name
{- LATER:
  | Unk2	FAST_INT	-- same as Unk but this FAST_INT is
				-- the index into hash table (makes for
				-- superbly great equality comparisons!)
		FAST_STRING
-}
\end{code}

%************************************************************************
%*									*
\subsection{Construction}
%*									*
%************************************************************************

\begin{code}
mkPreludeProtoName :: Name -> ProtoName

mkPreludeProtoName prel_name = Prel prel_name
\end{code}

%************************************************************************
%*									*
\subsection{Ordering}
%*									*
%************************************************************************

Comparing @ProtoNames@.  These functions are used to bring together
duplicate declarations for things, and eliminate all but one.

In general, the things thus manipulated are not prelude things, but we
still need to be able to compare prelude classes and type constructors
so that we can compare instance declarations.  However, since all
Prelude classes and type constructors come from @PreludeCore@, and
hence can't not be in scope, they will always be of the form (@Prel@
n), so we don't need to compare @Prel@ things against @Imp@ or @Unk@
things.

(Later the same night...: but, oh yes, you do:

Given two instance decls
    
\begin{verbatim}
instance Eq  {-PreludeCore-}	Foo
instance Bar {-user-defined-} 	Foo
\end{verbatim}

you will get a comparison of "Eq" (a Prel) with "Bar" (an {Unk,Imp})) 

@cmp_name@ compares either by ``local name'' (the string by which
the entity is known in this module, renaming and all) or by original
name, in which case the module name is also taken into account.
(Just watch what happens on @Imps@...)

\begin{code}
cmp_name :: Bool -> ProtoName -> ProtoName -> TAG_

cmp_name by_local (Unk n1) (Unk n2)        = _CMP_STRING_ n1 n2
cmp_name by_local (Unk n1) (Imp m n2 _ o2) = _CMP_STRING_ n1 (if by_local then o2 else n2)
cmp_name by_local (Unk n1) (Prel nm)
  =  let  (_, n2) = getOrigName nm  in
     _CMP_STRING_ n1 n2

cmp_name by_local (Prel n1) (Prel n2) = cmpName n1 n2

-- in ordering these things, it's *most* important to have "names" (vs "modules")
-- as the primary comparison key; otherwise, a list of ProtoNames like...
--
--	Imp H.T , Imp P.I , Unk T
--
-- will *not* be re-ordered to bring the "Imp H.T" and "Unk T" `next to each other'...
--

cmp_name True  (Imp _ _ _ o1) (Imp _ _ _ o2) = _CMP_STRING_ o1 o2

cmp_name False (Imp m1 n1 _ _) (Imp m2 n2 _ _)
  = case _CMP_STRING_ n1 n2 of {
      LT_ -> LT_;
      EQ_ -> case _CMP_STRING_ m1 m2 of {
	       EQ_ -> EQ_;
	       xxx -> if _NULL_ m1 || _NULL_ m2
		      then EQ_
		      else xxx
	     };
      GT__ -> GT_
    }
    -- That's a real **HACK** on comparing "original module" names!
    -- The thing is: we `make up' ProtoNames for instances for
    -- sorting-out-interfaces purposes, but we *may* not know the
    -- original module, so it will be Nil.  This is the *ONLY* way
    -- that a "" `module name' can arise!  Rather than say "not equal",
    -- we want that Nil to compare as a `wildcard', matching anything.
    --
    -- We could do this elsewhere in the compiler, but there is
    -- an efficiency issue -- we plow through *piles* of instances.

cmp_name True (Imp _ _ _ o1) (Prel nm)
  = let
	n2 = case (getOrigName nm) of { (_, x) -> x } -- stricter for speed
    in
    _CMP_STRING_ o1 n2

cmp_name False (Imp m1 n1 _ _) (Prel nm)
  = case getOrigName nm   of { (m2, n2) ->
    case _CMP_STRING_ n1 n2 of { LT_ -> LT_; EQ_ -> _CMP_STRING_ m1 m2; GT__ -> GT_ }}

cmp_name by_local other_p1 other_p2
  = case cmp_name by_local other_p2 other_p1 of -- compare the other way around
      LT_  -> GT_
      EQ_  -> EQ_
      GT__ -> LT_
\end{code}

\begin{code}
eqProtoName, eqByLocalName :: ProtoName -> ProtoName -> Bool

eqProtoName a b
  = case cmp_name False a b of { EQ_ -> True; _ -> False }

cmpProtoName a b = cmp_name False a b

eqByLocalName a b
  = case cmp_name True  a b of { EQ_ -> True; _ -> False }

cmpByLocalName a b = cmp_name True a b
\end{code}

\begin{code}
elemProtoNames, elemByLocalNames :: ProtoName -> [ProtoName] -> Bool

elemProtoNames _ []	= False
elemProtoNames x (y:ys)
  = case cmp_name False x y of
      LT_  -> elemProtoNames x ys
      EQ_  -> True
      GT__ -> elemProtoNames x ys

elemByLocalNames _ []	  = False
elemByLocalNames x (y:ys)
  = case cmp_name True x y of
      LT_  -> elemByLocalNames x ys
      EQ_  -> True
      GT__ -> elemByLocalNames x ys

isConopPN :: ProtoName -> Bool
isConopPN (Unk s)	= isConop s
isConopPN (Imp _ n _ _) = isConop n -- ToDo: should use occurrence name???
\end{code}

%************************************************************************
%*									*
\subsection{Instances}
%*									*
%************************************************************************

********** REMOVE THESE WHEN WE FIX THE SET-ery IN RenameBinds4 *********

\begin{code}
{- THESE INSTANCES ARE TOO DELICATE TO BE USED!
Use eqByLocalName, ...., etc. instead

instance Eq ProtoName where
    a == b = case cmp_name False a b of { EQ_ -> True; _ -> False }

instance Ord ProtoName where
    a <  b = case cmp_name False a b of { LT_ -> True; EQ_ -> False; GT__ -> False }
    a <= b = case cmp_name False a b of { LT_ -> True; EQ_ -> True;  GT__ -> False }
-}
\end{code}

\begin{code}
instance NamedThing ProtoName where

    getOrigName (Unk _)		= panic "NamedThing.ProtoName.getOrigName (Unk)"
    getOrigName (Imp m s _ _)	= (m, s)
    getOrigName (Prel name)	= getOrigName name

    getOccurrenceName (Unk s)	    = s
    getOccurrenceName (Imp m s _ o) = o
    getOccurrenceName (Prel name)   = getOccurrenceName name

    hasType pn			= False

#ifdef DEBUG
    getSrcLoc pn		= panic "NamedThing.ProtoName.getSrcLoc"
    getInformingModules pn	= panic "NamedThing.ProtoName.getInformingModule"
    getTheUnique pn		= panic "NamedThing.ProtoName.getUnique"
    fromPreludeCore pn		= panic "NamedThing.ProtoName.fromPreludeCore"
    getExportFlag pn		= panic "NamedThing.ProtoName.getExportFlag"
    isLocallyDefined pn		= panic "NamedThing.ProtoName.isLocallyDefined"
    getType pn			= panic "NamedThing.ProtoName.getType"
#endif
\end{code}

\begin{code}
instance Outputable ProtoName where
    ppr sty (Unk s)     = ppPStr s
    ppr sty (Prel name) = ppBeside (ppr sty name) (ifPprShowAll sty (ppPStr SLIT("/PREL")))
    ppr sty (Imp mod dec imod loc)
      = ppBesides [ppPStr mod, ppChar '.', ppPStr dec, pp_occur_name dec loc ]
	-- ToDo: print "informant modules" if high debugging level
      where
	 pp_occur_name s o | s /= o    = ppBesides [ppChar '{', ppPStr o, ppChar '}']
			   | otherwise = ppNil
\end{code}
