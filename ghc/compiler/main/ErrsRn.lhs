%
% (c) The AQUA Project, Glasgow University, 1994-1995
%
\section[ErrsRn]{Reporting errors from the renamer}

This is an internal module---access to these functions is through
@Errors@.

\begin{code}
#include "HsVersions.h"

module ErrsRn where

import AbsSyn		-- we print a bunch of stuff in here
import AbsUniType	( TyVarTemplate )
import UniType		( UniType(..) )
			-- UniType is concrete, to make some errors
			-- more informative.
import ErrUtils
import Name		( cmpName )
import Outputable
import Pretty		-- to pretty-print error messages
import SrcLoc		( mkUnknownSrcLoc, SrcLoc )
import Util
\end{code}

\begin{code}
badClassOpErr :: Name{-class-} -> ProtoName{-op-} -> SrcLoc -> Error
	-- Class op expected but something else found
badClassOpErr clas op locn
  = addErrLoc locn "" ( \ sty ->
    ppBesides [ppChar '`', ppr sty op, ppStr "' is not an operation of class `",
	      ppr sty clas, ppStr "'."] )

----------------------------------------------------------------
badExportNameErr :: String -> String -> Error

badExportNameErr name whats_wrong
  = dontAddErrLoc
	"Error in the export list" ( \ sty ->
    ppBesides [ppChar '`', ppStr name, ppStr "' ", ppStr whats_wrong] )

----------------------------------------------------------------
badImportNameErr :: String -> String -> String -> SrcLoc -> Error

badImportNameErr mod name whats_wrong locn
  = addErrLoc locn
	("Error in an import list for the module `"++mod++"'") ( \ sty ->
    ppBesides [ppChar '`', ppStr name, ppStr "' ", ppStr whats_wrong] )

----------------------------------------------------------------
derivingInIfaceErr :: ProtoName -> [ProtoName] -> SrcLoc -> Error
	-- GHC doesn't support "deriving" in interfaces

derivingInIfaceErr ty deriveds locn
  = addErrLoc locn "Glasgow Haskell doesn't support `deriving' in interfaces" ( \ sty ->
    ppBesides [ ppStr "type: ", ppr sty ty,
		ppStr "; derived: ", interpp'SP sty deriveds ] )

----------------------------------------------------------------
derivingNonStdClassErr :: Name -> ProtoName -> SrcLoc -> Error
	-- if "deriving" specified for a non-standard class

derivingNonStdClassErr tycon clas locn
  = addErrLoc locn "Can't have a derived instance of this class" ( \ sty ->
    ppBesides [ppStr "type constructor: ", ppr sty tycon,
				 ppStr "; class: ", ppr sty clas] )

----------------------------------------------------------------
dupNamesErr :: String -> [(ProtoName,SrcLoc)] -> Error

dupNamesErr descriptor ((first_pname,locn1) : dup_things) sty
  = ppAboves (first_item : map dup_item dup_things)
  where
    first_item
      = ppBesides [ ppr PprForUser locn1,
	    ppStr ": multiple declarations of a ", ppStr descriptor, ppStr ": ",
	    ppr sty first_pname ]

    dup_item (pname, locn)
      = ppBesides [ ppr PprForUser locn,
	    ppStr ": here was another declaration of `", ppr sty pname, ppStr "'" ]

----------------------------------------------------------------
dupPreludeNameErr :: String -> (ProtoName, SrcLoc) -> Error

dupPreludeNameErr descriptor (nm, locn)
  = addShortErrLocLine locn ( \ sty ->
    ppBesides [ ppStr "A conflict with a Prelude ", ppStr descriptor,
		ppStr ": ", ppr sty nm ])

----------------------------------------------------------------
dupSigDeclErr :: [RenamedSig] -> Error
	-- Duplicate signatures in a group; the sigs have locns on them
dupSigDeclErr sigs
  = let
	undup_sigs = fst (removeDups cmp_sig sigs)
    in
    addErrLoc locn1
	("more than one "++what_it_is++"\n\thas been given for these variables") ( \ sty ->
    ppAboves (map (ppr sty) undup_sigs) )
  where
    (what_it_is, locn1)
      = case (head sigs) of
	  Sig        _ _ _ loc -> ("type signature",loc)
	  ClassOpSig _ _ _ loc -> ("class-method type signature", loc)
	  SpecSig    _ _ _ loc -> ("SPECIALIZE pragma",loc)
	  InlineSig  _ _   loc -> ("INLINE pragma",loc)
	  MagicUnfoldingSig _ _ loc -> ("MAGIC_UNFOLDING pragma",loc)

    cmp_sig a b = get_name a `cmpName` get_name b

    get_name (Sig        n _ _ _) = n
    get_name (ClassOpSig n _ _ _) = n
    get_name (SpecSig    n _ _ _) = n
    get_name (InlineSig  n _   _) = n
    get_name (MagicUnfoldingSig n _ _) = n

----------------------------------------------------------------
duplicateImportsInInterfaceErr :: String -> [ProtoName] -> Error
duplicateImportsInInterfaceErr iface dups
  = panic "duplicateImportsInInterfaceErr: NOT DONE YET?"

----------------------------------------------------------------
inlineInRecursiveBindsErr  :: [(Name, SrcLoc)] -> Error

inlineInRecursiveBindsErr [(name, locn)]
  = addShortErrLocLine locn ( \ sty ->
    ppBesides [ppStr "INLINE pragma for a recursive definition: ",
	ppr sty name] )
inlineInRecursiveBindsErr names_n_locns
  = \ sty ->
    ppHang (ppStr "INLINE pragmas for some recursive definitions:")
	 4 (ppAboves [ ppBesides [ppr PprForUser locn, ppStr ": ", ppr sty n]
		     | (n, locn) <- names_n_locns ])

----------------------------------------------------------------
--mismatchedPragmasErr :: (Annotations, SrcLoc)
--		     -> (Annotations, SrcLoc)
--		     -> Error
{- UNUSED:
mismatchedPragmasErr (anns1, _) (anns2, _)
  = dontAddErrLoc "Mismatched pragmas from interfaces" ( \ sty ->
    ppSep [ppr sty anns1, ppr sty anns2] )
-}

----------------------------------------------------------------
shadowedNameErr :: Name -> SrcLoc -> Error
shadowedNameErr shadow locn
  = addShortErrLocLine locn ( \ sty ->
    ppBesides [ppStr "more than one value with the same name (shadowing): ",
	ppr sty shadow] )

----------------------------------------------------------------
unknownNameErr :: String -> ProtoName -> SrcLoc -> Error
unknownNameErr descriptor undef_thing locn
  = addShortErrLocLine locn ( \ sty ->
    ppBesides [ppStr "undefined ", ppStr descriptor, ppStr ": ",
	ppr sty undef_thing] )

----------------------------------------------------------------
missingSigErr :: SrcLoc -> ProtoName -> Error
	-- Top-level definition without a type signature
	-- (when SigsRequired flag is in use)
missingSigErr locn var
  = addShortErrLocLine locn ( \ sty ->
    ppBesides [ppStr "a definition but no type signature for `",
	       ppr sty var,
	       ppStr "'."])

----------------------------------------------------------------
unknownSigDeclErr :: String -> ProtoName -> SrcLoc -> Error
	-- Signature/Pragma given for unknown variable
unknownSigDeclErr flavor var locn
  = addShortErrLocLine locn ( \ sty ->
    ppBesides [ppStr flavor, ppStr " but no definition for `",
	       ppr sty var,
	       ppStr "'."])

----------------------------------------------------------------
weirdImportExportConstraintErr :: ProtoName -> IE -> SrcLoc -> Error

weirdImportExportConstraintErr thing constraint locn
  = addShortErrLocLine locn ( \ sty ->
    ppBesides [ppStr "Illegal import/export constraint on `",
	       ppr sty thing,
	       ppStr "': ", ppr PprForUser constraint])

----------------------------------------------------------------
methodBindErr :: ProtoNameMonoBinds -> SrcLoc -> Error
methodBindErr mbind locn
 = addErrLoc locn "Can't handle multiple methods defined by one pattern binding"
	(\ sty -> ppr sty mbind)
\end{code}
