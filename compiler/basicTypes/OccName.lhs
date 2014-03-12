%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

\begin{code}
-- |
-- #name_types#
-- GHC uses several kinds of name internally:
--
-- * 'OccName.OccName' represents names as strings with just a little more information:
--   the \"namespace\" that the name came from, e.g. the namespace of value, type constructors or
--   data constructors
--
-- * 'RdrName.RdrName': see "RdrName#name_types"
--
-- * 'Name.Name': see "Name#name_types"
--
-- * 'Id.Id': see "Id#name_types"
--
-- * 'Var.Var': see "Var#name_types"

{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module OccName (
	-- * The 'NameSpace' type
	NameSpace, -- Abstract
	
	-- ** Construction
	-- $real_vs_source_data_constructors
	tcName, clsName, tcClsName, dataName, varName, 
	tvName, srcDataName,

	-- ** Pretty Printing
	pprNameSpace, pprNonVarNameSpace, pprNameSpaceBrief,

	-- * The 'OccName' type
	OccName, 	-- Abstract, instance of Outputable
	pprOccName, 

	-- ** Construction	
	mkOccName, mkOccNameFS, 
	mkVarOcc, mkVarOccFS,
	mkDataOcc, mkDataOccFS,
	mkTyVarOcc, mkTyVarOccFS,
	mkTcOcc, mkTcOccFS,
	mkClsOcc, mkClsOccFS,
        mkDFunOcc,
	mkTupleOcc, 
	setOccNameSpace,
        demoteOccName,
        HasOccName(..),

	-- ** Derived 'OccName's
        isDerivedOccName,
	mkDataConWrapperOcc, mkWorkerOcc, mkMatcherOcc, mkDefaultMethodOcc,
        mkGenDefMethodOcc, 
	mkDerivedTyConOcc, mkNewTyCoOcc, mkClassOpAuxOcc,
        mkCon2TagOcc, mkTag2ConOcc, mkMaxTagOcc,
	mkClassDataConOcc, mkDictOcc, mkIPOcc,
	mkSpecOcc, mkForeignExportOcc, mkRepEqOcc, mkGenOcc1, mkGenOcc2,
	mkGenD, mkGenR, mkGen1R, mkGenRCo, mkGenC, mkGenS,
        mkDataTOcc, mkDataCOcc, mkDataConWorkerOcc,
	mkSuperDictSelOcc, mkLocalOcc, mkMethodOcc, mkInstTyTcOcc,
	mkInstTyCoOcc, mkEqPredCoOcc,
        mkVectOcc, mkVectTyConOcc, mkVectDataConOcc, mkVectIsoOcc,
        mkPDataTyConOcc,  mkPDataDataConOcc,
	mkPDatasTyConOcc, mkPDatasDataConOcc,
        mkPReprTyConOcc, 
        mkPADFunOcc,

	-- ** Deconstruction
	occNameFS, occNameString, occNameSpace, 

	isVarOcc, isTvOcc, isTcOcc, isDataOcc, isDataSymOcc, isSymOcc, isValOcc,
	parenSymOcc, startsWithUnderscore, 
	
	isTcClsNameSpace, isTvNameSpace, isDataConNameSpace, isVarNameSpace, isValNameSpace,

	isTupleOcc_maybe,

	-- * The 'OccEnv' type
	OccEnv, emptyOccEnv, unitOccEnv, extendOccEnv, mapOccEnv,
	lookupOccEnv, mkOccEnv, mkOccEnv_C, extendOccEnvList, elemOccEnv,
	occEnvElts, foldOccEnv, plusOccEnv, plusOccEnv_C, extendOccEnv_C,
        extendOccEnv_Acc, filterOccEnv, delListFromOccEnv, delFromOccEnv,
        alterOccEnv, 

	-- * The 'OccSet' type
	OccSet, emptyOccSet, unitOccSet, mkOccSet, extendOccSet, 
	extendOccSetList,
	unionOccSets, unionManyOccSets, minusOccSet, elemOccSet, occSetElts, 
	foldOccSet, isEmptyOccSet, intersectOccSet, intersectsOccSet,
                  
	-- * Tidying up
	TidyOccEnv, emptyTidyOccEnv, tidyOccName, initTidyOccEnv,

	-- * Lexical characteristics of Haskell names
	isLexCon, isLexVar, isLexId, isLexSym,
	isLexConId, isLexConSym, isLexVarId, isLexVarSym,
	startsVarSym, startsVarId, startsConSym, startsConId
    ) where

import Util
import Unique
import BasicTypes
import DynFlags
import UniqFM
import UniqSet
import FastString
import Outputable
import Binary
import Data.Char
import Data.Data
\end{code}

%************************************************************************
%*									*
\subsection{Name space}
%*									*
%************************************************************************

\begin{code}
data NameSpace = VarName	-- Variables, including "real" data constructors
	       | DataName	-- "Source" data constructors 
	       | TvName		-- Type variables
	       | TcClsName	-- Type constructors and classes; Haskell has them
				-- in the same name space for now.
	       deriving( Eq, Ord )
   {-! derive: Binary !-}

-- Note [Data Constructors]  
-- see also: Note [Data Constructor Naming] in DataCon.lhs
--
-- $real_vs_source_data_constructors
-- There are two forms of data constructor:
--
--	[Source data constructors] The data constructors mentioned in Haskell source code
--
--	[Real data constructors] The data constructors of the representation type, which may not be the same as the source type
--
-- For example:
--
-- > data T = T !(Int, Int)
--
-- The source datacon has type @(Int, Int) -> T@
-- The real   datacon has type @Int -> Int -> T@
--
-- GHC chooses a representation based on the strictness etc.

tcName, clsName, tcClsName :: NameSpace
dataName, srcDataName      :: NameSpace
tvName, varName            :: NameSpace

-- Though type constructors and classes are in the same name space now,
-- the NameSpace type is abstract, so we can easily separate them later
tcName    = TcClsName		-- Type constructors
clsName   = TcClsName		-- Classes
tcClsName = TcClsName		-- Not sure which!

dataName    = DataName
srcDataName = DataName	-- Haskell-source data constructors should be
			-- in the Data name space

tvName      = TvName
varName     = VarName

isDataConNameSpace :: NameSpace -> Bool
isDataConNameSpace DataName = True
isDataConNameSpace _        = False

isTcClsNameSpace :: NameSpace -> Bool
isTcClsNameSpace TcClsName = True
isTcClsNameSpace _         = False

isTvNameSpace :: NameSpace -> Bool
isTvNameSpace TvName = True
isTvNameSpace _      = False

isVarNameSpace :: NameSpace -> Bool	-- Variables or type variables, but not constructors
isVarNameSpace TvName  = True
isVarNameSpace VarName = True
isVarNameSpace _       = False

isValNameSpace :: NameSpace -> Bool
isValNameSpace DataName = True
isValNameSpace VarName  = True
isValNameSpace _        = False

pprNameSpace :: NameSpace -> SDoc
pprNameSpace DataName  = ptext (sLit "data constructor")
pprNameSpace VarName   = ptext (sLit "variable")
pprNameSpace TvName    = ptext (sLit "type variable")
pprNameSpace TcClsName = ptext (sLit "type constructor or class")

pprNonVarNameSpace :: NameSpace -> SDoc
pprNonVarNameSpace VarName = empty
pprNonVarNameSpace ns = pprNameSpace ns

pprNameSpaceBrief :: NameSpace -> SDoc
pprNameSpaceBrief DataName  = char 'd'
pprNameSpaceBrief VarName   = char 'v'
pprNameSpaceBrief TvName    = ptext (sLit "tv")
pprNameSpaceBrief TcClsName = ptext (sLit "tc")

-- demoteNameSpace lowers the NameSpace if possible.  We can not know
-- in advance, since a TvName can appear in an HsTyVar.
-- See Note [Demotion] in RnEnv
demoteNameSpace :: NameSpace -> Maybe NameSpace
demoteNameSpace VarName = Nothing
demoteNameSpace DataName = Nothing
demoteNameSpace TvName = Nothing
demoteNameSpace TcClsName = Just DataName
\end{code}


%************************************************************************
%*									*
\subsection[Name-pieces-datatypes]{The @OccName@ datatypes}
%*									*
%************************************************************************

\begin{code}
data OccName = OccName 
    { occNameSpace  :: !NameSpace
    , occNameFS     :: !FastString
    }
    deriving Typeable
\end{code}


\begin{code}
instance Eq OccName where
    (OccName sp1 s1) == (OccName sp2 s2) = s1 == s2 && sp1 == sp2

instance Ord OccName where
	-- Compares lexicographically, *not* by Unique of the string
    compare (OccName sp1 s1) (OccName sp2 s2) 
	= (s1  `compare` s2) `thenCmp` (sp1 `compare` sp2)

instance Data OccName where
  -- don't traverse?
  toConstr _   = abstractConstr "OccName"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "OccName"
\end{code}


%************************************************************************
%*									*
\subsection{Printing}
%*									*
%************************************************************************
 
\begin{code}
instance Outputable OccName where
    ppr = pprOccName

instance OutputableBndr OccName where
    pprBndr _ = ppr
    pprInfixOcc n = pprInfixVar (isSymOcc n) (ppr n)
    pprPrefixOcc n = pprPrefixVar (isSymOcc n) (ppr n)

pprOccName :: OccName -> SDoc
pprOccName (OccName sp occ) 
  = getPprStyle $ \ sty ->
    if codeStyle sty 
    then ztext (zEncodeFS occ)
    else pp_occ <> pp_debug sty
  where
    pp_debug sty | debugStyle sty = braces (pprNameSpaceBrief sp)
	         | otherwise      = empty

    pp_occ = sdocWithDynFlags $ \dflags ->
             if gopt Opt_SuppressUniques dflags
             then text (strip_th_unique (unpackFS occ))
             else ftext occ

	-- See Note [Suppressing uniques in OccNames]
    strip_th_unique ('[' : c : _) | isAlphaNum c = []
    strip_th_unique (c : cs) = c : strip_th_unique cs
    strip_th_unique []       = []
\end{code}

Note [Suppressing uniques in OccNames]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This is a hack to de-wobblify the OccNames that contain uniques from
Template Haskell that have been turned into a string in the OccName.
See Note [Unique OccNames from Template Haskell] in Convert.hs

%************************************************************************
%*									*
\subsection{Construction}
%*									*
%************************************************************************

\begin{code}
mkOccName :: NameSpace -> String -> OccName
mkOccName occ_sp str = OccName occ_sp (mkFastString str)

mkOccNameFS :: NameSpace -> FastString -> OccName
mkOccNameFS occ_sp fs = OccName occ_sp fs

mkVarOcc :: String -> OccName
mkVarOcc s = mkOccName varName s

mkVarOccFS :: FastString -> OccName
mkVarOccFS fs = mkOccNameFS varName fs

mkDataOcc :: String -> OccName
mkDataOcc = mkOccName dataName

mkDataOccFS :: FastString -> OccName
mkDataOccFS = mkOccNameFS dataName

mkTyVarOcc :: String -> OccName
mkTyVarOcc = mkOccName tvName

mkTyVarOccFS :: FastString -> OccName
mkTyVarOccFS fs = mkOccNameFS tvName fs

mkTcOcc :: String -> OccName
mkTcOcc = mkOccName tcName

mkTcOccFS :: FastString -> OccName
mkTcOccFS = mkOccNameFS tcName

mkClsOcc :: String -> OccName
mkClsOcc = mkOccName clsName

mkClsOccFS :: FastString -> OccName
mkClsOccFS = mkOccNameFS clsName

-- demoteOccName lowers the Namespace of OccName.
-- see Note [Demotion]
demoteOccName :: OccName -> Maybe OccName
demoteOccName (OccName space name) = do
  space' <- demoteNameSpace space
  return $ OccName space' name

{- | Other names in the compiler add aditional information to an OccName.
This class provides a consistent way to access the underlying OccName. -}
class HasOccName name where
  occName :: name -> OccName
\end{code}


%************************************************************************
%*									*
		Environments
%*									*
%************************************************************************

OccEnvs are used mainly for the envts in ModIfaces.

Note [The Unique of an OccName]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
They are efficient, because FastStrings have unique Int# keys.  We assume
this key is less than 2^24, and indeed FastStrings are allocated keys 
sequentially starting at 0.

So we can make a Unique using
	mkUnique ns key  :: Unique
where 'ns' is a Char representing the name space.  This in turn makes it
easy to build an OccEnv.

\begin{code}
instance Uniquable OccName where
      -- See Note [The Unique of an OccName]
  getUnique (OccName VarName   fs) = mkVarOccUnique  fs
  getUnique (OccName DataName  fs) = mkDataOccUnique fs
  getUnique (OccName TvName    fs) = mkTvOccUnique   fs
  getUnique (OccName TcClsName fs) = mkTcOccUnique   fs

newtype OccEnv a = A (UniqFM a)

emptyOccEnv :: OccEnv a
unitOccEnv  :: OccName -> a -> OccEnv a
extendOccEnv :: OccEnv a -> OccName -> a -> OccEnv a
extendOccEnvList :: OccEnv a -> [(OccName, a)] -> OccEnv a
lookupOccEnv :: OccEnv a -> OccName -> Maybe a
mkOccEnv     :: [(OccName,a)] -> OccEnv a
mkOccEnv_C   :: (a -> a -> a) -> [(OccName,a)] -> OccEnv a
elemOccEnv   :: OccName -> OccEnv a -> Bool
foldOccEnv   :: (a -> b -> b) -> b -> OccEnv a -> b
occEnvElts   :: OccEnv a -> [a]
extendOccEnv_C :: (a->a->a) -> OccEnv a -> OccName -> a -> OccEnv a
extendOccEnv_Acc :: (a->b->b) -> (a->b) -> OccEnv b -> OccName -> a -> OccEnv b
plusOccEnv     :: OccEnv a -> OccEnv a -> OccEnv a
plusOccEnv_C   :: (a->a->a) -> OccEnv a -> OccEnv a -> OccEnv a
mapOccEnv      :: (a->b) -> OccEnv a -> OccEnv b
delFromOccEnv 	   :: OccEnv a -> OccName -> OccEnv a
delListFromOccEnv :: OccEnv a -> [OccName] -> OccEnv a
filterOccEnv	   :: (elt -> Bool) -> OccEnv elt -> OccEnv elt
alterOccEnv	   :: (Maybe elt -> Maybe elt) -> OccEnv elt -> OccName -> OccEnv elt

emptyOccEnv  	 = A emptyUFM
unitOccEnv x y = A $ unitUFM x y 
extendOccEnv (A x) y z = A $ addToUFM x y z
extendOccEnvList (A x) l = A $ addListToUFM x l
lookupOccEnv (A x) y = lookupUFM x y
mkOccEnv     l    = A $ listToUFM l
elemOccEnv x (A y) 	 = elemUFM x y
foldOccEnv a b (A c)	 = foldUFM a b c 
occEnvElts (A x)	 = eltsUFM x
plusOccEnv (A x) (A y)	 = A $ plusUFM x y 
plusOccEnv_C f (A x) (A y)	 = A $ plusUFM_C f x y 
extendOccEnv_C f (A x) y z   = A $ addToUFM_C f x y z
extendOccEnv_Acc f g (A x) y z   = A $ addToUFM_Acc f g x y z
mapOccEnv f (A x)	 = A $ mapUFM f x
mkOccEnv_C comb l = A $ addListToUFM_C comb emptyUFM l
delFromOccEnv (A x) y    = A $ delFromUFM x y
delListFromOccEnv (A x) y  = A $ delListFromUFM x y
filterOccEnv x (A y)       = A $ filterUFM x y
alterOccEnv fn (A y) k     = A $ alterUFM fn y k

instance Outputable a => Outputable (OccEnv a) where
    ppr (A x) = ppr x

type OccSet = UniqSet OccName

emptyOccSet	  :: OccSet
unitOccSet	  :: OccName -> OccSet
mkOccSet          :: [OccName] -> OccSet
extendOccSet      :: OccSet -> OccName -> OccSet
extendOccSetList  :: OccSet -> [OccName] -> OccSet
unionOccSets	  :: OccSet -> OccSet -> OccSet
unionManyOccSets  :: [OccSet] -> OccSet
minusOccSet 	  :: OccSet -> OccSet -> OccSet
elemOccSet	  :: OccName -> OccSet -> Bool
occSetElts	  :: OccSet -> [OccName]
foldOccSet	  :: (OccName -> b -> b) -> b -> OccSet -> b
isEmptyOccSet	  :: OccSet -> Bool
intersectOccSet   :: OccSet -> OccSet -> OccSet
intersectsOccSet  :: OccSet -> OccSet -> Bool

emptyOccSet	  = emptyUniqSet
unitOccSet	  = unitUniqSet
mkOccSet          = mkUniqSet
extendOccSet	  = addOneToUniqSet
extendOccSetList  = addListToUniqSet
unionOccSets      = unionUniqSets
unionManyOccSets  = unionManyUniqSets
minusOccSet	  = minusUniqSet
elemOccSet        = elementOfUniqSet
occSetElts        = uniqSetToList
foldOccSet	  = foldUniqSet
isEmptyOccSet     = isEmptyUniqSet
intersectOccSet   = intersectUniqSets
intersectsOccSet s1 s2 = not (isEmptyOccSet (s1 `intersectOccSet` s2))
\end{code}


%************************************************************************
%*									*
\subsection{Predicates and taking them apart}
%*									*
%************************************************************************

\begin{code}
occNameString :: OccName -> String
occNameString (OccName _ s) = unpackFS s

setOccNameSpace :: NameSpace -> OccName -> OccName
setOccNameSpace sp (OccName _ occ) = OccName sp occ

isVarOcc, isTvOcc, isTcOcc, isDataOcc :: OccName -> Bool

isVarOcc (OccName VarName _) = True
isVarOcc _                   = False

isTvOcc (OccName TvName _) = True
isTvOcc _                  = False

isTcOcc (OccName TcClsName _) = True
isTcOcc _                     = False

-- | /Value/ 'OccNames's are those that are either in 
-- the variable or data constructor namespaces
isValOcc :: OccName -> Bool
isValOcc (OccName VarName  _) = True
isValOcc (OccName DataName _) = True
isValOcc _                    = False

isDataOcc (OccName DataName _) = True
isDataOcc _                    = False

-- | Test if the 'OccName' is a data constructor that starts with
-- a symbol (e.g. @:@, or @[]@)
isDataSymOcc :: OccName -> Bool
isDataSymOcc (OccName DataName s) = isLexConSym s
isDataSymOcc _                    = False
-- Pretty inefficient!

-- | Test if the 'OccName' is that for any operator (whether 
-- it is a data constructor or variable or whatever)
isSymOcc :: OccName -> Bool
isSymOcc (OccName DataName s)  = isLexConSym s
isSymOcc (OccName TcClsName s) = isLexConSym s || isLexVarSym s
isSymOcc (OccName VarName s)   = isLexSym s
isSymOcc (OccName TvName s)    = isLexSym s
-- Pretty inefficient!

parenSymOcc :: OccName -> SDoc -> SDoc
-- ^ Wrap parens around an operator
parenSymOcc occ doc | isSymOcc occ = parens doc
		    | otherwise    = doc
\end{code}


\begin{code}
startsWithUnderscore :: OccName -> Bool
-- ^ Haskell 98 encourages compilers to suppress warnings about unsed
-- names in a pattern if they start with @_@: this implements that test
startsWithUnderscore occ = case occNameString occ of
			     ('_' : _) -> True
			     _other    -> False
\end{code}


%************************************************************************
%*									*
\subsection{Making system names}
%*									*
%************************************************************************

Here's our convention for splitting up the interface file name space:

   d...		dictionary identifiers
   		(local variables, so no name-clash worries)

All of these other OccNames contain a mixture of alphabetic
and symbolic characters, and hence cannot possibly clash with
a user-written type or function name

   $f...	Dict-fun identifiers (from inst decls)
   $dmop	Default method for 'op'
   $pnC		n'th superclass selector for class C
   $wf		Worker for functtoin 'f'
   $sf..	Specialised version of f
   T:C		Tycon for dictionary for class C
   D:C		Data constructor for dictionary for class C
   NTCo:T       Coercion connecting newtype T with its representation type
   TFCo:R       Coercion connecting a data family to its respresentation type R

In encoded form these appear as Zdfxxx etc

	:...		keywords (export:, letrec: etc.)
--- I THINK THIS IS WRONG!

This knowledge is encoded in the following functions.

@mk_deriv@ generates an @OccName@ from the prefix and a string.
NB: The string must already be encoded!

\begin{code}
mk_deriv :: NameSpace 
	 -> String		-- Distinguishes one sort of derived name from another
	 -> String
	 -> OccName

mk_deriv occ_sp sys_prefix str = mkOccName occ_sp (sys_prefix ++ str)

isDerivedOccName :: OccName -> Bool
isDerivedOccName occ = 
   case occNameString occ of
     '$':c:_ | isAlphaNum c -> True
     ':':c:_ | isAlphaNum c -> True
     _other                 -> False
\end{code}

\begin{code}
mkDataConWrapperOcc, mkWorkerOcc, mkMatcherOcc, mkDefaultMethodOcc,
        mkGenDefMethodOcc, mkDerivedTyConOcc, mkClassDataConOcc, mkDictOcc,
	mkIPOcc, mkSpecOcc, mkForeignExportOcc, mkRepEqOcc, mkGenOcc1, mkGenOcc2,
 	mkGenD, mkGenR, mkGen1R, mkGenRCo,
	mkDataTOcc, mkDataCOcc, mkDataConWorkerOcc, mkNewTyCoOcc,
	mkInstTyCoOcc, mkEqPredCoOcc, mkClassOpAuxOcc,
        mkCon2TagOcc, mkTag2ConOcc, mkMaxTagOcc
   :: OccName -> OccName

-- These derived variables have a prefix that no Haskell value could have
mkDataConWrapperOcc = mk_simple_deriv varName  "$W"
mkWorkerOcc         = mk_simple_deriv varName  "$w"
mkMatcherOcc        = mk_simple_deriv varName  "$m"
mkDefaultMethodOcc  = mk_simple_deriv varName  "$dm"
mkGenDefMethodOcc   = mk_simple_deriv varName  "$gdm"
mkClassOpAuxOcc     = mk_simple_deriv varName  "$c"
mkDerivedTyConOcc   = mk_simple_deriv tcName   ":"	-- The : prefix makes sure it classifies as a tycon/datacon
mkClassDataConOcc   = mk_simple_deriv dataName "D:"	-- We go straight to the "real" data con
							-- for datacons from classes
mkDictOcc	    = mk_simple_deriv varName  "$d"
mkIPOcc		    = mk_simple_deriv varName  "$i"
mkSpecOcc	    = mk_simple_deriv varName  "$s"
mkForeignExportOcc  = mk_simple_deriv varName  "$f"
mkRepEqOcc          = mk_simple_deriv tvName   "$r"      -- In RULES involving Coercible
mkNewTyCoOcc        = mk_simple_deriv tcName   "NTCo:"	-- Coercion for newtypes
mkInstTyCoOcc       = mk_simple_deriv tcName   "TFCo:"   -- Coercion for type functions
mkEqPredCoOcc	    = mk_simple_deriv tcName   "$co"

-- used in derived instances
mkCon2TagOcc        = mk_simple_deriv varName  "$con2tag_"
mkTag2ConOcc        = mk_simple_deriv varName  "$tag2con_"
mkMaxTagOcc         = mk_simple_deriv varName  "$maxtag_"

-- Generic derivable classes (old)
mkGenOcc1           = mk_simple_deriv varName  "$gfrom"
mkGenOcc2           = mk_simple_deriv varName  "$gto" 

-- Generic deriving mechanism (new)
mkGenD         = mk_simple_deriv tcName "D1"

mkGenC :: OccName -> Int -> OccName
mkGenC occ m   = mk_deriv tcName ("C1_" ++ show m) (occNameString occ)

mkGenS :: OccName -> Int -> Int -> OccName
mkGenS occ m n = mk_deriv tcName ("S1_" ++ show m ++ "_" ++ show n)
                   (occNameString occ)

mkGenR   = mk_simple_deriv tcName "Rep_"
mkGen1R  = mk_simple_deriv tcName "Rep1_"
mkGenRCo = mk_simple_deriv tcName "CoRep_"

-- data T = MkT ... deriving( Data ) needs definitions for 
--	$tT   :: Data.Generics.Basics.DataType
--	$cMkT :: Data.Generics.Basics.Constr
mkDataTOcc = mk_simple_deriv varName  "$t"
mkDataCOcc = mk_simple_deriv varName  "$c"

-- Vectorisation
mkVectOcc, mkVectTyConOcc, mkVectDataConOcc, mkVectIsoOcc,
 mkPADFunOcc,      mkPReprTyConOcc,
 mkPDataTyConOcc,  mkPDataDataConOcc,
 mkPDatasTyConOcc, mkPDatasDataConOcc
  :: Maybe String -> OccName -> OccName
mkVectOcc          = mk_simple_deriv_with varName  "$v"
mkVectTyConOcc     = mk_simple_deriv_with tcName   "V:"
mkVectDataConOcc   = mk_simple_deriv_with dataName "VD:"
mkVectIsoOcc       = mk_simple_deriv_with varName  "$vi"
mkPADFunOcc        = mk_simple_deriv_with varName  "$pa"
mkPReprTyConOcc    = mk_simple_deriv_with tcName   "VR:"
mkPDataTyConOcc    = mk_simple_deriv_with tcName   "VP:"
mkPDatasTyConOcc   = mk_simple_deriv_with tcName   "VPs:"
mkPDataDataConOcc  = mk_simple_deriv_with dataName "VPD:"
mkPDatasDataConOcc = mk_simple_deriv_with dataName "VPDs:"

mk_simple_deriv :: NameSpace -> String -> OccName -> OccName
mk_simple_deriv sp px occ = mk_deriv sp px (occNameString occ)

mk_simple_deriv_with :: NameSpace -> String -> Maybe String -> OccName -> OccName
mk_simple_deriv_with sp px Nothing     occ = mk_deriv sp px                  (occNameString occ)
mk_simple_deriv_with sp px (Just with) occ = mk_deriv sp (px ++ with ++ "_") (occNameString occ)

-- Data constructor workers are made by setting the name space
-- of the data constructor OccName (which should be a DataName)
-- to VarName
mkDataConWorkerOcc datacon_occ = setOccNameSpace varName datacon_occ 
\end{code}

\begin{code}
mkSuperDictSelOcc :: Int 	-- ^ Index of superclass, e.g. 3
		  -> OccName 	-- ^ Class, e.g. @Ord@
		  -> OccName	-- ^ Derived 'Occname', e.g. @$p3Ord@
mkSuperDictSelOcc index cls_tc_occ
  = mk_deriv varName "$p" (show index ++ occNameString cls_tc_occ)

mkLocalOcc :: Unique 		-- ^ Unique to combine with the 'OccName'
	   -> OccName		-- ^ Local name, e.g. @sat@
	   -> OccName		-- ^ Nice unique version, e.g. @$L23sat@
mkLocalOcc uniq occ
   = mk_deriv varName ("$L" ++ show uniq) (occNameString occ)
	-- The Unique might print with characters 
	-- that need encoding (e.g. 'z'!)
\end{code}

\begin{code}
-- | Derive a name for the representation type constructor of a
-- @data@\/@newtype@ instance.
mkInstTyTcOcc :: String 		-- ^ Family name, e.g. @Map@
              -> OccSet                 -- ^ avoid these Occs
	      -> OccName		-- ^ @R:Map@
mkInstTyTcOcc str set =
  chooseUniqueOcc tcName ('R' : ':' : str) set
\end{code}

\begin{code}
mkDFunOcc :: String		-- ^ Typically the class and type glommed together e.g. @OrdMaybe@.
				-- Only used in debug mode, for extra clarity
	  -> Bool		-- ^ Is this a hs-boot instance DFun?
          -> OccSet             -- ^ avoid these Occs
	  -> OccName		-- ^ E.g. @$f3OrdMaybe@

-- In hs-boot files we make dict funs like $fx7ClsTy, which get bound to the real
-- thing when we compile the mother module. Reason: we don't know exactly
-- what the  mother module will call it.

mkDFunOcc info_str is_boot set
  = chooseUniqueOcc VarName (prefix ++ info_str) set
  where
    prefix | is_boot   = "$fx"
	   | otherwise = "$f"
\end{code}

Sometimes we need to pick an OccName that has not already been used,
given a set of in-use OccNames.

\begin{code}
chooseUniqueOcc :: NameSpace -> String -> OccSet -> OccName
chooseUniqueOcc ns str set = loop (mkOccName ns str) (0::Int)
  where
  loop occ n
   | occ `elemOccSet` set = loop (mkOccName ns (str ++ show n)) (n+1)
   | otherwise            = occ
\end{code}

We used to add a '$m' to indicate a method, but that gives rise to bad
error messages from the type checker when we print the function name or pattern
of an instance-decl binding.  Why? Because the binding is zapped
to use the method name in place of the selector name.
(See TcClassDcl.tcMethodBind)

The way it is now, -ddump-xx output may look confusing, but
you can always say -dppr-debug to get the uniques.

However, we *do* have to zap the first character to be lower case,
because overloaded constructors (blarg) generate methods too.
And convert to VarName space

e.g. a call to constructor MkFoo where
	data (Ord a) => Foo a = MkFoo a

If this is necessary, we do it by prefixing '$m'.  These 
guys never show up in error messages.  What a hack.

\begin{code}
mkMethodOcc :: OccName -> OccName
mkMethodOcc occ@(OccName VarName _) = occ
mkMethodOcc occ                     = mk_simple_deriv varName "$m" occ
\end{code}


%************************************************************************
%*									*
\subsection{Tidying them up}
%*									*
%************************************************************************

Before we print chunks of code we like to rename it so that
we don't have to print lots of silly uniques in it.  But we mustn't
accidentally introduce name clashes!  So the idea is that we leave the
OccName alone unless it accidentally clashes with one that is already
in scope; if so, we tack on '1' at the end and try again, then '2', and
so on till we find a unique one.

There's a wrinkle for operators.  Consider '>>='.  We can't use '>>=1' 
because that isn't a single lexeme.  So we encode it to 'lle' and *then*
tack on the '1', if necessary.

Note [TidyOccEnv]
~~~~~~~~~~~~~~~~~
type TidyOccEnv = UniqFM Int

* Domain = The OccName's FastString. These FastStrings are "taken";
           make sure that we don't re-use

* Int, n = A plausible starting point for new guesses
           There is no guarantee that "FSn" is available; 
           you must look that up in the TidyOccEnv.  But
           it's a good place to start looking.

* When looking for a renaming for "foo2" we strip off the "2" and start
  with "foo".  Otherwise if we tidy twice we get silly names like foo23.

\begin{code}
type TidyOccEnv = UniqFM Int	-- The in-scope OccNames
  -- See Note [TidyOccEnv]

emptyTidyOccEnv :: TidyOccEnv
emptyTidyOccEnv = emptyUFM

initTidyOccEnv :: [OccName] -> TidyOccEnv	-- Initialise with names to avoid!
initTidyOccEnv = foldl add emptyUFM
  where
    add env (OccName _ fs) = addToUFM env fs 1

tidyOccName :: TidyOccEnv -> OccName -> (TidyOccEnv, OccName)
tidyOccName env occ@(OccName occ_sp fs)
  = case lookupUFM env fs of
	Just n  -> find n
	Nothing -> (addToUFM env fs 1, occ)
  where
    base :: String  -- Drop trailing digits (see Note [TidyOccEnv])
    base = reverse (dropWhile isDigit (reverse (unpackFS fs)))
 
    find n 
      = case lookupUFM env new_fs of
          Just n' -> find (n1 `max` n')
                     -- The max ensures that n increases, avoiding loops
          Nothing -> (addToUFM (addToUFM env fs n1) new_fs n1,
                      OccName occ_sp new_fs)
                     -- We update only the beginning and end of the
                     -- chain that find explores; it's a little harder to
                     -- update the middle and there's no real need.
       where
         n1 = n+1
         new_fs = mkFastString (base ++ show n)
\end{code}

%************************************************************************
%*									*
		Stuff for dealing with tuples
%*									*
%************************************************************************

\begin{code}
mkTupleOcc :: NameSpace -> TupleSort -> Arity -> OccName
mkTupleOcc ns sort ar = OccName ns (mkFastString str)
  where
 	-- no need to cache these, the caching is done in the caller
	-- (TysWiredIn.mk_tuple)
    str = case sort of
		UnboxedTuple    -> '(' : '#' : commas ++ "#)"
		BoxedTuple      -> '(' : commas ++ ")"
                ConstraintTuple -> '(' : commas ++ ")"
                  -- Cute hack: reuse the standard tuple OccNames (and hence code)
                  -- for fact tuples, but give them different Uniques so they are not equal.
                  --
                  -- You might think that this will go wrong because isTupleOcc_maybe won't
                  -- be able to tell the difference between boxed tuples and fact tuples. BUT:
                  --  1. Fact tuples never occur directly in user code, so it doesn't matter
                  --     that we can't detect them in Orig OccNames originating from the user
                  --     programs (or those built by setRdrNameSpace used on an Exact tuple Name)
                  --  2. Interface files have a special representation for tuple *occurrences*
                  --     in IfaceTyCons, their workers (in IfaceSyn) and their DataCons (in case
                  --     alternatives). Thus we don't rely on the OccName to figure out what kind
                  --     of tuple an occurrence was trying to use in these situations.
                  --  3. We *don't* represent tuple data type declarations specially, so those
                  --     are still turned into wired-in names via isTupleOcc_maybe. But that's OK
                  --     because we don't actually need to declare fact tuples thanks to this hack.
                  --
                  -- So basically any OccName like (,,) flowing to isTupleOcc_maybe will always
                  -- refer to the standard boxed tuple. Cool :-)

    commas = take (ar-1) (repeat ',')

isTupleOcc_maybe :: OccName -> Maybe (NameSpace, TupleSort, Arity)
-- Tuples are special, because there are so many of them!
isTupleOcc_maybe (OccName ns fs)
  = case unpackFS fs of
	'(':'#':',':rest     -> Just (ns, UnboxedTuple, 2 + count_commas rest)
	'(':',':rest         -> Just (ns, BoxedTuple,   2 + count_commas rest)
	_other               -> Nothing
  where
    count_commas (',':rest) = 1 + count_commas rest
    count_commas _          = 0
\end{code}

%************************************************************************
%*									*
\subsection{Lexical categories}
%*									*
%************************************************************************

These functions test strings to see if they fit the lexical categories
defined in the Haskell report.

\begin{code}
isLexCon,   isLexVar,    isLexId,    isLexSym    :: FastString -> Bool
isLexConId, isLexConSym, isLexVarId, isLexVarSym :: FastString -> Bool

isLexCon cs = isLexConId  cs || isLexConSym cs
isLexVar cs = isLexVarId  cs || isLexVarSym cs

isLexId  cs = isLexConId  cs || isLexVarId  cs
isLexSym cs = isLexConSym cs || isLexVarSym cs

-------------

isLexConId cs				-- Prefix type or data constructors
  | nullFS cs	       = False		-- 	e.g. "Foo", "[]", "(,)" 
  | cs == (fsLit "[]") = True
  | otherwise	       = startsConId (headFS cs)

isLexVarId cs				-- Ordinary prefix identifiers
  | nullFS cs	      = False		-- 	e.g. "x", "_x"
  | otherwise         = startsVarId (headFS cs)

isLexConSym cs				-- Infix type or data constructors
  | nullFS cs	       = False		--	e.g. ":-:", ":", "->"
  | cs == (fsLit "->") = True
  | otherwise	       = startsConSym (headFS cs)

isLexVarSym cs				-- Infix identifiers
  | nullFS cs	      = False		-- 	e.g. "+"
  | otherwise         = startsVarSym (headFS cs)

-------------
startsVarSym, startsVarId, startsConSym, startsConId :: Char -> Bool
startsVarSym c = isSymbolASCII c || (ord c > 0x7f && isSymbol c) -- Infix Ids
startsConSym c = c == ':'				-- Infix data constructors
startsVarId c  = isLower c || c == '_'	-- Ordinary Ids
startsConId c  = isUpper c || c == '('	-- Ordinary type constructors and data constructors

isSymbolASCII :: Char -> Bool
isSymbolASCII c = c `elem` "!#$%&*+./<=>?@\\^|~-"
\end{code}

%************************************************************************
%*									*
		Binary instance
    Here rather than BinIface because OccName is abstract
%*									*
%************************************************************************

\begin{code}
instance Binary NameSpace where
    put_ bh VarName = do
	    putByte bh 0
    put_ bh DataName = do
	    putByte bh 1
    put_ bh TvName = do
	    putByte bh 2
    put_ bh TcClsName = do
	    putByte bh 3
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return VarName
	      1 -> do return DataName
	      2 -> do return TvName
	      _ -> do return TcClsName

instance Binary OccName where
    put_ bh (OccName aa ab) = do
	    put_ bh aa
	    put_ bh ab
    get bh = do
	  aa <- get bh
	  ab <- get bh
	  return (OccName aa ab)
\end{code}
