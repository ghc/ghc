%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsBinds]{Abstract syntax: top-level bindings and signatures}

Datatype for: @BindGroup@, @Bind@, @Sig@, @Bind@.

\begin{code}
{-# OPTIONS -fno-warn-incomplete-patterns #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details
{-# LANGUAGE DeriveDataTypeable #-}

module HsBinds where

import {-# SOURCE #-} HsExpr ( pprExpr, LHsExpr,
			       MatchGroup, pprFunBind,
			       GRHSs, pprPatBind )
import {-# SOURCE #-} HsPat  ( LPat )

import HsTypes
import PprCore ()
import Coercion
import Type
import Name
import NameSet
import BasicTypes
import Outputable	
import SrcLoc
import Util
import VarEnv
import Var
import Bag
import Unique
import FastString

import Data.IORef( IORef )
import Data.Data hiding ( Fixity )
\end{code}

%************************************************************************
%*									*
\subsection{Bindings: @BindGroup@}
%*									*
%************************************************************************

Global bindings (where clauses)

\begin{code}
-- During renaming, we need bindings where the left-hand sides
-- have been renamed but the the right-hand sides have not.
-- the ...LR datatypes are parametrized by two id types,
-- one for the left and one for the right.
-- Other than during renaming, these will be the same.

type HsLocalBinds id = HsLocalBindsLR id id

data HsLocalBindsLR idL idR	-- Bindings in a 'let' expression
			       -- or a 'where' clause
  = HsValBinds (HsValBindsLR idL idR)
  | HsIPBinds  (HsIPBinds idR)
  | EmptyLocalBinds
  deriving (Data, Typeable)

type HsValBinds id = HsValBindsLR id id

data HsValBindsLR idL idR  -- Value bindings (not implicit parameters)
  = ValBindsIn             -- Before renaming
	(LHsBindsLR idL idR) [LSig idR]	-- Not dependency analysed
					-- Recursive by default

  | ValBindsOut		   -- After renaming
	[(RecFlag, LHsBinds idL)]	-- Dependency analysed, later bindings 
                                        -- in the list may depend on earlier
                                        -- ones.
	[LSig Name]
  deriving (Data, Typeable)

type LHsBinds id = Bag (LHsBind id)
type LHsBind  id = Located (HsBind id)
type HsBind id   = HsBindLR id id

type LHsBindLR idL idR = Located (HsBindLR idL idR)
type LHsBindsLR idL idR = Bag (LHsBindLR idL idR)

data HsBindLR idL idR
  = -- | FunBind is used for both functions   @f x = e@
    -- and variables                          @f = \x -> e@
    --
    -- Reason 1: Special case for type inference: see 'TcBinds.tcMonoBinds'.
    --
    -- Reason 2: Instance decls can only have FunBinds, which is convenient.
    --           If you change this, you'll need to change e.g. rnMethodBinds
    --
    -- But note that the form                 @f :: a->a = ...@
    -- parses as a pattern binding, just like
    --                                        @(f :: a -> a) = ... @
    FunBind {

	fun_id :: Located idL,

	fun_infix :: Bool,	-- ^ True => infix declaration

	fun_matches :: MatchGroup idR,	-- ^ The payload

	fun_co_fn :: HsWrapper,	-- ^ Coercion from the type of the MatchGroup to the type of
				-- the Id.  Example:
                                -- @
				--	f :: Int -> forall a. a -> a
				--	f x y = y
                                -- @
				-- Then the MatchGroup will have type (Int -> a' -> a')
				-- (with a free type variable a').  The coercion will take
				-- a CoreExpr of this type and convert it to a CoreExpr of
				-- type 	Int -> forall a'. a' -> a'
				-- Notice that the coercion captures the free a'.

	bind_fvs :: NameSet,	-- ^ After the renamer, this contains a superset of the
				-- Names of the other binders in this binding group that 
				-- are free in the RHS of the defn
				-- Before renaming, and after typechecking, 
				-- the field is unused; it's just an error thunk

        fun_tick :: Maybe (Int,[Id])   -- ^ This is the (optional) module-local tick number.
    }

  | PatBind {	-- The pattern is never a simple variable;
		-- That case is done by FunBind
	pat_lhs    :: LPat idL,
	pat_rhs    :: GRHSs idR,
	pat_rhs_ty :: PostTcType,	-- Type of the GRHSs
	bind_fvs   :: NameSet		-- Same as for FunBind
    }

  | VarBind {	-- Dictionary binding and suchlike 
	var_id     :: idL,	     -- All VarBinds are introduced by the type checker
	var_rhs    :: LHsExpr idR,   -- Located only for consistency
	var_inline :: Bool           -- True <=> inline this binding regardless
				     --	(used for implication constraints only)
    }

  | AbsBinds {				-- Binds abstraction; TRANSLATION
        abs_tvs     :: [TyVar],  
	abs_ev_vars :: [EvVar],	 -- Includes equality constraints

       -- AbsBinds only gets used when idL = idR after renaming,
       -- but these need to be idL's for the collect... code in HsUtil to have
       -- the right type
	abs_exports :: [([TyVar], idL, idL, TcSpecPrags)],	-- (tvs, poly_id, mono_id, prags)

        abs_ev_binds :: TcEvBinds,     -- Evidence bindings
	abs_binds    :: LHsBinds idL   -- Typechecked user bindings
    }
  deriving (Data, Typeable)
	-- Consider (AbsBinds tvs ds [(ftvs, poly_f, mono_f) binds]
	-- 
	-- Creates bindings for (polymorphic, overloaded) poly_f
	-- in terms of monomorphic, non-overloaded mono_f
	--
	-- Invariants: 
	--	1. 'binds' binds mono_f
	--	2. ftvs is a subset of tvs
	--	3. ftvs includes all tyvars free in ds
	--
	-- See section 9 of static semantics paper for more details.
	-- (You can get a PhD for explaining the True Meaning
	--  of this last construct.)

placeHolderNames :: NameSet
-- Used for the NameSet in FunBind and PatBind prior to the renamer
placeHolderNames = panic "placeHolderNames"

------------
instance (OutputableBndr idL, OutputableBndr idR) => Outputable (HsLocalBindsLR idL idR) where
  ppr (HsValBinds bs) = ppr bs
  ppr (HsIPBinds bs)  = ppr bs
  ppr EmptyLocalBinds = empty

instance (OutputableBndr idL, OutputableBndr idR) => Outputable (HsValBindsLR idL idR) where
  ppr (ValBindsIn binds sigs)
   = pprValBindsForUser binds sigs

  ppr (ValBindsOut sccs sigs) 
    = getPprStyle $ \ sty ->
      if debugStyle sty then	-- Print with sccs showing
	vcat (map ppr sigs) $$ vcat (map ppr_scc sccs)
     else
	pprValBindsForUser (unionManyBags (map snd sccs)) sigs
   where
     ppr_scc (rec_flag, binds) = pp_rec rec_flag <+> pprLHsBinds binds
     pp_rec Recursive    = ptext (sLit "rec")
     pp_rec NonRecursive = ptext (sLit "nonrec")

--  *not* pprLHsBinds because we don't want braces; 'let' and
-- 'where' include a list of HsBindGroups and we don't want
-- several groups of bindings each with braces around.
-- Sort by location before printing
pprValBindsForUser :: (OutputableBndr idL, OutputableBndr idR, OutputableBndr id2)
		   => LHsBindsLR idL idR -> [LSig id2] -> SDoc
pprValBindsForUser binds sigs
  = pprDeeperList vcat (map snd (sort_by_loc decls))
  where

    decls :: [(SrcSpan, SDoc)]
    decls = [(loc, ppr sig)  | L loc sig <- sigs] ++
    	     [(loc, ppr bind) | L loc bind <- bagToList binds]

    sort_by_loc decls = sortLe (\(l1,_) (l2,_) -> l1 <= l2) decls

pprLHsBinds :: (OutputableBndr idL, OutputableBndr idR) => LHsBindsLR idL idR -> SDoc
pprLHsBinds binds 
  | isEmptyLHsBinds binds = empty
  | otherwise = lbrace <+> pprDeeperList vcat (map ppr (bagToList binds)) <+> rbrace

------------
emptyLocalBinds :: HsLocalBindsLR a b
emptyLocalBinds = EmptyLocalBinds

isEmptyLocalBinds :: HsLocalBindsLR a b -> Bool
isEmptyLocalBinds (HsValBinds ds) = isEmptyValBinds ds
isEmptyLocalBinds (HsIPBinds ds)  = isEmptyIPBinds ds
isEmptyLocalBinds EmptyLocalBinds = True

isEmptyValBinds :: HsValBindsLR a b -> Bool
isEmptyValBinds (ValBindsIn ds sigs)  = isEmptyLHsBinds ds && null sigs
isEmptyValBinds (ValBindsOut ds sigs) = null ds && null sigs

emptyValBindsIn, emptyValBindsOut :: HsValBindsLR a b
emptyValBindsIn  = ValBindsIn emptyBag []
emptyValBindsOut = ValBindsOut []      []

emptyLHsBinds :: LHsBindsLR idL idR
emptyLHsBinds = emptyBag

isEmptyLHsBinds :: LHsBindsLR idL idR -> Bool
isEmptyLHsBinds = isEmptyBag

------------
plusHsValBinds :: HsValBinds a -> HsValBinds a -> HsValBinds a
plusHsValBinds (ValBindsIn ds1 sigs1) (ValBindsIn ds2 sigs2)
  = ValBindsIn (ds1 `unionBags` ds2) (sigs1 ++ sigs2)
plusHsValBinds (ValBindsOut ds1 sigs1) (ValBindsOut ds2 sigs2)
  = ValBindsOut (ds1 ++ ds2) (sigs1 ++ sigs2)
\end{code}

What AbsBinds means
~~~~~~~~~~~~~~~~~~~
	 AbsBinds tvs
		  [d1,d2]
		  [(tvs1, f1p, f1m), 
		   (tvs2, f2p, f2m)]
		  BIND
means

	f1p = /\ tvs -> \ [d1,d2] -> letrec DBINDS and BIND 
				      in fm

	gp = ...same again, with gm instead of fm

This is a pretty bad translation, because it duplicates all the bindings.
So the desugarer tries to do a better job:

	fp = /\ [a,b] -> \ [d1,d2] -> case tp [a,b] [d1,d2] of
					(fm,gm) -> fm
	..ditto for gp..

	tp = /\ [a,b] -> \ [d1,d2] -> letrec DBINDS and BIND 
				       in (fm,gm)

\begin{code}
instance (OutputableBndr idL, OutputableBndr idR) => Outputable (HsBindLR idL idR) where
    ppr mbind = ppr_monobind mbind

ppr_monobind :: (OutputableBndr idL, OutputableBndr idR) => HsBindLR idL idR -> SDoc

ppr_monobind (PatBind { pat_lhs = pat, pat_rhs = grhss })
  = pprPatBind pat grhss
ppr_monobind (VarBind { var_id = var, var_rhs = rhs })    
  = sep [pprBndr CaseBind var, nest 2 $ equals <+> pprExpr (unLoc rhs)]
ppr_monobind (FunBind { fun_id = fun, fun_infix = inf,
	     	        fun_co_fn = wrap, 
			fun_matches = matches,
		        fun_tick = tick })
  = pprTicks empty (case tick of 
			Nothing -> empty
			Just t  -> text "-- tick id = " <> ppr t)
    $$  pprFunBind (unLoc fun) inf matches
    $$  ifPprDebug (ppr wrap)

ppr_monobind (AbsBinds { abs_tvs = tyvars, abs_ev_vars = dictvars 
		       , abs_exports = exports, abs_binds = val_binds
                       , abs_ev_binds = ev_binds })
  = sep [ptext (sLit "AbsBinds"),
  	 brackets (interpp'SP tyvars),
  	 brackets (interpp'SP dictvars),
  	 brackets (sep (punctuate comma (map ppr_exp exports)))]
    $$
    nest 2 ( vcat [pprBndr LetBind x | (_,x,_,_) <- exports]
  			-- Print type signatures
  	     $$ pprLHsBinds val_binds )
    $$
    ifPprDebug (ppr ev_binds)
  where
    ppr_exp (tvs, gbl, lcl, prags)
	= vcat [ppr gbl <+> ptext (sLit "<=") <+> ppr tvs <+> ppr lcl,
	  	nest 2 (pprTcSpecPrags gbl prags)]
\end{code}


\begin{code}
pprTicks :: SDoc -> SDoc -> SDoc
-- Print stuff about ticks only when -dppr-debug is on, to avoid
-- them appearing in error messages (from the desugarer); see Trac # 3263
pprTicks pp_no_debug pp_when_debug
  = getPprStyle (\ sty -> if debugStyle sty then pp_when_debug 
                                            else pp_no_debug)
\end{code}

%************************************************************************
%*									*
		Implicit parameter bindings
%*									*
%************************************************************************

\begin{code}
data HsIPBinds id
  = IPBinds 
	[LIPBind id] 
	TcEvBinds	-- Only in typechecker output; binds 
			-- uses of the implicit parameters
  deriving (Data, Typeable)

isEmptyIPBinds :: HsIPBinds id -> Bool
isEmptyIPBinds (IPBinds is ds) = null is && isEmptyTcEvBinds ds

type LIPBind id = Located (IPBind id)

-- | Implicit parameter bindings.
data IPBind id
  = IPBind
	(IPName id)
	(LHsExpr id)
  deriving (Data, Typeable)

instance (OutputableBndr id) => Outputable (HsIPBinds id) where
  ppr (IPBinds bs ds) = pprDeeperList vcat (map ppr bs) 
			$$ ppr ds

instance (OutputableBndr id) => Outputable (IPBind id) where
  ppr (IPBind id rhs) = pprBndr LetBind id <+> equals <+> pprExpr (unLoc rhs)
\end{code}


%************************************************************************
%*									*
\subsection{Coercion functions}
%*									*
%************************************************************************

\begin{code}
-- A HsWrapper is an expression with a hole in it
-- We need coercions to have concrete form so that we can zonk them

data HsWrapper
  = WpHole			-- The identity coercion

  | WpCompose HsWrapper HsWrapper	
       -- (wrap1 `WpCompse` wrap2)[e] = wrap1[ wrap2[ e ]]
       -- 
       -- Hence  (\a. []) `WpCompose` (\b. []) = (\a b. [])
       -- But    ([] a)   `WpCompose` ([] b)   = ([] b a)

  | WpCast Coercion		-- A cast:  [] `cast` co
				-- Guaranteed not the identity coercion

	-- Evidence abstraction and application
        -- (both dictionaries and coercions)
  | WpEvLam EvVar 		-- \d. []	the 'd' is an evidence variable
  | WpEvApp EvTerm		-- [] d		the 'd' is evidence for a constraint

	-- Type abstraction and application
  | WpTyLam TyVar 		-- \a. []	the 'a' is a type variable (not coercion var)
  | WpTyApp Type		-- [] t		the 't' is a type (not coercion)


  | WpLet TcEvBinds      	-- Non-empty (or possibly non-empty) evidence bindings,
                                -- so that the identity coercion is always exactly WpHole
  deriving (Data, Typeable)


data TcEvBinds 
  = TcEvBinds		-- Mutable evidence bindings
       EvBindsVar	-- Mutable because they are updated "later"
     	       	       	--    when an implication constraint is solved

  | EvBinds 		-- Immutable after zonking
       (Bag EvBind)

  deriving( Typeable )

data EvBindsVar = EvBindsVar (IORef EvBindMap) Unique
     -- The Unique is only for debug printing

-----------------
type EvBindMap = VarEnv EvBind

emptyEvBindMap :: EvBindMap
emptyEvBindMap = emptyVarEnv

extendEvBinds :: EvBindMap -> EvVar -> EvTerm -> EvBindMap
extendEvBinds bs v t = extendVarEnv bs v (EvBind v t)

lookupEvBind :: EvBindMap -> EvVar -> Maybe EvBind
lookupEvBind = lookupVarEnv

evBindMapBinds :: EvBindMap -> Bag EvBind
evBindMapBinds = foldVarEnv consBag emptyBag

-----------------
instance Data TcEvBinds where
  -- Placeholder; we can't travers into TcEvBinds
  toConstr _   = abstractConstr "TcEvBinds"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "TcEvBinds"

-- All evidence is bound by EvBinds; no side effects
data EvBind = EvBind EvVar EvTerm

data EvTerm
  = EvId EvId                  -- Term-level variable-to-variable bindings 
                               -- (no coercion variables! they come via EvCoercion)

  | EvCoercion Coercion        -- Coercion bindings

  | EvCast EvVar Coercion      -- d |> co

  | EvDFunApp DFunId           -- Dictionary instance application
       [Type] [EvVar]  

  | EvSuperClass DictId Int    -- n'th superclass. Used for both equalities and
                               -- dictionaries, even though the former have no
			       -- selector Id.  We count up from _0_ 
			       
  deriving( Data, Typeable)

evVarTerm :: EvVar -> EvTerm
evVarTerm v | isCoVar v = EvCoercion (mkCoVarCoercion v)
            | otherwise = EvId v
\end{code}

Note [EvBinds/EvTerm]
~~~~~~~~~~~~~~~~~~~~~
How evidence is created and updated. Bindings for dictionaries, 
and coercions and implicit parameters are carried around in TcEvBinds
which during constraint generation and simplification is always of the
form (TcEvBinds ref). After constraint simplification is finished it 
will be transformed to t an (EvBinds ev_bag). 

Evidence for coercions *SHOULD* be filled in using the TcEvBinds 
However, all EvVars that correspond to *wanted* coercion terms in 
an EvBind must be mutable variables so that they can be readily 
inlined (by zonking) after constraint simplification is finished.

Conclusion: a new wanted coercion variable should be made mutable. 
[Notice though that evidence variables that bind coercion terms 
 from super classes will be "given" and hence rigid] 


\begin{code}
emptyTcEvBinds :: TcEvBinds
emptyTcEvBinds = EvBinds emptyBag

isEmptyTcEvBinds :: TcEvBinds -> Bool
isEmptyTcEvBinds (EvBinds b)    = isEmptyBag b
isEmptyTcEvBinds (TcEvBinds {}) = panic "isEmptyTcEvBinds"
 
(<.>) :: HsWrapper -> HsWrapper -> HsWrapper
WpHole <.> c = c
c <.> WpHole = c
c1 <.> c2    = c1 `WpCompose` c2

mkWpTyApps :: [Type] -> HsWrapper
mkWpTyApps tys = mk_co_app_fn WpTyApp tys

mkWpEvApps :: [EvTerm] -> HsWrapper
mkWpEvApps args = mk_co_app_fn WpEvApp args

mkWpEvVarApps :: [EvVar] -> HsWrapper
mkWpEvVarApps vs = mkWpEvApps (map evVarTerm vs)

mkWpTyLams :: [TyVar] -> HsWrapper
mkWpTyLams ids = mk_co_lam_fn WpTyLam ids

mkWpLams :: [Var] -> HsWrapper
mkWpLams ids = mk_co_lam_fn WpEvLam ids

mkWpLet :: TcEvBinds -> HsWrapper
-- This no-op is a quite a common case
mkWpLet (EvBinds b) | isEmptyBag b = WpHole
mkWpLet ev_binds                   = WpLet ev_binds

mk_co_lam_fn :: (a -> HsWrapper) -> [a] -> HsWrapper
mk_co_lam_fn f as = foldr (\x wrap -> f x `WpCompose` wrap) WpHole as

mk_co_app_fn :: (a -> HsWrapper) -> [a] -> HsWrapper
-- For applications, the *first* argument must
-- come *last* in the composition sequence
mk_co_app_fn f as = foldr (\x wrap -> wrap `WpCompose` f x) WpHole as

idHsWrapper :: HsWrapper
idHsWrapper = WpHole

isIdHsWrapper :: HsWrapper -> Bool
isIdHsWrapper WpHole = True
isIdHsWrapper _      = False
\end{code}

Pretty printing

\begin{code}
instance Outputable HsWrapper where 
  ppr co_fn = pprHsWrapper (ptext (sLit "<>")) co_fn

pprHsWrapper :: SDoc -> HsWrapper -> SDoc
-- In debug mode, print the wrapper
-- otherwise just print what's inside
pprHsWrapper doc wrap
  = getPprStyle (\ s -> if debugStyle s then (help (add_parens doc) wrap False) else doc)
  where
    help :: (Bool -> SDoc) -> HsWrapper -> Bool -> SDoc
    -- True  <=> appears in function application position
    -- False <=> appears as body of let or lambda
    help it WpHole             = it
    help it (WpCompose f1 f2)  = help (help it f2) f1
    help it (WpCast co)   = add_parens $ sep [it False, nest 2 (ptext (sLit "|>") 
                                                 <+> pprParendType co)]
    help it (WpEvApp id)  = no_parens  $ sep [it True, nest 2 (ppr id)]
    help it (WpTyApp ty)  = no_parens  $ sep [it True, ptext (sLit "@") <+> pprParendType ty]
    help it (WpEvLam id)  = add_parens $ sep [ ptext (sLit "\\") <> pp_bndr id, it False]
    help it (WpTyLam tv)  = add_parens $ sep [ptext (sLit "/\\") <> pp_bndr tv, it False]
    help it (WpLet binds) = add_parens $ sep [ptext (sLit "let") <+> braces (ppr binds), it False]

    pp_bndr v = pprBndr LambdaBind v <> dot

    add_parens, no_parens :: SDoc -> Bool -> SDoc
    add_parens d True  = parens d
    add_parens d False = d
    no_parens d _ = d

instance Outputable TcEvBinds where
  ppr (TcEvBinds v) = ppr v
  ppr (EvBinds bs)  = ptext (sLit "EvBinds") <> braces (ppr bs)

instance Outputable EvBindsVar where
  ppr (EvBindsVar _ u) = ptext (sLit "EvBindsVar") <> angleBrackets (ppr u)

instance Outputable EvBind where
  ppr (EvBind v e)   = ppr v <+> equals <+> ppr e

instance Outputable EvTerm where
  ppr (EvId v)        	 = ppr v
  ppr (EvCast v co)   	 = ppr v <+> (ptext (sLit "`cast`")) <+> pprParendType co
  ppr (EvCoercion co)    = ppr co
  ppr (EvSuperClass d n) = ptext (sLit "sc") <> parens (ppr (d,n))
  ppr (EvDFunApp df tys ts) = ppr df <+> sep [ char '@' <> ppr tys
                                             , ppr ts ]
\end{code}

%************************************************************************
%*									*
\subsection{@Sig@: type signatures and value-modifying user pragmas}
%*									*
%************************************************************************

It is convenient to lump ``value-modifying'' user-pragmas (e.g.,
``specialise this function to these four types...'') in with type
signatures.  Then all the machinery to move them into place, etc.,
serves for both.

\begin{code}
type LSig name = Located (Sig name)

data Sig name	-- Signatures and pragmas
  = 	-- An ordinary type signature
	-- f :: Num a => a -> a
    TypeSig (Located name) (LHsType name)

	-- A type signature in generated code, notably the code
	-- generated for record selectors.  We simply record
	-- the desired Id itself, replete with its name, type
	-- and IdDetails.  Otherwise it's just like a type 
	-- signature: there should be an accompanying binding
  | IdSig Id

	-- An ordinary fixity declaration
	--	infixl *** 8
  | FixSig (FixitySig name)

	-- An inline pragma
	-- {#- INLINE f #-}
  | InlineSig	(Located name)	-- Function name
		InlinePragma	-- Never defaultInlinePragma

	-- A specialisation pragma
	-- {-# SPECIALISE f :: Int -> Int #-}
  | SpecSig 	(Located name)	-- Specialise a function or datatype ...
		(LHsType name)	-- ... to these types
		InlinePragma    -- The pragma on SPECIALISE_INLINE form
				-- If it's just defaultInlinePragma, then we said
				--    SPECIALISE, not SPECIALISE_INLINE

	-- A specialisation pragma for instance declarations only
	-- {-# SPECIALISE instance Eq [Int] #-}
  | SpecInstSig (LHsType name)	-- (Class tys); should be a specialisation of the 
				-- current instance decl
  deriving (Data, Typeable)


type LFixitySig name = Located (FixitySig name)
data FixitySig name = FixitySig (Located name) Fixity 
  deriving (Data, Typeable)

-- TsSpecPrags conveys pragmas from the type checker to the desugarer
data TcSpecPrags 
  = IsDefaultMethod	-- Super-specialised: a default method should 
    			-- be macro-expanded at every call site
  | SpecPrags [Located TcSpecPrag]
  deriving (Data, Typeable)

data TcSpecPrag 
  = SpecPrag   
	HsWrapper	-- An wrapper, that specialises the polymorphic function
	InlinePragma 	-- Inlining spec for the specialised function
  deriving (Data, Typeable)

noSpecPrags :: TcSpecPrags
noSpecPrags = SpecPrags []

hasSpecPrags :: TcSpecPrags -> Bool
hasSpecPrags (SpecPrags ps) = not (null ps)
hasSpecPrags IsDefaultMethod = False

isDefaultMethod :: TcSpecPrags -> Bool
isDefaultMethod IsDefaultMethod = True
isDefaultMethod (SpecPrags {})  = False

\end{code}

\begin{code}
okBindSig :: Sig a -> Bool
okBindSig _ = True

okHsBootSig :: Sig a -> Bool
okHsBootSig (TypeSig  _ _) = True
okHsBootSig (FixSig _) 	   = True
okHsBootSig _              = False

okClsDclSig :: Sig a -> Bool
okClsDclSig (SpecInstSig _) = False
okClsDclSig _               = True        -- All others OK

okInstDclSig :: Sig a -> Bool
okInstDclSig (TypeSig _ _)   = False
okInstDclSig (FixSig _)      = False
okInstDclSig _ 	             = True

sigForThisGroup :: NameSet -> LSig Name -> Bool
sigForThisGroup ns sig
  = case sigName sig of
	Nothing -> False
	Just n  -> n `elemNameSet` ns

sigName :: LSig name -> Maybe name
sigName (L _ sig) = sigNameNoLoc sig

sigNameNoLoc :: Sig name -> Maybe name    
sigNameNoLoc (TypeSig   n _)          = Just (unLoc n)
sigNameNoLoc (SpecSig   n _ _)        = Just (unLoc n)
sigNameNoLoc (InlineSig n _)          = Just (unLoc n)
sigNameNoLoc (FixSig (FixitySig n _)) = Just (unLoc n)
sigNameNoLoc _                        = Nothing

isFixityLSig :: LSig name -> Bool
isFixityLSig (L _ (FixSig {})) = True
isFixityLSig _	               = False

isVanillaLSig :: LSig name -> Bool	 -- User type signatures
-- A badly-named function, but it's part of the GHCi (used
-- by Haddock) so I don't want to change it gratuitously.
isVanillaLSig (L _(TypeSig {})) = True
isVanillaLSig _                 = False

isTypeLSig :: LSig name -> Bool	 -- Type signatures
isTypeLSig (L _(TypeSig {})) = True
isTypeLSig (L _(IdSig {}))   = True
isTypeLSig _                 = False

isSpecLSig :: LSig name -> Bool
isSpecLSig (L _(SpecSig {})) = True
isSpecLSig _                 = False

isSpecInstLSig :: LSig name -> Bool
isSpecInstLSig (L _ (SpecInstSig {})) = True
isSpecInstLSig _                      = False

isPragLSig :: LSig name -> Bool
	-- Identifies pragmas 
isPragLSig (L _ (SpecSig {}))   = True
isPragLSig (L _ (InlineSig {})) = True
isPragLSig _                    = False

isInlineLSig :: LSig name -> Bool
	-- Identifies inline pragmas 
isInlineLSig (L _ (InlineSig {})) = True
isInlineLSig _                    = False

hsSigDoc :: Sig name -> SDoc
hsSigDoc (TypeSig {}) 		= ptext (sLit "type signature")
hsSigDoc (IdSig {}) 		= ptext (sLit "id signature")
hsSigDoc (SpecSig {})	 	= ptext (sLit "SPECIALISE pragma")
hsSigDoc (InlineSig {})         = ptext (sLit "INLINE pragma")
hsSigDoc (SpecInstSig {})	= ptext (sLit "SPECIALISE instance pragma")
hsSigDoc (FixSig {}) 		= ptext (sLit "fixity declaration")
\end{code}

Signature equality is used when checking for duplicate signatures

\begin{code}
eqHsSig :: Eq a => LSig a -> LSig a -> Bool
eqHsSig (L _ (FixSig (FixitySig n1 _))) (L _ (FixSig (FixitySig n2 _))) = unLoc n1 == unLoc n2
eqHsSig (L _ (IdSig n1))         	(L _ (IdSig n2))                = n1 == n2
eqHsSig (L _ (TypeSig n1 _))         	(L _ (TypeSig n2 _))            = unLoc n1 == unLoc n2
eqHsSig (L _ (InlineSig n1 _))          (L _ (InlineSig n2 _))          = unLoc n1 == unLoc n2
 	-- For specialisations, we don't have equality over
	-- HsType, so it's not convenient to spot duplicate 
	-- specialisations here.  Check for this later, when we're in Type land
eqHsSig _other1 _other2 = False
\end{code}

\begin{code}
instance (OutputableBndr name) => Outputable (Sig name) where
    ppr sig = ppr_sig sig

ppr_sig :: OutputableBndr name => Sig name -> SDoc
ppr_sig (TypeSig var ty)	  = pprVarSig (unLoc var) (ppr ty)
ppr_sig (IdSig id)	          = pprVarSig id (ppr (varType id))
ppr_sig (FixSig fix_sig) 	  = ppr fix_sig
ppr_sig (SpecSig var ty inl) 	  = pragBrackets (pprSpec var (ppr ty) inl)
ppr_sig (InlineSig var inl)       = pragBrackets (ppr inl <+> ppr var)
ppr_sig (SpecInstSig ty) 	  = pragBrackets (ptext (sLit "SPECIALIZE instance") <+> ppr ty)

instance Outputable name => Outputable (FixitySig name) where
  ppr (FixitySig name fixity) = sep [ppr fixity, ppr name]

pragBrackets :: SDoc -> SDoc
pragBrackets doc = ptext (sLit "{-#") <+> doc <+> ptext (sLit "#-}") 

pprVarSig :: (Outputable id) => id -> SDoc -> SDoc
pprVarSig var pp_ty = sep [ppr var <+> dcolon, nest 2 pp_ty]

pprSpec :: (Outputable id) => id -> SDoc -> InlinePragma -> SDoc
pprSpec var pp_ty inl = ptext (sLit "SPECIALIZE") <+> pp_inl <+> pprVarSig var pp_ty
  where
    pp_inl | isDefaultInlinePragma inl = empty
           | otherwise = ppr inl

pprTcSpecPrags :: Outputable id => id -> TcSpecPrags -> SDoc
pprTcSpecPrags _   IsDefaultMethod = ptext (sLit "<default method>")
pprTcSpecPrags gbl (SpecPrags ps)  = vcat (map (pprSpecPrag gbl) ps)

pprSpecPrag :: Outputable id => id -> Located TcSpecPrag -> SDoc
pprSpecPrag var (L _ (SpecPrag _expr inl)) = pprSpec var (ptext (sLit "<type>")) inl

instance Outputable TcSpecPrag where
  ppr (SpecPrag _ p) = ptext (sLit "SpecPrag") <+> ppr p
\end{code}

