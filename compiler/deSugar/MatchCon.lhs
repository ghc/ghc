%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Pattern-matching constructors

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module MatchCon ( matchConFamily, matchPatSyn ) where

#include "HsVersions.h"

import {-# SOURCE #-} Match	( match )

import HsSyn
import DsBinds
import ConLike
import DataCon
import PatSyn
import TcType
import DsMonad
import DsUtils
import MkCore   ( mkCoreLets )
import Util
import ListSetOps ( runs )
import Id
import NameEnv
import SrcLoc
import DynFlags
import Outputable
import Control.Monad(liftM)
\end{code}

We are confronted with the first column of patterns in a set of
equations, all beginning with constructors from one ``family'' (e.g.,
@[]@ and @:@ make up the @List@ ``family'').  We want to generate the
alternatives for a @Case@ expression.  There are several choices:
\begin{enumerate}
\item
Generate an alternative for every constructor in the family, whether
they are used in this set of equations or not; this is what the Wadler
chapter does.
\begin{description}
\item[Advantages:]
(a)~Simple.  (b)~It may also be that large sparsely-used constructor
families are mainly handled by the code for literals.
\item[Disadvantages:]
(a)~Not practical for large sparsely-used constructor families, e.g.,
the ASCII character set.  (b)~Have to look up a list of what
constructors make up the whole family.
\end{description}

\item
Generate an alternative for each constructor used, then add a default
alternative in case some constructors in the family weren't used.
\begin{description}
\item[Advantages:]
(a)~Alternatives aren't generated for unused constructors.  (b)~The
STG is quite happy with defaults.  (c)~No lookup in an environment needed.
\item[Disadvantages:]
(a)~A spurious default alternative may be generated.
\end{description}

\item
``Do it right:'' generate an alternative for each constructor used,
and add a default alternative if all constructors in the family
weren't used.
\begin{description}
\item[Advantages:]
(a)~You will get cases with only one alternative (and no default),
which should be amenable to optimisation.  Tuples are a common example.
\item[Disadvantages:]
(b)~Have to look up constructor families in TDE (as above).
\end{description}
\end{enumerate}

We are implementing the ``do-it-right'' option for now.  The arguments
to @matchConFamily@ are the same as to @match@; the extra @Int@
returned is the number of constructors in the family.

The function @matchConFamily@ is concerned with this
have-we-used-all-the-constructors? question; the local function
@match_cons_used@ does all the real work.
\begin{code}
matchConFamily :: [Id]
               -> Type
	       -> [[EquationInfo]]
	       -> DsM MatchResult
-- Each group of eqns is for a single constructor
matchConFamily (var:vars) ty groups
  = do dflags <- getDynFlags
       alts <- mapM (fmap toRealAlt . matchOneConLike vars ty) groups
       return (mkCoAlgCaseMatchResult dflags var ty alts)
  where
    toRealAlt alt = case alt_pat alt of
        RealDataCon dcon -> alt{ alt_pat = dcon }
        _ -> panic "matchConFamily: not RealDataCon"
matchConFamily [] _ _ = panic "matchConFamily []"

matchPatSyn :: [Id]
            -> Type
            -> [EquationInfo]
            -> DsM MatchResult
matchPatSyn (var:vars) ty eqns
  = do alt <- fmap toSynAlt $ matchOneConLike vars ty eqns
       return (mkCoSynCaseMatchResult var ty alt)
  where
    toSynAlt alt = case alt_pat alt of
        PatSynCon psyn -> alt{ alt_pat = psyn }
        _ -> panic "matchPatSyn: not PatSynCon"
matchPatSyn _ _ _ = panic "matchPatSyn []"

type ConArgPats = HsConDetails (LPat Id) (HsRecFields Id (LPat Id))

matchOneConLike :: [Id]
                -> Type
                -> [EquationInfo]
                -> DsM (CaseAlt ConLike)
matchOneConLike vars ty (eqn1 : eqns)	-- All eqns for a single constructor
  = do	{ arg_vars <- selectConMatchVars val_arg_tys args1
	 	-- Use the first equation as a source of 
		-- suggestions for the new variables

	-- Divide into sub-groups; see Note [Record patterns]
        ; let groups :: [[(ConArgPats, EquationInfo)]]
	      groups = runs compatible_pats [ (pat_args (firstPat eqn), eqn) 
	      	       	    	            | eqn <- eqn1:eqns ]

	; match_results <- mapM (match_group arg_vars) groups

        ; return $ MkCaseAlt{ alt_pat = con1,
                              alt_bndrs = tvs1 ++ dicts1 ++ arg_vars,
                              alt_wrapper = wrapper1,
                              alt_result = foldr1 combineMatchResults match_results } }
  where
    ConPatOut { pat_con = L _ con1, pat_arg_tys = arg_tys, pat_wrap = wrapper1,
	        pat_tvs = tvs1, pat_dicts = dicts1, pat_args = args1 }
	      = firstPat eqn1
    fields1 = case con1 of
        	RealDataCon dcon1 -> dataConFieldLabels dcon1
        	PatSynCon{}       -> []

    val_arg_tys = case con1 of
                    RealDataCon dcon1 -> dataConInstOrigArgTys dcon1 inst_tys
                    PatSynCon psyn1   -> patSynInstArgTys      psyn1 inst_tys
    inst_tys = ASSERT( tvs1 `equalLength` ex_tvs )
               arg_tys ++ mkTyVarTys tvs1
	-- dataConInstOrigArgTys takes the univ and existential tyvars
	-- and returns the types of the *value* args, which is what we want

    ex_tvs = case con1 of
               RealDataCon dcon1 -> dataConExTyVars dcon1
               PatSynCon psyn1   -> patSynExTyVars psyn1

    match_group :: [Id] -> [(ConArgPats, EquationInfo)] -> DsM MatchResult
    -- All members of the group have compatible ConArgPats
    match_group arg_vars arg_eqn_prs
      = ASSERT( notNull arg_eqn_prs )
        do { (wraps, eqns') <- liftM unzip (mapM shift arg_eqn_prs)
    	   ; let group_arg_vars = select_arg_vars arg_vars arg_eqn_prs
    	   ; match_result <- match (group_arg_vars ++ vars) ty eqns'
    	   ; return (adjustMatchResult (foldr1 (.) wraps) match_result) }

    shift (_, eqn@(EqnInfo { eqn_pats = ConPatOut{ pat_tvs = tvs, pat_dicts = ds, 
					           pat_binds = bind, pat_args = args
					} : pats }))
      = do ds_bind <- dsTcEvBinds bind
           return ( wrapBinds (tvs `zip` tvs1)
                  . wrapBinds (ds  `zip` dicts1)
                  . mkCoreLets ds_bind
                  , eqn { eqn_pats = conArgPats val_arg_tys args ++ pats }
                  )
    shift (_, (EqnInfo { eqn_pats = ps })) = pprPanic "matchOneCon/shift" (ppr ps)

    -- Choose the right arg_vars in the right order for this group
    -- Note [Record patterns]
    select_arg_vars arg_vars ((arg_pats, _) : _)
      | RecCon flds <- arg_pats
      , let rpats = rec_flds flds  
      , not (null rpats)     -- Treated specially; cf conArgPats
      = ASSERT2( length fields1 == length arg_vars, 
                 ppr con1 $$ ppr fields1 $$ ppr arg_vars )
        map lookup_fld rpats
      | otherwise
      = arg_vars
      where
        fld_var_env = mkNameEnv $ zipEqual "get_arg_vars" fields1 arg_vars
	lookup_fld rpat = lookupNameEnv_NF fld_var_env 
		   	  		   (idName (unLoc (hsRecFieldId rpat)))
    select_arg_vars _ [] = panic "matchOneCon/select_arg_vars []"
matchOneConLike _ _ [] = panic "matchOneCon []"

-----------------
compatible_pats :: (ConArgPats,a) -> (ConArgPats,a) -> Bool
-- Two constructors have compatible argument patterns if the number
-- and order of sub-matches is the same in both cases
compatible_pats (RecCon flds1, _) (RecCon flds2, _) = same_fields flds1 flds2
compatible_pats (RecCon flds1, _) _                 = null (rec_flds flds1)
compatible_pats _                 (RecCon flds2, _) = null (rec_flds flds2)
compatible_pats _                 _                 = True -- Prefix or infix con

same_fields :: HsRecFields Id (LPat Id) -> HsRecFields Id (LPat Id) -> Bool
same_fields flds1 flds2 
  = all2 (\f1 f2 -> unLoc (hsRecFieldId f1) == unLoc (hsRecFieldId f2))
	 (rec_flds flds1) (rec_flds flds2)


-----------------
selectConMatchVars :: [Type] -> ConArgPats -> DsM [Id]
selectConMatchVars arg_tys (RecCon {})      = newSysLocalsDs arg_tys
selectConMatchVars _       (PrefixCon ps)   = selectMatchVars (map unLoc ps)
selectConMatchVars _       (InfixCon p1 p2) = selectMatchVars [unLoc p1, unLoc p2]

conArgPats :: [Type]	-- Instantiated argument types 
			-- Used only to fill in the types of WildPats, which
			-- are probably never looked at anyway
	   -> ConArgPats
	   -> [Pat Id]
conArgPats _arg_tys (PrefixCon ps)   = map unLoc ps
conArgPats _arg_tys (InfixCon p1 p2) = [unLoc p1, unLoc p2]
conArgPats  arg_tys (RecCon (HsRecFields { rec_flds = rpats }))
  | null rpats = map WildPat arg_tys
	-- Important special case for C {}, which can be used for a 
 	-- datacon that isn't declared to have fields at all
  | otherwise  = map (unLoc . hsRecFieldArg) rpats
\end{code}

Note [Record patterns]
~~~~~~~~~~~~~~~~~~~~~~
Consider 
	 data T = T { x,y,z :: Bool }

	 f (T { y=True, x=False }) = ...

We must match the patterns IN THE ORDER GIVEN, thus for the first
one we match y=True before x=False.  See Trac #246; or imagine 
matching against (T { y=False, x=undefined }): should fail without
touching the undefined. 

Now consider:

	 f (T { y=True, x=False }) = ...
	 f (T { x=True, y= False}) = ...

In the first we must test y first; in the second we must test x 
first.  So we must divide even the equations for a single constructor
T into sub-goups, based on whether they match the same field in the
same order.  That's what the (runs compatible_pats) grouping.

All non-record patterns are "compatible" in this sense, because the
positional patterns (T a b) and (a `T` b) all match the arguments
in order.  Also T {} is special because it's equivalent to (T _ _).
Hence the (null rpats) checks here and there.


Note [Existentials in shift_con_pat]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
	data T = forall a. Ord a => T a (a->Int)

	f (T x f) True  = ...expr1...
	f (T y g) False = ...expr2..

When we put in the tyvars etc we get

	f (T a (d::Ord a) (x::a) (f::a->Int)) True =  ...expr1...
	f (T b (e::Ord b) (y::a) (g::a->Int)) True =  ...expr2...

After desugaring etc we'll get a single case:

	f = \t::T b::Bool -> 
	    case t of
	       T a (d::Ord a) (x::a) (f::a->Int)) ->
	    case b of
		True  -> ...expr1...
		False -> ...expr2...

*** We have to substitute [a/b, d/e] in expr2! **
Hence
		False -> ....((/\b\(e:Ord b).expr2) a d)....

Originally I tried to use 
	(\b -> let e = d in expr2) a 
to do this substitution.  While this is "correct" in a way, it fails
Lint, because e::Ord b but d::Ord a.  

