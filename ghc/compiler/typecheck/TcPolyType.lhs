%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[TcPolyType]{Typechecking user-specified @PolyTypes@}

\begin{code}
module TcPolyType ( tcPolyType ) where

#include "HsVersions.h"

import TcMonad		-- typechecking monad machinery
import AbsSyn		-- the stuff being typechecked

import AbsUniType	( mkTyVarTemplateTy, mkSysTyVarTemplate, mkSigmaTy,
			  mkForallTy, SigmaType(..)
			)
import CE		( CE(..) )
import Maybes		( Maybe(..) )
import TCE		( TCE(..), UniqFM )
import TVE		-- ( mkTVE, plusTVE, unitTVE, lookupTVE_NoFail, TVE(..) )
import TcContext	( tcContext )
import TcMonoType	( tcMonoType )
import Util
\end{code}

The TVE passed into @tcPolyType@ binds type variables which are
in scope; in practice this is always either empty (ordinary type sigs)
or a singleton (class signatures).  @tcPolyType@ generates a type which
is polymorphic in all the {\em other} type varaibles mentioned in the
type.

Very Important Note: when we have a type signature in an interface, say
\begin{verbatim}
	f :: a -> b -> a
\end{verbatim}
which of the following polytypes do we return?
\begin{verbatim}
	forall a b. a -> b -> a
--or
	forall b a. a -> b -> a
\end{verbatim}

It makes a difference, because it affects the order in which f takes
its type arguments.  Now this makes a difference in two ways:
\begin{itemize}
\item
It's essential to get it right if an inlining for f is also exported
by the interface.
\item
It's essential to get it right if the interface tells that there's a specialised
version of f, because specialisations are known by their function-name/type-arg 
combinations.
\end{itemize}

By convention, the foralls on a type read in from somewhere (notably interfaces)
are 
	{\em in alphabetical order of their type variables}

When printing types we make sure that we assign print-names to the forall'd type
variables which are also in alphabetical order.

\begin{code}
tcPolyType :: CE -> TCE -> TVE  -> RenamedPolyType -> Baby_TcM UniType

tcPolyType ce tce tve (ForAllTy tvs ty)
  = let
	new_tv_tmpls_w_uniqs = map tc_uf_tyvar_template tvs
	new_tv_tmpls	     = map snd new_tv_tmpls_w_uniqs
	new_tve
	  = foldr plusTVE tve
	    [ unitTVE u (mkTyVarTemplateTy tv)
	    | (u, tv) <- new_tv_tmpls_w_uniqs ]
    in
    tcMonoType ce tce new_tve ty	`thenB_Tc` \ new_ty ->
    returnB_Tc (mkForallTy new_tv_tmpls new_ty)
  where
    tc_uf_tyvar_template (Short u _) = (u, mkSysTyVarTemplate u SLIT("a"))

tcPolyType ce tce tve (OverloadedTy   ctxt ty) = tc_poly ce tce tve ctxt ty
tcPolyType ce tce tve (UnoverloadedTy ty)      = tc_poly ce tce tve []   ty

tc_poly ce tce tve ctxt ty
  = let	-- BUILD THE NEW TVE
	used_tyvar_names	= extractMonoTyNames (==) ty
	poly_tyvar_names	= drop_tyvars_if_in_TVE used_tyvar_names

	-- Sort them into alphabetical order; see notes above.
	sorted_tyvar_names	= sortLt lt_by_string poly_tyvar_names

	(local_tve, tyvars, _)	= mkTVE sorted_tyvar_names
	new_tve			= plusTVE tve local_tve
    in
	 -- TYPE CHECK THE CONTEXT AND MONOTYPE
    tcContext ce tce new_tve ctxt	`thenB_Tc` \ theta ->
    tcMonoType ce tce new_tve ty	`thenB_Tc` \ tau_ty ->

	 -- BUILD THE POLYTYPE AND RETURN
    returnB_Tc (mkSigmaTy tyvars theta tau_ty)
 where
    drop_tyvars_if_in_TVE [] = []
    drop_tyvars_if_in_TVE (n:ns)
      = let rest = drop_tyvars_if_in_TVE ns
	in
      	case (lookupTVE_NoFail tve n) of
	  Just _    -> rest	-- drop it
	  Nothing   -> n : rest

    lt_by_string :: Name -> Name -> Bool
    lt_by_string a b = getOccurrenceName a < getOccurrenceName b
\end{code}
