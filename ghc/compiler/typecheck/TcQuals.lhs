%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[TcQuals]{TcQuals}

\begin{code}
#include "HsVersions.h"

module TcQuals ( tcQuals ) where

import TcMonad		-- typechecking monad machinery
import AbsSyn		-- the stuff being typechecked

import AbsPrel		( boolTy, mkListTy )
import E		( E, TCE(..), UniqFM, CE(..) )
			-- TCE and CE for pragmas only
import Errors		( UnifyErrContext(..) )
import LIE		( LIE, plusLIE )
import TcExpr		( tcExpr )
import TcPat		( tcPat )
import Unify		( unifyTauTy )
import Util
\end{code}

There will be at least one @Qual@.

\begin{code}
tcQuals :: E -> [RenamedQual] -> TcM ([TypecheckedQual], LIE)

tcQuals e [qual]
  = tcQual e qual   `thenTc` \ (new_qual, lie) ->
    returnTc ([new_qual], lie)

tcQuals e (qual:quals)
  = tcQual  e qual  `thenTc` \ (new_qual,  lie1) ->
    tcQuals e quals `thenTc` \ (new_quals, lie2) ->
    returnTc (new_qual : new_quals, lie1 `plusLIE` lie2)

---

tcQual e (FilterQual expr)
  = tcExpr e expr			   `thenTc` \ (expr', lie, ty) ->
    unifyTauTy ty boolTy (FilterCtxt expr) `thenTc_`
    returnTc (FilterQual expr', lie)

tcQual e (GeneratorQual pat expr)
  = tcPat e pat			`thenTc` \ (pat',  lie_pat,  pat_ty)  ->
    tcExpr e expr		`thenTc` \ (expr', lie_expr, expr_ty) ->

    unifyTauTy expr_ty (mkListTy pat_ty) (GeneratorCtxt pat expr) `thenTc_`

    returnTc (GeneratorQual pat' expr', lie_pat `plusLIE` lie_expr)
\end{code}


