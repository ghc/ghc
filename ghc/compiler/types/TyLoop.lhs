
\begin{code}
module AllTypes(
	TyCon, Arity(..),
	Class, ClassOp,
	GenTyVar, GenType, Type,
	Id,

	-- Functions which are, alas, necessary to break loops
	mkTupleCon,	-- Used in TyCon


	Kind,		-- Not necessary to break loops, but useful
	GenUsage	-- to get when importing AllTypes
) where

import TyCon	( TyCon, Arity(..) )
import Type	( GenTyVar, TyVar(..), GenType, Type(..) )
import Class	( Class,ClassOp )
import Id	( Id, mkTupleCon )
import Kind	( Kind )
import Usage	( GenUsage, Usage(..) )
\end{code}
