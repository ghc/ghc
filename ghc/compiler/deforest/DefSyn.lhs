%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[DefSyn]{A temporary datatype for the deforestation pass}

> module DefSyn where

> import CoreSyn
> import Outputable
> import Util

This is exactly the same as core, except that the argument to
application can be an arbitrary expression.

> type DefProgram 		= [CoreBinding 	Id DefBindee]
> type DefBinding 		= CoreBinding  	Id DefBindee
> type DefExpr    		= CoreExpr     	Id DefBindee
> type DefAtom    		= CoreAtom	DefBindee
> type DefCaseAlternatives	= CoreCaseAlternatives Id DefBindee
> type DefCaseDefault		= CoreCaseDefault Id DefBindee

> type DefCoreArg = CoreArg DefBindee

> data DefBindee 
> 	= DefArgExpr DefExpr		-- arbitrary expressions as argumemts
>	| DefArgVar  Id			-- or just ids
>	| Label DefExpr DefExpr		-- labels for detecting cycles


Ok, I've cheated horribly here.  Instead of defining a new data type
including the new Label construct, I've just defined a new
parameterisation of Core in which a variable can be one of {variable,
expression, label}.  This gives us both arbitrary expressions on the
right hand side of application, in addition to the new Label
construct.

The penalty for this is that expressions will have extra indirections
as compared with a new datatype.  The saving is basically not having
to define a new datatype almost identical to Core.

Because our parameterised datatype is a little too general (i.e. it
distinguishes expressions that we wish to equate), there are some
invariants that will be adhered to during the transformation.  The
following are alternative representations for certain expressions.
The forms on the left are disallowed:

CoVar (DefArgExpr e)	==  e
CoVarAtom (Label l e)	==  CoVarAtom (DefArgExpr (CoVar (Label l e)))

For completeness, we should also have:

CoVarAtom (DefArgVar v) == CoVarAtom (DefArgExpr (CoVar (DefArgVar v)))
CoLitAtom l		== CoVarAtom (DefArgExpr (CoLit l))

In other words, atoms must all be of the form (CoVarAtom (DefArgExpr
_)) and the argument to a CoVar can only be Label or DefArgVar.

> mkLabel :: DefExpr -> DefExpr -> DefExpr
> mkLabel l e = CoVar (Label l e)
