/**********************************************************************
*                                                                     *
*                                                                     *
*     Syntax-related Utility Functions                                *
*                                                                     *
*                                                                     *
**********************************************************************/

#include <stdio.h>
#include <ctype.h>

#include "hspincl.h"
#include "constants.h"
#include "utils.h"
#include "tree.h"

#include "hsparser.tab.h"

/* Imported values */
extern short icontexts;
extern list Lnil;
extern unsigned endlineno, startlineno;
extern BOOLEAN hashIds, etags;

/* Forward Declarations */

char *ineg		    PROTO((char *));
static tree unparen	    PROTO((tree));
static void is_conapp_patt  PROTO((int, tree, tree));
static void rearrangeprec   PROTO((tree, tree));
static void error_if_expr_wanted PROTO((int, char *));
static void error_if_patt_wanted PROTO((int, char *));

qid	fns[MAX_CONTEXTS] = { NULL };
BOOLEAN samefn[MAX_CONTEXTS] = { FALSE };
tree	prevpatt[MAX_CONTEXTS] = { NULL };

BOOLEAN inpat = FALSE;

static BOOLEAN	 checkorder2 PROTO((binding, BOOLEAN));
static BOOLEAN	 checksig PROTO((BOOLEAN, binding));

/*
  check infix value in range 0..9
*/


int
checkfixity(vals)
  char *vals;
{
  int value;
  sscanf(vals,"%d",&value);

  if (value < 0 || value > 9)
    {
      int oldvalue = value;
      value = value < 0 ? 0 : 9;
      fprintf(stderr,"Precedence must be between 0 and 9 (value given: %d, changed to %d)\n",
	      oldvalue,value);
    }
  return(value);
}


/*
  Check Previous Pattern usage
*/

void
checksamefn(fn)
  qid fn;
{
  char *this = qid_to_string(fn);
  char *was  = (FN==NULL) ? NULL : qid_to_string(FN);

  SAMEFN = (was != NULL && strcmp(this,was) == 0);

  if(!SAMEFN && etags)
#if 1/*etags*/
    printf("%u\n",startlineno);
#else
    fprintf(stderr,"%u\tchecksamefn:%s\n",startlineno,this);
#endif
}


void
checkinpat()
{
  if(!inpat)
    hsperror("pattern syntax used in expression");
}

/* ------------------------------------------------------------------------
*/

void
expORpat(int wanted, tree e)
{
  switch(ttree(e))
    {
      case ident: /* a pattern or expr */
	break;

      case wildp:
	error_if_expr_wanted(wanted, "wildcard in expression");
	break;

      case as:
	error_if_expr_wanted(wanted, "`as'-pattern instead of an expression");
	expORpat(wanted, gase(e));
	break;

      case lazyp:
	error_if_expr_wanted(wanted, "irrefutable pattern instead of an expression");
	expORpat(wanted, glazyp(e));
	break;

      case lit:
	switch (tliteral(glit(e))) {
	  case integer:
	  case intprim:
	  case floatr:
	  case doubleprim:
	  case floatprim:
	  case string:
	  case stringprim:
	  case charr:
	  case charprim:
	    break; /* pattern or expr */

	  case clitlit:
	    error_if_patt_wanted(wanted, "``literal-literal'' in pattern");

	  default: /* the others only occur in pragmas */
	    hsperror("not a valid literal pattern or expression");
	}
	break;

      case negate:
	{ tree sub = gnexp(e);
	  if (ttree(sub) != lit) {
	      error_if_patt_wanted(wanted, "\"-\" applied to a non-literal");
	  } else {
	      literal l = glit(sub);

	      if (tliteral(l) != integer && tliteral(l) != floatr) {
		error_if_patt_wanted(wanted, "\"-\" applied to a non-number");
	      }
	  }
	  expORpat(wanted, sub);
	}
	break;

      case ap:
	{
	  tree f = gfun(e);
	  tree a = garg(e);

	  is_conapp_patt(wanted, f, a); /* does nothing unless wanted == LEGIT_PATT */
	  expORpat(wanted, f);
	  expORpat(wanted, a);
	}
	break;

      case infixap:
	{
	  qid  f  = ginffun ((struct Sinfixap *)e);
	  tree a1 = ginfarg1((struct Sinfixap *)e);
	  tree a2 = ginfarg2((struct Sinfixap *)e);

	  expORpat(wanted, a1);
	  expORpat(wanted, a2);

	  if (wanted == LEGIT_PATT && !isconstr(qid_to_string(f)))
	     hsperror("variable application in pattern");
	}
	break;

      case record:
	{
          list field;
	  for (field = grbinds(e); tlist(field) == lcons; field = ltl(field)) {
	      expORpat(wanted, lhd(field));
	  }
	}
	break;

      case rbind:
	if (tmaybe(grbindexp(e)) == just)
	    expORpat(wanted, gthing(grbindexp(e)));
	break;

      case tuple:
	{
	  list tup;
	  for (tup = gtuplelist(e); tlist(tup) == lcons; tup = ltl(tup)) {
	      expORpat(wanted, lhd(tup));
	  }
	}
	break;

      case llist:
	{
	  list l;
	  for (l = gllist(e); tlist(l) == lcons; l = ltl(l)) {
	      expORpat(wanted, lhd(l));
	  }
	}
	break;

      case par: /* parenthesised */
	expORpat(wanted, gpare(e));
	break;

      case restr:
      case lambda:
      case let:
      case casee:
      case ife:
      case doe:
      case ccall:
      case scc:
      case rupdate:
      case comprh:
      case eenum:
      case lsection:
      case rsection:
	error_if_patt_wanted(wanted, "unexpected construct in a pattern");
	break;

      default:
	hsperror("not a pattern or expression");
      }
}

static void
is_conapp_patt(int wanted, tree f, tree a)
{
  if (wanted == LEGIT_EXPR)
     return; /* that was easy */

  switch(ttree(f))
    {
      case ident:
        if (isconstr(qid_to_string(gident(f))))
	  {
	    expORpat(wanted, a);
	    return;
	  }
	{
	  char errbuf[ERR_BUF_SIZE];
	  sprintf(errbuf,"not a constructor application -- %s",qid_to_string(gident(f)));
	  hsperror(errbuf);
	}

      case ap:
	is_conapp_patt(wanted, gfun(f), garg(f));
	expORpat(wanted, a);
	return;

      case par:
	is_conapp_patt(wanted, gpare(f), a);
	break;

      case tuple:
	{
	   char errbuf[ERR_BUF_SIZE];
	   sprintf(errbuf,"tuple pattern `applied' to arguments (missing comma?)");
	   hsperror(errbuf);
	}
	break;

      default:
	hsperror("not a constructor application");
      }
}

static void
error_if_expr_wanted(int wanted, char *msg)
{
    if (wanted == LEGIT_EXPR)
	hsperror(msg);
}

static void
error_if_patt_wanted(int wanted, char *msg)
{
    if (wanted == LEGIT_PATT)
	hsperror(msg);
}

/* ---------------------------------------------------------------------- */

BOOLEAN /* return TRUE if LHS is a pattern */
lhs_is_patt(tree e)
{
  switch(ttree(e))
    {
      case lit:
	switch (tliteral(glit(e))) {
	  case integer:
	  case intprim:
	  case floatr:
	  case doubleprim:
	  case floatprim:
	  case string:
	  case charr:
	  case charprim:
	  case stringprim:
	    return TRUE;
	  default:
	    hsperror("Literal is not a valid LHS");
	}

      case wildp:
        return TRUE;

      case as:
      case lazyp:
      case llist:
      case tuple:
      case negate:
	expORpat(LEGIT_PATT, e);
	return TRUE;

      case ident:
	return(TRUE);
	/* This change might break ap infixop below.  BEWARE.
	   return (isconstr(qid_to_string(gident(e))));
        */

      case ap:
	{
	  tree f = function(e);
	  tree a = garg(e);       /* do not "unparen", otherwise the error
				       fromInteger ((x,y) {-no comma-} z)
				     will be missed.
			          */

	  /* definitions must have pattern arguments */
	  expORpat(LEGIT_PATT, a);

	  if(ttree(f) == ident)
	    return(isconstr(qid_to_string(gident(f))));

	  else if(ttree(f) == infixap)
	    return(lhs_is_patt(f));

	  else
	    hsperror("Not a legal pattern binding in LHS");
	}

      case infixap:
	{
	  qid  f  = ginffun((struct Sinfixap *)e);
	  tree a1 = unparen(ginfarg1((struct Sinfixap *)e)),
	       a2 = unparen(ginfarg2((struct Sinfixap *)e));

	  /* definitions must have pattern arguments */
	  expORpat(LEGIT_PATT, a1);
	  expORpat(LEGIT_PATT, a2);

	  return(isconstr(qid_to_string(f)));
	}

      case par:
	return(lhs_is_patt(gpare(e)));

      /* Anything else must be an illegal LHS */
      default:
	hsperror("Not a valid LHS");
      }

  abort(); /* should never get here */
  return(FALSE);
}


/*
  Return the function at the root of a series of applications.
*/

tree
function(e)
  tree e;
{
  switch (ttree(e))
    {
      case ap:
        expORpat(LEGIT_PATT, garg(e));
        return(function(gfun(e)));

      case par:
	return(function(gpare(e)));
	
      default:
	return(e);
    }
}


static tree
unparen(e)
  tree e;
{
  while (ttree(e) == par)
      e = gpare(e);

  return(e);
}


/*
  Extend a function by adding a new definition to its list of bindings.
*/

void
extendfn(bind,rule)
binding bind;
binding rule;
{
/*  fprintf(stderr,"extending binding (%d)\n",tbinding(bind));*/
  if(tbinding(bind) == abind)
    bind = gabindsnd(bind);

  if(tbinding(bind) == pbind)
    gpbindl(bind) = lconc(gpbindl(bind), gpbindl(rule));
  else if(tbinding(bind) == fbind)
    gfbindl(bind) = lconc(gfbindl(bind), gfbindl(rule));
  else
    fprintf(stderr,"bind error in decl (%d)\n",tbinding(bind));
}

/* 

  Precedence Parser for Haskell.  By default operators are left-associative, 
  so it is only necessary to rearrange the parse tree where the new operator
  has a greater precedence than the existing one, or where two operators have
  the same precedence and are both right-associative. Error conditions are
  handled.

  Note:  Prefix negation has the same precedence as infix minus.
         The algorithm must thus take account of explicit negates.
*/

void
precparse(tree t)
{
  if(ttree(t) == infixap)
    {
      tree left = ginfarg1(t);

      if(ttree(left) == negate)
	{
	  struct infix *ttabpos = infixlookup(ginffun(t));
	  struct infix *ntabpos = infixlookup(mknoqual(install_literal("-")));
	  
	  if(pprecedence(ntabpos) < pprecedence(ttabpos))
	    {
	      /* (-x)*y  ==> -(x*y) */
	      qid  lop  = ginffun(t);
	      tree arg1 = gnexp(left);
	      tree arg2 = ginfarg2(t);

	      t->tag = negate;
	      gnexp(t) = left;
	      gnxxx1(t) = NULL;
	      gnxxx2(t) = NULL;

	      left->tag = infixap;
	      ginffun(left)  = lop;
	      ginfarg1(left) = arg1;
	      ginfarg2(left) = arg2;

	      precparse(left);
	    }
	}

      else if(ttree(left) == infixap)
	{
	  struct infix *ttabpos    = infixlookup(ginffun(t));
	  struct infix *lefttabpos = infixlookup(ginffun(left));

	  if(pprecedence(lefttabpos) < pprecedence(ttabpos))
	    rearrangeprec(left,t);

	  else if(pprecedence(lefttabpos) == pprecedence(ttabpos))
	    {
	      if(pfixity(lefttabpos) == INFIXR && pfixity(ttabpos) == INFIXR)
		rearrangeprec(left,t);

	      else if(pfixity(lefttabpos) == INFIXL && pfixity(ttabpos) == INFIXL)
		/* SKIP */;

	      else
		{
		  char errbuf[ERR_BUF_SIZE];
		  sprintf(errbuf,"Cannot mix %s and %s in the same infix expression", 
			  qid_to_string(ginffun(left)), qid_to_string(ginffun(t)));
		  hsperror(errbuf);
	      }
	    }
	}
    }
}


/*
  Rearrange a tree to effectively insert an operator in the correct place.

  x+y*z ==parsed== (x+y)*z  ==>  x+(y*z)

  The recursive call to precparse ensures this filters down as necessary.
*/

static void
rearrangeprec(tree left, tree t)
{
  qid top  = ginffun(left);
  qid lop  = ginffun(t);
  tree arg1 = ginfarg1(left);
  tree arg2 = ginfarg2(left);
  tree arg3 = ginfarg2(t);

  ginffun(t)  = top;
  ginfarg1(t) = arg1;
  ginfarg2(t) = left;

  ginffun(left)  = lop;
  ginfarg1(left) = arg2;
  ginfarg2(left) = arg3;

  precparse(left);
}

pbinding
createpat(guards,where)
  pbinding guards;
  binding where;
{
  qid func;

  if(FN != NULL)
    func = FN;
  else
    func = mknoqual(install_literal(""));

  return(mkpgrhs(PREVPATT,guards,where,func,endlineno));
}

char *
ineg(i)
  char *i;
{
  char *p = xmalloc(strlen(i)+2);

  *p = '-';
  strcpy(p+1,i);
  return(p);
}

#if 0
/* UNUSED: at the moment */
void
checkmodname(import,interface)
  id import, interface;
{
  if(strcmp(import,interface) != 0)
    {
      char errbuf[ERR_BUF_SIZE];
      sprintf(errbuf,"interface name (%s) does not agree with import name (%s)",interface,import);
      hsperror(errbuf);
    }
}
#endif /* 0 */

/*
  Check the ordering of declarations in a cbody.
  All signatures must appear before any declarations.
*/

void
checkorder(decls)
  binding decls;
{
  /* The ordering must be correct for a singleton */
  if(tbinding(decls)!=abind)
    return;

  checkorder2(decls,TRUE);
}

static BOOLEAN
checkorder2(decls,sigs)
  binding decls;
  BOOLEAN sigs;
{
  while(tbinding(decls)==abind)
    {
      /* Perform a left-traversal if necessary */
      binding left = gabindfst(decls);
      if(tbinding(left)==abind)
	sigs = checkorder2(left,sigs);
      else
	sigs = checksig(sigs,left);
      decls = gabindsnd(decls);
    }

  return(checksig(sigs,decls));
}


static BOOLEAN
checksig(sig,decl)
  BOOLEAN sig;
  binding decl;
{
  BOOLEAN issig = tbinding(decl) == sbind || tbinding(decl) == nullbind;
  if(!sig && issig)
    hsperror("Signature appears after definition in class body");

  return(issig);
}


/*
  Check the last expression in a list of do statements.
*/

void
checkdostmts(stmts)
  list stmts;
{
  if (tlist(stmts) == lnil)
      hsperror("do expression with no statements");

  for(; tlist(ltl(stmts)) != lnil; stmts = ltl(stmts))
      ;
  if (ttree(lhd(stmts)) != doexp)
      hsperror("do statements must end with expression");
}


/*
  Check the precedence of a pattern or expression to ensure that
  sections and function definitions have the correct parse.
*/

void
checkprec(exp,qfn,right)
  tree exp;
  qid qfn;
  BOOLEAN right;
{
  if(ttree(exp) == infixap)
    {
      struct infix *ftabpos = infixlookup(qfn);
      struct infix *etabpos = infixlookup(ginffun(exp));

      if (pprecedence(etabpos) > pprecedence(ftabpos) ||
	 (pprecedence(etabpos) == pprecedence(ftabpos) &&
	  ((pfixity(etabpos) == INFIXR && pfixity(ftabpos) == INFIXR && right) ||
	  ((pfixity(etabpos) == INFIXL && pfixity(ftabpos) == INFIXL && !right)))))
	/* SKIP */;
      else
	{
	  char errbuf[ERR_BUF_SIZE];
	  sprintf(errbuf,"Cannot mix %s and %s on a LHS or in a section", 
		  qid_to_string(qfn), qid_to_string(ginffun(exp)));
	  hsperror(errbuf);
	}
    }
}


/*
  Checks there are no bangs in a tycon application.
*/

void
checknobangs(app)
  ttype app;
{
  if(tttype(app) == tapp)
    {
      if(tttype(gtarg((struct Stapp *)app)) == tbang)
	hsperror("syntax error: unexpected ! in type");

      checknobangs(gtapp((struct Stapp *)app));
    }	  
}


/*
  Splits a tycon application into its constructor and a list of types.
*/

void
splittyconapp(app, tyc, tys)
  ttype app;
  qid *tyc;
  list *tys;
{
  if(tttype(app) == tapp) 
    {
      splittyconapp(gtapp((struct Stapp *)app), tyc, tys);
      *tys = lapp(*tys, gtarg((struct Stapp *)app));
    }
  else if(tttype(app) == tname)
    {
      *tyc = gtypeid((struct Stname *)app);
      *tys = Lnil;
    }
  else
    {
      hsperror("panic: splittyconap: bad tycon application (no tycon)");
    }
}
