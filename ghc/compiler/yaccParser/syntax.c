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
#ifdef DPH
#include "tree-DPH.h"
#else
#include "tree.h"
#endif

/* 
   This file, syntax.c, is used both for the regular parser
   and for parseint; however, we use the tab.h file from
   the regular parser.  This could get us in trouble...
*/
#ifdef DPH
#include "hsparser-DPH.tab.h"
#else
#include "hsparser.tab.h"
#endif /* Data Parallel Haskell */

/* Imported values */
extern short icontexts;
extern list Lnil;
extern unsigned endlineno, startlineno;
extern BOOLEAN hashIds, etags;

/* Forward Declarations */

char *ineg    PROTO((char *));
tree unparen  PROTO((tree));

tree  fns[MAX_CONTEXTS] = { NULL };
short samefn[MAX_CONTEXTS] = { 0 };
tree  prevpatt[MAX_CONTEXTS] = { NULL };

BOOLEAN inpat = FALSE;


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
checkprevpatt()
{
  if (PREVPATT == NULL)
    hsperror("\"'\" used before a function definition");
}

void
checksamefn(fn)
  char *fn;
{
  SAMEFN = (hashIds && fn == (char *)FN) || (FN != NULL && strcmp(fn,gident(FN)) == 0);
  if(!SAMEFN && etags)
#if 1/*etags*/
    printf("%u\n",startlineno);
#else
    fprintf(stderr,"%u\tchecksamefn:%s\n",startlineno,fn);
#endif
}


/*
  Check that a list of types is a list of contexts
*/

void
checkcontext(context)
  list context;
{
  ttype ty; list tl;
  int valid;

  while (tlist(context) == lcons)
    {
      ty = (ttype) lhd(context);
      valid = tttype(ty) == tname;
      if (valid)
	{
	  tl = gtypel(ty);
	  valid = tlist(tl) != lnil && tlist(ltl(tl)) == lnil && tttype((ttype) lhd(tl)) == namedtvar;
	}

      if (!valid)
	hsperror("Not a valid context");

      context = ltl(context);
    }
}

void
checkinpat()
{
  if(!inpat)
    hsperror("syntax error");
}

void
checkpatt(e)
  tree e;
{
  switch(ttree(e))
    {
      case ident:
      case wildp:
        break;

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
	    break;
	  default:
	    hsperror("not a valid literal pattern");
	}
	break;

      case negate:
	if (ttree(gnexp(e)) != lit) {
	    hsperror("syntax error: \"-\" applied to a non-literal");
	} else {
	    literal l = glit(gnexp(e));

	    if (tliteral(l) != integer && tliteral(l) != floatr) {
	      hsperror("syntax error: \"-\" applied to a non-number");
	    }
	}
	break;

      case ap:
	{
	  tree f = gfun(e);
	  tree a = garg(e);

	  checkconap(f, a);
	}
	break;

      case as:
	checkpatt(gase(e));
	break;

      case lazyp:
	checkpatt(glazyp(e));
	break;

      case plusp:
	checkpatt(gplusp(e));
	break;

      case tinfixop:
	{
	  tree f = ginfun((struct Sap *)e),
	       a1 = ginarg1((struct Sap *)e),
	       a2 = ginarg2((struct Sap *)e);

	  struct Splusp *e_plus;

	  checkpatt(a1);

	  if (ttree(f) == ident && strcmp(id_to_string(gident(f)),"+")==0)
	    {
	      if(ttree(a2) != lit || tliteral((literal) ttree(a2)) != integer)
		hsperror("syntax error: non-integer in (n+k) pattern");

	      if(ttree(a1) == wildp || (ttree(a1) == ident && !isconstr(gident(a1))))
		{
		  e->tag = plusp;
		  e_plus = (struct Splusp *) e;
		  *Rgplusp(e_plus) = a1;
		  *Rgplusi(e_plus) = glit(a2);
		}
	      else
		hsperror("syntax error: non-variable in (n+k) pattern");
	    }
	  else
	    {
	      if(ttree(f) == ident && !isconstr(gident(f)))
		hsperror("syntax error: variable application in pattern");
	      checkpatt(a2);
	    }
	}
	break;

      case tuple:
	{
	  list tup = gtuplelist(e);
	  while (tlist(tup) == lcons)
	    {
	      checkpatt(lhd(tup));
	      tup = ltl(tup);
	    }
	}
	break;

      case par:
	checkpatt(gpare(e));
	break;

      case llist:
	{
	  list l = gllist(e);
	  while (tlist(l) == lcons)
	    {
	      checkpatt(lhd(l));
	      l = ltl(l);
	    }
	}
	break;

#ifdef DPH
      case proc:
        {
          list pids = gprocid(e);
	  while (tlist(pids) == lcons)
	    {
	      checkpatt(lhd(pids));
	      pids = ltl(pids);
	    }
	  checkpatt(gprocdata(e));
	}
	break;
#endif /* DPH */

      default:
	hsperror("not a pattern");
      }
}


BOOLEAN /* return TRUE if LHS is a pattern; FALSE if a function */
is_patt_or_fun(e, outer_level)
  tree e;
  BOOLEAN outer_level;
	/* only needed because x+y is a *function* at
	   the "outer level", but an n+k *pattern* at
	   any "inner" level.  Sigh. */
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
      case plusp:
      case llist:
      case tuple:
      case negate:
#ifdef DPH
      case proc:
#endif
	checkpatt(e);
	return TRUE;

      case ident:
	return(TRUE);
	/* This change might break ap infixop below.  BEWARE.
	  return (isconstr(gident(e)));
        */

      case ap:
	{
	  tree a  = garg(e);
		    /* do not "unparen", otherwise the error
			fromInteger ((x,y) {-no comma-} z)
		       will be missed.
		    */
	  tree fn = function(e);

/*fprintf(stderr,"ap:f=%d %s (%d),a=%d %s\n",ttree(gfun(e)),(ttree(gfun(e)) == ident) ? (gident(gfun(e))) : "",ttree(fn),ttree(garg(e)),(ttree(garg(e)) == ident) ? (gident(garg(e))) : "");*/
	  checkpatt(a);

	  if(ttree(fn) == ident)
	    return(isconstr(gident(fn)));

	  else if(ttree(fn) == tinfixop)
	    return(is_patt_or_fun(fn, TRUE/*still at "outer level"*/));

	  else
	    hsperror("Not a legal pattern binding in LHS");
	}

      case tinfixop:
	{
	  tree f =  ginfun((struct Sap *)e),
	       a1 = unparen(ginarg1((struct Sap *)e)),
	       a2 = unparen(ginarg2((struct Sap *)e));

	  struct Splusp *e_plus;

	  /* Even function definitions must have pattern arguments */
	  checkpatt(a1);
	  checkpatt(a2);

	  if (ttree(f) == ident)
	    {
	      if(strcmp(id_to_string(gident(f)),"+")==0 && ttree(a1) == ident)
		{
		  /* n+k is a function at the top level */
		  if(outer_level || ttree(a2) != lit || tliteral((literal) ttree(a2)) != integer)
		    return FALSE;

		  e->tag = plusp;
		  e_plus = (struct Splusp *) e;
		  *Rgplusp(e_plus) = a1;
		  *Rgplusi(e_plus) = glit(a2);
		  return TRUE;
		}
	      else
		return(isconstr(gident(f)));
	    }

	  else
	    hsperror("Strange infix op");
	}

      case par:
	return(is_patt_or_fun(gpare(e), FALSE /*no longer at "outer level"*/));

      /* Anything else must be an illegal LHS */
      default:
	hsperror("Not a valid LHS");
      }

  abort(); /* should never get here */
  return(FALSE);
}

/* interface for the outside world */
BOOLEAN
lhs_is_patt(e)
  tree e;
{
  return(is_patt_or_fun(e, TRUE /*outer-level*/));
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
        checkpatt(garg(e));
        return(function(gfun(e)));

      case par:
	return(function(gpare(e)));
	
      default:
	return(e);
    }
}


tree
unparen(e)
  tree e;
{
  while (ttree(e) == par)
      e = gpare(e);

  return(e);
}

void
checkconap(f, a)
  tree f, a;
{
  switch(ttree(f))
    {
      case ident:
        if (isconstr(gident(f)))
	  {
	    checkpatt(a);
	    return;
	  }
	{
	  char errbuf[ERR_BUF_SIZE];
	  sprintf(errbuf,"syntax error: not a constructor application -- %s",gident(f));
	  hsperror(errbuf);
	}

      case ap:
	checkconap(gfun(f), garg(f));
	checkpatt(a);
	return;

      case par:
	checkconap(gpare(f), a);
	break;

      case tuple:
	{
	   char errbuf[ERR_BUF_SIZE];
	   sprintf(errbuf,"syntax error: tuple pattern `applied' to arguments (missing comma?)");
	   hsperror(errbuf);
	}
	break;

      default:
	hsperror("syntax error: not a constructor application");
      }
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
precparse(t)
  tree t;
{
#if 0
#ifdef HSP_DEBUG
  fprintf(stderr,"precparse %x\n",ttree(t));
#endif
#endif
  if(ttree(t) == tinfixop)
    {
      tree left =  ginarg1((struct Sap *)t);

#if 0
#ifdef HSP_DEBUG
      fprintf(stderr,"precparse:t=");ptree(t);printf("\nleft=");ptree(left);printf("\n");
#endif
#endif

      if(ttree(left) == negate)
	{
	  id tid = gident(ginfun((struct Sap *)t));
	  struct infix *ttabpos = infixlookup(tid);
	  struct infix *ntabpos = infixlookup(install_literal("-")); /* This should be static, but C won't allow that. */
	  
	  if(pprecedence(ntabpos) < pprecedence(ttabpos))
	    {
	      tree right = ginarg2((struct Sap *)t);
	      t->tag = negate;
	      gnexp(t) = mkinfixop(tid,gnexp(left),right);
	    }
	}

      else if(ttree(left) == tinfixop)
	{
	  id lid = gident(ginfun((struct Sap *)left)),
	     tid = gident(ginfun((struct Sap *)t));

	  struct infix *lefttabpos = infixlookup(lid),
	               *ttabpos    = infixlookup(tid);

#if 0
#ifdef HSP_DEBUG
	  fprintf(stderr,"precparse: lid=%s; tid=%s,ltab=%d,ttab=%d\n",
		  id_to_string(lid),id_to_string(tid),pprecedence(lefttabpos),pprecedence(ttabpos));
#endif
#endif

	  if (pprecedence(lefttabpos) < pprecedence(ttabpos))
	    rearrangeprec(left,t);

	  else if (pprecedence(lefttabpos) == pprecedence(ttabpos))
	    {
	      if(pfixity(lefttabpos) == INFIXR && pfixity(ttabpos) == INFIXR)
		  rearrangeprec(left,t);

	      else if(pfixity(lefttabpos) == INFIXL && pfixity(ttabpos) == INFIXL)
		/* SKIP */;

	      else
		{
		  char errbuf[ERR_BUF_SIZE];
		  sprintf(errbuf,"Cannot mix %s and %s in the same infix expression", 
			  id_to_string(lid), id_to_string(tid));
		  hsperror(errbuf);
	      }
	    }
	}
    }
}


/*
  Rearrange a tree to effectively insert an operator in the correct place.
  The recursive call to precparse ensures this filters down as necessary.
*/

void
rearrangeprec(t1,t2)
  tree t1, t2;
{
  tree arg3 = ginarg2((struct Sap *)t2);
  id id1 = gident(ginfun((struct Sap *)t1)),
     id2 = gident(ginfun((struct Sap *)t2));
  gident(ginfun((struct Sap *)t1)) = id2;
  gident(ginfun((struct Sap *)t2)) = id1;

  ginarg2((struct Sap *)t2) = t1;
  ginarg1((struct Sap *)t2) = ginarg1((struct Sap *)t1);
  ginarg1((struct Sap *)t1) = ginarg2((struct Sap *)t1);
  ginarg2((struct Sap *)t1) = arg3;
  precparse(t1);
}

pbinding
createpat(guards,where)
  list    guards;
  binding where;
{
  char *func;

  if(FN != NULL)
    func = gident(FN);
  else
    func = install_literal("");

  /* I don't think I need to allocate func here -- KH */
  return(mkpgrhs(PREVPATT,guards,where,func,endlineno));
}


list
mktruecase(expr)
  tree expr;
{
/* partain: want a more magical symbol ???
  return(ldub(mkbool(1),expr));
*/
  return(ldub(mkident(install_literal("__o")),expr)); /* __otherwise */
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

BOOLEAN
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


BOOLEAN
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
  Check the precedence of a pattern or expression to ensure that
  sections and function definitions have the correct parse.
*/

void
checkprec(exp,fn,right)
  tree exp;
  id fn;
  BOOLEAN right;
{
  if(ttree(exp) == tinfixop)
    {
      struct infix *ftabpos = infixlookup(fn);
      struct infix *etabpos = infixlookup(gident(ginfun((struct Sap *)exp)));

      if (pprecedence(etabpos) > pprecedence(ftabpos) ||
	 (pprecedence(etabpos) == pprecedence(ftabpos) &&
	  ((pfixity(etabpos) == INFIXR && pfixity(ftabpos) == INFIXR && right) ||
	  ((pfixity(etabpos) == INFIXL && pfixity(ftabpos) == INFIXL && !right)))))
	/* SKIP */;

      else
	{
	  char errbuf[ERR_BUF_SIZE];
	  sprintf(errbuf,"Cannot mix %s and %s on a LHS or in a section", 
		  id_to_string(fn), id_to_string(gident(ginfun((struct Sap *)exp))));
	  hsperror(errbuf);
	}
    }
}

