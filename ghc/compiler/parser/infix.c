/*
 *	Infix operator stuff -- modified from LML
 */

#include <stdio.h>

#include "hspincl.h"
#include "hsparser.tab.h"
#include "constants.h"
#include "utils.h"

static struct infix {
    char *imod;
    char *iop;
    short thismod;
    short unqualok;
    short ifixity;
    short iprecedence;
} infixtab[MAX_INFIX];

static int ninfix = 0;

void
makeinfix(opid, fixity, precedence, modid, imported,
	  withas, impmodid, impasid, withqual,
	  withspec, withhiding, importspec)
  id opid;
  int fixity, precedence;
  long imported, withas, withqual, withspec, withhiding;
  id modid, impmodid, impasid;
  list importspec;
/*
  ToDo: Throw away infix operator if hidden by importspec!
*/
{
    int i;
    char *op = id_to_string(opid);
    char *mod = id_to_string(imported ? (withas ? impasid : impmodid) : modid);
    short thismod = ! imported;
    short unqualok = ! (imported && withqual);

    for(i=0; i < ninfix; ++i)
      {
	if(strcmp(op,infixtab[i].iop)==0 &&
	   strcmp(mod,infixtab[i].imod)==0 &&
	   unqualok==infixtab[i].unqualok)
	  {
	    /* Allow duplicate definitions if they are identical */
	    if (infixtab[i].ifixity==fixity && 
	        infixtab[i].iprecedence==precedence)
	      {
		return;
	      }

	    /* Allow local definition to override an import */
	    else if(thismod && !infixtab[i].thismod)
	      {
		/*continue*/
	      }

	    else
	      {
		char errbuf[ERR_BUF_SIZE];
		sprintf(errbuf,"%s.%s %s already declared to be %s %d\n",
			mod, op, unqualok ? "(unqualified)" : "(qualified)",
			infixstr(infixtab[i].ifixity),
			infixtab[i].iprecedence);
		hsperror(errbuf);
	      }
	  }
      }

    if (ninfix >= MAX_INFIX) {
        char errbuf[ERR_BUF_SIZE];
	sprintf(errbuf,"Too many Infix identifiers (> %d)",MAX_INFIX);
	hsperror(errbuf);
    }

#ifdef HSP_DEBUG
    fprintf(stderr,"makeinfix: %s.%s, fixity=%d prec=%d\n",mod,op,infixint(fixity),precedence);
#endif
    infixtab[ninfix].imod = mod;
    infixtab[ninfix].iop = op;
    infixtab[ninfix].thismod = thismod;
    infixtab[ninfix].unqualok = unqualok;
    infixtab[ninfix].ifixity = fixity;
    infixtab[ninfix].iprecedence = precedence;
    ninfix++;
}

struct infix *
infixlookup(name)
  qid name;
{
    int i;
    struct infix *found = NULL;
    char *op  = qid_to_string(name);
    char *mod = qid_to_mod(name);
    short unqual = mod == NULL;

    for(i = 0; i < ninfix; i++)
      {
	if(strcmp(op,infixtab[i].iop)==0 &&
	   ( (unqual && infixtab[i].unqualok) ||
	     (!unqual && strcmp(mod,infixtab[i].imod)==0)
	   ))
	  {
	    if (! found)
	      {
		/* first find */
		found = infixtab+i;
	      }
	    else if (found && ! found->thismod && infixtab[i].thismod)
	      {
		/* new find for this module; overrides */
		found = infixtab+i;
	      }
	    else if (found && found->thismod && ! infixtab[i].thismod)
	      {
		/* prev find for this module */
	      }
	    else if (found->ifixity == infixtab[i].ifixity &&
		     found->iprecedence == infixtab[i].iprecedence)
	      {
	        /* finds are identical */
	      }
	    else
	      {
		char errbuf[ERR_BUF_SIZE];
		sprintf(errbuf,"conflicting infix declarations for %s.%s\n  %s.%s %s (%s,%d) and %s.%s %s (%s,%d)\n",
			qid_to_pmod(name), op,
			found->imod, found->iop, found->unqualok ? "(unqualified)" : "(qualified)",
			   infixstr(found->ifixity),found->iprecedence,
			infixtab[i].imod, infixtab[i].iop, infixtab[i].unqualok ? "(unqualified)" : "(qualified)",
			   infixstr(infixtab[i].ifixity),infixtab[i].iprecedence);
		hsperror(errbuf);

	      }
	  }
      }

#ifdef HSP_DEBUG
  fprintf(stderr,"infixlookup: %s.%s = fixity=%d prec=%d\n",qid_to_pmod(name),op,infixint(pfixity(found)),pprecedence(found));
#endif

  return(found);
}

int
pfixity(ip)
  struct infix *ip;
{
  return(ip == NULL? INFIXL: ip->ifixity);
}

int
pprecedence(ip)
  struct infix *ip;
{
  return(ip == NULL? 9: ip->iprecedence);
}

char *
infixstr(n)
  int n;
{
  switch(n) {
    case INFIXL:
      return "infixl";
      
    case INFIXR:
      return "infixr";
	
    case INFIX:
      return "infix";

    default:
      hsperror("infixstr");
  }
}

long
infixint(n)
  int n;
{
  switch(n) {
    case INFIXL:
      return -1;
      
    case INFIX:
      return 0;

    case INFIXR:
      return 1;
	
    default:
      hsperror("infixint");
  }
}

