/*
 *	Infix operator stuff -- modified from LML
 */

#include <stdio.h>

#include "hspincl.h"
#ifdef DPH
#include "hsparser-DPH.tab.h"
#else
#include "hsparser.tab.h"
#endif
#include "constants.h"
#include "utils.h"

static short iscope = 1;

static struct infix {
    char *iname;
    short ilen;
    short ifixity;
    short iprecedence;
} infixtab[INFIX_SCOPES][MAX_INFIX] =
  {
  /*
	Name		Len	Fixity		Precedence
  */
	"$",		1,  	INFIXR,		0,
    	":=", 		2,	INFIX,		1,
 	"||",		2,	INFIXR,		2,
	"&&",		2,	INFIXR,		3,
      	"==", 		2,	INFIX,		4,
    	"/=", 		2,	INFIX,		4,
    	"<", 		1,	INFIX,		4,
    	"<=", 		2,	INFIX,		4,
    	">", 		1,	INFIX,		4,
    	">=", 		2,	INFIX,		4,
	"elem",		4,	INFIX,		4,
	"notElem",	7,	INFIX,		4,
    	"\\\\",		2,	INFIX,		5,
    	":", 		1,	INFIXR,		5,
    	"++", 		2,	INFIXR,		5,
    	"+", 		1,	INFIXL,		6,
    	"-", 		1,	INFIXL,		6,
    	":+", 		2,	INFIX,		6,
    	"*", 		1,	INFIXL,		7,
    	"/", 		1,	INFIXL,		7,
    	"mod",	 	3,	INFIXL,		7,
    	"div", 		3,	INFIXL,	        7,
    	"rem", 		3,	INFIXL,		7,
    	"quot",		4,	INFIXL,		7,
    	":%", 		2,	INFIXL,		7, /* possibly wrong; should be omitted? */
    	"%", 		1,	INFIXL,		7,
    	"**", 		2,	INFIXR,		8,
    	"^", 		1,	INFIXR,		8,
    	"^^", 		2,	INFIXR,		8,
    	"!", 		1,	INFIXL,		9,
    	"!!", 		2,	INFIXL,		9,
	"//",		2,	INFIXL,		9,
    	".", 		1,	INFIXR,		9
};


#define NFIX 31						/* The number of predefined operators */
#define ninfix (ninfixtab[iscope])
static int ninfixtab[INFIX_SCOPES] = {NFIX,0};		/* # of predefined operators */
static char infixstr[MAX_ISTR];
static char *infixp = infixstr;

/* An "iscope" is an "infix scope": the scope of infix declarations
   (either the main module or an interface) */

void
enteriscope()
{
  if(++iscope > INFIX_SCOPES)
    {
      char errbuf[ERR_BUF_SIZE];
      sprintf(errbuf,"Too many infix scopes (> %d)\n",INFIX_SCOPES);
    }
  ninfix = 0;
}

#if 0
/* UNUSED */
void
exitiscope()
{
  --iscope;
}
#endif

void
exposeis()
{
  int i;
  --iscope;

  for (i=0; i < ninfixtab[iscope+1]; ++i)
    {
      struct infix *ip = infixtab[iscope+1] + i;
      makeinfix(install_literal(ip->iname),ip->ifixity,ip->iprecedence);
    }
}


static int
ionelookup(id name, int iscope)
{
  int i;
  char *iname = id_to_string(name);

  for(i = 0; i < ninfixtab[iscope]; i++)
    {
      if(strcmp(iname,infixtab[iscope][i].iname)==0)
	return(i);
    }

  return(-1);
}


struct infix *
infixlookup(name)
  id name;
{
  int i;
  for (i=iscope; i >= 0; --i)
    {
     int n = ionelookup(name,i);
      if (n >= 0)
	return (infixtab[i]+n);
    }
  return (NULL);
}

int
nfixes()
{
	return ninfix;
}

char *
fixop(int n)
{
	return infixtab[iscope][n].iname;
}

char *
fixtype(int n)
{
	switch(infixtab[iscope][n].ifixity) {
	case INFIXL:
		   return "infixl";

	case INFIXR:
		   return "infixr";

	case INFIX:
		 return "infix";

	default : return 0;
	/* Why might it return 0 ?? (WDP 94/11) */
	}
}

#if 0
/* UNUSED? */
int
fixity(n)
  int n;
{
#ifdef HSP_DEBUG
  fprintf(stderr,"fixity of %s (at %d) is %d\n",infixtab[iscope][n].iname,n,infixtab[iscope][n].ifixity);
#endif
  return(n < 0? INFIXL: infixtab[iscope][n].ifixity);
}
#endif /* 0 */


long int
precedence(n)
  int n;
{
#ifdef HSP_DEBUG
  fprintf(stderr,"precedence of %s (at %d) is %d\n",infixtab[iscope][n].iname,n,infixtab[iscope][n].iprecedence);
#endif
  return(n < 0? 9: infixtab[iscope][n].iprecedence);
}


int
pfixity(ip)
  struct infix *ip;
{
#ifdef HSP_DEBUG
  fprintf(stderr,"fixity of %s is %d\n",ip->iname,ip->ifixity);
#endif
  return(ip == NULL? INFIXL: ip->ifixity);
}

int
pprecedence(ip)
  struct infix *ip;
{
#ifdef HSP_DEBUG
  fprintf(stderr,"precedence of %s (at %d) is %d\n",ip->iname,ip->iprecedence);
#endif
  return(ip == NULL? 9: ip->iprecedence);
}


void
makeinfix(ssi, fixity, precedence)
  id ssi;
  int fixity, precedence;
{
    register int i, l;
    char s[1000];
    char *ss = id_to_string(ssi);

    for(i=0; i < ninfix; ++i)
      {
	if(strcmp(ss,infixtab[iscope][i].iname)==0)
	  {
	    /* Allow duplicate definitions if they are identical */
	    if(infixtab[iscope][i].ifixity!=fixity || 
	       infixtab[iscope][i].iprecedence!=precedence )
	      {
		char errbuf[ERR_BUF_SIZE];
		sprintf(errbuf,"(%s) already declared to be %s %d\n",
			ss,
			fixtype(i),
			infixtab[iscope][i].iprecedence);
		hsperror(errbuf);
	      }
	    return;
	  }
      }

    strcpy(s, ss);
    l = strlen(s);
    s[l] = 0;

    if (ninfix >= MAX_INFIX || infixp+l+1 >= &infixstr[MAX_ISTR]) {
        char errbuf[ERR_BUF_SIZE];
	sprintf(errbuf,"Too many Infix identifiers (> %d)",MAX_INFIX);
	hsperror(errbuf);
    }

#ifdef HSP_DEBUG
    fprintf(stderr,"adding %s (was %s), fixity=%d, prec=%d\n",s,ss,fixity,precedence);
#endif
    infixtab[iscope][ninfix].iname = infixp;
    strcpy(infixp, s);
    infixp += l+1;
    infixtab[iscope][ninfix].ifixity = fixity;
    infixtab[iscope][ninfix].iprecedence = precedence;
    infixtab[iscope][ninfix].ilen = l-1;
    ninfix++;
}
