/**********************************************************************
*                                                                     *
*                                                                     *
*      Convert Types to Contexts                                      *
*                                                                     *
*                                                                     *
**********************************************************************/


#include <stdio.h>
#include "hspincl.h"
#include "constants.h"
#include "utils.h"

/* 
    partain: see also the comment by "decl" in hsparser.y.

    Here, we've been given a type that must be of the form
    "C a" or "(C1 a, C2 a, ...)" [otherwise an error]

    Convert it to a list.
*/


list
type2context(t)
  ttype t;
{
    list  args;

    switch (tttype(t)) {
      case ttuple:
	/* returning the list is OK, but ensure items are right format */
	args = gttuple(t);

	if (tlist(args) == lnil)
	  hsperror ("type2context: () found instead of a context");

	while (tlist(args) != lnil) 
	  {
	    is_context_format(lhd(args), 0);
	    args = ltl(args);
	  }

	return(gttuple(t)); /* args */
	
      case tname:
	switch(tqid(gtypeid(t))) {
   	  case gid:
	     if (strcmp("()",gidname(gtypeid(t))) == 0)
	       return (Lnil);
          default: ;
        }
      case tapp:
	/* a single item, ensure correct format */
	is_context_format(t, 0);
	return(lsing(t));

      case namedtvar:
	fprintf(stderr, "namedtvar: %d %s\n", hashIds, gnamedtvar(t));
        if (strcmp("()", gnamedtvar(t)) == 0)
	       return (Lnil);
    	hsperror ("type2context: unexpected namedtvar found in a context");

      case tllist:
    	hsperror ("type2context: list constructor found in a context");

      case tfun:
    	hsperror ("type2context: arrow (->) constructor found in a context");

      default:
    	hsperror ("type2context: totally unexpected input");
    }
    abort(); /* should never get here! */
}


/* is_context_format is the same as "type2context" except that it just performs checking */
/* ttype is either "tycon" [class] or "tycon (named)tvar" [class var] */

void
is_context_format(t, tyvars)
  ttype t;
  int tyvars;
{
    list  rest_args;
    ttype first_arg;

    switch (tttype(t)) 
      {
        case tname :
	  /* should be just: ":: C a =>" */

	  if (tyvars == 0)
	    hsperror("is_context_format: type missing after class name");

	  /* tyvars > 0; everything is cool */
	  break;

	case tapp:
	  is_context_format(gtapp(t), tyvars+1);
	  break;

	case ttuple:
	  hsperror ("is_context_format: tuple found in a context");

	case namedtvar:
	  hsperror ("is_context_format: unexpected namedtvar found in a context");

	case tllist:
	  hsperror ("is_context_format: list constructor found in a context");

	case tfun:
	  hsperror ("is_context_format: arrow (->) constructor found in a context");
	default:
	    hsperror ("is_context_format: totally unexpected input");
      }
}


