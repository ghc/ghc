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

/*  Imported Values */
extern list Lnil;

static void is_context_format PROTO((ttype)); /* forward */

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
    char *tycon_name;
    list  args, rest_args;
    ttype first_arg;

    switch (tttype(t)) {
      case ttuple:
	/* returning the list is OK, but ensure items are right format */
	args = gttuple(t);

	if (tlist(args) == lnil)
	  hsperror ("type2context: () found instead of a context");

	while (tlist(args) != lnil) 
	  {
	    is_context_format(lhd(args));
	    args = ltl(args);
	  }

	return(gttuple(t)); /* args */


      case tname :
    	tycon_name = gtypeid(t);

	/* just a class name ":: C =>" */	
	if (tlist(gtypel(t)) == lnil) 
	    return (mklcons(t, Lnil));

	/* should be just: ":: C a =>" */
	else
	  {
	    first_arg = (ttype) lhd(gtypel(t));
	    rest_args = ltl(gtypel(t)); /* should be nil */

	    if (tlist(rest_args) != lnil)
	      hsperror ("type2context: too many variables after class name");

	    switch (tttype(first_arg)) 
	      {
	        case namedtvar:	/* ToDo: right? */
		  return (mklcons(t, Lnil));
		  break;

	        default:
		  hsperror ("type2context: something wrong with variable after class name");
	      }
	  }
    	break;

      case namedtvar:
    	hsperror ("type2context: unexpected namedtvar found in a context");

      case tllist:
    	hsperror ("type2context: list constructor found in a context");

      case tfun:
    	hsperror ("type2context: arrow (->) constructor found in a context");

      case context:
    	hsperror ("type2context: unexpected context-thing found in a context");

      default    :
    	hsperror ("type2context: totally unexpected input");
    }
    abort(); /* should never get here! */
}


/* is_context_format is the same as "type2context" except that it just performs checking */
/* ttype is either "tycon" [class] or "tycon (named)tvar" [class var] */

static void
is_context_format(t)
  ttype t;
{
    char *tycon_name;
    list  rest_args;
    ttype first_arg;

    switch (tttype(t)) 
      {
        case tname :
	  tycon_name = gtypeid(t);

	  /* just a class name ":: C =>" */
	  if (tlist(gtypel(t)) == lnil) 
	    hsperror("is_context_format: variable missing after class name");

	  /* should be just: ":: C a =>" */
	  else
	    {
	      first_arg = (ttype) lhd(gtypel(t));
	      rest_args = ltl(gtypel(t)); /* should be nil */
	      if (tlist(rest_args) != lnil)
		hsperror ("is_context_format: too many variables after class name");

	      switch (tttype(first_arg))
		{
	          case namedtvar:	/* ToDo: right? */
		    /* everything is cool; will fall off the end */
		    break;
		  default:
		    hsperror ("is_context_format: something wrong with variable after class name");
		}
	    }
	  break;

	case ttuple:
	  hsperror ("is_context_format: tuple found in a context");

	case namedtvar:
	  hsperror ("is_context_format: unexpected namedtvar found in a context");

	case tllist:
	  hsperror ("is_context_format: list constructor found in a context");

	case tfun:
	  hsperror ("is_context_format: arrow (->) constructor found in a context");

	case context:
	  hsperror ("is_context_format: unexpected context-thing found in a context");

	default:
	    hsperror ("is_context_format: totally unexpected input");
      }
}

