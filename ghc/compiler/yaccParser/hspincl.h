#ifndef HSPINCL_H
#define HSPINCL_H

#include "../../includes/config.h"

#if __STDC__
#define PROTO(x)	x
#define NO_ARGS		void
#define CONST		const
#define VOID		void
#define VOID_STAR	void *
#define VOLATILE	volatile
#else
#define PROTO(x)	()
#define NO_ARGS		/* no args */
#define CONST		/* no const */
#define VOID		void /* hope for the best... */
#define VOID_STAR	long *
#define VOLATILE	/* no volatile */
#endif /* ! __STDC__ */

#if defined(STDC_HEADERS) || defined(HAVE_STRING_H)
#include <string.h>
/* An ANSI string.h and pre-ANSI memory.h might conflict.  */
#if !defined(STDC_HEADERS) && defined(HAVE_MEMORY_H)
#include <memory.h>
#endif /* not STDC_HEADERS and HAVE_MEMORY_H */
#define index strchr
#define rindex strrchr
#define bcopy(s, d, n) memcpy ((d), (s), (n))
#define bcmp(s1, s2, n) memcmp ((s1), (s2), (n))
#define bzero(s, n) memset ((s), 0, (n))
#else /* not STDC_HEADERS and not HAVE_STRING_H */
#include <strings.h>
/* memory.h and strings.h conflict on some systems.  */
#endif /* not STDC_HEADERS and not HAVE_STRING_H */

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "id.h"
#include "literal.h"
#include "list.h"
#ifdef DPH
#include "ttype-DPH.h"
#else
#include "ttype.h"
#endif
#include "atype.h"
#include "coresyn.h"
#include "hpragma.h"
#include "binding.h"
#include "finfot.h"
/*#include "impidt.h"*/
#include "entidt.h"
#ifdef DPH
#include "tree-DPH.h"
#else
#define infixTree tree
#include "tree.h"
#endif
#include "pbinding.h"

extern char *input_filename;

extern tree *Rginfun  PROTO((struct Sap *));
extern tree *Rginarg1 PROTO((struct Sap *));
extern tree *Rginarg2 PROTO((struct Sap *));

#endif /* HSPINCL_H */
