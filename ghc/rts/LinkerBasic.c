/* -----------------------------------------------------------------------------
 * $Id: LinkerBasic.c,v 1.1 2001/02/11 17:51:07 simonmar Exp $
 *
 * (c) The GHC Team, 2000
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "StoragePriv.h"
#include "LinkerInternals.h"

/* List of currently loaded objects */
ObjectCode *objects = NULL;	/* initially empty */

/* -----------------------------------------------------------------------------
 * Look up an address to discover whether it is in text or data space.
 *
 * Used by the garbage collector when walking the stack.
 * -------------------------------------------------------------------------- */

static __inline__ SectionKind
lookupSection ( void* addr )
{
   int          i;
   ObjectCode*  oc;
   
   for ( oc = objects; oc; oc = oc->next ) {
       for (i = 0; i < oc->n_sections; i++) {
	   if (oc->sections[i].start <= addr 
	       && addr <= oc->sections[i].end)
	       return oc->sections[i].kind;
       }
   }
   return SECTIONKIND_OTHER;
}

int
is_dynamically_loaded_code_or_rodata_ptr ( void* p )
{
   SectionKind sk = lookupSection(p);
   ASSERT (sk != SECTIONKIND_NOINFOAVAIL);
   return (sk == SECTIONKIND_CODE_OR_RODATA);
}


int
is_dynamically_loaded_rwdata_ptr ( void* p )
{
   SectionKind sk = lookupSection(p);
   ASSERT (sk != SECTIONKIND_NOINFOAVAIL);
   return (sk == SECTIONKIND_RWDATA);
}


int
is_not_dynamically_loaded_ptr ( void* p )
{
   SectionKind sk = lookupSection(p);
   ASSERT (sk != SECTIONKIND_NOINFOAVAIL);
   return (sk == SECTIONKIND_OTHER);
}
