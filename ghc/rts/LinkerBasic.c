/* -----------------------------------------------------------------------------
 * $Id: LinkerBasic.c,v 1.4 2001/09/04 16:33:04 sewardj Exp $
 *
 * (c) The GHC Team, 2000
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "Hash.h"
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
   Section*    se;
   ObjectCode* oc;
   
   for (oc=objects; oc; oc=oc->next) {
       for (se=oc->sections; se; se=se->next) {
	   if (se->start <= addr && addr <= se->end)
	       return se->kind;
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
