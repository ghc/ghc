/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001
 * Author: Sungwoo Park
 *
 * Retainer set implementation for retainer profiling (see RetainerProfile.c)
 *
 * ---------------------------------------------------------------------------*/

#ifdef PROFILING

#include "Rts.h"
#include "RtsFlags.h"
#include "Stats.h"
#include "RtsUtils.h"
#include "RetainerSet.h"
#include "Arena.h"
#include "Profiling.h"

#include <stdlib.h>
#include <string.h>

#define HASH_TABLE_SIZE 255
#define hash(hk)  (hk % HASH_TABLE_SIZE)
static RetainerSet *hashTable[HASH_TABLE_SIZE];

static Arena *arena;		// arena in which we store retainer sets

static int nextId;              // id of next retainer set       

/* -----------------------------------------------------------------------------
 * rs_MANY is a distinguished retainer set, such that
 *
 *        isMember(e, rs_MANY)   = True
 *
 *	  addElement(e, rs)      = rs_MANY,   if rs->num >= maxRetainerSetSize
 *	  addElement(e, rs_MANY) = rs_MANY
 *
 * The point of rs_MANY is to keep the total number of retainer sets
 * from growing too large.
 * -------------------------------------------------------------------------- */
RetainerSet rs_MANY = {
    num : 0,
    hashKey : 0,
    link : NULL,
    id : 1,
    element : {}
};

/* -----------------------------------------------------------------------------
 * calculate the size of a RetainerSet structure
 * -------------------------------------------------------------------------- */
STATIC_INLINE size_t
sizeofRetainerSet( int elems )
{
    return (sizeof(RetainerSet) + elems * sizeof(retainer));
}

/* -----------------------------------------------------------------------------
 * Creates the first pool and initializes hashTable[].
 * Frees all pools if any.
 * -------------------------------------------------------------------------- */
void
initializeAllRetainerSet(void)
{
    int i;

    arena = newArena();

    for (i = 0; i < HASH_TABLE_SIZE; i++)
	hashTable[i] = NULL;
    nextId = 2;   // Initial value must be positive, 2 is MANY.
}

/* -----------------------------------------------------------------------------
 * Refreshes all pools for reuse and initializes hashTable[].
 * -------------------------------------------------------------------------- */
void
refreshAllRetainerSet(void)
{
#ifdef FIRST_APPROACH
    int i;

    // first approach: completely refresh
    arenaFree(arena);
    arena = newArena();

    for (i = 0; i < HASH_TABLE_SIZE; i++)
	hashTable[i] = NULL;
    nextId = 2;
#endif // FIRST_APPROACH
}

/* -----------------------------------------------------------------------------
 * Frees all pools.
 * -------------------------------------------------------------------------- */
void
closeAllRetainerSet(void)
{
    arenaFree(arena);
}

/* -----------------------------------------------------------------------------
 *  Finds or creates if needed a singleton retainer set.
 * -------------------------------------------------------------------------- */
RetainerSet *
singleton(retainer r)
{
    RetainerSet *rs;
    StgWord hk;

    hk = hashKeySingleton(r);
    for (rs = hashTable[hash(hk)]; rs != NULL; rs = rs->link)
	if (rs->num == 1 &&  rs->element[0] == r) return rs;    // found it

    // create it
    rs = arenaAlloc( arena, sizeofRetainerSet(1) );
    rs->num = 1;
    rs->hashKey = hk;
    rs->link = hashTable[hash(hk)];
    rs->id = nextId++;
    rs->element[0] = r;

    // The new retainer set is placed at the head of the linked list.
    hashTable[hash(hk)] = rs;

    return rs;
}

/* -----------------------------------------------------------------------------
 *   Finds or creates a retainer set *rs augmented with r.
 *   Invariants:
 *     r is not a member of rs, i.e., isMember(r, rs) returns rtsFalse.
 *     rs is not NULL.
 *   Note:
 *     We could check if rs is NULL, in which case this function call
 *     reverts to singleton(). We do not choose this strategy because
 *     in most cases addElement() is invoked with non-NULL rs.
 * -------------------------------------------------------------------------- */
RetainerSet *
addElement(retainer r, RetainerSet *rs)
{
    nat i;
    nat nl;             // Number of retainers in *rs Less than r
    RetainerSet *nrs;   // New Retainer Set
    StgWord hk;         // Hash Key

#ifdef DEBUG_RETAINER
    // debugBelch("addElement(%p, %p) = ", r, rs);
#endif

    ASSERT(rs != NULL);
    ASSERT(rs->num <= RtsFlags.ProfFlags.maxRetainerSetSize);

    if (rs == &rs_MANY || rs->num == RtsFlags.ProfFlags.maxRetainerSetSize) {
	return &rs_MANY;
    }

    ASSERT(!isMember(r, rs));

    for (nl = 0; nl < rs->num; nl++)
	if (r < rs->element[nl]) break;
    // Now nl is the index for r into the new set.
    // Also it denotes the number of retainers less than r in *rs.
    // Thus, compare the first nl retainers, then r itself, and finally the
    // remaining (rs->num - nl) retainers.

    hk = hashKeyAddElement(r, rs);
    for (nrs = hashTable[hash(hk)]; nrs != NULL; nrs = nrs->link) {
	// test *rs and *nrs for equality

	// check their size
	if (rs->num + 1 != nrs->num) continue;

	// compare the first nl retainers and find the first non-matching one.
	for (i = 0; i < nl; i++)
	    if (rs->element[i] != nrs->element[i]) break;
	if (i < nl) continue;

	// compare r itself
	if (r != nrs->element[i]) continue;       // i == nl

	// compare the remaining retainers
	for (; i < rs->num; i++)
	    if (rs->element[i] != nrs->element[i + 1]) break;
	if (i < rs->num) continue;

#ifdef DEBUG_RETAINER
	// debugBelch("%p\n", nrs);
#endif
	// The set we are seeking already exists!
	return nrs;
    }

    // create a new retainer set
    nrs = arenaAlloc( arena, sizeofRetainerSet(rs->num + 1) );
    nrs->num = rs->num + 1;
    nrs->hashKey = hk;
    nrs->link = hashTable[hash(hk)];
    nrs->id = nextId++;
    for (i = 0; i < nl; i++) {              // copy the first nl retainers
	nrs->element[i] = rs->element[i];
    }
    nrs->element[i] = r;                    // copy r
    for (; i < rs->num; i++) {              // copy the remaining retainers
	nrs->element[i + 1] = rs->element[i];
    }

    hashTable[hash(hk)] = nrs;

#ifdef DEBUG_RETAINER
    // debugBelch("%p\n", nrs);
#endif
    return nrs;
}

/* -----------------------------------------------------------------------------
 *  Call f() for each retainer set.
 * -------------------------------------------------------------------------- */
void
traverseAllRetainerSet(void (*f)(RetainerSet *))
{
    int i;
    RetainerSet *rs;

    (*f)(&rs_MANY);
    for (i = 0; i < HASH_TABLE_SIZE; i++)
	for (rs = hashTable[i]; rs != NULL; rs = rs->link)
	    (*f)(rs);
}


/* -----------------------------------------------------------------------------
 *  printRetainer() prints the full information on a given retainer,
 *  not a retainer set.
 * -------------------------------------------------------------------------- */
#if defined(RETAINER_SCHEME_INFO)
// Retainer scheme 1: retainer = info table
void
printRetainer(FILE *f, retainer itbl)
{
    fprintf(f, "%s[%s]", itbl->prof.closure_desc, itbl->prof.closure_type);
}
#elif defined(RETAINER_SCHEME_CCS)
// Retainer scheme 2: retainer = cost centre stack
void
printRetainer(FILE *f, retainer ccs)
{
    fprintCCS(f, ccs);
}
#elif defined(RETAINER_SCHEME_CC)
// Retainer scheme 3: retainer = cost centre
void
printRetainer(FILE *f, retainer cc)
{
    fprintf(f,"%s.%s", cc->module, cc->label);
}
#endif

/* -----------------------------------------------------------------------------
 *  printRetainerSetShort() should always display the same output for
 *  a given retainer set regardless of the time of invocation.
 * -------------------------------------------------------------------------- */
#ifdef SECOND_APPROACH
#if defined(RETAINER_SCHEME_INFO)
// Retainer scheme 1: retainer = info table
void
printRetainerSetShort(FILE *f, RetainerSet *rs)
{
#define MAX_RETAINER_SET_SPACE  24
    char tmp[MAX_RETAINER_SET_SPACE + 1];
    int size;
    nat j;

    ASSERT(rs->id < 0);

    tmp[MAX_RETAINER_SET_SPACE] = '\0';

    // No blank characters are allowed.
    sprintf(tmp + 0, "(%d)", -(rs->id));
    size = strlen(tmp);
    ASSERT(size < MAX_RETAINER_SET_SPACE);

    for (j = 0; j < rs->num; j++) {
	if (j < rs->num - 1) {
	    strncpy(tmp + size, rs->element[j]->prof.closure_desc, MAX_RETAINER_SET_SPACE - size);
	    size = strlen(tmp);
	    if (size == MAX_RETAINER_SET_SPACE)
		break;
	    strncpy(tmp + size, ",", MAX_RETAINER_SET_SPACE - size);
	    size = strlen(tmp);
	    if (size == MAX_RETAINER_SET_SPACE)
		break;
	}
	else {
	    strncpy(tmp + size, rs->element[j]->prof.closure_desc, MAX_RETAINER_SET_SPACE - size);
	    // size = strlen(tmp);
	}
    }
    fprintf(f, tmp);
}
#elif defined(RETAINER_SCHEME_CC)
// Retainer scheme 3: retainer = cost centre
void
printRetainerSetShort(FILE *f, RetainerSet *rs)
{
#define MAX_RETAINER_SET_SPACE  24
    char tmp[MAX_RETAINER_SET_SPACE + 1];
    int size;
    nat j;

}
#elif defined(RETAINER_SCHEME_CCS)
// Retainer scheme 2: retainer = cost centre stack
void
printRetainerSetShort(FILE *f, RetainerSet *rs)
{
#define MAX_RETAINER_SET_SPACE  24
    char tmp[MAX_RETAINER_SET_SPACE + 1];
    int size;
    nat j;

    ASSERT(rs->id < 0);

    tmp[MAX_RETAINER_SET_SPACE] = '\0';

    // No blank characters are allowed.
    sprintf(tmp + 0, "(%d)", -(rs->id));
    size = strlen(tmp);
    ASSERT(size < MAX_RETAINER_SET_SPACE);

    for (j = 0; j < rs->num; j++) {
	if (j < rs->num - 1) {
	    strncpy(tmp + size, rs->element[j]->cc->label, MAX_RETAINER_SET_SPACE - size);
	    size = strlen(tmp);
	    if (size == MAX_RETAINER_SET_SPACE)
		break;
	    strncpy(tmp + size, ",", MAX_RETAINER_SET_SPACE - size);
	    size = strlen(tmp);
	    if (size == MAX_RETAINER_SET_SPACE)
		break;
	}
	else {
	    strncpy(tmp + size, rs->element[j]->cc->label, MAX_RETAINER_SET_SPACE - size);
	    // size = strlen(tmp);
	}
    }
    fprintf(f, tmp);
}
#elif defined(RETAINER_SCHEME_CC)
// Retainer scheme 3: retainer = cost centre
static void
printRetainerSetShort(FILE *f, retainerSet *rs)
{
#define MAX_RETAINER_SET_SPACE  24
    char tmp[MAX_RETAINER_SET_SPACE + 1];
    int size;
    nat j;

    ASSERT(rs->id < 0);

    tmp[MAX_RETAINER_SET_SPACE] = '\0';

    // No blank characters are allowed.
    sprintf(tmp + 0, "(%d)", -(rs->id));
    size = strlen(tmp);
    ASSERT(size < MAX_RETAINER_SET_SPACE);

    for (j = 0; j < rs->num; j++) {
	if (j < rs->num - 1) {
	    strncpy(tmp + size, rs->element[j]->label,
		    MAX_RETAINER_SET_SPACE - size);
	    size = strlen(tmp);
	    if (size == MAX_RETAINER_SET_SPACE)
		break;
	    strncpy(tmp + size, ",", MAX_RETAINER_SET_SPACE - size);
	    size = strlen(tmp);
	    if (size == MAX_RETAINER_SET_SPACE)
		break;
	}
	else {
	    strncpy(tmp + size, rs->element[j]->label,
		    MAX_RETAINER_SET_SPACE - size);
	    // size = strlen(tmp);
	}
    }
    fprintf(f, tmp);
/*
  #define MAX_RETAINER_SET_SPACE  24
  #define DOT_NUMBER              3
  // 1. 32 > MAX_RETAINER_SET_SPACE + 1 (1 for '\0')
  // 2. (MAX_RETAINER_SET_SPACE - DOT_NUMBER ) characters should be enough for
  //    printing one natural number (plus '(' and ')').
  char tmp[32];
  int size, ts;
  nat j;

  ASSERT(rs->id < 0);

  // No blank characters are allowed.
  sprintf(tmp + 0, "(%d)", -(rs->id));
  size = strlen(tmp);
  ASSERT(size < MAX_RETAINER_SET_SPACE - DOT_NUMBER);

  for (j = 0; j < rs->num; j++) {
    ts = strlen(rs->element[j]->label);
    if (j < rs->num - 1) {
      if (size + ts + 1 > MAX_RETAINER_SET_SPACE - DOT_NUMBER) {
        sprintf(tmp + size, "...");
        break;
      }
      sprintf(tmp + size, "%s,", rs->element[j]->label);
      size += ts + 1;
    }
    else {
      if (size + ts > MAX_RETAINER_SET_SPACE - DOT_NUMBER) {
        sprintf(tmp + size, "...");
        break;
      }
      sprintf(tmp + size, "%s", rs->element[j]->label);
      size += ts;
    }
  }
  fprintf(f, tmp);
*/
}
#endif /* RETAINER_SCHEME_CC */
#endif /* SECOND_APPROACH */

/* -----------------------------------------------------------------------------
 * Dump the contents of each retainer set into the log file at the end
 * of the run, so the user can find out for a given retainer set ID
 * the full contents of that set.
 * --------------------------------------------------------------------------- */
#ifdef SECOND_APPROACH
void
outputAllRetainerSet(FILE *prof_file)
{
    nat i, j;
    nat numSet;
    RetainerSet *rs, **rsArray, *tmp;

    // find out the number of retainer sets which have had a non-zero cost at
    // least once during retainer profiling
    numSet = 0;
    for (i = 0; i < HASH_TABLE_SIZE; i++)
	for (rs = hashTable[i]; rs != NULL; rs = rs->link) {
	    if (rs->id < 0)
		numSet++;
	}

    if (numSet == 0)      // retainer profiling was not done at all.
	return;

    // allocate memory
    rsArray = stgMallocBytes(numSet * sizeof(RetainerSet *),
			     "outputAllRetainerSet()");

    // prepare for sorting
    j = 0;
    for (i = 0; i < HASH_TABLE_SIZE; i++)
	for (rs = hashTable[i]; rs != NULL; rs = rs->link) {
	    if (rs->id < 0) {
		rsArray[j] = rs;
		j++;
	    }
	}

    ASSERT(j == numSet);

    // sort rsArray[] according to the id of each retainer set
    for (i = numSet - 1; i > 0; i--) {
	for (j = 0; j <= i - 1; j++) {
	    // if (-(rsArray[j]->id) < -(rsArray[j + 1]->id))
	    if (rsArray[j]->id < rsArray[j + 1]->id) {
		tmp = rsArray[j];
		rsArray[j] = rsArray[j + 1];
		rsArray[j + 1] = tmp;
	    }
	}
    }

    fprintf(prof_file, "\nRetainer sets created during profiling:\n");
    for (i = 0;i < numSet; i++) {
	fprintf(prof_file, "SET %u = {", -(rsArray[i]->id));
	for (j = 0; j < rsArray[i]->num - 1; j++) {
	    printRetainer(prof_file, rsArray[i]->element[j]);
	    fprintf(prof_file, ", ");
	}
	printRetainer(prof_file, rsArray[i]->element[j]);
	fprintf(prof_file, "}\n");
    }

    stgFree(rsArray);
}
#endif // SECOND_APPROACH

#endif /* PROFILING */
