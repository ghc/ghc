/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001
 * Author: Sungwoo Park
 *
 * Retainer set interface for retainer profiling.
 *
 * ---------------------------------------------------------------------------*/

#ifndef RETAINERSET_H
#define RETAINERSET_H

#include <stdio.h>

#ifdef PROFILING

/*
  Type 'retainer' defines the retainer identity.

  Invariant:
    1. The retainer identity of a given retainer cannot change during 
    program execution, no matter where it is actually stored.
    For instance, the memory address of a retainer cannot be used as
    its retainer identity because its location may change during garbage
    collections.
    2. Type 'retainer' must come with comparison operations as well as
    an equality operation. That it, <, >, and == must be supported -
    this is necessary to store retainers in a sorted order in retainer sets.
    Therefore, you cannot use a huge structure type as 'retainer', for instance.

  We illustrate three possibilities of defining 'retainer identity'.
  Choose one of the following three compiler directives:

   Retainer scheme 1 (RETAINER_SCHEME_INFO) : retainer = info table
   Retainer scheme 2 (RETAINER_SCHEME_CCS)  : retainer = cost centre stack
   Retainer scheme 3 (RETAINER_SCHEME_CC)   : retainer = cost centre
*/

// #define RETAINER_SCHEME_INFO
#define RETAINER_SCHEME_CCS
// #define RETAINER_SCHEME_CC

#ifdef RETAINER_SCHEME_INFO
struct _StgInfoTable;
typedef struct _StgInfoTable *retainer;
#endif

#ifdef RETAINER_SCHEME_CCS
typedef CostCentreStack *retainer;
#endif

#ifdef RETAINER_SCHEME_CC
typedef CostCentre *retainer;
#endif

/*
  Type 'retainerSet' defines an abstract datatype for sets of retainers.  

  Invariants:
    A retainer set stores its elements in increasing order (in element[] array).
 */

typedef struct _RetainerSet {
  nat num;                      // number of elements
  StgWord hashKey;              // hash key for this retainer set
  struct _RetainerSet *link;    // link to the next retainer set in the bucket
  int id;   // unique id of this retainer set (used when printing)
            // Its absolute value is interpreted as its true id; if id is
            // negative, it indicates that this retainer set has had a postive
            // cost after some retainer profiling.
  retainer element[0];          // elements of this retainer set
  // do not put anything below here!
} RetainerSet;

/*
  Note: 
    There are two ways of maintaining all retainer sets. The first is simply by
    freeing all the retainer sets and re-initialize the hash table at each
    retainer profiling. The second is by setting the cost field of each 
    retainer set. The second is preferred to the first if most retainer sets 
    are likely to be observed again during the next retainer profiling. Note 
    that in the first approach, we do not free the memory allocated for 
    retainer sets; we just invalidate all retainer sets.
 */
#ifdef DEBUG_RETAINER
// In thise case, FIRST_APPROACH must be turned on because the memory pool
// for retainer sets is freed each time.
#define FIRST_APPROACH
#else
// #define FIRST_APPROACH
#define SECOND_APPROACH
#endif

// Creates the first pool and initializes a hash table. Frees all pools if any.
void initializeAllRetainerSet(void);

// Refreshes all pools for reuse and initializes a hash table.
void refreshAllRetainerSet(void);

// Frees all pools.
void closeAllRetainerSet(void);

// Finds or creates if needed a singleton retainer set.
RetainerSet *singleton(retainer r);

extern RetainerSet rs_MANY;

// Checks if a given retainer is a memeber of the retainer set.
// 
// Note & (maybe) Todo:
//   This function needs to be declared as an inline function, so it is declared
//   as an inline static function here.
//   This make the interface really bad, but isMember() returns a value, so
//   it is not easy either to write it as a macro (due to my lack of C 
//   programming experience). Sungwoo
//
// rtsBool isMember(retainer, retainerSet *);
/*
  Returns rtsTrue if r is a member of *rs.
  Invariants:
    rs is not NULL.
  Note:
    The efficiency of this function is subject to the typical size of
    retainer sets. If it is small, linear scan is better. If it
    is large in most cases, binary scan is better. 
    The current implementation mixes the two search strategies.
 */

#define BINARY_SEARCH_THRESHOLD   8
INLINE_HEADER rtsBool
isMember(retainer r, RetainerSet *rs)
{
  int i, left, right;       // must be int, not nat (because -1 can appear)
  retainer ri;

  if (rs == &rs_MANY) { return rtsTrue; }

  if (rs->num < BINARY_SEARCH_THRESHOLD) {
    for (i = 0; i < (int)rs->num; i++) {
      ri = rs->element[i];
      if (r == ri) return rtsTrue;
      else if (r < ri) return rtsFalse;
    }
  } else {
    left = 0;
    right = rs->num - 1;
    while (left <= right) {
      i = (left + right) / 2;
      ri = rs->element[i];
      if (r == ri) return rtsTrue;
      else if (r < ri) right = i - 1;
      else left = i + 1;
    }
  }
  return rtsFalse;
}

// Finds or creates a retainer set augmented with a new retainer.
RetainerSet *addElement(retainer, RetainerSet *);

// Call f() for each retainer set.
void traverseAllRetainerSet(void (*f)(RetainerSet *));

#ifdef SECOND_APPROACH
// Prints a single retainer set.
void printRetainerSetShort(FILE *, RetainerSet *);
#endif

// Print the statistics on all the retainer sets.
// store the sum of all costs and the number of all retainer sets. 
void outputRetainerSet(FILE *, nat *, nat *);

#ifdef SECOND_APPROACH
// Print all retainer sets at the exit of the program.
void outputAllRetainerSet(FILE *);
#endif

// Hashing functions
/*
  Invariants:
    Once either initializeAllRetainerSet() or refreshAllRetainerSet()
    is called, there exists only one copy of any retainer set created
    through singleton() and addElement().  The pool (the storage for
    retainer sets) is consumed linearly.  All the retainer sets of the
    same hash function value are linked together from an element in
    hashTable[].  See the invariants of allocateInPool() for the
    maximum size of retainer sets.  The hashing function is defined by
    hashKeySingleton() and hashKeyAddElement(). The hash key for a set
    must be unique regardless of the order its elements are inserted,
    i.e., the hashing function must be additive(?).
*/
#define hashKeySingleton(r)       ((StgWord)(r))
#define hashKeyAddElement(r, s)   (hashKeySingleton((r)) + (s)->hashKey)

// Prints the full information on a given retainer.
// Note: This function is not part of retainerSet interface, but this is
//       the best place to define it.
void printRetainer(FILE *, retainer);

#endif /* PROFILING */
#endif /* RETAINERSET_H */
