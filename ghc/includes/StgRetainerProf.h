/* -----------------------------------------------------------------------------
 * $Id: StgRetainerProf.h,v 1.1 2001/11/22 14:25:12 simonmar Exp $
 *
 * (c) The GHC Team, 2001
 *
 * Retainer profiling
 * ---------------------------------------------------------------------------*/

#ifndef STGRETAINERPROF_H
#define STGRETAINERPROF_H

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
  nat cost;                     // cost associated with this retainer set
  StgWord hashKey;              // hash key for this retainer set
  struct _RetainerSet *link;    // link to the next retainer set in the bucket
  int id;   // unique id of this retainer set (used when printing)
            // Its absolute value is interpreted as its true id; if id is
            // negative, it indicates that this retainer set has had a postive
            // cost after some retainer profiling.
  retainer element[0];          // elements of this retainer set
  // do not put anything below here!
} RetainerSet;

//
// retainerSet - interface: see rts/RetainerSet.h
//

#endif /* STGRETAINERPROF_H */
