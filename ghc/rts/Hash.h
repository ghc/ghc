/*-----------------------------------------------------------------------------
 * $Id: Hash.h,v 1.2 2000/01/13 14:34:03 hwloidl Exp $
 *
 * (c) The GHC Team, 1999
 *
 * Prototypes for Hash.c
 *
 * -------------------------------------------------------------------------- */

typedef struct hashtable HashTable; /* abstract */

void *      lookupHashTable ( HashTable *table, StgWord key );
void        insertHashTable ( HashTable *table, StgWord key, void *data );
void *      removeHashTable ( HashTable *table, StgWord key, void *data );
void        freeHashTable   ( HashTable *table, void (*freeDataFun)(void *) );
HashTable * allocHashTable  ( void );

