/*-----------------------------------------------------------------------------
 * $Id: Hash.h,v 1.1 1999/01/27 12:11:26 simonm Exp $
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
