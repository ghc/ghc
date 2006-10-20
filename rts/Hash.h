/*-----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1999
 *
 * Prototypes for Hash.c
 *
 * -------------------------------------------------------------------------- */

#ifndef HASH_H
#define HASH_H

typedef struct hashtable HashTable; /* abstract */

/* Hash table access where the keys are StgWords */
HashTable * allocHashTable    ( void );
void *      lookupHashTable ( HashTable *table, StgWord key );
void        insertHashTable ( HashTable *table, StgWord key, void *data );
void *      removeHashTable ( HashTable *table, StgWord key, void *data );

/* Hash table access where the keys are C strings (the strings are
 * assumed to be allocated by the caller, and mustn't be deallocated
 * until the corresponding hash table entry has been removed).
 */
HashTable * allocStrHashTable ( void );

#define lookupStrHashTable(table, key)  \
   (lookupHashTable(table, (StgWord)key))

#define insertStrHashTable(table, key, data)  \
   (insertHashTable(table, (StgWord)key, data))

#define removeStrHashTable(table, key, data) \
   (removeHashTable(table, (StgWord)key, data))

/* Freeing hash tables 
 */
void freeHashTable ( HashTable *table, void (*freeDataFun)(void *) );

void exitHashTable ( void );

#endif /* HASH_H */

