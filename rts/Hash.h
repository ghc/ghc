/*-----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1999
 *
 * Prototypes for Hash.c
 *
 * -------------------------------------------------------------------------- */

#ifndef HASH_H
#define HASH_H

#include "BeginPrivate.h"

typedef struct hashtable HashTable; /* abstract */

/* Hash table access where the keys are StgWords */
HashTable * allocHashTable    ( void );
void *      lookupHashTable ( const HashTable *table, StgWord key );
void        insertHashTable ( HashTable *table, StgWord key, void *data );
void *      removeHashTable ( HashTable *table, StgWord key, void *data );

int keyCountHashTable (HashTable *table);

// Puts up to szKeys keys of the hash table into the given array. Returns the
// actual amount of keys that have been retrieved.
//
// If the table is modified concurrently, the function behavior is undefined.
//
int keysHashTable(HashTable *table, StgWord keys[], int szKeys);

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

/* Hash tables for arbitrary keys */
typedef int HashFunction(const HashTable *table, StgWord key);
typedef int CompareFunction(StgWord key1, StgWord key2);
HashTable * allocHashTable_(HashFunction *hash, CompareFunction *compare);
int hashWord(const HashTable *table, StgWord key);
int hashStr(const HashTable *table, char *key);

/* Freeing hash tables
 */
void freeHashTable ( HashTable *table, void (*freeDataFun)(void *) );

void exitHashTable ( void );

#include "EndPrivate.h"

#endif /* HASH_H */
