/*-----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1999
 *
 * Prototypes for Hash.c
 *
 * -------------------------------------------------------------------------- */

#pragma once

#include "BeginPrivate.h"

typedef struct hashtable HashTable; /* abstract */

/* Hash table access where the keys are StgWords.
 * Values are passed into the hash table and stored as `const void *` values,
 * but when the value is looked up or removed, the value is returned without the
 * `const` so that calling function can mutate what the pointer points to if it
 * needs to.
 */
HashTable * allocHashTable    ( void );
void        insertHashTable ( HashTable *table, StgWord key, const void *data );
void *      lookupHashTable ( const HashTable *table, StgWord key );
void *      removeHashTable ( HashTable *table, StgWord key, const void *data );

int keyCountHashTable (HashTable *table);

// Puts up to szKeys keys of the hash table into the given array. Returns the
// actual amount of keys that have been retrieved.
//
// If the table is modified concurrently, the function behavior is undefined.
//
int keysHashTable(HashTable *table, StgWord keys[], int szKeys);

typedef void (*MapHashFn)(void *data, StgWord key, const void *value);

void mapHashTable(HashTable *table, void *data, MapHashFn fn);

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
int hashStr(const HashTable *table, StgWord key);

/* Freeing hash tables
 */
void freeHashTable ( HashTable *table, void (*freeDataFun)(void *) );

void exitHashTable ( void );

#include "EndPrivate.h"
