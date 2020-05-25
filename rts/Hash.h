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
typedef struct strhashtable StrHashTable;

/* Hash table access where the keys are StgWords.
 * Values are passed into the hash table and stored as `const void *` values,
 * but when the value is looked up or removed, the value is returned without the
 * `const` so that calling function can mutate what the pointer points to if it
 * needs to.
 */
HashTable * allocHashTable  ( void );
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
typedef void (*MapHashFnKeys)(void *data, StgWord *key, const void *value);
// Return true -> continue; false -> stop
typedef bool (*IterHashFn)(void *data, StgWord key, const void *value);

void mapHashTable(HashTable *table, void *data, MapHashFn fn);
void mapHashTableKeys(HashTable *table, void *data, MapHashFnKeys fn);
void iterHashTable(HashTable *table, void *data, IterHashFn);

/* Hash table access where the keys are C strings (the strings are
 * assumed to be allocated by the caller, and mustn't be deallocated
 * until the corresponding hash table entry has been removed).
 */
INLINE_HEADER StrHashTable * allocStrHashTable ( void )
{
    return (StrHashTable*) allocHashTable();
}

void        insertStrHashTable ( StrHashTable *table, const char * key,
                                 const void *data );
void *      lookupStrHashTable ( const StrHashTable *table, const char * key);
void *      removeStrHashTable ( StrHashTable *table, const char * key,
                                 const void *data );

/*
 * Hash tables for arbitrary key types.
 * Generally, these functions allow for the specification of the
 * HashFunction and CompareFunction. It's recommended that those
 * are inlinable so there's a chance the compiler can discard
 * some parameter-passing, as well as function calls, though note
 * it's not guaranteed. Either way, the functions are parameters
 * as the types should be statically known and thus
 * storing them is unnecessary.
 */
typedef int HashFunction(const HashTable *table, StgWord key);
typedef int CompareFunction(StgWord key1, StgWord key2);
int hashWord(const HashTable *table, StgWord key);
int hashStr(const HashTable *table, StgWord w);
void        insertHashTable_ ( HashTable *table, StgWord key,
                               const void *data, HashFunction f );
void *      lookupHashTable_ ( const HashTable *table, StgWord key,
                               HashFunction f, CompareFunction cmp );
void *      removeHashTable_ ( HashTable *table, StgWord key,
                               const void *data, HashFunction f,
                               CompareFunction cmp );

/* Freeing hash tables
 */
void freeHashTable ( HashTable *table, void (*freeDataFun)(void *) );
#define freeStrHashTable(table, f) \
    (freeHashTable((HashTable*) table, f))

void exitHashTable ( void );

#include "EndPrivate.h"
