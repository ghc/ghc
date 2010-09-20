/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2009
 *
 * RTS Object Linker
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_LINKER_H
#define RTS_LINKER_H

/* initialize the object linker */
void initLinker( void );

/* insert a stable symbol in the hash table */
void insertStableSymbol(char* obj_name, char* key, StgPtr data);

/* insert a symbol in the hash table */
void insertSymbol(char* obj_name, char* key, void* data);

/* lookup a symbol in the hash table */
void *lookupSymbol( char *lbl );

/* delete an object from the pool */
HsInt unloadObj( char *path );

/* add an obj (populate the global symbol table, but don't resolve yet) */
HsInt loadObj( char *path );

/* add an arch (populate the global symbol table, but don't resolve yet) */
HsInt loadArchive( char *path );

/* resolve all the currently unlinked objects in memory */
HsInt resolveObjs( void );

/* load a dynamic library */
const char *addDLL( char* dll_name );

#endif /* RTS_LINKER_H */
