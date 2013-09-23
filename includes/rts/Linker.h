/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2009
 *
 * RTS Object Linker
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_LINKER_H
#define RTS_LINKER_H

#if defined(mingw32_HOST_OS)
typedef wchar_t pathchar;
#define PATH_FMT "ls"
#else
typedef char    pathchar;
#define PATH_FMT "s"
#endif

/* initialize the object linker */
void initLinker( void );

/* insert a stable symbol in the hash table */
void insertStableSymbol(pathchar* obj_name, char* key, StgPtr data);

/* insert a symbol in the hash table */
void insertSymbol(pathchar* obj_name, char* key, void* data);

/* lookup a symbol in the hash table */
void *lookupSymbol( char *lbl );

/* delete an object from the pool */
HsInt unloadObj( pathchar *path );

/* add an obj (populate the global symbol table, but don't resolve yet) */
HsInt loadObj( pathchar *path );

/* add an arch (populate the global symbol table, but don't resolve yet) */
HsInt loadArchive( pathchar *path );

/* resolve all the currently unlinked objects in memory */
HsInt resolveObjs( void );

/* load a dynamic library */
const char *addDLL( pathchar* dll_name );

/* called by the initialization code for a module, not a user API */
StgStablePtr foreignExportStablePtr (StgPtr p);

#endif /* RTS_LINKER_H */
