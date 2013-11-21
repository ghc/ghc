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

/* Initialize the object linker. Equivalent to initLinker_(1). */
void initLinker (void);

/* Initialize the object linker.
 * The retain_cafs argument is:
 *
 *   non-zero => Retain CAFs unconditionally in linked Haskell code.
 *               Note that this prevents any code from being unloaded.
 *               It should not be necessary unless you are GHCi or
 *               hs-plugins, which needs to be able call any function
 *               in the compiled code.
 *
 *   zero     => Do not retain CAFs.  Everything reachable from foreign
 *               exports will be retained, due to the StablePtrs
 *               created by the module initialisation code.  unloadObj
 *               free these StablePtrs, which will allow the CAFs to
 *               be GC'd and the code to be removed.
 */
void initLinker_ (int retain_cafs);

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
