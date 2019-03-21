/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2009
 *
 * RTS Object Linker
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

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
 *               frees these StablePtrs, which will allow the CAFs to
 *               be GC'd and the code to be removed.
 */
void initLinker_ (int retain_cafs);

/* insert a symbol in the hash table */
HsInt insertSymbol(pathchar* obj_name, char* key, void* data);

/* lookup a symbol in the hash table */
void *lookupSymbol( char *lbl );

/* See Linker.c Note [runtime-linker-phases] */
typedef enum {
    OBJECT_LOADED,
    OBJECT_NEEDED,
    OBJECT_RESOLVED,
    OBJECT_UNLOADED,
    OBJECT_DONT_RESOLVE,
    OBJECT_NOT_LOADED     /* The object was either never loaded or has been
                             fully unloaded */
} OStatus;

/* check object load status */
OStatus getObjectLoadStatus( pathchar *path );

/* delete an object from the pool */
HsInt unloadObj( pathchar *path );

/* purge an object's symbols from the symbol table, but don't unload it */
HsInt purgeObj( pathchar *path );

/* add an obj (populate the global symbol table, but don't resolve yet) */
HsInt loadObj( pathchar *path );

/* add an arch (populate the global symbol table, but don't resolve yet) */
HsInt loadArchive( pathchar *path );

/* resolve all the currently unlinked objects in memory */
HsInt resolveObjs( void );

/* load a dynamic library */
const char *addDLL( pathchar* dll_name );

/* add a path to the library search path */
HsPtr addLibrarySearchPath(pathchar* dll_path);

/* removes a directory from the search path,
   path must have been added using addLibrarySearchPath */
HsBool removeLibrarySearchPath(HsPtr dll_path_index);

/* give a warning about missing Windows patches that would make
   the linker work better */
void warnMissingKBLibraryPaths( void );

/* -----------------------------------------------------------------------------
* Searches the system directories to determine if there is a system DLL that
* satisfies the given name. This prevent GHCi from linking against a static
* library if a DLL is available.
*/
pathchar* findSystemLibrary(pathchar* dll_name);

/* called by the initialization code for a module, not a user API */
StgStablePtr foreignExportStablePtr (StgPtr p);
