/* -----------------------------------------------------------------------------
 * $Id: Linker.h,v 1.3 2001/02/13 18:01:22 simonmar Exp $
 *
 * (c) The GHC Team, 2000
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

/* initialize the object linker */
void initLinker( void );

/* lookup a symbol in the hash table */
void *lookupSymbol( char *lbl );

/* delete an object from the pool */
HsInt unloadObj( char *path );

/* add an obj (populate the global symbol table, but don't resolve yet) */
HsInt loadObj( char *path );

/* resolve all the currently unlinked objects in memory */
HsInt resolveObjs( void );

/* load a dynamic library */
HsInt addDLL( char *path );
