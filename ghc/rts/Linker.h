/* -----------------------------------------------------------------------------
 * $Id: Linker.h,v 1.4 2001/02/14 11:02:36 sewardj Exp $
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
char *addDLL( char *path );
