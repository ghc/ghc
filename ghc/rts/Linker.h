/* -----------------------------------------------------------------------------
 * $Id: Linker.h,v 1.1 2000/10/06 15:33:27 simonmar Exp $
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

/* These three are used by the garbage collector (see ClosureMacros.h,
   IS_CODE_PTR etc.). */
int is_dynamically_loaded_code_or_rodata_ptr ( char* p );
int is_dynamically_loaded_rwdata_ptr ( char* p );
int is_not_dynamically_loaded_ptr ( char* p );
