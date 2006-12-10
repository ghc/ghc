/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

#ifndef LINKER_H
#define LINKER_H

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

/* resolve all the currently unlinked objects in memory */
HsInt resolveObjs( void );

/* load a dynamic library */
char *addDLL( char* dll_name );

/* lookup an address in the datacon tbl */
char *lookupDataCon( StgWord addr);

extern void markRootPtrTable(void (*)(StgClosure **));

#endif /* LINKER_H */
