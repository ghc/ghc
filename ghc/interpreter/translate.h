extern Void stgDefn       Args(( Name n, Int arity, Cell e ));

extern  Void   implementForeignImport Args((Name));
extern  Void   implementForeignExport Args((Name));
extern  Void   implementCfun          Args((Name, List));
extern  Void   implementConToTag Args((Tycon));
extern  Void   implementTagToCon Args((Tycon));
extern  Void   implementPrim     Args((Name));
extern  Void   implementTuple    Args((Int));
#if TREX			 
extern  Name   implementRecShw   Args((Text));
extern  Name   implementRecEq    Args((Text));
#endif

/* Association list storing globals assigned to dictionaries, tuples, etc */
extern List stgGlobals;


