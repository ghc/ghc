extern  Type   typeCheckExp     Args((Bool));
extern  Void   typeCheckDefns   Args((Void));
extern  Cell   provePred        Args((Kinds,List,Cell));
extern  List   simpleContext    Args((List,Int));
extern  Cell   rhsExpr          Args((Cell));
extern  Int    rhsLine          Args((Cell));
extern  List   offsetTyvarsIn   Args((Type,List));
extern  Type   primType         Args((Int/*AsmMonad*/,String,String));
extern  Type   conToTagType     Args((Tycon));
extern  Type   tagToConType     Args((Tycon));
extern  Void   mkTypes          Args((Void));

