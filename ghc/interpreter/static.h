extern List  unqualImports;             /* unqualified import list         */

#if DERIVE_SHOW | DERIVE_READ
extern  List   cfunSfuns;
#endif
extern  Void   startModule      Args((Cell));
extern  Void   setExportList    Args((List));
extern  Void   setExports       Args((List));
extern  Void   addQualImport    Args((Text,Text));
extern  Void   addUnqualImport  Args((Text,List));
extern  Void   tyconDefn        Args((Int,Cell,Cell,Cell));
extern  Void   setTypeIns       Args((List));
extern  Void   clearTypeIns     Args((Void));
extern  Type   fullExpand       Args((Type));
extern  Bool   isAmbiguous      Args((Type));
extern  Void   ambigError       Args((Int,String,Cell,Type));
extern  Void   classDefn        Args((Int,Cell,Cell));
extern  Void   instDefn         Args((Int,Cell,Cell));
extern  Void   addTupInst       Args((Class,Int));
#if TREX
extern  Inst   addRecShowInst   Args((Class,Ext));
extern  Inst   addRecEqInst     Args((Class,Ext));
#endif
extern  Void   addEvalInst      Args((Int,Cell,Int,List));
extern  Void   foreignImport	Args((Cell,Pair,Cell,Cell));
extern  Void   foreignExport	Args((Cell,Cell,Cell,Cell));
extern  Void   defaultDefn      Args((Int,List));
extern  Void   checkExp         Args((Void));
extern  Void   checkDefns       Args((Void));

