/* -*- mode: hugs-c; -*- */

extern Void loadInterface  Args((String));

extern Void openGHCIface   Args((Text));
extern Void loadSharedLib  Args((String));
extern Void addGHCImport   Args((Int,Text,String));
extern Void addGHCVar      Args((Int,Text,Type));
extern Void addGHCSynonym  Args((Int,Cell,List,Type));
extern Void addGHCDataDecl Args((Int,Cell,List,List,List));
extern Void addGHCNewType  Args((Int,Cell,List,Cell));
extern Void addGHCClass    Args((Int,List,Cell,List,List));
extern Void addGHCInstance Args((Int,Cell,Pair,Text));

