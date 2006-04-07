/*
 * C callable bridge to the .NET object model
 *
 * (c) 2003, sof.
 *
 */
#ifndef __DNINVOKE_H__
#define __DNINVOKE_H__
#include "Dotnet.h"

extern char* DN_invokeStatic ( char       *assemName,
			       char       *methName,
			       DotnetArg  *args,
			       int        n_args,
			       DotnetType resultTy,
			       void       *res);
extern char* DN_getStatic ( char       *assemName,
			    char       *fieldClsName,
			    DotnetArg  *args,
			    int        n_args,
			    DotnetType resultTy,
			    void       *res);
extern char* DN_setStatic ( char       *assemName,
			    char       *fieldClsName,
			    DotnetArg  *args,
			    int        n_args,
			    DotnetType resultTy,
			    void       *res);
extern char* DN_createObject ( char       *assemName,
			       char       *methName,
			       DotnetArg  *args,
			       int        n_args,
			       DotnetType resultTy,
			       void       *res);

extern char* DN_invokeMethod ( char       *methName,
			       DotnetArg  *args,
			       int        n_args,
			       DotnetType resultTy,
			       void       *res);

extern char* DN_getField ( char       *methName,
			   DotnetArg  *args,
			   int        n_args,
			   DotnetType resultTy,
			   void       *res);
extern char* DN_setField ( char       *clsAndMethName,
			   DotnetArg  *args,
			   int        n_args,
			   DotnetType resultTy,
			   void       *res);

extern void stopDotnetBridge(void);

#endif /* __DNINVOKE_H__ */
