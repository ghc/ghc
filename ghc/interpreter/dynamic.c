
/* --------------------------------------------------------------------------
 * Dynamic loading (of .dll or .so files) for Hugs
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: dynamic.c,v $
 * $Revision: 1.12 $
 * $Date: 1999/10/29 13:41:23 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "errors.h"
#include "dynamic.h"

#if HAVE_WINDOWS_H && !defined(__MSDOS__)

#include <windows.h>

void* getDLLSymbol(line,dll0,symbol0) /* load dll and lookup symbol */
Int    line;
String dll0;
String symbol0; {
    void*      sym;
    char       dll[1000];
    char       symbol[100];
    ObjectFile instance;

    if (strlen(dll0) > 996) {
       ERRMSG(line) "Excessively long library name:\n%s\n",dll0
       EEND;
    }
    strcpy(dll,dll0);
    strcat(dll, ".dll");

    if (strlen(symbol0) > 96) {
       ERRMSG(line) "Excessively long symbol name:\n%s\n",symbol0
       EEND;
    }
    strcpy(&(symbol[1]),symbol0); 
    symbol[0] = '_';

    instance = LoadLibrary(dll);
    if (NULL == instance) {
        /* GetLastError allegedly provides more detail - in practice,
	 * it tells you nothing more.
         */
        ERRMSG(line) "Can't open library \"%s\"", dll
        EEND;
    }
    sym = GetProcAddress(instance,symbol0);
    return sym;
}

Bool stdcallAllowed ( void )
{
   return TRUE;
}






#elif HAVE_DLFCN_H /* eg LINUX, SOLARIS, ULTRIX */

#include <stdio.h>
#include <dlfcn.h>

void* getDLLSymbol(line,dll0,symbol)  /* load dll and lookup symbol */
Int    line;
String dll0;
String symbol; {
    void*      sym;
    char       dll[1000];
    ObjectFile instance;
    if (strlen(dll0) > 996) {
       ERRMSG(line) "Excessively long library name:\n%s\n",dll0
       EEND;
    }
    strcpy(dll,dll0);
    strcat(dll, ".so");
#ifdef RTLD_NOW
    instance = dlopen(dll,RTLD_NOW);
#elif defined RTLD_LAZY /* eg SunOS4 doesn't have RTLD_NOW */
    instance = dlopen(dll,RTLD_LAZY);
#else /* eg FreeBSD doesn't have RTLD_LAZY */
    instance = dlopen(dll,1);
#endif

    if (NULL == instance) {
	ERRMSG(line) "Can't open library \"%s\":\n      %s\n",dll,dlerror()
        EEND;
    }
    if ((sym = dlsym(instance,symbol)))
        return sym;

    ERRMSG(line) "Can't find symbol \"%s\" in library \"%s\"",symbol,dll
    EEND;
}

Bool stdcallAllowed ( void )
{
   return FALSE;
}






#elif HAVE_DL_H /* eg HPUX */

#include <dl.h>

void* getDLLSymbol(line,dll0,symbol)  /* load dll and lookup symbol */
Int    line;
String dll0;
String symbol; {
    ObjectFile instance = shl_load(dll,BIND_IMMEDIATE,0L);
    void* r;
    if (NULL == instance) {
        ERRMSG(line) "Error while importing DLL \"%s\"", dll0
        EEND;
    }
    return (0 == shl_findsym(&instance,symbol,TYPE_PROCEDURE,&r)) ? r : 0;
}

Bool stdcallAllowed ( void )
{
   return FALSE;
}






#else /* Dynamic loading not available */

void* getDLLSymbol(line,dll0,symbol)  /* load dll and lookup symbol */
Int    line;
String dll0;
String symbol; {
#if 1 /* very little to choose between these options */
    return 0;
#else
    ERRMSG(line) "This Hugs build does not support dynamic loading\n"
    EEND;
#endif
}

Bool stdcallAllowed ( void )
{
   return FALSE;
}

#endif /* Dynamic loading not available */

