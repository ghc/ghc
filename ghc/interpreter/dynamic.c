
/* --------------------------------------------------------------------------
 * Dynamic loading (of .dll or .so files) for Hugs
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: dynamic.c,v $
 * $Revision: 1.6 $
 * $Date: 1999/10/15 19:11:54 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "errors.h"
#include "dynamic.h"

#if HAVE_WINDOWS_H && !defined(__MSDOS__)

#include <windows.h>

ObjectFile loadLibrary(fn)
String fn; {
    return LoadLibrary(fn);
}

void* lookupSymbol(file,symbol)
ObjectFile file;
String symbol; {
    return GetProcAddress(file,symbol);
}

const char *dlerror(void)
{
   return "<unknown>";
}

void* getDLLSymbol(dll,symbol)  /* load dll and lookup symbol */
String dll;
String symbol; {
    ObjectFile instance = LoadLibrary(dll);
    if (NULL == instance) {
        /* GetLastError allegedly provides more detail - in practice,
	 * it tells you nothing more.
         */
        ERRMSG(0) "Error while importing DLL \"%s\"", dll
        EEND;
    }
    return GetProcAddress(instance,symbol);
}

#elif HAVE_DLFCN_H /* eg LINUX, SOLARIS, ULTRIX */

#include <stdio.h>
#include <dlfcn.h>

ObjectFile loadLibrary(fn)
String fn; {
    return dlopen(fn,RTLD_NOW | RTLD_GLOBAL);
}

void* lookupSymbol(file,symbol)
ObjectFile file;
String symbol; {
    return dlsym(file,symbol);
}

void* getDLLSymbol(dll,symbol)  /* load dll and lookup symbol */
String dll;
String symbol; {
#ifdef RTLD_NOW
    ObjectFile instance = dlopen(dll,RTLD_NOW);
#elif defined RTLD_LAZY /* eg SunOS4 doesn't have RTLD_NOW */
    ObjectFile instance = dlopen(dll,RTLD_LAZY);
#else /* eg FreeBSD doesn't have RTLD_LAZY */
    ObjectFile instance = dlopen(dll,1);
#endif
    if (NULL == instance) {
        ERRMSG(0) "Error %s while importing DLL \"%s\"", dlerror(), dll
        EEND;
    }
    return dlsym(instance,symbol);
}

#elif HAVE_DL_H /* eg HPUX */

#include <dl.h>

void* getDLLSymbol(dll,symbol)  /* load dll and lookup symbol */
String dll;
String symbol; {
    ObjectFile instance = shl_load(dll,BIND_IMMEDIATE,0L);
    void* r;
    if (NULL == instance) {
        ERRMSG(0) "Error while importing DLL \"%s\"", dll
        EEND;
    }
    return (0 == shl_findsym(&instance,symbol,TYPE_PROCEDURE,&r)) ? r : 0;
}

#else /* Dynamic loading not available */

void* getDLLSymbol(dll,symbol)  /* load dll and lookup symbol */
String dll;
String symbol; {
#if 1 /* very little to choose between these options */
    return 0;
#else
    ERRMSG(0) "This Hugs build does not support dynamic loading\n"
    EEND;
#endif
}

#endif /* Dynamic loading not available */

