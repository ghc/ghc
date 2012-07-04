
#include "Rts.h"

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif

/* I'd be mildly surprised if this wasn't defined, but still. */
#if defined(COMPILING_WINDOWS_DLL)
BOOL
WINAPI
DllMain ( HINSTANCE hInstance
        , DWORD reason
       , LPVOID reserved
       );
#endif

