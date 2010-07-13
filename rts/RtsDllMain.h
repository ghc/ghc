
#include "Rts.h"

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif

/* I'd be mildly surprised if this wasn't defined, but still. */
#if defined(__PIC__) && defined(mingw32_HOST_OS)
BOOL
WINAPI
DllMain ( HINSTANCE hInstance
        , DWORD reason
       , LPVOID reserved
       );
#endif

