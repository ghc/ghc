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

// Local Variables:
// mode: C
// fill-column: 80
// indent-tabs-mode: nil
// c-basic-offset: 4
// buffer-file-coding-system: utf-8-unix
// End:
