
#pragma once

#if defined(COMPILING_RTS) && defined(DYNAMIC)
    #if defined(mingw32_HOST_OS)
        #define RTS_PUBLIC __attribute__((dllexport))
    #else
        #define RTS_PUBLIC __attribute__((visibility("default")))
    #endif
#else
    #define RTS_PUBLIC /**/
#endif
