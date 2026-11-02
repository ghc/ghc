
#pragma once

// Define ERROR_IF_IN_RTS_PRIVATE_GROUP as /**/ incase Begin/EndPrivate has never been called yet.
#if !defined(ERROR_IF_IN_RTS_PRIVATE_GROUP)
    #define ERROR_IF_IN_RTS_PRIVATE_GROUP /**/
#endif

#if defined(COMPILING_RTS) && defined(DYNAMIC)
    #if defined(mingw32_HOST_OS)
        #define RTS_PUBLIC __attribute__((dllexport))
    #else
        #define RTS_PUBLIC __attribute__((visibility("default")))
    #endif
#else
    #define RTS_PUBLIC ERROR_IF_IN_RTS_PRIVATE_GROUP
#endif
