
#pragma once

// Define ERROR_IF_IN_RTS_PRIVATE_GROUP as /**/ incase Begin/EndPrivate has never been called yet.
#if !defined(ERROR_IF_IN_RTS_PRIVATE_GROUP)
#define ERROR_IF_IN_RTS_PRIVATE_GROUP /**/
#endif

#if defined(COMPILING_RTS) && defined(mingw32_HOST_OS) && defined(DYNAMIC)
#define RTS_PUBLIC ERROR_IF_IN_RTS_PRIVATE_GROUP __attribute__((dllexport))
#else
#define RTS_PUBLIC ERROR_IF_IN_RTS_PRIVATE_GROUP
#endif
