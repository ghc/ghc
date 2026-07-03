
#pragma once

#if defined(COMPILING_RTS) && defined(mingw32_HOST_OS) && defined(DYNAMIC)
#define RTS_PUBLIC __attribute__((dllexport))
#else
#define RTS_PUBLIC /**/
#endif
