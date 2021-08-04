#ifndef FOUNDATION_SYSTEM_H
# define FOUNDATION_SYSTEM_H

#ifdef _WIN32
   #define FOUNDATION_SYSTEM_WINDOWS
   #define FOUNDATION_SYSTEM_API_NO_CLOCK

   //define something for Windows (32-bit and 64-bit, this part is common)
   #ifdef _WIN64
      #define FOUNDATION_SYSTEM_WINDOWS_64
      //define something for Windows (64-bit only)
   #else
      #define FOUNDATION_SYSTEM_WINDOWS_32
      //define something for Windows (32-bit only)
   #endif
#elif __APPLE__
    #include "TargetConditionals.h"
    #include "Availability.h"

    #if TARGET_OS_MAC
      #define FOUNDATION_SYSTEM_UNIX
      #define FOUNDATION_SYSTEM_MACOS

      #if !defined(__MAC_10_12) || __MAC_OS_X_VERSION_MIN_REQUIRED < __MAC_10_12
      #define FOUNDATION_SYSTEM_API_NO_CLOCK
      #endif
      // Other kinds of Mac OS
    #else
    #   error "foundation: system: Unknown Apple platform"
    #endif
#elif __linux__
    #define FOUNDATION_SYSTEM_UNIX
    #define FOUNDATION_SYSTEM_LINUX
    // linux
#elif defined(__FreeBSD__)
    #define FOUNDATION_SYSTEM_UNIX
    #define FOUNDATION_SYSTEM_BSD
    #define FOUNDATION_SYSTEM_FREEBSD
    // freeBSD
#elif defined(__NetBSD__)
    #define FOUNDATION_SYSTEM_UNIX
    #define FOUNDATION_SYSTEM_BSD
    #define FOUNDATION_SYSTEM_NETBSD
    // NetBSD
#elif defined(__OpenBSD__)
    #define FOUNDATION_SYSTEM_UNIX
    #define FOUNDATION_SYSTEM_BSD
    #define FOUNDATION_SYSTEM_OPENBSD
    // OpenBSD
#elif __unix__ // all unices not caught above
    #define FOUNDATION_SYSTEM_UNIX
    // Unix
#elif defined(_POSIX_VERSION)
    #define FOUNDATION_SYSTEM_UNIX
    // POSIX
#else
#   error "foundation: system: Unknown compiler"
#endif

#endif
