/*
 * (c) Tamar Christina, 2019.
 *
 * Structures supporting the IOCP based I/O Manager or Windows.
 */

#include <windows.h>
#include <stdint.h>

#if defined(_WIN64)
#  define ALIGNMENT __attribute__ ((aligned (8)))
#elif defined(_WIN32)
#  define ALIGNMENT __attribute__ ((aligned (8)))
#else
#  error "unknown environment, can't determine alignment"
#endif

/* Completion data structure.  Must be kept in sync with that in
   GHC.Event.Windows or horrible things happen.  */
typedef struct _CompletionData {
  /* The Handle to the object for which the I/O operation is in progress.  */
  HWND cdHandle;
  /* Handle to the callback routine to call to notify that an operation has
     finished.  This value is opaque as it shouldn't be accessible
     outside the Haskell world.  */
  uintptr_t cdCallback;
} CompletionData, *LPCompletionData;

/* The Windows API Requires an OVERLAPPED struct for asynchronous access,
   however if we pad the structure we can give extra book keeping information
   without needing to look these up later.  Do not modify this struct unless
   you know what you're doing.   */
typedef struct _HASKELL_OVERLAPPED {
  /* Windows OVERLAPPED structure.  NOTE: MUST BE FIRST element.  */
  OVERLAPPED hoOverlapped;
  /* Pointer to additional payload in Haskell land.  This will contain a
     foreign pointer.  We only use atomic operations to access this field in
     order to correctly handle multiple threads using it.  */
  LPCompletionData hoData ALIGNMENT;
} HASKELL_OVERLAPPED;
