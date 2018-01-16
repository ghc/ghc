#ifndef WINTERNL_COMPAT_H
#define WINTERNL_COMPAT_H

/*
 * winternl.h is not included in MinGW, which was shipped with the 32-bit
 * Windows version of GHC prior to the 7.10.3 release.
 */
#if defined(x86_64_HOST_ARCH) || \
   __GLASGOW_HASKELL__ >= 711 || \
   (__GLASGOW_HASKELL__ == 710 && \
    defined(__GLASGOW_HASKELL_PATCHLEVEL1__) && \
    __GLASGOW_HASKELL_PATCHLEVEL1__ >= 2)
# include <winternl.h>
#else
// Some declarations from winternl.h that we need in Win32
# include <windows.h>

typedef enum _OBJECT_INFORMATION_CLASS {
   ObjectBasicInformation,
   ObjectNameInformation,
   ObjectTypeInformation,
   ObjectAllInformation,
   ObjectDataInformation
} OBJECT_INFORMATION_CLASS, *POBJECT_INFORMATION_CLASS;

typedef LONG NTSTATUS, *PNTSTATUS;

typedef struct _UNICODE_STRING {
  USHORT Length;
  USHORT MaximumLength;
  PWSTR  Buffer;
} UNICODE_STRING, *PUNICODE_STRING;

typedef struct _OBJECT_NAME_INFORMATION {
  UNICODE_STRING Name;
} OBJECT_NAME_INFORMATION, *POBJECT_NAME_INFORMATION;
#endif

#endif /* WINTERNL_COMPAT_H */
