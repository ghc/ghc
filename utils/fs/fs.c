/* -----------------------------------------------------------------------------
 *
 * (c) Tamar Christina 2018
 *
 * Windows I/O routines for file opening.
 *
 * NOTE: Only modify this file in utils/fs/ and rerun configure. Do not edit
 *       this file in any other directory as it will be overwritten.
 *
 * ---------------------------------------------------------------------------*/
#include "fs.h"
#include <stdio.h>

#if defined(_WIN32)

#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>

#include <windows.h>
#include <io.h>
#include <fcntl.h>
#include <wchar.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <share.h>

/* This function converts Windows paths between namespaces. More specifically
   It converts an explorer style path into a NT or Win32 namespace.
   This has several caveats but they are caviats that are native to Windows and
   not POSIX. See
   https://msdn.microsoft.com/en-us/library/windows/desktop/aa365247.aspx.
   Anything else such as raw device paths we leave untouched.  The main benefit
   of doing any of this is that we can break the MAX_PATH restriction and also
   access raw handles that we couldn't before.  */
static wchar_t* __hs_create_device_name (const wchar_t* filename) {
  const wchar_t* win32_dev_namespace  = L"\\\\.\\";
  const wchar_t* win32_file_namespace = L"\\\\?\\";
  const wchar_t* nt_device_namespace  = L"\\Device\\";
  const wchar_t* unc_prefix           = L"UNC\\";
  const wchar_t* network_share        = L"\\\\";

  wchar_t* result = _wcsdup (filename);
  wchar_t ns[10] = {0};

  /* If the file is already in a native namespace don't change it.  */
  if (   wcsncmp (win32_dev_namespace , filename, 4) == 0
      || wcsncmp (win32_file_namespace, filename, 4) == 0
      || wcsncmp (nt_device_namespace , filename, 8) == 0)
    return result;

  /* Since we're using the lower level APIs we must normalize slashes now.  The
     Win32 API layer will no longer convert '/' into '\\' for us.  */
  for (size_t i = 0; i < wcslen (result); i++)
    {
      if (result[i] == L'/')
        result[i] = L'\\';
    }

  /* Now resolve any . and .. in the path or subsequent API calls may fail since
     Win32 will no longer resolve them.  */
  DWORD nResult = GetFullPathNameW (result, 0, NULL, NULL) + 1;
  wchar_t *temp = _wcsdup (result);
  result = malloc (nResult * sizeof (wchar_t));
  if (GetFullPathNameW (temp, nResult, result, NULL) == 0)
    {
      goto cleanup;
    }

  free (temp);

  if (wcsncmp (network_share, result, 2) == 0)
    {
      if (swprintf (ns, 10, L"%ls%ls", win32_file_namespace, unc_prefix) <= 0)
        {
          goto cleanup;
        }
    }
  else if (swprintf (ns, 10, L"%ls", win32_file_namespace) <= 0)
    {
      goto cleanup;
    }

  /* Create new string.  */
  int bLen = wcslen (result) + wcslen (ns) + 1;
  temp = _wcsdup (result);
  result = malloc (bLen * sizeof (wchar_t));
  if (swprintf (result, bLen, L"%ls%ls", ns, temp) <= 0)
    {
      goto cleanup;
    }

  free (temp);

  return result;

cleanup:
  free (temp);
  free (result);
  return NULL;
}

#define HAS_FLAG(a,b) ((a & b) == b)

int FS(swopen) (const wchar_t* filename, int oflag, int shflag, int pmode)
{
  /* Construct access mode.  */
  DWORD dwDesiredAccess = 0;
  if (HAS_FLAG (oflag, _O_RDONLY))
    dwDesiredAccess |= GENERIC_READ | FILE_READ_DATA | FILE_READ_ATTRIBUTES;
  if (HAS_FLAG (oflag, _O_RDWR))
    dwDesiredAccess |= GENERIC_WRITE | GENERIC_READ | FILE_READ_DATA |
                       FILE_WRITE_DATA | FILE_READ_ATTRIBUTES |
                       FILE_WRITE_ATTRIBUTES;
  if (HAS_FLAG (oflag,  _O_WRONLY))
    dwDesiredAccess|= GENERIC_WRITE | FILE_WRITE_DATA | FILE_WRITE_ATTRIBUTES;

  /* Construct shared mode.  */
  DWORD dwShareMode = FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE;
  if (HAS_FLAG (shflag, _SH_DENYRW))
    dwShareMode &= ~(FILE_SHARE_READ | FILE_SHARE_WRITE);
  if (HAS_FLAG (shflag, _SH_DENYWR))
    dwShareMode &= ~FILE_SHARE_WRITE;
  if (HAS_FLAG (shflag, _SH_DENYRD))
    dwShareMode &= ~FILE_SHARE_READ;
  if (HAS_FLAG (pmode, _S_IWRITE))
    dwShareMode |= FILE_SHARE_READ | FILE_SHARE_WRITE;
  if (HAS_FLAG (pmode, _S_IREAD))
    dwShareMode |= FILE_SHARE_READ;

  /* Override access mode with pmode if creating file.  */
  if (HAS_FLAG (oflag, _O_CREAT))
    {
      if (HAS_FLAG (pmode, _S_IWRITE))
        dwDesiredAccess |= FILE_GENERIC_WRITE;
      if (HAS_FLAG (pmode, _S_IREAD))
        dwDesiredAccess |= FILE_GENERIC_READ;
    }

  /* Create file disposition.  */
  DWORD dwCreationDisposition = OPEN_EXISTING;
  if (HAS_FLAG (oflag, _O_CREAT))
    dwCreationDisposition = OPEN_ALWAYS;
  if (HAS_FLAG (oflag, (_O_CREAT | _O_EXCL)))
    dwCreationDisposition = CREATE_NEW;
  if (HAS_FLAG (oflag, _O_TRUNC) && !HAS_FLAG (oflag, _O_CREAT))
    dwCreationDisposition = TRUNCATE_EXISTING;

  /* Set file access attributes.  */
  DWORD dwFlagsAndAttributes = FILE_ATTRIBUTE_NORMAL;
  if (HAS_FLAG (oflag, _O_RDONLY))
    dwFlagsAndAttributes |= 0; /* No special attribute.  */
  if (HAS_FLAG (oflag, (_O_CREAT | _O_TEMPORARY)))
    dwFlagsAndAttributes |= FILE_FLAG_DELETE_ON_CLOSE;
  if (HAS_FLAG (oflag, (_O_CREAT | _O_SHORT_LIVED)))
    dwFlagsAndAttributes |= FILE_ATTRIBUTE_TEMPORARY;
  if (HAS_FLAG (oflag, _O_RANDOM))
    dwFlagsAndAttributes |= FILE_FLAG_RANDOM_ACCESS;
  if (HAS_FLAG (oflag, _O_SEQUENTIAL))
    dwFlagsAndAttributes |= FILE_FLAG_SEQUENTIAL_SCAN;
  /* Flag is only valid on it's own.  */
  if (dwFlagsAndAttributes != FILE_ATTRIBUTE_NORMAL)
    dwFlagsAndAttributes &= ~FILE_ATTRIBUTE_NORMAL;

  /* Set security attributes.  */
  SECURITY_ATTRIBUTES securityAttributes;
  ZeroMemory (&securityAttributes, sizeof(SECURITY_ATTRIBUTES));
  securityAttributes.bInheritHandle       = !(oflag & _O_NOINHERIT);
  securityAttributes.lpSecurityDescriptor = NULL;
  securityAttributes.nLength              = sizeof(SECURITY_ATTRIBUTES);

  wchar_t* _filename = __hs_create_device_name (filename);
  if (!_filename)
    return -1;

  HANDLE hResult
    = CreateFileW (_filename, dwDesiredAccess, dwShareMode, &securityAttributes,
                   dwCreationDisposition, dwFlagsAndAttributes, NULL);
  free (_filename);
  if (INVALID_HANDLE_VALUE == hResult)
    return -1;

  /* Now we have a Windows handle, we have to convert it to an FD and apply
     the remaining flags.  */
  const int flag_mask = _O_APPEND | _O_RDONLY | _O_TEXT | _O_WTEXT;
  int fd = _open_osfhandle ((intptr_t)hResult, oflag & flag_mask);
  if (-1 == fd)
    return -1;

  /* Finally we can change the mode to the requested one.  */
  const int mode_mask = _O_TEXT | _O_BINARY | _O_U16TEXT | _O_U8TEXT | _O_WTEXT;
  if ((oflag & mode_mask) && (-1 == _setmode (fd, oflag & mode_mask)))
    return -1;

  return fd;
}

FILE *FS(fwopen) (const wchar_t* filename, const wchar_t* mode)
{
  int shflag = 0;
  int pmode  = 0;
  int oflag  = 0;

  int len = wcslen (mode);
  int i;
  #define IS_EXT(X) ((i < (len - 1)) && mode[i] == X)

  for (i = 0; i < len; i++)
    {
      switch (mode[i])
        {
          case L'a':
            if (IS_EXT (L'+'))
              oflag |= _O_RDWR | _O_CREAT | _O_APPEND;
            else
              oflag |= _O_WRONLY | _O_CREAT | _O_APPEND;
            break;
          case L'r':
            if (IS_EXT (L'+'))
              oflag |= _O_RDWR;
            else
              oflag |= _O_RDONLY;
            break;
          case L'w':
            if (IS_EXT (L'+'))
              oflag |= _O_RDWR | _O_CREAT | _O_TRUNC;
            else
              oflag |= _O_WRONLY | _O_CREAT | _O_TRUNC;
            break;
          case L'b':
            oflag |= _O_BINARY;
            break;
          case L't':
            oflag |= _O_TEXT;
            break;
          case L'c':
          case L'n':
            oflag |= 0;
            break;
          case L'S':
            oflag |= _O_SEQUENTIAL;
            break;
          case L'R':
            oflag |= _O_RANDOM;
            break;
          case L'T':
            oflag |= _O_SHORT_LIVED;
            break;
          case L'D':
            oflag |= _O_TEMPORARY;
            break;
          default:
            if (wcsncmp (mode, L"ccs=UNICODE", 11) == 0)
              oflag |= _O_WTEXT;
            else if (wcsncmp (mode, L"ccs=UTF-8", 9) == 0)
              oflag |= _O_U8TEXT;
            else if (wcsncmp (mode, L"ccs=UTF-16LE", 12) == 0)
              oflag |= _O_U16TEXT;
            else continue;
        }
    }
  #undef IS_EXT

  int fd = FS(swopen) (filename, oflag, shflag, pmode);
  FILE* file = _wfdopen (fd, mode);
  return file;
}

FILE *FS(fopen) (const char* filename, const char* mode)
{
  size_t len = mbstowcs (NULL, filename, 0);
  wchar_t *w_filename = malloc (sizeof (wchar_t) * (len + 1));
  mbstowcs (w_filename, filename, len);
  w_filename[len] = L'\0';

  len = mbstowcs (NULL, mode, 0);
  wchar_t *w_mode = malloc (sizeof (wchar_t) * (len + 1));
  mbstowcs (w_mode, mode, len);
  w_mode[len] = L'\0';

  FILE *result = FS(fwopen) (w_filename, w_mode);
  free (w_filename);
  free (w_mode);
  return result;
}
#else
FILE *FS(fopen) (const char* filename, const char* mode)
{
  return fopen (filename, mode);
}
#endif
