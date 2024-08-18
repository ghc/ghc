/*
 * We need Vista headers to use the GetFileInformationByHandleEx function and
 * the FILE_NAME_INFO struct.
 */
#define WINVER 0x0600
#define _WIN32_WINNT 0x0600

#include <stdbool.h>
#include <windows.h>
#include "isMinTTY.h"

bool isMinTTY() {
  const HANDLE h = GetStdHandle(STD_ERROR_HANDLE);
  if (h == NULL || h == INVALID_HANDLE_VALUE) {
    return false;
  } else if (GetFileType(h) != FILE_TYPE_PIPE) {
    return false;
  }

  const unsigned long bufSize = sizeof(DWORD) + MAX_PATH * sizeof(WCHAR);
  BYTE buf[bufSize];
  PFILE_NAME_INFO pfni = (PFILE_NAME_INFO) buf;

  if (!GetFileInformationByHandleEx(h, FileNameInfo, buf, bufSize)) {
    return false;
  }

  PWSTR fn = pfni->FileName;
  fn[pfni->FileNameLength] = L'\0';

  return ((wcsstr(fn, L"\\cygwin-") || wcsstr(fn, L"\\msys-")) &&
           wcsstr(fn, L"-pty") && wcsstr(fn, L"-master"));
}
