#include <stdarg.h>
#include <stdio.h>
#include <windows.h>

const char *prog = "runexe";

#define BUFLEN 65537

void die(char *fmt, ...)
{
  va_list ap = va_start(ap, fmt);

  fprintf(stderr, "%s: ", prog);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  va_end(ap);
  exit(1);
}

void warn(char *fmt, ...)
{
  va_list ap = va_start(ap, fmt);

  fprintf(stderr, "%s: ", prog);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  va_end(ap);
}

int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpszCmdParam, int nCmdShow)
{
  STARTUPINFO sInfo;
  PROCESS_INFORMATION pInfo;
  TCHAR buf[BUFLEN];
  DWORD retCode;

  sInfo.cb              = sizeof(STARTUPINFO);
  sInfo.lpReserved      = NULL;
  sInfo.lpReserved2     = NULL;
  sInfo.cbReserved2     = 0;
  sInfo.lpDesktop       = NULL;
  sInfo.lpTitle         = NULL;
  sInfo.dwFlags         = 0;

  if (GetCurrentDirectory(BUFLEN, buf) == 0) die("couldn't get current directory");
  if (strlen(lpszCmdParam) == 0) die("no parameters given");
  warn("cwd: %s\n", buf);
  warn("runexing >>>%s<<<\n", lpszCmdParam);
  if (!CreateProcess(NULL, lpszCmdParam, NULL, NULL, TRUE, 0, NULL, NULL, &sInfo, &pInfo))
    die("could not create process");

  WaitForSingleObject(pInfo.hProcess, INFINITE);
  if (GetExitCodeProcess(pInfo.hProcess, &retCode) == 0) retCode = -1;
  CloseHandle(pInfo.hProcess);
  CloseHandle(pInfo.hThread);
  printf("return code %ld\n", retCode);

  return retCode;
}
