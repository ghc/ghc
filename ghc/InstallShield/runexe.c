#include <stdarg.h>
#include <stdio.h>
#include <windows.h>

const char *prog = "runexe";

#define BUFLEN 1025

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

  sInfo.cb              = sizeof(STARTUPINFO);
  sInfo.lpReserved      = NULL;
  sInfo.lpReserved2     = NULL;
  sInfo.cbReserved2     = 0;
  sInfo.lpDesktop       = NULL;
  sInfo.lpTitle         = NULL;
  sInfo.dwFlags         = 0;

  if (GetCurrentDirectory(BUFLEN, buf) == 0) die("no parameters given");
  if (strlen(lpszCmdParam) == 0) warn("couldn't get current directory");
  warn("cwd: %s\n", buf);
  warn("runexing >>>%s<<<\n", lpszCmdParam);
  CreateProcess(NULL, lpszCmdParam, NULL, NULL, FALSE, 0, NULL, NULL, &sInfo, &pInfo);
  return 0;
}
