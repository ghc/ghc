#include <windows.h>

const char *prog = "runexe";

int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpszCmdParam, int nCmdShow)
{
  STARTUPINFO sInfo;
  PROCESS_INFORMATION pInfo;

  sInfo.cb              = sizeof(STARTUPINFO);
  sInfo.lpReserved      = NULL;
  sInfo.lpReserved2     = NULL;
  sInfo.cbReserved2     = 0;
  sInfo.lpDesktop       = NULL;
  sInfo.lpTitle         = NULL;
  sInfo.dwFlags         = 0;
  
  if (strlen(lpszCmdParam) == 0) {
    printf("%s: no parameters given\n", prog);
    exit(1);
  }
  CreateProcess(NULL, lpszCmdParam, NULL, NULL, FALSE, 0, NULL, NULL, &sInfo, &pInfo);
  return 0;
}
