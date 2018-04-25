#ifndef _WIN_CONSOLE_H
#define _WIN_CONSOLE_H
#include <windows.h>

BOOL haskeline_SetPosition(HANDLE h, COORD* c);
BOOL haskeline_FillConsoleCharacter(HANDLE h, TCHAR c, DWORD l, COORD *p, LPDWORD n);
BOOL haskeline_FillConsoleAttribute(HANDLE h, WORD c, DWORD l, COORD *p, LPDWORD n);

#endif
