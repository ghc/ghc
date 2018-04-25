#include "win_console.h"

BOOL haskeline_SetPosition(HANDLE h, COORD* c) {
    return SetConsoleCursorPosition(h,*c);
}

BOOL haskeline_FillConsoleCharacter(HANDLE h, TCHAR c, DWORD l, COORD *p, LPDWORD n) {
    return FillConsoleOutputCharacter(h,c,l,*p,n);
}

BOOL haskeline_FillConsoleAttribute(HANDLE h, WORD a, DWORD l, COORD *p, LPDWORD n) {
    return FillConsoleOutputAttribute(h,a,l,*p,n);
}
