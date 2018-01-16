#ifndef __WNDPROC_H
#define __WNDPROC_H

#define UNICODE
#include <windows.h>

extern LRESULT CALLBACK genericWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);

#endif /* __WNDPROC_H */
