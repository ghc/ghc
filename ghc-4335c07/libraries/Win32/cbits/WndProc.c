#include "WndProc.h"
#include <stdio.h>


/* Debugging code - might come in handy. */
#if 0
HWND
mkWin(long l)
{
  static char appN[] = "TestWin";
  HWND hw;
  WNDCLASSEX wndclass;
 
  wndclass.cbSize = sizeof(wndclass);
  wndclass.style  = CS_HREDRAW | CS_VREDRAW;
  wndclass.lpfnWndProc = genericWndProc;
  wndclass.cbClsExtra  = 0;
  wndclass.cbWndExtra  = 0;
  wndclass.hInstance   = GetModuleHandle(NULL);
  wndclass.hIcon       = LoadIcon(NULL, IDI_APPLICATION);
  wndclass.hCursor     = LoadCursor(NULL, IDC_ARROW);
  wndclass.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH);
  wndclass.lpszMenuName  = NULL;
  wndclass.lpszClassName = appN;
  wndclass.hIconSm       = LoadIcon(NULL, IDI_APPLICATION);

  RegisterClassEx(&wndclass);

  hw = CreateWindow(appN, "test", WS_OVERLAPPEDWINDOW,100,100,100,100, NULL, NULL, GetModuleHandle(NULL),NULL);
  //ShowWindow   (hw, SW_SHOWNORMAL);
  //UpdateWindow (hw);
  /*WndPump();*/
  //SetWindowLong( hw, GWL_USERDATA,l);
  return hw;
}

void
WndPump()
{
  MSG msg;

     fprintf(stderr, "Getting..\n");
  while (GetMessage(&msg, NULL, 0,0) != 0) {
     fprintf(stderr, "..got,\n");
     TranslateMessage(&msg);
     fprintf(stderr, "delivering.\n");
     DispatchMessage(&msg);
     fprintf(stderr, "Getting..\n");
  }
}
#endif

#ifdef DEBUG
char* __current_fun__ = NULL;
#endif

void
WndPump ()
{
  MSG msg;
  while(1) {
    GetMessage(&msg,NULL, 0,0);
    TranslateMessage(&msg);
    DispatchMessage(&msg);
  }
  return;
}

LRESULT CALLBACK genericWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
    LRESULT lr;

    if (hwnd) {
	LONG_PTR wndprocptr = GetWindowLongPtr(hwnd,GWLP_USERDATA);
	if (wndprocptr) {
	    lr = ((LRESULT (*)(HWND,UINT,WPARAM,LPARAM))(wndprocptr))(hwnd,msg,wParam,lParam);
#if 0
	    if (lr == -1) {
	        return DefWindowProc(hwnd, msg, wParam, lParam);
	    } else {
	      return lr;
	    }
#else
	    return lr;
#endif
	}
    }
    return DefWindowProc(hwnd, msg, wParam, lParam);
}

