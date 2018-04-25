#ifndef _DIATEMP_H_
#define _DIATEMP_H_

#include <windows.h>

typedef struct {
  LPDLGTEMPLATE dtemplate;
  unsigned int bytes_left; /* bytes left in the chunk that 'dtemplate' points to. */
  unsigned int bytes_alloced;
  LPDLGITEMTEMPLATE next_dia_item;
} DIA_TEMPLATE;

extern LPDLGTEMPLATE getFinalDialog(DIA_TEMPLATE* dt);

extern DIA_TEMPLATE* mkDiaTemplate
      ( UINT size, int x, int y, int cx, int cy
      , DWORD style, DWORD exstyle
      , LPCWSTR menu, LPCWSTR class
      , LPCWSTR caption, LPCWSTR font
      , int height
      );
extern DIA_TEMPLATE* addDiaControl
         ( DIA_TEMPLATE* dia
	 , LPCWSTR text, short id
	 , LPCWSTR classname, DWORD style
	 , int x, int y, int cx, int cy
	 , DWORD exstyle
	 );

#endif
