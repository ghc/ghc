/*
 * Helper functions for filling in DLG(ITEM)TEMPLATEs -
 * closely based on code provided Rector & Newcomer
 * in their book, Win32 programming.
 *
 * The only change here is to make it possible to
 * add any number of controls to the dialog without
 * having to worry about overrunning the chunk of
 * memory that we're writing all this info into.
 *
 */

#define UNICODE
#include <windows.h>
#include <wchar.h>
#include <stdlib.h>
#include "diatemp.h"

#define DLGTEMPLATE_WORKING_SIZE 4096

LPDLGTEMPLATE getFinalDialog(DIA_TEMPLATE* dt)
{ 
  LPDLGTEMPLATE ptr = dt->dtemplate;
  free(dt);
  return ptr;
}

LPWORD
appendString (LPWORD ptr, LPCWSTR text)
{
 LPWSTR str = (LPWSTR)ptr;
 wcscpy(str, text);
 ptr = (LPWORD)(str + wcslen(str) + 1);
 return ptr;
}

LPWORD
setClassAtom(LPDLGITEMTEMPLATE item, WORD classatom)
{
  LPWORD ptr = (LPWORD)&item[1];
  *ptr++ = 0xffff;
  *ptr++ = classatom;
  return ptr;
}

LPWORD
setClassName(LPDLGITEMTEMPLATE item, LPCWSTR classname)
{
  LPWORD ptr = (LPWORD)&item[1];
  ptr = appendString(ptr, classname);
  return ptr;
}

LPWORD
setResourceID(LPWORD ptr, WORD id)
{
 *ptr++ = 0xffff;
 *ptr++ = (WORD)id;
 return ptr;
}

DIA_TEMPLATE*
mkDiaTemplate
      ( UINT size, int x, int y, int cx, int cy
      , DWORD style, DWORD exstyle
      , LPCWSTR menu, LPCWSTR class
      , LPCWSTR caption, LPCWSTR font
      , int height
      )
{
  LPDLGTEMPLATE dlg;
  LPWORD ptr;
  DIA_TEMPLATE* dtemp;
  
  if ( size == 0 ) {
     size = DLGTEMPLATE_WORKING_SIZE;
  }
  dlg = (LPDLGTEMPLATE)malloc(size);
  if (dlg == NULL) {
    return NULL;
  }

  dlg->x  = x;
  dlg->y  = y;
  dlg->cx = cx;
  dlg->cy = cy;
  
  dlg->cdit = 0;
  
  dlg->style = style;
  if (font == NULL) {
    dlg->style &= ~ DS_SETFONT;
  } else {
    dlg->style |= DS_SETFONT;
  }
  dlg->dwExtendedStyle = exstyle;

  ptr= (LPWORD)&dlg[1];
  if (menu == NULL) {
    *ptr++ = 0;
  } else if (HIWORD(menu) == 0) {
    ptr = setResourceID(ptr, LOWORD(menu));
  } else {
    ptr = appendString(ptr, menu);
  }
  
  if ( class == NULL ) {
    *ptr++ = 0;
  } else if ( HIWORD(class) == 0 ) {
    ptr = setResourceID(ptr, LOWORD(class));
  } else {
    ptr = appendString(ptr, class);
  }

  ptr = appendString(ptr, (caption == NULL ? L"" : caption));  

  if ( font != NULL ) {
    *ptr++ = height;
    ptr = appendString(ptr, font);
  }

  dtemp = (DIA_TEMPLATE*)malloc(sizeof(DIA_TEMPLATE));
  if ( dtemp == NULL )
    return NULL;
  
  dtemp->dtemplate     = dlg;
  dtemp->next_dia_item = (LPDLGITEMTEMPLATE)ptr;
  dtemp->bytes_left    = (unsigned int)(((char*)dlg + size) - (char*)ptr);
  dtemp->bytes_alloced = size;

  return dtemp;
}

static
DIA_TEMPLATE*
check_if_enough_mem(DIA_TEMPLATE* dia, LPCWSTR text, LPCWSTR classname)
{
 unsigned int sz = 0;

 sz += sizeof(DLGITEMTEMPLATE);

 if ( HIWORD(classname) == 0 ) {
    sz += sizeof(WORD);
 } else {
    sz += wcslen(classname) + 1;
 }
 if ( HIWORD(text) == 0 ) {
    sz += sizeof(WORD);
 } else {
    sz += wcslen(text) + 1;
 }

 if ( sz >= dia->bytes_left ) {
   unsigned int diff;
   dia->bytes_left = dia->bytes_left + dia->bytes_alloced;
   dia->bytes_alloced *= 2;
   /* Being defensive here.. */
   diff = (unsigned int)((char*)dia->next_dia_item - (char*)dia->dtemplate);
   dia->dtemplate = (LPDLGTEMPLATE)realloc((void*)dia->dtemplate, dia->bytes_alloced);
   if ( dia->dtemplate == NULL )
     return NULL;
   dia->next_dia_item  = (LPDLGITEMTEMPLATE)((char*)dia->dtemplate + diff);
   return dia;
  } else {
   return dia;
  }
}

static
LPWORD noParms (LPDLGITEMTEMPLATE item, LPWORD ptr)
{
  *ptr++ = 0;
  if ( (((LPWORD)item) - ptr) & 0x1)
     *ptr++ = 0;
     
  return ptr;
}

DIA_TEMPLATE*
addDiaControl
         ( DIA_TEMPLATE* dia
	 , LPCWSTR text, short id
	 , LPCWSTR classname, DWORD style
	 , int x, int y, int cx, int cy
	 , DWORD exstyle
	 )
{
  LPWORD ptr;
  LPDLGITEMTEMPLATE item;

  dia = check_if_enough_mem(dia, text, classname);

  ptr = (LPWORD)&(dia->next_dia_item[1]);

  item = dia->next_dia_item;

  item->style = WS_CHILD | style;
  item->dwExtendedStyle = exstyle;
  item->x  = x;
  item->y  = y;
  item->cx = cx;
  item->cy = cy;
  item->id = (WORD)id;
  
  if ( HIWORD(classname) != 0 ) {
     ptr = setClassName(item, classname);
  } else {
     ptr = setResourceID(ptr, LOWORD(classname));
  }

  if ( HIWORD(text) != 0 ) {
    ptr = appendString(ptr, text);
  } else {
    ptr = setResourceID(ptr, (short)(LOWORD(text)));
  }
  
  ptr = noParms(item, ptr);
  
  dia->bytes_left    -= ((char*)ptr - ((char*)dia->next_dia_item));
  dia->next_dia_item  = (LPDLGITEMTEMPLATE)ptr;
  
  return dia;
}

