

#include <stdio.h>
#include <assert.h>
#include <windows.h>
//#include "../includes/DietHEP.h"


typedef enum { dh_stdcall, dh_ccall } DH_CALLCONV;
typedef int                           DH_MODULE;
typedef char*                         DH_LPCSTR;

__declspec(dllimport)
extern DH_MODULE DH_LoadLibrary    ( DH_LPCSTR modname );
__declspec(dllimport)
extern void*     DH_GetProcAddress ( DH_CALLCONV  cconv, 
                                     DH_MODULE    hModule, 
                                     DH_LPCSTR    lpProcName );


int main ( int argc, char** argv )
{
   {
   DH_MODULE hModule;
   void(*proc)(int);

   hModule = DH_LoadLibrary("Dh_Demo");   /* note no .hs */
   assert(hModule != 0);
   proc = DH_GetProcAddress ( dh_ccall, hModule, "wurble" );
   assert(proc);

   proc(44);
   proc(45);
   proc(46);
   }
   return 0;
}
