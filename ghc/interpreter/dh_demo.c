

#include <stdio.h>
#include <assert.h>
#include <windows.h>
#include "../includes/DietHEP.h"

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
