
typedef enum { dh_stdcall, dh_ccall } DH_CALLCONV;
typedef int                           DH_MODULE;
typedef char*                         DH_LPCSTR;

extern __attribute__((__stdcall__))
       DH_MODULE DH_LoadLibrary    ( DH_LPCSTR modname );
extern __attribute__((__stdcall__))
       void*     DH_GetProcAddress ( DH_CALLCONV  cconv, 
                                     DH_MODULE    hModule, 
                                     DH_LPCSTR    lpProcName );


