
typedef enum { dh_stdcall, dh_ccall } DHCALLCONV;
typedef int                           HMODULE;
typedef char*                         LPCSTR;

extern HMODULE LoadLibrary ( LPCSTR modname );
extern void*   GetProcAddr ( DHCALLCONV cconv, 
                             HMODULE    hModule, 
                             LPCSTR     lpProcName );


