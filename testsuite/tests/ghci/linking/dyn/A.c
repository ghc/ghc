#if defined(_MSC_VER)
    //  Microsoft
    #define EXPORT __declspec(dllexport)
#elif defined(_GCC)
    //  GCC
    #define EXPORT __attribute__((visibility("default")))
#else
    //  do nothing and hope for the best?
    #define EXPORT
#endif

extern EXPORT int foo();

EXPORT int foo()
{
    return 2;
}
