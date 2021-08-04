#include <stdint.h>

#include <windows.h>

uint64_t hs_hashable_init() {
    /* Handy list at https://stackoverflow.com/a/3487338/1308058 */

    uint64_t a = GetCurrentProcessId(); /* DWORD */
    uint64_t b = GetCurrentThreadId(); /* DWORD */
    uint64_t c = GetTickCount(); /* DWORD */

    SYSTEMTIME t = {0,0,0,0,0,0,0,0};
    GetSystemTime(&t);

    LARGE_INTEGER i;
    QueryPerformanceCounter(&i);

    return a ^ (b << 32) ^ (c << 16)
        ^ ((uint64_t) t.wYear         << 56)
        ^ ((uint64_t) t.wMonth        << 48)
        ^ ((uint64_t) t.wDayOfWeek    << 40)
        ^ ((uint64_t) t.wDay          << 32)
        ^ ((uint64_t) t.wHour         << 24)
        ^ ((uint64_t) t.wMinute       << 16)
        ^ ((uint64_t) t.wSecond       << 8)
        ^ ((uint64_t) t.wMilliseconds << 0)
        ^ ((uint64_t) i.QuadPart);
}
