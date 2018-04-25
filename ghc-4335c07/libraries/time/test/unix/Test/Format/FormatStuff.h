#include <time.h>

size_t format_time (
    char *s, size_t maxsize,
    const char *format,
    int isdst,int gmtoff,char* zonename,time_t t);
