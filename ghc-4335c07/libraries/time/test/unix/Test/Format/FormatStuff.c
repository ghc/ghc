#include "FormatStuff.h"

size_t format_time (
    char* buffer, size_t maxsize,
    const char* format,
    int isdst,int gmtoff,char* zonename,time_t t)
{
    t += gmtoff;
    struct tm tmd;
    gmtime_r(&t,&tmd);
    tmd.tm_isdst = isdst;
    tmd.tm_gmtoff = gmtoff;
    tmd.tm_zone = zonename;
    return strftime(buffer,maxsize,format,&tmd);
}
