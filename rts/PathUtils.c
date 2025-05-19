#include <Rts.h>
#include "RtsUtils.h"
#include "PathUtils.h"

#include <string.h>
#include <stddef.h>

#include <libgen.h>
#include <ctype.h>
#if defined(mingw32_HOST_OS)
/* Using Secure APIs */
#define MINGW_HAS_SECURE_API 1
#include <wchar.h>
#endif

pathchar* pathdup(const pathchar *path)
{
    pathchar *ret;
#if defined(mingw32_HOST_OS)
    ret = wcsdup(path);
#else
    /* sigh, strdup() isn't a POSIX function, so do it the long way */
    ret = stgMallocBytes( strlen(path)+1, "pathdup" );
    strcpy(ret, path);
#endif
    return ret;
}

pathchar* pathdir(const pathchar *path)
{
    pathchar *ret;
#if defined(mingw32_HOST_OS)
    pathchar *drive, *dirName;
    size_t memberLen = pathlen(path) + 1;
    dirName = stgMallocBytes(pathsize * memberLen, "pathdir(path)");
    ret     = stgMallocBytes(pathsize * memberLen, "pathdir(path)");
    drive   = stgMallocBytes(pathsize * _MAX_DRIVE, "pathdir(path)");
    _wsplitpath_s(path, drive, _MAX_DRIVE, dirName, pathsize * pathlen(path), NULL, 0, NULL, 0);
    pathprintf(ret, memberLen, WSTR("%" PATH_FMT "%" PATH_FMT), drive, dirName);
    stgFree(drive);
    stgFree(dirName);
#else
    // N.B. cast is safe as we do not modify dirName
    const pathchar* dirName = dirname((pathchar *) path);
    size_t memberLen  = pathlen(dirName);
    ret = stgMallocBytes(pathsize * (memberLen + 2), "pathdir(path)");
    strcpy(ret, dirName);
    ret[memberLen  ] = '/';
    ret[memberLen+1] = '\0';
#endif
    return ret;
}

pathchar* mkPath(const char* path)
{
#if defined(mingw32_HOST_OS)
    size_t required = mbstowcs(NULL, path, 0);
    pathchar *ret = stgMallocBytes(sizeof(pathchar) * (required + 1), "mkPath");
    if (mbstowcs(ret, path, required) == (size_t)-1)
    {
        barf("mkPath failed converting char* to wchar_t*");
    }
    ret[required] = '\0';
    return ret;
#else
    return pathdup(path);
#endif
}

HsBool endsWithPath(const pathchar* base, const pathchar* str) {
    int blen = pathlen(base);
    int slen = pathlen(str);
    return (blen >= slen) && (0 == pathcmp(base + blen - slen, str));
}
