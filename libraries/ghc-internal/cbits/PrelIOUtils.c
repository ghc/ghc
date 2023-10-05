/*
 * (c) The University of Glasgow 2002
 *
 * static versions of the inline functions in HsBase.h
 */

#define INLINE

#include "Rts.h"
#include "HsBase.h"

void errorBelch2(const char*s, char *t)
{
    errorBelch(s,t);
}

void debugBelch2(const char*s, char *t)
{
    debugBelch(s,t);
}

#if defined(HAVE_LIBCHARSET)
#  include <libcharset.h>
#elif defined(HAVE_LANGINFO_H)
#  include <langinfo.h>
#endif

#if !defined(mingw32_HOST_OS)
const char* localeEncoding(void)
{
#if defined(HAVE_LIBCHARSET)
    return locale_charset();

#elif defined(HAVE_LANGINFO_H)
    return nl_langinfo(CODESET);

#else
#warning Depending on the unportable behavior of GNU iconv due to absence of both libcharset and langinfo.h
    /* GNU iconv accepts "" to mean the current locale's
     * encoding. Warning: This isn't portable.
     */
    return "";
#endif
}
#endif
