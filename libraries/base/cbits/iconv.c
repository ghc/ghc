#ifndef __MINGW32__

#include <stdlib.h>
#include <iconv.h>

iconv_t hs_iconv_open(const char* tocode,
		      const char* fromcode)
{
	return iconv_open(tocode, fromcode);
}

size_t hs_iconv(iconv_t cd,
		const char* * inbuf, size_t * inbytesleft,
		char* * outbuf, size_t * outbytesleft)
{
    // (void*) cast avoids a warning.  Some iconvs use (const
    // char**inbuf), other use (char **inbuf).
    return iconv(cd, (void*)inbuf, inbytesleft, outbuf, outbytesleft);
}

int hs_iconv_close(iconv_t cd) {
	return iconv_close(cd);
}

#endif
