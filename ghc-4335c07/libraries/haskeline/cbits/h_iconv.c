#include "h_iconv.h"

// Wrapper functions, since iconv_open et al are macros in libiconv.
iconv_t haskeline_iconv_open(const char *tocode, const char *fromcode) {
    return iconv_open(tocode, fromcode);
}

void haskeline_iconv_close(iconv_t cd) {
    iconv_close(cd);
}

size_t haskeline_iconv(iconv_t cd, char **inbuf, size_t *inbytesleft,
                char **outbuf, size_t *outbytesleft) {
    // Cast inbuf to (void*) so that it works both on Solaris, which expects
    // a (const char**), and on other platforms (e.g. Linux), which expect
    // a (char **).
    return iconv(cd, (void*)inbuf, inbytesleft, outbuf, outbytesleft);
}
