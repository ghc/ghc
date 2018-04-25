#include <iconv.h>

iconv_t haskeline_iconv_open(const char *tocode, const char *fromcode);

void haskeline_iconv_close(iconv_t cd);

size_t haskeline_iconv(iconv_t cd, char **inbuf, size_t *inbytesleft,
                char **outbuf, size_t *outbytesleft);

