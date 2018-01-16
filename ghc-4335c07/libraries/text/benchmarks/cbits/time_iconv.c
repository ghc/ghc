#include <iconv.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

int time_iconv(char *srcbuf, size_t srcbufsize)
{
  uint16_t *destbuf = NULL;
  size_t destbufsize;
  static uint16_t *origdestbuf;
  static size_t origdestbufsize;
  iconv_t ic = (iconv_t) -1;
  int ret = 0;

  if (ic == (iconv_t) -1) {
    ic = iconv_open("UTF-16LE", "UTF-8");
    if (ic == (iconv_t) -1) {
      ret = -1;
      goto done;
    }
  }
  
  destbufsize = srcbufsize * sizeof(uint16_t);
  if (destbufsize > origdestbufsize) {
    free(origdestbuf);
    origdestbuf = destbuf = malloc(origdestbufsize = destbufsize);
  } else {
    destbuf = origdestbuf;
  }

  iconv(ic, &srcbuf, &srcbufsize, (char**) &destbuf, &destbufsize);

 done:
  return ret;
}
