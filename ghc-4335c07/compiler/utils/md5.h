/* MD5 message digest */
#pragma once

#include "HsFFI.h"

typedef HsWord32 word32;
typedef HsWord8  byte;

struct MD5Context {
        word32 buf[4];
        word32 bytes[2];
        word32 in[16];
};

void MD5Init(struct MD5Context *context);
void MD5Update(struct MD5Context *context, byte const *buf, int len);
void MD5Final(byte digest[16], struct MD5Context *context);
void MD5Transform(word32 buf[4], word32 const in[16]);
