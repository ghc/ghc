/* MD5 message digest */
#ifndef _MD5_H
#define _MD5_H

typedef unsigned long word32;
typedef unsigned char byte;

struct MD5Context {
	word32 buf[4];
	word32 bytes[2];
	word32 in[16];
};

void MD5Init(StgByteArray context);
/*ORIG: void MD5Init(struct MD5Context *context);*/
void MD5Update(StgByteArray context, void *buf, int len);
/*ORIG: void MD5Update(struct MD5Context *context, byte const *buf, int len); */
void MD5Final(StgByteArray digest, StgByteArray context);
/*ORIG: void MD5Final(byte digest[16], struct MD5Context *context);*/

#endif /* _MD5_H */



