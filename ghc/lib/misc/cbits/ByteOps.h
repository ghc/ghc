#ifndef BYTEOPS_H
#define BYTEOPS_H

/* "Native" support */
/* sigh again: without these some (notably "float") willnae work */
I_ long2bytes__	  PROTO((long,	unsigned char *));
I_ int2bytes__	  PROTO((int,	unsigned char *));
I_ short2bytes__  PROTO((short,	unsigned char *));
I_ float2bytes__  PROTO((float,	unsigned char *));
I_ double2bytes__ PROTO((double, unsigned char *));

I_ bytes2long__	  PROTO((P_, I_ *));
I_ bytes2int__	  PROTO((P_, I_ *));
I_ bytes2short__  PROTO((P_, I_ *));
I_ bytes2float__  PROTO((P_, StgFloat *));
I_ bytes2double__ PROTO((P_, StgDouble *));

#endif
