#ifndef BYTEOPS_H
#define BYTEOPS_H

/* "Native" support */
/* sigh again: without these some (notably "float") willnae work */
I_ long2bytes__	  (long,   unsigned char *);
I_ int2bytes__	  (int,	   unsigned char *);
I_ short2bytes__  (short,  unsigned char *);
I_ float2bytes__  (float,  unsigned char *);
I_ double2bytes__ (double, unsigned char *);

I_ bytes2long__	  (P_, I_ *);
I_ bytes2int__	  (P_, I_ *);
I_ bytes2short__  (P_, I_ *);
I_ bytes2float__  (P_, StgFloat *);
I_ bytes2double__ (P_, StgDouble *);

#endif
