/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-
 *
 * IO / Handle support.
 */
#ifndef __PRELIOUTILS_H__
#define __PRELIOUTILS_H__

/* PrelIOUtils.c */
extern HsBool prel_supportsTextMode();
extern HsInt  prel_bufsiz();
extern HsInt prel_seek_cur();
extern HsInt prel_seek_set();
extern HsInt prel_seek_end();

extern HsInt prel_o_binary();

extern HsInt prel_setmode(HsInt fd, HsBool isBin);

extern HsInt prel_PrelHandle_write(HsInt fd, HsBool isSock, HsAddr ptr, HsInt off, int sz);
extern HsInt prel_PrelHandle_read(HsInt fd, HsBool isSock, HsAddr ptr, HsInt off, int sz);

extern void* prel_PrelIO_memcpy(char *dst, HsInt dst_off, const char *src, size_t sz);

/* writeError.c */
extern void writeErrString__(HsAddr msg_hdr, HsAddr msg, HsInt len);

#endif /* __PRELIOUTILS_H__ */

