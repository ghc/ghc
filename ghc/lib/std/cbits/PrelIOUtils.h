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

extern HsInt prel_sizeof_stat();
extern time_t prel_st_mtime(struct stat* st);
extern off_t  prel_st_size(struct stat* st);
extern mode_t prel_st_mode(struct stat* st);

extern HsInt prel_sizeof_termios();
extern HsInt prel_sizeof_sigset_t();

#if HAVE_TERMIOS_H
extern tcflag_t prel_lflag(struct termios* ts);
extern void     prel_poke_lflag(struct termios* ts, tcflag_t t);
extern unsigned char* prel_ptr_c_cc(struct termios* ts);
#endif

extern int prel_o_binary();
extern int prel_o_rdonly();
extern int prel_o_wronly();
extern int prel_o_rdwr();
extern int prel_o_append();
extern int prel_o_creat();
extern int prel_o_excl();
extern int prel_o_trunc();
extern int prel_o_noctty();
extern int prel_o_nonblock();

extern int prel_echo();
extern int prel_tcsanow();
extern int prel_icanon();
extern int prel_vmin();
extern int prel_vtime();
extern int prel_sigttou();
extern int prel_sig_block();
extern int prel_sig_setmask();
extern int prel_f_getfl();
extern int prel_f_setfl();

extern HsInt prel_setmode(HsInt fd, HsBool isBin);

extern HsInt prel_PrelHandle_write(HsInt fd, HsBool isSock, HsAddr ptr, HsInt off, int sz);
extern HsInt prel_PrelHandle_read(HsInt fd, HsBool isSock, HsAddr ptr, HsInt off, int sz);

extern void* prel_PrelIO_memcpy(char *dst, HsInt dst_off, const char *src, HsInt src_off, size_t sz);

/* writeError.c */
extern void writeErrString__(HsAddr msg_hdr, HsAddr msg, HsInt len);

extern int s_isreg_PrelPosix_wrap(int);
extern int s_isdir_PrelPosix_wrap(int);
extern int s_isfifo_PrelPosix_wrap(int);
extern int s_isblk_PrelPosix_wrap(int);
extern int s_ischr_PrelPosix_wrap(int);
#ifndef mingw32_TARGET_OS
extern int s_issock_PrelPosix_wrap(int);
extern void sigemptyset_PrelPosix_wrap(sigset_t *set);
#endif


#endif /* __PRELIOUTILS_H__ */

