/*
 * (c) sof, 1999
 *
 * Stubs to help implement Select module
 */
#ifndef __SELECTFROM_H__
#define __SELECTFROM_H__

extern StgInt sizeof_fd_set__();
extern void fd_zero__(StgByteArray fds);
extern void fd_set__(StgByteArray a, StgInt fd);
extern StgInt is_fd_set__(StgByteArray a, StgInt fd);
extern StgInt selectFrom__
            ( StgByteArray rfd
            , StgByteArray wfd
	    , StgByteArray efd
	    , StgInt mFd
	    , StgInt tout
	    );

#endif /* __SELECTFROM_H__ */
