/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: stgio.h,v 1.8 1998/12/02 13:27:58 simonm Exp $
 *
 * Helper code for GHC's IO subsystem.
 */

#ifndef STGIO_H
#define STGIO_H

#include "fileObject.h"

/* Decls for routines in ghc/lib/cbits/ only used there.
 * This file is used when compiling the Haskell library
 * that _ccalls_ those routines; and when compiling those
 * routines (to check consistency).
 */

#include "error.h"

/* closeFile.c */
StgAddr allocMemory__ (StgInt);

/* closeFile.c */
StgInt closeFile (StgForeignPtr,StgInt);

/* createDirectory.c */
StgInt createDirectory (StgByteArray);

/* directoryAux.c */
StgAddr openDir__         (StgByteArray);
StgAddr readDir__         (StgAddr);
StgAddr get_dirent_d_name (StgAddr);
StgWord get_stat_st_mode  (StgAddr);
StgInt64 get_stat_st_mtime(StgAddr);
void     set_stat_st_mtime(StgByteArray, StgByteArray);
StgInt  sizeof_stat       (void);
StgInt  prim_stat         (StgAddr,StgAddr);
StgInt  const_F_OK        (void);
StgWord const_S_IRUSR 	  (void);
StgWord const_S_IWUSR 	  (void);
StgWord const_S_IXUSR 	  (void);
StgInt  prim_S_ISDIR  	  (StgWord);
StgInt  prim_S_ISREG  	  (StgWord);

/* echoAux.c */
StgInt setTerminalEcho (StgForeignPtr, StgInt);
StgInt getTerminalEcho (StgForeignPtr);
StgInt isTerminalDevice (StgForeignPtr);

/* env.c */
char *	strDup		(const char *);
int	setenviron	(char **);
int	copyenv		(void);
int	_setenv		(char *);
int	delenv		(char *);

/* errno.c */
extern	int ghc_errno;
extern	int ghc_errtype;
extern	char* ghc_errstr;

void	cvtErrno(void);
void	stdErrno(void);
void    convertErrno(void);
StgAddr getErrStr__(void);
StgInt  getErrNo__(void);
StgInt  getErrType__(void);

/* execvpe.c */
int	execvpe (char *, char **, char **);

/* fileEOF.c */
StgInt	fileEOF (StgForeignPtr);
/* fileGetc.c */
StgInt	fileGetc (StgForeignPtr);

/* fileLookAhead.c */
StgInt	fileLookAhead (StgForeignPtr);
StgInt	ungetChar (StgForeignPtr,StgChar);

/* fileObject.c */
void    setBufFlags (StgForeignPtr, StgInt);
void    setBufWPtr  (StgForeignPtr, StgInt);
StgInt  getBufWPtr  (StgForeignPtr);
void    setBuf      (StgForeignPtr, StgAddr, StgInt);
StgAddr getBuf      (StgForeignPtr);
StgAddr getWriteableBuf (StgForeignPtr);
StgAddr getBufStart (StgForeignPtr,StgInt);
StgInt  getBufSize  (StgForeignPtr);
void    setFilePtr  (StgForeignPtr, StgAddr);
StgAddr getFilePtr  (StgForeignPtr);
void    setConnectedTo  (StgForeignPtr, StgForeignPtr, StgInt);
void    setPushbackBufSize (StgInt);
StgInt  getPushbackBufSize (void);
void    setNonBlockingIOFlag__ (StgForeignPtr);
void    clearNonBlockingIOFlag__ (StgForeignPtr);
void    setConnNonBlockingIOFlag__ (StgForeignPtr);
void    clearConnNonBlockingIOFlag__ (StgForeignPtr);
StgInt  getFileFd  (StgForeignPtr);
StgInt  getConnFileFd  (StgForeignPtr);
StgInt  fill_up_line_buffer(IOFileObject*);

/* filePosn.c */
StgInt	getFilePosn (StgForeignPtr);
StgInt	setFilePosn (StgForeignPtr, StgInt);

/* filePutc.c */
StgInt	filePutc    (StgForeignPtr, StgChar);

/* fileSize.c */
StgInt	fileSize    (StgForeignPtr, StgByteArray);
StgInt	fileSize_int64 (StgForeignPtr, StgByteArray);

/* flushFile.c */
StgInt	flushFile   (StgForeignPtr);
StgInt	flushBuffer (StgForeignPtr);
StgInt	flushReadBuffer (StgForeignPtr);

/* freeFile.c */
void freeStdFile (StgForeignPtr);
void freeFile (StgForeignPtr);
void freeStdFileObject (StgForeignPtr);
void freeFileObject (StgForeignPtr);

StgAddr ref_freeStdFileObject (void);
StgAddr ref_freeFileObject    (void);

/* getBufferMode.c */
StgInt	getBufferMode (StgForeignPtr);

/* getClockTime.c */
StgInt	getClockTime (StgByteArray, StgByteArray);
StgInt  prim_getClockTime(StgByteArray, StgByteArray);

/* getCPUTime.c */
StgByteArray getCPUTime (StgByteArray);
StgInt clockTicks(void);

/* getCurrentDirectory.c */
StgAddr getCurrentDirectory(void);

/* getDirectoryContents.c */
StgAddr getDirectoryContents (StgByteArray);

/* getLock.c */
int     lockFile    (int, int);
int     unlockFile  (int);
StgInt	getLock	    (StgInt, StgInt);

/* inputReady.c */
StgInt	inputReady  (StgForeignPtr, StgInt);

/* openFile.c */
IOFileObject* openFile    (StgByteArray, StgInt, StgInt, StgInt);
IOFileObject* openFd      (StgInt, StgInt, StgInt);
IOFileObject* openStdFile (StgInt, StgInt, StgInt);

/* progargs.c */
StgAddr get_prog_argv(void);
StgInt  get_prog_argc(void);

/* readFile.c */
StgInt	readBlock (StgForeignPtr);
StgInt	readChunk (StgForeignPtr,StgAddr,StgInt);
StgInt	readLine  (StgForeignPtr);
StgInt	readChar  (StgForeignPtr);

/* removeDirectory.c */
StgInt removeDirectory (StgByteArray);

/* removeFile.c */
StgInt removeFile (StgByteArray);

/* renameDirectory.c */
StgInt renameDirectory (StgByteArray, StgByteArray);

/* renameFile.c */
StgInt renameFile (StgByteArray, StgByteArray);

/* seekFile.c */
StgInt	seekFile  (StgForeignPtr, StgInt, StgInt, StgByteArray);
StgInt	seekFile_int64 (StgForeignPtr, StgInt, StgInt64);
StgInt	seekFileP (StgForeignPtr);

/* setBuffering.c */
StgInt	setBuffering (StgForeignPtr, StgInt);
StgInt  const_BUFSIZ (void);

/* setCurrentDirectory.c */
StgInt setCurrentDirectory (StgByteArray);

/* showTime.c */
StgAddr showTime (StgInt, StgByteArray, StgByteArray);

/* system.c */
StgInt	systemCmd (StgByteArray);

/* timezone.c */
StgInt get_tm_sec   ( StgAddr );
StgInt get_tm_min   ( StgAddr );
StgInt get_tm_hour  ( StgAddr );
StgInt get_tm_mday  ( StgAddr );
StgInt get_tm_mon   ( StgAddr );
StgInt get_tm_year  ( StgAddr );
StgInt get_tm_wday  ( StgAddr );
StgInt get_tm_yday  ( StgAddr );
StgInt get_tm_isdst ( StgAddr );
StgAddr prim_ZONE    ( StgAddr );
StgInt prim_GMTOFF  ( StgAddr );
StgInt prim_SETZONE ( StgAddr, StgAddr );
StgInt sizeof_word      ( void ); 
StgInt sizeof_struct_tm	( void );
StgInt sizeof_time_t    ( void );

/* toLocalTime.c */
StgAddr toLocalTime (StgInt, StgByteArray, StgByteArray);
StgInt prim_toLocalTime ( StgInt64,StgByteArray );

/* toUTCTime.c */
StgAddr toUTCTime (StgInt, StgByteArray, StgByteArray);
StgInt prim_toUTCTime ( StgInt64,StgByteArray );

/* toClockSec.c */
StgAddr toClockSec (StgInt, StgInt, StgInt, StgInt, StgInt, StgInt, StgInt, StgByteArray);
StgInt prim_toClockSec(StgInt, StgInt, StgInt, StgInt, StgInt, StgInt, StgInt, StgByteArray);

/* writeError.c */
void    writeErrString__ (StgAddr, StgByteArray, StgInt);
/* writeFile.c */
StgInt	writeBuf  (StgForeignPtr, StgAddr, StgInt);
StgInt	writeBufBA  (StgForeignPtr, StgByteArray, StgInt);
StgInt	writeFileObject (StgForeignPtr, StgInt);
StgInt	writeBuffer (StgForeignPtr, StgInt);

#endif /* ! STGIO_H */


