#ifndef STGIO_H
#define STGIO_H

#include "fileObject.h"

/* Decls for routines in ghc/lib/cbits/ only used there.
 * This file is used when compiling the Haskell library
 * that _ccalls_ those routines; and when compiling those
 * routines (to check consistency).
 */

/* closeFile.lc */
StgAddr allocMemory__ PROTO((StgInt));

/* closeFile.lc */
StgInt closeFile PROTO((StgForeignObj,StgInt));

/* createDirectory.lc */
StgInt createDirectory PROTO((StgByteArray));

/* directoryAux.lc */
StgAddr openDir__ PROTO((StgByteArray));
StgAddr readDir__ PROTO((StgAddr));

/* echoAux.lc */
StgInt setTerminalEcho PROTO((StgForeignObj, StgInt));
StgInt getTerminalEcho PROTO((StgForeignObj));
StgInt isTerminalDevice PROTO((StgForeignObj));

/* env.lc */
char *	strDup		PROTO((const char *));
int	setenviron	PROTO((char **));
int	copyenv		(STG_NO_ARGS);
int	_setenv		PROTO((char *));
int	delenv		PROTO((char *));

/* errno.lc */
extern	int ghc_errno;
extern	int ghc_errtype;
void	cvtErrno(STG_NO_ARGS);
void	stdErrno(STG_NO_ARGS);
StgAddr getErrStr__(STG_NO_ARGS);
StgInt  getErrNo__(STG_NO_ARGS);
StgInt  getErrType__(STG_NO_ARGS);

/* execvpe.lc */
int	execvpe PROTO((char *, char **, char **));

/* fileEOF.lc */
StgInt	fileEOF PROTO((StgForeignObj));
/* fileGetc.lc */
StgInt	fileGetc PROTO((StgForeignObj));

/* fileLookAhead.lc */
StgInt	fileLookAhead PROTO((StgForeignObj));
StgInt	ungetChar PROTO((StgForeignObj,StgChar));

/* fileObject.lc */
void    setBufFlags PROTO((StgForeignObj, StgInt));
void    setBufWPtr  PROTO((StgForeignObj, StgInt));
StgInt  getBufWPtr  PROTO((StgForeignObj));
void    setBuf      PROTO((StgForeignObj, StgAddr, StgInt));
StgAddr getBuf      PROTO((StgForeignObj));
StgAddr getWriteableBuf PROTO((StgForeignObj));
StgAddr getBufStart PROTO((StgForeignObj,StgInt));
StgInt  getBufSize  PROTO((StgForeignObj));
void    setFilePtr  PROTO((StgForeignObj, StgAddr));
StgAddr getFilePtr  PROTO((StgForeignObj));
void    setConnectedTo  PROTO((StgForeignObj, StgForeignObj, StgInt));
void    setPushbackBufSize PROTO((StgInt));
StgInt  getPushbackBufSize (STG_NO_ARGS);
void    setNonBlockingIOFlag__ PROTO((StgForeignObj));
void    clearNonBlockingIOFlag__ PROTO((StgForeignObj));
void    setConnNonBlockingIOFlag__ PROTO((StgForeignObj));
void    clearConnNonBlockingIOFlag__ PROTO((StgForeignObj));
StgInt  getFileFd  PROTO((StgForeignObj));
StgInt  getConnFileFd  PROTO((StgForeignObj));

/* filePosn.lc */
StgInt	getFilePosn PROTO((StgForeignObj));
StgInt	setFilePosn PROTO((StgForeignObj, StgInt));

/* filePutc.lc */
StgInt	filePutc    PROTO((StgForeignObj, StgChar));

/* fileSize.lc */
StgInt	fileSize    PROTO((StgForeignObj, StgByteArray));

/* flushFile.lc */
StgInt	flushFile   PROTO((StgForeignObj));
StgInt	flushBuffer PROTO((StgForeignObj));
StgInt	flushReadBuffer PROTO((StgForeignObj));

/* freeFile.lc */
void freeStdFile PROTO((StgForeignObj));
void freeFile PROTO((StgForeignObj));
void freeStdFileObject PROTO((StgForeignObj));
void freeFileObject PROTO((StgForeignObj));

/* getBufferMode.lc */
StgInt	getBufferMode PROTO((StgForeignObj));

/* getClockTime.lc */
StgInt	getClockTime PROTO((StgByteArray, StgByteArray));
StgAddr	showTime     PROTO((I_, StgByteArray, StgByteArray));
StgAddr	toClockSec   PROTO((I_, I_, I_, I_, I_, I_, I_, StgByteArray));
StgAddr	toLocalTime  PROTO((I_, StgByteArray, StgByteArray));
StgAddr	toUTCTime    PROTO((I_, StgByteArray, StgByteArray));

/* getCPUTime.lc */
StgByteArray getCPUTime PROTO((StgByteArray));
StgInt clockTicks();

/* getCurrentDirectory.lc */
StgAddr getCurrentDirectory(STG_NO_ARGS);

/* getLock.lc */
int     lockFile    PROTO((int, int));
int     unlockFile  PROTO((int));
StgInt	getLock	    PROTO((StgInt, StgInt));

/* inputReady.lc */
StgInt	inputReady  PROTO((StgForeignObj,StgInt));

/* openFile.lc */
IOFileObject* openFile    PROTO((StgByteArray, StgInt, StgInt, StgInt));
IOFileObject* openFd      PROTO((StgInt, StgInt, StgInt));
IOFileObject* openStdFile PROTO((StgInt, StgInt, StgInt));

/* readFile.lc */
StgInt	readBlock PROTO((StgForeignObj));
StgInt	readChunk PROTO((StgForeignObj,StgAddr,StgInt));
StgInt	readLine PROTO((StgForeignObj));
StgInt	readChar PROTO((StgForeignObj));

/* removeDirectory.lc */
StgInt removeDirectory PROTO((StgByteArray));

/* removeFile.lc */
StgInt removeFile PROTO((StgByteArray));

/* renameDirectory.lc */
StgInt renameDirectory PROTO((StgByteArray, StgByteArray));

/* renameFile.lc */
StgInt renameFile PROTO((StgByteArray, StgByteArray));

/* seekFile.lc */
StgInt	seekFile  PROTO((StgForeignObj, StgInt, StgInt, StgByteArray));
StgInt	seekFileP PROTO((StgForeignObj));

/* setBuffering.lc */
StgInt	setBuffering PROTO((StgForeignObj, StgInt));

/* setCurrentDirectory.lc */
StgInt setCurrentDirectory PROTO((StgByteArray));

/* showTime.lc */
StgAddr showTime PROTO((StgInt, StgByteArray, StgByteArray));

/* system.lc */
StgInt	systemCmd PROTO((StgByteArray));

/* toLocalTime.lc */
StgAddr toLocalTime PROTO((StgInt, StgByteArray, StgByteArray));

/* toUTCTime.lc */
StgAddr toUTCTime PROTO((StgInt, StgByteArray, StgByteArray));

/* toClockSec.lc */
StgAddr toClockSec PROTO((StgInt, StgInt, StgInt, StgInt, StgInt, StgInt, StgInt, StgByteArray));

/* writeError.lc */
void    writeErrString__ PROTO((StgAddr, StgByteArray, StgInt));
/* writeFile.lc */
StgInt	writeFile PROTO((StgAddr, StgForeignObj, StgInt));
StgInt	writeBuf  PROTO((StgForeignObj, StgAddr, StgInt));
StgInt	writeBufBA  PROTO((StgForeignObj, StgByteArray, StgInt));
StgInt	writeFileObject PROTO((StgForeignObj, StgInt));
StgInt	writeBuffer PROTO((StgForeignObj, StgInt));

#endif /* ! STGIO_H */
