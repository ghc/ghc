#ifndef STGIO_H
#define STGIO_H

/* Decls for routines in ghc/runtime/io/ only used there.
 * This file is used when compiling the Haskell library
 * that _ccalls_ those routines; and when compiling those
 * routines (to check consistency).
 */

/* closeFile.lc */
StgInt closeFile PROTO((StgAddr));

/* createDirectory.lc */
StgInt createDirectory PROTO((StgByteArray));

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

/* execvpe.lc */
int	execvpe PROTO((char *, char **, char **));

/* fileEOF.lc */
StgInt	fileEOF PROTO((StgAddr));

/* fileGetc.lc */
StgInt	fileGetc PROTO((StgAddr));

/* fileLookAhead.lc */
StgInt	fileLookAhead PROTO((StgAddr));

/* filePosn.lc */
StgInt	getFilePosn PROTO((StgAddr));
StgInt	setFilePosn PROTO((StgAddr, StgInt));

/* filePutc.lc */
StgInt	filePutc    PROTO((StgAddr, StgInt));

/* fileSize.lc */
StgInt	fileSize    PROTO((StgAddr, StgByteArray));

/* flushFile.lc */
StgInt	flushFile   PROTO((StgAddr));

/* getBufferMode.lc */
StgInt	getBufferMode PROTO((StgAddr));

/* getClockTime.lc */
StgInt	getClockTime PROTO((StgByteArray, StgByteArray));

/* getCPUTime.lc */
StgByteArray getCPUTime PROTO((StgByteArray));

/* getCurrentDirectory.lc */
StgAddr getCurrentDirectory(STG_NO_ARGS);

/* getDirectoryContents.lc */
StgAddr getDirectoryContents PROTO((StgByteArray));

/* getLock.lc */
int     lockFile    PROTO((int, int));
void    unlockFile  PROTO((int));
StgInt	getLock	    PROTO((StgAddr, StgInt));

/* inputReady.lc */
StgInt	inputReady  PROTO((StgAddr));

/* openFile.lc */
StgAddr openFile PROTO((StgByteArray, StgByteArray));

/* readFile.lc */
StgInt	readBlock PROTO((StgAddr, StgAddr, StgInt));
StgInt	readLine PROTO((StgAddr, StgAddr, StgInt));
StgInt	readChar PROTO((StgAddr));

/* removeDirectory.lc */
StgInt removeDirectory PROTO((StgByteArray));

/* removeFile.lc */
StgInt removeFile PROTO((StgByteArray));

/* renameDirectory.lc */
StgInt renameDirectory PROTO((StgByteArray, StgByteArray));

/* renameFile.lc */
StgInt renameFile PROTO((StgByteArray, StgByteArray));

/* seekFile.lc */
StgInt	seekFile  PROTO((StgAddr, StgInt, StgInt, StgByteArray));
StgInt	seekFileP PROTO((StgAddr));

/* setBuffering.lc */
StgInt	setBuffering PROTO((StgAddr, StgInt));

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

/* writeFile.lc */
StgInt	writeFile PROTO((StgAddr, StgAddr, StgInt));

#endif /* ! STGIO_H */
