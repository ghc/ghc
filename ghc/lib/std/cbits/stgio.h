#ifndef STGIO_H
#define STGIO_H

/* Decls for routines in ghc/lib/cbits/ only used there.
 * This file is used when compiling the Haskell library
 * that _ccalls_ those routines; and when compiling those
 * routines (to check consistency).
 */

/* closeFile.lc */
StgInt closeFile PROTO((StgForeignObj));

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
StgInt	fileEOF PROTO((StgForeignObj));
/* fileGetc.lc */
StgInt	fileGetc PROTO((StgForeignObj));

/* fileLookAhead.lc */
StgInt	fileLookAhead PROTO((StgForeignObj));

/* filePosn.lc */
StgInt	getFilePosn PROTO((StgForeignObj));
StgInt	setFilePosn PROTO((StgForeignObj, StgInt));

/* filePutc.lc */
StgInt	filePutc    PROTO((StgForeignObj, StgInt));

/* fileSize.lc */
StgInt	fileSize    PROTO((StgForeignObj, StgByteArray));

/* flushFile.lc */
StgInt	flushFile   PROTO((StgForeignObj));

/* freeFile.lc */
void freeStdFile PROTO((StgForeignObj));
void freeFile PROTO((StgForeignObj));

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

/* getDirectoryContents.lc */
StgAddr getDirectoryContents PROTO((StgByteArray));

/* getLock.lc */
int     lockFile    PROTO((int, int));
int     unlockFile  PROTO((int));
StgInt	getLock	    PROTO((StgForeignObj, StgInt));

/* inputReady.lc */
StgInt	inputReady  PROTO((StgForeignObj,StgInt));

/* openFile.lc */
StgAddr openFile PROTO((StgByteArray, StgByteArray));

/* readFile.lc */
StgInt	readBlock PROTO((StgAddr, StgForeignObj, StgInt));
StgInt	readLine PROTO((StgAddr,  StgForeignObj, StgInt));
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

/* writeFile.lc */
StgInt	writeFile PROTO((StgAddr, StgForeignObj, StgInt));

#endif /* ! STGIO_H */
