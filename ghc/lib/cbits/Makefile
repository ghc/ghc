# $Id: Makefile,v 1.2 1996/11/21 16:47:46 simonm Exp $

TOP = ../../..
UnlitSuffixRules = YES
include $(TOP)/ghc/mk/ghc.mk

ARCHIVE=libHS_cbits.a
DESTDIR=$(INSTLIBDIR_GHC)

SRCS=\
closeFile.lc		createDirectory.lc		\
errno.lc		fileEOF.lc			\
fileGetc.lc		fileLookAhead.lc		\
filePosn.lc		filePutc.lc			\
fileSize.lc		flushFile.lc			\
getBufferMode.lc	getCurrentDirectory.lc		\
getDirectoryContents.lc getLock.lc			\
inputReady.lc		openFile.lc			\
readFile.lc		removeDirectory.lc		\
removeFile.lc		renameDirectory.lc		\
renameFile.lc		seekFile.lc			\
setBuffering.lc		setCurrentDirectory.lc		\
system.lc		writeFile.lc

LIBOBJS = $(patsubst %.lc, %.o, $(SRCS))

%.o : %.c
	@$(RM) $@
	$(GHC) $(GHCFLAGS) -c $< -o $@

clean ::
	$(RM) $(SRCS:.lc=.c)

C_DEP_SRCS = $(SRCS) 
MKDEPENDC_OPTS = -I$(GHC_INCLUDES)

include $(TOP)/mk/lib.mk
