
# --------------------------------------------------------------------------- #
# $Id: Makefile,v 1.24 1999/12/20 16:55:26 sewardj Exp $                      #
# --------------------------------------------------------------------------- #

TOP = ..
include $(TOP)/mk/boilerplate.mk
SUBDIRS = lib

# --------------------------------------------------------------------- #
# interpreter and relevant .a/.so files                                 #
# --------------------------------------------------------------------- #

ifeq "$(TARGETPLATFORM)" "i386-unknown-cygwin32"
DYN_EXT=.dll
LIB_DL=
else
DYN_EXT=.so
LIB_DL=-ldl
endif

ifeq "$(HaveLibReadline)$" "YES"
LIB_READLINE=-lreadline -ltermcap
else
LIB_READLINE=
endif

YACC = bison -y
%.c: %.y
	-$(YACC) $<
	mv y.tab.c $@
	rm -f input.o

HS_SRCS =

Y_SRCS = parser.y
C_SRCS = link.c type.c static.c storage.c derive.c input.c compiler.c subst.c \
     translate.c codegen.c lift.c free.c stgSubst.c output.c   \
     hugs.c dynamic.c stg.c sainteger.c object.c interface.c

SRC_CC_OPTS = -g -O -I$(GHC_INTERPRETER_DIR) -I$(GHC_INCLUDE_DIR) -I$(GHC_RUNTIME_DIR) -D__HUGS__ -DCOMPILING_RTS -Wall -Wstrict-prototypes -Wno-unused -DDEBUG -Winline

GHC_LIBS_NEEDED = $(GHC_RUNTIME_DIR)/libHSrts.a

all :: parser.c $(GHC_LIBS_NEEDED) nHandle$(DYN_EXT) hugs

### EXTREMELY hacky
hugs: $(C_OBJS) ../rts/Sanity.o ../rts/Assembler.o ../rts/Disassembler.o \
      ../rts/Evaluator.o ../rts/ForeignCall.o ../rts/GC.o ../rts/Printer.o \
      ../rts/StgCRun.o
	$(CC) -o $@ $(CC_OPTS) $^ $(GHC_LIBS_NEEDED) \
		 -lbfd -liberty $(LIB_READLINE) $(LIB_DL) -lgmp -lm

nHandle$(DYN_EXT): nHandle.c
ifeq "$(TARGETPLATFORM)" "i386-unknown-cygwin32"
	gcc -mno-cygwin -O -Wall -o nHandle.o -c nHandle.c
	dllwrap -mno-cygwin --target=i386-mingw32 -o nHandle.dll \
                -def nHandle.def nHandle.o
else
	gcc -O -Wall -shared -fPIC -o nHandle.so nHandle.c
endif

$(GHC_RUNTIME_DIR)/libHSrts.a:
	(cd $(GHC_RUNTIME_DIR) ; make clean ; make EXTRA_CC_OPTS=-I$(GHC_INTERPRETER_DIR))

cleanish:
	/bin/rm *.o
rtsclean:
	(cd $(GHC_RUNTIME_DIR) ; make clean)

binaries:
	tar cvzf stghugs.tar.gz hugs$(exeext) nHandle$(DYN_EXT) lib/*lhs lib/Prelude.hs

snapshot:
	/bin/rm -f snapshot.tar
	tar cvf snapshot.tar Makefile *.[chy] \
             ../rts/Assembler.c ../rts/Evaluator.c ../rts/Disassembler.c \
             ../rts/ForeignCall.c ../rts/Printer.c ../rts/QueueTemplate.h \
             ../includes/options.h ../includes/Assembler.h nHandle.c \
             ../includes/Assembler.h ../rts/Bytecodes.h ../includes/ClosureMacros.h \
             lib/*.hs runnofib runallnofib


# --------------------------------------------------------------------- #
# Testing                                                               #
# --------------------------------------------------------------------- #

check :: all
	./test/runtests test/static/*.hs
	./test/runtests test/typechecker/*.hs
	./test/runtests test/runtime/*.hs
	./test/runtests test/std/*.hs
	./test/runtests test/exts/*.hs

checkrun: all
	./test/runtests test/runtime/*.hs
	./test/runtests test/std/*.hs
	./test/runtests test/exts/*.hs

# --------------------------------------------------------------------- #
# Cleanery & misc                                                       #
# --------------------------------------------------------------------- #

CLEAN_FILES += hugs nHandle.dll
CLEAN_FILES += $(TOP)/ghc/rts/libHSrts.a $(TOP)/ghc/rts/*.o
CLEAN_FILES += parser.c

INSTALL_LIBEXECS = hugs

depend :: $(LOOPS) $(SRCS_UGNHS)


include $(TOP)/mk/target.mk


