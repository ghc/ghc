
# ----------------------------------------------------------------------------- #
# $Id: Makefile,v 1.14 1999/10/29 14:33:57 sewardj Exp $                        #
# ----------------------------------------------------------------------------- #

TOP = ../..
include $(TOP)/mk/boilerplate.mk

GHC_DIR = $(TOP)/ghc
RTS_DIR = $(TOP)/ghc/rts

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

YACC = bison -y
%.c: %.y
	-$(YACC) $<
	mv y.tab.c $@
	rm -f input.o


HS_SRCS =

Y_SRCS = parser.y
C_SRCS = link.c type.c static.c storage.c derive.c input.c compiler.c subst.c \
     translate.c codegen.c lift.c free.c stgSubst.c optimise.c output.c   \
     hugs.c dynamic.c stg.c sainteger.c interface.c

SRC_CC_OPTS = -g -O -I$(GHC_DIR)/interpreter -I$(GHC_DIR)/includes -I$(GHC_DIR)/rts -D__HUGS__ -DCOMPILING_RTS -Wall -Wstrict-prototypes -Wno-unused -DDEBUG -DDEBUG_EXTRA -Winline

GHC_LIBS_NEEDED = $(TOP)/ghc/rts/libHSrts.a

all :: parser.c $(GHC_LIBS_NEEDED) nHandle.$(DYN_EXT) hugs

### EXTREMELY hacky
hugs: $(C_OBJS) ../rts/Sanity.o ../rts/Assembler.o ../rts/Disassembler.o \
      ../rts/Evaluator.o ../rts/ForeignCall.o ../rts/GC.o ../rts/Printer.o \
      ../rts/StgCRun.o
	$(CC) -o $@ $(CC_OPTS) $^ $(GHC_LIBS_NEEDED) -lbfd -liberty $(LIB_DL) -lm


nHandle.$(DYN_EXT): nHandle.c
ifeq "$(TARGETPLATFORM)" "i386-unknown-cygwin32"
	gcc -mno-cygwin -O -Wall -o nHandle.o -c nHandle.c
	dllwrap -mno-cygwin --target=i386-mingw32 -o nHandle.dll \
                -def nHandle.def nHandle.o
else
	gcc -O -Wall -shared -fPIC -o nHandle.so nHandle.c
endif

$(TOP)/ghc/rts/libHSrts.a:
	(cd $(TOP)/ghc/rts ; make clean ; make EXTRA_CC_OPTS=-I$(GHC_DIR)/interpreter)

cleanish:
	/bin/rm *.o

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


