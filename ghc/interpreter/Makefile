
# ----------------------------------------------------------------------------- #
# $Id: Makefile,v 1.8 1999/04/27 10:59:29 sewardj Exp $                         #
# ----------------------------------------------------------------------------- #

TOP = ../..
include $(TOP)/mk/boilerplate.mk

GHC_DIR = $(TOP)/ghc
RTS_DIR = $(TOP)/ghc/rts

# --------------------------------------------------------------------- #
# interpreter and relevant .a/.so files                                 #
# --------------------------------------------------------------------- #

YACC = bison -y
%.c: %.y
	-$(YACC) $<
	mv y.tab.c $@


HS_SRCS =

Y_SRCS = parser.y
C_SRCS = link.c type.c static.c storage.c derive.c input.c compiler.c subst.c \
     translate.c codegen.c lift.c free.c stgSubst.c optimise.c output.c   \
     hugs.c dynamic.c stg.c sainteger.c

SRC_CC_OPTS = -O2 -g -I$(GHC_DIR)/interpreter -I$(GHC_DIR)/includes -I$(GHC_DIR)/rts -D__HUGS__ -DCOMPILING_RTS -Wall -Wstrict-prototypes -Wno-unused

GHC_LIBS_NEEDED = $(TOP)/ghc/rts/libHSrts.a

all :: parser.c $(GHC_LIBS_NEEDED) nHandle.so hugs

### EXTREMELY hacky
hugs: $(C_OBJS) ../rts/Sanity.o ../rts/Assembler.o ../rts/Disassembler.o \
      ../rts/Evaluator.o ../rts/ForeignCall.o ../rts/GC.o ../rts/Printer.o \
      nHandle.so
	$(CC) -o $@ -rdynamic $(CC_OPTS) $^ $(GHC_LIBS_NEEDED) -lbfd -liberty -ldl -lm

nHandle.so:
	gcc -O -fPIC -shared -o nHandle.so nHandle.c

$(TOP)/ghc/rts/libHSrts.a:
	(cd $(TOP)/ghc/rts ; make clean ; make EXTRA_CC_OPTS=-I$(GHC_DIR)/interpreter)

cleanish:
	/bin/rm *.o

snapshot:
	/bin/rm -f snapshot.tar
	tar cvf snapshot.tar Makefile *.[chy] *-ORIG-* \
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

CLEAN_FILES += hugs nHandle.so
CLEAN_FILES += $(TOP)/ghc/rts/libHSrts.a $(TOP)/ghc/rts/*.o
CLEAN_FILES += parser.c

INSTALL_LIBEXECS = hugs

depend :: $(LOOPS) $(SRCS_UGNHS)


include $(TOP)/mk/target.mk


