
# ----------------------------------------------------------------------------- #
# $Id: Makefile,v 1.6 1999/03/09 14:51:03 sewardj Exp $                         #
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
     hugs.c dynamic.c stg.c

SRC_CC_OPTS = -O2 -Winline -g -I$(GHC_DIR)/includes -I$(GHC_DIR)/rts -D__HUGS__ -Wall -Wstrict-prototypes 

GHC_LIBS_NEEDED = $(TOP)/ghc/rts/libHSrts.a $(TOP)/ghc/rts/gmp/libgmp.a
GHC_DYN_CBITS_DIR = $(TOP)/ghc/lib/std/cbits
GHC_DYN_CBITS = $(GHC_DYN_CBITS_DIR)/libHS_cbits.so

all :: parser.c $(GHC_LIBS_NEEDED) $(GHC_DYN_CBITS) hugs

### EXTREMELY hacky
hugs: $(C_OBJS) ../rts/Sanity.o ../rts/Assembler.o ../rts/Disassembler.o ../rts/Evaluator.o ../rts/ForeignCall.o ../rts/GC.o \
                ../rts/Printer.o
	$(CC) -o $@ $(CC_OPTS) $^ $(GHC_LIBS_NEEDED) -lbfd -liberty -ldl -lm

$(GHC_DYN_CBITS):
###	(cd $(GHC_DYN_CBITS_DIR); make EXTRA_CC_OPTS="-fpic -optc-g" ; gcc -shared -o libHS_cbits.so *.o)
	(cd $(GHC_DYN_CBITS_DIR); rm -f *.o ; gcc -I../../../includes -fPIC -g -Wall -c *.c ; gcc -shared -o libHS_cbits.so *.o)
	cp -f $(GHC_DYN_CBITS) .

$(TOP)/ghc/rts/libHSrts.a:
	(cd $(TOP)/ghc/rts ; make clean ; make)
$(TOP)/ghc/rts/gmp/libgmp.a:
	(cd $(TOP)/ghc/rts/gmp ; make clean ; make)

cleanish:
	/bin/rm *.o

snapshot:
	/bin/rm -f snapshot.tar
	tar cvf snapshot.tar Makefile *.[chy] *-ORIG-* \
             ../rts/Assembler.c ../rts/Evaluator.c ../rts/Disassembler.c \
             ../rts/ForeignCall.c ../rts/Printer.c \
             ../includes/options.h ../includes/Assembler.h nHandle.c \
             ../includes/Assembler.h ../rts/Bytecodes.h \
             lib/*.hs


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

CLEAN_FILES += hugs libHS_cbits.so $(GHC_DYN_CBITS) $(GHC_DYN_CBITS_DIR)/*.o 
CLEAN_FILES += $(TOP)/ghc/rts/libHSrts.a $(TOP)/ghc/rts/*.o
CLEAN_FILES += $(TOP)/ghc/rts/gmp/libgmp.a  $(TOP)/ghc/rts/gmp/*.o $(TOP)/ghc/rts/gmp/*/*.o
CLEAN_FILES += parser.c

INSTALL_LIBEXECS = hugs

depend :: $(LOOPS) $(SRCS_UGNHS)


include $(TOP)/mk/target.mk


