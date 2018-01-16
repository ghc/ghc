TOP=..
include $(TOP)/mk/boilerplate.mk

PROG = nofib-analyse

$(PROG): *.hs
	$(BOOT_HC) -O -cpp --make Main -o $(PROG)

all :: $(PROG)

boot :: $(PROG)

clean:
	rm -f $(PROG)
	rm -f CmdLine.hi GenUtils.hi Main.hi Slurp.hi
	rm -f CmdLine.o  GenUtils.o  Main.o  Slurp.o

