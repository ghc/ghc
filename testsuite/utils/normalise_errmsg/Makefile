
NEMS_SRCS= Makefile

all: normalise_errmsg

boot:
	@echo "Make boot is not needed here"

normalise_errmsg: $(NEMS_SRCS)
	ghc-5.00.2 --make Main -o normalise_errmsg

clean:
	/bin/rm -f normalise_errmsg *.o *.hi

