################################################################
# Start of Makefile
#
# To use this, try the following:
# cd ../..
# rm -f config.cache && autoheader && autoconf && ./configure --enable-hc-boot && make -C ghc/includes boot
# cd ghc/interpreter
# make hugs Prelude.hs
# ./hugs -h300k
################################################################

# This rule goes first to make it the default choice
default		:: hugs 

RTS_DIR	= ..

CPPFLAGS	+= -I$(RTS_DIR)/includes 
CPPFLAGS	+= -D__HUGS__ 

CC		= gcc

CFLAGS		+= -Wall 
CFLAGS		+= -Wno-unused 
CFLAGS		+= -W
CFLAGS		+= -Wstrict-prototypes
CFLAGS		+= -Wmissing-prototypes 
CFLAGS		+= -Wmissing-declarations
#CFLAGS		+= -Wredundant-decls 
#CFLAGS		+= -Wnested-externs
#CFLAGS		+= -Wshadow
CFLAGS		+= -Winline
CFLAGS		+= -Waggregate-return

#CFLAGS		+= -ggdb3 -O0    # debug with gdb, minimal confusion
#CFLAGS		+= -pg -O2       # Profile with gprof
#CFLAGS		+= -pg -g        # Profile more with gprof
#CFLAGS		+= -pg -g -a     # Profile basic blocks with gprof (disnae work)
CFLAGS		+= -O2           # we just want to run it!
#CFLAGS		+= -O2 -g        # we just want to run it!

LIBS		+= -lm           # math - may not need it
#LIBS		+= -lreadline    # cool library
#LIBS		+= -lmcheck      # GNU extension - consistency check malloc usage
#LIBS		+= -ldl          # dynamic loading

YACC		= bison -y
RM		= /bin/rm -f

# grep include *.c | grep .c\"
#INCFILES	= parser.c preds.c kind.c timer.c scc.c

C_FILES		= $(wildcard *.c)
Y_FILES		= $(wildcard *.y)

OBJECTS		= hugs.o connect.o \
		  storage.o machdep.o charset.o input.o output.o \
		  static.o modules.o interface.o type.o subst.o link.o \
                  compiler.o desugar.o pmc.o pat.o derive.o \
		  stg.o translate.o pp.o \
		  codegen.o free.o lift.o stgSubst.o optimise.o \
		  dynamic.o

source:
	echo $(addsuffix .h,$(basename $(OBJECTS)))

################################################################
# Building the rts library
# 
# Two ways to link Hugs:
# o Static link to libHSrts.a using -rdynamic so that GHC generated .so files
#   can access libHSrts.a
# o Dynamic link to libHSrts.so
# There's not that much to choose between these two methods: static linking
# makes debugging easier, dynamic linking will make delivered systems
# smaller (if you use multiple clients).
################################################################

.PHONY: libgmp.a libHSrts.a libHSrts.so
libgmp.a:
	   @$(MAKE) -C $(RTS_DIR)/rts/gmp libgmp.a
libHSrts.a:
	   @$(MAKE) -C $(RTS_DIR)/rts -fadr libHSrts.a
libHSrts.so:
	   @$(MAKE) -C $(RTS_DIR)/rts -fadr libHSrts.so

# libHS_cbits.so: $(wildcard ../lib/std/cbits/*.c)
# 	  @$(MAKE) -C ../lib/std/cbits libHS_cbits.a
# 	  rm -f $@
# 	  $(CC) -shared $(patsubst %.c,%.o,$^) -L$(HOME)/lib -lc -o $@ 

libHS_cbits.so: $(wildcard ../lib/std/cbits/*.c)
	  rm -f $@
#	  $(CC) -shared $(CPPFLAGS) -D__CONCURRENT_HASKELL__ $^ -lc -o $@ 
#	  dlltool --def libHS_cbits.def --output-exp libHS_cbits.exp --output-lib libHS_cbits.a --dllname libHS_cbits.dll
	  $(CC) -c $(CPPFLAGS) -D__CONCURRENT_HASKELL__ $^ -lc
#	  $(LD) -o libHS_cbits.dll libHS_cbits.exp 

CBITS_C = $(wildcard ../lib/std/cbits/*.c)
CBITS_O = allocMem.o closeFile.o createDirectory.o directoryAux.o echoAux.o errno.o fileEOF.o fileGetc.o fileLookAhead.o fileObject.o filePosn.o filePutc.o fileSize.o flushFile.o freeFile.o getBufferMode.o getCPUTime.o getClockTime.o getCurrentDirectory.o getDirectoryContents.o getLock.o inputReady.o openFile.o readFile.o removeDirectory.o removeFile.o renameDirectory.o renameFile.o seekFile.o setBuffering.o setCurrentDirectory.o showTime.o system.o timezone.o toClockSec.o toLocalTime.o toUTCTime.o writeError.o writeFile.o                      

libHS_cbits.dll: $(CBITS_O)
	$(LD) --image-base=0x3000000 -o libHS_cbits.dll libHS_cbits.exp $(CBITS_O) -lcygwin -lkernel32

clean:: 
	rm -f libHS_cbits.so

LIBS += $(RTS_DIR)/rts/libHSrts.a $(RTS_DIR)/rts/gmp/libgmp.a
#LIBS += $(RTS_DIR)/rts/libHSrts.so
#LIBS += -L$(HOME)/lib -lgmp
#LIBS += -lbfd -liberty 
LIBS += -ladvapi32

SHARED_LINK = gcc -shared 


hugs		: $(OBJECTS) libHSrts.a libgmp.a
		  $(CC) $(LDFLAGS) $(CPPFLAGS) $(CFLAGS) $(OBJECTS) $(LIBS) -o hugs 
#		  $(CC) $(LDFLAGS) -rdynamic $(CPPFLAGS) $(CFLAGS) $(OBJECTS) $(LIBS) -o hugs 
		  
clean		::
		  $(RM) *.o *.so
		  $(RM) depends.mk
		  $(RM) TAGS
		  $(RM) hugs
#default 	:: TAGS
TAGS		::
		  etags *.[ych] ../{interpreter,rts,includes}/*.{y,hc,c,S,h}

# copied from the standard rules so that I can use @ to make it quieter
# you'll want to comment this out if you have to debug this Makefile
%.o		: %.c
		  @echo Compiling $<
		  @$(CC) $(CPPFLAGS) $(CFLAGS) -c $<

%.c		: %.y
		  -$(YACC) $<
		  mv y.tab.c $@
clean		::
		  $(RM) parser.c

################################################################
# Various test libraries:
################################################################

AB.so: A.o B.o
	$(SHARED_LINK) $^ -o $@

AB.myhi: A.myhi B.myhi
	cat $^ > $@


################################################################
# Building .so files I can load into Hugs
################################################################

.PRECIOUS: A.hc B.hc C.hc D.hc 

%.hc: %.hs
	$(RTS_DIR)/driver/ghc -recomp -i$(RTS_DIR)/lib/std -C $<

# The substitutions rename constructor wrappers to match our expectations
# And replace Z-encoded $ signs with underscores
%.c: %.hc
	echo "#include \"Rts.h\"" >  $@
	cat  $<                   >> $@
	perl -p -i -e "s/a_closure/A_closure/g" $@
	perl -p -i -e "s/b_closure/B_closure/g" $@
	perl -p -i -e "s/mkT_closure/MkT_closure/g" $@
	perl -p -i -e "s/Z36/_/g" $@

HCFLAGS += -DDEBUG=1 
HCFLAGS += -DDEBUG_EXTRA=1 
HCFLAGS += -ULAZY_BLACKHOLING -DEAGER_BLACKHOLING
HCFLAGS += -DUSE_MINIINTERPRETER=1 
HCFLAGS += -DINTERPRETER_ONLY=1 
HCFLAGS += -DNO_REGS

# The substitutions rename constructor wrappers to match our expectations
%.o: %.hc
	echo "#include \"Rts.h\"" >> $<.c
	cat  $<                   >> $<.c
	perl -p -i -e "s/Comb_a_closure/Comb_A_closure/g" $<.c
	perl -p -i -e "s/Comb_b_closure/Comb_B_closure/g" $<.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $(HCFLAGS) -xc -c $<.c -o $@

%.so: %.c
	gcc -shared $(CPPFLAGS) $(CFLAGS) $(HCFLAGS) $< -lm -lbfd -liberty -o $@

################################################################
# Floppy disk for me to take home at night
################################################################

# We avoid using zip because we're fed up being bitten by the
# default=non-recursive bug
floppy:		clean
		mount /mnt/floppy
		tar zcvf /mnt/floppy/stghugs.tgz . --exclude=CVS
		umount /mnt/floppy

################################################################
# Prelude
################################################################

# HPPFLAGS += "-DBEGIN_FOR_HUGS={-"
# HPPFLAGS += "-DEND_FOR_HUGS=-}"

HPP = gcc -E -P -traditional -xc -DSTD_PRELUDE=0 $(HPPFLAGS) $(CPPFLAGS) -Iprelude -Ilibrary -I.
UNLIT = ../utils/unlit/unlit

# we cleanup by deleting adjacent blank lines - which just happen to be the
# only duplicate adjacent lines in all the files we process
CLEANUP = uniq

# Fiendishly cunning this: 
# o PreludeBuiltin.hs contains the BODY of the libraries it requires.
# o All the other libraries just contain the HEAD of the file.
Prelude.hs	: $(wildcard prelude/*.hs) $(wildcard library/*.hs) $(wildcard ../lib/*/*.lhs) libHS_cbits.dll $(RTS_DIR)/includes/options.h
		@ echo Building PreludeBuiltin
		@ $(HPP) ../lib/std/PrelHandle.lhs     | $(UNLIT) - PrelHandle.unlit
		@ $(HPP) ../lib/std/PrelIOBase.lhs     | $(UNLIT) - PrelIOBase.unlit
		@ $(HPP) ../lib/std/PrelException.lhs  | $(UNLIT) - PrelException.unlit
		@ $(HPP) ../lib/std/PrelDynamic.lhs    | $(UNLIT) - PrelDynamic.unlit
		@ $(HPP) -DBODY ../lib/std/IO.lhs      | $(UNLIT) - IO.unlit
		@ $(HPP) -DHEAD ../lib/std/IO.lhs      | $(UNLIT) - IO.hs
		@ $(HPP) -DBODY prelude/Prelude.hs     | $(CLEANUP) > PreludeBuiltin.hs
		@ $(HPP) -DHEAD prelude/Prelude.hs     | $(CLEANUP) > Prelude.hs
		@ $(HPP) -DHEAD library/Array.hs       | $(CLEANUP) > Array.hs      
		@ $(HPP) -DHEAD library/Char.hs        | $(CLEANUP) > Char.hs       	  
		@ $(HPP) -DHEAD library/Ix.hs          | $(CLEANUP) > Ix.hs         	  
		@ $(HPP) -DHEAD library/List.hs        | $(CLEANUP) > List.hs       	  
		@ $(HPP) -DHEAD library/Maybe.hs       | $(CLEANUP) > Maybe.hs      	  
		@ $(HPP) -DHEAD library/Numeric.hs     | $(CLEANUP) > Numeric.hs    
		@ $(HPP) -DHEAD library/Ratio.hs       | $(CLEANUP) > Ratio.hs      
		@ $(HPP) -DHEAD library/UnicodePrims.hs| $(CLEANUP) > UnicodePrims.hs      
		@ $(HPP) -DHEAD prelude/PreludeIO.hs   | $(CLEANUP) > PreludeIO.hs  
		@ $(HPP) -DHEAD prelude/PreludeList.hs | $(CLEANUP) > PreludeList.hs
		@ $(HPP) -DHEAD prelude/PreludeText.hs | $(CLEANUP) > PreludeText.hs      
		@ $(HPP) -DHEAD prelude/PrelConc.hs    | $(CLEANUP) > PrelConc.hs
		@ echo "Building standard libraries"
		@ $(HPP) library/Complex.hs       > Complex.hs    	  
		@ $(HPP) library/Monad.hs         > Monad.hs      
		@ $(HPP) ../lib/std/System.lhs    > System.lhs  
		@ $(HPP) ../lib/std/Directory.lhs > Directory.lhs  	  
		@ $(HPP) ../lib/std/Locale.lhs    > Locale.lhs  
		@ $(HPP) ../lib/std/Random.lhs    > Random.lhs  
		@ $(HPP) ../lib/std/CPUTime.lhs   > CPUTime.lhs  
		@ $(HPP) ../lib/std/Time.lhs      > Time.lhs  
		@ echo "And some standard libraries which ain't done yet"
		@ # $(HPP) library/IO.hs            > IO.hs         	  
		@ #
		@ echo "Building Hugs-GHC libraries"
		@ $(HPP) ../lib/exts/ST.lhs        > ST.lhs     
		@ $(HPP) ../lib/misc/Pretty.lhs    > Pretty.lhs     
		@ $(HPP) ../lib/exts/IOExts.lhs    > IOExts.lhs     
		@ $(HPP) ../lib/exts/NumExts.lhs   > NumExts.lhs     
		@ $(HPP) ../lib/exts/Dynamic.lhs   > Dynamic.lhs     
		@ $(HPP) ../lib/exts/Bits.lhs      > Bits.lhs     
		@ $(HPP) ../lib/exts/Exception.lhs > Exception.lhs     
		@ $(HPP) library/Int.hs     > Int.hs     
		@ $(HPP) library/Word.hs    > Word.hs     
		@ $(HPP) ../lib/exts/Addr.lhs    > Addr.lhs     
		@ $(HPP) ../lib/concurrent/Channel.lhs    > Channel.lhs    
		@ $(HPP) ../lib/concurrent/ChannelVar.lhs > ChannelVar.lhs 
		@ $(HPP) ../lib/concurrent/Concurrent.lhs > Concurrent.lhs 
		@ $(HPP) ../lib/concurrent/Merge.lhs      > Merge.lhs      
		@ $(HPP) ../lib/concurrent/SampleVar.lhs  > SampleVar.lhs 
		@ $(HPP) ../lib/concurrent/Semaphore.lhs  > Semaphore.lhs 
		@ echo "And some libraries which ain't converted yet"
		@ # $(HPP) ../lib/exts/Foreign.lhs          > Foreign.lhs 
		@ #
		@ # $(HPP) ../lib/concurrent/Parallel.lhs   > Parallel.lhs   

clean		::
		$(RM) Array.hs           Dynamic.lhs        NumExts.lhs        Pretty.lhs
		$(RM) Bits.lhs           Exception.lhs      Numeric.hs         Ratio.hs
		$(RM) Channel.lhs        IOExts.lhs         PrelConc.hs        ST.lhs
		$(RM) ChannelVar.lhs     Ix.hs              Prelude.hs         SampleVar.lhs
		$(RM) Char.hs            List.hs            PreludeBuiltin.hs  Semaphore.lhs
		$(RM) Complex.hs         Maybe.hs           PreludeIO.hs       System.lhs
		$(RM) Concurrent.lhs     Merge.lhs          PreludeList.hs     UnicodePrims.hs
		$(RM) Directory.lhs      Monad.hs           PreludeText.hs
		$(RM) Locale.lhs Int.hs IO.hs Addr.lhs Time.lhs Word.hs
		$(RM) *.unlit

################################################################
# Greencard:
#
# This works - at least, it seems to:
#
# echo "runGreenCard \"--target ffi StdDIS.gc\"" | ./hugs -w -h500k ../../green-card/src/GreenCard.lhs
# make StdDIS_stub.so
# env LD_LIBRARY_PATH=. ./hugs -h300k StdDIS.hs 
################################################################


################################################################
# Regression tests (Unix only).  Run "make install" first
# 
# Uses runstdtest (from ghc-0.26/ghc/glafp-utils/scripts), perl 5
# and /bin/sh (Bourne shell).
#
# "make check" generates a lot of output to explain what is going on
# and reassure you that progress is being made.  This is great if you've
# never run these tests before - but if you just want to reassure yourself
# that nothing has broken since the last release, you might prefer to
# run this command which removes all the explanations and success
# stories - leaving just the useful output.
#
#  make check | grep -v "^--" -
#
################################################################

check		: hugs Prelude.hs
		  ./test/runtests test/static/*.hs
		  ./test/runtests test/typechecker/*.hs
		  ./test/runtests test/runtime/*.hs
		  ./test/runtests test/std/*.hs
		  ./test/runtests test/exts/*.hs
clean		::
		  $(RM) testFile

################################################################
# Dependencies
################################################################

DEP_FILES	= $(addsuffix .d,$(basename $(C_FILES)) $(basename $(Y_FILES)))

include $(DEP_FILES)

#Copied from the gmake manual - builds a dependency file for every C file
%.d		: %.c
		@echo "Making dependency file $@"
		@$(SHELL) -ec '$(CC) -MM $(CPPFLAGS) $< \
		 | sed '\''s/\($*\)\.o[ :]*/\1.o $@ : /g'\'' > $@ \
		 ; [ -s $@ ] || rm -f $@'

clean::
	$(RM) $(DEP_FILES)

################################################################
# End of Makefile
################################################################
