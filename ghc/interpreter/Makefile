
# ----------------------------------------------------------------------------- #
# $Id: Makefile,v 1.2 1999/01/12 14:13:51 sewardj Exp $                        #
# ----------------------------------------------------------------------------- #

TOP = ../..
include $(TOP)/mk/boilerplate.mk

GHC_DIR = $(TOP)/ghc
RTS_DIR = $(TOP)/ghc/rts

# --------------------------------------------------------------------- #
# interpreter and relevant .a/.so files                                 #
# --------------------------------------------------------------------- #

HS_SRCS =

C_SRCS = \
  charset.c codegen.c compiler.c connect.c derive.c desugar.c \
  dynamic.c free.c hugs.c input.c interface.c lift.c link.c \
  machdep.c modules.c optimise.c output.c pat.c pmc.c pp.c static.c \
  stg.c stgSubst.c storage.c subst.c translate.c type.c

SRC_CC_OPTS = -g -I$(GHC_DIR)/includes -D__HUGS__

GHC_LIBS_NEEDED = $(TOP)/ghc/rts/libHSrts.a $(TOP)/ghc/rts/gmp/libgmp.a
GHC_DYN_CBITS_DIR = $(TOP)/ghc/lib/std/cbits
GHC_DYN_CBITS = $(GHC_DYN_CBITS_DIR)/libHS_cbits.so

all :: $(GHC_LIBS_NEEDED) $(GHC_DYN_CBITS) hugs

hugs: $(C_OBJS)
	$(CC) -rdynamic -o $@ $(CC_OPTS) $^ $(GHC_LIBS_NEEDED) -lbfd -liberty -ldl -lm

$(GHC_DYN_CBITS):
	(cd $(GHC_DYN_CBITS_DIR); make EXTRA_CC_OPTS=-optc-g ; gcc -shared -o libHS_cbits.so *.o)
	cp -f $(GHC_DYN_CBITS) .

$(TOP)/ghc/rts/libHSrts.a:
	(cd $(TOP)/ghc/rts ; make clean ; make)
$(TOP)/ghc/rts/gmp/libgmp.a:
	(cd $(TOP)/ghc/rts/gmp ; make clean ; make)


# --------------------------------------------------------------------- #
# Prelude                                                               #
# --------------------------------------------------------------------- #

# HPPFLAGS += "-DBEGIN_FOR_HUGS={-"
# HPPFLAGS += "-DEND_FOR_HUGS=-}"

CPPFLAGS        += -I$(GHC_DIR)/includes 
CPPFLAGS        += -D__HUGS__ 
HPP = gcc -E -P -traditional -xc -DSTD_PRELUDE=0 $(HPPFLAGS) $(CPPFLAGS) -Iprelude -Ilibrary -I.
UNLIT = ../utils/unlit/unlit

# we cleanup by deleting adjacent blank lines - which just happen to be the
# only duplicate adjacent lines in all the files we process
CLEANUP = uniq

# Fiendishly cunning this: 
# o PreludeBuiltin.hs contains the BODY of the libraries it requires.
# o All the other libraries just contain the HEAD of the file.
Prelude.hs      : $(wildcard prelude/*.hs) $(wildcard library/*.hs) $(wildcard ../lib/*/*.lhs)
	echo Building PreludeBuiltin
	$(HPP) ../lib/std/PrelHandle.lhs     | $(UNLIT) - PrelHandle.unlit
	$(HPP) ../lib/std/PrelIOBase.lhs     | $(UNLIT) - PrelIOBase.unlit
	$(HPP) ../lib/std/PrelException.lhs  | $(UNLIT) - PrelException.unlit
	$(HPP) ../lib/std/PrelDynamic.lhs    | $(UNLIT) - PrelDynamic.unlit
	$(HPP) -DBODY ../lib/std/IO.lhs      | $(UNLIT) - IO.unlit
	$(HPP) -DHEAD ../lib/std/IO.lhs      | $(UNLIT) - IO.hs
	$(HPP) -DBODY prelude/Prelude.hs     | $(CLEANUP) > PreludeBuiltin.hs
	$(HPP) -DHEAD prelude/Prelude.hs     | $(CLEANUP) > Prelude.hs
	$(HPP) -DHEAD library/Array.hs       | $(CLEANUP) > Array.hs      
	$(HPP) -DHEAD library/Char.hs        | $(CLEANUP) > Char.hs             
	$(HPP) -DHEAD library/Ix.hs          | $(CLEANUP) > Ix.hs               
	$(HPP) -DHEAD library/List.hs        | $(CLEANUP) > List.hs             
	$(HPP) -DHEAD library/Maybe.hs       | $(CLEANUP) > Maybe.hs            
	$(HPP) -DHEAD library/Numeric.hs     | $(CLEANUP) > Numeric.hs    
	$(HPP) -DHEAD library/Ratio.hs       | $(CLEANUP) > Ratio.hs      
	$(HPP) -DHEAD library/UnicodePrims.hs| $(CLEANUP) > UnicodePrims.hs      
	$(HPP) -DHEAD prelude/PreludeIO.hs   | $(CLEANUP) > PreludeIO.hs  
	$(HPP) -DHEAD prelude/PreludeList.hs | $(CLEANUP) > PreludeList.hs
	$(HPP) -DHEAD prelude/PreludeText.hs | $(CLEANUP) > PreludeText.hs      
	$(HPP) -DHEAD prelude/PrelConc.hs    | $(CLEANUP) > PrelConc.hs
	echo "Building standard libraries"
	$(HPP) library/Complex.hs       > Complex.hs            
	$(HPP) library/Monad.hs         > Monad.hs      
	$(HPP) ../lib/std/System.lhs    > System.lhs  
	$(HPP) ../lib/std/Directory.lhs > Directory.lhs         
	$(HPP) ../lib/std/Locale.lhs    > Locale.lhs  
	$(HPP) ../lib/std/Random.lhs    > Random.lhs  
	$(HPP) ../lib/std/CPUTime.lhs   > CPUTime.lhs  
	$(HPP) ../lib/std/Time.lhs      > Time.lhs  
	echo "And some standard libraries which ain't done yet"
	# $(HPP) library/IO.hs            > IO.hs               
	#
	echo "Building Hugs-GHC libraries"
	$(HPP) ../lib/exts/ST.lhs        > ST.lhs     
	$(HPP) ../lib/misc/Pretty.lhs    > Pretty.lhs     
	$(HPP) ../lib/exts/IOExts.lhs    > IOExts.lhs     
	$(HPP) ../lib/exts/NumExts.lhs   > NumExts.lhs     
	$(HPP) ../lib/exts/Dynamic.lhs   > Dynamic.lhs     
	$(HPP) ../lib/exts/Bits.lhs      > Bits.lhs     
	$(HPP) ../lib/exts/Exception.lhs > Exception.lhs     
	$(HPP) library/Int.hs     > Int.hs     
	$(HPP) library/Word.hs    > Word.hs     
	$(HPP) ../lib/exts/Addr.lhs    > Addr.lhs     
	$(HPP) ../lib/concurrent/Channel.lhs    > Channel.lhs    
	$(HPP) ../lib/concurrent/ChannelVar.lhs > ChannelVar.lhs 
	$(HPP) ../lib/concurrent/Concurrent.lhs > Concurrent.lhs 
	$(HPP) ../lib/concurrent/Merge.lhs      > Merge.lhs      
	$(HPP) ../lib/concurrent/SampleVar.lhs  > SampleVar.lhs 
	$(HPP) ../lib/concurrent/Semaphore.lhs  > Semaphore.lhs 
	echo "And some libraries which ain't converted yet"
	# $(HPP) ../lib/exts/Foreign.lhs          > Foreign.lhs 
	#
	# $(HPP) ../lib/concurrent/Parallel.lhs   > Parallel.lhs   

prelclean:
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


# --------------------------------------------------------------------- #
# Testing                                                               #
# --------------------------------------------------------------------- #

check :: all
	./test/runtests test/static/*.hs
	./test/runtests test/typechecker/*.hs
	./test/runtests test/runtime/*.hs
	./test/runtests test/std/*.hs
	./test/runtests test/exts/*.hs


# --------------------------------------------------------------------- #
# Cleanery & misc                                                       #
# --------------------------------------------------------------------- #

CLEAN_FILES += hugs libHS_cbits.so $(GHC_DYN_CBITS) $(GHC_DYN_CBITS_DIR)/*.o 
CLEAN_FILES += $(TOP)/ghc/rts/libHSrts.a $(TOP)/ghc/rts/*.o
CLEAN_FILES += $(TOP)/ghc/rts/gmp/libgmp.a  $(TOP)/ghc/rts/gmp/*.o $(TOP)/ghc/rts/gmp/*/*.o

INSTALL_LIBEXECS = hugs

clean :: prelclean

depend :: $(LOOPS) $(SRCS_UGNHS)


include $(TOP)/mk/target.mk


