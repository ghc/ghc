include ../../../lib/std/Makefile.src

# These settings are if you use a visual studio build
CVS=cvs
CORENV_DEBUG=
CORENV_RETAIL=
LOCALRUN=./
ILX_FAST=x
ifeq ($(HOSTNAME),msrc-hilda)
CORENV_DEBUG="call devvsnearerb1gen.bat"
CORENV_RETAIL="call devvsnearerb1gen.bat retail"
LOCALRUN=.\\
ILX_FAST=
endif    

ILXASM_HOME=C:/devel/fcom/src
ILXASM=$(ILXASM_HOME)/bin/ilxasm$(ILX_FAST).exe 
ILVALID=$(ILXASM_HOME)/bin/ilvalid$(ILX_FAST).exe 
ILXASM_FLAGS=-l $(ILXASM_HOME)/ilxasm --no-ilasm  --tailcall-indirect

ghc:
	$(MAKE) -C ../.. ghc

ilxasm:
	$(MAKE) -C $(ILXASM_HOME) bin/ilxasm.exe

ilxasmx:
	$(MAKE) -C $(ILXASM_HOME) bin/ilxasmx.exe

ilvalid:
	$(MAKE) -C $(ILXASM_HOME) bin/ilvalid.exe

ilvalidx:
	$(MAKE) -C $(ILXASM_HOME) bin/ilvalidx.exe

prel:
	$(MAKE) -C ../../../lib/std ilxasm std.Onot.nongeneric.boxed.dll
	$(MAKE) -C $(ILXASM_HOME)  bin/msilxlib.nongeneric.boxed.dll

prelq:
	$(MAKE) -C ../../../lib/std ilxasm std.Onot.nongeneric.boxed.trial.dll
	$(MAKE) -C $(ILXASM_HOME)  bin/msilxlib.nongeneric.boxed.trial.dll

oprel:
	$(MAKE) -C ../../../lib/std ilxasm std.O.nongeneric.boxed.dll
	$(MAKE) -C $(ILXASM_HOME)  bin/msilxlib.nongeneric.boxed.dll

oprelq:
	$(MAKE) -C ../../../lib/std ilxasm std.O.nongeneric.boxed.trial.dll
	$(MAKE) -C $(ILXASM_HOME)  bin/msilxlib.nongeneric.boxed.trial.dll

tprel:
	$(MAKE) -C ../../../lib/std ilxasm std.Onot.nongeneric.boxed.traced.dll
	$(MAKE) -C $(ILXASM_HOME)  bin/msilxlib.nongeneric.boxed.traced.dll

otprel:
	$(MAKE) -C ../../../lib/std ilxasm std.O.nongeneric.boxed.traced.dll
	$(MAKE) -C $(ILXASM_HOME)  bin/msilxlib.nongeneric.boxed.traced.dll

#gprel:
#	$(MAKE) -C ../../../lib/std ilxasm std.Onot.generic.dll
#	$(MAKE) -C $(ILXASM_HOME)  bin/msilxlib.generic.dll
#
#ogprel:
#	$(MAKE) -C ../../../lib/std ilxasm std.O.generic.dll
#	$(MAKE) -C $(ILXASM_HOME)  bin/msilxlib.generic.dll
#
#gtprel:
#	$(MAKE) -C ../../../lib/std ilxasm std.Onot.generic.traced.dll
#	$(MAKE) -C $(ILXASM_HOME)  bin/msilxlib.generic.traced.dll

vgprel:
	$(MAKE) -C ../../../lib/std ilxasm std.Onot.vmeth-erased.generic.dll
	$(MAKE) -C $(ILXASM_HOME)  bin/msilxlib.vmeth-erased.generic.dll

ovgprel:
	$(MAKE) -C ../../../lib/std ilxasm std.O.vmeth-erased.generic.dll
	$(MAKE) -C $(ILXASM_HOME)  bin/msilxlib.vmeth-erased.generic.dll

ovgprelq:
	$(MAKE) -C ../../../lib/std ilxasm std.O.vmeth-erased.generic.trial.dll
	$(MAKE) -C $(ILXASM_HOME)  bin/msilxlib.vmeth-erased.generic.trial.dll

vgtprel:
	$(MAKE) -C ../../../lib/std ilxasm std.Onot.vmeth-erased.generic.traced.dll
	$(MAKE) -C $(ILXASM_HOME)  bin/msilxlib.vmeth-erased.generic.traced.dll

ovgtprel:
	$(MAKE) -C ../../../lib/std ilxasm std.O.vmeth-erased.generic.traced.dll
	$(MAKE) -C $(ILXASM_HOME)  bin/msilxlib.vmeth-erased.generic.traced.dll

%.o: %.hs ../../ghc-4.11
	../../ghc-inplace -o $@ -c $*.hs

std_NONGENERIC_STATIC_IL=$(patsubst %.lhs,../../../lib/std/%.nongeneric.boxed.static.il,$(std_SRC)) ../../../lib/std/PrelGHC.nongeneric.boxed.static.il
std_GENERIC_STATIC_IL=$(patsubst %.lhs,../../../lib/std/%.generic.static.il,$(std_SRC)) ../../../lib/std/PrelGHC.generic.static.il

#========================================================================
# 1. From Haskell to ILX 

%.Onot.ilx: %.hs ../../ghc-4.11
	../../ghc-inplace -c -fglasgow-exts -o $@ -i../../../lib/std/.Onot -Onot -filx $*.hs -osuf Onot.ilx

%.O.ilx: %.hs ../../ghc-4.11
	../../ghc-inplace -c -fglasgow-exts -o $@ -i../../../lib/std/.O -O -filx $*.hs -osuf O.ilx

../Entry.Onot.ilx: ../Entry.ilx
	sed -e "s|ilx std|ilx std.Onot|g" ../Entry.ilx > $@.tmp
	mv $@.tmp $@

%.Onot.dlllib.ilx: %.Onot.ilx ../Entry.Onot.ilx 
	cat ../Entry.Onot.ilx  $*.Onot.ilx > $@.tmp
	mv $@.tmp $@

../Entry.O.ilx: ../Entry.ilx
	sed -e "s|ilx std|ilx std.O|g" ../Entry.ilx > $@.tmp
	mv $@.tmp $@

%.O.dlllib.ilx: %.O.ilx ../Entry.O.ilx 
	cat ../Entry.O.ilx  $*.O.ilx > $@.tmp
	mv $@.tmp $@

HSstd_cbits.dll: ../../../lib/std/cbits/HSstd_cbits.dll
	cp $< $@


#========================================================================
# 2. From ILX to IL

#------------------------------------------------------------------------
# Compile for a vanilla VM against a vanilla library organised as a
# seperate assembly/DLL.

%.nongeneric.boxed.dlllib.il: $(ILXASM) %.dlllib.ilx
	$(ILXASM) --box-everything --no-stdlib $(ILXASM_FLAGS) -o $@.tmp $*.dlllib.ilx
	mv $@.tmp $@

#------------------------------------------------------------------------
# Same
# 	- running a trial optimization 
# 	- for traced code on a vanilla VM
%.nongeneric.boxed.trial.dlllib.il: $(ILXASM) %.dlllib.ilx
	$(ILXASM) --trial-opt --box-everything --no-stdlib $(ILXASM_FLAGS) -o $@.tmp $*.dlllib.ilx
	mv $@.tmp $@

%.nongeneric.boxed.traced.dlllib.il: $(ILXASM) %.dlllib.ilx
	$(ILXASM) --box-everything --trace-il --no-stdlib $(ILXASM_FLAGS) -o $@.tmp $*.dlllib.ilx
	mv $@.tmp $@

#------------------------------------------------------------------------
# Same, for a generic library and generic VM

%.generic.dlllib.il: $(ILXASM) %.dlllib.ilx
	$(ILXASM) --no-pp --no-stdlib $(ILXASM_FLAGS) -o  $@.tmp $*.dlllib.ilx  
	mv $@.tmp $@

%.generic.trial.dlllib.il: $(ILXASM) %.dlllib.ilx
	$(ILXASM) --trial-opt --no-pp --no-stdlib $(ILXASM_FLAGS) -o  $@.tmp $*.dlllib.ilx  
	mv $@.tmp $@

%.generic.traced.dlllib.il: $(ILXASM) %.dlllib.ilx
	$(ILXASM) --trace-il --no-pp --no-stdlib $(ILXASM_FLAGS) -o  $@.tmp $*.dlllib.ilx  
	mv $@.tmp $@

%.vmeth-erased.generic.dlllib.il: $(ILXASM) %.dlllib.ilx
	$(ILXASM) --poly-virtual-method-erase --no-pp --no-stdlib $(ILXASM_FLAGS) -o  $@.tmp $*.dlllib.ilx  
	mv $@.tmp $@

%.vmeth-erased.generic.trial.dlllib.il: $(ILXASM) %.dlllib.ilx
	$(ILXASM) --trial-opt --poly-virtual-method-erase --no-pp --no-stdlib $(ILXASM_FLAGS) -o  $@.tmp $*.dlllib.ilx  
	mv $@.tmp $@

%.vmeth-erased.generic.traced.dlllib.il: $(ILXASM) %.dlllib.ilx
	$(ILXASM) --poly-virtual-method-erase --trace-il --no-pp --no-stdlib $(ILXASM_FLAGS) -o  $@.tmp $*.dlllib.ilx  
	mv $@.tmp $@


#------------------------------------------------------------------------
# Compile for a vanilla VM against a vanilla library compiled to IL code
# to be statically linked as one big module.  We hack this up by textually
# stripping out all the assembly qualifications (apart from mscorlib) 
# from the ILX forward files and IL code itself.  We then just 
# concatenate all the IL code together and compile it as a single .EXE.

%.staticlib.ilx: %.ilx ../Entry.ilx 
	cat  ../Entry.ilx  $*.ilx | \
		sed -e "sQ\[std\]QQg" |   \
		sed -e "sQ\['std'\]QQg" > $@.tmp
	cat $@.tmp > $@.tmp2
	mv $@.tmp2 $@
	rm $@.tmp


%.generic.staticlib.il: $(ILXASM) %.staticlib.ilx
	$(ILXASM) --static --no-pp $(ILXASM_FLAGS) -o $@.tmp $*.staticlib.ilx  
	cat $(std_GENERIC_STATIC_IL) $@.tmp > $@.tmp2
	mv $@.tmp2 $@
	rm $@.tmp


%.nongeneric.boxed.staticlib.il: $(ILXASM) %.staticlib.ilx
	$(ILXASM) --box-everything --static $(ILXASM_FLAGS) -o $@.tmp $*.staticlib.ilx
	cat $(std_NONGENERIC_STATIC_IL) $@.tmp > $@.tmp2
	mv $@.tmp2 $@
	rm $@.tmp


#------------------------------------------------------------------------
# For compiling test cases that don't use the standard library at all.

%.nolib.ilx: %.ilx PrelBase.test.ilx   ../PrelGHC.ilx ../Entry.ilx 
	cat ../PrelGHC.ilx   PrelBase.test.ilx ../Entry.ilx  $*.ilx > $@.tmp
	mv $@.tmp $@

%.nolib.il: $(ILXASM) $(ILXASM_HOME)/ilxasm/stdlib-func-by-mcalli.ilx %.nolib.ilx
	$(ILXASM) $(ILXASM_FLAGS) -o $@.tmp $*.nolib.ilx 
	mv $@.tmp $@

%.nolib.traced.il: $(ILXASM) $(ILXASM_HOME)/ilxasm/stdlib-func-by-mcalli.ilx %.nolib.ilx
	$(ILXASM) --trace-il $(ILXASM_FLAGS) $*.nolib.ilx > $@.tmp
	mv $@.tmp $@


#------------------------------------------------------------------------
# From IL to .EXE

%.retail.exe: %.il ../Entry.Onot.mono.il
	cat %.il ../Entry.Onot.mono.il > $@.tmp
	echo "$(CORENV_RETAIL)" > $@.bat
	echo "ilasm -exe -quiet -out=$(subst /,\\,$@.tmp) $(subst /,\\,$<)" >> $@.bat
	time -p cmd /c $(subst /,\\,$@).bat
	rm $@.bat

%.debug.exe: %.il
	echo "$(CORENV_RETAIL)" > $@.bat
	echo "ilasm -exe -quiet -debug -out=$(subst /,\\,$@) $(subst /,\\,$<)" >> $@.bat
	time -p cmd /c $(subst /,\\,$@).bat
	rm $@.bat
#------------------------------------------------------------------------
# From .HS to .EXE without using ILX
# Used to run performance comparisons against native code GHC

%.Onot.exe: %.hs
	ghc -Onot -o $@ $<

%.O.exe: %.hs
	ghc -O -o $@ $<

%.o: %.hs ../../hsc.exe
	../../../driver/ghc-inplace -o $@ -c $*.hs

%.run: %.exe
	time -p $<

#------------------------------------------------------------------------
# Running:

%.debug.run: HSstd_cbits.dll %.debug.exe
	echo "$(CORENV_DEBUG)" > $@.bat
	echo "set CORPATH=$(subst /,\\,$(ILXASM_HOME))\\bin;\\GHC\\fptools\\ghc\\lib\\std;%CORPATH%" >> $@.bat
	echo "$(LOCALRUN)$(subst /,\\,$*).debug.exe 2>&1" >> $@.bat
	time -p cmd /c $(subst /,\\,$@).bat
	rm $@.bat

%.retail.run: HSstd_cbits.dll %.retail.exe
	echo "$(CORENV_RETAIL)" > $@.bat
	echo "set CORPATH=$(subst /,\\,$(ILXASM_HOME))\\bin;\\GHC\\fptools\\ghc\\lib\\std;%CORPATH%" >> $@.bat
	echo "$(LOCALRUN)$(subst /,\\,$*).retail.exe 2>&1" >> $@.bat
	time -p cmd /c $(subst /,\\,$@).bat
	rm $@.bat


%.run: %.exe
	time -p $<



#--------------------

%.debug.exe: %.nolib.il
	echo "$(CORENV_RETAIL)" > $@.bat
	echo "$(CORENV_RETAIL)ilasm /DEBUG /OUT=$(subst /,\\,$@) $(subst /,\\,$<)" >> $@.bat
	time -p cmd /c $(subst /,\\,$@).bat
	rm $@.bat

%.trace-il.nolib.il: $(ILXASM) $(ILXASM_HOME)/ilxasm/stdlib-func-by-mcalli.ilx %.nolib.ilx
	$(ILXASM) $(ILXASM_FLAGS) --trace-il  $*.nolib.ilx > $@.tmp
	mv $@.tmp $@

%.mvl: %.nolib.il
	ILVALID_HOME=$(ILXASM_HOME) $(ILVALID) $*.nolib.il

ci:
	(cd $(ILXASM_HOME); $(CVS) ci -m "")
	(cd ../..; cvs ci -m "")
	(cd ../../../lib/std; $(CVS) ci -m "")

upd:
	(cd $(ILXASM_HOME); $(CVS) up)
	(cd ../..; $(CVS) up)
	(cd ../../../lib/std; $(CVS) up)

.PRECIOUS: %.nongeneric.boxed.dlllib.il  %.generic.dlllib.il  %.generic.il %.nolib.il %.Onot.ilx %.O.ilx %.nolib.ilx %.dlllib.ilx %.exe  %.debug.exe %.dll %.O.exe

.PHONY: %.run
