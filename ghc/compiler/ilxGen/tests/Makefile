
# These settings are if you use a visual studio build
CVS=cvs
CORENV_DEBUG=
CORENV_RETAIL=
LOCALRUN=./
ILX_FAST=x
ifeq ($(HOSTNAME),MSRC-HILDA)
CORENV_DEBUG="call devcorb2gen.bat fastchecked"
CORENV_RETAIL="call devcorb2gen.bat free"
LOCALRUN=.\\
ILX_FAST=
endif    

ILX2IL_HOME=C:/devel/fcom/src
ILX2IL=$(ILX2IL_HOME)/bin/ilx2il.opt.exe 
ILVALID=$(ILX2IL_HOME)/bin/ilvalid.opt.exe 

ghc:
	$(MAKE) -C ../.. 

ilx:
	$(MAKE) -C $(ILX2IL_HOME) ilxdefault

prel: ilx
	$(MAKE) -C ../../../lib/std std.Onot.mono-b2.dll

%.o: %.hs ../../ghc-4.11
	../../ghc-inplace -o $@ -c $*.hs

#========================================================================
# 1. From Haskell to ILX 

%.Onot.ilx: %.hs ../../ghc-4.11
	../../ghc-inplace -c -fglasgow-exts -o $@ -i../../../lib/std/.Onot -Onot -filx -fkeep-stg-types $*.hs -osuf Onot.ilx

%.O.ilx: %.hs ../../ghc-4.11
	../../ghc-inplace -c -fglasgow-exts -o $@ -i../../../lib/std/.O -O -filx -fkeep-stg-types $*.hs -osuf O.ilx

../Entry.Onot.ilx: ../Entry.ilx
	sed -e "s|ilx std|ilx std.Onot|g" ../Entry.ilx > $@.tmp
	mv $@.tmp $@

../Entry.O.ilx: ../Entry.ilx
	sed -e "s|ilx std|ilx std.O|g" ../Entry.ilx > $@.tmp
	mv $@.tmp $@


#========================================================================
# 2. From ILX to IL

%.generic.il: $(ILX2IL) %.ilx
	$(ILX2IL) --generic $(ILX2IL_FLAGS) -o $@.tmp $*.ilx
	mv $@.tmp $@

%.mono.il: $(ILX2IL) %.ilx
	$(ILX2IL) --mono $(ILX2IL_FLAGS) -o $@.tmp $*.ilx
	mv $@.tmp $@

#------------------------------------------------------------------------
# From IL to .EXE

%.generic.exe: %.generic.il ../Entry.Onot.generic.il
	cat $*.generic.il ../Entry.Onot.generic.il > $@.tmp
	echo "$(CORENV_RETAIL)" > $@.bat
	echo "ilasm /exe /quiet /out=$(subst /,\\,$@.tmp) $(subst /,\\,$@.tmp)" >> $@.bat
	time -p cmd /c $(subst /,\\,$@).bat
	rm $@.bat

%.mono.exe: %.mono.il ../Entry.Onot.mono.il
	cat $*.mono.il ../Entry.Onot.mono.il > $@.tmp
	echo "$(CORENV_RETAIL)" > $@.bat
	echo "ilasm /exe /quiet /out=$(subst /,\\,$@.tmp) $(subst /,\\,$@.tmp)" >> $@.bat
	time -p cmd /c $(subst /,\\,$@).bat
	rm $@.bat

#------------------------------------------------------------------------
# From .HS to .EXE without using ILX
# Used to run performance comparisons against native code GHC

%.Onot.exe: %.hs
	ghc -Onot -o $@ $<

%.O.exe: %.hs
	ghc -O -o $@ $<

%.run: %.exe
	time -p $<

#------------------------------------------------------------------------
# Running:

HSstd_cbits.dll: ../../../lib/std/cbits/HSstd_cbits.dll
	cp $< $@

%.debug.run: HSstd_cbits.dll %.exe
	echo "$(CORENV_DEBUG)" > $@.bat
	echo "set CORPATH=$(subst /,\\,$(ILX2IL_HOME))\\bin;\\GHC\\fptools\\ghc\\lib\\std;%CORPATH%" >> $@.bat
	echo "$(LOCALRUN)$(subst /,\\,$*).debug.exe 2>&1" >> $@.bat
	time -p cmd /c $(subst /,\\,$@).bat
	rm $@.bat

%.retail.run: HSstd_cbits.dll %.exe
	echo "$(CORENV_RETAIL)" > $@.bat
	echo "set CORPATH=$(subst /,\\,$(ILX2IL_HOME))\\bin;\\GHC\\fptools\\ghc\\lib\\std;%CORPATH%" >> $@.bat
	echo "$(LOCALRUN)$(subst /,\\,$*).retail.exe 2>&1" >> $@.bat
	time -p cmd /c $(subst /,\\,$@).bat
	rm $@.bat


%.run: %.exe
	time -p $<


#--------------------

%.mvl: %.nolib.il
	ILVALID_HOME=$(ILX2IL_HOME) $(ILVALID) $*.nolib.il

ci:
	(cd $(ILX2IL_HOME); $(CVS) ci -m "")
	(cd ../..; cvs ci -m "")
	(cd ../../../lib/std; $(CVS) ci -m "")

upd:
	(cd $(ILX2IL_HOME); $(CVS) up)
	(cd ../..; $(CVS) up)
	(cd ../../../lib/std; $(CVS) up)

.PRECIOUS: %.mono-nonstatic.il  %.fullgeneric-nonstatic.il  %.fullgeneric.il %.nolib.il %.Onot.ilx %.O.ilx %.nolib.ilx %-nonstatic.ilx %.exe  %.debug.exe %.dll %.O.exe

.PHONY: %.run
