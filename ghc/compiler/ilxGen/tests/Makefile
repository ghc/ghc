
TOP = ../../..
include $(TOP)/mk/boilerplate.mk

WAYS=$(GhcLibWays)

#-----------------------------------------------------------------------------
# 	Setting the standard variables
#

HC = $(GHC_INPLACE)
SRC_HC_OPTS+=-cpp -fglasgow-exts

#-----------------------------------------------------------------------------
# 
CORENV_DEBUG=
CORENV_RETAIL=
LOCALRUN=./
ifeq ($(HOSTNAME),MSRC-HILDA)
CORENV_DEBUG="call devcorb2gen.bat fastchecked"
CORENV_RETAIL="call devcorb2gen.bat free"
LOCALRUN=.\\
endif    

ILX2IL_HOME=C:/devel/fcom/src
ILX2IL=$(ILX2IL_HOME)/bin/ilx2il.opt.exe 
ILVALID=$(ILX2IL_HOME)/bin/ilvalid.opt.exe 

ghc:
	$(MAKE) -C ../.. 

ilx:
	$(MAKE) -C $(ILX2IL_HOME) ilxdefault

prel: ilx
	$(MAKE) -C ../../../lib/std std.$(ilx_way).dll std.$(ilx_way).vlb

#========================================================================
# 1. From Haskell to ILX and then to IL - see build.mk 

#------------------------------------------------------------------------
# 2. From IL to .EXE

%.$(ilx_way).exe : %.$(ilx_way).il ../Entry.$(ilx_way).il 
	cat $*.$(ilx_way).il ../Entry.$(ilx_way).il > $@.tmp
	echo "call devcorb2gen free" > tmp.bat
	echo "ilasm /DEBUG /QUIET /OUT=$@ $@.tmp" >> tmp.bat
	cmd /c tmp.bat

../Entry.$(hs2ilx_suffix)_o: ../Entry.ilx
	sed -e "s|ilx std|ilx std.$(hs2ilx_suffix)|g" ../Entry.ilx > $@.tmp
	mv $@.tmp $@


%.$(ilx_way).mvl:  %.$(ilx_way).il
	(ILVALID_HOME=c:\\devel\\fcom\\src\\ ILVALID_MSCORLIB=mscorlib.vlb $(ILVALID) c:\\devel\\fcom\\src\\bin\\msilxlib$(ilx2il_suffix).vlb $(TOP)/lib/std/std.$(ilx_way).vlb $<) 2>&1


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

%.cordbg.run: HSstd_cbits.dll %.exe
	cp $(TOP)/lib/std/*.$(ilx_way).dll .
	cp $(ILX2IL_HOME)/bin/msilxlib$(ilx2il_suffix).dll .
	cp $(TOP)/lib/std/*.$(ilx_way).pdb .
	cp $(ILX2IL_HOME)/bin/msilxlib$(ilx2il_suffix).pdb .
	echo "call devcorb2gen fastchecked" > $@.bat
	echo "$(LOCALRUN)$(subst /,\\,$*).exe 2>&1" >> $@.bat
	time -p cmd /c $(subst /,\\,$@).bat
	rm $@.bat

%.debug.run: HSstd_cbits.dll %.exe
	cp $(TOP)/lib/std/*.$(ilx_way).dll .
	cp $(ILX2IL_HOME)/bin/msilxlib$(ilx2il_suffix).dll .
	echo "call devcorb2gen fastchecked" > $@.bat
	echo "$(LOCALRUN)$(subst /,\\,$*).exe 2>&1" >> $@.bat
	time -p cmd /c $(subst /,\\,$@).bat
	rm $@.bat

%.retail.run: HSstd_cbits.dll %.exe
	cp $(TOP)/lib/std/*.$(ilx_way).dll .
	cp $(ILX2IL_HOME)/bin/msilxlib$(ilx2il_suffix).dll .
	echo "call devcorb2gen free" > $@.bat
	echo "$(LOCALRUN)$(subst /,\\,$*).exe 2>&1" >> $@.bat
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


.PHONY: %.run

include $(TOP)/mk/target.mk
