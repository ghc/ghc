TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

LIBDIR := "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

clean:
	rm -f *.o *.hi

.PHONY: RenameCase1
RenameCase1:
	$(CHECK_EXACT) $(LIBDIR) RenameCase1.hs changeRenameCase1

.PHONY: LayoutLet2
LayoutLet2:
	$(CHECK_EXACT) $(LIBDIR) LayoutLet2.hs changeLayoutLet2

.PHONY: LayoutLet3
LayoutLet3:
	$(CHECK_EXACT) $(LIBDIR) LayoutLet3.hs changeLayoutLet3

.PHONY: LayoutLet4
LayoutLet4:
	$(CHECK_EXACT) $(LIBDIR) LayoutLet4.hs changeLayoutLet3

.PHONY: Rename1
Rename1:
	$(CHECK_EXACT) $(LIBDIR) Rename1.hs changeRename1

.PHONY: Rename2
Rename2:
	$(CHECK_EXACT) $(LIBDIR) Rename2.hs changeRename2

.PHONY: LayoutIn1
LayoutIn1:
	$(CHECK_EXACT) $(LIBDIR) LayoutIn1.hs changeLayoutIn1

.PHONY: LayoutIn3
LayoutIn3:
	$(CHECK_EXACT) $(LIBDIR) LayoutIn3.hs changeLayoutIn3

.PHONY: LayoutIn3a
LayoutIn3a:
	$(CHECK_EXACT) $(LIBDIR) LayoutIn3a.hs changeLayoutIn3

.PHONY: LayoutIn3b
LayoutIn3b:
	$(CHECK_EXACT) $(LIBDIR) LayoutIn3b.hs changeLayoutIn3

.PHONY: LayoutIn4
LayoutIn4:
	$(CHECK_EXACT) $(LIBDIR) LayoutIn4.hs changeLayoutIn4

.PHONY: LocToName
LocToName:
	$(CHECK_EXACT) $(LIBDIR) LocToName.hs changeLocToName

.PHONY: LetIn1
LetIn1:
	$(CHECK_EXACT) $(LIBDIR) LetIn1.hs changeLetIn1

.PHONY: WhereIn4
WhereIn4:
	$(CHECK_EXACT) $(LIBDIR) WhereIn4.hs changeWhereIn4

.PHONY: AddDecl1
AddDecl1:
	$(CHECK_EXACT) $(LIBDIR) AddDecl1.hs changeAddDecl1

.PHONY: AddDecl2
AddDecl2:
	$(CHECK_EXACT) $(LIBDIR) AddDecl2.hs changeAddDecl2

.PHONY: AddDecl3
AddDecl3:
	$(CHECK_EXACT) $(LIBDIR) AddDecl3.hs changeAddDecl3

.PHONY: LocalDecls
LocalDecls:
	$(CHECK_EXACT) $(LIBDIR) LocalDecls.hs changeLocalDecls

.PHONY: LocalDecls2
LocalDecls2:
	$(CHECK_EXACT) $(LIBDIR) LocalDecls2.hs changeLocalDecls2

.PHONY: WhereIn3a
WhereIn3a:
	$(CHECK_EXACT) $(LIBDIR) WhereIn3a.hs changeWhereIn3a

.PHONY: WhereIn3b
WhereIn3b:
	$(CHECK_EXACT) $(LIBDIR) WhereIn3b.hs changeWhereIn3b

.PHONY: AddLocalDecl1
AddLocalDecl1:
	$(CHECK_EXACT) $(LIBDIR) AddLocalDecl1.hs ADDLOCALDECL1

.PHONY: AddLocalDecl2
AddLocalDecl2:
	$(CHECK_EXACT) $(LIBDIR) AddLocalDecl2.hs ADDLOCALDECL2

.PHONY: AddLocalDecl3
AddLocalDecl3:
	$(CHECK_EXACT) $(LIBDIR) AddLocalDecl3.hs ADDLOCALDECL3

.PHONY: AddLocalDecl4
AddLocalDecl4:
	$(CHECK_EXACT) $(LIBDIR) AddLocalDecl4.hs ADDLOCALDECL4

.PHONY: AddLocalDecl5
AddLocalDecl5:
	$(CHECK_EXACT) $(LIBDIR) AddLocalDecl5.hs ADDLOCALDECL5

.PHONY: AddLocalDecl6
AddLocalDecl6:
	$(CHECK_EXACT) $(LIBDIR) AddLocalDecl6.hs ADDLOCALDECL6

.PHONY: RmDecl1
RmDecl1:
	$(CHECK_EXACT) $(LIBDIR) RmDecl1.hs rmDecl1

.PHONY: RmDecl2
RmDecl2:
	$(CHECK_EXACT) $(LIBDIR) RmDecl2.hs rmDecl2

.PHONY: RmDecl3
RmDecl3:
	$(CHECK_EXACT) $(LIBDIR) RmDecl3.hs rmDecl3

.PHONY: RmDecl4
RmDecl4:
	$(CHECK_EXACT) $(LIBDIR) RmDecl4.hs rmDecl4

.PHONY: RmDecl5
RmDecl5:
	$(CHECK_EXACT) $(LIBDIR) RmDecl5.hs rmDecl5

.PHONY: RmDecl6
RmDecl6:
	$(CHECK_EXACT) $(LIBDIR) RmDecl6.hs rmDecl6

.PHONY: RmDecl7
RmDecl7:
	$(CHECK_EXACT) $(LIBDIR) RmDecl7.hs rmDecl7

.PHONY: RmTypeSig1
RmTypeSig1:
	$(CHECK_EXACT) $(LIBDIR) RmTypeSig1.hs rmTypeSig1

.PHONY: RmTypeSig2
RmTypeSig2:
	$(CHECK_EXACT) $(LIBDIR) RmTypeSig2.hs rmTypeSig2

.PHONY: AddHiding1
AddHiding1:
	$(CHECK_EXACT) $(LIBDIR) AddHiding1.hs addHiding1

.PHONY: AddHiding2
AddHiding2:
	$(CHECK_EXACT) $(LIBDIR) AddHiding2.hs addHiding2

.PHONY: AddClassMethod
AddClassMethod:
	$(CHECK_EXACT) $(LIBDIR) AddClassMethod.hs addClassMethod
