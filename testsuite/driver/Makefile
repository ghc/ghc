TOP = ..
include $(TOP)/mk/boilerplate.mk

RUNTESTS_SRCS= \
	TopSort.hs CmdLexer.hs CmdParser.hs CmdSemantics.hs CmdSyntax.hs KludgedSystem.hs Main.hs \
	basicRxLib/AbsTreeDefs.hs basicRxLib/Assertions.hs  \
	basicRxLib/AugTreeDefs.hs basicRxLib/AugmentTree.hs \
	basicRxLib/BagRE.hs basicRxLib/CompileREP.hs \
	basicRxLib/CompileREPB.hs basicRxLib/CompileRES.hs \
	basicRxLib/ConstructorMonad.hs basicRxLib/ExecRE.hs \
	basicRxLib/FiniteMap.hs basicRxLib/Global.hs \
	basicRxLib/HandleInterval.hs basicRxLib/HandleSubexp.hs \
	basicRxLib/IsPrefix.hs basicRxLib/MakeNFA.hs \
	basicRxLib/MakeNFANode.hs basicRxLib/Matchers.hs \
	basicRxLib/NFADefs.hs basicRxLib/OrdList.hs \
	basicRxLib/ParsePolyRegexp.hs basicRxLib/ParsePolyRegexpBasic.hs \
	basicRxLib/ParseStringRegexp.hs basicRxLib/Parsers.hs basicRxLib/Regexp.hs

all :: runtests

SRC_HC_OPTS += -package lang -package util  -cpp  -W -fno-warn-unused-matches  -ibasicRxLib

runtests: $(RUNTESTS_SRCS)
	$(GHC) $(HC_OPTS) --make Main -o runtests

CLEAN_FILES += CmdParser.hs basicRxLib/*.o basicRxLib/*.hi

include $(TOP)/mk/target.mk
