
# Yes, it's a hack.  Complaints to /dev/null.

RUNTESTS_SRCS= \
	TopSort.hs CmdLexer.hs CmdParser.hs CmdSemantics.hs CmdSyntax.hs Main.hs \
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


all: runtests

boot:
	@echo "Make boot is not needed here"

runtests: $(RUNTESTS_SRCS)
	ghc -W -fno-warn-unused-matches --make Main -o runtests -ibasicRxLib -package lang -package util -cpp

clean:
	/bin/rm -f runtests *.o *.hi basicRxLib/*.o basicRxLib/*.hi

HAPPY_OPTS = -gac

%.hs : %.y
	happy $(HAPPY_OPTS) -o $@ $<
