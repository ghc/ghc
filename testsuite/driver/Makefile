
# Yes, it's a hack.  Complaints to /dev/null.

TESTDRIVER_SRCS= \
	CmdLexer.hs CmdParser.hs CmdSemantics.hs CmdSyntax.hs Main.hs \
	RunOneTest.hs basicRxLib/AbsTreeDefs.hs basicRxLib/Assertions.hs  \
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


all: testdriver

boot:
	@echo "Make boot is not needed here"

testdriver: $(TESTDRIVER_SRCS)
	../../ghc/compiler/ghc-inplace --make Main -o testdriver -i -ibasicRxLib -package lang -cpp

clean:
	/bin/rm -f testdriver *.o *.hi basicRxLib/*.o basicRxLib/*.hi

