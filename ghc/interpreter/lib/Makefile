# -------------------------------------------------------------------------- #
# $Id: Makefile,v 1.8 2000/03/20 04:26:23 andy Exp $ 
# -------------------------------------------------------------------------- #

TOP = ../..
include $(TOP)/mk/boilerplate.mk

PRELUDE  = Prelude.hs

STD_LIBS = Array.lhs Char.lhs Complex.lhs CPUTime.lhs \
	   Directory.lhs IO.lhs Ix.lhs List.lhs Locale.lhs \
	   Maybe.lhs Monad.lhs Numeric.lhs Ratio.lhs \
	   Random.lhs System.lhs 

# To Fix: Time, Directory

DATA_LIBS = FiniteMap.lhs Set.lhs \
	    EdisonPrelude.hs \
	Assoc.hs AssocDefaults.hs AssocList.hs	PatriciaLoMap.hs \
	Collection.hs CollectionDefaults.hs CollectionUtils.hs \
	LazyPairingHeap.hs LeftistHeap.hs MinHeap.hs SkewHeap.hs \
	SplayHeap.hs TestOrdBag.hs TestOrdSet.hs UnbalancedSet.hs \
	BankersQueue.hs	BinaryRandList.hs BraunSeq.hs JoinList.hs \
	ListSeq.hs MyersStack.hs RandList.hs RevSeq.hs Sequence.hs \
	SequenceDefaults.hs SimpleQueue.hs SizedSeq.hs TestSeq.hs

TEXT_LIBS = Pretty.lhs Html.lhs HtmlBlockTable.lhs \
	Haskell2Xml.hs \
	ParseSTLib.hs \
	Xml2Haskell.hs \
	XmlCombinators.hs \
	XmlHtmlGen.hs \
	XmlHtmlPP.hs \
	XmlHtmlParse.hs \
	XmlLex.hs \
	XmlLib.hs \
	XmlPP.hs \
	XmlParse.hs \
	XmlTypes.hs

LANG_LIBS = Addr.lhs Bits.lhs ByteArray.lhs Dynamic.lhs \
	Exception.lhs Int.lhs IOExts.lhs LazyST.lhs \
	MonadEither.lhs MonadFix.lhs MonadIdentity.lhs \
	MonadReader.lhs MonadRWS.lhs MonadState.lhs \
	MonadTrans.lhs MonadWriter.lhs Monoid.lhs \
	MutableArray.lhs NumExts.lhs PackedString.lhs \
	ShowFunctions.lhs ST.lhs Stable.lhs StablePtr.lhs \
	TimeExts.lhs Weak.lhs Word.lhs

UTIL_LIBS = QuickCheck.hs QuickCheckBatch.hs QuickCheckPoly.hs \
	QuickCheckUtils.hs GetOpt.lhs \
	Regex.lhs RegexString.lhs Observe.lhs Memo.lhs Readline.lhs \
	Select.lhs 

CONC_LIBS = Channel.lhs ChannelVar.lhs Concurrent.lhs Merge.lhs \
	Parallel.lhs SampleVar.lhs Semaphore.lhs Strategies.lhs


LIBS =  $(PRELUDE) \
        $(STD_LIBS) \
	$(DATA_LIBS) \
	$(LANG_LIBS) \
	$(TEXT_LIBS) \
	$(CONC_LIBS) \
	$(UTIL_LIBS) 

all :: $(LIBS)


HUGSCPP = ../../utils/hscpp/hscpp -D__HUGS__ -DUSE_REPORT_PRELUDE -D__HASKELL98__

%.lhs :: $(GHC_LIB_DIR)/std/%.lhs
	$(HUGSCPP) -I../../includes $< > $*.lhs

%.hs :: $(GHC_LIB_DIR)/hugs/%.hs
	$(HUGSCPP) -I../../includes $< > $*.hs

%.lhs :: $(FPTOOLS_TOP)/hslibs/concurrent/%.lhs
	$(HUGSCPP) -I../../includes $< > $*.lhs

%.lhs :: $(FPTOOLS_TOP)/hslibs/data/%.lhs
	$(HUGSCPP) -I../../includes $< > $*.lhs

%.hs :: $(FPTOOLS_TOP)/hslibs/data/edison/%.hs
	$(HUGSCPP) -I../../includes $< > $*.hs
%.hs :: $(FPTOOLS_TOP)/hslibs/data/edison/Seq/%.hs
	$(HUGSCPP) -I../../includes $< > $*.hs
%.hs :: $(FPTOOLS_TOP)/hslibs/data/edison/Coll/%.hs
	$(HUGSCPP) -I../../includes $< > $*.hs
%.hs :: $(FPTOOLS_TOP)/hslibs/data/edison/Assoc/%.hs
	$(HUGSCPP) -I../../includes $< > $*.hs

%.lhs :: $(FPTOOLS_TOP)/hslibs/lang/%.lhs
	$(HUGSCPP) -I../../includes $< > $*.lhs
%.lhs :: $(FPTOOLS_TOP)/hslibs/lang/monads/%.lhs
	$(HUGSCPP) -I../../includes $< > $*.lhs

%.lhs :: $(FPTOOLS_TOP)/hslibs/net/%.lhs
	$(HUGSCPP) -I../../includes $< > $*.lhs

%.lhs :: $(FPTOOLS_TOP)/hslibs/posix/%.lhs
	$(HUGSCPP) -I../../includes $< > $*.lhs

%.lhs :: $(FPTOOLS_TOP)/hslibs/text/%.lhs
	$(HUGSCPP) -I../../includes $< > $*.lhs
%.lhs :: $(FPTOOLS_TOP)/hslibs/text/html/%.lhs
	$(HUGSCPP) -I../../includes $< > $*.lhs
%.hs :: $(FPTOOLS_TOP)/hslibs/text/haxml/lib/%.hs
	$(HUGSCPP) -I../../includes $< > $*.hs


%.lhs :: $(FPTOOLS_TOP)/hslibs/util/%.lhs
	$(HUGSCPP) -I../../includes $< > $*.lhs
%.hs :: $(FPTOOLS_TOP)/hslibs/util/check/%.hs
	$(HUGSCPP) -I../../includes $< > $*.hs

CLEAN_FILES += $(LIBS)

include $(TOP)/mk/target.mk

