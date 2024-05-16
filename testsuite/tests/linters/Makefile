TOP=../..

ifeq "$(LINT_NOTES)" ""
LINT_NOTES := $(abspath $(TOP)/../inplace/bin/lint-notes)
endif

ifeq "$(LINT_WHITESPACE)" ""
LINT_WHITESPACE := $(abspath $(TOP)/../inplace/bin/lint-whitespace)
endif

uniques:
	python3 checkUniques/check-uniques.py $(TOP)/..

makefiles:
	(cd $(TOP)/tests/linters/ && python3 regex-linters/check-makefiles.py tracked)

version-number:
	regex-linters/check-version-number.sh ${TOP}/..

cpp:
	(cd $(TOP)/tests/linters/ && python3 regex-linters/check-cpp.py tracked)

rts-includes:
	(cd $(TOP)/tests/linters/ && python3 regex-linters/check-rts-includes.py tracked)

changelogs:
	regex-linters/check-changelogs.sh $(TOP)/..

notes:
	(cd $(TOP)/.. && $(LINT_NOTES) broken-refs)

whitespace:
	(cd $(TOP)/.. &&\
	  $(LINT_WHITESPACE) tracked\
			--ignore-dirs\
				testsuite\
				libraries/base/cbits\
				libraries/base/tests\
				utils/hp2ps\
				utils/hpc\
				utils/unlit\
				utils/haddock\
			--ignore-files\
				libraries/base/GHC/IO/Encoding/CodePage/Table.hs\
				libraries/base/Control/Concurrent/QSem.hs\
				libraries/base/Control/Concurrent/QSemN.hs\
				libraries/base/Control/Monad/ST/Imp.hs\
				libraries/base/Control/Monad/ST/Lazy.hs\
				libraries/base/Data/Char.hs\
				libraries/base/Data/Eq.hs\
				libraries/base/Data/IORef.hs\
				libraries/base/Data/Int.hs\
				libraries/base/Data/Ix.hs\
				libraries/base/Data/Ratio.hs\
				libraries/base/Data/STRef/Lazy.hs\
				libraries/base/Data/STRef/Strict.hs\
				libraries/base/Foreign.hs\
				libraries/base/Foreign/C.hs\
				libraries/base/Foreign/Concurrent.hs\
				libraries/base/Foreign/ForeignPtr.hs\
				libraries/base/Foreign/ForeignPtr/Imp.hs\
				libraries/base/Foreign/ForeignPtr/Safe.hs\
				libraries/base/Foreign/ForeignPtr/Unsafe.hs\
				libraries/base/Foreign/Marshal.hs\
				libraries/base/Foreign/Marshal/Alloc.hs\
				libraries/base/Foreign/Marshal/Error.hs\
				libraries/base/Foreign/Marshal/Safe.hs\
				libraries/base/Foreign/Marshal/Unsafe.hs\
				libraries/base/Foreign/Safe.hs\
				libraries/base/Foreign/StablePtr.hs\
				libraries/base/Foreign/Storable.hs\
				libraries/base/GHC/IO/Encoding/Latin1.hs\
				libraries/base/GHC/IO/Encoding/Types.hs\
				libraries/base/GHC/IO/Handle/FD.hs\
				libraries/base/GHC/IO/IOMode.hs\
				libraries/base/System/Console/GetOpt.hs\
				libraries/base/System/IO/Unsafe.hs\
				libraries/base/System/Mem.hs\
				libraries/base/Text/Show.hs\
				libraries/base/include/HsBase.h\
				libraries/base/include/HsEvent.h\
				libraries/base/include/md5.h\
				libraries/ghc-prim/GHC/Tuple.hs\
				libraries/template-haskell/Language/Haskell/TH/Quote.hs\
				rts/STM.h\
				rts/Sparks.h\
				rts/Threads.h\
				rts/hooks/OnExit.c\
				rts/sm/Evac.h\
				rts/sm/MarkStack.h\
				rts/sm/MarkWeak.h\
				rts/sm/Scav.h\
				rts/sm/Sweep.c\
				rts/sm/Sweep.h\
				rts/win32/veh_excn.h\
				utils/genprimopcode/Parser.y\
				utils/genprimopcode/Syntax.hs\
  )
