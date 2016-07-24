GhcStage1HcOpts += -DDEBUG
GhcStage2HcOpts += -DDEBUG
#GhcStage2HcOpts += -ddump-to-file -ddump-asm -ddump-cmm -dppr-debug
GhcLibHcOpts += -g
GhcLibHcOpts += -ddump-to-file -ddump-asm -ddump-cmm -ddump-debug -dppr-debug
GhcRtsHcOpts += -g
BUILD_PROF_LIBS      = NO
DYNAMIC_GHC_PROGRAMS = NO
