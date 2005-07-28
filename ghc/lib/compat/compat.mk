# Settings for using the libghccompat.a library elsewhere in the build
# tree: this file is just included into Makefiles, see 
# ghc/utils/ghc-pkg/Makefile for example.
#
# This is a poor-mans package, but simpler because we don't
# have to deal with variations in the package support of different
# versions of GHC.

# Use libghccompat.a:
SRC_HC_OPTS += -i$(GHC_LIB_COMPAT_DIR)
SRC_LD_OPTS += -L$(GHC_LIB_COMPAT_DIR) -lghccompat

# And similarly for when booting from .hc files:
HC_BOOT_LD_OPTS += -L$(GHC_LIB_COMPAT_DIR)
HC_BOOT_LIBS += -lghccompat

ifeq "$(Windows)" "YES"
# not very nice, but required for -lghccompat on Windows
SRC_LD_OPTS += -lshell32
endif

# This is horrible.  We ought to be able to omit the entire directory
# from mkDependHS.
SRC_MKDEPENDHS_OPTS += \
	-optdep--exclude-module=Compat.RawSystem \
	-optdep--exclude-module=Compat.Directory \
	-optdep--exclude-module=Distribution.Compat.FilePath \
	-optdep--exclude-module=Distribution.Compat.ReadP \
	-optdep--exclude-module=Distribution.Extension \
	-optdep--exclude-module=Distribution.GetOpt \
	-optdep--exclude-module=Distribution.InstalledPackageInfo \
	-optdep--exclude-module=Distribution.License \
	-optdep--exclude-module=Distribution.Package \
	-optdep--exclude-module=Distribution.ParseUtils \
	-optdep--exclude-module=Distribution.Compiler \
	-optdep--exclude-module=Distribution.Version \
	-optdep--exclude-module=System.Directory.Internals
