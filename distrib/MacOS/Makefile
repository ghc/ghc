############################################################################
#
#		This is the GHC Makefile for MacOS X-specific targets
#
# Targets: 
#
#	framework-pkg  [MacOS only]
#		Builds /Library/Frameworks/GHC.framework wrapped into a Mac
#		installer package; must be executed in a ./configure'd tree
#		(--prefix doesn't matter as it will be overridden); other
#		./configure arguments are passed through, unless overwritten
#		with XCODE_EXTRA_CONFIGURE_ARGS.  A deployment target can
#		be specified by setting the corresponding ./configure
#		argument or by setting MACOSX_DEPLOYMENT_TARGET.
#
#	framework-binary-dist  [MacOS only]
#		Builds GHC.framework encapsulating a binary distribution
#		(to give a relocatable framework); must be used in a fully
#		built tree
#
############################################################################

include ../../mk/config.mk

ifeq "$(ProjectVersion)" ""
$(error Please run ./configure first)
endif

include ../../mk/custom-settings.mk

# The ProjectVersionInt is GHC's idea of an API version and hence determines
# the framework version.
FRAMEWORK_VERSION = $(ProjectVersionInt)

# Xcode requires CURRENT_PROJECT_VERSION to be an int or float.  We use this
# only as the build version (aka CFBundleVersion).
CURRENT_PROJECT_VERSION = $(ProjectVersionInt).$(ProjectPatchLevel)

# The user-visible CFBundleShortVersionString is set to the standard GHC 
# version number.
SHORT_VERSION_STRING = $(ProjectVersion)

# Name of the installer package
PACKAGE_NAME = GHC-$(ProjectVersion)-$(TargetArch_CPP).pkg

# Determine arguments that should be passed to ./configure from within Xcode
#
# By default, we pass whatever was used when the present tree was configured.
# The user can override this by setting XCODE_EXTRA_CONFIGURE_ARGS.  If
# MACOSX_DEPLOYMENT_TARGET is set, the target is added to whatever arguments
# are passed.
export XCODE_EXTRA_CONFIGURE_ARGS
ifeq "$(XCODE_EXTRA_CONFIGURE_ARGS)" ""
XCODE_EXTRA_CONFIGURE_ARGS = $(shell echo $(CONFIGURE_ARGS))
endif
ifneq "$(MACOSX_DEPLOYMENT_TARGET)" ""
XCODE_EXTRA_CONFIGURE_ARGS += --with-macosx-deployment-target=$(MACOSX_DEPLOYMENT_TARGET)
endif

# Determine whether we need to pass a "-target" option to packagemaker
#
# If a deployment target has been set, we use the same target for packagemaker.
ifneq "$(MACOSX_DEPLOYMENT_TARGET)" ""
PACKAGEMAKER_TARGET = -target $(MACOSX_DEPLOYMENT_TARGET)
endif

# Xcode's installation build product location (this is where the GHC.framework
# is assembled)
DSTROOT=/tmp/GHC.dst

# Tools directory for a system volume install
TOOLS_SYSTEM=$(DSTROOT)/Library/Frameworks/GHC.framework/Tools

PACKAGEMAKER=/Developer/usr/bin/packagemaker

# Build and package GHC.framework for /Library/Frameworks
#
# * Be careful to clean out Xcode's build product location first, as Xcode
#   sets permissions such that a second install on top of an existing one
#   fails
# * Make there are no contents component descriptions in the .pmdoc.  These are
#   created by the PackageMaker GUI and have per default the wrong ownership
#   and permissions for all files (as well as prevent correct setting of those
#   for files that are not in the content list).
framework-pkg:
	-chmod -fR u+w $(DSTROOT)
	-$(RM) -rf $(DSTROOT)
	mkdir -p $(TOOLS_SYSTEM)
	cp installer-scripts/Uninstaller $(TOOLS_SYSTEM)
	xcodebuild -target GHC-systemwide clean build\
		   CURRENT_PROJECT_VERSION=$(CURRENT_PROJECT_VERSION)\
	    	   SHORT_VERSION_STRING=$(SHORT_VERSION_STRING)\
	 	   FRAMEWORK_VERSION=$(FRAMEWORK_VERSION)\
		   CURRENT_LIBRARY_VERSION=$(FRAMEWORK_VERSION)\
		   COMMAND_MODE=unix2003
	xcodebuild -target GHC-systemwide install\
		   CURRENT_PROJECT_VERSION=$(CURRENT_PROJECT_VERSION)\
	    	   SHORT_VERSION_STRING=$(SHORT_VERSION_STRING)\
	 	   FRAMEWORK_VERSION=$(FRAMEWORK_VERSION)\
		   CURRENT_LIBRARY_VERSION=$(FRAMEWORK_VERSION)\
		   COMMAND_MODE=unix2003
	-$(RM) -f GHC-system.pmdoc/*-contents.xml
	$(PACKAGEMAKER) -v --doc GHC-system.pmdoc\
			$(PACKAGEMAKER_TARGET) -o $(TOP)/$(PACKAGE_NAME)\
			-i org.haskell.GHC.$(FRAMEWORK_VERSION)

# If we don't specify COMMAND_MODE=unix2003 then xcodebuild defaults
# to setting it to legacy, which means that ar builds archives
# without a table of contents. That makes the build fail later on.



# Instead of making 'binary-dist' a Makefile dependency, we let xcodebuild call
# 'make binary-dist'.  This has the advantage that xcode knows the framework
# path into which the distribution should be installed and can instruct 
# binary-dist to put it directly into the right place without copying the whole
# tree yet another time.
#
framework-binary-dist:
	xcodebuild -target GHC-relocatable clean build\
		   CURRENT_PROJECT_VERSION=$(CURRENT_PROJECT_VERSION)\
	    	   SHORT_VERSION_STRING=$(SHORT_VERSION_STRING)\
	 	   FRAMEWORK_VERSION=$(FRAMEWORK_VERSION)\
		   CURRENT_LIBRARY_VERSION=$(FRAMEWORK_VERSION)
	@echo "FIXME: call the packager"; exit 1
	# FIXME: The approach with the binary-dist in the package has the
	# problem that the BOM is wrong and the installer relocation feature
	# isn't sufficient as the layout in the binary-dist and the installed
	# tree is different
