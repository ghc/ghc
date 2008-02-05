############################################################################
#
#		This is the GHC Makefile for MacOS X-specific targets
#
# Targets: 
#
#	framework-pkg  [MacOS only]
#		Builds /Library/Frameworks/GHC.framework wrapped into a Mac
#		installer package; must be executed in a ./configure'd tree
#		(--prefix doesn't matter as it will be overridden)
#
#	framework-binary-dist  [MacOS only]
#		Builds GHC.framework encapsulating a binary distribution
#		(to give a relocatable framework); must be used in a fully
#		built tree
#
############################################################################

TOP=../..
include $(TOP)/mk/boilerplate.mk

# The ProjectVersionInt is GHC's idea of an API version and hence determines
# the framework version.
FRAMEWORK_VERSION = $(ProjectVersionInt)

# Xcode requires CURRENT_PROJECT_VERSION to be an int or float.  We use this
# only as the build version (aka CFBundleVersion).
CURRENT_PROJECT_VERSION = $(ProjectVersionInt).$(ProjectPatchLevel)

# The user-visible CFBundleShortVersionString is set to the standard GHC 
# version number.
SHORT_VERSION_STRING = $(ProjectVersion)

framework-pkg:
	xcodebuild -target GHC-systemwide install\
		   CURRENT_PROJECT_VERSION=$(CURRENT_PROJECT_VERSION)\
	    	   SHORT_VERSION_STRING=$(SHORT_VERSION_STRING)\
	 	   FRAMEWORK_VERSION=$(FRAMEWORK_VERSION)\
		   CURRENT_LIBRARY_VERSION=$(FRAMEWORK_VERSION)
	@echo "FIXME: call the packager"; exit 1

# Instead of making 'binary-dist' a Makefile dependency, we let xcodebuild call
# 'make binary-dist'.  This has the advantage that xcode knows the framework
# path into which the distribution should be installed and can instruct 
# binary-dist to put it directly into the right place without copying the whole
# tree yet another time.
#
framework-binary-dist:
	xcodebuild -target GHC-relocatable build\
		   CURRENT_PROJECT_VERSION=$(CURRENT_PROJECT_VERSION)\
	    	   SHORT_VERSION_STRING=$(SHORT_VERSION_STRING)\
	 	   FRAMEWORK_VERSION=$(FRAMEWORK_VERSION)\
		   CURRENT_LIBRARY_VERSION=$(FRAMEWORK_VERSION)
	@echo "FIXME: call the packager"; exit 1
