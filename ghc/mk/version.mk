#
# Project-specific version information.
#
# Note:
#   this config file is intended to centralise all
#   project version information. To bump up the version
#   info on your package, edit this file and recompile
#   all the dependents. This file lives in the source tree.
#
# In the case of the ghc/ project, if you make changes
# to this file, you'll *have to* to rebuild the driver
# in your build tree(s). The ghc/driver/Makefile has got
# a dependency that will force such rebuilding to happen,
# but it does require you to do a 'make' in ghc/driver.

#
# Ghc project settings:
# 
# ProjectVersion    is treated as a *string*
# ProjectVersionInt is treated as an *integer* (for cpp defines)

# Versioning scheme: A.BB.C
#         A: major version, any number of digits
#         B: minor version, two digits padded with leading zeros
#	  C: patchlevel, one digit, omitted if zero.
#
# ProjectVersionInt does *not* contain the patchlevel (rationale: this
# figure is used for conditional compilations, and library interfaces
# etc. are not supposed to change between patchlevels).

ProjectName       = The Glorious Glasgow Haskell Compilation System
ProjectNameShort  = ghc
ProjectVersion    = 4.05
ProjectVersionInt = 405

#
# Optionally, you can get the compiler driver to check the
# version consistency between the object files being linked.
# 
# Major numbers must always agree, minor disagreements yield a warning.
#
# These version numbers are currently separate from the project
# version - one (semi-valid) reason for having them separate is that
# object files produced by different versions of the compiler need
# not be incompatible..
HscMajorVersion=40
HscMinorVersion=0
CcMajorVersion=36
CcMinorVersion=1

#
# Interface file version
#
# If you should happen to make changes to the interface file format
# that will break compatibility with older versions, up this variable.
# 
HscIfaceFileVersion=5
#	But watch out: interface file format after Simon's renamer
#	hacking isn't the same as before, but it may not make
#	any difference for the GHC boot files.
#		May 1999
