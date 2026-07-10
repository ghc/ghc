# BINDIST_DETERMINE_LINKER
# ------------------------
#
# This is used to determine the linker within the bindists configure
#
# Notes:
# - usually, linking works by invoking $CC
# - objects are merged using $LD directly
# - $LD is a configure variable and is meaningless to $CC
# - gcc only knows linker *flavours*, it cannot use paths
#   which means that the linker path should not be an absolute path
# - clang konws --ld-path which means that it can be passed that
#   flag and also merge objs can be an absolute path
#
# Algorithm:
# if $LD is set
#    then if $CC accepts --ld-path=$(which $LD)
#            then set --ld-path=$(which $LD), MergeObjsCommand=$(which $LD)
#            else if $CC accepts --fuse-ld=$LD ($LD is a linker flavour, not an absolute path)
#                    then set -fuse-ld=$LD, MergeObjsCommand=$LD (not $(which ld))
#                    else Reject with
#                            "$LD is not compatible with $CC you chose. This means that $LD is either
#                             an unsupported linker flavour or your $CC does not support absolute linker
#                             paths"
#    else if --disable-ld-override is set or $target is macos
#            then if $CC accepts --ld-path=$(which ld)
#                    then set --ld-path=$(which ld), set MergeObjsCommand=$(which ld)
#                    else set *no* flag (equivalent to --fuse-ld=ld, if you will), set MergeObjsCommand=ld
#            else if $CC accepts --ld-path=$(which ld.lld)
#                    then set --ld-path=$(which ld.lld), MergeObjsCommand=$(which ld.lld)
#                    else if $CC accepts -fuse-ld=lld
#                            then set -fuse-ld=lld, MergeObjsCommand=ld.lld
#                            else set *no* flag (equivalent to --fuse-ld=ld), set MergeObjsCommand=ld
#
# $1 = the platform
# $2 = the variable to set with GHC options to configure gcc to use the chosen linker
#
AC_DEFUN([BINDIST_DETERMINE_LINKER],[
    AC_ARG_ENABLE(ld-override,
      [AS_HELP_STRING([--disable-ld-override],
        [Prevent GHC from overriding the default linker used by gcc. If ld-override is enabled GHC will try to tell gcc to use whichever linker is selected by the LD environment variable. [default=override enabled]])],
      [],
      [enable_ld_override=yes])

  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([AC_CANONICAL_TARGET])

  check_ld_path() {
       AC_MSG_CHECKING([whether C compiler supports --ld-path=[$]1])
       ld_path="[$]1"
       echo 'int main(void) { return 0; }' > conftest.c
       if $CC -o conftest.o "--ld-path=$ld_path" $LDFLAGS conftest.c > /dev/null 2>&1
       then
           AC_MSG_RESULT([yes])
           ld_path_ok=yes
       else
           AC_MSG_RESULT([no])
           ld_path_ok=no
       fi
       rm -f conftest.c conftest.o
  }

  check_fuse_ld() {
    AC_MSG_CHECKING([whether C compiler supports -fuse-ld=[$]1])
    ld="[$]1"
    echo 'int main(void) {return 0;}' > conftest.c
    if $CC -o conftest.o -fuse-ld=[$]1 $LDFLAGS conftest.c > /dev/null 2>&1
    then
        AC_MSG_RESULT([yes])
        fuse_ld_ok=yes
    else
        AC_MSG_RESULT([no])
        fuse_ld_ok=no
    fi
    rm -f conftest.c conftest.o
  }

  try_set_linker_to() {
    AC_MSG_CHECKING([whether linker can be set to [$]1])
    tmp_ld=[$]1
    abs_path=`command -v "$tmp_ld" 2>/dev/null` # get absolute path of $tmp_ld if it isn't already one
    ld_path_ok="no"
    if test "z$abs_path" != "z" && check_ld_path "$abs_path" && test "x$ld_path_ok" = "xyes";
       then $2="--ld-path=$abs_path"
            AC_CHECK_TARGET_TOOL([LD], [$abs_path])
            linker_set_successfully=yes
       else # --ld-path does not work or $LD cannot be resolved to an absolute path
            fuse_ld_ok=no
            if check_fuse_ld "$tmp_ld"  && test "x$fuse_ld_ok" = "xyes";
               then $2="-fuse-ld=$tmp_ld"
                    AC_CHECK_TARGET_TOOL([LD], [$tmp_ld])
                    linker_set_successfully=yes
               else AC_MSG_WARN(["$tmp_ld could not be set via either '--ld-path' or '-fuse-ld"])
                    linker_set_successfully=no
            fi
    fi
  }

  # we are lenient when $LD=ld and just act as if $LD wasn't set and 
  # enable-ld-override is off
  if test "z$LD" != "z" && test "z$LD" != "zld";
     then linker_set_successfully=no
          try_set_linker_to "$LD"
          if test "z$linker_set_successfully" != "zyes";
             then AC_MSG_FAILURE([ $tmp_ld is an invalid linker. If your C compiler accepts the '--ld-path' flag,
                  \$LD can be either of an executable name that is in \$PATH *or* a path to an executable.
                  If your C compiler only supports the '--fuse-ld' flag, \$LD can only be one of the linker flavours supported
                  by it. Mind that if your C compiler supports '--ld-path', 'configure' will always prefer using an absolute path
                  to your linker as that is less error-prone.])
          fi

     else # $LD is not set -- we will do the ld-override non-sense
          if test "x$enable_ld_override" = "xyes" && test "z$LD" != "zld" && case "$1" in
                    *-darwin) false ;; # don't do the ld override thing on macos
                    *) true ;;
                  esac;
             then
                  AC_MSG_NOTICE(["enable ld override was set and no more specific linker was chosen by setting \$LD, trying to find best possible linker"])
                  try_set_linker_to "lld"
                  if test "z$linker_set_successfully" != "zyes";
                     then # ... bail out, just use ld in path
                          $2=""
                          AC_CHECK_TARGET_TOOL([LD], [ld])
                  fi
             else # ld override is not set and $LD not set either
                  $2=""
                  AC_CHECK_TARGET_TOOL([LD], [ld])
          fi
  fi

  AC_MSG_NOTICE([linker discovery set $2 set to $$2])

  CHECK_LD_COPY_BUG([$1])
])
