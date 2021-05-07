# CHECK_LD_COPY_BUG()
# -------------------
# Check for binutils bug #16177 present in some versions of the bfd ld
# implementation affecting ARM relocations.
# https://sourceware.org/bugzilla/show_bug.cgi?id=16177
#
# $1 = the platform
#
AC_DEFUN([CHECK_LD_COPY_BUG],[
    case $1 in
      arm*linux*)
        AC_CHECK_TARGET_TOOL([READELF], [readelf])
        AC_CHECK_TARGET_TOOL([AS], [as])
        AC_MSG_CHECKING([for ld bug 16177])
        cat >actest.s <<-EOF
          .globl _start
          .p2align 4
        _start:
          bkpt

        .data
          .globl data_object
        object_reference:
          .long data_object
          .size object_reference, 4
EOF

        cat >aclib.s <<-EOF
          .data
          .globl data_object
          .type data_object, %object
          .size data_object, 4
        data_object:
            .long 123
EOF

        $AS -o aclib.o aclib.s
        $LD -shared -o aclib.so aclib.o

        $AS -o actest.o actest.s
        $LD -o actest actest.o aclib.so

        if $READELF -r actest | grep R_ARM_COPY > /dev/null; then
            AC_MSG_RESULT([affected])
            AC_MSG_ERROR(
              [Your linker is affected by binutils #16177, which
               critically breaks linkage of GHC objects. Please either upgrade
               binutils or supply a different linker with the LD environment
               variable.])
        else
            AC_MSG_RESULT([unaffected])
        fi

        rm -f aclib.s aclib.o aclib.so actest.s actest.o actest
        ;;
      *)
        ;;
    esac
])


