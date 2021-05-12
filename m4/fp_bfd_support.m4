# FP_BFD_SUPPORT()
# ----------------------
# whether to use libbfd for debugging RTS
AC_DEFUN([FP_BFD_SUPPORT], [
    HaveLibbfd=NO
    AC_ARG_ENABLE(bfd-debug,
        [AS_HELP_STRING([--enable-bfd-debug],
              [Enable symbol resolution for -debug rts ('+RTS -Di') via binutils' libbfd [default=no]])],
        [
            # don't pollute general LIBS environment
            save_LIBS="$LIBS"
            AC_CHECK_HEADERS([bfd.h])
            dnl ** check whether this machine has BFD and libiberty installed (used for debugging)
            dnl    the order of these tests matters: bfd needs libiberty
            AC_CHECK_LIB(iberty, xmalloc)
            dnl 'bfd_init' is a rare non-macro in libbfd
            AC_CHECK_LIB(bfd,    bfd_init)

            AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <bfd.h>]],
                        [[
                                /* mimic our rts/Printer.c */
                                bfd* abfd;
                                const char * name;
                                char **matching;

                                name = "some.executable";
                                bfd_init();
                                abfd = bfd_openr(name, "default");
                                bfd_check_format_matches (abfd, bfd_object, &matching);
                                {
                                    long storage_needed;
                                    storage_needed = bfd_get_symtab_upper_bound (abfd);
                                }
                                {
                                    asymbol **symbol_table;
                                    long number_of_symbols;
                                    symbol_info info;

                                    number_of_symbols = bfd_canonicalize_symtab (abfd, symbol_table);
                                    bfd_get_symbol_info(abfd,symbol_table[0],&info);
                                }
                        ]])],
                        HaveLibbfd=YES,dnl bfd seems to work
                        [AC_MSG_ERROR([can't use 'bfd' library])])
            LIBS="$save_LIBS"
        ]
    )
    AC_SUBST([UseLibbfd],[$HaveLibbfd])
])
