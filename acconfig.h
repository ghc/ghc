/* acconfig.h

   Descriptive text for the C preprocessor macros that
   the fptools configuration script can define.
   The current version may not use all of them; autoheader copies the ones
   your configure.in uses into your configuration header file templates.

   The entries are in sort -df order: alphabetical, case insensitive,
   ignoring punctuation (such as underscores).  Although this order
   can split up related entries, it makes it easier to check whether
   a given entry is in the file.

   Leave the following blank line there!!  Autoheader needs it.  */


@TOP@

/* Define to alignment constraint on chars */
#undef ALIGNMENT_CHAR

/* Define to alignment constraint on doubles */
#undef ALIGNMENT_DOUBLE

/* Define to alignment constraint on floats */
#undef ALIGNMENT_FLOAT

/* Define to alignment constraint on ints */
#undef ALIGNMENT_INT

/* Define to alignment constraint on longs */
#undef ALIGNMENT_LONG

/* Define to alignment constraint on long longs */
#undef ALIGNMENT_LONG_LONG

/* Define to alignment constraint on shorts */
#undef ALIGNMENT_SHORT

/* Define to alignment constraint on unsigned chars */
#undef ALIGNMENT_UNSIGNED_CHAR

/* Define to alignment constraint on unsigned ints */
#undef ALIGNMENT_UNSIGNED_INT

/* Define to alignment constraint on unsigned longs */
#undef ALIGNMENT_UNSIGNED_LONG

/* Define to alignment constraint on unsigned long longs */
#undef ALIGNMENT_UNSIGNED_LONG_LONG

/* Define to alignment constraint on unsigned shorts */
#undef ALIGNMENT_UNSIGNED_SHORT

/* Define to alignment constraint on void pointers */
#undef ALIGNMENT_VOID_P

/* Define if code lives before data in memory */
#undef CODE_BEFORE_DATA

/* Define as the symbol which marks the end of the data section */
#undef DATA_SECTION_END_MARKER

/* Define as the decl which terminates the data section */
#undef DATA_SECTION_END_MARKER_DECL

/* Define if time.h or sys/time.h define the altzone variable */
#undef HAVE_ALTZONE

/* Define if you have /bin/sh */
#define HAVE_BIN_SH 0

/* Define if you have the GetModuleFileName function.  */
#define HAVE_GETMODULEFILENAME 0

/* Define if C compiler supports long long types */
#undef HAVE_LONG_LONG

/* Define if fcntl.h defines O_BINARY */
#undef HAVE_O_BINARY

/* Define if compiler supports prototypes. */
#define HAVE_PROTOTYPES 0

/* Define if readline/readline.h and readline/history.h exist */
#undef HAVE_READLINE_HEADERS

/* Define if readline plus any additional libs needed for it exist */
#undef HAVE_READLINE_LIBS

/* Define if time.h or sys/time.h define the timezone variable */
#undef HAVE_TIMEZONE

/* Define if you have the WinExec function.  */
#define HAVE_WINEXEC 0

/* Define if you support the production (and use) of Win32 DLLs. */
#undef HAVE_WIN32_DLL_SUPPORT

/* Define if C Symbols have a leading underscore added by the compiler */
#undef LEADING_UNDERSCORE

/* Define as the symbol which marks the end of the text section */
#undef TEXT_SECTION_END_MARKER

/* Define to decl that terminates text section. */
#undef TEXT_SECTION_END_MARKER_DECL

/* Define to the type of the timezone variable (usually long or time_t) */
#undef TYPE_TIMEZONE

/* Define if signal handlers have type void (*)(int)
 * (Otherwise, they're assumed to have type int (*)(void).)
 */
#define VOID_INT_SIGNALS 0
 

/* Leave that blank line there!!  Autoheader needs it.
   If you're adding to this file, keep in mind:
   The entries are in sort -df order: alphabetical, case insensitive,
   ignoring punctuation (such as underscores).  */


/* autoheader doesn't grok AC_CHECK_LIB_NOWARN so we have to add them
   manually.  */

@BOTTOM@
