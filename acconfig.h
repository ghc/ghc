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

/* Define to alignment constraint on doubles or on unsigned int - whichever is the greater */
#undef ALIGNMENT_DOUBLE

/* Define to alignment constraint on floats or on unsigned int - whichever is the greater */
#undef ALIGNMENT_FLOAT

/* Define to alignment constraint on doubles or on unsigned int - whichever is the greater */
#undef ALIGNMENT_LONG

/* Define to alignment constraint on unsigned int - whichever is the greater */
#undef ALIGNMENT_UNSIGNED_INT

/* Define if we're have GNU libc. */
#undef _GNU_SOURCE

/* Define if time.h or sys/time.h define the altzone variable */
#undef HAVE_ALTZONE

/* Define if C Symbols have a leading underscore added by the compiler */
#undef LEADING_UNDERSCORE

/* Define to the type of the timezone variable (usually long or time_t) */
#undef TYPE_TIMEZONE


/* Leave that blank line there!!  Autoheader needs it.
   If you're adding to this file, keep in mind:
   The entries are in sort -df order: alphabetical, case insensitive,
   ignoring punctuation (such as underscores).  */


/* autoheader doesn't grok AC_CHECK_LIB_NOWARN so we have to add them
   manually.  */

@BOTTOM@
