divert(-1)
dnl
dnl  m4 macros for gmp assembly code, shared by all CPUs.
dnl
dnl  These macros are designed for use with any m4 and have been used on
dnl  GNU, FreeBSD, OpenBSD and SysV.
dnl
dnl  GNU m4 and OpenBSD 2.7 m4 will give filenames and line numbers in error
dnl  messages.


dnl  Copyright (C) 1999, 2000 Free Software Foundation, Inc.
dnl 
dnl  This file is part of the GNU MP Library.
dnl
dnl  The GNU MP Library is free software; you can redistribute it and/or
dnl  modify it under the terms of the GNU Lesser General Public License as
dnl  published by the Free Software Foundation; either version 2.1 of the
dnl  License, or (at your option) any later version.
dnl
dnl  The GNU MP Library is distributed in the hope that it will be useful,
dnl  but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl  Lesser General Public License for more details.
dnl
dnl  You should have received a copy of the GNU Lesser General Public
dnl  License along with the GNU MP Library; see the file COPYING.LIB.  If
dnl  not, write to the Free Software Foundation, Inc., 59 Temple Place -
dnl  Suite 330, Boston, MA 02111-1307, USA.


dnl  Macros:
dnl
dnl  Most new m4 specific macros have an "m4_" prefix to emphasise they're
dnl  m4 expansions.  But new defining things like deflit() and defreg() are
dnl  named like the builtin define(), and forloop() is named following the
dnl  GNU m4 example on which it's based.
dnl
dnl  GNU m4 with the -P option uses "m4_" as a prefix for builtins, but that
dnl  option isn't going to be used, so there's no conflict or confusion.
dnl
dnl
dnl  Comments in output:
dnl
dnl  The m4 comment delimiters are left at # and \n, the normal assembler
dnl  commenting for most CPUs.  m4 passes comment text through without
dnl  expanding macros in it, which is generally a good thing since it stops
dnl  unexpected expansions and possible resultant errors.
dnl
dnl  But note that when a quoted string is being read, a # isn't special, so
dnl  apostrophes in comments in quoted strings must be avoided or they'll be
dnl  interpreted as a closing quote mark.  But when the quoted text is
dnl  re-read # will still act like a normal comment, supressing macro
dnl  expansion.
dnl
dnl  For example,
dnl
dnl          # apostrophes in comments that're outside quotes are ok
dnl          # and using macro names like PROLOGUE is ok too
dnl          ...
dnl          ifdef(`PIC',`
dnl                  # but apostrophes aren't ok inside quotes
dnl                  #                     ^--wrong
dnl                  ...
dnl                  # though macro names like PROLOGUE are still ok
dnl                  ...
dnl          ')
dnl
dnl  If macro expansion in a comment is wanted, use `#' in the .asm (ie. a
dnl  quoted hash symbol), which will turn into # in the .s but get
dnl  expansions done on that line.  This can make the .s more readable to
dnl  humans, but it won't make a blind bit of difference to the assembler.
dnl
dnl  All the above applies, mutatis mutandis, when changecom() is used to
dnl  select @ ! ; or whatever other commenting.
dnl
dnl
dnl  Variations in m4 affecting gmp:
dnl
dnl  $# - When a macro is called as "foo" with no brackets, BSD m4 sets $#
dnl       to 1, whereas GNU or SysV m4 set it to 0.  In all cases though
dnl       "foo()" sets $# to 1.  This is worked around in various places.
dnl
dnl  len() - When "len()" is given an empty argument, BSD m4 evaluates to
dnl       nothing, whereas GNU, SysV, and the new OpenBSD, evaluate to 0.
dnl       See m4_length() below which works around this.
dnl
dnl  translit() - GNU m4 accepts character ranges like A-Z, and the new
dnl       OpenBSD m4 does under option -g, but basic BSD and SysV don't.
dnl
dnl  popdef() - in BSD and SysV m4 popdef() takes multiple arguments and
dnl       pops each, but GNU m4 only takes one argument.
dnl
dnl  push back - BSD m4 has some limits on the amount of text that can be
dnl       pushed back.  The limit is reasonably big and so long as macros
dnl       don't gratuitously duplicate big arguments it isn't a problem.
dnl       Normally an error message is given, but sometimes it just hangs.
dnl
dnl  eval() &,|,^ - GNU and SysV m4 have bitwise operators &,|,^ available,
dnl       but BSD m4 doesn't (contrary to what the man page suggests) and
dnl       instead ^ is exponentiation.
dnl
dnl  eval() ?: - The C ternary operator "?:" is available in BSD m4, but not
dnl       in SysV or GNU m4 (as of GNU m4 1.4 and betas of 1.5).
dnl
dnl  eval() -2^31 - BSD m4 has a bug where an eval() resulting in -2^31
dnl       (ie. -2147483648) gives "-(".  Using -2147483648 within an
dnl       expression is ok, it just can't be a final result.  "-(" will of
dnl       course upset parsing, with all sorts of strange effects.
dnl
dnl  eval() <<,>> - SysV m4 doesn't support shift operators in eval() (on
dnl       SunOS 5.7 /usr/xpg4/m4 has them but /usr/ccs/m4 doesn't).  See
dnl       m4_lshift() and m4_rshift() below for workarounds.
dnl
dnl  m4wrap() - in BSD m4, m4wrap() replaces any previous m4wrap() string,
dnl       in SysV m4 it appends to it, and in GNU m4 it prepends.  See
dnl       m4wrap_prepend() below which brings uniformity to this.
dnl
dnl  __file__,__line__ - GNU m4 and OpenBSD 2.7 m4 provide these, and
dnl       they're used here to make error messages more informative.  GNU m4
dnl       gives an unhelpful "NONE 0" in an m4wrap(), but that's worked
dnl       around.
dnl
dnl  __file__ quoting - OpenBSD m4, unlike GNU m4, doesn't quote the
dnl       filename in __file__, so care should be taken that no macro has
dnl       the same name as a file, or an unwanted expansion will occur when
dnl       printing an error or warning.
dnl
dnl  OpenBSD 2.6 m4 - this m4 rejects decimal constants containing an 8 or 9
dnl       in eval(), making it pretty much unusable.  This bug is confined
dnl       to version 2.6 (it's not in 2.5, and has been fixed in 2.7).
dnl
dnl  SunOS /usr/bin/m4 - this m4 lacks a number of desired features,
dnl       including $# and $@, defn(), m4exit(), m4wrap(), pushdef(),
dnl       popdef().  /usr/5bin/m4 is a SysV style m4 which should always be
dnl       available, and "configure" will reject /usr/bin/m4 in favour of
dnl       /usr/5bin/m4 (if necessary).
dnl
dnl       The sparc code actually has modest m4 requirements currently and
dnl       could manage with /usr/bin/m4, but there's no reason to put our
dnl       macros through contortions when /usr/5bin/m4 is available or GNU
dnl       m4 can be installed.


ifdef(`__ASM_DEFS_M4_INCLUDED__',
`m4_error(`asm-defs.m4 already included, dont include it twice
')m4exit(1)')
define(`__ASM_DEFS_M4_INCLUDED__')


dnl  Detect and give a message about the unsuitable OpenBSD 2.6 m4.

ifelse(eval(89),89,,
`errprint(
`This m4 doesnt accept 8 and/or 9 in constants in eval(), making it unusable.
This is probably OpenBSD 2.6 m4 (September 1999).  Upgrade to OpenBSD 2.7,
or get a bug fix from the CVS (expr.c rev 1.9), or get GNU m4.  Dont forget
to configure with M4=/wherever/m4 if you install one of these in a directory
not in $PATH.
')m4exit(1)')


dnl  Detect and give a message about the unsuitable SunOS /usr/bin/m4.
dnl
dnl  Unfortunately this test doesn't work when m4 is run in the normal way
dnl  from mpn/Makefile with "m4 -DOPERATION_foo foo.asm", since the bad m4
dnl  takes "-" in "-D..." to mean read stdin, so it will look like it just
dnl  hangs.  But running "m4 asm-defs.m4" to try it out will work.
dnl
dnl  We'd like to abort immediately on finding a problem, but unfortunately
dnl  the bad m4 doesn't have an m4exit(), nor does an invalid eval() kill
dnl  it.  Unexpanded $#'s in some m4_assert_numargs() later on will comment
dnl  out some closing parentheses and kill it with "m4: arg stack overflow".

define(m4_dollarhash_works_test,``$#'')
ifelse(m4_dollarhash_works_test(x),1,,
`errprint(
`This m4 doesnt support $# and cant be used for GMP asm processing.
If this is on SunOS, ./configure should choose /usr/5bin/m4 if you have that
or can get it, otherwise install GNU m4.  Dont forget to configure with
M4=/wherever/m4 if you install in a directory not in $PATH.
')')
undefine(`m4_dollarhash_works_test')


dnl  --------------------------------------------------------------------------
dnl  Basic error handling things.


dnl  Usage: m4_dollarhash_1_if_noparen_p
dnl
dnl  Expand to 1 if a call "foo" gives $# set to 1 (as opposed to 0 like GNU
dnl  and SysV m4 give).

define(m4_dollarhash_1_if_noparen_test,`$#')
define(m4_dollarhash_1_if_noparen_p,
eval(m4_dollarhash_1_if_noparen_test==1))
undefine(`m4_dollarhash_1_if_noparen_test')


dnl  Usage: m4wrap_prepend(string)
dnl
dnl  Prepend the given string to what will be exapanded under m4wrap at the
dnl  end of input.
dnl
dnl  This macro exists to work around variations in m4wrap() behaviour in
dnl  the various m4s (notes at the start of this file).  Don't use m4wrap()
dnl  directly since it will interfere with this scheme.

define(m4wrap_prepend,
m4_assert_numargs(1)
`define(`m4wrap_string',`$1'defn(`m4wrap_string'))')

m4wrap(`m4wrap_string')
define(m4wrap_string,`')


dnl  Usage: m4_file_and_line
dnl
dnl  Expand to the current file and line number, if the GNU m4 extensions
dnl  __file__ and __line__ are available.
dnl
dnl  In GNU m4 1.4 at the end of input when m4wrap text is expanded,
dnl  __file__ is NONE and __line__ is 0, which is not a helpful thing to
dnl  print.  If m4_file_seen() has been called to note the last file seen,
dnl  then that file at a big line number is used, otherwise "end of input"
dnl  is used (although "end of input" won't parse as an error message).

define(m4_file_and_line,
`ifdef(`__file__',
`ifelse(__file__`'__line__,`NONE0',
`ifdef(`m4_file_seen_last',`m4_file_seen_last: 999999: ',`end of input: ')',
`__file__: __line__: ')')')


dnl  Usage: m4_errprint_commas(arg,...)
dnl
dnl  The same as errprint(), but commas are printed between arguments
dnl  instead of spaces.

define(m4_errprint_commas,
`errprint(`$1')dnl
ifelse(eval($#>1),1,`errprint(`,')m4_errprint_commas(shift($@))')')


dnl  Usage: m4_error(args...)
dnl         m4_warning(args...)
dnl
dnl  Print an error message, using m4_errprint_commas, prefixed with the
dnl  current filename and line number (if available).  m4_error sets up to
dnl  give an error exit at the end of processing, m4_warning just prints.
dnl  These macros are the recommended way to print errors.
dnl
dnl  The arguments here should be quoted in the usual way to prevent them
dnl  being expanded when the macro call is read.  (m4_error takes care not
dnl  to do any further expansion.)
dnl
dnl  For example,
dnl
dnl         m4_error(`some error message
dnl         ')
dnl
dnl  which prints
dnl
dnl         foo.asm:123: some error message
dnl
dnl  or if __file__ and __line__ aren't available
dnl
dnl         some error message
dnl
dnl  The "file:line:" format is a basic style, used by gcc and GNU m4, so
dnl  emacs and other editors will recognise it in their normal error message
dnl  parsing.

define(m4_warning,
`m4_errprint_commas(m4_file_and_line`'$@)')

define(m4_error,
`define(`m4_error_occurred',1)m4_warning($@)')

define(`m4_error_occurred',0)

dnl  This m4wrap_prepend() is first, so it'll be executed last.
m4wrap_prepend(
`ifelse(m4_error_occurred,1,
`m4_error(`Errors occurred during m4 processing
')m4exit(1)')')


dnl  Usage: m4_assert_numargs(num)
dnl
dnl  Put this unquoted on a line on its own at the start of a macro
dnl  definition to add some code to check that num many arguments get passed
dnl  to the macro.  For example,
dnl
dnl         define(foo,
dnl         m4_assert_numargs(2)
dnl         `something `$1' and `$2' blah blah')
dnl
dnl  Then a call like foo(one,two,three) will provoke an error like
dnl
dnl         file:10: foo expected 2 arguments, got 3 arguments
dnl
dnl  Here are some calls and how many arguments they're interpreted as passing.
dnl
dnl         foo(abc,def)  2
dnl         foo(xyz)      1
dnl         foo()         0
dnl         foo          -1
dnl
dnl  The -1 for no parentheses at all means a macro that's meant to be used
dnl  that way can be checked with m4_assert_numargs(-1).  For example,
dnl
dnl         define(SPECIAL_SUFFIX,
dnl         m4_assert_numargs(-1)
dnl         `ifdef(`FOO',`_foo',`_bar')')
dnl
dnl  But as an alternative see also deflit() below where parenthesized
dnl  expressions following a macro are passed through to the output.
dnl
dnl  Note that in BSD m4 there's no way to differentiate calls "foo" and
dnl  "foo()", so in BSD m4 the distinction between the two isn't enforced.
dnl  (In GNU and SysV m4 it can be checked, and is.)


dnl  m4_assert_numargs is able to check its own arguments by calling
dnl  assert_numargs_internal directly.
dnl
dnl  m4_doublequote($`'0) expands to ``$0'', whereas ``$`'0'' would expand
dnl  to `$`'0' and do the wrong thing, and likewise for $1.  The same is
dnl  done in other assert macros.
dnl
dnl  $`#' leaves $# in the new macro being defined, and stops # being
dnl  interpreted as a comment character.
dnl
dnl  `dnl ' means an explicit dnl isn't necessary when m4_assert_numargs is
dnl  used.  The space means that if there is a dnl it'll still work.

dnl  Usage: m4_doublequote(x) expands to ``x''
define(m4_doublequote,
`m4_assert_numargs_internal(`$0',1,$#,len(`$1'))``$1''')

define(m4_assert_numargs,
`m4_assert_numargs_internal(`$0',1,$#,len(`$1'))dnl
`m4_assert_numargs_internal'(m4_doublequote($`'0),$1,$`#',`len'(m4_doublequote($`'1)))`dnl '')

dnl  Called: m4_assert_numargs_internal(`macroname',wantargs,$#,len(`$1'))
define(m4_assert_numargs_internal,
`m4_assert_numargs_internal_check(`$1',`$2',m4_numargs_count(`$3',`$4'))')

dnl  Called: m4_assert_numargs_internal_check(`macroname',wantargs,gotargs)
dnl
dnl  If m4_dollarhash_1_if_noparen_p (BSD m4) then gotargs can be 0 when it
dnl  should be -1.  If wantargs is -1 but gotargs is 0 and the two can't be
dnl  distinguished then it's allowed to pass.
dnl
define(m4_assert_numargs_internal_check,
`ifelse(eval($2 == $3
             || ($2==-1 && $3==0 && m4_dollarhash_1_if_noparen_p)),0,
`m4_error(`$1 expected 'm4_Narguments(`$2')`, got 'm4_Narguments(`$3')
)')')

dnl  Called: m4_numargs_count($#,len(`$1'))
dnl  If $#==0 then -1 args, if $#==1 but len(`$1')==0 then 0 args, otherwise
dnl  $# args.
define(m4_numargs_count,
`ifelse($1,0, -1,
`ifelse(eval($1==1 && $2-0==0),1, 0, $1)')')

dnl  Usage: m4_Narguments(N)
dnl  "$1 argument" or "$1 arguments" with the plural according to $1.
define(m4_Narguments,
`$1 argument`'ifelse(`$1',1,,s)')


dnl  --------------------------------------------------------------------------
dnl  Additional error checking things.


dnl  Usage: m4_file_seen()
dnl
dnl  Record __file__ for the benefit of m4_file_and_line in m4wrap text.
dnl  The basic __file__ macro comes out quoted, like `foo.asm', and
dnl  m4_file_seen_last is defined like that too.
dnl
dnl  This only needs to be used with something that could generate an error
dnl  message in m4wrap text.  The x86 PROLOGUE is the only such at the
dnl  moment (at end of input its m4wrap checks for missing EPILOGUE).  A few
dnl  include()s can easily trick this scheme, but you'd expect an EPILOGUE
dnl  in the same file as the PROLOGUE.

define(m4_file_seen,
m4_assert_numargs(0)
`ifelse(__file__,`NONE',,
`define(`m4_file_seen_last',m4_doublequote(__file__))')')


dnl  Usage: m4_assert_onearg()
dnl
dnl  Put this, unquoted, at the start of a macro definition to add some code
dnl  to check that one argument is passed to the macro, but with that
dnl  argument allowed to be empty.  For example,
dnl
dnl          define(foo,
dnl          m4_assert_onearg()
dnl          `blah blah $1 blah blah')
dnl
dnl  Calls "foo(xyz)" or "foo()" are accepted.  A call "foo(xyz,abc)" fails.
dnl  A call "foo" fails too, but BSD m4 can't detect this case (GNU and SysV
dnl  m4 can).

define(m4_assert_onearg,
m4_assert_numargs(0)
`m4_assert_onearg_internal'(m4_doublequote($`'0),$`#')`dnl ')

dnl  Called: m4_assert_onearg(`macroname',$#)
define(m4_assert_onearg_internal,
`ifelse($2,1,,
`m4_error(`$1 expected 1 argument, got 'm4_Narguments(`$2')
)')')


dnl  Usage: m4_assert_numargs_range(low,high)
dnl
dnl  Put this, unquoted, at the start of a macro definition to add some code
dnl  to check that between low and high many arguments get passed to the
dnl  macro.  For example,
dnl
dnl         define(foo,
dnl         m4_assert_numargs_range(3,5)
dnl         `mandatory $1 $2 $3 optional $4 $5 end')
dnl
dnl  See m4_assert_numargs() for more info.

define(m4_assert_numargs_range,
m4_assert_numargs(2)
``m4_assert_numargs_range_internal'(m4_doublequote($`'0),$1,$2,$`#',`len'(m4_doublequote($`'1)))`dnl '')

dnl  Called: m4_assert_numargs_range_internal(`name',low,high,$#,len(`$1'))
define(m4_assert_numargs_range_internal,
m4_assert_numargs(5)
`m4_assert_numargs_range_check(`$1',`$2',`$3',m4_numargs_count(`$4',`$5'))')

dnl  Called: m4_assert_numargs_range_check(`name',low,high,gotargs)
dnl
dnl  If m4_dollarhash_1_if_noparen_p (BSD m4) then gotargs can be 0 when it
dnl  should be -1.  To ensure a `high' of -1 works, a fudge is applied to
dnl  gotargs if it's 0 and the 0 and -1 cases can't be distinguished.
dnl
define(m4_assert_numargs_range_check,
m4_assert_numargs(4)
`ifelse(eval($2 <= $4 &&
             ($4 - ($4==0 && m4_dollarhash_1_if_noparen_p) <= $3)),0,
`m4_error(`$1 expected $2 to $3 arguments, got 'm4_Narguments(`$4')
)')')


dnl  Usage: m4_assert_defined(symbol)
dnl
dnl  Put this unquoted on a line of its own at the start of a macro
dnl  definition to add some code to check that the given symbol is defined
dnl  when the macro is used.  For example,
dnl
dnl          define(foo,
dnl          m4_assert_defined(`FOO_PREFIX')
dnl          `FOO_PREFIX whatever')
dnl
dnl  This is a convenient way to check that the user or ./configure or
dnl  whatever has defined the things needed by a macro, as opposed to
dnl  silently generating garbage.

define(m4_assert_defined,
m4_assert_numargs(1)
``m4_assert_defined_internal'(m4_doublequote($`'0),``$1'')`dnl '')

dnl  Called: m4_assert_defined_internal(`macroname',`define_required')
define(m4_assert_defined_internal,
m4_assert_numargs(2)
`ifdef(`$2',,
`m4_error(`$1 needs $2 defined
')')')


dnl  Usage: m4_not_for_expansion(`SYMBOL')
dnl         define_not_for_expansion(`SYMBOL')
dnl
dnl  m4_not_for_expansion turns SYMBOL, if defined, into something which
dnl  will give an error if expanded.  For example,
dnl
dnl         m4_not_for_expansion(`PIC')
dnl
dnl  define_not_for_expansion is the same, but always makes a definition.
dnl
dnl  These are for symbols that should be tested with ifdef(`FOO',...)
dnl  rather than be expanded as such.  They guard against accidentally
dnl  omitting the quotes, as in ifdef(FOO,...).  Note though that they only
dnl  catches this when FOO is defined, so be sure to test code both with and
dnl  without each definition.

define(m4_not_for_expansion,
m4_assert_numargs(1)
`ifdef(`$1',`define_not_for_expansion(`$1')')')

define(define_not_for_expansion,
m4_assert_numargs(1)
`ifelse(defn(`$1'),,,
`m4_error(``$1' has a non-empty value, maybe it shouldnt be munged with m4_not_for_expansion()
')')dnl
define(`$1',`m4_not_for_expansion_internal(`$1')')')

define(m4_not_for_expansion_internal,
`m4_error(``$1' is not meant to be expanded, perhaps you mean `ifdef(`$1',...)'
')')


dnl  --------------------------------------------------------------------------
dnl  Various generic m4 things.


dnl  Usage: m4_ifdef_anyof_p(`symbol',...)
dnl
dnl  Expand to 1 if any of the symbols in the argument list are defined, or
dnl  to 0 if not.

define(m4_ifdef_anyof_p,
`ifelse(eval($#<=1 && m4_length(`$1')==0),1, 0,
`ifdef(`$1', 1,
`m4_ifdef_anyof_p(shift($@))')')')


dnl  Usage: m4_length(string)
dnl
dnl  Determine the length of a string.  This is the same as len(), but
dnl  always expands to a number, working around the BSD len() which
dnl  evaluates to nothing given an empty argument.

define(m4_length,
m4_assert_onearg()
`eval(len(`$1')-0)')


dnl  Usage: m4_stringequal_p(x,y)
dnl
dnl  Expand to 1 or 0 according as strings x and y are equal or not.

define(m4_stringequal_p,
`ifelse(`$1',`$2',1,0)')


dnl  Usage: m4_incr_or_decr(n,last)
dnl
dnl  Do an incr(n) or decr(n), whichever is in the direction of "last".
dnl  Both n and last must be numbers of course.

define(m4_incr_or_decr,
m4_assert_numargs(2)
`ifelse(eval($1<$2),1,incr($1),decr($1))')


dnl  Usage: forloop(i, first, last, statement)
dnl
dnl  Based on GNU m4 examples/forloop.m4, but extended.
dnl
dnl  statement is expanded repeatedly, with i successively defined as
dnl
dnl         first, first+1, ..., last-1, last
dnl
dnl  Or if first > last, then it's
dnl
dnl         first, first-1, ..., last+1, last
dnl
dnl  If first == last, then one expansion is done.
dnl
dnl  A pushdef/popdef of i is done to preserve any previous definition (or
dnl  lack of definition).  first and last are eval()ed and so can be
dnl  expressions.
dnl
dnl  forloop_first is defined to 1 on the first iteration, 0 on the rest.
dnl  forloop_last is defined to 1 on the last iteration, 0 on the others.
dnl  Nested forloops are allowed, in which case forloop_first and
dnl  forloop_last apply to the innermost loop that's open.
dnl
dnl  A simple example,
dnl
dnl         forloop(i, 1, 2*2+1, `dnl
dnl         iteration number i ... ifelse(forloop_first,1,FIRST)
dnl         ')


dnl  "i" and "statement" are carefully quoted, but "first" and "last" are
dnl  just plain numbers once eval()ed.

define(`forloop',
m4_assert_numargs(4)
`pushdef(`$1',eval(`$2'))dnl
pushdef(`forloop_first',1)dnl
pushdef(`forloop_last',0)dnl
forloop_internal(`$1',eval(`$3'),`$4')`'dnl
popdef(`forloop_first')dnl
popdef(`forloop_last')dnl
popdef(`$1')')

dnl  Called: forloop_internal(`var',last,statement)
define(`forloop_internal',
m4_assert_numargs(3)
`ifelse($1,$2,
`define(`forloop_last',1)$3',
`$3`'dnl
define(`forloop_first',0)dnl
define(`$1',m4_incr_or_decr($1,$2))dnl
forloop_internal(`$1',$2,`$3')')')


dnl  Usage: m4_toupper(x)
dnl         m4_tolower(x)
dnl
dnl  Convert the argument string to upper or lower case, respectively.
dnl  Only one argument accepted.
dnl
dnl  BSD m4 doesn't take ranges like a-z in translit(), so the full alphabet
dnl  is written out.

define(m4_alphabet_lower, `abcdefghijklmnopqrstuvwxyz')
define(m4_alphabet_upper, `ABCDEFGHIJKLMNOPQRSTUVWXYZ')

define(m4_toupper,
m4_assert_onearg()
`translit(`$1', m4_alphabet_lower, m4_alphabet_upper)')

define(m4_tolower,
m4_assert_onearg()
`translit(`$1', m4_alphabet_upper, m4_alphabet_lower)')


dnl  Usage: m4_empty_if_zero(x)
dnl
dnl  Evaluate to x, or to nothing if x is 0.  x is eval()ed and so can be an
dnl  expression.
dnl
dnl  This is useful for x86 addressing mode displacements since forms like
dnl  (%ebx) are one byte shorter than 0(%ebx).  A macro `foo' for use as
dnl  foo(%ebx) could be defined with the following so it'll be empty if the
dnl  expression comes out zero.
dnl
dnl	   deflit(`foo', `m4_empty_if_zero(a+b*4-c)')
dnl
dnl  Naturally this shouldn't be done if, say, a computed jump depends on
dnl  the code being a particular size.

define(m4_empty_if_zero,
m4_assert_onearg()
`ifelse(eval($1),0,,eval($1))')


dnl  Usage: m4_log2(x)
dnl
dnl  Calculate a logarithm to base 2.
dnl  x must be an integral power of 2, between 2**0 and 2**30.
dnl  x is eval()ed, so it can be an expression.
dnl  An error results if x is invalid.
dnl
dnl  2**31 isn't supported, because an unsigned 2147483648 is out of range
dnl  of a 32-bit signed int.  Also, the bug in BSD m4 where an eval()
dnl  resulting in 2147483648 (or -2147483648 as the case may be) gives `-('
dnl  means tests like eval(1<<31==(x)) would be necessary, but that then
dnl  gives an unattractive explosion of eval() error messages if x isn't
dnl  numeric.

define(m4_log2,
m4_assert_numargs(1)
`m4_log2_internal(0,1,eval(`$1'))')

dnl  Called: m4_log2_internal(n,2**n,target)
define(m4_log2_internal,
m4_assert_numargs(3)
`ifelse($2,$3,$1,
`ifelse($1,30,
`m4_error(`m4_log2() argument too big or not a power of two: $3
')',
`m4_log2_internal(incr($1),eval(2*$2),$3)')')')


dnl  Usage:  m4_div2_towards_zero
dnl
dnl  m4 division is probably whatever a C signed division is, and C doesn't
dnl  specify what rounding gets used on negatives, so this expression forces
dnl  a rounding towards zero.

define(m4_div2_towards_zero,
m4_assert_numargs(1)
`eval((($1) + ((($1)<0) & ($1))) / 2)')


dnl  Usage: m4_lshift(n,count)
dnl         m4_rshift(n,count)
dnl
dnl  Calculate n shifted left or right by count many bits.  Both n and count
dnl  are eval()ed and so can be expressions.
dnl
dnl  Negative counts are allowed and mean a shift in the opposite direction.
dnl  Negative n is allowed and right shifts will be arithmetic (meaning
dnl  divide by 2**count, rounding towards zero, also meaning the sign bit is
dnl  duplicated).
dnl
dnl  Use these macros instead of << and >> in eval() since the basic ccs
dnl  SysV m4 doesn't have those operators.

define(m4_rshift,
m4_assert_numargs(2)
`m4_lshift(`$1',-(`$2'))')

define(m4_lshift,
m4_assert_numargs(2)
`m4_lshift_internal(eval(`$1'),eval(`$2'))')

define(m4_lshift_internal,
m4_assert_numargs(2)
`ifelse(eval($2-0==0),1,$1,
`ifelse(eval($2>0),1,
`m4_lshift_internal(eval($1*2),decr($2))',
`m4_lshift_internal(m4_div2_towards_zero($1),incr($2))')')')


dnl  Usage: deflit(name,value)
dnl
dnl  Like define(), but "name" expands like a literal, rather than taking
dnl  arguments.  For example "name(%eax)" expands to "value(%eax)".
dnl
dnl  Limitations:
dnl
dnl  $ characters in the value part must have quotes to stop them looking
dnl  like macro parameters.  For example, deflit(reg,`123+$`'4+567').  See
dnl  defreg() below for handling simple register definitions like $7 etc.
dnl
dnl  "name()" is turned into "name", unfortunately.  In GNU and SysV m4 an
dnl  error is generated when this happens, but in BSD m4 it will happen
dnl  silently.  The problem is that in BSD m4 $# is 1 in both "name" or
dnl  "name()", so there's no way to differentiate them.  Because we want
dnl  plain "name" to turn into plain "value", we end up with "name()"
dnl  turning into plain "value" too.
dnl
dnl  "name(foo)" will lose any whitespace after commas in "foo", for example
dnl  "disp(%eax, %ecx)" would become "128(%eax,%ecx)".
dnl
dnl  These parentheses oddities shouldn't matter in assembler text, but if
dnl  they do the suggested workaround is to write "name ()" or "name (foo)"
dnl  to stop the parentheses looking like a macro argument list.  If a space
dnl  isn't acceptable in the output, then write "name`'()" or "name`'(foo)".
dnl  The `' is stripped when read, but again stops the parentheses looking
dnl  like parameters.

dnl  Quoting for deflit_emptyargcheck is similar to m4_assert_numargs.  The
dnl  stuff in the ifelse gives a $#, $1 and $@ evaluated in the new macro
dnl  created, not in deflit.
define(deflit,
m4_assert_numargs(2)
`define(`$1',
`deflit_emptyargcheck'(``$1'',$`#',m4_doublequote($`'1))`dnl
$2`'dnl
ifelse(eval($'`#>1 || m4_length('m4_doublequote($`'1)`)!=0),1,($'`@))')')

dnl  Called: deflit_emptyargcheck(macroname,$#,`$1')
define(deflit_emptyargcheck,
`ifelse(eval($2==1 && !m4_dollarhash_1_if_noparen_p && m4_length(`$3')==0),1,
`m4_error(`dont use a deflit as $1() because it loses the brackets (see deflit in asm-incl.m4 for more information)
')')')


dnl  Usage: m4_assert(`expr')
dnl
dnl  Test a compile-time requirement with an m4 expression.  The expression
dnl  should be quoted, and will be eval()ed and expected to be non-zero.
dnl  For example,
dnl
dnl         m4_assert(`FOO*2+6 < 14')

define(m4_assert,
m4_assert_numargs(1)
`ifelse(eval($1),1,,
`m4_error(`assertion failed: $1
')')')


dnl  --------------------------------------------------------------------------
dnl  Various assembler things, not specific to any particular CPU.
dnl


dnl  Usage: include_mpn(`filename')
dnl
dnl  Like include(), but adds a path to the mpn source directory.  For
dnl  example,
dnl
dnl         include_mpn(`sparc64/addmul_1h.asm')

define(include_mpn,
m4_assert_numargs(1)
m4_assert_defined(`CONFIG_TOP_SRCDIR')
`include(CONFIG_TOP_SRCDIR`/mpn/$1')')


dnl  Usage: C comment ...
dnl
dnl  "C" works like a FORTRAN-style comment character.  This can be used for
dnl  comments to the right of assembly instructions, where just dnl would
dnl  remove the linefeed, and concatenate adjacent lines.
dnl
dnl  "C" and/or "dnl" are useful when an assembler doesn't support comments,
dnl  or where different assemblers for a particular CPU have different
dnl  comment styles.  The intermediate ".s" files will end up with no
dnl  comments, just code.
dnl
dnl  Using "C" is not intended to cause offence to anyone who doesn't like
dnl  FORTRAN; but if that happens it's an unexpected bonus.

define(C, `
dnl')


dnl  Various possible defines passed from the Makefile that are to be tested
dnl  with ifdef() rather than be expanded.

m4_not_for_expansion(`PIC')

dnl  aors_n
m4_not_for_expansion(`OPERATION_add_n')
m4_not_for_expansion(`OPERATION_sub_n')

dnl  aorsmul_n
m4_not_for_expansion(`OPERATION_addmul_1')
m4_not_for_expansion(`OPERATION_submul_1')

dnl  logops_n
m4_not_for_expansion(`OPERATION_and_n')
m4_not_for_expansion(`OPERATION_andn_n')
m4_not_for_expansion(`OPERATION_nand_n')
m4_not_for_expansion(`OPERATION_ior_n')
m4_not_for_expansion(`OPERATION_iorn_n')
m4_not_for_expansion(`OPERATION_nior_n')
m4_not_for_expansion(`OPERATION_xor_n')
m4_not_for_expansion(`OPERATION_xnor_n')

dnl  popham
m4_not_for_expansion(`OPERATION_popcount')
m4_not_for_expansion(`OPERATION_hamdist')


dnl  Usage: m4_config_gmp_mparam(`symbol')
dnl
dnl  Check that `symbol' is defined.  If it isn't, issue an error and
dnl  terminate immediately.  The error message explains that the symbol
dnl  should be in config.m4, copied from gmp-mparam.h.
dnl
dnl  Processing is terminated immediately since missing something like
dnl  KARATSUBA_SQR_THRESHOLD can lead to infinite loops with endless error
dnl  messages.

define(m4_config_gmp_mparam,
m4_assert_numargs(1)
`ifdef(`$1',,
`m4_error(`$1 is not defined.
	"configure" should have extracted this from gmp-mparam.h and put it
	in config.m4, but somehow this has failed.
')m4exit(1)')')


dnl  Usage: defreg(name,reg)
dnl
dnl  Give a name to a $ style register.  For example,
dnl
dnl         defreg(foo,$12)
dnl
dnl  defreg() inserts an extra pair of quotes after the $ so that it's not
dnl  interpreted as an m4 macro parameter, ie. foo is actually $`'12.  m4
dnl  strips those quotes when foo is expanded.
dnl
dnl  deflit() is used to make the new definition, so it will expand
dnl  literally even if followed by parentheses ie. foo(99) will become
dnl  $12(99).  (But there's nowhere that would be used is there?)
dnl
dnl  When making further definitions from existing defreg() macros, remember
dnl  to use defreg() again to protect the $ in the new definitions too.  For
dnl  example,
dnl
dnl         defreg(a0,$4)
dnl         defreg(a1,$5)
dnl         ...
dnl
dnl         defreg(PARAM_DST,a0)
dnl
dnl  This is only because a0 is expanding at the time the PARAM_DST
dnl  definition is made, leaving a literal $4 that must be re-quoted.  On
dnl  the other hand in something like the following ra is only expanded when
dnl  ret is used and its $`'31 protection will have its desired effect at
dnl  that time.
dnl
dnl         defreg(ra,$31)
dnl         ...
dnl         define(ret,`j ra')
dnl
dnl  Note that only $n forms are meant to be used here, and something like
dnl  128($30) doesn't get protected and will come out wrong.

define(defreg,
m4_assert_numargs(2)
`deflit(`$1',
substr(`$2',0,1)``''substr(`$2',1))')


dnl  Usage: m4_instruction_wrapper(num)
dnl
dnl  Put this, unquoted, on a line on its own, at the start of a macro
dnl  that's a wrapper around an assembler instruction.  It adds code to give
dnl  a descriptive error message if the macro is invoked without arguments.
dnl
dnl  For example, suppose jmp needs to be wrapped,
dnl
dnl         define(jmp,
dnl         m4_instruction_wrapper()
dnl         m4_assert_numargs(1)
dnl                 `.byte 0x42
dnl                 .long  $1
dnl                 nop')
dnl
dnl  The point of m4_instruction_wrapper is to get a better error message
dnl  than m4_assert_numargs would give if jmp is accidentally used as plain
dnl  "jmp foo" instead of the intended "jmp( foo)".  "jmp()" with no
dnl  argument also provokes the error message.
dnl
dnl  m4_instruction_wrapper should only be used with wrapped instructions
dnl  that take arguments, since obviously something meant to be used as
dnl  plain "ret", say, doesn't want to give an error when used that way.

define(m4_instruction_wrapper,
m4_assert_numargs(0)
``m4_instruction_wrapper_internal'(m4_doublequote($`'0),dnl
m4_doublequote(ifdef(`__file__',__file__,`the m4 sources')),dnl
$`#',m4_doublequote($`'1))`dnl'')

dnl  Called: m4_instruction_wrapper_internal($0,`filename',$#,$1)
define(m4_instruction_wrapper_internal,
`ifelse(eval($3<=1 && m4_length(`$4')==0),1,
`m4_error(`$1 is a macro replacing that instruction and needs arguments, see $2 for details
')')')


dnl  Usage: UNROLL_LOG2, UNROLL_MASK, UNROLL_BYTES
dnl         CHUNK_LOG2, CHUNK_MASK, CHUNK_BYTES
dnl
dnl  When code supports a variable amount of loop unrolling, the convention
dnl  is to define UNROLL_COUNT to the number of limbs processed per loop.
dnl  When testing code this can be varied to see how much the loop overhead
dnl  is costing.  For example,
dnl
dnl         deflit(UNROLL_COUNT, 32)
dnl
dnl  If the forloop() generating the unrolled loop has a pattern processing
dnl  more than one limb, the convention is to express this with CHUNK_COUNT.
dnl  For example,
dnl
dnl         deflit(CHUNK_COUNT, 2)
dnl
dnl  The LOG2, MASK and BYTES definitions below are derived from these COUNT
dnl  definitions.  If COUNT is redefined, the LOG2, MASK and BYTES follow
dnl  the new definition automatically.
dnl
dnl  LOG2 is the log base 2 of COUNT.  MASK is COUNT-1, which can be used as
dnl  a bit mask.  BYTES is BYTES_PER_MP_LIMB*COUNT, the number of bytes
dnl  processed in each unrolled loop.
dnl
dnl  BYTES_PER_MP_LIMB is defined in a CPU specific m4 include file.  It
dnl  exists only so the BYTES definitions here can be common to all CPUs.
dnl  In the actual code for a given CPU, an explicit 4 or 8 may as well be
dnl  used because the code is only for a particular CPU, it doesn't need to
dnl  be general.
dnl
dnl  Note that none of these macros do anything except give conventional
dnl  names to commonly used things.  You still have to write your own
dnl  expressions for a forloop() and the resulting address displacements.
dnl  Something like the following would be typical for 4 bytes per limb.
dnl
dnl         forloop(`i',0,UNROLL_COUNT-1,`
dnl                 deflit(`disp',eval(i*4))
dnl                 ...
dnl         ')
dnl
dnl  Or when using CHUNK_COUNT,
dnl
dnl         forloop(`i',0,UNROLL_COUNT/CHUNK_COUNT-1,`
dnl                 deflit(`disp0',eval(i*CHUNK_COUNT*4))
dnl                 deflit(`disp1',eval(disp0+4))
dnl                 ...
dnl         ')
dnl
dnl  Clearly `i' can be run starting from 1, or from high to low or whatever
dnl  best suits.

deflit(UNROLL_LOG2,
m4_assert_defined(`UNROLL_COUNT')
`m4_log2(UNROLL_COUNT)')

deflit(UNROLL_MASK,
m4_assert_defined(`UNROLL_COUNT')
`eval(UNROLL_COUNT-1)')

deflit(UNROLL_BYTES,
m4_assert_defined(`UNROLL_COUNT')
m4_assert_defined(`BYTES_PER_MP_LIMB')
`eval(UNROLL_COUNT * BYTES_PER_MP_LIMB)')
 
deflit(CHUNK_LOG2,
m4_assert_defined(`CHUNK_COUNT')
`m4_log2(CHUNK_COUNT)')

deflit(CHUNK_MASK,
m4_assert_defined(`CHUNK_COUNT')
`eval(CHUNK_COUNT-1)')

deflit(CHUNK_BYTES,
m4_assert_defined(`CHUNK_COUNT')
m4_assert_defined(`BYTES_PER_MP_LIMB')
`eval(CHUNK_COUNT * BYTES_PER_MP_LIMB)')


dnl  Usage: MPN(name)
dnl
dnl  Add MPN_PREFIX to a name.
dnl  MPN_PREFIX defaults to "__gmpn_" if not defined.

ifdef(`MPN_PREFIX',,
`define(`MPN_PREFIX',`__gmpn_')')

define(MPN,
m4_assert_numargs(1)
`MPN_PREFIX`'$1')


dnl  Usage: mpn_add_n, etc
dnl
dnl  Convenience definitions using MPN(), like the #defines in gmp.h.  Each
dnl  function that might be implemented in assembler is here.

define(define_mpn,
m4_assert_numargs(1)
`define(`mpn_$1',`MPN(`$1')')')

define_mpn(add)
define_mpn(add_1)
define_mpn(add_n)
define_mpn(add_nc)
define_mpn(addmul_1)
define_mpn(addmul_1c)
define_mpn(addsub_n)
define_mpn(addsub_nc)
define_mpn(and_n)
define_mpn(andn_n)
define_mpn(bdivmod)
define_mpn(cmp)
define_mpn(com_n)
define_mpn(copyd)
define_mpn(copyi)
define_mpn(divexact_by3c)
define_mpn(divrem)
define_mpn(divrem_1)
define_mpn(divrem_1c)
define_mpn(divrem_2)
define_mpn(divrem_classic)
define_mpn(divrem_newton)
define_mpn(dump)
define_mpn(gcd)
define_mpn(gcd_1)
define_mpn(gcdext)
define_mpn(get_str)
define_mpn(hamdist)
define_mpn(invert_limb)
define_mpn(ior_n)
define_mpn(iorn_n)
define_mpn(kara_mul_n)
define_mpn(kara_sqr_n)
define_mpn(lshift)
define_mpn(lshiftc)
define_mpn(mod_1)
define_mpn(mod_1c)
define_mpn(mul)
define_mpn(mul_1)
define_mpn(mul_1c)
define_mpn(mul_basecase)
define_mpn(mul_n)
define_mpn(perfect_square_p)
define_mpn(popcount)
define_mpn(preinv_mod_1)
define_mpn(nand_n)
define_mpn(nior_n)
define_mpn(random)
define_mpn(random2)
define_mpn(rshift)
define_mpn(rshiftc)
define_mpn(scan0)
define_mpn(scan1)
define_mpn(set_str)
define_mpn(sqr_basecase)
define_mpn(sub_n)
define_mpn(sqrtrem)
define_mpn(sub)
define_mpn(sub_1)
define_mpn(sub_n)
define_mpn(sub_nc)
define_mpn(submul_1)
define_mpn(submul_1c)
define_mpn(toom3_mul_n)
define_mpn(toom3_sqr_n)
define_mpn(umul_ppmm)
define_mpn(udiv_qrnnd)
define_mpn(xnor_n)
define_mpn(xor_n)

define(`ASM_START',
	`')

define(`PROLOGUE',
	`
	TEXT
	ALIGN(4)
	GLOBL	GSYM_PREFIX`$1'
	TYPE(GSYM_PREFIX`$1',`function')
GSYM_PREFIX`$1':')

define(`EPILOGUE',
	`
	SIZE(GSYM_PREFIX`$1',.-GSYM_PREFIX`$1')')

dnl  LSYM_PREFIX might be L$, so defn() must be used to quote it or the L
dnl  will expand as the L macro, an infinite recursion.
define(`L',`defn(`LSYM_PREFIX')$1')

define(`INT32',
	`
	ALIGN(4)
$1:
	W32	$2
	')

define(`INT64',
	`
	ALIGN(8)
$1:
	W32	$2
	W32	$3
	')


dnl  Usage: ALIGN(bytes)
dnl
dnl  Emit a ".align" directive.  The alignment is specified in bytes, and
dnl  will normally need to be a power of 2.  The actual ".align" generated
dnl  is either bytes or logarithmic according to what ./configure detects.
dnl
dnl  ALIGN_FILL_0x90, if defined and equal to "yes", means a ", 0x90" should
dnl  be appended (this is for x86).

define(ALIGN,
m4_assert_numargs(1)
m4_assert_defined(`ALIGN_LOGARITHMIC')
`.align	ifelse(ALIGN_LOGARITHMIC,yes,`m4_log2($1)',`eval($1)')dnl
ifelse(ALIGN_FILL_0x90,yes,`, 0x90')')


dnl  Usage: MULFUNC_PROLOGUE(function function...)
dnl
dnl  A dummy macro which is grepped for by ./configure to know what
dnl  functions a multi-function file is providing.  Use this if there aren't
dnl  explicit PROLOGUE()s for each possible function.
dnl
dnl  Multiple MULFUNC_PROLOGUEs can be used, or just one with the function
dnl  names separated by spaces.

define(`MULFUNC_PROLOGUE',
m4_assert_numargs(1)
`')


divert`'dnl
