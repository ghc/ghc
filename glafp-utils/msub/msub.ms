.SH
Theory of Operation
.LP
.I msub
reads the makefile in the current directory (or the file named after the
.B \-f
flag) and finds all variable definitions and references.
Definitions are lines of the form ``\fIname\fR = \fIvalue\fR''.
References are instances of ``$(\fIvar\fR)'' or ``${\fIvar\fR}'', either in
the value part of definition lines, or any part of dependency or command lines.
Then variable values are examined to eliminate references to other variables
by replacing the referenced variables with their values.
Variables which are referenced but never defined have a value of the empty
string.
.LP
Then
.I msub
reads any files named on the command line (or
.I stdin
if none) and looks for instances of ``$(\fIvar\fR)'' or ``${\fIvar\fR}'',
where
.I var
is known from the makefile, and substitutes the value of
.I var .
.SH
Purpose
.LP
The motivation for
.I msub
is that project configuration information is often contained in makefiles
and it is often desirable to make use of that information.
.I msub
allows files to be produced from templates that contain references
to makefile variables.
In this sense it is similar to the way that the C preprocessor
.I cpp
uses #define statements in header files; the
makefile is the implicit ``header'' file and definition lines are the
``#define'' statements.
.LP
Although it is possible to use
.I cpp
to perform substitutions on templates, a disadvantage of
.I cpp
is that it modifies its source in other ways (eg., lines beginning with
``#'' are special and comments (``/* ... */'') are deleted.
Also, it is necessary to pass a -D flag for every symbol to be substituted.
Similarly, for C programs it can be simpler to use
.I msub
to produce a single configuration header file from a template
than to pass a lot of -D flags to each invocation of
.I cc .
.LP
.I msub
performs no textual modification besides substitution of references
to makefile variables; it leaves everything else alone.
In particular,
references to variables not known from the makefile are left untouched.
This allows
.I msub
to be used to produce shell scripts that use variables of
their own.
.LP
.I msub
is particularly useful in software projects that use
.I imake .
Virtually all configuration information is propagated into makefiles
thus produced, so it is available for propagation into other files using
.I msub .
.SH
Problems
.LP
If
.I msub
is used to produce shell scripts, it is unwise for the script to use names
for its own variables that are the same as those in the makefile.
The problem can be avoided entirely if, for instance, variables in makefiles
are all uppercase and the scripts' own variables are lowercase or mixed case.
.LP
Circular references in makefile variables are not expanded.
If a makefile contains the definitions ``X = ${Y}'' and ``Y = ${X}'',
neither X nor Y will be expanded.
But that is really a problem with the makefile, not
.I msub .
.LP
.I make
recognizes
.B \-f\ \-
to mean the makefile should be read from
.I stdin .
.I msub
does not.
.LP
CC, YACC, etc., have implicit values in
.I make
even if not defined in the makefile.
Not true in
.I msub .
.LP
.I msub
is more restrictive in the variable names that it recognizes and
performs substitutions on.
Names that do not begin with a letter and
that contain any characters besides letters, digits and underscores are
ignored.
This means that variable names such as ``${1}'' are not recognized;
the reason for this is that shell scripts often use such variables and
they should be left untouched.
Also,
``$\fIx\fR'' is legal in
.I make
when
.I x
is a single character, but
.I msub
ignores such references.
.i msub
.LP
.I make
allows variables to be defined on the command line.
.I msub
does not.
.LP
If you try to be tricky with your variable references, e.g., by using
recursive references, you may get burned.
.LP
There is no way to prevent substitution, in a source file, of a reference to
a variable known from the makefile.
