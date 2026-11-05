HELP_TEXT = """filecheck - A Python-native clone of LLVMs FileCheck tool
usage: filecheck FLAGS check-file

FLAGS:
--input-file <file>             : Specify an input file, default is stdin (-)
--check-prefix <prefix>         : Check line prefix (default is CHECK)
--strict-whitespace             : Don't ignore indentation
--comment-prefixes <a>,<b>,<c>  : Ignore lines starting with these prefixes, even if
                                  they contain check lines (default RUN,COM)
--enable-var-scope              : Enables scope for regex variables. Variables not
                                  starting with $ are deleted after each CHECK-LABEL
                                  match.
--match-full-lines              : Expect every check line to match the whole line.
--reject-empty-vars             : Raise an error when a value captures an empty string.
--dump-input                    : Dump the input to stderr annotated with helpful
                                  information depending on the context. Allowed values
                                  are help, always, never, fail. Default is fail.
                                  Only fail and never is currently supported in this
                                  version of filecheck.

ARGUMENTS:
check-file                      : The file from which the check lines are to be read

This tries to be as close a clone of LLVMs FileCheck as possible. We use it in xDSL
for our tests. If a feature is missing, or behaviour deviates from LLVMs FileCheck,
please file a bug at github.com/AntonLydike/filecheck.
"""
