# cabal command line completion
# Copyright 2007-2008 "Lennart Kolmodin" <kolmodin@gentoo.org>
#                     "Duncan Coutts"     <dcoutts@gentoo.org>
#

# List cabal targets by type, pass:
#   - test-suite for test suites
#   - benchmark for benchmarks
#   - executable for executables
#   - executable|test-suite|benchmark for the three
_cabal_list()
{
    for f in ./*.cabal; do
        grep -Ei "^[[:space:]]*($1)[[:space:]]" "$f" |
        sed -e "s/.* \([^ ]*\).*/\1/"
    done
}

# List possible targets depending on the command supplied as parameter.  The
# ideal option would be to implement this via --list-options on cabal directly.
# This is a temporary workaround.
_cabal_targets()
{
    # If command ($*) contains build, repl, test or bench completes with
    # targets of according type.
    local comp
    for comp in "$@"; do
        [ "$comp" == new-build ] && _cabal_list "executable|test-suite|benchmark" && break
        [ "$comp" == build     ] && _cabal_list "executable|test-suite|benchmark" && break
        [ "$comp" == repl      ] && _cabal_list "executable|test-suite|benchmark" && break
        [ "$comp" == run       ] && _cabal_list "executable"                      && break
        [ "$comp" == test      ] && _cabal_list            "test-suite"           && break
        [ "$comp" == bench     ] && _cabal_list                       "benchmark" && break
    done
}

# List possible subcommands of a cabal subcommand.
#
# In example "sandbox" is a cabal subcommand that itself has subcommands. Since
# "cabal --list-options" doesn't work in such cases we have to get the list
# using other means.
_cabal_subcommands()
{
    local word
    for word in "$@"; do
        case "$word" in
          sandbox)
            # Get list of "cabal sandbox" subcommands from its help message.
            "$1" help sandbox |
            sed -n '1,/^Subcommands:$/d;/^Flags for sandbox:$/,$d;/^ /d;s/^\(.*\):/\1/p'
            break  # Terminate for loop.
            ;;
        esac
    done
}

__cabal_has_doubledash ()
{
    local c=1
    # Ignore the last word, because it is replaced anyways.
    # This allows expansion for flags on "cabal foo --",
    # but does not try to complete after "cabal foo -- ".
    local n=$((${#COMP_WORDS[@]} - 1))
    while [ $c -lt $n ]; do
        if [ "--" = "${COMP_WORDS[c]}" ]; then
            return 0
        fi
        ((c++))
    done
    return 1
}

_cabal()
{
    # no completion past cabal arguments.
    __cabal_has_doubledash && return

    # get the word currently being completed
    local cur
    cur=${COMP_WORDS[$COMP_CWORD]}

    # create a command line to run
    local cmd
    # copy all words the user has entered
    cmd=( ${COMP_WORDS[@]} )

    # replace the current word with --list-options
    cmd[${COMP_CWORD}]="--list-options"

    # the resulting completions should be put into this array
    COMPREPLY=( $( compgen -W "$( eval "${cmd[@]}" 2>/dev/null ) $( _cabal_targets "${cmd[@]}" ) $( _cabal_subcommands "${COMP_WORDS[@]}" )" -- "$cur" ) )
}

complete -F _cabal -o default cabal
