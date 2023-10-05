# ========================================
# Glasgow Haskell Compiler Bash Completion
# ========================================
#
# For how to use the GHC bash completion, see the README.
#
# This file implements bash completion for both GHC and GHCi.
#
#  - We use GHC's --show-options to get a list of the available
#    flags. It is aware that some flags are used for GHC, and others for GHCi.
#  - We understand when the argument;
#     * has to be a directory name (eg. following -hidir)
#     * cannot be completed (eg. following -e)
#
# Future work;
#  - Some flags needs their argument after an equal sign;
#      eg. -fmax-simplifier-iterations=N
#      Currently the flag will be completed without knowledge of
#      the required argument.
#  - Complete package names/ids.
#      eg. -package-id <TAB><TAB> should list valid package-ids
#  - The +RTS flags are not supported.
#      +RTS <TAB><TAB> should list valid RTS flags.

_ghc ()
{
  local completions=`$1 --show-options`
  local cur="${COMP_WORDS[COMP_CWORD]}"
  local prev="${COMP_WORDS[COMP_CWORD-1]}"

  # Complete the current flag based on the previous flag.
  case "$prev" in
    -hidir|-odir|-stubdir|-dumpdir|-outputdir|-tmpdir|-hpcdir|-dylib-install-name|-framework-path)
      # Complete only with directory names.
      compopt -o dirnames
      return 0
      ;;
    -package-name|-package|-hide-package|-ignore-package|-trust|-distrust)
      # Should complete package names. Not implemented.
      # To do this well, ghc has to be invoked with --show-packages with all
      # package related flags the user has provided.
      return 0
      ;;
    -e|-x|-hcsuf|-hisuf|-osuf|-framework)
      # Do nothing. Next argument is not a flag.
      return 0
      ;;
  esac

  # Look at the current flag.
  if [[ "$cur" == -* ]]; then
    # All GHC flags start with a dash, so we want to see this before we start
    # suggesting flags. Otherwise we would complete flags when the user might
    # want to type a file name.
    COMPREPLY=( $( compgen -W "$completions -x" -- "$cur" ) )
  fi
}

complete -F _ghc -o default ghc
complete -F _ghc -o default ghci
