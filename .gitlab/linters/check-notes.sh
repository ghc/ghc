#!/usr/bin/env bash
# Get all lines in any non-binary files that look like they contain notes and
# the lines that immediately follow them
# Then send them as input to the haskell script
# XXX JB exclude _build
# XXX JB exclude .hadrian_ghci
# XXX JB exclude .hie-bios
grep -rnI -A 1 'Note' $TOP | $GHC -ignore-dot-ghci "check-notes.hs" -e ":main"

# XXX JB EVERYTHING BELOW THIS LINE IS OBSOLETE

############################################################

# set -e

# COMMENT_START='-- {- # // /* dnl'
# COMMENT_END='-} */'
# ROOT_DIR='.'
# COLOR_RED="\e[31m"
# COLOR_NONE="\e[0m"

# # Only allow newlines as separator for loop, not spaces
# IFS=$'\n'

# # convert comment delimiters into regexes
# commentStart=$(printf '%s' "$COMMENT_START" | \
#   sed 's/\-/\\\-/g' | sed 's/\s\+/\\|/g' | sed 's/^/\\(/' | sed 's/$/\\)/')
# commentEnd=$(printf '%s' "$COMMENT_END" | \
# sed 's/\-/\\\-/g' | sed 's/\s\+/\\|/g' | sed 's/^/\\(/' | sed 's/$/\\)/')

# # find lines containing Notes
# lines=$(grep -rnI 'Note\s*\[[^]]*' "$ROOT_DIR")

# # Note names that span multiple lines or aren't closed (we want 0 of these)
# # XXX JB TODO
# unterminated=$(printf '%s' "$lines" | grep 'Note\s*\[[^]]*$')

# # note header (Note [<note-name>] must be the only thing in its line, aside
# # from possibly a comment delimiter)
# # We use [^:]*:[^:]*: here instead of ^ to indicate the beginning of a line since
# # the previous grep prepended the file name and line number
# headerLines=$(printf '%s' "$lines" | \
#   grep '[^:]*:[^:]*:\s*'"$commentStart"'\?\s*Note\s*\[[^]]*\]\s*'"$commentEnd"'\?\s*$')

# # Isolate just the Note names from the header lines
# noteHeaders=$(printf '%s' "$headerLines" | \
#   sed 's&[^:]*:[^:]*:\s*'"$commentStart"'\?\s*Note\s*\(.*\)&\2&')

# echo -e "beforecheck"

# # for each line that contains a note, we want to extract the note, and check
# # whether it exists in headerNames
# isSuccess=true
# for line in $lines
# do
#   # Split up notes references - there could be more than one on the same line -
#   # and isolate the note name in brackets, as well as escaping dashes
#   noteRefs=$(printf '%s' "$line" | sed 's/\[/\n[/g' | sed 's/\]/]\n/g' | \
#     grep '\[[^]]*\]' | sed 's/\-/\\\-/g')
#   for noteRef in $noteRefs
#   do
#     echo "Checking $noteRef"
#     # Check whether a header for the note reference exists
#     isSuccess=$(printf '%s' "$noteHeader" | grep -q "$noteRef" \
#     || (echo error -e "${COLOR_RED}error:${COLOR_NONE}" >&2 \
#         && $false) \
#     && $isSuccess)
#   done
# done

# echo -e aftercheck

# if ! $isSuccess
# then
#   echo -e 'Please make sure all refs have headers' >&2 # XXX JB better hint
# fi

# exit isSuccess // XXX JB

# # XXX JB for MR
# # Alternatives
# # - check whether there are duplicate note headers and tell people to reformat
# # the corresponding note ref or rename one note if there really are two notes
# # with the same name (duplicate note headers are bad for people who
# # want to automatically navigate to note headers)
# # - or even enforce that noterefs always have `see` before `Note` in the same
# # line - this would give a stronger guarantee that there are no dangling notes
# # - check multiline refs/headers - could take the ~~~~~~~ into account to
# # differentiate headers from refs
# # - Perhaps the script is complex enough that a different programming language
# # is preferable.
