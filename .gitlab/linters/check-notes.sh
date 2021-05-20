# set -e XXX JB

COMMENT_START='-- {- # // /* dnl'
COMMENT_END='-} # // /*'
ROOT_DIR='.'

# convert comment delimiters into regexes
commentStart=$(printf '%s' "$COMMENT_START" | \
  sed 's/\-/\\\-/g' | sed 's/\s\+/\\|/g' | sed 's/^/\\(/' | sed 's/$/\\)/')
commentEnd=$(printf '%s' "$COMMENT_END" | \
  sed 's/\-/\\\-/g' | sed 's/\s\+/\\|/g' | sed 's/^/\\(/' | sed 's/$/\\)/')

# find lines containing Notes
lines=$(grep -rn 'Note\s*\[[^]]*' "$ROOT_DIR" -I)

# Note names that span multiple lines or aren't closed (we want 0 of these)
unterminated=$(printf '%s' "$lines" | grep 'Note\s*\[[^]]*$')

# Note names (including surrounding square brackets)
notes=$(printf '%s' "$lines" | sed 's/\[/\n[/g' | sed 's/\]/]\n/g' | \
  grep '\[[^]]*\]' | sort | uniq)

# note header (Note [<note-name>] must be the only thing in its line, aside
# from possibly a comment delimiter)
# We use [^:]*:[^:]*: here instead of ^ to indicate the beginning of a line since
# the previous grep added the file name and line number
headers=$(printf '%s' "$lines" | \
  grep '[^:]*:[^:]*:\s*'"$commentStart"'\?\s*Note\s*\[[^]]*\]\s*'"$commentEnd"'\s*$')

# Notes should be the same as headerNames
headerNames=$(printf '%s' "$headers" | \
  sed 's&[^:]*:[^:]*:\s*'"$commentStart"'\?\s*Note\s*\(.*\)&\2&' | sort | uniq)

# But for reporting we actually need to have all the line numbers and such in
# the references, so... I think `notes` is largely useless.

# for each line that contains a note, we want to extract the note, and check
# whether it exists in headerNames
for line in $lines
do
  grep 
done

# Make sure that all the references point to notes that exist
