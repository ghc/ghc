# set -e XXX JB

COMMENT_START='-- {- # // /* dnl'
COMMENT_END='-} # // /*'
ROOT_DIR='.'

# convert into regex
commentStart=$(printf '%s' "$COMMENT_START" | sed 's/\s\+/\\|/g' | sed 's/^/\\(/' | sed 's/$/\\)/')
commentEnd=$(printf '%s' "$COMMENT_END" | sed 's/\s\+/\\|/g' | sed 's/^/\\(/' | sed 's/$/\\)/')

# find lines containing Notes
lines=$(grep -rn 'Note\s*\[[^]]*' "$ROOT_DIR" -I)

# Note names that span multiple lines or aren't closed (we want 0 of these)
unterminated=$(printf '%s' "$lines" | grep 'Note\s*\[[^]]*$')

# Note names (including surrounding square brackets)
notes=$(printf '%s' "$lines" | sed 's/\[/\n[/g' | sed 's/\]/]\n/g' | grep '\[[^]]*\]' | sort | uniq -u)

# note header (Note [<note-name>] must be the only thing in its line, aside
# from possibly a comment delimiter
# We use .*:.*: here instead of ^ to indicate the beginning of a line since
# the previous grep added the file name and line number
headers=$(printf '%s' "$lines" | grep '.*:.*:\s*'"$commentStart"'\?\s*Note\s*\[[^]]*\]\s*'"$commentEnd"'\s*$')
