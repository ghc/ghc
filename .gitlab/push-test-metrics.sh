#!/usr/bin/env bash
# vim: sw=2 et
set -euo pipefail

fail() {
  echo "ERROR: $*" >&2
  exit 1
}

GHC_ORIGIN="git@gitlab.haskell.org:ghc/ghc.git"

# Only push git notes when on the official gitlab ghc repo.
if [ "$CI_REPOSITORY_URL" != "$GHC_ORIGIN" ]; then
  echo "Not pushing performance git notes: expected repo $GHC_ORIGIN but on repo $CI_REPOSITORY_URL"
  exit 0
fi

# Branch must be master
if [ "$CI_COMMIT_REF_NAME" != "master" ]; then
  echo "Not pushing performance git notes: not on master branch (on branch/tag $CI_COMMIT_REF_NAME)."
  exit 0
fi

# TEST_ENV must be set.
if [ "$TEST_ENV" = "" ]; then
  fail "Not pushing performance git notes: TEST_ENV must be set."
fi

# Check that git notes don't already exist.
# This is a percausion as we reset refs/notes/perf and we want to avoid data loss.
if [ $(git notes --ref=perf list | wc -l) -ne 0 ]
then
  fail "Found an existing git note. Expected no git note."
fi

# Assert that the METRICS_FILE exists and can be read.
if [ "$METRICS_FILE" = "" ] || ! [ -r $METRICS_FILE ]
then
  fail "Metrics file not found: $METRICS_FILE"
fi

# Reset the git notes and append the metrics file to the notes, then push and return the result.
# This is favoured over a git notes merge as it avoids potential data loss/duplication from the merge strategy.
function reset_append_note_push {
  git fetch -f $GHC_ORIGIN refs/notes/perf:refs/notes/perf || true
  echo "git notes --ref=perf append -F $METRICS_FILE HEAD"
  git notes --ref=perf append -F $METRICS_FILE HEAD
  git push $GHC_ORIGIN refs/notes/perf
}

# Push the metrics file as a git note. This may fail if another task pushes a note first. In that case
# the latest note is fetched and appended.
MAX_RETRY=20
until reset_append_note_push || [ $MAX_RETRY -le 0 ]
do
  ((MAX_RETRY--))
  echo ""
  echo "Failed to push git notes. Fetching, appending, and retrying... $MAX_RETRY retries left."
done
