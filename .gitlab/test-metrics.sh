#!/usr/bin/env bash
# vim: sw=2 et
set -euo pipefail

NOTES_ORIGIN="https://gitlab.haskell.org/ghc/ghc-performance-notes.git"
REF="perf"

run() {
  echo "$@"
  $@
}

fail() {
  echo "ERROR: $*" >&2
  exit 1
}

function pull() {
  local ref="refs/notes/$REF"
  # 2023-10-04: `git fetch` started failing, first on Darwin in CI and then on
  # Linux locally, both using git version 2.40.1. See #24055. One workaround is
  # to set a larger http.postBuffer, although this is definitely a workaround.
  # The default should work just fine. The error could be in git, GitLab, or
  # perhaps the networking tube (including all proxies etc) between the two.
  run git -c http.postBuffer=2097152 fetch --filter=blob:none -f "$NOTES_ORIGIN" "$ref:$ref"
  echo "perf notes ref $ref is $(git rev-parse $ref)"
}

# Reset the git notes and append the metrics file to the notes, then push and return the result.
# This is favoured over a git notes merge as it avoids potential data loss/duplication from the merge strategy.
function reset_append_note_push {
  pull || true
  run git notes --ref="$REF" append -F "$METRICS_FILE" HEAD
  run git push "$PERF_NOTES_PUSH_REPO" "refs/notes/$REF"
}

function push() {
  # PERF_NOTES_PUSH_CREDENTIALS is a CI variable set on all GitLab protected branches.
  if [ -z "${PERF_NOTES_PUSH_REPO:-}" ]; then
    if [ -n "${PERF_NOTES_PUSH_CREDENTIALS:-}" ]; then
      PERF_NOTES_PUSH_REPO="https://$PERF_NOTES_PUSH_CREDENTIALS@gitlab.haskell.org/ghc/ghc-performance-notes.git"
    else
      echo "Not pushing performance git notes: PERF_NOTES_PUSH_REPO or PERF_NOTES_PUSH_CREDENTIALS not set."
      exit 0
    fi
  fi

  # TEST_ENV must be set.
  if [ -z "${TEST_ENV:-}" ]
  then
    fail "Not pushing performance git notes: TEST_ENV must be set."
  fi

  # Assert that the METRICS_FILE exists and can be read.
  if [ -z "${METRICS_FILE:-}" ]
  then
    fail "\$METRICS_FILE not set."
  fi
  if ! [ -r "$METRICS_FILE" ]
  then
    fail "Metrics file not found: $METRICS_FILE"
  fi

  # Push the metrics file as a git note. This may fail if another task pushes a note first. In that case
  # the latest note is fetched and appended.
  MAX_RETRY=20
  until reset_append_note_push || [ $MAX_RETRY -le 0 ]
  do
    ((MAX_RETRY--))
    echo ""
    echo "Failed to push git notes. Fetching, appending, and retrying... $MAX_RETRY retries left."
  done

  if [ "$MAX_RETRY" -le 0 ]; then
    fail "Failed to push git notes, and no more retries remain."
  fi
}

case $1 in
  push) push ;;
  pull) pull ;;
  *) fail "Invalid mode $1" ;;
esac
