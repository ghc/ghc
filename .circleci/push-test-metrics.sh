#!/usr/bin/env bash
# vim: sw=2 et
set -euo pipefail

fail() {
  echo "ERROR: $*" >&2
  exit 1
}

if [ "$CIRCLE_REPOSITORY_URL" != "git@github.com:ghc/ghc.git" ]; then
  exit 0
fi

GHC_ORIGIN=git@git.haskell.org:ghc

# Add git.haskell.org as a known host.
echo "|1|F3mPVCE55+KfApNIMYQ3Dv39sGE=|1bRkvJEJhAN2R0LE/lAjFCEJGl0= ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBBUZS9jGBkE5UzpSo6irnIgcQcfzvbuIOsFc8+N61FwtZncRntbaKPuUimOFPgeaUZLl6Iajz6IIs7aduU0/v+I=" >> ~/.ssh/known_hosts
echo "|1|2VUMjYSRVpT2qJPA0rA9ap9xILY=|5OThkI4ED9V0J+Es7D5FOD55Klk= ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC+3TLluLAO4lkW60W+N2DFkS+WoRFGqLwHzgd1ifxG9TIm31wChPY3E/hgMnJmgGqWCF4UDUemmyCycEaL7FtKfzjTAclg9EfpQnozyE3T5hIo2WL7SN5O8ttG/bYGuDnn14jLnWwJyN4oz/znWFiDG9e2Oc9YFNlQ+PK8ae5xR4gqBB7EOoj9J1EiPqG2OXRr5Mei3TLsRDU6fnz/e4oFJpKWWeN6M63oePv0qoaGjxcrATZUWsuWrxVMmYo9kP1xRuFJbAUw2m4uVP+793SW1zxySi1HBMtJG+gCDdZZSwYbkV1hassLWBHv1qPttncfX8Zek3Z3VolaTmfWJTo9" >> ~/.ssh/known_hosts

# Check that a git notes dont already exist.
# This is a percausion as we reset refs/notes/perf and we want to avoid data loss.
if [ $(git notes --ref=perf list | wc -l) -ne 0 ]
then
  fail "Found an existing git note on HEAD. Expected no git note."
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
