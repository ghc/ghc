#!/usr/bin/env bash
# vim: sw=2 et
set -euo pipefail




# TODO CHANGE ME BEFORE MERGE
GHC_ORIGIN="git@gitlab.haskell.org:DavidEichmann/ghc-perf-notes-test.git"
REF="perf_tmp"





fail() {
  echo "ERROR: $*" >&2
  exit 1
}

# Check that private key is available (Set on all GitLab protected branches).
if [ "$PERF_NOTE_KEY" = "" ]; then
  echo "Not pushing performance git notes: PERF_NOTE_KEY is not set."
  exit 0
fi

# TEST_ENV must be set.
if [ "$TEST_ENV" = "" ]; then
  fail "Not pushing performance git notes: TEST_ENV must be set."
fi

# Setup ssh keys.
eval `ssh-agent`
mkdir -p ~/.ssh
echo "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDoMpqMpHea4BphxjEn7khvW4S64FaWqnSTSAHXTX0CWUS3d0WhvT0GZRjiZ8ULJUCbKXHLhjNYo2b9do2Cd+pCeeVc2GPxajLxyJT5bvSouLhFV5LpMqeZz49A90Xbeux61RuVlxCRDGHy6lCw8c8BmWLJcLXdSflM1YbHcDZyg5LGAdBrG4dmDlfWqDAC381T3mFSTy9O60qNAMHjVb2CwmekhkKeN5q53sHV0kb5KryDTSHDprd8tcSMHKbcXsyFbtc2JSy8vYeBRqfCXluIJGMv1YvT0+bdgGF+aIrnqMmqWYnvoNfQxc98c0tDry8pfTNS4SzTeEzNaq0hJcHf david@david-OctaneV" > ~/.ssh/perf_rsa.pub
touch ~/.ssh/perf_rsa
chmod 0600 ~/.ssh/perf_rsa
echo $PERF_NOTE_KEY >> ~/.ssh/perf_rsa
ssh-add ~/.ssh/perf_rsa

# Check that git notes don't already exist.
# This is a percausion as we reset refs/notes/perf and we want to avoid data loss.



git notes --ref=$REF append -m "test note"




if [ $(git notes --ref=$REF list | wc -l) -ne 0 ]
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
  git fetch -f $GHC_ORIGIN refs/notes/$REF:refs/notes/$REF || true
  echo "git notes --ref=$REF append -F $METRICS_FILE HEAD"
  git notes --ref=$REF append -F $METRICS_FILE HEAD
  git push $GHC_ORIGIN refs/notes/$REF
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
