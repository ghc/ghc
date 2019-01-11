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

# Add gitlab as a known host.
echo "|1|+AUrMGS1elvPeLNt+NHGa5+c6pU=|4XvfRsQftO1OgZD4c0JJ7oNaii8= ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDXilA5l4kOZPx0nM6xDATF+t4fS6te0eYPDwBI/jLWD9cJVtCnsrwMl5ar+/NfmcD0jnCYztUiVHuXyTaWPJYSQpwltfpTeqpo9/z/0MxkPtSl1uMP2cLbDiqA01OWveChktOXwU6hRQ+7MmO+dNRS/iXrRmYrGv/p1W811QgLBLS9fefEdF25n+0dP71L7Ov7riOawlDmd0C11FraE/R8HX6gs6lbXta1kisdxGyKojYSiCtobUaJxRoatMfUP0a9rwTAyl8tf56LgB+igjMky879VAbL7eQ/AmfHYPrSGJ/YlWP6Jj23Dnos5nOVlWL/rVTs9Y/NakLpPwMs75KTC0Pd74hdf2e3folDdAi2kLrQgO2SI6so7rOYZ+mFkCM751QdDVy4DzjmDvSgSIVf9SV7RQf7e7unE7pSZ/ILupZqz9KhR1MOwVO+ePa5qJMNSdC204PIsRWkIO5KP0QLl507NI9Ri84+aODoHD7gDIWNhU08J2P8/E6r0wcC8uWaxh+HaOjI9BkHjqRYsrgfn54BAuO9kw1cDvyi3c8n7VFlNtvQP15lANwim3gr9upV+r95KEPJCgZMYWJBDPIVtp4GdYxCfXxWj5oMXbA5pf0tNixwNJjAsY7I6RN2htHbuySH36JybOZk+gCj6mQkxpCT/tKaUn14hBJWLq7Q+Q==" >> ~/.ssh/known_hosts
echo "|1|JZkdAPJmpX6SzGeqhmQLfMWLGQA=|4vTELroOlbFxbCr0WX+PK9EcpD0= ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJknufU+I6A5Nm58lmse4/o11Ai2UzYbYe7782J1+kRk" >> ~/.ssh/known_hosts

# Setup ssh keys.
eval `ssh-agent`
mkdir -p ~/.ssh
echo "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDoMpqMpHea4BphxjEn7khvW4S64FaWqnSTSAHXTX0CWUS3d0WhvT0GZRjiZ8ULJUCbKXHLhjNYo2b9do2Cd+pCeeVc2GPxajLxyJT5bvSouLhFV5LpMqeZz49A90Xbeux61RuVlxCRDGHy6lCw8c8BmWLJcLXdSflM1YbHcDZyg5LGAdBrG4dmDlfWqDAC381T3mFSTy9O60qNAMHjVb2CwmekhkKeN5q53sHV0kb5KryDTSHDprd8tcSMHKbcXsyFbtc2JSy8vYeBRqfCXluIJGMv1YvT0+bdgGF+aIrnqMmqWYnvoNfQxc98c0tDry8pfTNS4SzTeEzNaq0hJcHf david@david-OctaneV" > ~/.ssh/perf_rsa.pub
touch ~/.ssh/perf_rsa
chmod 0600 ~/.ssh/perf_rsa
echo "$PERF_NOTE_KEY" >> ~/.ssh/perf_rsa
ssh-add ~/.ssh/perf_rsa

# Check that git notes don't already exist.
# This is a percausion as we reset refs/notes/perf and we want to avoid data loss.



git notes --ref=$REF append -m "test note"




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
