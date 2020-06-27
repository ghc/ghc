#!/usr/bin/env bash
# vim: sw=2 et
set -euo pipefail

NOTES_ORIGIN="https://gitlab.haskell.org/ghc/ghc-performance-notes.git"
NOTES_ORIGIN_PUSH="git@gitlab.haskell.org:ghc/ghc-performance-notes.git"
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
  run git fetch -f $NOTES_ORIGIN $ref:$ref
  echo "perf notes ref $ref is $(git rev-parse $ref)"
}

function setup_ssh() {
  # Add gitlab as a known host.
  # This can be generated with `ssh-keyscan -H gitlab.haskell.org`
  mkdir -p ~/.ssh
  echo "|1|cta91z3DoAGdpX2Epe9WF+sr+Rk=|1qlsbqiTTa8YsDyQBjVnzANFQ3Y= ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDSzzl8mwY6ohtW6MftKaQfta8yTL8cTxtA7lcueo2mkPpwBBQ7FA6z3nFATx25QwdV7fa7DuNRDX57f/a/W7+wMhXZ6yyQr+gwr0h4vdZ8Nt4XNfNdkdGw4fZKRApWxyvfSkxjs/E9+G0o3eQLspxjVohBkmkcsowpFUI5Aazv/K6QIf1gKt+4iPvYcB/dBJ1yF1qmpayz4htrKyUC5l3GCBEwvMdAjIQ2bX8pyjTtqcJDLosAVzQ5wprkdgkL29MgJXEbM+B1d1log0hnX4AsbOlL7tWhTO1Je2hSuEeiVaDDPFUyCoGQRFDrisQU5lb8NrzuN3jpNc+PxOHbXHfaTppAoED/++UepvgtLF1zUM13cRk56YmpmABOa48W72VJuzLLm8DF+KBWBs6TDuVk3y9z/SS6zDS0VGkHotldopW2kpsjErJIdWVKIL3RP/Flay7mzl3l/izIMTHXXKMxV3/+XaBjG/gDOCld3JjORQXah2hvJfvXeNaePE1RKAMS63cj3XTE77fsYH7VmEdE34RTBDtsZR5WhEjdf29hjEcQDPf0vDphxRHr6IqUSwVcd7ps6nVoccTfaepJm62IIXDgOsc2piWl2xXNZJVtph6U+MzsPDSSbu1MTwalwgqpApcYK7ZzUjGHA7+NBhjjSuUZO6eHzwxjAn0FXZyrpQ==" >> ~/.ssh/known_hosts
  echo "|1|uZkjsBS2bmdh7L/8zBquxJd/F20=|by/tpuDAPT6BpEXrDOiOv1/Zx/A= ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA7ltOZyaULDgxE3Vw6RgQVp+OPKQi79ssUenbhdWy36" >> ~/.ssh/known_hosts

  # Setup ssh keys.
  eval `ssh-agent`
  echo "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJPR1vrZgeGTXmgJw2PsJfMjf22LcDnVVwt3l0rwTZ+8Q2J0bHaYxMRKBco1sON6LGcZepw0Hy76RQ87v057pTz18SXvnfE7U/B6v9qBk0ILJz+4BOX9sEhxu2XmScp/wMxkG9IoyruMlsxXzd1sz09o+rzzx24U2Rp27PRm08vG0oipve6BWLbYEqYrE4/nCufqOJmGd56fju7OTU0lTpEkGDEDWGMxutaX2CbTbDju7qy07Ld8BjSc9aHfvuQaslUbj3ex3EF8EXahURzGpHQn/UFFzVGMokFumiJCAagHQb7cj6jOkKseZLaysbA/mTBQsOzjWiRmkN23bQf1wF ben+ghc-ci@smart-cactus.org" > ~/.ssh/perf_rsa.pub
  touch ~/.ssh/perf_rsa
  chmod 0600 ~/.ssh/perf_rsa
  echo "$PERF_NOTE_KEY" >> ~/.ssh/perf_rsa
  ssh-add ~/.ssh/perf_rsa
}

# Reset the git notes and append the metrics file to the notes, then push and return the result.
# This is favoured over a git notes merge as it avoids potential data loss/duplication from the merge strategy.
function reset_append_note_push {
  pull || true
  run git notes --ref=$REF append -F $METRICS_FILE HEAD
  run git push $NOTES_ORIGIN_PUSH refs/notes/$REF
}

function push() {
  # Check that private key is available (Set on all GitLab protected branches).
  if [ -z ${PERF_NOTE_KEY+"$PERF_NOTE_KEY"} ]
  then
    echo "Not pushing performance git notes: PERF_NOTE_KEY is not set."
    exit 0
  fi

  # TEST_ENV must be set.
  if [ -z ${TEST_ENV+"$TEST_ENV"} ]
  then
    fail "Not pushing performance git notes: TEST_ENV must be set."
  fi

  # Assert that the METRICS_FILE exists and can be read.
  if [ -z ${METRICS_FILE+"$METRICS_FILE"} ]
  then
    fail "\$METRICS_FILE not set."
  fi
  if ! [ -r $METRICS_FILE ]
  then
    fail "Metrics file not found: $METRICS_FILE"
  fi

  setup_ssh

  # Push the metrics file as a git note. This may fail if another task pushes a note first. In that case
  # the latest note is fetched and appended.
  MAX_RETRY=20
  until reset_append_note_push || [ $MAX_RETRY -le 0 ]
  do
    ((MAX_RETRY--))
    echo ""
    echo "Failed to push git notes. Fetching, appending, and retrying... $MAX_RETRY retries left."
  done
}

case $1 in
  push) push ;;
  pull) pull ;;
  *) fail "Invalid mode $1" ;;
esac
