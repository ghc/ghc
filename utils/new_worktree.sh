#!/usr/bin/env bash

# This create must be run at the top-level of GHC source tree. E.g.
#
#   ./utils/new_worktree.sh myfeature wip/me/myfeature
#
# It creates a new worktree (in ../myfeature) for a new branch
# ("wip/me/myfeature").
#
# The main advantage over `git worktree add ...` is that this script also copies
# submodules from the current worktree so that you don't have to download them
# again in the new worktree.

set -e

if [ -z $1 ]; then
    echo "Worktree name required."
    echo "Usage: $0 WORKTREE_NAME NEW_BRANCH_NAME"
    exit
fi

if [ -z $2 ]; then
    echo "Branch name required."
    echo "Usage: $0 WORKTREE_NAME NEW_BRANCH_NAME"
    exit
fi


NAME=$1
BRANCH=$2

WORKTREE="../$NAME"

if [ -d "$WORKTREE" ]; then
   echo "fatal: $WORKTREE already exists."
   exit
fi

echo Create a worktree in $WORKTREE for new branch \"$BRANCH\"

git worktree add $WORKTREE -b $BRANCH

git submodule foreach "
    PIN=\$toplevel/\$displaypath;
    POUT=\$toplevel/$WORKTREE/\$displaypath;
    #echo Copying \$PIN into \$POUT ;
    mkdir -p \$POUT ;
    cp -r \$PIN/. \$POUT ;
"

echo New worktree created in $WORKTREE
