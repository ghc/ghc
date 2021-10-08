#!/bin/bash

# Script to refresh the bundled NPM packages

STATUS=`git status --porcelain`
if [ ${#STATUS} -gt 0 ]
then
  echo "working tree is dirty, run from a clean working tree"
  exit 1
fi

#####################################
# 1. fetch all NPM code
#####################################

npm install

#####################################
# 2. add commit to the `dist` branch
#####################################

# add the new code to the index

CURRENT_TREE_HEAD=`git rev-parse HEAD`
DIST_TREE_HEAD=`git show-ref -s dist`
CURRENT_TREE_MESSAGE=`git show --format="%B" -s ${CURRENT_TREE_HEAD}`
COMMIT_MESSAGE="update to ${CURRENT_TREE_HEAD}: ${CURRENT_TREE_MESSAGE}"

# save our updated code in a tree object

git add node_modules
GIT_TREE_OBJECT=`git write-tree`

echo "tree object: ${GIT_TREE_OBJECT}"
echo "dist tree HEAD: ${DIST_TREE_HEAD}"
echo "current tree HEAD: ${CURRENT_TREE_HEAD}"
echo "commit message: ${COMMIT_MESSAGE}"

# create merge commit and update the HEAD of the `dist` branch to point to it

# there's probably a better way to do the commit message...
echo "${COMMIT_MESSAGE}" > commit-message.tmp.txt
MERGE_COMMIT=`git commit-tree -p ${DIST_TREE_HEAD} -p ${CURRENT_TREE_HEAD} -F commit-message.tmp.txt ${GIT_TREE_OBJECT}`
rm commit-message.tmp.txt

# echo "merge commit: ${MERGE_COMMIT}"

git branch -f dist ${MERGE_COMMIT}

# restore our working dir

rm -r node_modules
git reset

# print new commit id
echo "changes added to dist branch, commit id: ${MERGE_COMMIT}"
 
