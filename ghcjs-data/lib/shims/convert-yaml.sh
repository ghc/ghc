#!/usr/bin/env bash

# convert old-style yaml shims package descriptions to json
# uses yaml2json from yaml Haskell package

for FILE in *.yaml; do
  echo "converting $FILE"
  yaml2json "$FILE" > "${FILE%.yaml}.json"
done

