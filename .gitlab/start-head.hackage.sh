#!/usr/bin/env bash
set -e

# Start a head.hackage job and wait for completion.

if [ -z "$HEAD_HACKAGE_TRIGGER_TOKEN" ]; then
  echo "Error: Expected head.hackage trigger token in HEAD_HACKAGE_TRIGGER_TOKEN"
  exit 1
fi

if [ -z "$CI_PIPELINE_ID" ]; then
  echo "Error: Expected pipeline id in CI_PIPELINE_ID"
  exit 1
fi

if [ -z "$HEAD_HACKAGE_PROJECT_ID" ]; then
  HEAD_HACKAGE_PROJECT_ID="ghc%2Fhead.hackage"
fi

echo curl --silent --show-error \
  --request POST \
  -F "token=$HEAD_HACKAGE_TRIGGER_TOKEN" \
  -F "ref=gitlab-ci-nix" \
  -F "variables[GHC_PIPELINE_ID]=$CI_PIPELINE_ID" \
  https://gitlab.haskell.org/api/v4/projects/$HEAD_HACKAGE_PROJECT_ID/trigger/pipeline

curl --silent --show-error \
  --request POST \
  -F "token=$HEAD_HACKAGE_TRIGGER_TOKEN" \
  -F "ref=gitlab-ci-nix" \
  -F "variables[GHC_PIPELINE_ID]=$CI_PIPELINE_ID" \
  https://gitlab.haskell.org/api/v4/projects/$HEAD_HACKAGE_PROJECT_ID/trigger/pipeline \
  | tee resp.json

pipeline_id=$(jq .id < resp.json)
url=$(jq .web_url < resp.json)
echo "Started head.hackage pipeline $pipeline_id: $url"

running=
echo "Waiting for build to complete..."
while true; do
  sleep 10
  curl --silent --show-error \
    --request GET \
    -F "token=$HEAD_HACKAGE_TRIGGER_TOKEN" \
    https://gitlab.haskell.org/api/v4/projects/$HEAD_HACKAGE_PROJECT_ID/pipelines/$pipeline_id \
    | tee resp.json
  status=$(jq .status < resp.json)

  case $status in
  pending)
    ;;
  running)
    if [ -z "$running" ]; then
      echo "Pipeline $pipeline_id is running."
      running=1
    fi
    ;;
  success)
    echo "Pipeline $pipeline_id finished successfully."
    exit 0
    ;;
  failed)
    echo "Pipeline $pipeline_id failed."
    exit 1
    ;;
  canceled)
    echo "Pipeline $pipeline_id was canceled."
    exit 1
    ;;
  skipped)
    echo "Pipeline $pipeline_id was skipped."
    exit 1
    ;;
  *)
    echo "Error: Unknown pipeline status $status"
    exit 2
    ;;
  esac
done
