#!/usr/bin/env bash
set -e

# Start a head.hackage job and wait for completion. Expected to be called from
# GitLab CI.

if [ -z "$HEAD_HACKAGE_TRIGGER_TOKEN" ]; then
  echo "Error: Expected head.hackage trigger token in HEAD_HACKAGE_TRIGGER_TOKEN"
  exit 1
fi

if [ -z "$CI_PIPELINE_ID" ]; then
  echo "Error: Expected pipeline id in CI_PIPELINE_ID"
  exit 1
fi

if [ -z "$HEAD_HACKAGE_PROJECT_ID" ]; then
  HEAD_HACKAGE_PROJECT_ID="78"
fi

# Start pipeline
curl --silent --show-error \
  --request POST \
  -F "token=$HEAD_HACKAGE_TRIGGER_TOKEN" \
  -F "ref=master" \
  -F "variables[GHC_PIPELINE_ID]=$CI_PIPELINE_ID" \
  -F "variables[EXTRA_HC_OPTS]=-dcore-lint" \
  https://gitlab.haskell.org/api/v4/projects/$HEAD_HACKAGE_PROJECT_ID/trigger/pipeline \
  | tee resp.json

echo
pipeline_id=$(jq .id < resp.json)
url=$(jq .web_url < resp.json)
echo
echo "Started head.hackage pipeline $pipeline_id: $url"

# Wait for completion
running=
echo "Waiting for build to complete..."
while true; do
  sleep 10
  curl --silent --show-error \
    --request GET \
    -F "job_token=$CI_JOB_TOKEN" \
    https://gitlab.haskell.org/api/v4/projects/$HEAD_HACKAGE_PROJECT_ID/pipelines/$pipeline_id \
    > resp.json
  status=$(jq .status < resp.json)

  case $status in
  "\"pending\"")
    ;;
  "\"running\"")
    if [ -z "$running" ]; then
      echo "Pipeline $pipeline_id is now running."
      running=1
    fi
    ;;
  "\"success\"")
    echo "Pipeline $pipeline_id finished successfully."
    exit 0
    ;;
  "\"failed\"")
    echo "Pipeline $pipeline_id failed."
    exit 1
    ;;
  "\"canceled\"")
    echo "Pipeline $pipeline_id was canceled."
    exit 1
    ;;
  "\"skipped\"")
    echo "Pipeline $pipeline_id was skipped."
    exit 1
    ;;
  *)
    cat resp.json
    echo "Error: Unknown pipeline status $status"
    exit 2
    ;;
  esac
done
