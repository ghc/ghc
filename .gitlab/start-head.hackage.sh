#!/usr/bin/env bash
set -e

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

running=0
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
    if [ "$running" = "0" ]; then
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
