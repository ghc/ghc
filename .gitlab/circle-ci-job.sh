# Circle CI "backend" for Gitlab CI
# =================================
#
# Usage example:
#   .gitlab/circle-ci-job.sh validate-x86_64-linux
#
# There are two things to configure to get artifacts to be
# uploaded to gitlab properly:
#
# - At https://<gitlab host>/admin/application_settings, expand the
#   Continuous Integration and Deployment section and set the
#   "Maximum artifacts size (MB)" field to something large enough
#   to contain the bindists (the test reports are tiny in comparison).
#   500MB seems to work fine, but 200MB might be sufficient.
#
# - If gitlab is exposed behind some form of proxy (e.g nginx), make sure
#   the maximum client request body size is large enough to contain all the
#   artifacts of a build. For nginx, this would be the following configuration
#   option: https://nginx.org/en/docs/http/ngx_http_core_module.html#client_max_body_size
#   (which can be set with services.nginx.clientMaxBodySize on nixos).

#!/usr/bin/env sh

set -e

GHCCI_URL="localhost:8888"

[ $# -gt 0 ] || (echo You need to pass the Circle CI job type as argument to this script; exit 1)
[ ${CI_RUNNER_ID:-} ] || (echo "CI_RUNNER_ID is not set"; exit 1)
[ ${CI_JOB_ID:-} ] || (echo "CI_JOB_ID is not set"; exit 1)
[ ${CI_COMMIT_SHA:-} ] || (echo "CI_COMMIT_SHA is not set"; exit 1)
[ ${CI_REPOSITORY_URL:-} ] || (echo "CI_REPOSITORY_URL is not set"; exit 1)
[ ${CI_PIPELINE_ID:-} ] || (echo "CI_PIPELINE_ID is not set"; exit 1)
# the first argument to this script is the Circle CI job type:
# validate-x86_64-linux, validate-i386-linux, ...
CIRCLE_JOB="circleci-$1"

gitlab_user=$(echo $CI_REPOSITORY_URL | cut -d/ -f4)
gitlab_repo=$(echo $CI_REPOSITORY_URL | cut -d/ -f5 | cut -d. -f1)

BODY="{ \"jobType\": \"$CIRCLE_JOB\", \"source\": { \"user\": \"$gitlab_user\", \"project\":\"$gitlab_repo\", \"commit\":\"$CI_COMMIT_SHA\" }, \"pipelineID\": $CI_PIPELINE_ID, \"runnerID\": $CI_RUNNER_ID, \"jobID\": $CI_JOB_ID }"


RESP=$(curl -s -XPOST -H "Content-Type: application/json" -d "$BODY" \
	    http://${GHCCI_URL}/job)

if [ $? -eq 0 ]; then
    build_num=$(echo $RESP | jq '.build_num')
    circle_url=$(echo $RESP | jq '.url')
else
    echo "Couldn't submit job"
    echo $RESP
    exit 1
fi

echo Circle CI build number: $build_num
echo Circle CI build page: $circle_url

outcome="null"
STATUS_URL="http://${GHCCI_URL}/job/${build_num}"
STATUS_RESP=""

while [ "$outcome" == "null" ]; do
    sleep 30s
    STATUS_RESP=$(curl -s $STATUS_URL)
    if [ $? -eq 0 ]; then
	new_outcome=$(echo $STATUS_RESP | jq '.outcome')
	jq_exitcode=$?
	if [ "$new_outcome" == "null" ] && [ $jq_exitcode -ne 0 ]; then
	    echo "Couldn't read 'outcome' field in JSON:"
	    echo $STATUS_RESP
	    echo "Skipping"
	else
	    outcome="$new_outcome"
	fi
    else
	echo "curl failed:"
	echo $STATUS_RESP
	echo "Skipping"
    fi
done

if [ "$outcome" == "\"success\"" ]; then
    echo The build passed
    artifactsBody=$(curl -s http://${GHCCI_URL}/job/${build_num}/artifacts)
    (echo $artifactsBody | jq '.[] | .url' | xargs wget -q) || echo "No artifacts"
    exit 0
else
    echo The build failed

    artifactsBody=$(curl -s http://${GHCCI_URL}/job/${build_num}/artifacts)
    (echo $artifactsBody | jq '.[] | .url' | xargs wget -q) || echo "No artifacts"

    failing_step=$(echo $STATUS_RESP | jq '.steps | .[] | .actions | .[] | select(.status != "success")')
    failing_step_name=$(echo $failing_step | jq '.name' | sed -e 's/^"//' -e 's/"$//' -e 's/\\r\\n/\n/')
    echo "Failing step: $failing_step_name"

    failing_cmds=$(echo $failing_step | jq '.bash_command' | sed -e 's/^"//' -e 's/"$//' -e 's/\\r\\n/\n/')
    echo "Failing command(s):"
    echo $failing_cmds

    log_url=$(echo $failing_step | jq '.output_url' | sed -e 's/^"//' -e 's/"$//' -e 's/\\r\\n/\n/')
    echo "Log url: $log_url"

    last_log_lines=$(curl -s $log_url | gunzip | jq '.[] | select(.type == "out") | .message' | sed -e 's/^"//' -e 's/"$//' -e 's/\\r\\n/\n/' | tail -50)
    echo End of the build log:
    echo $last_log_lines

    exit 1
fi
