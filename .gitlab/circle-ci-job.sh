# Circle CI "backend" for Gitlab CI
# =================================
#
# Usage example:
#   .gitlab/circle-ci-job.sh validate-x86_64-linux
#
# It currently requires the following environment variables to
# be set:
#
# - CI_RUNNER_ID, CI_JOB_ID, CI_COMMIT_SHA, set automatically,
#   as per: https://docs.gitlab.com/ce/ci/variables/
#
# - CIRCLECI_TOKEN, which should be set as a Gitlab
#   CI "variable", as per:
#   https://docs.gitlab.com/ce/ci/variables/README.html#variables
#
# - SSH_PRIVATE_KEY, variable set in the gitlab interface, as per:
#   https://docs.gitlab.com/ce/ci/ssh_keys/#ssh-keys-when-using-the-docker-executor
#   This script itself doesn't actually need that one, but it is
#   needed for the .gitlab-ci.yml script.
#
#
# Finally, there are two other things to configure to get artifacts to be
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

[ $# -gt 0 ] || (echo You need to pass the Circle CI job type as argument to this script; exit 1)
[ ${CI_RUNNER_ID:-} ] || (echo "CI_RUNNER_ID is not set"; exit 1)
[ ${CI_JOB_ID:-} ] || (echo "CI_JOB_ID is not set"; exit 1)
[ ${CI_COMMIT_SHA:-} ] || (echo "CI_COMMIT_SHA is not set"; exit 1)

GITHUB_ORG="ghc"
GITHUB_PROJECT="ghc-diffs"
GITHUB_BRANCH="gitlab/${CI_RUNNER_ID}/${CI_JOB_ID}"

# the first argument to this script is the Circle CI job type:
# validate-x86_64-linux, validate-i386-linux, ...
CIRCLE_JOB=$1

git remote add gh git@github.com:${GITHUB_ORG}/${GITHUB_PROJECT} &> /dev/null || echo "gh remote already present"
git checkout -b ${GITHUB_BRANCH} || true # if we've already done this before
git push gh ${GITHUB_BRANCH} || true # if we've already done this before

BODY="{ \"revision\": \"${CI_COMMIT_SHA}\", \"build_parameters\": { \"CIRCLE_JOB\": \"${CIRCLE_JOB}\" } }"
RESP=$(curl -s -X POST -H "Content-Type: application/json" -d "$BODY" \
	    https://circleci.com/api/v1.1/project/github/${GITHUB_ORG}/${GITHUB_PROJECT}/tree/${GITHUB_BRANCH}?circle-token=${CIRCLECI_TOKEN})

if [ $? -eq 0 ]; then
  build_num=$(echo $RESP | jq '.build_num')
else
    echo "Couldn't submit job"
    echo $RESP
    exit 1
fi

echo Circle CI build number: $build_num
echo Circle CI build page: https://circleci.com/gh/${GITHUB_ORG}/${GITHUB_PROJECT}/$build_num

outcome="null"
STATUS_URL="https://circleci.com/api/v1.1/project/github/${GITHUB_ORG}/${GITHUB_PROJECT}/${build_num}?circle-token=${CIRCLECI_TOKEN}"
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
    artifactsBody=$(curl -s https://circleci.com/api/v1.1/project/github/${GITHUB_ORG}/${GITHUB_PROJECT}/${build_num}/artifacts?circle-token=${CIRCLECI_TOKEN})
    (echo $artifactsBody | jq '.[] | .url' | xargs wget -q) || echo "No artifacts"
    exit 0
else
    echo The build failed

    artifactsBody=$(curl -s https://circleci.com/api/v1.1/project/github/${GITHUB_ORG}/${GITHUB_PROJECT}/${build_num}/artifacts?circle-token=${CIRCLECI_TOKEN})
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
