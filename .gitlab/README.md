# Where the GitLab happens


## Updating PERF_NOTES_PUSH_CREDENTIALS

This CI variable is used by test-metrics.sh to push performance data as a git
note to https://gitlab.haskell.org/ghc/ghc-performance-notes.

The current token will expire on 2025-07-02.

### STEPS

Set and fetch the updated token:

```
GITLAB_WRITE=<Your Gitlab API token>

one_year_later="$(date --date='1 year' --iso-8601)"
curl -X POST --header "PRIVATE-TOKEN: $GITLAB_WRITE" -H "Content-Type: application/json" \
    --data '{"name":"test-metrics.sh", "scopes":["write_repository"], "expires_at":"$one_year_later"}' \
    https://gitlab.haskell.org/api/v4/projects/117/access_tokens \
    | jq .token
```

Update the variable:

```
GITLAB_WRITE=<Your Gitlab API token>

NEW_VALUE=<Output from the above>

curl --fail-with-body --request PUT --header "PRIVATE-TOKEN: $GITLAB_WRITE" \
    "https://gitlab.haskell.org/api/v4/projects/1/variables/PERF_NOTES_PUSH_CREDENTIALS" \
    --form "value=$NEW_VALUE"

```
