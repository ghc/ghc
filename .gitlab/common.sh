# Common bash utilities
# ----------------------

# Colors
BLACK="0;30"
GRAY="1;30"
RED="0;31"
LT_RED="1;31"
BROWN="0;33"
LT_BROWN="1;33"
GREEN="0;32"
LT_GREEN="1;32"
BLUE="0;34"
LT_BLUE="1;34"
PURPLE="0;35"
LT_PURPLE="1;35"
CYAN="0;36"
LT_CYAN="1;36"
WHITE="1;37"
LT_GRAY="0;37"

# GitLab Pipelines log section delimiters
# https://gitlab.com/gitlab-org/gitlab-foss/issues/14664
start_section() {
  name="$1"
  echo -e "section_start:$(date +%s):$name\015\033[0K"
}

end_section() {
  name="$1"
  echo -e "section_end:$(date +%s):$name\015\033[0K"
}

echo_color() {
  local color="$1"
  local msg="$2"
  echo -e "\033[${color}m${msg}\033[0m"
}

error() { echo_color "${RED}" "$1"; }
warn() { echo_color "${LT_BROWN}" "$1"; }
info() { echo_color "${LT_BLUE}" "$1"; }

fail() { error "error: $1"; exit 1; }

function run() {
  info "Running $*..."
  "$@" || ( error "$* failed"; return 1; )
}

