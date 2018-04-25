set -e

HACKAGE_REPO_TOOL_VERSION="0.1.1"
CABAL_VERSION="2.1.0.0"

if [ "$TRAVIS_OS_NAME" = "linux" ]; then
    ARCH="x86_64-linux"
else
    ARCH="x86_64-osx"
fi

CABAL_STORE_DB="${HOME}/.cabal/store/ghc-${GHCVER}/package.db"
CABAL_LOCAL_DB="${TRAVIS_BUILD_DIR}/dist-newstyle/packagedb/ghc-${GHCVER}"
CABAL_BDIR="${TRAVIS_BUILD_DIR}/dist-newstyle/build/$ARCH/ghc-$GHCVER/Cabal-${CABAL_VERSION}"
CABAL_TESTSUITE_BDIR="${TRAVIS_BUILD_DIR}/dist-newstyle/build/$ARCH/ghc-$GHCVER/cabal-testsuite-${CABAL_VERSION}"
CABAL_INSTALL_BDIR="${TRAVIS_BUILD_DIR}/dist-newstyle/build/$ARCH/ghc-$GHCVER/cabal-install-${CABAL_VERSION}"
CABAL_INSTALL_SETUP="${CABAL_INSTALL_BDIR}/setup/setup"
SOLVER_BENCHMARKS_BDIR="${TRAVIS_BUILD_DIR}/dist-newstyle/build/$ARCH/ghc-$GHCVER/solver-benchmarks-${CABAL_VERSION}"
HACKAGE_REPO_TOOL_BDIR="${TRAVIS_BUILD_DIR}/dist-newstyle/build/$ARCH/ghc-$GHCVER/hackage-repo-tool-${HACKAGE_REPO_TOOL_VERSION}/c/hackage-repo-tool"

# ---------------------------------------------------------------------
# Timing / diagnostic output
# ---------------------------------------------------------------------

JOB_START_TIME=$(date +%s)

timed() {
    echo "\$ $*"
    start_time=$(date +%s)

	# Run the job
    $* || exit $?

	# Calculate the durations
    end_time=$(date +%s)
    duration=$((end_time - start_time))
	total_duration=$((end_time - JOB_START_TIME))

	# Print them
    echo "$* took $duration seconds."
    echo "whole job took $total_duration seconds so far."

	# Terminate on OSX
	if [ $total_duration -ge 2400 -a $(uname) = "Darwin" ]; then
		echo "Job taking over 40 minutes. Terminating"
		exit 1
	fi
    echo "----"
}

travis_retry () {
    $*  || (sleep 1 && $*) || (sleep 2 && $*)
}
