This is the template to be used to create release tracking tickets.  To make a new release:

* **Milestone**.  Create a new Gitlab milestone for the release.  We refer to it here as **TODO milestone**.
* **Tracking ticket**.  Create a new Gitlab issue, the tracking ticket; copy this template into it; and fill in the **TODO**s. 

In the template, those items marked with *major-only* can be ignored for minor releases.

# The template to copy/paste starts here!

This is the release checklist for the **TODO milestone**. *(When filling in the template, make that "**TODO milestone**" into a link to the milestone, and delete this sentence.)*

See the milestone for the planned release schedule, and other details. See https://gitlab.haskell.org/ghc/ghc-hq/-/blob/main/release-management.mkd for release policies.

See #16816 for the template that this ticket was derived from.

# Pre-fork checklist (*major-only*)

When forking a new release branch for a new major release series (e.g. `ghc-9.0`),  please check the following:

* [ ] Add the release to the [status page](https://gitlab.haskell.org/ghc/ghc/-/wikis/GHC-Status), including a link to the milestone.
* [ ] Verify that the platform support documentation is up-to-date with reality:
  * https://gitlab.haskell.org/ghc/ghc/-/wikis/platforms/windows
  * https://gitlab.haskell.org/ghc/ghc/-/wikis/platforms/
* [ ] Remove the release notes for the previous release (e.g. `docs/users_guide/8.6.*-notes.rst`)
* [ ] In `configure.ac` update `MinBootGhcVersion` to be the major version of the last supported bootstrap compiler. We currently guarantee bootstrapping with the last two major releases; therefore, when preparing, e.g., GHC 9.4 you should set `MinBootGhcVersion` to `9.0`.
* [ ] Create the new branch: `git branch ghc-9.0`
* [ ] In the new branch bump the version number in `configure.ac` to, e.g., `9.0`
* [ ] In `master` bump the version number in `configure.ac` to that of the next development series, e.g., `9.1` and ensure that `MinBootGhcVersion` is set correctly
* [ ] Verify that package versions have been bumped as necessary under PVP:
   * [ ] `base`
   * [ ] `template-haskell`
   * [ ] `ghc-prim`
* [ ] Tag this `master` commit as, e.g., `ghc-9.1-start`
* [ ] Push all of the above tags and branches
* [ ] Create a new worksheet in the GHC submodule tracking spreadsheet (ask in `#ghc` for a link), populate the "current" and "desired" version columns, and send an email to submodule maintainers and `ghc-releases@haskell.org` asking them to notify us of their needs
* [ ] After forking, remove release notes from `master` and start a new set of release notes for the next release
* [ ] After forking, notify `ghc-releases@haskell.org` that fork has been made
* [ ] Create a weekly scheduled pipeline building the new branch to ensure that it does not bit-rot and bindists are persistently available

# Pre-release checklist

* [ ] Ensure that the Haddock (haddock-library, haddock-api) version numbers have been appropriately bumped.
* [ ] Ensure that submodules are on released tags (the below is produced using [this script](https://gitlab.haskell.org/bgamari/ghc-utils/blob/master/rel-eng/submod-release-summary.py)):
   * [ ] `libraries/Cabal`: version *todo*
   * [ ] `libraries/Win32`: version *todo*
   * [ ] `libraries/binary`: version *todo*
   * [ ] `libraries/bytestring`: version *todo*
   * [ ] `libraries/containers`: version *todo*
   * [ ] `libraries/deepseq`: version *todo*
   * [ ] `libraries/directory`: version *todo*
   * [ ] `libraries/filepath`: version *todo*
   * [ ] `libraries/haskeline`: version *todo*
   * [ ] `libraries/mtl`: version *todo*
   * [ ] `libraries/parsec`: version *todo*
   * [ ] `libraries/pretty`: version *todo*
   * [ ] `libraries/process`: version *todo*
   * [ ] `libraries/terminfo`: version *todo*
   * [ ] `libraries/text`: version *todo*
   * [ ] `libraries/time`: version *todo*
   * [ ] `libraries/transformers`: version *todo*
   * [ ] `libraries/unix`: version *todo*
   * [ ] `libraries/xhtml`: version *todo*
   * [ ] `utils/haddock`: version *todo*
   * [ ] `utils/hsc2hs`: version *todo*
* [ ] Ensure that all of the versions above are newer than the ones for older major releases and incorporate any patch updates 
* [ ] Notify stakeholders of release progress: `Julian Ospald <hasufell@posteo.de>, GHC releases <ghc-releases@haskell.org>, GHC developers <ghc-devs@haskell.org>`
* [ ] Non-released submodules up-to-date:
   * [ ] `nofib` (upstream: ghc/nofib>)
   * [ ] `libffi-tarballs` (upstream: ghc/libffi-tarballs>)
   * [ ] `libraries/ghc-bignum/gmp/gmp-tarballs` (upstream: ghc/gmp-tarballs>)
* [ ] `LlvmMinVersion` and `LlvmMaxVersion` in `configure.ac` is targetting intended LLVM version
* [ ] `llvm-targets` file [updated](https://gitlab.haskell.org/ghc/ghc/wikis/making-releases#updating-the-tree)
* [ ] Release notes (`docs/users_guide/x.y.z-notes.rst`) written
   * Release notes linked in `docs/users_guide/release-notes.rst`
   * LLVM version requirement mentioned
* [ ] Remove "Included libraries" sections from old release notes
* [ ] `autoconf` scripts [updated](https://gitlab.haskell.org/ghc/ghc/wikis/making-releases#updating-the-tree)
* [ ] Check that Unicode database in `base` (`libraries/base/cbits/README.Unicode`) reflects current standard release (http://www.unicode.org/versions/latest/).
* [ ] Verify that the ~"backport needed" label has no more issues/merge requests needing backport
* [ ] Verify that all CI builds are green before moving to *release checklist*
* [ ] Update hadrian bootstrap plans for the latest compiler releases:
   * Update `hadrian/bootstrap/generate_bootstrap_plans` to include any new compiler releases in the last two major release series
   * Drop any plans that are outside of the bootstrap support window
   * Run `hadrian/bootstrap/generate_bootstrap_plans` and commit the files that it generates
   * Update `bootstrap_matrix` in `.gitlab-ci.yml` to reflect the most recent minor release in each major series supported by bootstrapping

# Release candidate checklist

* [ ] Announce on: `GHC developers <ghc-devs@haskell.org>, GHC releases <ghc-releases@haskell.org>, GHC users <glasgow-haskell-users@haskell.org>` and the matrix ecosystem channel.
* [ ] For minor releases: Ask stackage maintainers to test the release candidate.

# Release checklist

* [ ] Ensure that the [Migration](https://gitlab.haskell.org/ghc/ghc/wikis/migration/) page is up-to-date
* [ ] Push a provision provisional release commit to trigger the release builds using `git push -o ci.variable="RELEASE_JOB=yes"`
* [ ] Wait until builds finish; verify that they finished successfully
* [ ] Write down a link to the release pipeline here: <URL>
* [ ] Fetch release artifacts:
  ```bash
  $ mkdir $VERSION
  $ cd $VERSION
  $ nix run $GHC/.gitlab/rel_eng#fetch-gitlab --pipeline $PIPELINE_ID --release $VERSION
  ```
  where `GHC` is the path to a GHC source tree, `VERSION` is the version number, and `PIPELINE_ID` is the release pipeline id.
* [ ] Recompress release artifacts: `nix run $GHC/.gitlab/rel_eng#upload -- recompress`
* [ ] Generate hashes: `nix run $GHC/.gitlab/rel_eng#upload -- gen_hashes`
* [ ] Sign release artifacts: `nix run $GHC/.gitlab/rel_eng#upload -- sign`
* [ ] Upload to `downloads.haskell.org`: `nix run $GHC/.gitlab/rel_eng#upload -- upload`
* [ ] If most recent release: Update `latest` symlink: `nix run $GHC/.gitlab/rel_eng#upload -- set_symlink latest`
* [ ] Update `X.Y-latest` symlink: `nix run $GHC/.gitlab/rel_eng#upload -- set_symlink 9.8-latest`
* [ ] Purge CDN cache: `nix run $GHC/.gitlab/rel_eng#upload -- purge`
* [ ] Submit `ghcup` metadata upstream:
   * Manually trigger the `ghcup-metadata-testing-release` job of the release pipeline
   * Download the `metadata_test.yaml` artifact from the `ghcup-metadata-release` job of the release pipeline
   * Clone [`ghcup-metadata`](https://github.com/haskell/ghcup-metadata/)
   * Integrate the appropriate release section into `ghcup-metadata/ghcup-0.0.7.yaml` (or `ghcup-prerelease-0.0.7.yaml` for pre-releases)
   * Replace the `&id<n>` identifiers with unique names
   * Add the `Latest` (or `LatestPrerelease`) to the new release's `viTags` field
   * Remove `Latest` for the previous release's `viTags` field; for pre-releases add the `Prerelease` tag)
   * Add the `base-w.x.y.z` (e.g. `base-4.19.0.0`) tag to `viTags`
   * Sign the resulting metadata YAML file with `gpg --detach-sign`
   * Open a merge request submitting the metadata upstream
   * Start a "Bindist installation" [workflow](https://github.com/haskell/ghcup-metadata/actions/workflows/bindists.yaml)
* [ ] [Make a release tag](https://gitlab.haskell.org/ghc/ghc/wikis/making-releases#tagging-the-release)
* [ ] Release/revise GHC-maintained libraries on Hackage using [this script](https://gitlab.haskell.org/ghc/ghc/-/blob/master/.gitlab/rel_eng/upload_ghc_libs.py):
   * [ ] `libraries/base`
   * [ ] `libraries/ghc-prim`
   * [ ] `libraries/array`
   * [ ] `libraries/stm`
   * [ ] `libraries/ghc-heap`
   * [ ] `libraries/ghc-compact`
   * [ ] `libraries/ghc-boot`
   * [ ] `libraries/ghc-boot-th`
   * [ ] `libraries/hpc`
   * [ ] `libraries/libiserv`
   * [ ] `libraries/template-haskell`
   * [ ] `libraries/ghc-bignum`
* [ ] Update ghc/homepage>:
   * [ ] Write download page (see ghc/homepage>)
   * [ ] Make sure that the Signing Key is correct for the relevant release
   * [ ] Add news item to [`index.html`](https://gitlab.haskell.org/ghc/homepage/blob/master/index.shtml)
   * [ ] Add link to [`download.shtml`](https://gitlab.haskell.org/ghc/homepage/blob/master/download.shtml). Be sure to link to the Migration guide.
   * [ ] Look over changes locally
   * [ ] Add release announcement to [GHC blog](https://gitlab.haskell.org/ghc/homepage/tree/master/blog). Be sure to link to the Migration guide.
   * [ ] Push changes to `master`
* [ ] Announce on: `GHC developers <ghc-devs@haskell.org>, GHC releases <ghc-releases@haskell.org>, GHC users <glasgow-haskell-users@haskell.org>, Haskell Cafe <haskell-cafe@haskell.org>`
   * Mention sponsors
   * Mention ghc/ghc-hq>
   * Link to release notes and migration guide
* [ ] Announce on: [Haskell Discourse](https://discourse.haskell.org/), [/r/haskell](https://reddit.com/r/haskell), [/m/haskell](https://kbin.social/m/haskell)
* [ ] When releasing the latest version, update `latest` symlink on `downloads.haskell.org`, e.g. `ln -sfT 9.6.3 latest` if 9.6 was the latest major series.

# Post-release checklist

* [ ] Update the Wiki [status page](https://gitlab.haskell.org/ghc/ghc/-/wikis/GHC-Status)
* [ ] Update the [language pragma history](https://gitlab.haskell.org/ghc/ghc/wikis/language-pragma-history)
* [ ] Mark milestone as *closed*
* [ ] Update `/topic` in `#ghc`
* [ ] Update the [VersionHistory](https://gitlab.haskell.org/ghc/ghc/wikis/commentary/libraries/version-history) wiki page
* [ ] Set `RELEASE=NO`
* [ ] Create a release tracking ticket for the next release using the template in #16816
