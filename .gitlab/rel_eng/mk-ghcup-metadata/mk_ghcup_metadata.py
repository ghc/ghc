#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p curl  "python3.withPackages (ps:[ps.pyyaml ps.python-gitlab ])"

"""
A tool for generating metadata suitable for GHCUp

There are two ways to prepare metadata:

* From a nightly pipeline.
* From a release pipeline.

In any case the script takes the same arguments:


* --metadata: The path to existing GHCup metadata to which we want to add the new entry.
* --version: GHC version of the pipeline
* --pipeline-id: The pipeline to generate metadata for
* --release-mode: Download from a release pipeline but generate URLs to point to downloads folder.
* --fragment: Only print out the updated fragment rather than the modified file

The script will then download the relevant bindists to compute the hashes. The
generated metadata is printed to stdout.

The metadata can then be used by passing the `--url-source` flag to ghcup.
"""

from subprocess import run, check_call
from getpass import getpass
import shutil
from pathlib import Path
from typing import NamedTuple, Callable, List, Dict, Optional
import tempfile
import re
import pickle
import os
import yaml
import gitlab
from urllib.request import urlopen
from urllib.parse import urlparse
import hashlib
import sys
import json
import urllib.parse
import fetch_gitlab

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)


gl = gitlab.Gitlab('https://gitlab.haskell.org', per_page=100)

# TODO: Take this file as an argument
metadata_file = ".gitlab/jobs-metadata.json"

release_base = "https://downloads.haskell.org/~ghc/{version}/ghc-{version}-{bindistName}"

eprint(f"Reading job metadata from {metadata_file}.")
with open(metadata_file, 'r') as f:
  job_mapping = json.load(f)

eprint(f"Supported platforms: {job_mapping.keys()}")


# Artifact precisely specifies a job what the bindist to download is called.
class Artifact(NamedTuple):
    job_name: str
    download_name: str
    output_name: Optional[str]
    subdir: str
    anchor_name: str

# Platform spec provides a specification which is agnostic to Job
# PlatformSpecs are converted into Artifacts by looking in the jobs-metadata.json file.
class PlatformSpec(NamedTuple):
    name: str
    subdir: str

source_artifact = Artifact('source-tarball'
                          , 'ghc-{version}-src.tar.xz'
                          , 'ghc-{version}-src.tar.xz'
                          , 'ghc-{version}'
                          , 'ghc{version}-src')
test_artifact = Artifact('source-tarball'
                        , 'ghc-{version}-testsuite.tar.xz'
                        , 'ghc-{version}-testsuite.tar.xz'
                        , 'ghc-{version}/testsuite'
                        , 'ghc{version}-testsuite')

def debian(n, arch='x86_64'):
    return linux_platform(arch, "{arch}-linux-deb{n}".format(arch=arch, n=n))

def darwin(arch):
    return PlatformSpec ( '{arch}-darwin'.format(arch=arch)
                        , 'ghc-{version}-{arch}-apple-darwin'.format(arch=arch, version="{version}") )

windowsArtifact = PlatformSpec ( 'x86_64-windows'
                               , 'ghc-{version}-x86_64-unknown-mingw32' )

def centos(n, arch='x86_64'):
    return linux_platform(arch, "x86_64-linux-centos{n}".format(n=n))

def fedora(n, arch='x86_64'):
    return linux_platform(arch, "x86_64-linux-fedora{n}".format(n=n))

def alpine(n, arch='x86_64'):
    return linux_platform(arch, "x86_64-linux-alpine{n}".format(n=n))

def rocky(n, arch='x86_64'):
    return linux_platform(arch, "x86_64-linux-rocky{n}".format(n=n))

def ubuntu(n, arch='x86_64'):
    return linux_platform(arch, "x86_64-linux-ubuntu{n}".format(n=n))

def linux_platform(arch, opsys):
    return PlatformSpec( opsys, 'ghc-{version}-{arch}-unknown-linux'.format(version="{version}", arch=arch) )


base_url = 'https://gitlab.haskell.org/api/v4/projects/1/jobs/{job_id}/artifacts/{artifact_name}'

hash_cache = {} # type: Dict[str, str]

# Download a URL and return its hash
def download_and_hash(url):
    if url in hash_cache: return hash_cache[url]
    eprint ("Opening {}".format(url))
    response = urlopen(url)
    sz = response.headers['content-length']
    hasher = hashlib.sha256()
    CHUNK = 2**22
    for n,text in enumerate(iter(lambda: response.read(CHUNK), '')):
        if not text: break
        eprint("{:.2f}% {} / {} of {}".format (((n + 1) * CHUNK) / int(sz) * 100, (n + 1) * CHUNK, sz, url))
        hasher.update(text)
    digest = hasher.hexdigest()
    hash_cache[url] = digest
    return digest

uri_to_anchor_cache=dict()

# Make the metadata for one platform.
def mk_one_metadata(release_mode, version, job_map, artifact):
    job_id = job_map[artifact.job_name].id

    url = base_url.format(job_id=job_id, artifact_name=urllib.parse.quote_plus(artifact.download_name.format(version=version)))

    # In --release-mode, the URL in the metadata needs to point into the downloads folder
    # rather then the pipeline.
    if release_mode:
        # the test artifact is bundled with the source artifact, so it doesn't have its own job name
        # So we must manually set the name of the bindist location
        if artifact == test_artifact:
            bindist_name = "testsuite"
        else:
            bindist_name = fetch_gitlab.job_triple(artifact.job_name)
        final_url = release_base.format( version=version
                                       , bindistName=urllib.parse.quote_plus(f"{bindist_name}.tar.xz"))
    else:
        final_url = url

    eprint(f"Making metadata for: {artifact}")
    eprint(f"Bindist URL: {url}")
    eprint(f"Download URL: {final_url}")

    # Download and hash from the release pipeline, this must not change anyway during upload.
    h = download_and_hash(url)

    res = { "dlUri": final_url
          , "dlSubdir": artifact.subdir.format(version=version)
          , "dlHash" : h }

    eprint(res)

    # add the uri to the anchor name cache so we can lookup an anchor for this uri
    uri_to_anchor_cache[final_url] = artifact.anchor_name
    return res

# Turns a platform into an Artifact respecting pipeline_type
# Looks up the right job to use from the .gitlab/jobs-metadata.json file
def mk_from_platform(release_mode, pipeline_type, platform):
    info = job_mapping[platform.name][pipeline_type]
    eprint(f"From {platform.name} / {pipeline_type} selecting {info['name']}")
    return Artifact(info['name']
                   , f"{info['jobInfo']['bindistName']}.tar.xz"
                   , "ghc-{version}-{pn}.tar.xz".format(version="{version}", pn=platform.name)
                   , platform.subdir
                   , f"ghc{{version}}-{platform.name}")

# Generate the new metadata for a specific GHC mode etc
def mk_new_yaml(release_mode, version, date, pipeline_type, job_map):
    def mk(platform):
        eprint("\n=== " + platform.name + " " + ('=' * (75 - len(platform.name))))
        return mk_one_metadata(release_mode, version, job_map, mk_from_platform(release_mode, pipeline_type, platform))

    # Here are all the bindists we can distribute
    ubuntu1804 = mk(ubuntu("18_04"))
    ubuntu2004 = mk(ubuntu("20_04"))
    rocky8 = mk(rocky("8"))
    centos7 = mk(centos(7))
    fedora33 = mk(fedora(33))
    darwin_x86 = mk(darwin("x86_64"))
    darwin_arm64 = mk(darwin("aarch64"))
    windows = mk(windowsArtifact)
    alpine3_12 = mk(alpine("3_12"))
    alpine3_18 = mk(alpine("3_18"))
    alpine3_18_arm64 = mk(alpine("3_18"), arch='aarch64')
    deb9 = mk(debian(9, "x86_64"))
    deb10 = mk(debian(10, "x86_64"))
    deb11 = mk(debian(11, "x86_64"))
    deb12 = mk(debian(12, "x86_64"))
    deb10_arm64 = mk(debian(10, "aarch64"))
    deb11_arm64 = mk(debian(11, "aarch64"))
    deb10_i386 = mk(debian(10, "i386"))

    source = mk_one_metadata(release_mode, version, job_map, source_artifact)
    test = mk_one_metadata(release_mode, version, job_map, test_artifact)

    # The actual metadata, this is not a precise science, but just what the ghcup
    # developers want.

    a64 = { "Linux_Debian": { "< 10": deb9
                           , "( >= 10 && < 11 )": deb10
                           , "( >= 11 && < 12 )": deb11
                           , ">= 11": deb12
                           , "unknown_versioning": deb11 }
          , "Linux_Ubuntu" : { "unknown_versioning": ubuntu2004
                             , "( >= 16 && < 18 )": deb9
                             , "( >= 18 && < 19 )": ubuntu1804 }
          , "Linux_Mint"   : { "< 20": ubuntu1804
                             , ">= 20": ubuntu2004
                             , "unknown_versioning": ubuntu2004 }
          , "Linux_CentOS"  : { "( >= 7 && < 8 )" : centos7
                              , "unknown_versioning" : centos7  }
          , "Linux_Fedora"  : { ">= 33": fedora33
                              , "unknown_versioning": centos7 }
          , "Linux_RedHat"  : { "< 9": centos7
                              , ">= 9": fedora33
                              , "unknown_versioning": fedora33 }
          , "Linux_UnknownLinux" : { "unknown_versioning": rocky8 }
          , "Darwin" : { "unknown_versioning" : darwin_x86 }
          , "Windows" : { "unknown_versioning" :  windows }
          , "Linux_Alpine" : { "( >= 3.12 && < 3.18 )": alpine_3_12
                             , ">= 3.18": alpine_3_18
                             , "unknown_versioning": alpine3_12 }

          }

    a32 = { "Linux_Debian": { "unknown_versioning": deb10_i386 }
          , "Linux_Ubuntu": { "unknown_versioning": deb10_i386 }
          , "Linux_Mint" : { "unknown_versioning": deb10_i386 }
          , "Linux_UnknownLinux" : { "unknown_versioning": deb10_i386 }
          }

    arm64 = { "Linux_UnknownLinux": { "unknown_versioning": deb10_arm64 }
            , "Linux_Alpine" : { "unknown_versioning": alpine3_18_arm64 }
            , "Linux_Debian": { "( >= 10 && < 11 )": deb10_arm64
                              , "( >= 11 && < 12 )": deb11_arm64
                              , "unknown_versioning": deb10_arm64
                              }
            , "Darwin": { "unknown_versioning": darwin_arm64 }
            }

    if release_mode:
        version_parts = version.split('.')
        if len(version_parts) == 3:
            final_version = version
        elif len(version_parts) == 4:
            final_version = '.'.join(version_parts[:2] + [str(int(version_parts[2]) + 1)])
        change_log = f"https://downloads.haskell.org/~ghc/{version}/docs/users_guide/{final_version}-notes.html"
    else:
        change_log =  "https://gitlab.haskell.org"

    if release_mode:
        tags =  ["Latest", "TODO_base_version"]
    else:
        tags = ["LatestNightly"]


    return { "viTags": tags
        , "viReleaseDay": date
        # Check that this link exists
        , "viChangeLog": change_log
        , "viSourceDL": source
        , "viTestDL": test
        , "viArch": { "A_64": a64
                    , "A_32": a32
                    , "A_ARM64": arm64
                    }
        }


def setNightlyTags(ghcup_metadata):
    for version in ghcup_metadata['ghcupDownloads']['GHC']:
        if "LatestNightly" in ghcup_metadata['ghcupDownloads']['GHC'][version]["viTags"]:
            ghcup_metadata['ghcupDownloads']['GHC'][version]["viTags"].remove("LatestNightly")
            ghcup_metadata['ghcupDownloads']['GHC'][version]["viTags"].append("Nightly")


def mk_dumper(version):
  class CustomAliasDumper(yaml.Dumper):
      def __init__(self, *args, **kwargs):
          super().__init__(*args, **kwargs)

      def generate_anchor(self, node):
          if isinstance(node, yaml.MappingNode):
            node_dict = { k.value : v.value for (k,v) in node.value }
            if 'dlUri' in node_dict:
              return uri_to_anchor_cache[node_dict['dlUri']].format(version=version.replace('.',''))
          return super().generate_anchor(node)

  return CustomAliasDumper


def main() -> None:
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--metadata', required=True, type=Path, help='Path to GHCUp metadata')
    parser.add_argument('--pipeline-id', required=True, type=int, help='Which pipeline to generate metadata for')
    parser.add_argument('--release-mode', action='store_true', help='Generate metadata which points to downloads folder')
    parser.add_argument('--fragment', action='store_true', help='Output the generated fragment rather than whole modified file')
    # TODO: We could work out the --version from the project-version CI job.
    parser.add_argument('--version', required=True, type=str, help='Version of the GHC compiler')
    parser.add_argument('--date', required=True, type=str, help='Date of the compiler release')
    args = parser.parse_args()

    project = gl.projects.get(1, lazy=True)
    pipeline = project.pipelines.get(args.pipeline_id)
    jobs = pipeline.jobs.list()
    job_map = { job.name: job for job in jobs }
    # Bit of a hacky way to determine what pipeline we are dealing with but
    # the aarch64-darwin job should stay stable for a long time.
    if 'nightly-aarch64-darwin-validate' in job_map:
        pipeline_type = 'n'
        if args.release_mode:
            raise Exception("Incompatible arguments: nightly pipeline but using --release-mode")

    elif 'release-aarch64-darwin-release' in job_map:
        pipeline_type = 'r'
    else:
        raise Exception("Not a nightly nor release pipeline")
    eprint(f"Pipeline Type: {pipeline_type}")


    new_yaml = mk_new_yaml(args.release_mode, args.version, args.date, pipeline_type, job_map)
    if args.fragment:
        print(yaml.dump({ args.version : new_yaml }, Dumper=mk_dumper(args.version)))

    else:
        with open(args.metadata, 'r') as file:
            ghcup_metadata = yaml.safe_load(file)
            if  args.version in ghcup_metadata['ghcupDownloads']['GHC']:
                raise RuntimeError("Refusing to override existing version in metadata")
            setNightlyTags(ghcup_metadata)
            ghcup_metadata['ghcupDownloads']['GHC'][args.version] = new_yaml
            print(yaml.dump(ghcup_metadata))



if __name__ == '__main__':
    main()

