import logging
from pathlib import Path
import subprocess
import gitlab
import json

logging.basicConfig(level=logging.INFO)

def strip_prefix(s, prefix):
    if s.startswith(prefix):
        return s[len(prefix):]
    else:
        return None

do_not_distribute = set(["release-x86_64-linux-fedora33-release-hackage"])

def job_triple(job_name):
    bindists = {
        'release-x86_64-windows-release': 'x86_64-unknown-mingw32',
        'release-x86_64-windows-int_native-release': 'x86_64-unknown-mingw32-int_native',
        'release-x86_64-linux-rocky8-release': 'x86_64-rocky8-linux',
        'release-x86_64-linux-ubuntu22_04-release': 'x86_64-ubuntu22_04-linux',
        'release-x86_64-linux-ubuntu20_04-release': 'x86_64-ubuntu20_04-linux',
        'release-x86_64-linux-ubuntu18_04-release': 'x86_64-ubuntu18_04-linux',
        'release-x86_64-linux-fedora33-release+debug_info': 'x86_64-fedora33-linux-dwarf',
        'release-x86_64-linux-fedora33-release': 'x86_64-fedora33-linux',
        'release-x86_64-linux-fedora27-release': 'x86_64-fedora27-linux',
        'release-x86_64-linux-deb11-release': 'x86_64-deb11-linux',
        'release-x86_64-linux-deb10-release+debug_info': 'x86_64-deb10-linux-dwarf',
        'release-x86_64-linux-deb10-release': 'x86_64-deb10-linux',
        'release-x86_64-linux-deb9-release': 'x86_64-deb9-linux',
        'release-x86_64-linux-centos7-release': 'x86_64-centos7-linux',
        'release-x86_64-linux-alpine3_12-release+fully_static': 'x86_64-alpine3_12-linux-static',
        'release-x86_64-linux-alpine3_12-release': 'x86_64-alpine3_12-linux',
        'release-x86_64-linux-alpine3_12-int_native-release+fully_static': 'x86_64-alpine3_12-linux-static-int_native',
        'release-x86_64-darwin-release': 'x86_64-apple-darwin',
        'release-i386-linux-deb9-release': 'i386-deb9-linux',
        'release-armv7-linux-deb10-release': 'armv7-deb10-linux',
        'release-aarch64-linux-deb10-release': 'aarch64-deb10-linux',
        'release-aarch64-darwin-release': 'aarch64-apple-darwin',

        'source-tarball': 'src',
        'package-hadrian-bootstrap-sources': 'hadrian-bootstrap-sources',
        'doc-tarball': 'docs',
        'hackage-doc-tarball': 'hackage_docs',
    }

    # Some bindists use the +no_split_sections transformer due to upstream
    # toolchain bugs.
    bindists.update({
        f'{k}+no_split_sections': v
        for k,v in bindists.items()
    })

    if job_name in bindists:
        return bindists[job_name]
    else:
        #return strip_prefix(job.name, 'validate-')
        return None

class UnhandledJobException(Exception):
    # Raised when there is a release job in the pipeline but we don't explicitly handle it.
    def __init__(self, name):
        self.message = f"{name} is a release job but not downloaded"
        super().__init__(self.message)

def fetch_artifacts(release: str, pipeline_id: int,
                    dest_dir: Path, gl: gitlab.Gitlab):
    dest_dir.mkdir(exist_ok=True)
    # Write the pipeline id into output directory
    with open(f"{dest_dir}/metadata.json", 'w') as out: json.dump({ "pipeline_id": pipeline_id }, out)

    proj = gl.projects.get('ghc/ghc')
    pipeline = proj.pipelines.get(pipeline_id)
    tmpdir = Path("fetch-gitlab")
    tmpdir.mkdir(exist_ok=True)
    for pipeline_job in pipeline.jobs.list(all=True):
        if len(pipeline_job.artifacts) == 0:
            logging.info(f'job {pipeline_job.name} ({pipeline_job.id}) has no artifacts')
            continue

        job = proj.jobs.get(pipeline_job.id)
        triple = job_triple(job.name)
        if triple is None:
            if job.name.startswith("release") and not (job.name in do_not_distribute):
                raise(UnhandledJobException(job.name))
            logging.info(f'ignoring {job.name}')
            continue

        #artifactZips = [ artifact
        #                 for artifact in job.artifacts
        #                 if artifact['filename'] == 'artifacts.zip' ]
        try:
            destdir = tmpdir / job.name
            zip_name = Path(f"{tmpdir}/{job.name}.zip")
            if not zip_name.exists() or zip_name.stat().st_size == 0:
                logging.info(f'downloading archive {zip_name} for job {job.name} (job {job.id})...')
                with open(zip_name, 'wb') as f:
                    job.artifacts(streamed=True, action=f.write)

            if zip_name.stat().st_size == 0:
                logging.info(f'artifact archive for job {job.name} (job {job.id}) is empty')
                continue


            subprocess.run(['unzip', '-bo', zip_name, '-d', destdir])
            bindist_files = list(destdir.glob('ghc*.tar.xz'))

            if job.name == 'source-tarball':
                for f in bindist_files:
                    dest = dest_dir / f.name
                    logging.info(f'extracted {job.name} to {dest}')
                    f.replace(dest)
            elif job.name == 'package-hadrian-bootstrap-sources':
                all_bootstrap_sources = destdir / 'hadrian-bootstrap-sources-all.tar.gz'
                dest = dest_dir / 'hadrian-bootstrap-sources'
                dest.mkdir()
                subprocess.run(['tar', '-xf', all_bootstrap_sources, '-C', dest])
                logging.info(f'extracted {job.name}/{all_bootstrap_sources} to {dest}')
            elif job.name == 'doc-tarball':
                dest = dest_dir / 'docs'
                dest.mkdir()
                doc_files = list(destdir.glob('*.tar.xz'))
                for f in doc_files:
                    subprocess.run(['tar', '-xf', f, '-C', dest])
                    logging.info(f'extracted docs {f} to {dest}')
                index_path = destdir / 'docs' / 'index.html'
                index_path.replace(dest / 'index.html')
            elif job.name == 'hackage-doc-tarball':
                dest = dest_dir / 'hackage_docs'
                logging.info(f'moved hackage_docs to {dest}')
                (destdir / 'hackage_docs').replace(dest)
            else:
                dest = dest_dir / f'ghc-{release}-{triple}.tar.xz'
                if dest.exists():
                    logging.info(f'bindist {dest} already exists')
                    continue
                if len(bindist_files) == 0:
                    logging.warn(f'Bindist does not exist')
                    continue
                bindist = bindist_files[0]
                logging.info(f'extracted {job.name} to {dest}')
                bindist.replace(dest)
        except Exception as e:
            logging.error(f'Error fetching job {job.name}: {e}')
            pass

def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--pipeline', '-p', required=True, type=int, help="pipeline id")
    parser.add_argument('--release', '-r', required=True, type=str, help="release name")
    parser.add_argument('--output', '-o', type=Path, default=Path.cwd(), help="output directory")
    parser.add_argument('--profile', '-P', default='haskell',
                        help='python-gitlab.cfg profile name')
    args = parser.parse_args()
    gl = gitlab.Gitlab.from_config(args.profile)
    fetch_artifacts(args.release, args.pipeline,
                    dest_dir=args.output, gl=gl)
