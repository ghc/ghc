from pathlib import Path
import re
import subprocess
import sys


def apply(source: Path, patch: Path) -> None:
    first, *parts = re.split(rb'(?m)(?=^--- a/)', patch.read_bytes())
    assert not first
    for index, part in enumerate(parts):
        old, new, hunks = part.split(b'\n', maxsplit=2)
        name = old.removeprefix(b'--- a/')
        assert new == b'+++ b/' + name
        original = (source / name.decode()).read_bytes()
        if b'\r\n' in original:
            if original.count(b'\r\n') != original.count(b'\n'):
                raise ValueError(f'Mixed line endings in {name.decode()}')
            # Keep diff headers in LF. Match each hunk to the released file's bytes.
            hunks = re.sub(rb'(?m)^([ +\-].*)\n', rb'\1\r\n', hunks)
        parts[index] = b'\n'.join((old, new, hunks))
    subprocess.run(['patch', '--binary', '--batch', '--forward', '--fuzz=0', '-p1'],
                   cwd=source, input=b''.join(parts), check=True)


if __name__ == '__main__':
    if len(sys.argv) != 3 or sys.argv[2] not in ('matchability', 'cusks'):
        raise SystemExit('Usage: apply.py SOURCE_ROOT {matchability,cusks}')
    source_root = Path(sys.argv[1])
    patches = {'matchability': '01-fork-matchability.patch', 'cusks': '02-cusk-migration.patch'}
    apply(source_root, Path(__file__).with_name(patches[sys.argv[2]]))
