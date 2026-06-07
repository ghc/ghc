#!/usr/bin/env python3
"""Check that ghc --info output matches pinned expected values."""
import ast
import subprocess
import sys

sys.path.insert(0, '.')
from ConfigPinsExpected import GHC_INFO_EXPECTED, PINNED_GHC_INFO_FIELDS, platform_key


def info_platform_key(info_map):
    return platform_key(
        info_map.get("target platform string", "<unknown>"),
        info_map.get("cross compiling", "NO") == "YES",
        info_map.get("Unregisterised", "NO") == "YES",
        info_map.get("Tables next to code", "YES") == "NO",
    )


def main():
    if len(sys.argv) != 2:
        print("Usage: GhcInfoPins.py <compiler-path>")
        sys.exit(1)

    compiler = sys.argv[1]
    try:
        result = subprocess.run([compiler, "--info"], capture_output=True, text=True)
    except OSError as e:
        print(f"Failed to run {compiler!r} --info: {e}")
        sys.exit(1)
    if result.returncode != 0:
        print(f"Error running {compiler!r} --info:\n{result.stderr}")
        sys.exit(1)
    try:
        info_map = dict(ast.literal_eval(result.stdout.strip()))
    except (ValueError, SyntaxError) as e:
        print(f"Failed to parse {compiler!r} --info output: {e}")
        sys.exit(1)
    key = info_platform_key(info_map)

    actual = {f: info_map.get(f, "<missing>") for f in PINNED_GHC_INFO_FIELDS}

    if key not in GHC_INFO_EXPECTED:
        print(f"Platform key {key!r} not in ConfigPinsExpected.")
        print("Add this entry to GHC_INFO_EXPECTED in ConfigPinsExpected.py:")
        print()
        print(f"    {key!r}: {{")
        for f, v in actual.items():
            print(f"        {f!r}: {v!r},")
        print("    },")
        sys.exit(1)

    expected = GHC_INFO_EXPECTED[key]
    mismatches = [
        (f, ev, actual.get(f, "<missing>"))
        for f, ev in sorted(expected.items())
        if ev != actual.get(f, "<missing>")
    ]

    if mismatches:
        print(f"MISMATCH for {key!r}:")
        for f, ev, av in mismatches:
            print(f"  {f!r}: expected {ev!r}, got {av!r}")
        sys.exit(1)

    print(f"OK: {key}")


if __name__ == "__main__":
    main()
