#!/usr/bin/env python3
"""Check that Hadrian-supplied testsuite config values match pinned expected values."""
import sys

sys.path.insert(0, '.')
from ConfigPinsExpected import TESTSUITE_CONFIG_EXPECTED, platform_key


def main():
    if len(sys.argv) != 13:
        print("Usage: TestsuiteConfigPins.py platform arch wordsize "
              "have_ncg have_interp unreg tntc leading_us has_smp "
              "rts_linker interp_dyn cross")
        print(f"Got {len(sys.argv) - 1} args: {sys.argv[1:]}")
        sys.exit(1)

    (platform, arch, wordsize,
     have_ncg, have_interp, unreg, tntc,
     lead_us, has_smp, rts_linker, interp_dyn, cross) = sys.argv[1:]

    b = lambda s: s == "1"
    actual = {
        "platform":            platform,
        "arch":                arch,
        "wordsize":            wordsize,
        "have_ncg":            b(have_ncg),
        "have_interp":         b(have_interp),
        "unregisterised":      b(unreg),
        "tables_next_to_code": b(tntc),
        "leading_underscore":  b(lead_us),
        "target_has_smp":      b(has_smp),
        "have_RTS_linker":     b(rts_linker),
        "interp_force_dyn":    b(interp_dyn),
        "cross":               b(cross),
    }
    key = platform_key(platform, b(cross), b(unreg), not b(tntc))

    if key not in TESTSUITE_CONFIG_EXPECTED:
        print(f"Platform key {key!r} not in ConfigPinsExpected.")
        print("Add this entry to TESTSUITE_CONFIG_EXPECTED in ConfigPinsExpected.py:")
        print()
        print(f"    {key!r}: {{")
        for k, v in actual.items():
            print(f"        {k!r}: {v!r},")
        print("    },")
        sys.exit(1)

    expected = TESTSUITE_CONFIG_EXPECTED[key]
    mismatches = [
        (k, ev, actual[k])
        for k, ev in expected.items()
        if ev != actual.get(k)
    ]

    if mismatches:
        print(f"MISMATCH for {key!r}:")
        for k, ev, av in mismatches:
            print(f"  {k!r}: expected {ev!r}, got {av!r}")
        sys.exit(1)

    print(f"OK: {key}")


if __name__ == "__main__":
    main()
