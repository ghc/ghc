#!/usr/bin/env python3

"""Generate a large multiple-home-unit DAG for manual performance testing."""

from __future__ import annotations

import argparse
import os
from pathlib import Path


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Generate many home units in a DAG, one module per unit."
    )
    parser.add_argument("--units", type=int, default=1000)
    parser.add_argument(
        "--out-dir",
        type=Path,
        default=Path("generated-home-unit-dag"),
        help="Directory in which to generate the unit folders and unit files.",
    )
    return parser.parse_args()


def unit_name(i: int) -> str:
    return f"u{i:04d}"


def unit_file_name(i: int) -> str:
    return f"unit{i:04d}"


def module_name(i: int) -> str:
    return f"U{i:04d}"


def deps_for(i: int) -> list[int]:
    if i <= 1:
        return []

    target = min(100, i - 1)

    # Start with a sliding window of the most recent predecessors. This makes
    # later units dense while keeping the graph acyclic.
    start = max(1, i - target)
    result = list(range(start, i))

    # For larger graphs, mix in a few long-range edges and then refill from the
    # end to keep at least `target` dependencies after deduplication.
    extras = [i // 2, i // 3, i // 4, i // 5, i // 8]
    seen = set()
    dense = []
    for dep in extras + result:
        if 1 <= dep < i and dep not in seen:
            seen.add(dep)
            dense.append(dep)

    dep = i - 1
    while len(dense) < target:
        if dep not in seen:
            seen.add(dep)
            dense.append(dep)
        dep -= 1

    dense.sort()
    return dense


def render_module(i: int, deps: list[int]) -> str:
    imports = "".join(f"import {module_name(dep)}\n" for dep in deps)
    dep_expr = " + ".join(f"unitValue{dep:04d}" for dep in deps) or "0"
    return (
        f"module {module_name(i)} where\n\n"
        f"{imports}"
        f"unitValue{i:04d} :: Int\n"
        f"unitValue{i:04d} = 1 + ({dep_expr})\n"
    )


def render_unit_file(i: int, deps: list[int], out_dir: Path) -> str:
    working_dir = Path(os.path.normpath(out_dir / unit_name(i))).as_posix()
    flags = [
        "-working-dir",
        working_dir,
        module_name(i),
        "-i",
        "-i.",
        "-this-unit-id",
        unit_name(i),
        "-this-package-name",
        unit_name(i),
    ]
    for dep in deps:
        flags.extend(["-package-id", unit_name(dep)])
    return " ".join(flags) + "\n"


def main() -> None:
    args = parse_args()
    out_dir = args.out_dir
    out_dir.mkdir(parents=True, exist_ok=True)

    for i in range(1, args.units + 1):
        deps = deps_for(i)
        unit_dir = out_dir / unit_name(i)
        unit_dir.mkdir(parents=True, exist_ok=True)
        (unit_dir / f"{module_name(i)}.hs").write_text(render_module(i, deps))
        (out_dir / unit_file_name(i)).write_text(render_unit_file(i, deps, out_dir))


if __name__ == "__main__":
    main()
