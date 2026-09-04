#!/usr/bin/env python3
"""wavedrom2svg.py - render WaveDrom-style JSON timing diagrams to SVG.

Usage:
    python3 wavedrom2svg.py input.json [-o output.svg]
    cat diagram.json | python3 wavedrom2svg.py > diagram.svg

Rendering is done by the `wavedrom` package (https://pypi.org/project/wavedrom/),
a Python port of the WaveDrom renderer, so the full WaveDrom signal language is
supported -- minisim only uses a subset of it.
"""

import argparse
import json
import sys

try:
    import wavedrom
except ImportError:
    sys.stderr.write(
        "error: this script needs the 'wavedrom' package "
        "(pip install wavedrom, or pacman -S python-wavedrom)\n")
    sys.exit(1)


def main():
    ap = argparse.ArgumentParser(description="Render WaveDrom JSON to SVG")
    ap.add_argument("input", nargs="?", help="input JSON file (default: stdin)")
    ap.add_argument("-o", "--output", help="output SVG file (default: stdout)")
    args = ap.parse_args()

    if args.input:
        with open(args.input, "r", encoding="utf-8") as f:
            source = f.read()
    else:
        source = sys.stdin.read()

    try:
        parsed = json.loads(source)
    except json.JSONDecodeError as e:
        sys.stderr.write("error: invalid JSON input: %s\n" % e)
        sys.exit(1)

    if not isinstance(parsed, dict) or not any(
        k in parsed for k in ("signal", "assign", "reg")
    ):
        sys.stderr.write(
            "error: input is not a WaveDrom diagram "
            "(no 'signal', 'assign' or 'reg' key)\n")
        sys.exit(1)

    # wavedrom.render re-parses its (YAML-ish) string input; hand it the
    # JSON we already validated so errors stay ours.
    drawing = wavedrom.render(json.dumps(parsed))

    if args.output:
        with open(args.output, "w", encoding="utf-8") as f:
            drawing.write(f)
    else:
        sys.stdout.write(drawing.tostring())
        sys.stdout.write("\n")


if __name__ == "__main__":
    main()
