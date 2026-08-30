#!/usr/bin/env python3
"""Regression tests: run minisim on the examples and check the WaveDrom JSON
against hand-computed expectations, then render each to SVG and validate it.

Usage:  python3 tests/check.py
"""

import json
import subprocess
import sys
import xml.etree.ElementTree as ET
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
MINISIM = ROOT / "minisim"
SVG = ROOT / "wavedrom2svg.py"

# name -> (hdl file, {signal name: (wave, data-or-None)})
EXPECTED = {
    "sample": ("sample.txt", {
        "c1":     ("1010101", None),
        "c2":     ("01.0.1.", None),
        "w1":     ("1010...", None),
        "w2":     ("2222...", ["011", "00d", "005", "000"]),
        "w3":     ("1..0.10", None),
        "w4":     ("x......", None),
        "w5":     ("2..2.22", ["xx", "xx", "xx", "xx"]),
        "w6":     ("1010...", None),
        "a":      ("10.....", None),
        "b":      ("10.....", None),
        "and_A":  ("10.....", None),
        "and_B":  ("01.0...", None),
        "and_out": ("0......", None),
    }),
    "shift": ("examples/shift.hdl", {
        "c1":   ("10101010", None),
        "din":  ("1.0.1010", None),
        "q0":   ("x.1.0...", None),
        "q1":   ("x...1.0.", None),
        "q2":   ("x.....1.", None),
        "pair": ("x.2.2.2.", ["x", "2", "0"]),
        "ored": ("x.1...0.", None),
    }),
    "latch": ("examples/latch.hdl", {
        "c1":      ("10101010", None),
        "d":       ("01...0..", None),
        "en":      ("1..0.10.", None),
        "q_latch": ("01...0..", None),
        "q_dff":   ("x.1...0.", None),
    }),
    "clocks": ("examples/clocks.hdl", {
        "c1": ("1010101010101010", None),
        "c2": ("01.0.1.0.1.0.1.0", None),
        "c4": ("0..1...0...1...0", None),
        "d":  ("010.10..10..10..", None),
        "q2": ("x0...1..........", None),
        "x1": ("0.10..10..10..10", None),
        "y":  ("1.01..01..01..01", None),
    }),
}

# name -> (hdl file, expected stderr fragment)
EXPECTED_ERRORS = {
    "bad_loop":  ("examples/bad_loop.hdl", "combinational loop"),
    "bad_twice": ("examples/bad_twice.hdl", "already assigned"),
    "bad_clk":   ("examples/bad_clk.hdl", "power of two"),
    "bad_cp":    ("examples/bad_cp.hdl", "expression of clocks"),
}

failures = []


def check(name, cond, msg):
    if not cond:
        failures.append(f"{name}: {msg}")
        print(f"  FAIL {msg}")
    else:
        print(f"  ok   {msg}")


def sig_dict(js):
    return {s["name"]: s for s in js["signal"] if "name" in s}


def main():
    print("== simulation results")
    outdir = ROOT / "out"
    outdir.mkdir(exist_ok=True)
    for name, (hdl, expect) in EXPECTED.items():
        print(f"[{name}] {hdl}")
        r = subprocess.run([str(MINISIM), str(ROOT / hdl)],
                           capture_output=True, text=True, timeout=20)
        check(name, r.returncode == 0, f"exit code 0 (stderr: {r.stderr.strip()[:120]})")
        if r.returncode != 0:
            continue
        js = json.loads(r.stdout)
        (outdir / f"{name}.json").write_text(r.stdout)
        sigs = sig_dict(js)
        check(name, set(sigs) == set(expect),
              f"signal set {sorted(sigs)} == {sorted(expect)}")
        for signame, (wave, data) in expect.items():
            if signame not in sigs:
                continue
            got = sigs[signame]
            check(name, got.get("wave") == wave,
                  f"{signame}.wave {got.get('wave')!r} == {wave!r}")
            if data is not None:
                check(name, got.get("data") == data,
                      f"{signame}.data {got.get('data')} == {data}")

    print("== error cases")
    for name, (hdl, frag) in EXPECTED_ERRORS.items():
        r = subprocess.run([str(MINISIM), str(ROOT / hdl)],
                           capture_output=True, text=True, timeout=20)
        check(name, r.returncode != 0, "non-zero exit")
        check(name, frag in r.stderr, f"stderr contains {frag!r} (got: {r.stderr.strip()[:120]})")

    print("== SVG rendering")
    for name in EXPECTED:
        svg_file = outdir / f"{name}.svg"
        r = subprocess.run([sys.executable, str(SVG), str(outdir / f"{name}.json"),
                            "-o", str(svg_file)], capture_output=True, text=True)
        check(name, r.returncode == 0, f"renderer exit 0 ({r.stderr.strip()[:120]})")
        if r.returncode == 0:
            tree = ET.parse(svg_file)   # raises on malformed XML
            ns = "{http://www.w3.org/2000/svg}"
            n_sig = len(EXPECTED[name][1])
            texts = [e.text or "" for e in tree.iter(ns + "text")]
            check(name, any(t.isdigit() for t in texts), "has tick numbers")
            check(name, len(list(tree.iter(ns + "path"))) > 0, "has wave paths")

    print()
    if failures:
        print(f"{len(failures)} FAILURE(S)")
        sys.exit(1)
    print("all checks passed")


if __name__ == "__main__":
    main()
