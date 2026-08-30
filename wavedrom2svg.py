#!/usr/bin/env python3
"""wavedrom2svg.py - render WaveDrom-style JSON timing diagrams to SVG.

Usage:
    python3 wavedrom2svg.py input.json [-o output.svg]
    cat diagram.json | python3 wavedrom2svg.py > diagram.svg

Supports the subset of WaveDrom produced by minisim (and a bit more):
  signal: [{name, wave, data?}, ...]
  wave characters:
      0 1 x z    - logic levels / unknown / high impedance
      h l        - weak high / low (drawn as 1 / 0)
      .          - repeat previous state
      = 2..9     - data state, consumes one entry from `data`
      p P        - clock waveform (rising / falling first), '.' continues it
      |          - gap (nothing drawn)
  head: {tick: ..., text: ...}, foot: {text: ...}

Timestamps are numbered starting at 1 (minisim convention).
Only the Python standard library is used.
"""

import argparse
import json
import sys

CW = 44          # width of one timestamp, px
LH = 40          # lane height, px
TOP = 30         # space for the tick ruler
BOT = 18         # bottom margin
LEFT = 16        # left margin
TICK_H = 16      # height of the tick ruler

FS_NAME = 14     # font sizes
FS_DATA = 13
FS_TICK = 11


def esc(s):
    return (s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;"))


def expand(sig):
    """Expand a wave string into per-timestamp states.

    Returns a list of (kind, text, fresh) where kind is one of
    '0','1','x','z','d','g','p','P', text is the data value for 'd',
    and fresh is True when this state started here (not via '.').
    """
    data = iter(sig.get("data") or [])
    out = []
    prev = None
    for ch in sig.get("wave", ""):
        if ch == ".":
            out.append((prev[0], prev[1], False) if prev else ("x", None, False))
        elif ch in "01xzhl":
            kind = {"h": "1", "l": "0"}.get(ch, ch)
            prev = (kind, None)
            out.append((kind, None, True))
        elif ch in "=23456789":
            try:
                txt = str(next(data))
            except StopIteration:
                txt = "?"
            prev = ("d", txt)
            out.append(("d", txt, True))
        elif ch in "pP":
            prev = (ch, None)
            out.append((ch, None, True))
        elif ch == "|":
            prev = ("g", None)
            out.append(("g", None, True))
        else:
            raise ValueError("unsupported wave character %r" % ch)
    return out


class Svg:
    def __init__(self, width, height):
        self.width = width
        self.height = height
        self.parts = []

    def add(self, s):
        self.parts.append(s)

    def render(self):
        return (
            '<?xml version="1.0" encoding="UTF-8"?>\n'
            '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" '
            'viewBox="0 0 %d %d">\n' % (self.width, self.height, self.width, self.height)
            + "".join(self.parts)
            + "</svg>\n"
        )


def render(diagram):
    signals = [s for s in diagram.get("signal", []) if isinstance(s, dict)]
    states = [expand(s) for s in signals]
    n = max([len(st) for st in states] + [1])

    namew = max([80] + [FS_NAME * 0.62 * len(str(s.get("name", ""))) + 22 for s in signals])

    head_text = (diagram.get("head") or {}).get("text", "")
    foot_text = (diagram.get("foot") or {}).get("text", "")
    head_h = 26 if head_text else 0
    foot_h = 26 if foot_text else 0

    W = int(LEFT + namew + n * CW + 24)
    H = TOP + head_h + len(signals) * LH + BOT + foot_h
    svg = Svg(W, H)
    svg.add('<rect x="0" y="0" width="%d" height="%d" fill="#ffffff"/>\n' % (W, H))

    # grid + ticks (minisim timestamps start at 1)
    if "head" in diagram and "tick" in (diagram.get("head") or {}):
        for t in range(n):
            x = LEFT + namew + t * CW
            svg.add('<line x1="%.1f" y1="%d" x2="%.1f" y2="%d" stroke="#dddddd" '
                    'stroke-width="1"/>\n'
                    % (x, TOP + head_h, x, TOP + head_h + len(signals) * LH))
            svg.add('<text x="%.1f" y="%d" font-family="sans-serif" font-size="%d" '
                    'fill="#777777" text-anchor="middle">%d</text>\n'
                    % (x + CW / 2, TOP + head_h - 6, FS_TICK, t + 1))

    if head_text:
        svg.add('<text x="%.1f" y="%d" font-family="sans-serif" font-size="15" '
                'text-anchor="middle">%s</text>\n'
                % (W / 2, 20, esc(str(head_text))))
    if foot_text:
        svg.add('<text x="%.1f" y="%d" font-family="sans-serif" font-size="15" '
                'text-anchor="middle">%s</text>\n'
                % (W / 2, H - 8, esc(str(foot_text))))

    for i, (sig, st) in enumerate(zip(signals, states)):
        draw_lane(svg, sig, st, LEFT, namew, TOP + head_h + i * LH, n, i)

    return svg.render()


def draw_lane(svg, sig, st, left, namew, y, n, idx):
    hi = y + LH * 0.25
    lo = y + LH * 0.75
    mid = y + LH * 0.5
    x = lambda t: left + namew + t * CW  # noqa: E731

    name = str(sig.get("name", ""))
    svg.add('<text x="%.1f" y="%.1f" font-family="sans-serif" font-size="%d" '
            'text-anchor="end">%s</text>\n' % (left + namew - 8, mid + 5, FS_NAME, esc(name)))

    # separate lane background (alternating) -- subtle
    if idx % 2 == 1:
        svg.add('<rect x="%d" y="%d" width="%d" height="%d" fill="#f7f7f7"/>\n'
                % (left, y, namew + n * CW, LH))

    t = 0
    while t < len(st):
        kind, text, _fresh = st[t]
        if kind in "01zpP":
            # collect a maximal run of drawable bit/clock states
            pts = []
            start = t
            while t < len(st) and st[t][0] in "01zpP":
                k = st[t][0]
                x0, x1, xm = x(t), x(t + 1), x(t) + CW / 2
                if k == "0":
                    pts += [(x0, lo), (x1, lo)]
                elif k == "1":
                    pts += [(x0, hi), (x1, hi)]
                elif k == "p":
                    pts += [(x0, lo), (xm, lo), (xm, hi), (x1, hi)]
                elif k == "P":
                    pts += [(x0, hi), (xm, hi), (xm, lo), (x1, lo)]
                elif k == "z":
                    if pts:
                        flush_path(svg, pts)
                        pts = []
                    svg.add('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" '
                            'stroke="#555555" stroke-width="2" stroke-dasharray="4,3"/>\n'
                            % (x0, mid, x1, mid))
                t += 1
            flush_path(svg, pts)
            _ = start
        elif kind == "x":
            t0 = t
            while t < len(st) and st[t][0] == "x":
                t += 1
            bx0, bx1 = x(t0), x(t)
            draw_xbox(svg, bx0, bx1, hi, lo)
        elif kind == "d":
            t0 = t
            while t < len(st) and st[t][0] == "d":
                if t > t0 and st[t][2]:      # fresh '2'/'=' starts a new box
                    break
                t += 1
            bx0, bx1 = x(t0), x(t)
            draw_dbox(svg, bx0, bx1, y, st[t0][1])
        else:  # gap
            t += 1


def flush_path(svg, pts):
    if len(pts) < 2:
        return
    d = "M %.1f %.1f " % pts[0] + " ".join("L %.1f %.1f" % p for p in pts[1:])
    svg.add('<path d="%s" fill="none" stroke="#000000" stroke-width="2" '
            'stroke-linejoin="miter"/>\n' % d)


def draw_xbox(svg, x0, x1, hi, lo):
    w = x1 - x0
    svg.add('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" fill="#f8d7da" '
            'stroke="#c98a90" stroke-width="1"/>\n' % (x0, hi, w, lo - hi))
    if w > 14:
        svg.add('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#cc4444" '
                'stroke-width="1.5"/>\n' % (x0 + 3, lo - 3, x1 - 3, hi + 3))
        svg.add('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#cc4444" '
                'stroke-width="1.5"/>\n' % (x0 + 3, hi + 3, x1 - 3, lo - 3))
    else:
        svg.add('<text x="%.1f" y="%.1f" font-family="sans-serif" font-size="12" '
                'fill="#cc2222" text-anchor="middle">x</text>\n' % ((x0 + x1) / 2, (hi + lo) / 2 + 4))


def draw_dbox(svg, x0, x1, y, text):
    by0 = y + LH * 0.2
    by1 = y + LH * 0.8
    h = by1 - by0
    svg.add('<path d="M %.1f %.1f L %.1f %.1f L %.1f %.1f L %.1f %.1f Z" '
            'fill="#ffffff" stroke="#000000" stroke-width="1.5"/>\n'
            % (x0 + 3, by0, x1 - 3, by0, x1 - 3, by1, x0 + 3, by1))
    # double-line hint at the left edge, like WaveDrom's data arrow
    svg.add('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#000000" '
            'stroke-width="1.5"/>\n' % (x0 + 6, by0, x0 + 6, by1))
    svg.add('<text x="%.1f" y="%.1f" font-family="sans-serif" font-size="%d" '
            'text-anchor="middle">%s</text>\n'
            % ((x0 + x1) / 2, (by0 + by1) / 2 + FS_DATA * 0.36, FS_DATA, esc(str(text))))


def main():
    ap = argparse.ArgumentParser(description="Render WaveDrom JSON to SVG")
    ap.add_argument("input", nargs="?", help="input JSON file (default: stdin)")
    ap.add_argument("-o", "--output", help="output SVG file (default: stdout)")
    args = ap.parse_args()

    src = open(args.input, "r", encoding="utf-8") if args.input else sys.stdin
    diagram = json.load(src)
    if args.input:
        src.close()

    svg = render(diagram)
    if args.output:
        with open(args.output, "w", encoding="utf-8") as f:
            f.write(svg)
    else:
        sys.stdout.write(svg)


if __name__ == "__main__":
    main()
