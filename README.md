# minisim

A tiny HDL simulator: it parses a small Verilog-flavoured HDL (with parsec),
simulates it for a number of timestamps and emits a
[WaveDrom](https://wavedrom.com/) JSON timing diagram. A Python script renders
that JSON to SVG for visual verification.

```
            parsec                elaboration             simulation
sample.hdl ────────► AST ───────► IExpr driver graph ────────► values ───► WaveDrom JSON ───► SVG
```

## Quick start

```
make             # build with cabal (needs `cabal update` once) and copy to ./minisim
make ghc         # offline fallback: plain ghc (parsec/mtl ship with GHC)
./minisim sample.txt              # WaveDrom JSON to stdout
cabal run minisim -- sample.txt   # same, without copying the binary
./minisim --text sample.txt       # ASCII table (handy for debugging)
python3 wavedrom2svg.py out/sample.json -o out/sample.svg
make test       # all tests: HUnit unit tests, renderer unit tests, end-to-end regressions
make svg        # render all examples to out/*.svg and out/*.png
```

## Language

Statements live one per line (`;` chains statements on a line), `#` starts a
comment, component bodies are indented (Python-style).

```c
# clocks: divided from the implicit clock by a power of two
clk c1 1          # same frequency as the implicit clock
clk c2 2          # half frequency

sim 12            # optional: simulate timestamps 1..12 (see "Length")

wire w1           # 1-bit wire
wire w2[10]       # 10-bit wire (like a C array of bits)
wire w3, w4, w5[5]
wire w6 = w1      # declare + assign

assign w1 = 1010000           # bit-sequence: 1 at t1, 0 at t2, 1 at t3, ...
assign w2 = 0x11, 13, 0x5     # per-timestamp constants, 0-padded at the end
assign w3 = c1 ? w1 & w2[0] : !00010   # C-like expression
assign w5[1] = w3             # bit select; each bit is assigned at most once

def and(A, B) -> Y: Y = A & B          # one-line component
def or(A[2]) -> Y:                     # multi-line body (indented)
	return A[0] | A[1]
wire o = or(and_A, B=and_B)            # positional or port=expr arguments

# built-in, cannot be redefined:
wire q = dff(D, CP)   # flip-flop: Q(t+1)=D(t) on a rising CP edge, else hold; initial x
wire r = latch(D, E)  # transparent latch: E=1 -> Q=D, else hold; initial x
```

### Expressions

C-like precedence `?:` < `|` < `^` < `&` < `! ~` < primaries, plus
parentheses, `w[i]` bit select and component calls. Operands are clocks,
wires, constants (`13`, `0x11`) and bit-sequences.

* **Bit-sequence literal**: two or more digits of `0`/`1` only (`1010000`) --
  the n-th digit is the value at timestamp n, padded with `0` afterwards.
* **Constant**: anything else, including single `0`/`1` and hex `0x..`.
  Constants adapt to the width of their context (zero-extended / truncated).
* Binary operators work bitwise; the narrower operand is zero-extended to the
  wider one. `!` (logical not) yields 1 bit, `~` (bitwise not) keeps the width.
* Assignments require matching widths (except constants, which adapt).
* `x` (unknown) originates only from dff/latch initial state and never-assigned
  bits; it propagates Verilog-style (`0 & x = 0`, `1 | x = 1`, otherwise `x`).

### Clocks and time

Timestamps run `t = 1..T`. The implicit clock is `1` at `t = k*2+1` and `0` at
`t = k*2`. A `clk cN m` clock (m a power of two) is `1` when `(t / m)` is odd
(period `2m`). Clocks may be used like any 1-bit signal in expressions, and
are visible inside component bodies.

### Length (T)

`T` is the length of the longest bit-sequence / value-list literal in the
program; `sim N` fixes it explicitly instead (a literal longer than `N` is an
error). Without either the length is unknown and the program is rejected.

### Simulation algorithm (simulation.md)

For every timestamp `t`:

1. Compute all dff outputs first. This is valid because a dff only needs the
   input values of the previous timestamp (its CP input is restricted to
   clocks/expressions of clocks, whose values are pure functions of `t`; the
   elaborator enforces this).
2. Compute every remaining wire in dependency order -- the graph is split into
   trees rooted at dff outputs; a wire can be calculated once all predecessors
   in every tree are (implemented as memoized depth-first evaluation).
3. A wire that can never be calculated (e.g. `wire w1=~w2; wire w2=~w1`) is a
   combinational loop and reported as an error, e.g.
   `combinational loop: w1 -> w2 -> w1`.

A **latch** is transparent (`E=1 -> Q=D` at the same timestamp, per the
spec in `sample.txt`), so it is evaluated as part of step 2, reading only its
own previous state; this differs cosmetically from simulation.md's step 1
wording ("only needs the input value of the last timestamp"), which describes
dff exactly. If you prefer the delayed variant, it is a one-line change in
`Minisim.Sim` (`ILatch` case).

### Components

`def name(port[width], ...) -> out:` followed by an inline
`out = expr` (or `return expr`) or an indented body with a single result
statement. Components are single-output; instantiation inlines the body with
argument expressions substituted for ports (widths must match; clocks and
constants are visible inside bodies, wires are not -- pass them as ports).
Recursive components are rejected. `dff`/`latch` are reserved built-ins
available everywhere, one state element per instantiation site (also inside
components, e.g. `def stage(D) -> Q: return dff(D, c1)`).

## Adjustments to the original sample syntax

* `def dff(...)` / `def latch(...)` header lines in `sample.txt` are
  documentation of built-ins and are commented out (`dff`/`latch` are reserved
  words that cannot be redefined).
* Added the `sim N` statement to set the simulation length explicitly.
* Single `0`/`1` are constants; only 2+ digit `0`/`1` runs are waveforms
  (otherwise `wire a = 1` would silently mean "one timestamp long").
* Bit-select indices are constants; unassigned bits simulate as `x` (a warning
  is printed).

## Output

WaveDrom JSON: 1-bit signals as `0/1/x` wave strings (`.` = unchanged);
multi-bit signals as data states (`2` + `data` in hex; a hex digit whose bits
contain an unknown is rendered `x`). Signals appear in declaration order,
clocks first. `wavedrom2svg.py` renders this JSON (plus a little more of the
WaveDrom grammar: `p P z h l = |`, `head.text`, `foot.text`) to a standalone
SVG using only the Python standard library; ticks are numbered from 1 to match
the simulator's timestamps.

## CI & releases (GitHub Actions)

`.github/workflows/ci.yml` runs on every push and pull request:

- **test** — `cabal check`, `cabal build`, the HUnit unit tests (`cabal test`),
  the Python renderer unit tests and the end-to-end regression checks
  (`tests/check.py`).
- **binaries** — after **test** passes, builds stripped standalone binaries
  on a matrix: `linux-x86_64` (built on `ubuntu-22.04`, so it runs on any
  glibc >= 2.35 distro; GHC bundles everything else) and `windows-x86_64`.
  Each binary is smoke-tested, then packaged as a tar.gz/zip together with
  `README.md`, `wavedrom2svg.py` and `examples/`, and uploaded as a workflow
  artifact.

Pushing a version tag creates a GitHub release with both archives attached:

```
git tag v0.1.0
git push origin v0.1.0     # -> release "minisim v0.1.0" with the two archives
```

## Layout

```
app/Main.hs             CLI, --text debug dump
src/Minisim/Ast.hs      AST + Bit type
src/Minisim/Parser.hs   parsec parser (line/indentation aware)
src/Minisim/Elab.hs     name/width checks, component inlining, sim length
src/Minisim/Sim.hs      the simulation loop (x propagation, loop detection)
src/Minisim/WaveDrom.hs WaveDrom JSON emission
wavedrom2svg.py         JSON -> SVG renderer (stdlib only)
tests/check.py          end-to-end regression tests with hand-computed expectations
tests/test_svg.py       unit tests for the renderer (unittest)
test/                   Haskell unit tests (HUnit, via `cabal test`)
examples/*.hdl          samples incl. intentional error cases (bad_*.hdl)
cabal.project           cabal project (make ghc needs no index)
```
