# minisim

> This is (once again) a vibe-coded project. This remark will be deleted once I have enough time to maintain it.

A tiny HDL simulator: it parses a small Verilog-flavoured HDL (with parsec),
simulates it for a number of timestamps and emits a
[WaveDrom](https://wavedrom.com/) JSON timing diagram. A Python script renders
that JSON to SVG for visual verification.

```
            parsec                elaboration             simulation
sample.hdl ────────► AST ───────► IExpr driver graph ────────► values ───► WaveDrom JSON ───► SVG
                      │
                      └─ --diagram ─► HDElk circuit-diagram JSON (see "Circuit diagrams")
```

## Quick start

```
make             # build with cabal (needs `cabal update` once) and copy to ./minisim
make ghc         # offline fallback: plain ghc (parsec/mtl ship with GHC)
./minisim sample.txt              # WaveDrom JSON to stdout
cabal run minisim -- sample.txt   # same, without copying the binary
./minisim --text sample.txt       # ASCII table (handy for debugging)
./minisim --diagram sample.txt    # HDElk circuit-diagram JSON (see below)
./minisim a.hdl b.hdl            # several files: concatenated in order, simulated as one
python3 wavedrom2svg.py out/sample.json -o out/sample.svg
make test       # unit tests (cabal test) + binary smoke test
make svg        # render all examples to out/*.svg and out/*.png
make diagrams   # HDElk circuit-diagram JSON for all examples, to out/*.hdelk.json
                                 # (+ out/*.hdelk.svg when the hdelk tools are set up)
make hdelk-tools  # once: fetch hdelk from GitHub + npm-install jsdom (gitignored tools/)
node hdelk2svg.js out/sample.hdelk.json -o out/sample.hdelk.svg   # one diagram to SVG
```

## Multiple input files

`./minisim a.hdl b.hdl ...` concatenates the given files (in command-line
order, each terminated by a newline) and simulates the result as one
program — handy for keeping components in a library file, e.g.
`./minisim examples/multi_top.hdl examples/multi_lib.hdl`.
Parse errors are reported against the file (and line within it) the
offending text came from; other errors carry the file list as a prefix.

## Language

Statements live one per line (`;` chains statements on a line), `#` starts a
comment, component bodies are indented (Python-style).

```python
# clocks: divided from the implicit clock by a power of two
clk c1 1          # same frequency as the implicit clock
clk c2 2          # half frequency

sim 12            # optional: simulate timestamps 1..12 (see "Length")

wire w1           # 1-bit wire
wire w2[10]       # 10-bit wire (like a C array of bits)
wire w3, w4, w5[5]
wire w6 = w1      # declare + assign
wire notrace h    # 'notrace': simulated, but hidden from the waveform

const table[16] = 12345    # a constant signal, folded at elaboration time
const notrace mask = 0xff  # width inferred from the value when [n] is omitted

assign w1 = 1010000           # bit-sequence: 1 at t1, 0 at t2, 1 at t3, ...
assign w2 = 0x11, 13, 0x5     # per-timestamp constants, 0-padded at the end
assign w3 = c1 ? w1 & w2[0] : !00010   # C-like expression
assign w5[1] = w3             # bit select; each bit is assigned at most once
wire cat[4] = {w1, w2[0], 1, table[3]}  # concatenation, leftmost = MSB
wire y = table[cat]           # index with a signal: a mux over the bits

def and(A, B) -> Y: Y = A & B          # one-line component
def or(A[2]) -> Y:                     # multi-line body (indented)
	return A[0] | A[1]
wire o = or(and_A, B=and_B)            # positional or port=expr arguments

def c1(a, b) -> c, d, e:               # several outputs: each is assigned once
	c = a & b                           # a call yields {c, d, e} (c is the MSB)
	d = a | b
	e = a ^ b
wire out[3] = c1(w1, w2)

# built-in, cannot be redefined:
wire q = dff(D, CP)   # flip-flop: Q(t+1)=D(t) on a rising CP edge, else hold; initial x
wire r = latch(D, E)  # transparent latch: E=1 -> Q=D, else hold; initial x
```

### Expressions

C-like precedence `?:` < `|` < `^` < `&` < `! ~` < primaries, plus
parentheses, `w[i]` bit select, `{a, b, c}` concatenation and component
calls. Operands are clocks, wires, constants (`13`, `0x11`), parameters and
bit-sequences.

* **Bit-sequence literal**: two or more digits of `0`/`1` only (`1010000`) --
  the n-th digit is the value at timestamp n, padded with `0` afterwards.
* **Constant**: anything else, including single `0`/`1` and hex `0x..`.
  Constants adapt to the width of their context: they are zero-extended when
  the context is wider, but a constant that does not fit the required width
  (e.g. `assign w5[2] = 2`, where the target is a single bit) is an error --
  values are never silently truncated.
* **Concatenation** `{a, b[2], 1}`: the width is the sum of the parts, the
  leftmost element becomes the most significant bits.
* **Bit select** `w[i]`: a constant index is checked against the width at
  elaboration. `w[k]` with a signal `k` selects the bit whose number is the
  value of `k` at each timestamp (a mux); an unknown index, or one out of
  range, yields `x`.
* Binary operators work bitwise; the narrower operand is zero-extended to the
  wider one. `!` (logical not) yields 1 bit, `~` (bitwise not) keeps the width.
* Assignments require matching widths (except constants, which adapt).
* `x` (unknown) originates only from dff/latch initial state and never-assigned
  bits; it propagates Verilog-style (`0 & x = 0`, `1 | x = 1`, otherwise `x`).
  It cannot be written by the user.

### Clocks and time

Timestamps run `t = 1..T`. The implicit clock is `1` at `t = k*2+1` and `0` at
`t = k*2`. A `clk cN m` clock (m a power of two) is `1` while `((t-1) / m)` is
even (period `2m`): high for timestamps `1..m`, low for `m+1..2m`, and so on —
its rising edges align with the implicit clock's, so every clock starts at
`1`. Clocks may be used like any 1-bit signal in expressions, and
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

`def name<P1, P2>(port[width], ...) -> out1, out2[width]:` followed by an
inline `out = expr` or `return expr` body (single-output components only) or
an indented body. The optional `<>` list names Verilog-like **parameters**
(elaboration-time integers, given at the instantiation site); they are usable
as constants and as widths inside the body. A body may also declare

* local **wires** (`wire key[4] = {D,C,B,A}`),
* local **constants** (`const notrace table[16] = Num`),
* **named instances** of other components (`Lut<12345> l1, l2`).

A component has one or more **output ports** (`-> c, d, e:`). Every output is
assigned exactly once (`c = expr`); an omitted width is inferred from the
assigned expression, an explicit `out[n]` (constants adapt to it) must match.
`return expr` is the short form for single-output components. A call
evaluates to the concatenation of all outputs in declaration order (first
output = MSB, like `{c, d, e}`), so `wire q[3] = c1(a, b)` gives
c = `q[2]`, d = `q[1]`, e = `q[0]`; grab individual outputs with bit selects,
or route them through local wires (outputs cannot be read inside the body).
Each instantiation binds arguments to ports (widths must match; clocks and
constants are visible inside bodies, wires are not -- pass them as ports).
Recursive components are rejected. `dff`/`latch` are reserved built-ins
available everywhere, one state element per instantiation site (also inside
components, e.g. `def stage(D) -> Q: return dff(D, c1)`).

### Instantiation and hierarchy

A component can be instantiated with or without a name:

```c
Lut<12345> l1, l2          # statement: declares named instances
def Lut<Num>(A,B,C,D) -> Y: ...
wire out1 = l1(in1,in2,in3,in4)   # use a named instance (exactly once)
wire out4 = Lut<23456>(...)       # anonymous: a fresh instance per call
```

Each named instantiation is one physical component and can be used exactly
once (using it twice is an error); anonymous calls make a fresh instance
every time (`Lut$1`, `Lut$2`, ...). Local signals are hoisted into the design
under hierarchical names and appear that way in the waveform: the wire `key`
inside instance `l1` is `l1.key`; an instance `s2` used inside a component
instantiated as `s1` gives `s1.s2.wire1`. Declaring a wire `notrace` (or a
`const notrace`) hides it from the waveform while keeping it in the design;
declaring a whole component `notrace` (`def notrace c1(w1) -> w2:`) hides
every internal signal of each of its instances — local wires, consts and the
internals of nested instances — wherever they are routed (its outputs are
expressions and appear as usual at the call site). Hidden signals are still
simulated and keep their history, they are just not rendered.
See `examples/lut.hdl` for all of this in one place,
`examples/multiout.hdl` for multi-output components (half/full adders) and
`examples/notrace.hdl` for module-level `notrace`.

## Adjustments to the original sample syntax

* `def dff(...)` / `def latch(...)` header lines in `sample.txt` are
  documentation of built-ins and are commented out (`dff`/`latch` are reserved
  words that cannot be redefined).
* Added the `sim N` statement to set the simulation length explicitly.
* Single `0`/`1` are constants; only 2+ digit `0`/`1` runs are waveforms
  (otherwise `wire a = 1` would silently mean "one timestamp long").
* Fixed the `assign w5[2] = 2` typo in `sample.txt` (`2` does not fit a single
  bit; it is now `= 1`).
* Bit-select indices are constants; unassigned bits simulate as `x` (a warning
  is printed).
* `examples/lut.hdl` extends the language with parameters (`<>`), local
  wires/consts in bodies, `notrace`, `{}` concatenation, signal indexing and
  named instances (see "Instantiation and hierarchy" above).

## Output

WaveDrom JSON: 1-bit signals as `0/1/x` wave strings (`.` = unchanged);
multi-bit signals as data states (`2` + `data` in hex; a hex digit whose bits
contain an unknown is rendered `x`). Signals appear in declaration order,
clocks first. `wavedrom2svg.py` renders this JSON to a standalone SVG using the
[`wavedrom`](https://pypi.org/project/wavedrom/) Python package (so the full
WaveDrom grammar works, not just minisim's subset); ticks are numbered from 1
to match the simulator's timestamps.

## Circuit diagrams (`--diagram`)

`minisim --diagram input.hdl` skips the simulation and emits an
[HDElk](https://davidthings.github.io/hdelk/) JSON graph of the circuit's
*structure* instead (a `--diagram` variant of the multi-input form works too;
`--text` and `--diagram` are mutually exclusive). The mapping is:

* `assign a = b&c|d` (or `wire a = ...`) is **one component**: a node with
  inputs `b, c, d` (one port per referenced signal, in textual order),
  output `a`, and the assigned expression as its `type`. `assign w[i] = e`
  drives a node named `w[i]`.
* wires -- clocks, bit-sequence / value-list drivers and constant-driven
  wires -- are drawn as input pins and (notched) constant nodes and carry
  no `type`; literals used inside expressions appear as shared constant
  nodes. Only components (expressions, `dff`/`latch`, instances) and `const`
  declarations carry a `type`.
* `dff`/`latch` calls are leaf nodes (`D`/`CP` or `D`/`E` in, `Q` out).
* each instantiation of a user-defined component is one node carrying the
  component's ports (and `Name=value` parameters); its internals — the
  body's gates, constants and nested instances — are drawn as children,
  recursively, **unless** the component is declared `def notrace`, in which
  case it stays a black box. Clocks and top-level constants visible inside a
  body are re-drawn as pins within each expanded instance (HDElk/ELK edges
  cannot cross hierarchy levels).
* multi-bit connections are drawn as bus edges; a whole reference to a
  bit-driven wire (`assign pair[0] = ...; ... or(pair)`) fans in from every
  bit's node.

Diagram mode only parses and resolves names, so it needs no simulation
length (no bit-sequence literal or `sim N` required), while still reporting
unknown names, unknown components and recursive definitions. Structural
views of programs the simulator would reject (combinational loops, twice-used
instances) are drawn anyway.

### Rendering diagram JSON to SVG

`hdelk2svg.js` renders the JSON with HDElk's own sources
(`hdelk.js` + `elk.bundled.js` + `svg.min.js` from
[github.com/davidthings/hdelk](https://github.com/davidthings/hdelk)), loaded
into a [jsdom](https://github.com/jsdom/jsdom) window — the same trick
HDElk's own headless smoke test uses (current `hdelk.js` also gets two small
in-memory fixes there: its font sizes are unitless CSS values that every
engine drops, and its port-label inset ignores svg.js's line spacing — both
still correct in the browser renders the hdelk project ships). Nothing of
hdelk's is committed here:

```
make hdelk-tools     # once: shallow-clones hdelk to tools/hdelk and
                     # npm-installs jsdom to tools/node_modules (both gitignored)
node hdelk2svg.js out/shift.hdelk.json -o out/shift.hdelk.svg
cat out/shift.hdelk.json | node hdelk2svg.js > out/shift.hdelk.svg
make diagrams        # JSON for all examples; also renders *.hdelk.svg when the
                     # tools are set up (skips SVG rendering otherwise)
```

`HDELK_HOME` / `HDELK_JSDOM` point the script at an existing hdelk checkout
or jsdom module; jsdom has no font engine, so text widths are approximated
and label spacing differs slightly from a browser rendering (paste the JSON
into the HDElk [editor](https://davidthings.github.io/hdelk/editor) for a
pixel-perfect view).

## CI & releases (GitHub Actions)

`.github/workflows/ci.yml` runs on every push and pull request:

- **test** — `cabal check`, `cabal build`, the HUnit unit tests (`cabal test`)
  and a smoke run of the built binary on `sample.txt`.
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
app/Main.hs             CLI, --text debug dump, --diagram circuit diagrams
src/Minisim/Ast.hs      AST + Bit type
src/Minisim/Parser.hs   parsec parser (line/indentation aware)
src/Minisim/Elab.hs     name/width checks, component inlining, sim length
src/Minisim/Sim.hs      the simulation loop (x propagation, loop detection)
src/Minisim/WaveDrom.hs WaveDrom JSON emission
src/Minisim/Diagram.hs  HDElk circuit-diagram JSON emission (--diagram)
wavedrom2svg.py         JSON -> SVG renderer (needs the `wavedrom` pip/pkg package)
hdelk2svg.js            HDElk-diagram JSON -> SVG, using hdelk's own sources
                        (needs node + jsdom; run `make hdelk-tools` once)
tools/setup.sh          one-time fetch of hdelk + jsdom (gitignored tools/)
test/                   Haskell unit tests (HUnit, via `cabal test`)
examples/*.hdl          samples incl. intentional error cases (bad_*.hdl)
cabal.project           cabal project (make ghc needs no index)
```
