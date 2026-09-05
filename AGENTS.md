# minisim

A tiny HDL simulator in Haskell (Cabal, GHC 9.10.x in CI): parses a small
Verilog-flavoured HDL, simulates it, emits WaveDrom JSON. `wavedrom2svg.py`
renders that JSON to SVG. All deps are GHC boot libraries (parsec, containers, mtl).

## Commands

- `make` — build with cabal, copy binary to `./minisim` (needs `cabal update` once)
- `make ghc` — offline fallback build with plain ghc
- `make test` — HUnit tests (`cabal test`) + binary smoke test; or run `cabal test` directly
- `make svg` — render all examples to `out/*.svg`/`*.png` (needs python3, rsvg-convert)
- `make diagrams` — `--diagram` HDElk JSON for all examples, to `out/*.hdelk.json` (+ `.svg` when hdelk tools are set up)
- `make hdelk-tools` — one-time: shallow-clone hdelk from GitHub into `tools/hdelk`, npm-install jsdom into `tools/node_modules` (gitignored; hdelk code is never committed)
- `node hdelk2svg.js in.json [-o out.svg]` — render HDElk diagram JSON to SVG via hdelk's own sources
- `cabal run minisim -- [--text|--diagram] [-o out.json] input.hdl [more.hdl ...]` — run without copying; `--text` gives an ASCII debug table, `--diagram` an HDElk circuit-diagram JSON; several inputs are concatenated (in order) and simulated as one program
- `make clean` — remove build artifacts (`dist-newstyle/`, `build/`, `out/`, `./minisim`)

## Architecture

Pipeline: `Minisim.Parser` (parsec) → `Minisim.Ast` → `Minisim.Elab` (elaboration
to a flat `IExpr` driver graph, constants folded) → `Minisim.Sim` (per-timestamp
event simulation; dff outputs first, then combinational wires via memoized DFS;
combinational loops are errors) → `Minisim.WaveDrom` (JSON). CLI in `app/Main.hs`.
`--diagram` instead goes AST → `Minisim.Diagram` (HDElk JSON circuit diagram,
built from the AST alone — no simulation length needed; `def notrace`
components stay black boxes).

- `src/Minisim/` — library modules; `test/` — HUnit suite (TestMain + one file per module)
- `examples/*.hdl` — sample programs; `bad_*.hdl` are intentionally invalid (error-message checks); `multi_top.hdl` + `multi_lib.hdl` are a multi-input pair
- `README.md` — authoritative language spec (literals, x-propagation, clocks, components)
- `.github/workflows/ci.yml` — CI does `cabal check`, build, test, smoke test, release binaries

## Conventions

- Built with `-Wall`; errors are `Either String` with user-facing messages.
- New test modules must be registered under `other-modules` in `minisim.cabal`.
- `out/`, `build/`, `dist-newstyle/`, and the `./minisim` binary are generated and gitignored.
- `sim N` length and semantics details live in README.md — check it before changing parser/elab behaviour.
