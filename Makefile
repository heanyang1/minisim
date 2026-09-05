# minisim - build & test
#
# Default build uses cabal (run `cabal update` once; all dependencies are
# GHC boot libraries, so nothing is downloaded afterwards).
# `make ghc` is an offline fallback that builds with plain ghc.

SRC := $(wildcard src/Minisim/*.hs) app/Main.hs

.PHONY: all test svg diagrams hdelk-tools ghc clean

all: minisim

minisim: $(SRC) minisim.cabal cabal.project
	cabal build
	cp "$$(cabal list-bin minisim)" minisim

# offline fallback (parsec/mtl/containers ship with GHC)
ghc:
	ghc -O -isrc -iapp -outputdir build -o minisim \
	    -package parsec -package mtl -package containers app/Main.hs

test: minisim
	cabal test                                 # HUnit unit tests
	cabal run minisim -- --text sample.txt > /dev/null   # binary smoke test

# render all examples to out/*.json / out/*.svg / out/*.png
svg: minisim
	mkdir -p out
	./minisim sample.txt           -o out/sample.json 2>/dev/null
	./minisim examples/shift.hdl   -o out/shift.json  2>/dev/null
	./minisim examples/latch.hdl   -o out/latch.json  2>/dev/null
	./minisim examples/clocks.hdl  -o out/clocks.json 2>/dev/null
	./minisim examples/lut.hdl     -o out/lut.json    2>/dev/null
	./minisim examples/notrace.hdl -o out/notrace.json 2>/dev/null
	./minisim examples/multi_top.hdl examples/multi_lib.hdl -o out/multi.json 2>/dev/null
	for j in out/*.json; do \
	  python3 wavedrom2svg.py $$j -o $${j%.json}.svg && \
	  rsvg-convert -w 1400 $${j%.json}.svg -o $${j%.json}.png; \
	done

# HDElk circuit-diagram JSON for every example, to out/*.hdelk.json
# (render these with https://davidthings.github.io/hdelk/ ; there is no
# offline renderer)
diagrams: minisim
	mkdir -p out
	./minisim --diagram sample.txt -o out/sample.hdelk.json 2>/dev/null
	for h in examples/*.hdl; do \
	  case $$h in *bad_*) continue;; esac; \
	  ./minisim --diagram $$h -o out/$$(basename $${h%.hdl}).hdelk.json 2>/dev/null; \
	done
	./minisim --diagram examples/multi_top.hdl examples/multi_lib.hdl \
	  -o out/multi.hdelk.json 2>/dev/null
	if [ -f tools/hdelk/js/hdelk.js ] && [ -d tools/node_modules/jsdom ]; then \
	  for j in out/*.hdelk.json; do \
	    node hdelk2svg.js $$j -o $${j%.json}.svg || exit 1; \
	  done; \
	else \
	  echo 'diagrams: SVG rendering skipped (run "make hdelk-tools" once to enable)'; \
	fi

# one-time setup for hdelk2svg.js: shallow-clone hdelk from GitHub into
# tools/hdelk and npm-install jsdom into tools/node_modules (both gitignored;
# hdelk's code is never committed to this repository)
hdelk-tools:
	tools/setup.sh

clean:
	rm -rf build dist-newstyle minisim out
