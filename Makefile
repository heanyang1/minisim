# minisim - build & test
#
# Default build uses cabal (run `cabal update` once; all dependencies are
# GHC boot libraries, so nothing is downloaded afterwards).
# `make ghc` is an offline fallback that builds with plain ghc.

SRC := $(wildcard src/Minisim/*.hs) app/Main.hs

.PHONY: all test svg ghc clean

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
	for j in out/*.json; do \
	  python3 wavedrom2svg.py $$j -o $${j%.json}.svg && \
	  rsvg-convert -w 1400 $${j%.json}.svg -o $${j%.json}.png; \
	done

clean:
	rm -rf build dist-newstyle minisim out
