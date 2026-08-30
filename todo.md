- [x] Write a HDL parser using parsec
      (src/Minisim/Parser.hs - line oriented, `;` chains, indented def bodies)
- [x] Implement the simulation algorithm
      (src/Minisim/Elab.hs + src/Minisim/Sim.hs - dff-first + topological
      evaluation, combinational-loop detection, x propagation)
- [x] Write a backend that generate WaveDrom json
      (src/Minisim/WaveDrom.hs - `minisim input.hdl` prints the JSON)
- [x] Verify the result by writing a python script to transform WaveDrom to SVG
      (wavedrom2svg.py; tests/check.py checks simulated waves against
      hand-computed expectations and renders/validates the SVGs)
