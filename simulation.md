# Simulation process

- The CP port of dff can only be a clock or expression of clocks.
- We split the graph into trees whose root are the D port of dff. e.g.
  ```
  clk c1 1
  wire w1
  wire w2=dff(w1,c1) # dff1
  wire w3=dff(~w2,c1) # dff2
  assign w1 = w2|w3
  ```
  becomes
  ```
  w1 -dff1-> w2 -> not_w2
             |
             w1

  not_w2 -dff2-> w3 -> w1
  ```
- For each timestamp `t`:
  1. We start calculate the output of dff or latch. It's valid since dff or latch only needs the input value of the last timestamp.
  2. The value of a wire can be calculated once its predecessors in every tree are calculated.
  3. Raise an error if it's impossible to calculate every variable (e.g. a loop `wire w1=~w2;wire w2=~w1`)