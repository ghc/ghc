Tests the basic infrastructure used to translate Cmm control flow to WebAssembly control flow:

  - Check a Cmm control-flow graph to see if it is reducible.

  - Convert an irreducible control-flow graph to an equivalent reducible control-flow graph.

  - Interpret both Cmm control-flow graphs and WebAssembly programs using a stream of bits to determine the direction of each conditional and `switch`.  Confirm that source and target programs take the same actions and make the same decisions.

The tests dump a lot of information about the code under test, including the number of execution paths tested.

The source codes for the tested control-flow graphs are written in a mix of Haskell and Cmm; they are found in directory `src`.
