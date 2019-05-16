# dung-k-revision

This repository provides type definitions and basic implementations to implement
AGM-style expansion and revision for argumentation frameworks and Dung-Logics.
The basic concepts and definitions of this repository can be found in
[*Baumann, Brewka, 2015*](https://www.researchgate.net/publication/323704530_AGM_meets_Abstract_Argumentation_Expansion_and_Revision_for_Dung_Frameworks).

The basic interface for defining custom expansion and revision operators can be found in `src/Language/Dung/Logics.hs`.
An example implementation for stable semantics can be found in `src/Language/Dung/Logics/Stb.hs`.

## Try it out as a REPL

If you execute `stack run` in this repositry, a REPL for AFs will open.
Here is an example for how to use it:

```
:af F := 1 ; 2 ; 2
AF [0,1,2] [(0,1),(1,2),(2,2)]
:af G := ; 2 ; 1 3
AF [1,2,3] [(1,2),(2,1),(2,3)]
:rev H := F G
AF [1,2,3,0] [(1,2),(2,1),(2,3),(0,1)]
:rev H := G F
AF [0,1,2,3] [(0,1),(1,2),(2,2)]
```

You can describe AFs like adjacency matrices.
`;` delimits indices and for each index you state which arguments are attacked by it.
`1 ; 2 ; 2` means that the AF has the attacks `(0,1)`, `(1,2)` and `(2, 2)`.

You can enter `:h` or `:help` for a full list of commands.

## Development

You can play around with the library by running `stack ghci`.

If you want to have a look at the documentation, run `stack haddock`.

## References

```
Baumann, Ringo & Brewka, Gerhard. (2015).
AGM meets Abstract Argumentation: Expansion and Revision for Dung Frameworks.
```
