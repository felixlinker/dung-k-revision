# dung-k-revision

This repository provides type definitions and basic implementations to implement
AGM-style expansion and revision for argumentation frameworks and Dung-Logics.
The basic concepts and definitions of this repository can be found in
[*Baumann, Brewka, 2015*](https://www.researchgate.net/publication/323704530_AGM_meets_Abstract_Argumentation_Expansion_and_Revision_for_Dung_Frameworks).

The basic interface for defining custom expansion and revision operators can be found in `src/Language/Dung/Logics.hs`.
An example implementation for stable semantics can be found in `src/Language/Dung/Logics/Stb.hs`.

## Development

You can play around with the library by running `stack ghci`.

If you want to have a look at the documentation, run `stack haddock`.

## References

```
Baumann, Ringo & Brewka, Gerhard. (2015).
AGM meets Abstract Argumentation: Expansion and Revision for Dung Frameworks.
```
