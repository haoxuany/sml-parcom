# CMLib (subset)

This directory contains a minimal subset of
[CMLib](https://github.com/standardml/cmlib), the Carnegie Mellon SML
utility library by Karl Crary et al.

Only the modules needed by sml-parcom are included:

- `ORDERED`, `IntOrdered`, `StringOrdered`, etc. — ordered type classes
- `SplayDict`, `SplaySet` — splay-tree-based dictionaries and sets
- `Quicksort` — stable quicksort
- `Stream`, `Susp` — lazy streams and suspensions
- `Bytestring`, `FromString` — supporting modules for `Stream`

The full library is available at https://github.com/standardml/cmlib.

CMLib is distributed under the MIT license; see [LICENSE](LICENSE) and
[AUTHORS](AUTHORS).
