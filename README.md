This is a system for software development called Aspectation. 
It has been implemented following a programming paradigm described in Multi-View Programming Integration Paradigm (MVPI) (https://github.com/jinnzest/multi-view_programming_integration).

Aspectation MVPI is dual-licensed under Apache 2.0 and MIT. 
See LICENSE-APACHE and LICENSE-MIT for details.

# Setting up a developer environment:

1. install GHCup: https://www.haskell.org/ghcup/
2. install Stack using GHCup
3. install Ormolu formatter: https://hackage.haskell.org/package/ormolu
4. install hlint: https://hackage.haskell.org/package/hlint

# Publishing

1. `scripts/format.sh`
2. `stack test`
4. `git push origin branch-name:branch-name`

# Running syntax parser of the main language

stack run `path-to-source-file`
where file_name - path to a file containing source code of the main language
example: `stack run project/snippets.astn`

To print coordinates of each item add `A_LOC=True` at the beginning
example: `A_LOC=True stack run project/snippets.astn`