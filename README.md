![Monad Road](assets/images/logo.png)
#

Clone of the game [Crossy Road](https://en.wikipedia.org/wiki/Crossy_Road) in Haskell, using the [Gloss](http://gloss.ouroborus.net/) library! Developed as a project for the IT Laboratories I (Laboratórios de Informática I) class, part of the first year's curriculum of the Software Engineering degree at University of Minho.

This project obtained a final grade of **20/20** ✨

![Game](assets/images/screenshots/jogo.png)

![Editor](assets/images/screenshots/editor.png)

## Interpreter

You can open the Haskell (GHCi) interpreter using cabal or directly.

1. Using `cabal`

```bash
$ cabal repl
```

2. Using GHCi

```bash
$ ghci -i="src" -i="tests" src/Main.hs
```

## Tests

This project uses the [HUnit](https://hackage.haskell.org/package/HUnit) library to build unit tests.

You can run the tests using any of the following alternatives:

1. Using `cabal`

```bash
$ cabal test
```

2. Using GHCi

```bash
$ ghci -i="src" -i="tests" tests/Spec.hs
>>> runTestsT1 -- Run first task's tests
>>> runTestsT2 -- Run second task's tests
>>> runTestsT3 -- Run third task's tests
>>> runTestsT4 -- Run fourth task's tests
>>> runTestsT5 -- Run fifth task's tests
>>> main -- Run all tests
```

3. Using the `runhaskell` wrapper

```bash
$ runhaskell -i="src" -i="tests" tests/Spec.hs
```

## Documentation

It's possible to generate documentation files using [Haddock](https://haskell-haddock.readthedocs.io/).

1. Using `cabal`

```bash
$ cabal haddock --haddock-all
```

2. Using `haddock` directly

```bash
$ haddock -h -o doc/html src/*.hs
```

## Group

- **A104356** [João d'Araújo Dias Lobo](https://github.com/joaodiaslobo)
- **A104439** [Rita da Cunha Camacho](https://github.com/ritacamacho)
