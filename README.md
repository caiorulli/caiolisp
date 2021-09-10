# Caiolisp

[![Build Status](https://cloud.drone.io/api/badges/caiorulli/caiolisp/status.svg?ref=refs/heads/deck)](https://cloud.drone.io/caiorulli/caiolisp)

> "Precept Forty-Seven: 'Make Your Own Weapon'. Only you know exactly what is needed in your weapon. I myself fashioned 'Life Ender' from shellwood at a young age. It has never failed me. Nor I it."
>
> - Zote the Mighty

Caiolisp is a programming language I have been writing in my own time. It's mostly for study purposes. Feel free to play around with it!

It is a simple Lisp based off the [Structure and Interpretation of Computer Programs](https://github.com/sarabander/sicp) book. The idea here is to explore the design space for functional programming using Lisp structures, first by trying to build a minimum core, then making it purely functional with your typical monad mischief, and then, who knows? Total functional programming? Dependent types? One can dream.

It's also an excuse to try and learn proper Haskell.

## How to play around with it

What I would recommend is to [install cabal through ghcup](https://www.haskell.org/ghcup/), and then run:

```sh
cabal run caiolisp-exe
```

If you'd like to run the tests:

```sh
cabal test --test-show-details=direct --test-option=--format=progress
```

## Currently implemented features

- Plus and minus
- Conditionals with `if`
- Variable and function definition, with `def`, `fn` and `defn`

### Examples

```
(+ 1 1)
> 1
(+ 3 (- 2 1))
> 2
(= 3 2)
> false
(if (= 2 2) 1 3)
> 1
(def conquest 42)
> 42
(defn inc (x) (+ x 1))
> <Fn>
(inc conquest)
> 43
```
