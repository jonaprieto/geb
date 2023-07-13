# Geb Programming Language

[![CI](https://github.com/anoma/geb/actions/workflows/ci.yml/badge.svg)](https://github.com/anoma/geb/actions/workflows/ci.yml)
 [![pages-build-deployment](https://github.com/anoma/geb/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/anoma/geb/actions/workflows/pages/pages-build-deployment)


Welcome to *Geb*, a yet powerful programming language designed with the purpose of providing a universal and unambiguous syntax for defining and translating among any other programming languages. Named in honor of Hofstadter’s iconic work, Gödel, Escher, Bach, Geb is pronounced as a single syllable, adhering to the convention of capitalizing only the first letter, despite its acronymic nature. Dive into Geb and discover an intuitive way to understand, define, and translate various programming languages.

## Who is using Geb?

Geb currently serves as one of the backends for [Juvix](https://docs.juvix.org), a high-level, strongly-typed, functional programming language designed for developing general-purpose decentralized applications on the [Anoma](https://anoma.net) blockchain. More specifically, Geb is used as an intermediate representation to compile Juvix down into [VampIR](https://github.com/anoma/vamp-ir) arithmetic circuits. These circuits are subsequently utilized within the [Anoma](https://anoma.net) blockchain through [Taiga](https://github.com/anoma/taiga).


## Documentation

You can find the latest [HTML documentation here](https://anoma.github.io/geb/) for the latest version of GEB.

## Code Coverage

For insight into our test coverage, you can check out the following:

- [SBCL test coverage](https://anoma.github.io/geb/tests/cover-index.html)
- [CCL test coverage](https://anoma.github.io/geb/tests/report.html) (currently under maintenance)

## Quickstart Guide

For those who want to quickly dive in, here's a quick rundown on how to use the GEB CLI.

### Installation

1. Install the [Roswell launcher](https://github.com/roswell/roswell/wiki/Installation). Remember to add `~/.roswell/bin` to your `PATH` environment variable. For macOS users, this can be done using the following command:

```bash
brew install roswell
```

2. Install GEB using the Roswell launcher with the following command:

```bash
ros install anoma/geb
```

After these steps, you should be able to run `geb` in your terminal. For example:

```bash
geb foo.lisp --stlc --vampir --output foo.pir
```

Here, `foo.lisp` is a file containing a valid lambda term. For instance:

```lisp
(lamb (list (coprod so1 so1))
    (index 0))
```

The output is a [vamp-ir](https://github.com/anoma/vamp-ir) expression:

```bash
$ cat foo.pir
def *entry* x {
  0
}
```

### Loading the Geb system

This project uses [common lisp](https://common-lisp.net/), so a few
   dependencies are needed to get around the code-base and start hacking. Namely:

1. [Lisp with quicklisp](https://lisp-lang.org/learn/getting-started/).

2. [Emacs](https://en.wikipedia.org/wiki/Emacs) along with one of the following:

    - [sly user manual](http://joaotavora.github.io/sly/)
    - [slime](https://github.com/slime/slime)

Assuming `sbcl` is installed, one popular implementation of Common Lisp, and you are in the `geb` directory of this repository, follow these steps to interact with the system using the Common Lisp REPL:

Open the SBCL `REPL`:

1. Run `sbcl` in the terminal:

```bash
sbcl .
```

If `sbcl` is not installed, you can install it using `ros`:

```bash
ros install sbcl
```

2. Load the system:

```lisp
(ql:quickload :geb/documentation)
```

If you're using Emacs, simply run either `M-x sly` or `M-x slime`, depending on whether you're using [sly](https://github.com/joaotavora/sly) or [slime](https://github.com/slime/slime).

To load from Emacs:

1. Open `geb.asd` and press `C-ck` (`sly-compile-and-load-file`, or `swank-compile-and-load-file` if you're using swank).

Once the file is open, you can load the system by writing:

```lisp
(ql:quickload :geb/documentation) ;; only necessary for the first time!
(asdf:load-system :geb/documentation) ;; if you want to load it in the future
```


## Formal Specification

Geb's design is grounded in its specification composed entirely of category-theoretic universal constructions. This enables the portability of Geb-written code across different interpreters. Moreover, thanks to its relatively weak internal logic, Geb can function as a sub-language of nearly any other general-purpose programming language, broadening its compatibility reach.

For more information on the formal specification of Geb, please visit the [Geb Specification](https://github.com/anoma/geb-spec) on GitHub.

### Future Key Features

We are currently developing and aiming to incorporate several key features into Geb, including:

1. **Metaprogramming**: The capability for programs to generate or alter other programs during execution.

2. **Effects**: The approach of separating side effects from the pure part of a program, simplifying code reasoning and testing.

3. **Dependent Types**: A powerful feature where types can depend on values, enhancing the expressiveness of the type systems.

4. **Programming Languages and Mathematical Logics as Built-in Types**: The unique ability to treat other languages and mathematical logics as intrinsic types.

5. **Composition of Languages and Logics**: The amalgamation of languages and logics, with individual components defined as universal constructions.

6. **Embeddable within Other Languages**: The ability for Geb to be embedded within other host languages, using its effects to call out of the embedded Geb code.

7. **Alternative Syntaxes**: The existence of a precise specification for functions an interpreter must support, without imposing any constraint on the syntax an interpreter should provide. There will be a correct mechanical translation (an isomorphism) between any two valid syntaxes.

8. **Category Theory as Internal and External Language**: Geb aims to leverage category theory as both its internal and external language, providing access to the broad domain of functions and theorems developed in category theory since its inception.
