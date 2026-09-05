# BNF Parser Generator

A full-stack parser generator that turns a compact Backus-Naur Form (BNF) grammar into typed Haskell parser-combinator code. The project includes grammar validation, Haskell code generation, and a browser interface for generating and running parsers interactively.

## What it does

The generator processes a grammar in three stages:

1. Parses the BNF source into an algebraic data type (AST).
2. Reports duplicate productions, undefined nonterminals, parameter-count mismatches, and direct or indirect left recursion.
3. Removes invalid productions and emits Haskell data types and parser functions for the remaining grammar.

The local web application can dynamically compile the generated module and run a selected parser against an input string.

## Key features

- Custom parser-combinator implementation with `Functor`, `Applicative`, `Monad`, and `Alternative` instances
- Haskell ADT and parser-function generation from BNF productions
- Quoted and bare terminals
- Built-in `[int]`, `[alpha]`, and `[newline]` macros
- Prefix token parsing with `tok` and postfix `*`, `+`, and `?` modifiers
- Parameterised nonterminals such as `<pair(a, b)>`
- Fixed-point grammar cleanup after structural validation
- Interactive TypeScript UI backed by a local Scotty API
- Regression fixtures for generated code and doctests for core combinators

## Example

Enter this grammar in the web interface:

```bnf
<number> ::= [int]
```

The generator produces:

```haskell
newtype Number = Number Int
    deriving Show

number :: Parser Number
number = Number <$> int
```

Select `number`, enter `42` as the parser input, and run it. The parser result is:

```text
Number 42
```

More complete grammars are available in `backend/examples/input/`.

## Technologies

- Haskell, Stack, and GHC
- Scotty and Aeson for the local HTTP API
- GHC API for in-memory compilation of generated parsers
- TypeScript, RxJS, and Vite for the browser interface

## Project structure

```text
.
├── backend/
│   ├── app/Main.hs                 # Local API server and generated-parser runner
│   ├── src/BNFGenerator.hs         # BNF AST, grammar parser, validation, and code generation
│   ├── src/Instances.hs            # Parser and parse-result types and instances
│   ├── src/Parser.hs               # Reusable parser combinators
│   ├── template/BNFParser.template # Runtime support prepended to generated modules
│   ├── examples/                   # BNF inputs and expected generated code
│   └── test/Spec.hs                # Doctests and regression checks
└── frontend/
    ├── src/                        # Reactive UI state and API integration
    └── public/                     # Browser styles
```

## Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/) 3.x
- Node.js 20.19+ or 22.12+
- npm

Stack uses the pinned Stackage snapshot in `backend/stack.yaml` and downloads the matching GHC toolchain when needed.

## Build and test

Build and test the Haskell backend:

```bash
cd backend
stack build
stack test
```

Build the frontend:

```bash
cd frontend
npm ci
npm run build
```

## Run the application

Start the backend from one terminal:

```bash
cd backend
stack run bnf-parser-server
```

Start the frontend development server from another terminal:

```bash
cd frontend
npm ci
npm run dev
```

Open `http://localhost:5173`, enter a grammar, choose one of its generated parser functions, and provide a string to parse. The UI also lets you download the generated Haskell module.

## Implementation concepts

The BNF grammar is represented with algebraic data types for productions, symbols, and modifiers. Small parser combinators are composed to mirror the grammar's recursive structure, while pattern matching keeps code generation explicit and type-directed.

Validation and generation share the same AST. Invalid productions are removed by repeatedly applying cleanup passes until the grammar reaches a fixed point, which handles cascading errors such as one removed rule causing another reference to become undefined. Parameterised productions are resolved during validation and translated into polymorphic Haskell data types and parser functions.

## Local-use note

The backend compiles generated Haskell code in memory through the GHC API. It is intended as a local development and learning tool; do not expose the server directly to untrusted networks without additional isolation and hardening.
