# ReScript Michelson

Run Michelson code using Rescript

## Installation

```sh
npm install
```

## Build

- Build: `npm run build`
- Clean: `npm run clean`
- Build & watch: `npm run start`
- Build, watch and run JS output code: `npm run watch`

## Run

```sh
node lib/es6/src/Index.bs.js
```

## Available instructions

`ABS`, `ADD`, `NIL`, `PAIR`, `PUSH`, `SUB`, `UNPAIR`

## Type system

Michelson types are represented by the recursive `m_type` variant, for example, a Michelson `int` is `m_type.Int` and a Michelson `list` is `m_type.List(m_type)`.

Values are represented by the recursive `m_value` variant which includes the name of the type and the associated value, for example, a Michelson value `5` of type `int` is `m_value.Int(5: m_int)` and a Michelson value `list{5, 6, 7}` is `m_value.List(list{5, 6, 7}: m_list<m_int>)`.
