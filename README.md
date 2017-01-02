<a name="top"></a>

# FLOw [![GitHub tag](https://img.shields.io/github/tag/szaghi/FLOw.svg)]() [![Join the chat at https://gitter.im/szaghi/FLOw](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/szaghi/FLOw?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[![License](https://img.shields.io/badge/license-GNU%20GeneraL%20Public%20License%20v3,%20GPLv3-blue.svg)]()
[![License](https://img.shields.io/badge/license-BSD2-red.svg)]()
[![License](https://img.shields.io/badge/license-BSD3-red.svg)]()
[![License](https://img.shields.io/badge/license-MIT-red.svg)]()

[![Status](https://img.shields.io/badge/status-unstable-red.svg)]()
[![Build Status](https://travis-ci.org/szaghi/FLOw.svg?branch=master)](https://travis-ci.org/szaghi/FLOw)
[![Coverage Status](https://img.shields.io/codecov/c/github/szaghi/FLOw.svg)](http://codecov.io/github/szaghi/FLOw?branch=master)

### FLOw, Fortran fLuid Object

A KISS pure Fortran Library providing a *Fluid Object* designed for building CFD codes:

- FLOw is a pure Fortran (KISS) library providing a Fluid Object designed for building CFD codes;
- FLOw is Fortran 2008+ standard compliant;
- FLOw is OOP designed;
- FLOw is TDD developed;
- FLOw is a Free, Open Source Project.

#### A taste of FLOw

```fortran
use flow
type(fluid_object) :: state
call state%initialize(pressure=1, density=0.125, cp=1024, cv=712)
...
```

To be completed.

#### Issues

[![GitHub issues](https://img.shields.io/github/issues/szaghi/FLOw.svg)]()
[![Ready in backlog](https://badge.waffle.io/szaghi/FLOw.png?label=ready&title=Ready)](https://waffle.io/szaghi/FLOw)
[![In Progress](https://badge.waffle.io/szaghi/FLOw.png?label=in%20progress&title=In%20Progress)](https://waffle.io/szaghi/FLOw)
[![Open bugs](https://badge.waffle.io/szaghi/FLOw.png?label=bug&title=Open%20Bugs)](https://waffle.io/szaghi/FLOw)

#### Compiler Support

[![Compiler](https://img.shields.io/badge/GNU-v6.1.1+-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/Intel-v16.1+-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/IBM%20XL-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/g95-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/NAG-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/PGI-not%20tested-yellow.svg)]()

---

[What is FLOw?](#what-is-flow) | [Main features](#main-features) | [Copyrights](#copyrights) | [Download](#download) | [Compilation](#compilation) | [Documentation](#documentation) | [References](#references)

---

## What is FLOw?

> **FLOw** is a modern Fortran library providing a *Fluid Object* designed for building CFD codes.

To be completed.

### How to use

To be written.

Go to [Top](#top)

## Main features

To be written.

Any feature request is welcome.

Go to [Top](#top)

## Copyrights

FLOw is a Free and Open Source Software (FOSS), it is distributed under a **very permissive** multi-licensing system: selectable licenses are [GPLv3](http://www.gnu.org/licenses/gpl-3.0.html), [BSD2-Clause](http://opensource.org/licenses/BSD-2-Clause), [BSD3-Clause](http://opensource.org/licenses/BSD-3-Clause) and [MIT](http://opensource.org/licenses/MIT), feel free to select the license that best matches your workflow.

> Anyone is interest to use, to develop or to contribute to FLOw is welcome.

More details can be found on [wiki](https://github.com/szaghi/FLOw/wiki/Copyrights).

Go to [Top](#top)

## Download

To be written.

Go to [Top](#top)

## Compilation

To be written.

## Documentation

Besides this README file the FLOw documentation is contained into its own [wiki](https://github.com/szaghi/FLOw/wiki). Detailed documentation of the API is contained into the [GitHub Pages](http://szaghi.github.io/FLOw/index.html) that can also be created locally by means of [ford tool](https://github.com/cmacmackin/ford).

Go to [Top](#top)
