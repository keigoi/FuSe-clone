Title:	FuSe — A Simple Library Implementation of Binary Sessions
Author:	Luca Padovani
CSS:	https://fonts.googleapis.com/css?family=Open+Sans:400,700|Roboto+Mono
CSS:    https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.9.0/styles/default.min.css
html header:
    <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.9.0/highlight.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.9.0/languages/ocaml.min.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
CSS:	FuSe.css

FuSe — A Simple Library Implementation of Binary Sessions
=========================================================

* [Overview][]
* [Download][]
* [Installation instructions][]
* [Documentation][]
* [Example][]

Overview
--------

FuSe is a lightweight OCaml module that implements the session-based
communication primitives in [#GayVasconcelos10] and enables session
type checking and inference. It works with any out-of-the-box
installation of OCaml and supports the following features:

* delegation
* equi-recursive session types
* polymorphic session types
* context-free session types [#ThiemannVasconcelos16] [#Padovani17B]
* session types with labeled branches
* session type inference
* duality constraints on session type variables
* hybrid static/dynamic linearity checking
* session subtyping [#GayHole05]
* higher-order resumption combinators
* shared channels for session initiation
* type pretty-printer (external utility)

For questions, suggestions and bug reports please contact [Luca
Padovani](http://www.di.unito.it/~padovani/index.html).

Download
--------

* [FuSe 0.7](FuSe-0.7.tar.gz) - new homepage, code cleanup, tutorial
  updated (15/01/2017)
* [FuSe 0.6](FuSe-0.6.tar.gz) - bug fixes, resumption combinators
  simplified, tutorial updated (05/09/2016)
* [FuSe 0.5](FuSe-0.5.tar.gz) - added type pretty printer and monadic
  interface (10/08/2016)
* [FuSe 0.4](FuSe-0.4.tar.gz) - further API cleanup, unused endpoint
  detection, tutorial (07/04/2016)
* [FuSe 0.3](FuSe-0.3.tar.gz) - API cleanup, higher-order iterators
  (31/03/2016)
* [FuSe 0.2](FuSe-0.2.tar.gz) - simpler representation of session
  types, subtyping (28/03/2016)
* [FuSe 0.1](FuSe-0.1.tar.gz) - initial release (30/09/2015)

Installation Instructions
-------------------------

You need [OCaml](http://www.ocaml.org) to compile FuSe.

1.  Compile the library and the examples:

    ```bash
    make
    ```

2.  Test an example:

    ```bash
    ./examples/Tmath
    ```

3.  Test session type inference:

    ```bash
    cd examples
    make Tmath.st
    ```

Documentation
-------------

Users may refer to the [API documentation extracted with
`ocamldoc`](index.html) and to [the tutorial](FuSe.pdf).

The principles and internals of FuSe are described in [#Padovani17A].

Example
-------

Below is an almost verbatim transcription in OCaml+FuSe of the
bookshop example found in [#GayVasconcelos10].

```ocaml
{{examples/bookshop.ml}}```

Here are the (session) types inferred by OCaml, pretty printed in a
more readable form by the `rosetta` utility that accompanies the
library (the [tutorial](FuSe.pdf) gives the correspondence between
OCaml types and this notation). Note the amount of parametric
polymorphism compared to the typing of the example as given in
[#GayVasconcelos10].

```ocaml
val shopLoop : rec X.&[ Add: ?α.X | CheckOut: ?β.?γ ] → α list → unit
val shop : rec X.&[ Add: ?α.X | CheckOut: ?β.?γ ] Service.t → unit
val isChildrensBook : α → bool
val voucher : α → β → rec X.⊕[ Add: !γ.X | CheckOut: !α.!β ] → γ → γ
val mother : α → β → &[ Add: ?γ.rec X.&[ Add: ?δ.X | CheckOut: ?α.?β ] ] Service.t →
             ?(δ → δ).!ε Service.t → γ → unit
val son : ?(α → β).!β Service.t → α → unit
```

[#GayHole05]: Simon J. Gay and Malcolm Hole: [Subtyping for session
types in the pi
calculus](http://dx.doi.org/10.1007/s00236-005-0177-z), *Acta
Informatica*, 2005.

[#GayVasconcelos10]: Simon J. Gay, Vasco T. Vasconcelos, [Linear type
theory for asynchronous session
types](http://doi.org/10.1017/S0956796809990268), *Journal of
Functional Programming*, 2010.

[#Padovani17A]: Luca Padovani: [A Simple Library Implementation of
Binary Sessions](http://dx.doi.org/10.1017/S0956796816000289),
*Journal of Functional Programming*, 2017.

[#Padovani17B]: Luca Padovani: [Context-Free Session Type
Inference](http://hal.archives-ouvertes.fr/hal-01385258/document),
*Proceedings of ESOP'17*, 2017.

[#ThiemannVasconcelos16]: Peter Thiemann and Vasco T. Vasconcelos:
[Context-Free Session Types](http://doi.org/10.1145/3022670.2951926),
*Proceedings of ICFP'16*, 2016.
