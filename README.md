HOL-Light libraries in Coq
--------------------------

This Coq library contains an automatic translation in [Coq](https://coq.inria.fr/) of the [HOL-Light](https://github.com/jrh13/hol-light) library [Multivariate/make_complex.ml](https://github.com/jrh13/hol-light/blob/master/Multivariate/make_complex.ml) with various HOL-Light types and functions [mapped](https://github.com/Deducteam/coq-hol-light/blob/main/With_N.lp) to the corresponding types and functions of the Coq standard library so that, for instance, a HOL-Light theorem on HOL-Light real numbers is translated into a Coq theorem on Coq real numbers. The provided theorems can therefore be readily reused and combined with other Coq developments based on the Coq standard library. More types and functions need to be aligned though.

It contains more than 20,000 theorems on arithmetic, wellfounded relations,
lists, real numbers, integers, basic set theory, permutations, group
theory, matroids, metric spaces, homology, vectors, determinants,
topology, convex sets and functions, paths, polytopes, Brouwer degree,
derivatives, Clifford algebra, integration, measure theory, complex
numbers and analysis, transcendental numbers, real analysis, complex
line integrals, etc. See HOL-Light files for more details.

The types and functions currently [aligned](https://github.com/Deducteam/coq-hol-light/blob/main/With_N.lp) are:
- types: unit, prod, list, option, sum, ascii, N, R, Z
- functions on N: pred, add, mul, pow, le, lt, ge, gt, max, min, sub, div, modulo
- functions on list: app, rev, map, removelast, In, hd, tl
- functions on R: Rle, Rplus, Rmult, Rinv, Ropp, Rabs, Rdiv, Rminus, Rge, Rgt, Rlt, Rmax, Rmin, IZR
Help is welcome to align more functions!

As HOL-Light is based on classical higher-order logic with choice, this library uses the following standard set of axioms in Coq:

```
Axiom classic : forall P:Prop, P \/ ~ P.
Axiom constructive_indefinite_description : forall (A : Type) (P : A->Prop), (exists x, P x) -> { x : A | P x }.
Axiom fun_ext : forall {A B : Type} {f g : A -> B}, (forall x, (f x) = (g x)) -> f = g.
Axiom prop_ext : forall {P Q : Prop}, (P -> Q) -> (Q -> P) -> P = Q.
Axiom proof_irrelevance : forall (P:Prop) (p1 p2:P), p1 = p2.
```

The translated theorems are provided as axioms in order to have fast Require's because the proofs currently extracted from HOL-Light are very big (91 Gb) and not very informative for they are low level (the translation is done at the kernel level, not at the source level). If you are skeptical, you can however generate and check them again by using [hol2dk](https://github.com/Deducteam/hol2dk) to extract and translate HOL-Light proofs to Lambdapi files, and [lambdapi](https://github.com/Deducteam/lambdapi) to translate Lambdapi files to Coq files.

**Installation using [opam](https://opam.ocaml.org/)**

dependencies: [coq-hol-light-real-with-N](https://github.com/Deducteam/coq-hol-light-real-with-N/), [coq-fourcolor-reals](https://github.com/coq-community/fourcolor)

```
opam repo add coq-released https://coq.inria.fr/opam/released
opam install coq-hol-light
```

**Usage in a Coq file**

```
Require Import HOLLight.theorems.
Check thm_DIV_DIV.
```

**Reproducibility**

Run [reproduce](https://github.com/Deducteam/hol2dk/blob/main/reproduce). It takes 35 hours with 32 processors Intel Core i9-13950HX and 128 Gb RAM. If every thing works well, the proofs will be in the directory `tmp/output`.
