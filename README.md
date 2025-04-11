HOL-Light libraries in Coq
--------------------------

This [Coq](https://coq.inria.fr/) library contains an automatic translation of the [HOL-Light](https://github.com/jrh13/hol-light) library [Multivariate/make_complex.ml](https://github.com/jrh13/hol-light/blob/master/Multivariate/make_complex.ml) with various HOL-Light types and functions [mapped](https://github.com/Deducteam/coq-hol-light/blob/main/mappings.lp) to the corresponding types and functions of the Coq standard library so that, for instance, a HOL-Light theorem on HOL-Light real numbers is translated to a Coq theorem on Coq real numbers. The provided theorems can therefore be readily used within other Coq developments based on the Coq standard library. More types and functions need to be aligned though (see below how to contribute). The translation has been done using [hol2dk](https://github.com/Deducteam/hol2dk) to extract and translate HOL-Light proofs to Lambdapi files, and [lambdapi](https://github.com/Deducteam/lambdapi) to translate Lambdapi files to Coq files.

It contains more than 20,000 theorems on arithmetic, wellfounded relations,
lists, real numbers, integers, basic set theory, permutations, group
theory, matroids, metric spaces, homology, vectors, determinants,
topology, convex sets and functions, paths, polytopes, Brouwer degree,
derivatives, Clifford algebra, integration, measure theory, complex
numbers and analysis, transcendental numbers, real analysis, complex
line integrals, etc. See HOL-Light files for more details.

The translated theorems are provided as axioms in order to have a fast Require because the proofs currently extracted from HOL-Light are very big (91 Gb) and not very informative for they are low level (the translation is done at the kernel level, not at the source level). If you are skeptical, you can however generate and check them again by using the script [reproduce](https://github.com/Deducteam/hol2dk/blob/main/reproduce). It however takes about 25 hours with 32 processors Intel Core i9-13950HX and 128 Gb RAM. If every thing works well, the proofs will be in the directory `tmp/output`.

The types and functions currently [aligned](https://github.com/Deducteam/coq-hol-light/blob/main/mappings.lp) are:
- types: unit, prod, list, option, sum, ascii, N, R, Z
- functions on N: pred, add, mul, pow, le, lt, ge, gt, max, min, sub, div, modulo, even, odd, factorial
- functions on Z: IZR, le, lt, ge, gt, opp, add, sub, mul, abs, sgn, max, min, pow, div, rem, divide, coprime, gcd, lcm
- functions on list: app, rev, map, removelast, In, hd, tl
- functions on R: Rle, Rplus, Rmult, Rinv, Ropp, Rabs, Rdiv, Rminus, Rge, Rgt, Rlt, Rmax, Rmin, IZR, Rsgn, Rmod_eq, Rpow

Your help is welcome to align more functions!

**How to contribute?**

You can easily contribute by proving the correctness of more mappings in Coq:

- Look in [terms.v](https://github.com/Deducteam/coq-hol-light/blob/main/terms.v) for the definition of a function symbol, say f, that you want to replace; note that it is followed by a lemma f_DEF stating what f is equal to.

- Copy and paste in [mappings.v](https://github.com/Deducteam/coq-hol-light/blob/main/mappings.v) the lemma f_DEF, and try to prove it if f is replaced by your own function.

- Create a [pull request](https://github.com/Deducteam/coq-hol-light/pulls).

You can also propose to change the mapping of some type in [mappings.v](https://github.com/Deducteam/coq-hol-light/blob/main/mappings.v). Every HOL-Light type `A` is axiomatized as being isomorphic to the subset of elements `x` of some already defined type `B` that satisfies some property `p:B->Prop`. `A` can always be mapped to the Coq type `{x:B|p(x)}` (see [mappings.v](https://github.com/Deducteam/coq-hol-light-real-with-nat/blob/main/mappings.v)) but it is possible to map it to some more convenient type `A'` by defining two functions:

- `mk:B->A'`

- `dest:A'->B`

and proving two lemmas:

- `mk_dest x: mk (dest x) = x`

- `dest_mk x: P x = (dest (mk x) = x)`

showing that `A'` is isomorphic to `{x:B|p(x)}`.

Note that the mappings of functions on natural numbers and lists are proved in [coq-hol-light-real-with-N](https://github.com/Deducteam/coq-hol-light-real-with-N/).

**Axioms used**

As HOL-Light is based on classical higher-order logic with choice, this library uses the following standard set of axioms in Coq:

```
Axiom classic : forall P:Prop, P \/ ~ P.
Axiom constructive_indefinite_description : forall (A : Type) (P : A->Prop), (exists x, P x) -> { x : A | P x }.
Axiom fun_ext : forall {A B : Type} {f g : A -> B}, (forall x, (f x) = (g x)) -> f = g.
Axiom prop_ext : forall {P Q : Prop}, (P -> Q) -> (Q -> P) -> P = Q.
Axiom proof_irrelevance : forall (P:Prop) (p1 p2:P), p1 = p2.
```

**Installation using [opam](https://opam.ocaml.org/)**

Dependencies: [coq-hol-light-real-with-N](https://github.com/Deducteam/coq-hol-light-real-with-N/), [coq-fourcolor-reals](https://github.com/coq-community/fourcolor)

```
opam repo add coq-released https://coq.inria.fr/opam/released
opam install coq-hol-light
```

**Usage in a Coq file**

```
Require Import HOLLight.theorems.
Check thm_DIV_DIV.
```
