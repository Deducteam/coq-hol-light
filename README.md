HOL-Light libraries in Coq
--------------------------

This Coq library contains an automatic translation in [Coq](https://coq.inria.fr/) of the [HOL-Light](https://github.com/jrh13/hol-light) base library [hol_lib.ml](https://github.com/jrh13/hol-light/blob/master/hol_lib.ml) with HOL-Light types and functions mapped to the corresponding types and functions in the Coq standard library so that, for instance, a HOL-Light theorem on HOL-Light real numbers is translated into a Coq theorem on Coq real numbers. The provided theorems can therefore be readily reused and combined with other Coq developments based on the Coq standard library.

It contains 2897 theorems including theorems on arithmetic,
wellfounded relations, lists, real numbers, integers, basic set
theory, etc.

As HOL-Light is based on classical higher-order logic with choice, this library uses the following standard set of axioms in Coq:

```
Axiom classic : forall P:Prop, P \/ ~ P.
Axiom constructive_indefinite_description : forall (A : Type) (P : A->Prop), (exists x, P x) -> { x : A | P x }.
Axiom fun_ext : forall {A B : Type} {f g : A -> B}, (forall x, (f x) = (g x)) -> f = g.
Axiom prop_ext : forall {P Q : Prop}, (P -> Q) -> (Q -> P) -> P = Q.
Axiom proof_irrelevance : forall (P:Prop) (p1 p2:P), p1 = p2.
```

The translated theorems are provided as axioms in order to have fast Require's because the proofs currently extracted from HOL-Light are very big (1.2 Gb) and not very informative for they are low level (the translation is done at the kernel level, not at the source level). If you are skeptical, you can however generate and check them again by using [hol2dk](https://github.com/Deducteam/hol2dk) to extract and translate HOL-Light proofs to Lambdapi files, and [lambdapi](https://github.com/Deducteam/lambdapi) to translate Lambdapi files to Coq files.

**Installation using [opam](https://opam.ocaml.org/)**

dependencies: [coq-hol-light-real](https://github.com/Deducteam/coq-hol-light-real/), [coq-fourcolor-reals](https://github.com/coq-community/fourcolor)

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

Run [reproduce](https://github.com/Deducteam/hol2dk/blob/main/reproduce). It takes 70 minutes with 32 processors Intel Core i9-13950HX and 64 Gb RAM. If every thing works well, the proofs will be in the directory `tmp/output`.
