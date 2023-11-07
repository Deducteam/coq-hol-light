HOL-Light library in Coq
------------------------

This library contains an automatic translation in [Coq](https://coq.inria.fr/) of (for the moment) some small part the [HOL-Light](https://github.com/jrh13/hol-light) library using [hol2dk](https://github.com/Deducteam/hol2dk) and [lambdapi](https://github.com/Deducteam/lambdapi).

As HOL-Light is based on higher-order logic, it assumes in Coq classical logic, Hilbert's ε operator, functional and propositional extensionnality (see [coq.v](https://github.com/Deducteam/coq-hol-light/blob/main/coq.v) for more details):

```
Axiom classic : forall P:Prop, P \/ ~ P.
Axiom constructive_indefinite_description : forall (A : Type) (P : A->Prop), (exists x, P x) -> { x : A | P x }.
Axiom fun_ext : forall {A B : Type} {f g : A -> B}, (forall x, (f x) = (g x)) -> f = g.
Axiom prop_ext : forall {P Q : Prop}, (P -> Q) -> (Q -> P) -> P = Q.
```

For the moment, it only contains the HOL-Light base library up to [arith.ml](https://github.com/jrh13/hol-light/blob/master/arith.ml), that is, basic theorems on first-order logic and on natural numbers arithmetic on the Coq functions pred, add, mul, pow, div, modulo, max, min, and the Coq predicates le, lt, ge, gt, Even, Odd.

We can translate to Coq much bigger parts of the HOL-Light library (at least all the base library [hol.ml](https://github.com/jrh13/hol-light/blob/master/hol.ml)). However, to get a library that is directly usable in Coq, we need to align HOL-Light functions and predicates to the ones defined in Coq standard library. This requires to (for the moment) manually prove that the HOL-Light definitions are equal to the corresponding Coq definitions. This has been done for the above functions and predicates but remains to be done for more functions and predicates on lists, real numbers, etc.

The translated theorems are (for the moment) provided as axioms in order to have fast Require's in Coq because the proofs currently extracted from HOL-Light are very big and not very informative for they are low level (the translation is done at the kernel level, not at the source level). If you are skeptical, you can however generate and check them again by using [hol2dk](https://github.com/Deducteam/hol2dk).

The current library contains 448 lemmas. The corresponding Coq proof files have a size of 49 Mo and take about 4m25s to check. The whole HOL-Light base library `hol.ml` has 2834 theorems. The corresponding Coq proof files have a size of 1.9 Go and take about 6 hours to check (with 64 Go RAM). The full HOL-Light library has 36471 theorems.

**Installation using [opam](https://opam.ocaml.org/)**

```
opam repo add coq-released https://coq.inria.fr/opam/released
opam install coq-hol-light
```

**Usage in a Coq file**

```
Require Import HOLLight.hol_upto_arith_opam.
Check thm_DIV_DIV.
```

**Reproducibility**

First, install HOL-Light and hol2dk as described in [README.md](https://github.com/Deducteam/hol2dk/blob/main/README.md). Then, do:

```
mkdir reproduce
cd reproduce
git clone https://github.com/jrh13/hol-light.git
export HOL_LIGHT_DIR=`pwd`/hol-light
git clone https://github.com/Deducteam/hol2dk.git
export HOL2DK_DIR=`pwd`/hol2dk
cd $HOL2DK_DIR
git checkout 983fc82
dune build && dune install
./patch $HOL_LIGHT_DIR
cd $HOL_LIGHT_DIR
git checkout 9c07c4f
make
cp hol.ml hol_upto_arith.ml
emacs hol_upto_arith.ml # edit hol_upto_arith.ml by removing every thing after ``loads "arith.ml";;''
hol2dk dump hol_upto_arith.ml
hol2dk pos hol_upto_arith
hol2dk use hol_upto_arith
cd ..
mkdir output-hol2dk
cd output-hol2dk
ln -s $HOL_LIGHT_DIR/hol_upto_arith.sig
ln -s $HOL_LIGHT_DIR/hol_upto_arith.prf
ln -s $HOL_LIGHT_DIR/hol_upto_arith.thm
ln -s $HOL_LIGHT_DIR/hol_upto_arith.pos
ln -s $HOL_LIGHT_DIR/hol_upto_arith.use
ln -s $HOL2DK_DIR/lambdapi.pkg
ln -s $HOL2DK_DIR/theory_hol.lp
ln -s $HOL2DK_DIR/_CoqProject
ln -s $HOL2DK_DIR/coq.v
hol2dk dg 7 hol_upto_arith
hol2dk mk-part hol_upto_arith
ln -s hol_upto_arith.mk Makefile
make -j7 vo
```
