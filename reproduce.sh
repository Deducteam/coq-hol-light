#!/bin/sh

mkdir reproduce
cd reproduce

echo install hol2dk ...
git clone https://github.com/Deducteam/hol2dk.git
cd hol2dk
export HOL2DK_DIR=`pwd`
git checkout 36bf943
dune build && dune install
cd ..

echo install and patch hol-light ...
git clone https://github.com/jrh13/hol-light.git
cd hol-light
export HOLLIGHT_DIR=`pwd`
git checkout 3d231f3
make
hol2dk patch

echo extract hol-light proofs
#cp hol.ml hol_upto_lists.ml
#emacs hol_upto_lists.ml # remove every thing after ``loads "lists.ml";;''
cp ../hol-light.ml .
hol2dk dump-simp-use coq_hol_light.ml # 39s
cd ..

mkdir output
cd output
hol2dk link coq_hol_light
make split # 0s
make -j32 lp # 10s
make dep-lpo # 10s
make -j32 v # 9s
make dep-vo # 0s
make -j32 vo # 4m
make opam
cd ..

mkdir opam
cd opam
../../create-lib.sh ../output