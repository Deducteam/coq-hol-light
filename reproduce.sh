#!/bin/sh

set -e # to exit as soon as there is an error

mkdir reproduce
cd reproduce

echo install hol2dk ...
git clone https://github.com/Deducteam/hol2dk.git
cd hol2dk
export HOL2DK_DIR=`pwd`
git checkout 3330bf7
dune build && dune install
cd ..

echo install and patch hol-light ...
git clone https://github.com/jrh13/hol-light.git
cd hol-light
export HOLLIGHT_DIR=`pwd`
git checkout 3d231f3
make
hol2dk patch

echo extract hol-light proofs ...
cp ../../coq_hol_light.ml .
hol2dk dump-simp-use coq_hol_light.ml # 39s
cd ..

echo install lambdapi ...
git clone https://github.com/Deducteam/lambdapi.git
cd lambdapi
git checkout 1bb724cf
opam install -y .
cd ..

echo translate HOL-Light proofs to lambdapi and coq ...
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

echo create opam library ...
mkdir opam
cd opam
../../create-lib.sh ../output
