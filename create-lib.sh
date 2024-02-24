#!/bin/sh

if test $# -ne 1; then echo "usage: `basename $0` DIR"; exit 1; fi

dir=$1

if test ! -f $dir/BASE; then echo "no $dir/BASE file"; exit 1; fi

base=`cat $dir/BASE`

cp $dir/coq.v .
cp $dir/theory_hol.v .
cp $dir/${base}_types.v types.v
cp $dir/${base}_terms.v terms.v
cp $dir/${base}_axioms.v axioms.v
cp $dir/${base}_opam.v hol_light.v

sed -i -e "s/${base}_//g" *.v
