Require Import HOLLight_Real_With_N.mappings HOLLight.With_N Coq.NArith.BinNat Coq.Reals.Rbase Coq.Reals.Rdefinitions Coq.Reals.Rbasic_fun.
Lemma TRANS {a : Type'} {x y z : a} (xy : x = y) (yz : y = z) : x = z.
Proof. exact (@EQ_MP (x = y) (x = z) (@MK_COMB a Prop (@eq a x) (@eq a x) y z (@eq_refl (a -> Prop) (@eq a x)) yz) xy). Qed.
Lemma SYM {a : Type'} {x y : a} (xy : x = y) : y = x.
Proof. exact (@EQ_MP (x = x) (y = x) (@MK_COMB a Prop (@eq a x) (@eq a y) x x (@MK_COMB a (a -> Prop) (@eq a) (@eq a) x y (@eq_refl (a -> a -> Prop) (@eq a)) xy) (@eq_refl a x)) (@eq_refl a x)). Qed.
