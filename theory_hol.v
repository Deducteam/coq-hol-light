Require Import HOLLight_Real.HOLLight_Real HOLLight Rdefinitions Rbasic_fun Raxioms.
Lemma TRANS {a : Type'} {x y z : a} (xy : x = y) (yz : y = z) : x = z.
Proof. exact (@EQ_MP (x = y) (x = z) (@MK_COMB a Prop (@eq a x) (@eq a x) y z (@eq_refl (a -> Prop) (@eq a x)) yz) xy). Qed.
Lemma SYM {a : Type'} {x y : a} (xy : x = y) : y = x.
Proof. exact (@EQ_MP (x = x) (y = x) (@MK_COMB a Prop (@eq a x) (@eq a y) x x (@MK_COMB a (a -> Prop) (@eq a) (@eq a) x y (@eq_refl (a -> a -> Prop) (@eq a)) xy) (@eq_refl a x)) (@eq_refl a x)). Qed.
