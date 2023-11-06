Require Import HOLLight.coq.
Require Import HOLLight.theory_hol.
Require Import HOLLight.hol_upto_arith_types.
Definition _FALSITY_ := False.
Lemma _FALSITY__def : _FALSITY_ = False.
Proof. exact (eq_refl _FALSITY_). Qed.
Lemma COND_def {A : Type'} : (@COND A) = (fun t : Prop => fun t1 : A => fun t2 : A => @ε A (fun x : A => ((t = True) -> x = t1) /\ ((t = False) -> x = t2))).
Proof. exact (eq_refl (@COND A)). Qed.
Definition o {A B C : Type'} := fun f : B -> C => fun g : A -> B => fun x : A => f (g x).
Lemma o_def {A B C : Type'} : (@o A B C) = (fun f : B -> C => fun g : A -> B => fun x : A => f (g x)).
Proof. exact (eq_refl (@o A B C)). Qed.
Definition I {A : Type'} := fun x : A => x.
Lemma I_def {A : Type'} : (@I A) = (fun x : A => x).
Proof. exact (eq_refl (@I A)). Qed.
Definition hashek := True.
Lemma hashek_def : hashek = True.
Proof. exact (eq_refl hashek). Qed.
Definition LET {A B : Type'} := fun f : A -> B => fun x : A => f x.
Lemma LET_def {A B : Type'} : (@LET A B) = (fun f : A -> B => fun x : A => f x).
Proof. exact (eq_refl (@LET A B)). Qed.
Definition LET_END {A : Type'} := fun t : A => t.
Lemma LET_END_def {A : Type'} : (@LET_END A) = (fun t : A => t).
Proof. exact (eq_refl (@LET_END A)). Qed.
Definition GABS {A : Type'} := fun P : A -> Prop => @ε A P.
Lemma GABS_def {A : Type'} : (@GABS A) = (fun P : A -> Prop => @ε A P).
Proof. exact (eq_refl (@GABS A)). Qed.
Definition GEQ {A : Type'} := fun a : A => fun b : A => a = b.
Lemma GEQ_def {A : Type'} : (@GEQ A) = (fun a : A => fun b : A => a = b).
Proof. exact (eq_refl (@GEQ A)). Qed.
Definition _SEQPATTERN {_4611 _4614 : Type'} := fun r : _4614 -> _4611 -> Prop => fun s : _4614 -> _4611 -> Prop => fun x : _4614 => @COND (_4611 -> Prop) (exists y : _4611, r x y) (r x) (s x).
Lemma _SEQPATTERN_def {_4611 _4614 : Type'} : (@_SEQPATTERN _4611 _4614) = (fun r : _4614 -> _4611 -> Prop => fun s : _4614 -> _4611 -> Prop => fun x : _4614 => @COND (_4611 -> Prop) (exists y : _4611, r x y) (r x) (s x)).
Proof. exact (eq_refl (@_SEQPATTERN _4611 _4614)). Qed.
Definition _UNGUARDED_PATTERN := fun p : Prop => fun r : Prop => p /\ r.
Lemma _UNGUARDED_PATTERN_def : _UNGUARDED_PATTERN = (fun p : Prop => fun r : Prop => p /\ r).
Proof. exact (eq_refl _UNGUARDED_PATTERN). Qed.
Definition _GUARDED_PATTERN := fun p : Prop => fun g : Prop => fun r : Prop => p /\ (g /\ r).
Lemma _GUARDED_PATTERN_def : _GUARDED_PATTERN = (fun p : Prop => fun g : Prop => fun r : Prop => p /\ (g /\ r)).
Proof. exact (eq_refl _GUARDED_PATTERN). Qed.
Definition _MATCH {_4656 _4660 : Type'} := fun e : _4656 => fun r : _4656 -> _4660 -> Prop => @COND _4660 (@ex1 _4660 (r e)) (@ε _4660 (r e)) (@ε _4660 (fun z : _4660 => False)).
Lemma _MATCH_def {_4656 _4660 : Type'} : (@_MATCH _4656 _4660) = (fun e : _4656 => fun r : _4656 -> _4660 -> Prop => @COND _4660 (@ex1 _4660 (r e)) (@ε _4660 (r e)) (@ε _4660 (fun z : _4660 => False))).
Proof. exact (eq_refl (@_MATCH _4656 _4660)). Qed.
Definition _FUNCTION {_4678 _4682 : Type'} := fun r : _4678 -> _4682 -> Prop => fun x : _4678 => @COND _4682 (@ex1 _4682 (r x)) (@ε _4682 (r x)) (@ε _4682 (fun z : _4682 => False)).
Lemma _FUNCTION_def {_4678 _4682 : Type'} : (@_FUNCTION _4678 _4682) = (fun r : _4678 -> _4682 -> Prop => fun x : _4678 => @COND _4682 (@ex1 _4682 (r x)) (@ε _4682 (r x)) (@ε _4682 (fun z : _4682 => False))).
Proof. exact (eq_refl (@_FUNCTION _4678 _4682)). Qed.
Lemma mk_pair_def {A B : Type'} : (@mk_pair A B) = (fun x : A => fun y : B => fun a : A => fun b : B => (a = x) /\ (b = y)).
Proof. exact (eq_refl (@mk_pair A B)). Qed.
Definition CURRY {A B C : Type'} := fun _1283 : (prod A B) -> C => fun _1284 : A => fun _1285 : B => _1283 (@pair A B _1284 _1285).
Lemma CURRY_def {A B C : Type'} : (@CURRY A B C) = (fun _1283 : (prod A B) -> C => fun _1284 : A => fun _1285 : B => _1283 (@pair A B _1284 _1285)).
Proof. exact (eq_refl (@CURRY A B C)). Qed.
Definition UNCURRY {A B C : Type'} := fun _1304 : A -> B -> C => fun _1305 : prod A B => _1304 (@fst A B _1305) (@snd A B _1305).
Lemma UNCURRY_def {A B C : Type'} : (@UNCURRY A B C) = (fun _1304 : A -> B -> C => fun _1305 : prod A B => _1304 (@fst A B _1305) (@snd A B _1305)).
Proof. exact (eq_refl (@UNCURRY A B C)). Qed.
Definition PASSOC {A B C D : Type'} := fun _1321 : (prod (prod A B) C) -> D => fun _1322 : prod A (prod B C) => _1321 (@pair (prod A B) C (@pair A B (@fst A (prod B C) _1322) (@fst B C (@snd A (prod B C) _1322))) (@snd B C (@snd A (prod B C) _1322))).
Lemma PASSOC_def {A B C D : Type'} : (@PASSOC A B C D) = (fun _1321 : (prod (prod A B) C) -> D => fun _1322 : prod A (prod B C) => _1321 (@pair (prod A B) C (@pair A B (@fst A (prod B C) _1322) (@fst B C (@snd A (prod B C) _1322))) (@snd B C (@snd A (prod B C) _1322)))).
Proof. exact (eq_refl (@PASSOC A B C D)). Qed.
Lemma ONE_ONE_def {A B : Type'} : (@ONE_ONE A B) = (fun _2064 : A -> B => forall x1 : A, forall x2 : A, ((_2064 x1) = (_2064 x2)) -> x1 = x2).
Proof. exact (eq_refl (@ONE_ONE A B)). Qed.
Lemma ONTO_def {A B : Type'} : (@ONTO A B) = (fun _2069 : A -> B => forall y : B, exists x : A, y = (_2069 x)).
Proof. exact (eq_refl (@ONTO A B)). Qed.
Lemma IND_SUC_def : IND_SUC = (@ε (ind -> ind) (fun f : ind -> ind => exists z : ind, (forall x1 : ind, forall x2 : ind, ((f x1) = (f x2)) = (x1 = x2)) /\ (forall x : ind, ~ ((f x) = z)))).
Proof. exact (eq_refl IND_SUC). Qed.
Lemma IND_0_def : IND_0 = (@ε ind (fun z : ind => (forall x1 : ind, forall x2 : ind, ((IND_SUC x1) = (IND_SUC x2)) = (x1 = x2)) /\ (forall x : ind, ~ ((IND_SUC x) = z)))).
Proof. exact (eq_refl IND_0). Qed.
Lemma NUM_REP_def : NUM_REP = (fun a : ind => forall NUM_REP' : ind -> Prop, (forall a' : ind, ((a' = IND_0) \/ (exists i : ind, (a' = (IND_SUC i)) /\ (NUM_REP' i))) -> NUM_REP' a') -> NUM_REP' a).
Proof. exact (eq_refl NUM_REP). Qed.
Definition NUMERAL := fun _2128 : nat => _2128.
Lemma NUMERAL_def : NUMERAL = (fun _2128 : nat => _2128).
Proof. exact (eq_refl NUMERAL). Qed.
Lemma BIT1_def : BIT1 = (fun _2143 : nat => S (BIT0 _2143)).
Proof. exact (eq_refl BIT1). Qed.
Definition minimal := fun _6373 : nat -> Prop => @ε nat (fun n : nat => (_6373 n) /\ (forall m : nat, (Peano.lt m n) -> ~ (_6373 m))).
Lemma minimal_def : minimal = (fun _6373 : nat -> Prop => @ε nat (fun n : nat => (_6373 n) /\ (forall m : nat, (Peano.lt m n) -> ~ (_6373 m)))).
Proof. exact (eq_refl minimal). Qed.
