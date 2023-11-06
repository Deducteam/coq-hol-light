Require Import coq.
Require Import theory_hol.
Require Import hol_upto_arith_types.
Require Import hol_upto_arith_terms.
Require Import hol_upto_arith_axioms.
Axiom thm_T_DEF : True = ((fun p : Prop => p) = (fun p : Prop => p)).
Axiom thm_AND_DEF : and = (fun p : Prop => fun q : Prop => (fun f : Prop -> Prop -> Prop => f p q) = (fun f : Prop -> Prop -> Prop => f True True)).
Axiom thm_IMP_DEF : imp = (fun p : Prop => fun q : Prop => (p /\ q) = p).
Axiom thm_FORALL_DEF : forall {A : Type'}, (@all A) = (fun P : A -> Prop => P = (fun x : A => True)).
Axiom thm_EXISTS_DEF : forall {A : Type'}, (@ex A) = (fun P : A -> Prop => forall q : Prop, (forall x : A, (P x) -> q) -> q).
Axiom thm_OR_DEF : or = (fun p : Prop => fun q : Prop => forall r : Prop, (p -> r) -> (q -> r) -> r).
Axiom thm_F_DEF : False = (forall p : Prop, p).
Axiom thm_NOT_DEF : not = (fun p : Prop => p -> False).
Axiom thm_EXISTS_UNIQUE_DEF : forall {A : Type'}, (@ex1 A) = (fun P : A -> Prop => (ex P) /\ (forall x : A, forall y : A, ((P x) /\ (P y)) -> x = y)).
Axiom thm__FALSITY_ : _FALSITY_ = False.
Axiom thm_EQ_REFL : forall {A : Type'}, forall x : A, x = x.
Axiom thm_REFL_CLAUSE : forall {A : Type'}, forall x : A, (x = x) = True.
Axiom thm_EQ_SYM : forall {A : Type'}, forall x : A, forall y : A, (x = y) -> y = x.
Axiom thm_EQ_SYM_EQ : forall {A : Type'}, forall x : A, forall y : A, (x = y) = (y = x).
Axiom thm_EQ_TRANS : forall {A : Type'}, forall x : A, forall y : A, forall z : A, ((x = y) /\ (y = z)) -> x = z.
Axiom thm_BETA_THM : forall {A B : Type'}, forall f : A -> B, forall y : A, ((fun x : A => f x) y) = (f y).
Axiom thm_ABS_SIMP : forall {A B : Type'}, forall t1 : A, forall t2 : B, ((fun x : B => t1) t2) = t1.
Axiom thm_CONJ_ASSOC : forall t1 : Prop, forall t2 : Prop, forall t3 : Prop, (t1 /\ (t2 /\ t3)) = ((t1 /\ t2) /\ t3).
Axiom thm_CONJ_SYM : forall t1 : Prop, forall t2 : Prop, (t1 /\ t2) = (t2 /\ t1).
Axiom thm_CONJ_ACI : forall (r : Prop) (p : Prop) (q : Prop), ((p /\ q) = (q /\ p)) /\ ((((p /\ q) /\ r) = (p /\ (q /\ r))) /\ (((p /\ (q /\ r)) = (q /\ (p /\ r))) /\ (((p /\ p) = p) /\ ((p /\ (p /\ q)) = (p /\ q))))).
Axiom thm_DISJ_ASSOC : forall t1 : Prop, forall t2 : Prop, forall t3 : Prop, (t1 \/ (t2 \/ t3)) = ((t1 \/ t2) \/ t3).
Axiom thm_DISJ_SYM : forall t1 : Prop, forall t2 : Prop, (t1 \/ t2) = (t2 \/ t1).
Axiom thm_DISJ_ACI : forall (r : Prop) (p : Prop) (q : Prop), ((p \/ q) = (q \/ p)) /\ ((((p \/ q) \/ r) = (p \/ (q \/ r))) /\ (((p \/ (q \/ r)) = (q \/ (p \/ r))) /\ (((p \/ p) = p) /\ ((p \/ (p \/ q)) = (p \/ q))))).
Axiom thm_IMP_CONJ : forall (p : Prop) (q : Prop) (r : Prop), ((p /\ q) -> r) = (p -> q -> r).
Axiom thm_IMP_CONJ_ALT : forall (q : Prop) (p : Prop) (r : Prop), ((p /\ q) -> r) = (q -> p -> r).
Axiom thm_LEFT_OR_DISTRIB : forall p : Prop, forall q : Prop, forall r : Prop, (p /\ (q \/ r)) = ((p /\ q) \/ (p /\ r)).
Axiom thm_RIGHT_OR_DISTRIB : forall p : Prop, forall q : Prop, forall r : Prop, ((p \/ q) /\ r) = ((p /\ r) \/ (q /\ r)).
Axiom thm_FORALL_SIMP : forall {A : Type'}, forall t : Prop, (forall x : A, t) = t.
Axiom thm_EXISTS_SIMP : forall {A : Type'}, forall t : Prop, (exists x : A, t) = t.
Axiom thm_EQ_CLAUSES : forall t : Prop, ((True = t) = t) /\ (((t = True) = t) /\ (((False = t) = (~ t)) /\ ((t = False) = (~ t)))).
Axiom thm_NOT_CLAUSES_WEAK : ((~ True) = False) /\ ((~ False) = True).
Axiom thm_AND_CLAUSES : forall t : Prop, ((True /\ t) = t) /\ (((t /\ True) = t) /\ (((False /\ t) = False) /\ (((t /\ False) = False) /\ ((t /\ t) = t)))).
Axiom thm_OR_CLAUSES : forall t : Prop, ((True \/ t) = True) /\ (((t \/ True) = True) /\ (((False \/ t) = t) /\ (((t \/ False) = t) /\ ((t \/ t) = t)))).
Axiom thm_IMP_CLAUSES : forall t : Prop, ((True -> t) = t) /\ (((t -> True) = True) /\ (((False -> t) = True) /\ (((t -> t) = True) /\ ((t -> False) = (~ t))))).
Axiom thm_EXISTS_UNIQUE_THM : forall {A : Type'}, forall P : A -> Prop, (@ex1 A (fun x : A => P x)) = ((exists x : A, P x) /\ (forall x : A, forall x' : A, ((P x) /\ (P x')) -> x = x')).
Axiom thm_EXISTS_REFL : forall {A : Type'}, forall a : A, exists x : A, x = a.
Axiom thm_EXISTS_UNIQUE_REFL : forall {A : Type'}, forall a : A, @ex1 A (fun x : A => x = a).
Axiom thm_UNWIND_THM1 : forall {A : Type'}, forall P : A -> Prop, forall a : A, (exists x : A, (a = x) /\ (P x)) = (P a).
Axiom thm_UNWIND_THM2 : forall {A : Type'}, forall P : A -> Prop, forall a : A, (exists x : A, (x = a) /\ (P x)) = (P a).
Axiom thm_FORALL_UNWIND_THM2 : forall {A : Type'}, forall P : A -> Prop, forall a : A, (forall x : A, (x = a) -> P x) = (P a).
Axiom thm_FORALL_UNWIND_THM1 : forall {_910 : Type'}, forall P : _910 -> Prop, forall a : _910, (forall x : _910, (a = x) -> P x) = (P a).
Axiom thm_SWAP_FORALL_THM : forall {A B : Type'}, forall P : A -> B -> Prop, (forall x : A, forall y : B, P x y) = (forall y : B, forall x : A, P x y).
Axiom thm_SWAP_EXISTS_THM : forall {A B : Type'}, forall P : A -> B -> Prop, (exists x : A, exists y : B, P x y) = (exists y : B, exists x : A, P x y).
Axiom thm_FORALL_AND_THM : forall {A : Type'}, forall P : A -> Prop, forall Q : A -> Prop, (forall x : A, (P x) /\ (Q x)) = ((forall x : A, P x) /\ (forall x : A, Q x)).
Axiom thm_AND_FORALL_THM : forall {A : Type'}, forall P : A -> Prop, forall Q : A -> Prop, ((forall x : A, P x) /\ (forall x : A, Q x)) = (forall x : A, (P x) /\ (Q x)).
Axiom thm_LEFT_AND_FORALL_THM : forall {A : Type'}, forall P : A -> Prop, forall Q : Prop, ((forall x : A, P x) /\ Q) = (forall x : A, (P x) /\ Q).
Axiom thm_RIGHT_AND_FORALL_THM : forall {A : Type'}, forall P : Prop, forall Q : A -> Prop, (P /\ (forall x : A, Q x)) = (forall x : A, P /\ (Q x)).
Axiom thm_EXISTS_OR_THM : forall {A : Type'}, forall P : A -> Prop, forall Q : A -> Prop, (exists x : A, (P x) \/ (Q x)) = ((exists x : A, P x) \/ (exists x : A, Q x)).
Axiom thm_OR_EXISTS_THM : forall {A : Type'}, forall P : A -> Prop, forall Q : A -> Prop, ((exists x : A, P x) \/ (exists x : A, Q x)) = (exists x : A, (P x) \/ (Q x)).
Axiom thm_LEFT_OR_EXISTS_THM : forall {A : Type'}, forall P : A -> Prop, forall Q : Prop, ((exists x : A, P x) \/ Q) = (exists x : A, (P x) \/ Q).
Axiom thm_RIGHT_OR_EXISTS_THM : forall {A : Type'}, forall P : Prop, forall Q : A -> Prop, (P \/ (exists x : A, Q x)) = (exists x : A, P \/ (Q x)).
Axiom thm_LEFT_EXISTS_AND_THM : forall {A : Type'}, forall P : A -> Prop, forall Q : Prop, (exists x : A, (P x) /\ Q) = ((exists x : A, P x) /\ Q).
Axiom thm_RIGHT_EXISTS_AND_THM : forall {A : Type'}, forall P : Prop, forall Q : A -> Prop, (exists x : A, P /\ (Q x)) = (P /\ (exists x : A, Q x)).
Axiom thm_TRIV_EXISTS_AND_THM : forall {A : Type'}, forall P : Prop, forall Q : Prop, (exists x : A, P /\ Q) = ((exists x : A, P) /\ (exists x : A, Q)).
Axiom thm_LEFT_AND_EXISTS_THM : forall {A : Type'}, forall P : A -> Prop, forall Q : Prop, ((exists x : A, P x) /\ Q) = (exists x : A, (P x) /\ Q).
Axiom thm_RIGHT_AND_EXISTS_THM : forall {A : Type'}, forall P : Prop, forall Q : A -> Prop, (P /\ (exists x : A, Q x)) = (exists x : A, P /\ (Q x)).
Axiom thm_TRIV_AND_EXISTS_THM : forall {A : Type'}, forall P : Prop, forall Q : Prop, ((exists x : A, P) /\ (exists x : A, Q)) = (exists x : A, P /\ Q).
Axiom thm_TRIV_FORALL_OR_THM : forall {A : Type'}, forall P : Prop, forall Q : Prop, (forall x : A, P \/ Q) = ((forall x : A, P) \/ (forall x : A, Q)).
Axiom thm_TRIV_OR_FORALL_THM : forall {A : Type'}, forall P : Prop, forall Q : Prop, ((forall x : A, P) \/ (forall x : A, Q)) = (forall x : A, P \/ Q).
Axiom thm_RIGHT_IMP_FORALL_THM : forall {A : Type'}, forall P : Prop, forall Q : A -> Prop, (P -> forall x : A, Q x) = (forall x : A, P -> Q x).
Axiom thm_RIGHT_FORALL_IMP_THM : forall {A : Type'}, forall P : Prop, forall Q : A -> Prop, (forall x : A, P -> Q x) = (P -> forall x : A, Q x).
Axiom thm_LEFT_IMP_EXISTS_THM : forall {A : Type'}, forall P : A -> Prop, forall Q : Prop, ((exists x : A, P x) -> Q) = (forall x : A, (P x) -> Q).
Axiom thm_LEFT_FORALL_IMP_THM : forall {A : Type'}, forall P : A -> Prop, forall Q : Prop, (forall x : A, (P x) -> Q) = ((exists x : A, P x) -> Q).
Axiom thm_TRIV_FORALL_IMP_THM : forall {A : Type'}, forall P : Prop, forall Q : Prop, (forall x : A, P -> Q) = ((exists x : A, P) -> forall x : A, Q).
Axiom thm_TRIV_EXISTS_IMP_THM : forall {A : Type'}, forall P : Prop, forall Q : Prop, (exists x : A, P -> Q) = ((forall x : A, P) -> exists x : A, Q).
Axiom thm_MONO_FORALL : forall {A : Type'} (P : A -> Prop) (Q : A -> Prop), (forall x : A, (P x) -> Q x) -> (forall x : A, P x) -> forall x : A, Q x.
Axiom thm_MONO_EXISTS : forall {A : Type'} (P : A -> Prop) (Q : A -> Prop), (forall x : A, (P x) -> Q x) -> (exists x : A, P x) -> exists x : A, Q x.
Axiom thm_WLOG_RELATION : forall {_1718 : Type'}, forall R : _1718 -> _1718 -> Prop, forall P : _1718 -> _1718 -> Prop, ((forall x : _1718, forall y : _1718, (P x y) -> P y x) /\ ((forall x : _1718, forall y : _1718, (R x y) \/ (R y x)) /\ (forall x : _1718, forall y : _1718, (R x y) -> P x y))) -> forall x : _1718, forall y : _1718, P x y.
Axiom thm_EXISTS_UNIQUE_ALT : forall {A : Type'}, forall P : A -> Prop, (@ex1 A (fun x : A => P x)) = (exists x : A, forall y : A, (P y) = (x = y)).
Axiom thm_EXISTS_UNIQUE : forall {A : Type'}, forall P : A -> Prop, (@ex1 A (fun x : A => P x)) = (exists x : A, (P x) /\ (forall y : A, (P y) -> y = x)).
Axiom thm_ETA_AX : forall {A B : Type'}, forall t : A -> B, (fun x : A => t x) = t.
Axiom thm_EQ_EXT : forall {A B : Type'}, forall f : A -> B, forall g : A -> B, (forall x : A, (f x) = (g x)) -> f = g.
Axiom thm_FUN_EQ_THM : forall {A B : Type'}, forall f : A -> B, forall g : A -> B, (f = g) = (forall x : A, (f x) = (g x)).
Axiom thm_SELECT_AX : forall {A : Type'}, forall P : A -> Prop, forall x : A, (P x) -> P (@ε A P).
Axiom thm_EXISTS_THM : forall {A : Type'}, (@ex A) = (fun P : A -> Prop => P (@ε A P)).
Axiom thm_SELECT_REFL : forall {A : Type'}, forall x : A, (@ε A (fun y : A => y = x)) = x.
Axiom thm_SELECT_UNIQUE : forall {A : Type'}, forall P : A -> Prop, forall x : A, (forall y : A, (P y) = (y = x)) -> (@ε A P) = x.
Axiom thm_EXCLUDED_MIDDLE : forall t : Prop, t \/ (~ t).
Axiom thm_BOOL_CASES_AX : forall t : Prop, (t = True) \/ (t = False).
Axiom thm_DE_MORGAN_THM : forall t1 : Prop, forall t2 : Prop, ((~ (t1 /\ t2)) = ((~ t1) \/ (~ t2))) /\ ((~ (t1 \/ t2)) = ((~ t1) /\ (~ t2))).
Axiom thm_NOT_CLAUSES : (forall t : Prop, (~ (~ t)) = t) /\ (((~ True) = False) /\ ((~ False) = True)).
Axiom thm_NOT_IMP : forall t1 : Prop, forall t2 : Prop, (~ (t1 -> t2)) = (t1 /\ (~ t2)).
Axiom thm_CONTRAPOS_THM : forall t1 : Prop, forall t2 : Prop, ((~ t1) -> ~ t2) = (t2 -> t1).
Axiom thm_NOT_EXISTS_THM : forall {A : Type'}, forall P : A -> Prop, (~ (exists x : A, P x)) = (forall x : A, ~ (P x)).
Axiom thm_EXISTS_NOT_THM : forall {A : Type'}, forall P : A -> Prop, (exists x : A, ~ (P x)) = (~ (forall x : A, P x)).
Axiom thm_NOT_FORALL_THM : forall {A : Type'}, forall P : A -> Prop, (~ (forall x : A, P x)) = (exists x : A, ~ (P x)).
Axiom thm_FORALL_NOT_THM : forall {A : Type'}, forall P : A -> Prop, (forall x : A, ~ (P x)) = (~ (exists x : A, P x)).
Axiom thm_FORALL_BOOL_THM : forall (P : Prop -> Prop), (forall b : Prop, P b) = ((P True) /\ (P False)).
Axiom thm_EXISTS_BOOL_THM : forall (P : Prop -> Prop), (exists b : Prop, P b) = ((P True) \/ (P False)).
Axiom thm_LEFT_FORALL_OR_THM : forall {A : Type'}, forall P : A -> Prop, forall Q : Prop, (forall x : A, (P x) \/ Q) = ((forall x : A, P x) \/ Q).
Axiom thm_RIGHT_FORALL_OR_THM : forall {A : Type'}, forall P : Prop, forall Q : A -> Prop, (forall x : A, P \/ (Q x)) = (P \/ (forall x : A, Q x)).
Axiom thm_LEFT_OR_FORALL_THM : forall {A : Type'}, forall P : A -> Prop, forall Q : Prop, ((forall x : A, P x) \/ Q) = (forall x : A, (P x) \/ Q).
Axiom thm_RIGHT_OR_FORALL_THM : forall {A : Type'}, forall P : Prop, forall Q : A -> Prop, (P \/ (forall x : A, Q x)) = (forall x : A, P \/ (Q x)).
Axiom thm_LEFT_IMP_FORALL_THM : forall {A : Type'}, forall P : A -> Prop, forall Q : Prop, ((forall x : A, P x) -> Q) = (exists x : A, (P x) -> Q).
Axiom thm_LEFT_EXISTS_IMP_THM : forall {A : Type'}, forall P : A -> Prop, forall Q : Prop, (exists x : A, (P x) -> Q) = ((forall x : A, P x) -> Q).
Axiom thm_RIGHT_IMP_EXISTS_THM : forall {A : Type'}, forall P : Prop, forall Q : A -> Prop, (P -> exists x : A, Q x) = (exists x : A, P -> Q x).
Axiom thm_RIGHT_EXISTS_IMP_THM : forall {A : Type'}, forall P : Prop, forall Q : A -> Prop, (exists x : A, P -> Q x) = (P -> exists x : A, Q x).
Axiom thm_COND_DEF : forall {A : Type'}, (@COND A) = (fun t : Prop => fun t1 : A => fun t2 : A => @ε A (fun x : A => ((t = True) -> x = t1) /\ ((t = False) -> x = t2))).
Axiom thm_COND_CLAUSES : forall {A : Type'}, forall t1 : A, forall t2 : A, ((@COND A True t1 t2) = t1) /\ ((@COND A False t1 t2) = t2).
Axiom thm_COND_EXPAND : forall b : Prop, forall t1 : Prop, forall t2 : Prop, (@COND Prop b t1 t2) = (((~ b) \/ t1) /\ (b \/ t2)).
Axiom thm_COND_ID : forall {A : Type'}, forall b : Prop, forall t : A, (@COND A b t t) = t.
Axiom thm_COND_RAND : forall {A B : Type'}, forall b : Prop, forall f : A -> B, forall x : A, forall y : A, (f (@COND A b x y)) = (@COND B b (f x) (f y)).
Axiom thm_COND_RATOR : forall {A B : Type'}, forall b : Prop, forall f : A -> B, forall g : A -> B, forall x : A, (@COND (A -> B) b f g x) = (@COND B b (f x) (g x)).
Axiom thm_COND_ABS : forall {A B : Type'}, forall b : Prop, forall f : A -> B, forall g : A -> B, (fun x : A => @COND B b (f x) (g x)) = (@COND (A -> B) b f g).
Axiom thm_COND_SWAP : forall {A : Type'}, forall p : Prop, forall x : A, forall y : A, (@COND A (~ p) x y) = (@COND A p y x).
Axiom thm_MONO_COND : forall (A : Prop) (C : Prop) (b : Prop) (B : Prop) (D : Prop), ((A -> B) /\ (C -> D)) -> (@COND Prop b A C) -> @COND Prop b B D.
Axiom thm_COND_ELIM_THM : forall {A : Type'} (x : A) (c : Prop) (P : A -> Prop) (y : A), (P (@COND A c x y)) = ((c -> P x) /\ ((~ c) -> P y)).
Axiom thm_SKOLEM_THM : forall {A B : Type'}, forall P : A -> B -> Prop, (forall x : A, exists y : B, P x y) = (exists y : A -> B, forall x : A, P x (y x)).
Axiom thm_SKOLEM_THM_GEN : forall {_2843 _2845 : Type'}, forall P : _2845 -> Prop, forall R : _2845 -> _2843 -> Prop, (forall x : _2845, (P x) -> exists y : _2843, R x y) = (exists f : _2845 -> _2843, forall x : _2845, (P x) -> R x (f x)).
Axiom thm_UNIQUE_SKOLEM_ALT : forall {A B : Type'}, forall P : A -> B -> Prop, (forall x : A, @ex1 B (fun y : B => P x y)) = (exists f : A -> B, forall x : A, forall y : B, (P x y) = ((f x) = y)).
Axiom thm_UNIQUE_SKOLEM_THM : forall {A B : Type'}, forall P : A -> B -> Prop, (forall x : A, @ex1 B (fun y : B => P x y)) = (@ex1 (A -> B) (fun f : A -> B => forall x : A, P x (f x))).
Axiom thm_bool_INDUCT : forall P : Prop -> Prop, ((P False) /\ (P True)) -> forall x : Prop, P x.
Axiom thm_bool_RECURSION : forall {A : Type'}, forall a : A, forall b : A, exists f : Prop -> A, ((f False) = a) /\ ((f True) = b).
Axiom thm_o_DEF : forall {A B C : Type'}, forall f : B -> C, forall g : A -> B, (@o A B C f g) = (fun x : A => f (g x)).
Axiom thm_I_DEF : forall {A : Type'}, (@I A) = (fun x : A => x).
Axiom thm_o_THM : forall {A B C : Type'}, forall f : B -> C, forall g : A -> B, forall x : A, (@o A B C f g x) = (f (g x)).
Axiom thm_o_ASSOC : forall {A B C D : Type'}, forall f : C -> D, forall g : B -> C, forall h : A -> B, (@o A C D f (@o A B C g h)) = (@o A B D (@o B C D f g) h).
Axiom thm_I_THM : forall {A : Type'}, forall x : A, (@I A x) = x.
Axiom thm_I_O_ID : forall {A B : Type'}, forall f : A -> B, ((@o A B B (@I B) f) = f) /\ ((@o A A B f (@I A)) = f).
Axiom thm_EXISTS_ONE_REP : exists b : Prop, b.
Axiom thm_one_DEF : tt = (@ε unit (fun x : unit => True)).
Axiom thm_one : forall v : unit, v = tt.
Axiom thm_one_axiom : forall {A : Type'}, forall f : A -> unit, forall g : A -> unit, f = g.
Axiom thm_one_INDUCT : forall P : unit -> Prop, (P tt) -> forall x : unit, P x.
Axiom thm_one_RECURSION : forall {A : Type'}, forall e : A, exists fn : unit -> A, (fn tt) = e.
Axiom thm_one_Axiom : forall {A : Type'}, forall e : A, @ex1 (unit -> A) (fun fn : unit -> A => (fn tt) = e).
Axiom thm_FORALL_ONE_THM : forall (P : unit -> Prop), (forall x : unit, P x) = (P tt).
Axiom thm_EXISTS_ONE_THM : forall (P : unit -> Prop), (exists x : unit, P x) = (P tt).
Axiom thm_LET_DEF : forall {A B : Type'}, forall f : A -> B, forall x : A, (@LET A B f x) = (f x).
Axiom thm_LET_END_DEF : forall {A : Type'}, forall t : A, (@LET_END A t) = t.
Axiom thm_GABS_DEF : forall {A : Type'}, forall P : A -> Prop, (@GABS A P) = (@ε A P).
Axiom thm_GEQ_DEF : forall {A : Type'}, forall a : A, forall b : A, (@GEQ A a b) = (a = b).
Axiom thm__SEQPATTERN : forall {_4611 _4614 : Type'}, (@_SEQPATTERN _4611 _4614) = (fun r : _4614 -> _4611 -> Prop => fun s : _4614 -> _4611 -> Prop => fun x : _4614 => @COND (_4611 -> Prop) (exists y : _4611, r x y) (r x) (s x)).
Axiom thm__UNGUARDED_PATTERN : _UNGUARDED_PATTERN = (fun p : Prop => fun r : Prop => p /\ r).
Axiom thm__GUARDED_PATTERN : _GUARDED_PATTERN = (fun p : Prop => fun g : Prop => fun r : Prop => p /\ (g /\ r)).
Axiom thm__MATCH : forall {_4656 _4660 : Type'}, (@_MATCH _4656 _4660) = (fun e : _4656 => fun r : _4656 -> _4660 -> Prop => @COND _4660 (@ex1 _4660 (r e)) (@ε _4660 (r e)) (@ε _4660 (fun z : _4660 => False))).
Axiom thm__FUNCTION : forall {_4678 _4682 : Type'}, (@_FUNCTION _4678 _4682) = (fun r : _4678 -> _4682 -> Prop => fun x : _4678 => @COND _4682 (@ex1 _4682 (r x)) (@ε _4682 (r x)) (@ε _4682 (fun z : _4682 => False))).
Axiom thm_mk_pair_def : forall {A B : Type'}, forall x : A, forall y : B, (@mk_pair A B x y) = (fun a : A => fun b : B => (a = x) /\ (b = y)).
Axiom thm_PAIR_EXISTS_THM : forall {A B : Type'}, exists x : A -> B -> Prop, exists a : A, exists b : B, x = (@mk_pair A B a b).
Axiom thm_REP_ABS_PAIR : forall {A B : Type'}, forall x : A, forall y : B, (@REP_prod A B (@ABS_prod A B (@mk_pair A B x y))) = (@mk_pair A B x y).
Axiom thm_COMMA_DEF : forall {A B : Type'}, forall x : A, forall y : B, (@pair A B x y) = (@ABS_prod A B (@mk_pair A B x y)).
Axiom thm_FST_DEF : forall {A B : Type'}, forall p : prod A B, (@fst A B p) = (@ε A (fun x : A => exists y : B, p = (@pair A B x y))).
Axiom thm_SND_DEF : forall {A B : Type'}, forall p : prod A B, (@snd A B p) = (@ε B (fun y : B => exists x : A, p = (@pair A B x y))).
Axiom thm_PAIR_EQ : forall {A B : Type'}, forall x : A, forall y : B, forall a : A, forall b : B, ((@pair A B x y) = (@pair A B a b)) = ((x = a) /\ (y = b)).
Axiom thm_PAIR_SURJECTIVE : forall {A B : Type'}, forall p : prod A B, exists x : A, exists y : B, p = (@pair A B x y).
Axiom thm_FST : forall {A B : Type'}, forall x : A, forall y : B, (@fst A B (@pair A B x y)) = x.
Axiom thm_SND : forall {A B : Type'}, forall x : A, forall y : B, (@snd A B (@pair A B x y)) = y.
Axiom thm_PAIR : forall {A B : Type'}, forall x : prod A B, (@pair A B (@fst A B x) (@snd A B x)) = x.
Axiom thm_pair_INDUCT : forall {_4949 _4950 : Type'}, forall P : (prod _4950 _4949) -> Prop, (forall x : _4950, forall y : _4949, P (@pair _4950 _4949 x y)) -> forall p : prod _4950 _4949, P p.
Axiom thm_pair_RECURSION : forall {A B C : Type'}, forall PAIR' : A -> B -> C, exists fn : (prod A B) -> C, forall a0 : A, forall a1 : B, (fn (@pair A B a0 a1)) = (PAIR' a0 a1).
Axiom thm_CURRY_DEF : forall {A B C : Type'}, forall f : (prod A B) -> C, forall x : A, forall y : B, (@CURRY A B C f x y) = (f (@pair A B x y)).
Axiom thm_UNCURRY_DEF : forall {A B C : Type'}, forall f : A -> B -> C, forall x : A, forall y : B, (@UNCURRY A B C f (@pair A B x y)) = (f x y).
Axiom thm_PASSOC_DEF : forall {A B C D : Type'}, forall f : (prod (prod A B) C) -> D, forall x : A, forall y : B, forall z : C, (@PASSOC A B C D f (@pair A (prod B C) x (@pair B C y z))) = (f (@pair (prod A B) C (@pair A B x y) z)).
Axiom thm_FORALL_PAIR_THM : forall {_5106 _5107 : Type'}, forall P : (prod _5107 _5106) -> Prop, (forall p : prod _5107 _5106, P p) = (forall p1 : _5107, forall p2 : _5106, P (@pair _5107 _5106 p1 p2)).
Axiom thm_EXISTS_PAIR_THM : forall {_5131 _5132 : Type'}, forall P : (prod _5132 _5131) -> Prop, (exists p : prod _5132 _5131, P p) = (exists p1 : _5132, exists p2 : _5131, P (@pair _5132 _5131 p1 p2)).
Axiom thm_LAMBDA_PAIR_THM : forall {_5146 _5153 _5154 : Type'}, forall t : (prod _5154 _5153) -> _5146, (fun p : prod _5154 _5153 => t p) = (@GABS ((prod _5154 _5153) -> _5146) (fun f : (prod _5154 _5153) -> _5146 => forall x : _5154, forall y : _5153, @GEQ _5146 (f (@pair _5154 _5153 x y)) (t (@pair _5154 _5153 x y)))).
Axiom thm_LAMBDA_PAIR : forall {A B C : Type'}, forall f : A -> B -> C, (@GABS ((prod A B) -> C) (fun f' : (prod A B) -> C => forall x : A, forall y : B, @GEQ C (f' (@pair A B x y)) (f x y))) = (fun p : prod A B => f (@fst A B p) (@snd A B p)).
Axiom thm_LAMBDA_TRIPLE_THM : forall {A B C D : Type'}, forall f : (prod A (prod B C)) -> D, (fun t : prod A (prod B C) => f t) = (@GABS ((prod A (prod B C)) -> D) (fun f' : (prod A (prod B C)) -> D => forall x : A, forall y : B, forall z : C, @GEQ D (f' (@pair A (prod B C) x (@pair B C y z))) (f (@pair A (prod B C) x (@pair B C y z))))).
Axiom thm_LAMBDA_TRIPLE : forall {A B C D : Type'}, forall f : A -> B -> C -> D, (@GABS ((prod A (prod B C)) -> D) (fun f' : (prod A (prod B C)) -> D => forall x : A, forall y : B, forall z : C, @GEQ D (f' (@pair A (prod B C) x (@pair B C y z))) (f x y z))) = (fun t : prod A (prod B C) => f (@fst A (prod B C) t) (@fst B C (@snd A (prod B C) t)) (@snd B C (@snd A (prod B C) t))).
Axiom thm_PAIRED_ETA_THM : forall {_5264 _5271 _5272 _5284 _5296 _5299 _5300 _5310 _5326 _5330 _5333 _5334 : Type'}, (forall f : (prod _5272 _5271) -> _5264, (@GABS ((prod _5272 _5271) -> _5264) (fun f' : (prod _5272 _5271) -> _5264 => forall x : _5272, forall y : _5271, @GEQ _5264 (f' (@pair _5272 _5271 x y)) (f (@pair _5272 _5271 x y)))) = f) /\ ((forall f : (prod _5296 (prod _5300 _5299)) -> _5284, (@GABS ((prod _5296 (prod _5300 _5299)) -> _5284) (fun f' : (prod _5296 (prod _5300 _5299)) -> _5284 => forall x : _5296, forall y : _5300, forall z : _5299, @GEQ _5284 (f' (@pair _5296 (prod _5300 _5299) x (@pair _5300 _5299 y z))) (f (@pair _5296 (prod _5300 _5299) x (@pair _5300 _5299 y z))))) = f) /\ (forall f : (prod _5326 (prod _5330 (prod _5334 _5333))) -> _5310, (@GABS ((prod _5326 (prod _5330 (prod _5334 _5333))) -> _5310) (fun f' : (prod _5326 (prod _5330 (prod _5334 _5333))) -> _5310 => forall w : _5326, forall x : _5330, forall y : _5334, forall z : _5333, @GEQ _5310 (f' (@pair _5326 (prod _5330 (prod _5334 _5333)) w (@pair _5330 (prod _5334 _5333) x (@pair _5334 _5333 y z)))) (f (@pair _5326 (prod _5330 (prod _5334 _5333)) w (@pair _5330 (prod _5334 _5333) x (@pair _5334 _5333 y z)))))) = f)).
Axiom thm_FORALL_UNCURRY : forall {A B C : Type'}, forall P : (A -> B -> C) -> Prop, (forall f : A -> B -> C, P f) = (forall f : (prod A B) -> C, P (fun a : A => fun b : B => f (@pair A B a b))).
Axiom thm_EXISTS_UNCURRY : forall {A B C : Type'}, forall P : (A -> B -> C) -> Prop, (exists f : A -> B -> C, P f) = (exists f : (prod A B) -> C, P (fun a : A => fun b : B => f (@pair A B a b))).
Axiom thm_EXISTS_CURRY : forall {_5431 _5437 _5438 : Type'}, forall P : ((prod _5438 _5437) -> _5431) -> Prop, (exists f : (prod _5438 _5437) -> _5431, P f) = (exists f : _5438 -> _5437 -> _5431, P (@GABS ((prod _5438 _5437) -> _5431) (fun f' : (prod _5438 _5437) -> _5431 => forall a : _5438, forall b : _5437, @GEQ _5431 (f' (@pair _5438 _5437 a b)) (f a b)))).
Axiom thm_FORALL_CURRY : forall {_5456 _5462 _5463 : Type'}, forall P : ((prod _5463 _5462) -> _5456) -> Prop, (forall f : (prod _5463 _5462) -> _5456, P f) = (forall f : _5463 -> _5462 -> _5456, P (@GABS ((prod _5463 _5462) -> _5456) (fun f' : (prod _5463 _5462) -> _5456 => forall a : _5463, forall b : _5462, @GEQ _5456 (f' (@pair _5463 _5462 a b)) (f a b)))).
Axiom thm_FORALL_UNPAIR_THM : forall {_5486 _5488 : Type'}, forall P : _5486 -> _5488 -> Prop, (forall x : _5486, forall y : _5488, P x y) = (forall z : prod _5486 _5488, P (@fst _5486 _5488 z) (@snd _5486 _5488 z)).
Axiom thm_EXISTS_UNPAIR_THM : forall {_5515 _5517 : Type'}, forall P : _5515 -> _5517 -> Prop, (exists x : _5515, exists y : _5517, P x y) = (exists z : prod _5515 _5517, P (@fst _5515 _5517 z) (@snd _5515 _5517 z)).
Axiom thm_FORALL_PAIR_FUN_THM : forall {A B C : Type'}, forall P : (A -> prod B C) -> Prop, (forall f : A -> prod B C, P f) = (forall g : A -> B, forall h : A -> C, P (fun a : A => @pair B C (g a) (h a))).
Axiom thm_EXISTS_PAIR_FUN_THM : forall {A B C : Type'}, forall P : (A -> prod B C) -> Prop, (exists f : A -> prod B C, P f) = (exists g : A -> B, exists h : A -> C, P (fun a : A => @pair B C (g a) (h a))).
Axiom thm_FORALL_UNPAIR_FUN_THM : forall {_5621 _5630 _5631 : Type'}, forall P : (_5621 -> _5630) -> (_5621 -> _5631) -> Prop, (forall f : _5621 -> _5630, forall g : _5621 -> _5631, P f g) = (forall h : _5621 -> prod _5630 _5631, P (@o _5621 (prod _5630 _5631) _5630 (@fst _5630 _5631) h) (@o _5621 (prod _5630 _5631) _5631 (@snd _5630 _5631) h)).
Axiom thm_EXISTS_UNPAIR_FUN_THM : forall {_5658 _5667 _5668 : Type'}, forall P : (_5658 -> _5667) -> (_5658 -> _5668) -> Prop, (exists f : _5658 -> _5667, exists g : _5658 -> _5668, P f g) = (exists h : _5658 -> prod _5667 _5668, P (@o _5658 (prod _5667 _5668) _5667 (@fst _5667 _5668) h) (@o _5658 (prod _5667 _5668) _5668 (@snd _5667 _5668) h)).
Axiom thm_EXISTS_SWAP_FUN_THM : forall {A B C : Type'}, forall P : (A -> B -> C) -> Prop, (exists f : A -> B -> C, P f) = (exists f : B -> A -> C, P (fun a : A => fun b : B => f b a)).
Axiom thm_FORALL_PAIRED_THM : forall {_5733 _5734 : Type'}, forall P : _5734 -> _5733 -> Prop, (all (@GABS ((prod _5734 _5733) -> Prop) (fun f : (prod _5734 _5733) -> Prop => forall x : _5734, forall y : _5733, @GEQ Prop (f (@pair _5734 _5733 x y)) (P x y)))) = (forall x : _5734, forall y : _5733, P x y).
Axiom thm_EXISTS_PAIRED_THM : forall {_5769 _5770 : Type'}, forall P : _5770 -> _5769 -> Prop, (ex (@GABS ((prod _5770 _5769) -> Prop) (fun f : (prod _5770 _5769) -> Prop => forall x : _5770, forall y : _5769, @GEQ Prop (f (@pair _5770 _5769 x y)) (P x y)))) = (exists x : _5770, exists y : _5769, P x y).
Axiom thm_FORALL_TRIPLED_THM : forall {_5805 _5806 _5807 : Type'}, forall P : _5807 -> _5806 -> _5805 -> Prop, (all (@GABS ((prod _5807 (prod _5806 _5805)) -> Prop) (fun f : (prod _5807 (prod _5806 _5805)) -> Prop => forall x : _5807, forall y : _5806, forall z : _5805, @GEQ Prop (f (@pair _5807 (prod _5806 _5805) x (@pair _5806 _5805 y z))) (P x y z)))) = (forall x : _5807, forall y : _5806, forall z : _5805, P x y z).
Axiom thm_EXISTS_TRIPLED_THM : forall {_5851 _5852 _5853 : Type'}, forall P : _5853 -> _5852 -> _5851 -> Prop, (ex (@GABS ((prod _5853 (prod _5852 _5851)) -> Prop) (fun f : (prod _5853 (prod _5852 _5851)) -> Prop => forall x : _5853, forall y : _5852, forall z : _5851, @GEQ Prop (f (@pair _5853 (prod _5852 _5851) x (@pair _5852 _5851 y z))) (P x y z)))) = (exists x : _5853, exists y : _5852, exists z : _5851, P x y z).
Axiom thm_CHOICE_UNPAIR_THM : forall {A B : Type'}, forall P : A -> B -> Prop, (@ε (prod A B) (@GABS ((prod A B) -> Prop) (fun f : (prod A B) -> Prop => forall x : A, forall y : B, @GEQ Prop (f (@pair A B x y)) (P x y)))) = (@ε (prod A B) (fun p : prod A B => P (@fst A B p) (@snd A B p))).
Axiom thm_CHOICE_PAIRED_THM : forall {A B : Type'}, forall P : A -> B -> Prop, forall Q : (prod A B) -> Prop, ((exists x : A, exists y : B, P x y) /\ (forall x : A, forall y : B, (P x y) -> Q (@pair A B x y))) -> Q (@ε (prod A B) (@GABS ((prod A B) -> Prop) (fun f : (prod A B) -> Prop => forall x : A, forall y : B, @GEQ Prop (f (@pair A B x y)) (P x y)))).
Axiom thm_ONE_ONE : forall {A B : Type'}, forall f : A -> B, (@ONE_ONE A B f) = (forall x1 : A, forall x2 : A, ((f x1) = (f x2)) -> x1 = x2).
Axiom thm_ONTO : forall {A B : Type'}, forall f : A -> B, (@ONTO A B f) = (forall y : B, exists x : A, y = (f x)).
Axiom thm_INFINITY_AX : exists f : ind -> ind, (@ONE_ONE ind ind f) /\ (~ (@ONTO ind ind f)).
Axiom thm_IND_SUC_0_EXISTS : exists f : ind -> ind, exists z : ind, (forall x1 : ind, forall x2 : ind, ((f x1) = (f x2)) = (x1 = x2)) /\ (forall x : ind, ~ ((f x) = z)).
Axiom thm_NUM_REP_RULES : (NUM_REP IND_0) /\ (forall i : ind, (NUM_REP i) -> NUM_REP (IND_SUC i)).
Axiom thm_NUM_REP_CASES : forall a : ind, (NUM_REP a) = ((a = IND_0) \/ (exists i : ind, (a = (IND_SUC i)) /\ (NUM_REP i))).
Axiom thm_NUM_REP_INDUCT : forall NUM_REP' : ind -> Prop, ((NUM_REP' IND_0) /\ (forall i : ind, (NUM_REP' i) -> NUM_REP' (IND_SUC i))) -> forall a : ind, (NUM_REP a) -> NUM_REP' a.
Axiom thm_ZERO_DEF : 0 = (mk_num IND_0).
Axiom thm_SUC_DEF : forall n : nat, (S n) = (mk_num (IND_SUC (dest_num n))).
Axiom thm_SUC_INJ : forall m : nat, forall n : nat, ((S m) = (S n)) = (m = n).
Axiom thm_NOT_SUC : forall n : nat, ~ ((S n) = (NUMERAL 0)).
Axiom thm_num_INDUCTION : forall P : nat -> Prop, ((P (NUMERAL 0)) /\ (forall n : nat, (P n) -> P (S n))) -> forall n : nat, P n.
Axiom thm_num_Axiom : forall {A : Type'}, forall e : A, forall f : A -> nat -> A, @ex1 (nat -> A) (fun fn : nat -> A => ((fn (NUMERAL 0)) = e) /\ (forall n : nat, (fn (S n)) = (f (fn n) n))).
Axiom thm_num_CASES : forall m : nat, (m = (NUMERAL 0)) \/ (exists n : nat, m = (S n)).
Axiom thm_PRE : ((Nat.pred (NUMERAL 0)) = (NUMERAL 0)) /\ (forall n : nat, (Nat.pred (S n)) = n).
Axiom thm_ADD : (forall n : nat, (Nat.add (NUMERAL 0) n) = n) /\ (forall m : nat, forall n : nat, (Nat.add (S m) n) = (S (Nat.add m n))).
Axiom thm_ADD_0 : forall m : nat, (Nat.add m (NUMERAL 0)) = m.
Axiom thm_ADD_SUC : forall m : nat, forall n : nat, (Nat.add m (S n)) = (S (Nat.add m n)).
Axiom thm_ADD_CLAUSES : (forall n : nat, (Nat.add (NUMERAL 0) n) = n) /\ ((forall m : nat, (Nat.add m (NUMERAL 0)) = m) /\ ((forall m : nat, forall n : nat, (Nat.add (S m) n) = (S (Nat.add m n))) /\ (forall m : nat, forall n : nat, (Nat.add m (S n)) = (S (Nat.add m n))))).
Axiom thm_ADD_SYM : forall m : nat, forall n : nat, (Nat.add m n) = (Nat.add n m).
Axiom thm_ADD_ASSOC : forall m : nat, forall n : nat, forall p : nat, (Nat.add m (Nat.add n p)) = (Nat.add (Nat.add m n) p).
Axiom thm_ADD_AC : forall (n : nat) (m : nat) (p : nat), ((Nat.add m n) = (Nat.add n m)) /\ (((Nat.add (Nat.add m n) p) = (Nat.add m (Nat.add n p))) /\ ((Nat.add m (Nat.add n p)) = (Nat.add n (Nat.add m p)))).
Axiom thm_ADD_EQ_0 : forall m : nat, forall n : nat, ((Nat.add m n) = (NUMERAL 0)) = ((m = (NUMERAL 0)) /\ (n = (NUMERAL 0))).
Axiom thm_EQ_ADD_LCANCEL : forall m : nat, forall n : nat, forall p : nat, ((Nat.add m n) = (Nat.add m p)) = (n = p).
Axiom thm_EQ_ADD_RCANCEL : forall m : nat, forall n : nat, forall p : nat, ((Nat.add m p) = (Nat.add n p)) = (m = n).
Axiom thm_EQ_ADD_LCANCEL_0 : forall m : nat, forall n : nat, ((Nat.add m n) = m) = (n = (NUMERAL 0)).
Axiom thm_EQ_ADD_RCANCEL_0 : forall m : nat, forall n : nat, ((Nat.add m n) = n) = (m = (NUMERAL 0)).
Axiom thm_BIT0 : forall n : nat, (BIT0 n) = (Nat.add n n).
Axiom thm_BIT1 : forall n : nat, (BIT1 n) = (S (Nat.add n n)).
Axiom thm_BIT0_THM : forall n : nat, (NUMERAL (BIT0 n)) = (Nat.add (NUMERAL n) (NUMERAL n)).
Axiom thm_BIT1_THM : forall n : nat, (NUMERAL (BIT1 n)) = (S (Nat.add (NUMERAL n) (NUMERAL n))).
Axiom thm_ONE : (NUMERAL (BIT1 0)) = (S (NUMERAL 0)).
Axiom thm_TWO : (NUMERAL (BIT0 (BIT1 0))) = (S (NUMERAL (BIT1 0))).
Axiom thm_ADD1 : forall m : nat, (S m) = (Nat.add m (NUMERAL (BIT1 0))).
Axiom thm_MULT : (forall n : nat, (Nat.mul (NUMERAL 0) n) = (NUMERAL 0)) /\ (forall m : nat, forall n : nat, (Nat.mul (S m) n) = (Nat.add (Nat.mul m n) n)).
Axiom thm_MULT_0 : forall m : nat, (Nat.mul m (NUMERAL 0)) = (NUMERAL 0).
Axiom thm_MULT_SUC : forall m : nat, forall n : nat, (Nat.mul m (S n)) = (Nat.add m (Nat.mul m n)).
Axiom thm_MULT_CLAUSES : (forall n : nat, (Nat.mul (NUMERAL 0) n) = (NUMERAL 0)) /\ ((forall m : nat, (Nat.mul m (NUMERAL 0)) = (NUMERAL 0)) /\ ((forall n : nat, (Nat.mul (NUMERAL (BIT1 0)) n) = n) /\ ((forall m : nat, (Nat.mul m (NUMERAL (BIT1 0))) = m) /\ ((forall m : nat, forall n : nat, (Nat.mul (S m) n) = (Nat.add (Nat.mul m n) n)) /\ (forall m : nat, forall n : nat, (Nat.mul m (S n)) = (Nat.add m (Nat.mul m n))))))).
Axiom thm_MULT_SYM : forall m : nat, forall n : nat, (Nat.mul m n) = (Nat.mul n m).
Axiom thm_LEFT_ADD_DISTRIB : forall m : nat, forall n : nat, forall p : nat, (Nat.mul m (Nat.add n p)) = (Nat.add (Nat.mul m n) (Nat.mul m p)).
Axiom thm_RIGHT_ADD_DISTRIB : forall m : nat, forall n : nat, forall p : nat, (Nat.mul (Nat.add m n) p) = (Nat.add (Nat.mul m p) (Nat.mul n p)).
Axiom thm_MULT_ASSOC : forall m : nat, forall n : nat, forall p : nat, (Nat.mul m (Nat.mul n p)) = (Nat.mul (Nat.mul m n) p).
Axiom thm_MULT_AC : forall (n : nat) (m : nat) (p : nat), ((Nat.mul m n) = (Nat.mul n m)) /\ (((Nat.mul (Nat.mul m n) p) = (Nat.mul m (Nat.mul n p))) /\ ((Nat.mul m (Nat.mul n p)) = (Nat.mul n (Nat.mul m p)))).
Axiom thm_MULT_EQ_0 : forall m : nat, forall n : nat, ((Nat.mul m n) = (NUMERAL 0)) = ((m = (NUMERAL 0)) \/ (n = (NUMERAL 0))).
Axiom thm_EQ_MULT_LCANCEL : forall m : nat, forall n : nat, forall p : nat, ((Nat.mul m n) = (Nat.mul m p)) = ((m = (NUMERAL 0)) \/ (n = p)).
Axiom thm_EQ_MULT_RCANCEL : forall m : nat, forall n : nat, forall p : nat, ((Nat.mul m p) = (Nat.mul n p)) = ((m = n) \/ (p = (NUMERAL 0))).
Axiom thm_MULT_2 : forall n : nat, (Nat.mul (NUMERAL (BIT0 (BIT1 0))) n) = (Nat.add n n).
Axiom thm_MULT_EQ_1 : forall m : nat, forall n : nat, ((Nat.mul m n) = (NUMERAL (BIT1 0))) = ((m = (NUMERAL (BIT1 0))) /\ (n = (NUMERAL (BIT1 0)))).
Axiom thm_EXP : (forall m : nat, (Nat.pow m (NUMERAL 0)) = (NUMERAL (BIT1 0))) /\ (forall m : nat, forall n : nat, (Nat.pow m (S n)) = (Nat.mul m (Nat.pow m n))).
Axiom thm_EXP_EQ_0 : forall m : nat, forall n : nat, ((Nat.pow m n) = (NUMERAL 0)) = ((m = (NUMERAL 0)) /\ (~ (n = (NUMERAL 0)))).
Axiom thm_EXP_EQ_1 : forall x : nat, forall n : nat, ((Nat.pow x n) = (NUMERAL (BIT1 0))) = ((x = (NUMERAL (BIT1 0))) \/ (n = (NUMERAL 0))).
Axiom thm_EXP_ZERO : forall n : nat, (Nat.pow (NUMERAL 0) n) = (@COND nat (n = (NUMERAL 0)) (NUMERAL (BIT1 0)) (NUMERAL 0)).
Axiom thm_EXP_ADD : forall m : nat, forall n : nat, forall p : nat, (Nat.pow m (Nat.add n p)) = (Nat.mul (Nat.pow m n) (Nat.pow m p)).
Axiom thm_EXP_ONE : forall n : nat, (Nat.pow (NUMERAL (BIT1 0)) n) = (NUMERAL (BIT1 0)).
Axiom thm_EXP_1 : forall n : nat, (Nat.pow n (NUMERAL (BIT1 0))) = n.
Axiom thm_EXP_2 : forall n : nat, (Nat.pow n (NUMERAL (BIT0 (BIT1 0)))) = (Nat.mul n n).
Axiom thm_MULT_EXP : forall p : nat, forall m : nat, forall n : nat, (Nat.pow (Nat.mul m n) p) = (Nat.mul (Nat.pow m p) (Nat.pow n p)).
Axiom thm_EXP_MULT : forall m : nat, forall n : nat, forall p : nat, (Nat.pow m (Nat.mul n p)) = (Nat.pow (Nat.pow m n) p).
Axiom thm_EXP_EXP : forall x : nat, forall m : nat, forall n : nat, (Nat.pow (Nat.pow x m) n) = (Nat.pow x (Nat.mul m n)).
Axiom thm_LE : (forall m : nat, (Peano.le m (NUMERAL 0)) = (m = (NUMERAL 0))) /\ (forall m : nat, forall n : nat, (Peano.le m (S n)) = ((m = (S n)) \/ (Peano.le m n))).
Axiom thm_LT : (forall m : nat, (Peano.lt m (NUMERAL 0)) = False) /\ (forall m : nat, forall n : nat, (Peano.lt m (S n)) = ((m = n) \/ (Peano.lt m n))).
Axiom thm_GE : forall n : nat, forall m : nat, (Peano.ge m n) = (Peano.le n m).
Axiom thm_GT : forall n : nat, forall m : nat, (Peano.gt m n) = (Peano.lt n m).
Axiom thm_MAX : forall m : nat, forall n : nat, (Nat.max m n) = (@COND nat (Peano.le m n) n m).
Axiom thm_MIN : forall m : nat, forall n : nat, (Nat.min m n) = (@COND nat (Peano.le m n) m n).
Axiom thm_LE_SUC_LT : forall m : nat, forall n : nat, (Peano.le (S m) n) = (Peano.lt m n).
Axiom thm_LT_SUC_LE : forall m : nat, forall n : nat, (Peano.lt m (S n)) = (Peano.le m n).
Axiom thm_LE_SUC : forall m : nat, forall n : nat, (Peano.le (S m) (S n)) = (Peano.le m n).
Axiom thm_LT_SUC : forall m : nat, forall n : nat, (Peano.lt (S m) (S n)) = (Peano.lt m n).
Axiom thm_LE_0 : forall n : nat, Peano.le (NUMERAL 0) n.
Axiom thm_LT_0 : forall n : nat, Peano.lt (NUMERAL 0) (S n).
Axiom thm_LE_REFL : forall n : nat, Peano.le n n.
Axiom thm_LT_REFL : forall n : nat, ~ (Peano.lt n n).
Axiom thm_LT_IMP_NE : forall m : nat, forall n : nat, (Peano.lt m n) -> ~ (m = n).
Axiom thm_LE_ANTISYM : forall m : nat, forall n : nat, ((Peano.le m n) /\ (Peano.le n m)) = (m = n).
Axiom thm_LT_ANTISYM : forall m : nat, forall n : nat, ~ ((Peano.lt m n) /\ (Peano.lt n m)).
Axiom thm_LET_ANTISYM : forall m : nat, forall n : nat, ~ ((Peano.le m n) /\ (Peano.lt n m)).
Axiom thm_LTE_ANTISYM : forall m : nat, forall n : nat, ~ ((Peano.lt m n) /\ (Peano.le n m)).
Axiom thm_LE_TRANS : forall m : nat, forall n : nat, forall p : nat, ((Peano.le m n) /\ (Peano.le n p)) -> Peano.le m p.
Axiom thm_LT_TRANS : forall m : nat, forall n : nat, forall p : nat, ((Peano.lt m n) /\ (Peano.lt n p)) -> Peano.lt m p.
Axiom thm_LET_TRANS : forall m : nat, forall n : nat, forall p : nat, ((Peano.le m n) /\ (Peano.lt n p)) -> Peano.lt m p.
Axiom thm_LTE_TRANS : forall m : nat, forall n : nat, forall p : nat, ((Peano.lt m n) /\ (Peano.le n p)) -> Peano.lt m p.
Axiom thm_LE_CASES : forall m : nat, forall n : nat, (Peano.le m n) \/ (Peano.le n m).
Axiom thm_LT_CASES : forall m : nat, forall n : nat, (Peano.lt m n) \/ ((Peano.lt n m) \/ (m = n)).
Axiom thm_LET_CASES : forall m : nat, forall n : nat, (Peano.le m n) \/ (Peano.lt n m).
Axiom thm_LTE_CASES : forall m : nat, forall n : nat, (Peano.lt m n) \/ (Peano.le n m).
Axiom thm_LE_LT : forall m : nat, forall n : nat, (Peano.le m n) = ((Peano.lt m n) \/ (m = n)).
Axiom thm_LT_LE : forall m : nat, forall n : nat, (Peano.lt m n) = ((Peano.le m n) /\ (~ (m = n))).
Axiom thm_NOT_LE : forall m : nat, forall n : nat, (~ (Peano.le m n)) = (Peano.lt n m).
Axiom thm_NOT_LT : forall m : nat, forall n : nat, (~ (Peano.lt m n)) = (Peano.le n m).
Axiom thm_LT_IMP_LE : forall m : nat, forall n : nat, (Peano.lt m n) -> Peano.le m n.
Axiom thm_EQ_IMP_LE : forall m : nat, forall n : nat, (m = n) -> Peano.le m n.
Axiom thm_LT_NZ : forall n : nat, (Peano.lt (NUMERAL 0) n) = (~ (n = (NUMERAL 0))).
Axiom thm_LE_1 : (forall n : nat, (~ (n = (NUMERAL 0))) -> Peano.lt (NUMERAL 0) n) /\ ((forall n : nat, (~ (n = (NUMERAL 0))) -> Peano.le (NUMERAL (BIT1 0)) n) /\ ((forall n : nat, (Peano.lt (NUMERAL 0) n) -> ~ (n = (NUMERAL 0))) /\ ((forall n : nat, (Peano.lt (NUMERAL 0) n) -> Peano.le (NUMERAL (BIT1 0)) n) /\ ((forall n : nat, (Peano.le (NUMERAL (BIT1 0)) n) -> Peano.lt (NUMERAL 0) n) /\ (forall n : nat, (Peano.le (NUMERAL (BIT1 0)) n) -> ~ (n = (NUMERAL 0))))))).
Axiom thm_LE_EXISTS : forall m : nat, forall n : nat, (Peano.le m n) = (exists d : nat, n = (Nat.add m d)).
Axiom thm_LT_EXISTS : forall m : nat, forall n : nat, (Peano.lt m n) = (exists d : nat, n = (Nat.add m (S d))).
Axiom thm_LE_ADD : forall m : nat, forall n : nat, Peano.le m (Nat.add m n).
Axiom thm_LE_ADDR : forall m : nat, forall n : nat, Peano.le n (Nat.add m n).
Axiom thm_LT_ADD : forall m : nat, forall n : nat, (Peano.lt m (Nat.add m n)) = (Peano.lt (NUMERAL 0) n).
Axiom thm_LT_ADDR : forall m : nat, forall n : nat, (Peano.lt n (Nat.add m n)) = (Peano.lt (NUMERAL 0) m).
Axiom thm_LE_ADD_LCANCEL : forall m : nat, forall n : nat, forall p : nat, (Peano.le (Nat.add m n) (Nat.add m p)) = (Peano.le n p).
Axiom thm_LE_ADD_RCANCEL : forall m : nat, forall n : nat, forall p : nat, (Peano.le (Nat.add m p) (Nat.add n p)) = (Peano.le m n).
Axiom thm_LT_ADD_LCANCEL : forall m : nat, forall n : nat, forall p : nat, (Peano.lt (Nat.add m n) (Nat.add m p)) = (Peano.lt n p).
Axiom thm_LT_ADD_RCANCEL : forall m : nat, forall n : nat, forall p : nat, (Peano.lt (Nat.add m p) (Nat.add n p)) = (Peano.lt m n).
Axiom thm_LE_ADD2 : forall m : nat, forall n : nat, forall p : nat, forall q : nat, ((Peano.le m p) /\ (Peano.le n q)) -> Peano.le (Nat.add m n) (Nat.add p q).
Axiom thm_LET_ADD2 : forall m : nat, forall n : nat, forall p : nat, forall q : nat, ((Peano.le m p) /\ (Peano.lt n q)) -> Peano.lt (Nat.add m n) (Nat.add p q).
Axiom thm_LTE_ADD2 : forall m : nat, forall n : nat, forall p : nat, forall q : nat, ((Peano.lt m p) /\ (Peano.le n q)) -> Peano.lt (Nat.add m n) (Nat.add p q).
Axiom thm_LT_ADD2 : forall m : nat, forall n : nat, forall p : nat, forall q : nat, ((Peano.lt m p) /\ (Peano.lt n q)) -> Peano.lt (Nat.add m n) (Nat.add p q).
Axiom thm_LT_MULT : forall m : nat, forall n : nat, (Peano.lt (NUMERAL 0) (Nat.mul m n)) = ((Peano.lt (NUMERAL 0) m) /\ (Peano.lt (NUMERAL 0) n)).
Axiom thm_LE_MULT2 : forall m : nat, forall n : nat, forall p : nat, forall q : nat, ((Peano.le m n) /\ (Peano.le p q)) -> Peano.le (Nat.mul m p) (Nat.mul n q).
Axiom thm_LT_LMULT : forall m : nat, forall n : nat, forall p : nat, ((~ (m = (NUMERAL 0))) /\ (Peano.lt n p)) -> Peano.lt (Nat.mul m n) (Nat.mul m p).
Axiom thm_LE_MULT_LCANCEL : forall m : nat, forall n : nat, forall p : nat, (Peano.le (Nat.mul m n) (Nat.mul m p)) = ((m = (NUMERAL 0)) \/ (Peano.le n p)).
Axiom thm_LE_MULT_RCANCEL : forall m : nat, forall n : nat, forall p : nat, (Peano.le (Nat.mul m p) (Nat.mul n p)) = ((Peano.le m n) \/ (p = (NUMERAL 0))).
Axiom thm_LT_MULT_LCANCEL : forall m : nat, forall n : nat, forall p : nat, (Peano.lt (Nat.mul m n) (Nat.mul m p)) = ((~ (m = (NUMERAL 0))) /\ (Peano.lt n p)).
Axiom thm_LT_MULT_RCANCEL : forall m : nat, forall n : nat, forall p : nat, (Peano.lt (Nat.mul m p) (Nat.mul n p)) = ((Peano.lt m n) /\ (~ (p = (NUMERAL 0)))).
Axiom thm_LT_MULT2 : forall m : nat, forall n : nat, forall p : nat, forall q : nat, ((Peano.lt m n) /\ (Peano.lt p q)) -> Peano.lt (Nat.mul m p) (Nat.mul n q).
Axiom thm_LE_SQUARE_REFL : forall n : nat, Peano.le n (Nat.mul n n).
Axiom thm_LT_POW2_REFL : forall n : nat, Peano.lt n (Nat.pow (NUMERAL (BIT0 (BIT1 0))) n).
Axiom thm_WLOG_LE : forall (P : nat -> nat -> Prop), ((forall m : nat, forall n : nat, (P m n) = (P n m)) /\ (forall m : nat, forall n : nat, (Peano.le m n) -> P m n)) -> forall m : nat, forall n : nat, P m n.
Axiom thm_WLOG_LT : forall (P : nat -> nat -> Prop), ((forall m : nat, P m m) /\ ((forall m : nat, forall n : nat, (P m n) = (P n m)) /\ (forall m : nat, forall n : nat, (Peano.lt m n) -> P m n))) -> forall m : nat, forall y : nat, P m y.
Axiom thm_WLOG_LE_3 : forall P : nat -> nat -> nat -> Prop, ((forall x : nat, forall y : nat, forall z : nat, (P x y z) -> (P y x z) /\ (P x z y)) /\ (forall x : nat, forall y : nat, forall z : nat, ((Peano.le x y) /\ (Peano.le y z)) -> P x y z)) -> forall x : nat, forall y : nat, forall z : nat, P x y z.
Axiom thm_num_WF : forall P : nat -> Prop, (forall n : nat, (forall m : nat, (Peano.lt m n) -> P m) -> P n) -> forall n : nat, P n.
Axiom thm_num_WOP : forall P : nat -> Prop, (exists n : nat, P n) = (exists n : nat, (P n) /\ (forall m : nat, (Peano.lt m n) -> ~ (P m))).
Axiom thm_num_MAX : forall P : nat -> Prop, ((exists x : nat, P x) /\ (exists M : nat, forall x : nat, (P x) -> Peano.le x M)) = (exists m : nat, (P m) /\ (forall x : nat, (P x) -> Peano.le x m)).
Axiom thm_LE_INDUCT : forall P : nat -> nat -> Prop, ((forall m : nat, P m m) /\ (forall m : nat, forall n : nat, ((Peano.le m n) /\ (P m n)) -> P m (S n))) -> forall m : nat, forall n : nat, (Peano.le m n) -> P m n.
Axiom thm_num_INDUCTION_DOWN : forall P : nat -> Prop, forall m : nat, ((forall n : nat, (Peano.le m n) -> P n) /\ (forall n : nat, ((Peano.lt n m) /\ (P (Nat.add n (NUMERAL (BIT1 0))))) -> P n)) -> forall n : nat, P n.
Axiom thm_EVEN : ((Coq.Arith.PeanoNat.Nat.Even (NUMERAL 0)) = True) /\ (forall n : nat, (Coq.Arith.PeanoNat.Nat.Even (S n)) = (~ (Coq.Arith.PeanoNat.Nat.Even n))).
Axiom thm_ODD : ((Coq.Arith.PeanoNat.Nat.Odd (NUMERAL 0)) = False) /\ (forall n : nat, (Coq.Arith.PeanoNat.Nat.Odd (S n)) = (~ (Coq.Arith.PeanoNat.Nat.Odd n))).
Axiom thm_NOT_EVEN : forall n : nat, (~ (Coq.Arith.PeanoNat.Nat.Even n)) = (Coq.Arith.PeanoNat.Nat.Odd n).
Axiom thm_NOT_ODD : forall n : nat, (~ (Coq.Arith.PeanoNat.Nat.Odd n)) = (Coq.Arith.PeanoNat.Nat.Even n).
Axiom thm_EVEN_OR_ODD : forall n : nat, (Coq.Arith.PeanoNat.Nat.Even n) \/ (Coq.Arith.PeanoNat.Nat.Odd n).
Axiom thm_EVEN_AND_ODD : forall n : nat, ~ ((Coq.Arith.PeanoNat.Nat.Even n) /\ (Coq.Arith.PeanoNat.Nat.Odd n)).
Axiom thm_EVEN_ADD : forall m : nat, forall n : nat, (Coq.Arith.PeanoNat.Nat.Even (Nat.add m n)) = ((Coq.Arith.PeanoNat.Nat.Even m) = (Coq.Arith.PeanoNat.Nat.Even n)).
Axiom thm_EVEN_MULT : forall m : nat, forall n : nat, (Coq.Arith.PeanoNat.Nat.Even (Nat.mul m n)) = ((Coq.Arith.PeanoNat.Nat.Even m) \/ (Coq.Arith.PeanoNat.Nat.Even n)).
Axiom thm_EVEN_EXP : forall m : nat, forall n : nat, (Coq.Arith.PeanoNat.Nat.Even (Nat.pow m n)) = ((Coq.Arith.PeanoNat.Nat.Even m) /\ (~ (n = (NUMERAL 0)))).
Axiom thm_ODD_ADD : forall m : nat, forall n : nat, (Coq.Arith.PeanoNat.Nat.Odd (Nat.add m n)) = (~ ((Coq.Arith.PeanoNat.Nat.Odd m) = (Coq.Arith.PeanoNat.Nat.Odd n))).
Axiom thm_ODD_MULT : forall m : nat, forall n : nat, (Coq.Arith.PeanoNat.Nat.Odd (Nat.mul m n)) = ((Coq.Arith.PeanoNat.Nat.Odd m) /\ (Coq.Arith.PeanoNat.Nat.Odd n)).
Axiom thm_ODD_EXP : forall m : nat, forall n : nat, (Coq.Arith.PeanoNat.Nat.Odd (Nat.pow m n)) = ((Coq.Arith.PeanoNat.Nat.Odd m) \/ (n = (NUMERAL 0))).
Axiom thm_EVEN_DOUBLE : forall n : nat, Coq.Arith.PeanoNat.Nat.Even (Nat.mul (NUMERAL (BIT0 (BIT1 0))) n).
Axiom thm_ODD_DOUBLE : forall n : nat, Coq.Arith.PeanoNat.Nat.Odd (S (Nat.mul (NUMERAL (BIT0 (BIT1 0))) n)).
Axiom thm_EVEN_EXISTS_LEMMA : forall n : nat, ((Coq.Arith.PeanoNat.Nat.Even n) -> exists m : nat, n = (Nat.mul (NUMERAL (BIT0 (BIT1 0))) m)) /\ ((~ (Coq.Arith.PeanoNat.Nat.Even n)) -> exists m : nat, n = (S (Nat.mul (NUMERAL (BIT0 (BIT1 0))) m))).
Axiom thm_EVEN_EXISTS : forall n : nat, (Coq.Arith.PeanoNat.Nat.Even n) = (exists m : nat, n = (Nat.mul (NUMERAL (BIT0 (BIT1 0))) m)).
Axiom thm_ODD_EXISTS : forall n : nat, (Coq.Arith.PeanoNat.Nat.Odd n) = (exists m : nat, n = (S (Nat.mul (NUMERAL (BIT0 (BIT1 0))) m))).
Axiom thm_EVEN_ODD_DECOMPOSITION : forall n : nat, (exists k : nat, exists m : nat, (Coq.Arith.PeanoNat.Nat.Odd m) /\ (n = (Nat.mul (Nat.pow (NUMERAL (BIT0 (BIT1 0))) k) m))) = (~ (n = (NUMERAL 0))).
Axiom thm_SUB : (forall m : nat, (Nat.sub m (NUMERAL 0)) = m) /\ (forall m : nat, forall n : nat, (Nat.sub m (S n)) = (Nat.pred (Nat.sub m n))).
Axiom thm_SUB_0 : forall m : nat, ((Nat.sub (NUMERAL 0) m) = (NUMERAL 0)) /\ ((Nat.sub m (NUMERAL 0)) = m).
Axiom thm_SUB_PRESUC : forall m : nat, forall n : nat, (Nat.pred (Nat.sub (S m) n)) = (Nat.sub m n).
Axiom thm_SUB_SUC : forall m : nat, forall n : nat, (Nat.sub (S m) (S n)) = (Nat.sub m n).
Axiom thm_SUB_REFL : forall n : nat, (Nat.sub n n) = (NUMERAL 0).
Axiom thm_ADD_SUB : forall m : nat, forall n : nat, (Nat.sub (Nat.add m n) n) = m.
Axiom thm_ADD_SUB2 : forall m : nat, forall n : nat, (Nat.sub (Nat.add m n) m) = n.
Axiom thm_SUB_EQ_0 : forall m : nat, forall n : nat, ((Nat.sub m n) = (NUMERAL 0)) = (Peano.le m n).
Axiom thm_ADD_SUBR2 : forall m : nat, forall n : nat, (Nat.sub m (Nat.add m n)) = (NUMERAL 0).
Axiom thm_ADD_SUBR : forall m : nat, forall n : nat, (Nat.sub n (Nat.add m n)) = (NUMERAL 0).
Axiom thm_SUB_ADD : forall m : nat, forall n : nat, (Peano.le n m) -> (Nat.add (Nat.sub m n) n) = m.
Axiom thm_SUB_ADD_LCANCEL : forall m : nat, forall n : nat, forall p : nat, (Nat.sub (Nat.add m n) (Nat.add m p)) = (Nat.sub n p).
Axiom thm_SUB_ADD_RCANCEL : forall m : nat, forall n : nat, forall p : nat, (Nat.sub (Nat.add m p) (Nat.add n p)) = (Nat.sub m n).
Axiom thm_LEFT_SUB_DISTRIB : forall m : nat, forall n : nat, forall p : nat, (Nat.mul m (Nat.sub n p)) = (Nat.sub (Nat.mul m n) (Nat.mul m p)).
Axiom thm_RIGHT_SUB_DISTRIB : forall m : nat, forall n : nat, forall p : nat, (Nat.mul (Nat.sub m n) p) = (Nat.sub (Nat.mul m p) (Nat.mul n p)).
Axiom thm_SUC_SUB1 : forall n : nat, (Nat.sub (S n) (NUMERAL (BIT1 0))) = n.
Axiom thm_EVEN_SUB : forall m : nat, forall n : nat, (Coq.Arith.PeanoNat.Nat.Even (Nat.sub m n)) = ((Peano.le m n) \/ ((Coq.Arith.PeanoNat.Nat.Even m) = (Coq.Arith.PeanoNat.Nat.Even n))).
Axiom thm_ODD_SUB : forall m : nat, forall n : nat, (Coq.Arith.PeanoNat.Nat.Odd (Nat.sub m n)) = ((Peano.lt n m) /\ (~ ((Coq.Arith.PeanoNat.Nat.Odd m) = (Coq.Arith.PeanoNat.Nat.Odd n)))).
Axiom thm_FACT : ((Factorial.fact (NUMERAL 0)) = (NUMERAL (BIT1 0))) /\ (forall n : nat, (Factorial.fact (S n)) = (Nat.mul (S n) (Factorial.fact n))).
Axiom thm_FACT_LT : forall n : nat, Peano.lt (NUMERAL 0) (Factorial.fact n).
Axiom thm_FACT_LE : forall n : nat, Peano.le (NUMERAL (BIT1 0)) (Factorial.fact n).
Axiom thm_FACT_NZ : forall n : nat, ~ ((Factorial.fact n) = (NUMERAL 0)).
Axiom thm_FACT_MONO : forall m : nat, forall n : nat, (Peano.le m n) -> Peano.le (Factorial.fact m) (Factorial.fact n).
Axiom thm_EXP_LT_0 : forall n : nat, forall x : nat, (Peano.lt (NUMERAL 0) (Nat.pow x n)) = ((~ (x = (NUMERAL 0))) \/ (n = (NUMERAL 0))).
Axiom thm_LT_EXP : forall x : nat, forall m : nat, forall n : nat, (Peano.lt (Nat.pow x m) (Nat.pow x n)) = (((Peano.le (NUMERAL (BIT0 (BIT1 0))) x) /\ (Peano.lt m n)) \/ ((x = (NUMERAL 0)) /\ ((~ (m = (NUMERAL 0))) /\ (n = (NUMERAL 0))))).
Axiom thm_LE_EXP : forall x : nat, forall m : nat, forall n : nat, (Peano.le (Nat.pow x m) (Nat.pow x n)) = (@COND Prop (x = (NUMERAL 0)) ((m = (NUMERAL 0)) -> n = (NUMERAL 0)) ((x = (NUMERAL (BIT1 0))) \/ (Peano.le m n))).
Axiom thm_EQ_EXP : forall x : nat, forall m : nat, forall n : nat, ((Nat.pow x m) = (Nat.pow x n)) = (@COND Prop (x = (NUMERAL 0)) ((m = (NUMERAL 0)) = (n = (NUMERAL 0))) ((x = (NUMERAL (BIT1 0))) \/ (m = n))).
Axiom thm_EXP_MONO_LE_IMP : forall x : nat, forall y : nat, forall n : nat, (Peano.le x y) -> Peano.le (Nat.pow x n) (Nat.pow y n).
Axiom thm_EXP_MONO_LT_IMP : forall x : nat, forall y : nat, forall n : nat, ((Peano.lt x y) /\ (~ (n = (NUMERAL 0)))) -> Peano.lt (Nat.pow x n) (Nat.pow y n).
Axiom thm_EXP_MONO_LE : forall x : nat, forall y : nat, forall n : nat, (Peano.le (Nat.pow x n) (Nat.pow y n)) = ((Peano.le x y) \/ (n = (NUMERAL 0))).
Axiom thm_EXP_MONO_LT : forall x : nat, forall y : nat, forall n : nat, (Peano.lt (Nat.pow x n) (Nat.pow y n)) = ((Peano.lt x y) /\ (~ (n = (NUMERAL 0)))).
Axiom thm_EXP_MONO_EQ : forall x : nat, forall y : nat, forall n : nat, ((Nat.pow x n) = (Nat.pow y n)) = ((x = y) \/ (n = (NUMERAL 0))).
Axiom thm_DIVMOD_EXIST : forall m : nat, forall n : nat, (~ (n = (NUMERAL 0))) -> exists q : nat, exists r : nat, (m = (Nat.add (Nat.mul q n) r)) /\ (Peano.lt r n).
Axiom thm_DIVMOD_EXIST_0 : forall m : nat, forall n : nat, exists q : nat, exists r : nat, @COND Prop (n = (NUMERAL 0)) ((q = (NUMERAL 0)) /\ (r = m)) ((m = (Nat.add (Nat.mul q n) r)) /\ (Peano.lt r n)).
Axiom thm_DIVISION : forall m : nat, forall n : nat, (~ (n = (NUMERAL 0))) -> (m = (Nat.add (Nat.mul (Nat.div m n) n) (Nat.modulo m n))) /\ (Peano.lt (Nat.modulo m n) n).
Axiom thm_DIV_ZERO : forall n : nat, (Nat.div n (NUMERAL 0)) = (NUMERAL 0).
Axiom thm_MOD_ZERO : forall n : nat, (Nat.modulo n (NUMERAL 0)) = n.
Axiom thm_DIVISION_SIMP : (forall m : nat, forall n : nat, (Nat.add (Nat.mul (Nat.div m n) n) (Nat.modulo m n)) = m) /\ (forall m : nat, forall n : nat, (Nat.add (Nat.mul n (Nat.div m n)) (Nat.modulo m n)) = m).
Axiom thm_EQ_DIVMOD : forall p : nat, forall m : nat, forall n : nat, (((Nat.div m p) = (Nat.div n p)) /\ ((Nat.modulo m p) = (Nat.modulo n p))) = (m = n).
Axiom thm_MOD_LT_EQ : forall m : nat, forall n : nat, (Peano.lt (Nat.modulo m n) n) = (~ (n = (NUMERAL 0))).
Axiom thm_MOD_LT_EQ_LT : forall m : nat, forall n : nat, (Peano.lt (Nat.modulo m n) n) = (Peano.lt (NUMERAL 0) n).
Axiom thm_DIVMOD_UNIQ_LEMMA : forall m : nat, forall n : nat, forall q1 : nat, forall r1 : nat, forall q2 : nat, forall r2 : nat, (((m = (Nat.add (Nat.mul q1 n) r1)) /\ (Peano.lt r1 n)) /\ ((m = (Nat.add (Nat.mul q2 n) r2)) /\ (Peano.lt r2 n))) -> (q1 = q2) /\ (r1 = r2).
Axiom thm_DIVMOD_UNIQ : forall m : nat, forall n : nat, forall q : nat, forall r : nat, ((m = (Nat.add (Nat.mul q n) r)) /\ (Peano.lt r n)) -> ((Nat.div m n) = q) /\ ((Nat.modulo m n) = r).
Axiom thm_MOD_UNIQ : forall m : nat, forall n : nat, forall q : nat, forall r : nat, ((m = (Nat.add (Nat.mul q n) r)) /\ (Peano.lt r n)) -> (Nat.modulo m n) = r.
Axiom thm_DIV_UNIQ : forall m : nat, forall n : nat, forall q : nat, forall r : nat, ((m = (Nat.add (Nat.mul q n) r)) /\ (Peano.lt r n)) -> (Nat.div m n) = q.
Axiom thm_MOD_0 : forall n : nat, (Nat.modulo (NUMERAL 0) n) = (NUMERAL 0).
Axiom thm_DIV_0 : forall n : nat, (Nat.div (NUMERAL 0) n) = (NUMERAL 0).
Axiom thm_MOD_MULT : forall m : nat, forall n : nat, (Nat.modulo (Nat.mul m n) m) = (NUMERAL 0).
Axiom thm_DIV_MULT : forall m : nat, forall n : nat, (~ (m = (NUMERAL 0))) -> (Nat.div (Nat.mul m n) m) = n.
Axiom thm_MOD_LT : forall m : nat, forall n : nat, (Peano.lt m n) -> (Nat.modulo m n) = m.
Axiom thm_MOD_EQ_SELF : forall m : nat, forall n : nat, ((Nat.modulo m n) = m) = ((n = (NUMERAL 0)) \/ (Peano.lt m n)).
Axiom thm_MOD_CASES : forall n : nat, forall p : nat, (Peano.lt n (Nat.mul (NUMERAL (BIT0 (BIT1 0))) p)) -> (Nat.modulo n p) = (@COND nat (Peano.lt n p) n (Nat.sub n p)).
Axiom thm_MOD_ADD_CASES : forall m : nat, forall n : nat, forall p : nat, ((Peano.lt m p) /\ (Peano.lt n p)) -> (Nat.modulo (Nat.add m n) p) = (@COND nat (Peano.lt (Nat.add m n) p) (Nat.add m n) (Nat.sub (Nat.add m n) p)).
Axiom thm_MOD_EQ : forall m : nat, forall n : nat, forall p : nat, forall q : nat, (m = (Nat.add n (Nat.mul q p))) -> (Nat.modulo m p) = (Nat.modulo n p).
Axiom thm_DIV_LE : forall m : nat, forall n : nat, Peano.le (Nat.div m n) m.
Axiom thm_DIV_MUL_LE : forall m : nat, forall n : nat, Peano.le (Nat.mul n (Nat.div m n)) m.
Axiom thm_MOD_LE_TWICE : forall m : nat, forall n : nat, ((Peano.lt (NUMERAL 0) m) /\ (Peano.le m n)) -> Peano.le (Nat.mul (NUMERAL (BIT0 (BIT1 0))) (Nat.modulo n m)) n.
Axiom thm_MOD_1 : forall n : nat, (Nat.modulo n (NUMERAL (BIT1 0))) = (NUMERAL 0).
Axiom thm_DIV_1 : forall n : nat, (Nat.div n (NUMERAL (BIT1 0))) = n.
Axiom thm_DIV_LT : forall m : nat, forall n : nat, (Peano.lt m n) -> (Nat.div m n) = (NUMERAL 0).
Axiom thm_MOD_MOD : forall m : nat, forall n : nat, forall p : nat, (Nat.modulo (Nat.modulo m (Nat.mul n p)) n) = (Nat.modulo m n).
Axiom thm_MOD_MOD_REFL : forall m : nat, forall n : nat, (Nat.modulo (Nat.modulo m n) n) = (Nat.modulo m n).
Axiom thm_MOD_MOD_LE : forall m : nat, forall n : nat, forall p : nat, ((~ (n = (NUMERAL 0))) /\ (Peano.le n p)) -> (Nat.modulo (Nat.modulo m n) p) = (Nat.modulo m n).
Axiom thm_MOD_EVEN_2 : forall m : nat, forall n : nat, (Coq.Arith.PeanoNat.Nat.Even n) -> (Nat.modulo (Nat.modulo m n) (NUMERAL (BIT0 (BIT1 0)))) = (Nat.modulo m (NUMERAL (BIT0 (BIT1 0)))).
Axiom thm_DIV_MULT2 : forall m : nat, forall n : nat, forall p : nat, (~ (m = (NUMERAL 0))) -> (Nat.div (Nat.mul m n) (Nat.mul m p)) = (Nat.div n p).
Axiom thm_MOD_MULT2 : forall m : nat, forall n : nat, forall p : nat, (Nat.modulo (Nat.mul m n) (Nat.mul m p)) = (Nat.mul m (Nat.modulo n p)).
Axiom thm_MOD_EXISTS : forall m : nat, forall n : nat, (exists q : nat, m = (Nat.mul n q)) = (@COND Prop (n = (NUMERAL 0)) (m = (NUMERAL 0)) ((Nat.modulo m n) = (NUMERAL 0))).
Axiom thm_LE_RDIV_EQ : forall a : nat, forall b : nat, forall n : nat, (~ (a = (NUMERAL 0))) -> (Peano.le n (Nat.div b a)) = (Peano.le (Nat.mul a n) b).
Axiom thm_RDIV_LT_EQ : forall a : nat, forall b : nat, forall n : nat, (~ (a = (NUMERAL 0))) -> (Peano.lt (Nat.div b a) n) = (Peano.lt b (Nat.mul a n)).
Axiom thm_LE_LDIV_EQ : forall a : nat, forall b : nat, forall n : nat, (~ (a = (NUMERAL 0))) -> (Peano.le (Nat.div b a) n) = (Peano.lt b (Nat.mul a (Nat.add n (NUMERAL (BIT1 0))))).
Axiom thm_LDIV_LT_EQ : forall a : nat, forall b : nat, forall n : nat, (~ (a = (NUMERAL 0))) -> (Peano.lt n (Nat.div b a)) = (Peano.le (Nat.mul a (Nat.add n (NUMERAL (BIT1 0)))) b).
Axiom thm_LE_LDIV : forall a : nat, forall b : nat, forall n : nat, ((~ (a = (NUMERAL 0))) /\ (Peano.le b (Nat.mul a n))) -> Peano.le (Nat.div b a) n.
Axiom thm_DIV_MONO : forall m : nat, forall n : nat, forall p : nat, (Peano.le m n) -> Peano.le (Nat.div m p) (Nat.div n p).
Axiom thm_DIV_MONO_LT : forall m : nat, forall n : nat, forall p : nat, ((~ (p = (NUMERAL 0))) /\ (Peano.le (Nat.add m p) n)) -> Peano.lt (Nat.div m p) (Nat.div n p).
Axiom thm_DIV_EQ_0 : forall m : nat, forall n : nat, (~ (n = (NUMERAL 0))) -> ((Nat.div m n) = (NUMERAL 0)) = (Peano.lt m n).
Axiom thm_MOD_DIV_EQ_0 : forall m : nat, forall n : nat, (~ (n = (NUMERAL 0))) -> (Nat.div (Nat.modulo m n) n) = (NUMERAL 0).
Axiom thm_MOD_EQ_0 : forall m : nat, forall n : nat, ((Nat.modulo m n) = (NUMERAL 0)) = (exists q : nat, m = (Nat.mul q n)).
Axiom thm_DIV_EQ_SELF : forall m : nat, forall n : nat, ((Nat.div m n) = m) = ((m = (NUMERAL 0)) \/ (n = (NUMERAL (BIT1 0)))).
Axiom thm_MOD_REFL : forall n : nat, (Nat.modulo n n) = (NUMERAL 0).
Axiom thm_EVEN_MOD : forall n : nat, (Coq.Arith.PeanoNat.Nat.Even n) = ((Nat.modulo n (NUMERAL (BIT0 (BIT1 0)))) = (NUMERAL 0)).
Axiom thm_ODD_MOD : forall n : nat, (Coq.Arith.PeanoNat.Nat.Odd n) = ((Nat.modulo n (NUMERAL (BIT0 (BIT1 0)))) = (NUMERAL (BIT1 0))).
Axiom thm_MOD_2_CASES : forall n : nat, (Nat.modulo n (NUMERAL (BIT0 (BIT1 0)))) = (@COND nat (Coq.Arith.PeanoNat.Nat.Even n) (NUMERAL 0) (NUMERAL (BIT1 0))).
Axiom thm_EVEN_MOD_EVEN : forall m : nat, forall n : nat, (Coq.Arith.PeanoNat.Nat.Even n) -> (Coq.Arith.PeanoNat.Nat.Even (Nat.modulo m n)) = (Coq.Arith.PeanoNat.Nat.Even m).
Axiom thm_ODD_MOD_EVEN : forall m : nat, forall n : nat, (Coq.Arith.PeanoNat.Nat.Even n) -> (Coq.Arith.PeanoNat.Nat.Odd (Nat.modulo m n)) = (Coq.Arith.PeanoNat.Nat.Odd m).
Axiom thm_HALF_DOUBLE : (forall n : nat, (Nat.div (Nat.mul (NUMERAL (BIT0 (BIT1 0))) n) (NUMERAL (BIT0 (BIT1 0)))) = n) /\ (forall n : nat, (Nat.div (Nat.mul n (NUMERAL (BIT0 (BIT1 0)))) (NUMERAL (BIT0 (BIT1 0)))) = n).
Axiom thm_DOUBLE_HALF : (forall n : nat, (Coq.Arith.PeanoNat.Nat.Even n) -> (Nat.mul (NUMERAL (BIT0 (BIT1 0))) (Nat.div n (NUMERAL (BIT0 (BIT1 0))))) = n) /\ (forall n : nat, (Coq.Arith.PeanoNat.Nat.Even n) -> (Nat.mul (Nat.div n (NUMERAL (BIT0 (BIT1 0)))) (NUMERAL (BIT0 (BIT1 0)))) = n).
Axiom thm_MOD_MULT_RMOD : forall m : nat, forall n : nat, forall p : nat, (Nat.modulo (Nat.mul m (Nat.modulo p n)) n) = (Nat.modulo (Nat.mul m p) n).
Axiom thm_MOD_MULT_LMOD : forall m : nat, forall n : nat, forall p : nat, (Nat.modulo (Nat.mul (Nat.modulo m n) p) n) = (Nat.modulo (Nat.mul m p) n).
Axiom thm_MOD_MULT_MOD2 : forall m : nat, forall n : nat, forall p : nat, (Nat.modulo (Nat.mul (Nat.modulo m n) (Nat.modulo p n)) n) = (Nat.modulo (Nat.mul m p) n).
Axiom thm_MOD_EXP_MOD : forall m : nat, forall n : nat, forall p : nat, (Nat.modulo (Nat.pow (Nat.modulo m n) p) n) = (Nat.modulo (Nat.pow m p) n).
Axiom thm_MOD_MULT_ADD : (forall m : nat, forall n : nat, forall p : nat, (Nat.modulo (Nat.add (Nat.mul m n) p) n) = (Nat.modulo p n)) /\ ((forall m : nat, forall n : nat, forall p : nat, (Nat.modulo (Nat.add (Nat.mul n m) p) n) = (Nat.modulo p n)) /\ ((forall m : nat, forall n : nat, forall p : nat, (Nat.modulo (Nat.add p (Nat.mul m n)) n) = (Nat.modulo p n)) /\ (forall m : nat, forall n : nat, forall p : nat, (Nat.modulo (Nat.add p (Nat.mul n m)) n) = (Nat.modulo p n)))).
Axiom thm_DIV_MULT_ADD : (forall a : nat, forall b : nat, forall n : nat, (~ (n = (NUMERAL 0))) -> (Nat.div (Nat.add (Nat.mul a n) b) n) = (Nat.add a (Nat.div b n))) /\ ((forall a : nat, forall b : nat, forall n : nat, (~ (n = (NUMERAL 0))) -> (Nat.div (Nat.add (Nat.mul n a) b) n) = (Nat.add a (Nat.div b n))) /\ ((forall a : nat, forall b : nat, forall n : nat, (~ (n = (NUMERAL 0))) -> (Nat.div (Nat.add b (Nat.mul a n)) n) = (Nat.add (Nat.div b n) a)) /\ (forall a : nat, forall b : nat, forall n : nat, (~ (n = (NUMERAL 0))) -> (Nat.div (Nat.add b (Nat.mul n a)) n) = (Nat.add (Nat.div b n) a)))).
Axiom thm_MOD_ADD_MOD : forall a : nat, forall b : nat, forall n : nat, (Nat.modulo (Nat.add (Nat.modulo a n) (Nat.modulo b n)) n) = (Nat.modulo (Nat.add a b) n).
Axiom thm_DIV_ADD_MOD : forall a : nat, forall b : nat, forall n : nat, (~ (n = (NUMERAL 0))) -> ((Nat.modulo (Nat.add a b) n) = (Nat.add (Nat.modulo a n) (Nat.modulo b n))) = ((Nat.div (Nat.add a b) n) = (Nat.add (Nat.div a n) (Nat.div b n))).
Axiom thm_DIV_REFL : forall n : nat, (~ (n = (NUMERAL 0))) -> (Nat.div n n) = (NUMERAL (BIT1 0)).
Axiom thm_MOD_LE : forall m : nat, forall n : nat, Peano.le (Nat.modulo m n) m.
Axiom thm_DIV_MONO2 : forall m : nat, forall n : nat, forall p : nat, ((~ (p = (NUMERAL 0))) /\ (Peano.le p m)) -> Peano.le (Nat.div n m) (Nat.div n p).
Axiom thm_DIV_LE_EXCLUSION : forall a : nat, forall b : nat, forall c : nat, forall d : nat, ((~ (b = (NUMERAL 0))) /\ (Peano.lt (Nat.mul b c) (Nat.mul (Nat.add a (NUMERAL (BIT1 0))) d))) -> Peano.le (Nat.div c d) (Nat.div a b).
Axiom thm_DIV_EQ_EXCLUSION : forall a : nat, forall b : nat, forall c : nat, forall d : nat, ((Peano.lt (Nat.mul b c) (Nat.mul (Nat.add a (NUMERAL (BIT1 0))) d)) /\ (Peano.lt (Nat.mul a d) (Nat.mul (Nat.add c (NUMERAL (BIT1 0))) b))) -> (Nat.div a b) = (Nat.div c d).
Axiom thm_MULT_DIV_LE : forall m : nat, forall n : nat, forall p : nat, Peano.le (Nat.mul m (Nat.div n p)) (Nat.div (Nat.mul m n) p).
Axiom thm_DIV_DIV : forall m : nat, forall n : nat, forall p : nat, (Nat.div (Nat.div m n) p) = (Nat.div m (Nat.mul n p)).
Axiom thm_DIV_MOD : forall m : nat, forall n : nat, forall p : nat, (Nat.modulo (Nat.div m n) p) = (Nat.div (Nat.modulo m (Nat.mul n p)) n).
Axiom thm_MOD_MULT_MOD : forall m : nat, forall n : nat, forall p : nat, (Nat.modulo m (Nat.mul n p)) = (Nat.add (Nat.mul n (Nat.modulo (Nat.div m n) p)) (Nat.modulo m n)).
Axiom thm_MOD_MOD_EXP_MIN : forall x : nat, forall p : nat, forall m : nat, forall n : nat, (Nat.modulo (Nat.modulo x (Nat.pow p m)) (Nat.pow p n)) = (Nat.modulo x (Nat.pow p (Nat.min m n))).
Axiom thm_MOD_EXP : forall m : nat, forall n : nat, forall p : nat, (~ (m = (NUMERAL 0))) -> (Nat.modulo (Nat.pow m n) (Nat.pow m p)) = (@COND nat ((Peano.le p n) \/ (m = (NUMERAL (BIT1 0)))) (NUMERAL 0) (Nat.pow m n)).
Axiom thm_DIV_EXP : forall m : nat, forall n : nat, forall p : nat, (~ (m = (NUMERAL 0))) -> (Nat.div (Nat.pow m n) (Nat.pow m p)) = (@COND nat (Peano.le p n) (Nat.pow m (Nat.sub n p)) (@COND nat (m = (NUMERAL (BIT1 0))) (NUMERAL (BIT1 0)) (NUMERAL 0))).
Axiom thm_FORALL_LT_MOD_THM : forall P : nat -> Prop, forall n : nat, (forall a : nat, (Peano.lt a n) -> P a) = ((n = (NUMERAL 0)) \/ (forall a : nat, P (Nat.modulo a n))).
Axiom thm_FORALL_MOD_THM : forall P : nat -> Prop, forall n : nat, (~ (n = (NUMERAL 0))) -> (forall a : nat, P (Nat.modulo a n)) = (forall a : nat, (Peano.lt a n) -> P a).
Axiom thm_EXISTS_LT_MOD_THM : forall P : nat -> Prop, forall n : nat, (exists a : nat, (Peano.lt a n) /\ (P a)) = ((~ (n = (NUMERAL 0))) /\ (exists a : nat, P (Nat.modulo a n))).
Axiom thm_EXISTS_MOD_THM : forall P : nat -> Prop, forall n : nat, (~ (n = (NUMERAL 0))) -> (exists a : nat, P (Nat.modulo a n)) = (exists a : nat, (Peano.lt a n) /\ (P a)).
Axiom thm_PRE_ELIM_THM : forall (n : nat) (P : nat -> Prop), (P (Nat.pred n)) = (forall m : nat, ((n = (S m)) \/ ((m = (NUMERAL 0)) /\ (n = (NUMERAL 0)))) -> P m).
Axiom thm_SUB_ELIM_THM : forall (a : nat) (b : nat) (P : nat -> Prop), (P (Nat.sub a b)) = (forall d : nat, ((a = (Nat.add b d)) \/ ((Peano.lt a b) /\ (d = (NUMERAL 0)))) -> P d).
Axiom thm_DIVMOD_ELIM_THM : forall (m : nat) (n : nat) (P : nat -> nat -> Prop), (P (Nat.div m n) (Nat.modulo m n)) = (forall q : nat, forall r : nat, (((n = (NUMERAL 0)) /\ ((q = (NUMERAL 0)) /\ (r = m))) \/ ((m = (Nat.add (Nat.mul q n) r)) /\ (Peano.lt r n))) -> P q r).
Axiom thm_minimal : forall P : nat -> Prop, (minimal P) = (@ε nat (fun n : nat => (P n) /\ (forall m : nat, (Peano.lt m n) -> ~ (P m)))).
Axiom thm_MINIMAL : forall P : nat -> Prop, (exists n : nat, P n) = ((P (minimal P)) /\ (forall m : nat, (Peano.lt m (minimal P)) -> ~ (P m))).
Axiom thm_MINIMAL_UNIQUE : forall P : nat -> Prop, forall n : nat, ((P n) /\ (forall m : nat, (Peano.lt m n) -> ~ (P m))) -> (minimal P) = n.
Axiom thm_LE_MINIMAL : forall P : nat -> Prop, forall n : nat, (exists r : nat, P r) -> (Peano.le n (minimal P)) = (forall i : nat, (P i) -> Peano.le n i).
Axiom thm_MINIMAL_LE : forall P : nat -> Prop, forall n : nat, (exists r : nat, P r) -> (Peano.le (minimal P) n) = (exists i : nat, (Peano.le i n) /\ (P i)).
Axiom thm_MINIMAL_UBOUND : forall P : nat -> Prop, forall n : nat, (P n) -> Peano.le (minimal P) n.
Axiom thm_MINIMAL_LBOUND : forall P : nat -> Prop, forall n : nat, ((exists r : nat, P r) /\ (forall m : nat, (Peano.lt m n) -> ~ (P m))) -> Peano.le n (minimal P).
Axiom thm_MINIMAL_MONO : forall P : nat -> Prop, forall Q : nat -> Prop, ((exists n : nat, P n) /\ (forall n : nat, (P n) -> Q n)) -> Peano.le (minimal Q) (minimal P).
Axiom thm_TRANSITIVE_STEPWISE_LT_EQ : forall R : nat -> nat -> Prop, (forall x : nat, forall y : nat, forall z : nat, ((R x y) /\ (R y z)) -> R x z) -> (forall m : nat, forall n : nat, (Peano.lt m n) -> R m n) = (forall n : nat, R n (S n)).
Axiom thm_TRANSITIVE_STEPWISE_LT : forall R : nat -> nat -> Prop, ((forall x : nat, forall y : nat, forall z : nat, ((R x y) /\ (R y z)) -> R x z) /\ (forall n : nat, R n (S n))) -> forall m : nat, forall n : nat, (Peano.lt m n) -> R m n.
Axiom thm_TRANSITIVE_STEPWISE_LE_EQ : forall R : nat -> nat -> Prop, ((forall x : nat, R x x) /\ (forall x : nat, forall y : nat, forall z : nat, ((R x y) /\ (R y z)) -> R x z)) -> (forall m : nat, forall n : nat, (Peano.le m n) -> R m n) = (forall n : nat, R n (S n)).
Axiom thm_TRANSITIVE_STEPWISE_LE : forall R : nat -> nat -> Prop, ((forall x : nat, R x x) /\ ((forall x : nat, forall y : nat, forall z : nat, ((R x y) /\ (R y z)) -> R x z) /\ (forall n : nat, R n (S n)))) -> forall m : nat, forall n : nat, (Peano.le m n) -> R m n.
Axiom thm_DEPENDENT_CHOICE_FIXED : forall {A : Type'}, forall P : nat -> A -> Prop, forall R : nat -> A -> A -> Prop, forall a : A, ((P (NUMERAL 0) a) /\ (forall n : nat, forall x : A, (P n x) -> exists y : A, (P (S n) y) /\ (R n x y))) -> exists f : nat -> A, ((f (NUMERAL 0)) = a) /\ ((forall n : nat, P n (f n)) /\ (forall n : nat, R n (f n) (f (S n)))).
Axiom thm_DEPENDENT_CHOICE : forall {A : Type'}, forall P : nat -> A -> Prop, forall R : nat -> A -> A -> Prop, ((exists a : A, P (NUMERAL 0) a) /\ (forall n : nat, forall x : A, (P n x) -> exists y : A, (P (S n) y) /\ (R n x y))) -> exists f : nat -> A, (forall n : nat, P n (f n)) /\ (forall n : nat, R n (f n) (f (S n))).
