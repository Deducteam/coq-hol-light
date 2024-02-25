Require Import coq.
Require Import theory_hol.
Require Import types.
Require Import terms.
Require Import axioms.
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
Axiom thm_MOD_ADD_EQ_EQ : forall n : nat, forall x : nat, forall y : nat, ((Nat.modulo (Nat.add x y) n) = (Nat.add (Nat.modulo x n) (Nat.modulo y n))) = ((n = (NUMERAL 0)) \/ (Peano.lt (Nat.add (Nat.modulo x n) (Nat.modulo y n)) n)).
Axiom thm_DIV_ADD_EQ_EQ : forall n : nat, forall x : nat, forall y : nat, ((Nat.div (Nat.add x y) n) = (Nat.add (Nat.div x n) (Nat.div y n))) = ((n = (NUMERAL 0)) \/ (Peano.lt (Nat.add (Nat.modulo x n) (Nat.modulo y n)) n)).
Axiom thm_DIV_ADD_EQ : forall n : nat, forall x : nat, forall y : nat, (Peano.lt (Nat.add (Nat.modulo x n) (Nat.modulo y n)) n) -> (Nat.div (Nat.add x y) n) = (Nat.add (Nat.div x n) (Nat.div y n)).
Axiom thm_MOD_ADD_EQ : forall n : nat, forall x : nat, forall y : nat, (Peano.lt (Nat.add (Nat.modulo x n) (Nat.modulo y n)) n) -> (Nat.modulo (Nat.add x y) n) = (Nat.add (Nat.modulo x n) (Nat.modulo y n)).
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
Axiom thm_WF : forall {A : Type'}, forall lt2 : A -> A -> Prop, (@WF A lt2) = (forall P : A -> Prop, (exists x : A, P x) -> exists x : A, (P x) /\ (forall y : A, (lt2 y x) -> ~ (P y))).
Axiom thm_WF_EQ : forall {A : Type'} (lt2 : A -> A -> Prop), (@WF A lt2) = (forall P : A -> Prop, (exists x : A, P x) = (exists x : A, (P x) /\ (forall y : A, (lt2 y x) -> ~ (P y)))).
Axiom thm_WF_IND : forall {A : Type'} (lt2 : A -> A -> Prop), (@WF A lt2) = (forall P : A -> Prop, (forall x : A, (forall y : A, (lt2 y x) -> P y) -> P x) -> forall x : A, P x).
Axiom thm_WF_DCHAIN : forall {A : Type'} (lt2 : A -> A -> Prop), (@WF A lt2) = (~ (exists s : nat -> A, forall n : nat, lt2 (s (S n)) (s n))).
Axiom thm_WF_DHAIN_TRANSITIVE : forall {A : Type'}, forall lt2 : A -> A -> Prop, (forall x : A, forall y : A, forall z : A, ((lt2 x y) /\ (lt2 y z)) -> lt2 x z) -> (@WF A lt2) = (~ (exists s : nat -> A, forall i : nat, forall j : nat, (Peano.lt i j) -> lt2 (s j) (s i))).
Axiom thm_WF_UREC : forall {A B : Type'} (lt2 : A -> A -> Prop), (@WF A lt2) -> forall H : (A -> B) -> A -> B, (forall f : A -> B, forall g : A -> B, forall x : A, (forall z : A, (lt2 z x) -> (f z) = (g z)) -> (H f x) = (H g x)) -> forall f : A -> B, forall g : A -> B, ((forall x : A, (f x) = (H f x)) /\ (forall x : A, (g x) = (H g x))) -> f = g.
Axiom thm_WF_UREC_WF : forall {A : Type'} (lt2 : A -> A -> Prop), (forall H : (A -> Prop) -> A -> Prop, (forall f : A -> Prop, forall g : A -> Prop, forall x : A, (forall z : A, (lt2 z x) -> (f z) = (g z)) -> (H f x) = (H g x)) -> forall f : A -> Prop, forall g : A -> Prop, ((forall x : A, (f x) = (H f x)) /\ (forall x : A, (g x) = (H g x))) -> f = g) -> @WF A lt2.
Axiom thm_WF_REC_INVARIANT : forall {A B : Type'} (lt2 : A -> A -> Prop), (@WF A lt2) -> forall H : (A -> B) -> A -> B, forall S : A -> B -> Prop, (forall f : A -> B, forall g : A -> B, forall x : A, (forall z : A, (lt2 z x) -> ((f z) = (g z)) /\ (S z (f z))) -> ((H f x) = (H g x)) /\ (S x (H f x))) -> exists f : A -> B, forall x : A, (f x) = (H f x).
Axiom thm_WF_REC : forall {A B : Type'} (lt2 : A -> A -> Prop), (@WF A lt2) -> forall H : (A -> B) -> A -> B, (forall f : A -> B, forall g : A -> B, forall x : A, (forall z : A, (lt2 z x) -> (f z) = (g z)) -> (H f x) = (H g x)) -> exists f : A -> B, forall x : A, (f x) = (H f x).
Axiom thm_WF_REC_WF : forall {A : Type'} (lt2 : A -> A -> Prop), (forall H : (A -> nat) -> A -> nat, (forall f : A -> nat, forall g : A -> nat, forall x : A, (forall z : A, (lt2 z x) -> (f z) = (g z)) -> (H f x) = (H g x)) -> exists f : A -> nat, forall x : A, (f x) = (H f x)) -> @WF A lt2.
Axiom thm_WF_EREC : forall {A B : Type'} (lt2 : A -> A -> Prop), (@WF A lt2) -> forall H : (A -> B) -> A -> B, (forall f : A -> B, forall g : A -> B, forall x : A, (forall z : A, (lt2 z x) -> (f z) = (g z)) -> (H f x) = (H g x)) -> @ex1 (A -> B) (fun f : A -> B => forall x : A, (f x) = (H f x)).
Axiom thm_WF_REC_EXISTS : forall {A B : Type'} (lt2 : A -> A -> Prop), (@WF A lt2) -> forall P : (A -> B) -> A -> B -> Prop, ((forall f : A -> B, forall g : A -> B, forall x : A, forall y : B, (forall z : A, (lt2 z x) -> (f z) = (g z)) -> (P f x y) = (P g x y)) /\ (forall f : A -> B, forall x : A, (forall z : A, (lt2 z x) -> P f z (f z)) -> exists y : B, P f x y)) -> exists f : A -> B, forall x : A, P f x (f x).
Axiom thm_WF_SUBSET : forall {A : Type'}, forall lt2 : A -> A -> Prop, forall lt3 : A -> A -> Prop, ((forall x : A, forall y : A, (lt2 x y) -> lt3 x y) /\ (@WF A lt3)) -> @WF A lt2.
Axiom thm_WF_RESTRICT : forall {A : Type'}, forall lt2 : A -> A -> Prop, forall P : A -> Prop, (@WF A lt2) -> @WF A (fun x : A => fun y : A => (P x) /\ ((P y) /\ (lt2 x y))).
Axiom thm_WF_MEASURE_GEN : forall {A B : Type'}, forall lt2 : B -> B -> Prop, forall m : A -> B, (@WF B lt2) -> @WF A (fun x : A => fun x' : A => lt2 (m x) (m x')).
Axiom thm_WF_LEX_DEPENDENT : forall {A B : Type'}, forall R : A -> A -> Prop, forall S : A -> B -> B -> Prop, ((@WF A R) /\ (forall a : A, @WF B (S a))) -> @WF (prod A B) (@GABS ((prod A B) -> (prod A B) -> Prop) (fun f : (prod A B) -> (prod A B) -> Prop => forall r1 : A, forall s1 : B, @GEQ ((prod A B) -> Prop) (f (@pair A B r1 s1)) (@GABS ((prod A B) -> Prop) (fun f' : (prod A B) -> Prop => forall r2 : A, forall s2 : B, @GEQ Prop (f' (@pair A B r2 s2)) ((R r1 r2) \/ ((r1 = r2) /\ (S r1 s1 s2))))))).
Axiom thm_WF_LEX : forall {A B : Type'}, forall R : A -> A -> Prop, forall S : B -> B -> Prop, ((@WF A R) /\ (@WF B S)) -> @WF (prod A B) (@GABS ((prod A B) -> (prod A B) -> Prop) (fun f : (prod A B) -> (prod A B) -> Prop => forall r1 : A, forall s1 : B, @GEQ ((prod A B) -> Prop) (f (@pair A B r1 s1)) (@GABS ((prod A B) -> Prop) (fun f' : (prod A B) -> Prop => forall r2 : A, forall s2 : B, @GEQ Prop (f' (@pair A B r2 s2)) ((R r1 r2) \/ ((r1 = r2) /\ (S s1 s2))))))).
Axiom thm_WF_POINTWISE : forall {A B : Type'} (lt2 : A -> A -> Prop) (lt3 : B -> B -> Prop), ((@WF A lt2) /\ (@WF B lt3)) -> @WF (prod A B) (@GABS ((prod A B) -> (prod A B) -> Prop) (fun f : (prod A B) -> (prod A B) -> Prop => forall x1 : A, forall y1 : B, @GEQ ((prod A B) -> Prop) (f (@pair A B x1 y1)) (@GABS ((prod A B) -> Prop) (fun f' : (prod A B) -> Prop => forall x2 : A, forall y2 : B, @GEQ Prop (f' (@pair A B x2 y2)) ((lt2 x1 x2) /\ (lt3 y1 y2)))))).
Axiom thm_WF_num : @WF nat Peano.lt.
Axiom thm_WF_REC_num : forall {A : Type'}, forall H : (nat -> A) -> nat -> A, (forall f : nat -> A, forall g : nat -> A, forall n : nat, (forall m : nat, (Peano.lt m n) -> (f m) = (g m)) -> (H f n) = (H g n)) -> exists f : nat -> A, forall n : nat, (f n) = (H f n).
Axiom thm_MEASURE : forall {_25210 : Type'}, forall m : _25210 -> nat, (@MEASURE _25210 m) = (fun x : _25210 => fun y : _25210 => Peano.lt (m x) (m y)).
Axiom thm_WF_MEASURE : forall {A : Type'}, forall m : A -> nat, @WF A (@MEASURE A m).
Axiom thm_MEASURE_LE : forall {_25240 : Type'} (a : _25240) (m : _25240 -> nat) (b : _25240), (forall y : _25240, (@MEASURE _25240 m y a) -> @MEASURE _25240 m y b) = (Peano.le (m a) (m b)).
Axiom thm_WF_ANTISYM : forall {A : Type'}, forall lt2 : A -> A -> Prop, forall x : A, forall y : A, (@WF A lt2) -> ~ ((lt2 x y) /\ (lt2 y x)).
Axiom thm_WF_REFL : forall {A : Type'} (lt2 : A -> A -> Prop), forall x : A, (@WF A lt2) -> ~ (lt2 x x).
Axiom thm_WF_FALSE : forall {A : Type'}, @WF A (fun x : A => fun y : A => False).
Axiom thm_MINIMAL_BAD_SEQUENCE : forall {A : Type'}, forall lt2 : A -> A -> Prop, forall bad : (nat -> A) -> Prop, ((@WF A lt2) /\ ((forall x : nat -> A, (~ (bad x)) -> exists n : nat, forall y : nat -> A, (forall k : nat, (Peano.lt k n) -> (y k) = (x k)) -> ~ (bad y)) /\ (exists x : nat -> A, bad x))) -> exists y : nat -> A, (bad y) /\ (forall z : nat -> A, forall n : nat, ((bad z) /\ (forall k : nat, (Peano.lt k n) -> (z k) = (y k))) -> ~ (lt2 (z n) (y n))).
Axiom thm_WF_REC_TAIL : forall {A B : Type'}, forall P : A -> Prop, forall g : A -> A, forall h : A -> B, exists f : A -> B, forall x : A, (f x) = (@COND B (P x) (f (g x)) (h x)).
Axiom thm_WF_REC_TAIL_GENERAL : forall {A B : Type'} (lt2 : A -> A -> Prop), forall P : (A -> B) -> A -> Prop, forall G : (A -> B) -> A -> A, forall H : (A -> B) -> A -> B, ((@WF A lt2) /\ ((forall f : A -> B, forall g : A -> B, forall x : A, (forall z : A, (lt2 z x) -> (f z) = (g z)) -> ((P f x) = (P g x)) /\ (((G f x) = (G g x)) /\ ((H f x) = (H g x)))) /\ ((forall f : A -> B, forall g : A -> B, forall x : A, (forall z : A, (lt2 z x) -> (f z) = (g z)) -> (H f x) = (H g x)) /\ (forall f : A -> B, forall x : A, forall y : A, ((P f x) /\ (lt2 y (G f x))) -> lt2 y x)))) -> exists f : A -> B, forall x : A, (f x) = (@COND B (P f x) (f (G f x)) (H f x)).
Axiom thm_ARITH_ZERO : ((NUMERAL (NUMERAL 0)) = (NUMERAL 0)) /\ ((BIT0 0) = 0).
Axiom thm_BIT0_0 : (BIT0 (NUMERAL 0)) = (NUMERAL 0).
Axiom thm_BIT1_0 : (BIT1 (NUMERAL 0)) = (NUMERAL (BIT1 0)).
Axiom thm_ARITH_SUC : (forall n : nat, (S (NUMERAL n)) = (NUMERAL (S n))) /\ (((S 0) = (BIT1 0)) /\ ((forall n : nat, (S (BIT0 n)) = (BIT1 n)) /\ (forall n : nat, (S (BIT1 n)) = (BIT0 (S n))))).
Axiom thm_ARITH_PRE : (forall n : nat, (Nat.pred (NUMERAL n)) = (NUMERAL (Nat.pred n))) /\ (((Nat.pred 0) = 0) /\ ((forall n : nat, (Nat.pred (BIT0 n)) = (@COND nat (n = 0) 0 (BIT1 (Nat.pred n)))) /\ (forall n : nat, (Nat.pred (BIT1 n)) = (BIT0 n)))).
Axiom thm_ARITH_ADD : (forall m : nat, forall n : nat, (Nat.add (NUMERAL m) (NUMERAL n)) = (NUMERAL (Nat.add m n))) /\ (((Nat.add 0 0) = 0) /\ ((forall n : nat, (Nat.add 0 (BIT0 n)) = (BIT0 n)) /\ ((forall n : nat, (Nat.add 0 (BIT1 n)) = (BIT1 n)) /\ ((forall n : nat, (Nat.add (BIT0 n) 0) = (BIT0 n)) /\ ((forall n : nat, (Nat.add (BIT1 n) 0) = (BIT1 n)) /\ ((forall m : nat, forall n : nat, (Nat.add (BIT0 m) (BIT0 n)) = (BIT0 (Nat.add m n))) /\ ((forall m : nat, forall n : nat, (Nat.add (BIT0 m) (BIT1 n)) = (BIT1 (Nat.add m n))) /\ ((forall m : nat, forall n : nat, (Nat.add (BIT1 m) (BIT0 n)) = (BIT1 (Nat.add m n))) /\ (forall m : nat, forall n : nat, (Nat.add (BIT1 m) (BIT1 n)) = (BIT0 (S (Nat.add m n)))))))))))).
Axiom thm_ARITH_MULT : (forall m : nat, forall n : nat, (Nat.mul (NUMERAL m) (NUMERAL n)) = (NUMERAL (Nat.mul m n))) /\ (((Nat.mul 0 0) = 0) /\ ((forall n : nat, (Nat.mul 0 (BIT0 n)) = 0) /\ ((forall n : nat, (Nat.mul 0 (BIT1 n)) = 0) /\ ((forall n : nat, (Nat.mul (BIT0 n) 0) = 0) /\ ((forall n : nat, (Nat.mul (BIT1 n) 0) = 0) /\ ((forall m : nat, forall n : nat, (Nat.mul (BIT0 m) (BIT0 n)) = (BIT0 (BIT0 (Nat.mul m n)))) /\ ((forall m : nat, forall n : nat, (Nat.mul (BIT0 m) (BIT1 n)) = (Nat.add (BIT0 m) (BIT0 (BIT0 (Nat.mul m n))))) /\ ((forall m : nat, forall n : nat, (Nat.mul (BIT1 m) (BIT0 n)) = (Nat.add (BIT0 n) (BIT0 (BIT0 (Nat.mul m n))))) /\ (forall m : nat, forall n : nat, (Nat.mul (BIT1 m) (BIT1 n)) = (Nat.add (BIT1 m) (Nat.add (BIT0 n) (BIT0 (BIT0 (Nat.mul m n)))))))))))))).
Axiom thm_ARITH_EXP : (forall m : nat, forall n : nat, (Nat.pow (NUMERAL m) (NUMERAL n)) = (NUMERAL (Nat.pow m n))) /\ (((Nat.pow 0 0) = (BIT1 0)) /\ ((forall m : nat, (Nat.pow (BIT0 m) 0) = (BIT1 0)) /\ ((forall m : nat, (Nat.pow (BIT1 m) 0) = (BIT1 0)) /\ ((forall n : nat, (Nat.pow 0 (BIT0 n)) = (Nat.mul (Nat.pow 0 n) (Nat.pow 0 n))) /\ ((forall m : nat, forall n : nat, (Nat.pow (BIT0 m) (BIT0 n)) = (Nat.mul (Nat.pow (BIT0 m) n) (Nat.pow (BIT0 m) n))) /\ ((forall m : nat, forall n : nat, (Nat.pow (BIT1 m) (BIT0 n)) = (Nat.mul (Nat.pow (BIT1 m) n) (Nat.pow (BIT1 m) n))) /\ ((forall n : nat, (Nat.pow 0 (BIT1 n)) = 0) /\ ((forall m : nat, forall n : nat, (Nat.pow (BIT0 m) (BIT1 n)) = (Nat.mul (BIT0 m) (Nat.mul (Nat.pow (BIT0 m) n) (Nat.pow (BIT0 m) n)))) /\ (forall m : nat, forall n : nat, (Nat.pow (BIT1 m) (BIT1 n)) = (Nat.mul (BIT1 m) (Nat.mul (Nat.pow (BIT1 m) n) (Nat.pow (BIT1 m) n)))))))))))).
Axiom thm_ARITH_EVEN : (forall n : nat, (Coq.Arith.PeanoNat.Nat.Even (NUMERAL n)) = (Coq.Arith.PeanoNat.Nat.Even n)) /\ (((Coq.Arith.PeanoNat.Nat.Even 0) = True) /\ ((forall n : nat, (Coq.Arith.PeanoNat.Nat.Even (BIT0 n)) = True) /\ (forall n : nat, (Coq.Arith.PeanoNat.Nat.Even (BIT1 n)) = False))).
Axiom thm_ARITH_ODD : (forall n : nat, (Coq.Arith.PeanoNat.Nat.Odd (NUMERAL n)) = (Coq.Arith.PeanoNat.Nat.Odd n)) /\ (((Coq.Arith.PeanoNat.Nat.Odd 0) = False) /\ ((forall n : nat, (Coq.Arith.PeanoNat.Nat.Odd (BIT0 n)) = False) /\ (forall n : nat, (Coq.Arith.PeanoNat.Nat.Odd (BIT1 n)) = True))).
Axiom thm_ARITH_LE : (forall m : nat, forall n : nat, (Peano.le (NUMERAL m) (NUMERAL n)) = (Peano.le m n)) /\ (((Peano.le 0 0) = True) /\ ((forall n : nat, (Peano.le (BIT0 n) 0) = (Peano.le n 0)) /\ ((forall n : nat, (Peano.le (BIT1 n) 0) = False) /\ ((forall n : nat, (Peano.le 0 (BIT0 n)) = True) /\ ((forall n : nat, (Peano.le 0 (BIT1 n)) = True) /\ ((forall m : nat, forall n : nat, (Peano.le (BIT0 m) (BIT0 n)) = (Peano.le m n)) /\ ((forall m : nat, forall n : nat, (Peano.le (BIT0 m) (BIT1 n)) = (Peano.le m n)) /\ ((forall m : nat, forall n : nat, (Peano.le (BIT1 m) (BIT0 n)) = (Peano.lt m n)) /\ (forall m : nat, forall n : nat, (Peano.le (BIT1 m) (BIT1 n)) = (Peano.le m n)))))))))).
Axiom thm_ARITH_LT : (forall m : nat, forall n : nat, (Peano.lt (NUMERAL m) (NUMERAL n)) = (Peano.lt m n)) /\ (((Peano.lt 0 0) = False) /\ ((forall n : nat, (Peano.lt (BIT0 n) 0) = False) /\ ((forall n : nat, (Peano.lt (BIT1 n) 0) = False) /\ ((forall n : nat, (Peano.lt 0 (BIT0 n)) = (Peano.lt 0 n)) /\ ((forall n : nat, (Peano.lt 0 (BIT1 n)) = True) /\ ((forall m : nat, forall n : nat, (Peano.lt (BIT0 m) (BIT0 n)) = (Peano.lt m n)) /\ ((forall m : nat, forall n : nat, (Peano.lt (BIT0 m) (BIT1 n)) = (Peano.le m n)) /\ ((forall m : nat, forall n : nat, (Peano.lt (BIT1 m) (BIT0 n)) = (Peano.lt m n)) /\ (forall m : nat, forall n : nat, (Peano.lt (BIT1 m) (BIT1 n)) = (Peano.lt m n)))))))))).
Axiom thm_ARITH_EQ : (forall m : nat, forall n : nat, ((NUMERAL m) = (NUMERAL n)) = (m = n)) /\ (((0 = 0) = True) /\ ((forall n : nat, ((BIT0 n) = 0) = (n = 0)) /\ ((forall n : nat, ((BIT1 n) = 0) = False) /\ ((forall n : nat, (0 = (BIT0 n)) = (0 = n)) /\ ((forall n : nat, (0 = (BIT1 n)) = False) /\ ((forall m : nat, forall n : nat, ((BIT0 m) = (BIT0 n)) = (m = n)) /\ ((forall m : nat, forall n : nat, ((BIT0 m) = (BIT1 n)) = False) /\ ((forall m : nat, forall n : nat, ((BIT1 m) = (BIT0 n)) = False) /\ (forall m : nat, forall n : nat, ((BIT1 m) = (BIT1 n)) = (m = n)))))))))).
Axiom thm_ARITH_SUB : (forall m : nat, forall n : nat, (Nat.sub (NUMERAL m) (NUMERAL n)) = (NUMERAL (Nat.sub m n))) /\ (((Nat.sub 0 0) = 0) /\ ((forall n : nat, (Nat.sub 0 (BIT0 n)) = 0) /\ ((forall n : nat, (Nat.sub 0 (BIT1 n)) = 0) /\ ((forall n : nat, (Nat.sub (BIT0 n) 0) = (BIT0 n)) /\ ((forall n : nat, (Nat.sub (BIT1 n) 0) = (BIT1 n)) /\ ((forall m : nat, forall n : nat, (Nat.sub (BIT0 m) (BIT0 n)) = (BIT0 (Nat.sub m n))) /\ ((forall m : nat, forall n : nat, (Nat.sub (BIT0 m) (BIT1 n)) = (Nat.pred (BIT0 (Nat.sub m n)))) /\ ((forall m : nat, forall n : nat, (Nat.sub (BIT1 m) (BIT0 n)) = (@COND nat (Peano.le n m) (BIT1 (Nat.sub m n)) 0)) /\ (forall m : nat, forall n : nat, (Nat.sub (BIT1 m) (BIT1 n)) = (BIT0 (Nat.sub m n))))))))))).
Axiom thm_EXP_2_NE_0 : forall n : nat, ~ ((Nat.pow (NUMERAL (BIT0 (BIT1 0))) n) = (NUMERAL 0)).
Axiom thm_INJ_INVERSE2 : forall {A B C : Type'}, forall P : A -> B -> C, (forall x1 : A, forall y1 : B, forall x2 : A, forall y2 : B, ((P x1 y1) = (P x2 y2)) = ((x1 = x2) /\ (y1 = y2))) -> exists X : C -> A, exists Y : C -> B, forall x : A, forall y : B, ((X (P x y)) = x) /\ ((Y (P x y)) = y).
Axiom thm_NUMPAIR : forall x : nat, forall y : nat, (NUMPAIR x y) = (Nat.mul (Nat.pow (NUMERAL (BIT0 (BIT1 0))) x) (Nat.add (Nat.mul (NUMERAL (BIT0 (BIT1 0))) y) (NUMERAL (BIT1 0)))).
Axiom thm_NUMPAIR_INJ_LEMMA : forall x1 : nat, forall y1 : nat, forall x2 : nat, forall y2 : nat, ((NUMPAIR x1 y1) = (NUMPAIR x2 y2)) -> x1 = x2.
Axiom thm_NUMPAIR_INJ : forall x1 : nat, forall y1 : nat, forall x2 : nat, forall y2 : nat, ((NUMPAIR x1 y1) = (NUMPAIR x2 y2)) = ((x1 = x2) /\ (y1 = y2)).
Axiom thm_NUMSUM : forall b : Prop, forall x : nat, (NUMSUM b x) = (@COND nat b (S (Nat.mul (NUMERAL (BIT0 (BIT1 0))) x)) (Nat.mul (NUMERAL (BIT0 (BIT1 0))) x)).
Axiom thm_NUMSUM_INJ : forall b1 : Prop, forall x1 : nat, forall b2 : Prop, forall x2 : nat, ((NUMSUM b1 x1) = (NUMSUM b2 x2)) = ((b1 = b2) /\ (x1 = x2)).
Axiom thm_INJN : forall {A : Type'}, forall m : nat, (@INJN A m) = (fun n : nat => fun a : A => n = m).
Axiom thm_INJN_INJ : forall {A : Type'}, forall n1 : nat, forall n2 : nat, ((@INJN A n1) = (@INJN A n2)) = (n1 = n2).
Axiom thm_INJA : forall {A : Type'}, forall a : A, (@INJA A a) = (fun n : nat => fun b : A => b = a).
Axiom thm_INJA_INJ : forall {A : Type'}, forall a1 : A, forall a2 : A, ((@INJA A a1) = (@INJA A a2)) = (a1 = a2).
Axiom thm_INJF : forall {A : Type'}, forall f : nat -> nat -> A -> Prop, (@INJF A f) = (fun n : nat => f (NUMFST n) (NUMSND n)).
Axiom thm_INJF_INJ : forall {A : Type'}, forall f1 : nat -> nat -> A -> Prop, forall f2 : nat -> nat -> A -> Prop, ((@INJF A f1) = (@INJF A f2)) = (f1 = f2).
Axiom thm_INJP : forall {A : Type'}, forall f1 : nat -> A -> Prop, forall f2 : nat -> A -> Prop, (@INJP A f1 f2) = (fun n : nat => fun a : A => @COND Prop (NUMLEFT n) (f1 (NUMRIGHT n) a) (f2 (NUMRIGHT n) a)).
Axiom thm_INJP_INJ : forall {A : Type'}, forall f1 : nat -> A -> Prop, forall f1' : nat -> A -> Prop, forall f2 : nat -> A -> Prop, forall f2' : nat -> A -> Prop, ((@INJP A f1 f2) = (@INJP A f1' f2')) = ((f1 = f1') /\ (f2 = f2')).
Axiom thm_ZCONSTR : forall {A : Type'}, forall c : nat, forall i : A, forall r : nat -> nat -> A -> Prop, (@ZCONSTR A c i r) = (@INJP A (@INJN A (S c)) (@INJP A (@INJA A i) (@INJF A r))).
Axiom thm_ZBOT : forall {A : Type'}, (@ZBOT A) = (@INJP A (@INJN A (NUMERAL 0)) (@ε (nat -> A -> Prop) (fun z : nat -> A -> Prop => True))).
Axiom thm_ZCONSTR_ZBOT : forall {A : Type'}, forall c : nat, forall i : A, forall r : nat -> nat -> A -> Prop, ~ ((@ZCONSTR A c i r) = (@ZBOT A)).
Axiom thm_ZRECSPACE_RULES : forall {A : Type'}, (@ZRECSPACE A (@ZBOT A)) /\ (forall c : nat, forall i : A, forall r : nat -> nat -> A -> Prop, (forall n : nat, @ZRECSPACE A (r n)) -> @ZRECSPACE A (@ZCONSTR A c i r)).
Axiom thm_ZRECSPACE_CASES : forall {A : Type'}, forall a : nat -> A -> Prop, (@ZRECSPACE A a) = ((a = (@ZBOT A)) \/ (exists c : nat, exists i : A, exists r : nat -> nat -> A -> Prop, (a = (@ZCONSTR A c i r)) /\ (forall n : nat, @ZRECSPACE A (r n)))).
Axiom thm_ZRECSPACE_INDUCT : forall {A : Type'}, forall ZRECSPACE' : (nat -> A -> Prop) -> Prop, ((ZRECSPACE' (@ZBOT A)) /\ (forall c : nat, forall i : A, forall r : nat -> nat -> A -> Prop, (forall n : nat, ZRECSPACE' (r n)) -> ZRECSPACE' (@ZCONSTR A c i r))) -> forall a : nat -> A -> Prop, (@ZRECSPACE A a) -> ZRECSPACE' a.
Axiom thm_BOTTOM : forall {A : Type'}, (@BOTTOM A) = (@_mk_rec A (@ZBOT A)).
Axiom thm_CONSTR : forall {A : Type'}, forall c : nat, forall i : A, forall r : nat -> recspace A, (@CONSTR A c i r) = (@_mk_rec A (@ZCONSTR A c i (fun n : nat => @_dest_rec A (r n)))).
Axiom thm_MK_REC_INJ : forall {A : Type'}, forall x : nat -> A -> Prop, forall y : nat -> A -> Prop, ((@_mk_rec A x) = (@_mk_rec A y)) -> ((@ZRECSPACE A x) /\ (@ZRECSPACE A y)) -> x = y.
Axiom thm_DEST_REC_INJ : forall {A : Type'}, forall x : recspace A, forall y : recspace A, ((@_dest_rec A x) = (@_dest_rec A y)) = (x = y).
Axiom thm_CONSTR_BOT : forall {A : Type'}, forall c : nat, forall i : A, forall r : nat -> recspace A, ~ ((@CONSTR A c i r) = (@BOTTOM A)).
Axiom thm_CONSTR_INJ : forall {A : Type'}, forall c1 : nat, forall i1 : A, forall r1 : nat -> recspace A, forall c2 : nat, forall i2 : A, forall r2 : nat -> recspace A, ((@CONSTR A c1 i1 r1) = (@CONSTR A c2 i2 r2)) = ((c1 = c2) /\ ((i1 = i2) /\ (r1 = r2))).
Axiom thm_CONSTR_IND : forall {A : Type'}, forall P : (recspace A) -> Prop, ((P (@BOTTOM A)) /\ (forall c : nat, forall i : A, forall r : nat -> recspace A, (forall n : nat, P (r n)) -> P (@CONSTR A c i r))) -> forall x : recspace A, P x.
Axiom thm_CONSTR_REC : forall {A B : Type'}, forall Fn : nat -> A -> (nat -> recspace A) -> (nat -> B) -> B, exists f : (recspace A) -> B, forall c : nat, forall i : A, forall r : nat -> recspace A, (f (@CONSTR A c i r)) = (Fn c i r (fun n : nat => f (r n))).
Axiom thm_FCONS : forall {A : Type'}, (forall a : A, forall f : nat -> A, (@FCONS A a f (NUMERAL 0)) = a) /\ (forall a : A, forall f : nat -> A, forall n : nat, (@FCONS A a f (S n)) = (f n)).
Axiom thm_FCONS_UNDO : forall {A : Type'}, forall f : nat -> A, f = (@FCONS A (f (NUMERAL 0)) (@o nat nat A f S)).
Axiom thm_FNIL : forall {A : Type'}, forall n : nat, (@FNIL A n) = (@ε A (fun x : A => True)).
Axiom thm_sum_INDUCT : forall {A B : Type'}, forall P : (sum A B) -> Prop, ((forall a : A, P (@inl A B a)) /\ (forall a : B, P (@inr A B a))) -> forall x : sum A B, P x.
Axiom thm_sum_RECURSION : forall {A B Z : Type'}, forall INL' : A -> Z, forall INR' : B -> Z, exists fn : (sum A B) -> Z, (forall a : A, (fn (@inl A B a)) = (INL' a)) /\ (forall a : B, (fn (@inr A B a)) = (INR' a)).
Axiom thm_OUTL : forall {A B : Type'} (x : A), (@OUTL A B (@inl A B x)) = x.
Axiom thm_OUTR : forall {A B : Type'} (y : B), (@OUTR A B (@inr A B y)) = y.
Axiom thm_option_INDUCT : forall {A : Type'}, forall P : (option A) -> Prop, ((P (@None A)) /\ (forall a : A, P (@Some A a))) -> forall x : option A, P x.
Axiom thm_option_RECURSION : forall {A Z : Type'}, forall NONE' : Z, forall SOME' : A -> Z, exists fn : (option A) -> Z, ((fn (@None A)) = NONE') /\ (forall a : A, (fn (@Some A a)) = (SOME' a)).
Axiom thm_list_INDUCT : forall {A : Type'}, forall P : (list A) -> Prop, ((P (@nil A)) /\ (forall a0 : A, forall a1 : list A, (P a1) -> P (@cons A a0 a1))) -> forall x : list A, P x.
Axiom thm_list_RECURSION : forall {A Z : Type'}, forall NIL' : Z, forall CONS' : A -> (list A) -> Z -> Z, exists fn : (list A) -> Z, ((fn (@nil A)) = NIL') /\ (forall a0 : A, forall a1 : list A, (fn (@cons A a0 a1)) = (CONS' a0 a1 (fn a1))).
Axiom thm_FORALL_OPTION_THM : forall {_33228 : Type'}, forall P : (option _33228) -> Prop, (forall x : option _33228, P x) = ((P (@None _33228)) /\ (forall a : _33228, P (@Some _33228 a))).
Axiom thm_EXISTS_OPTION_THM : forall {A : Type'}, forall P : (option A) -> Prop, (exists x : option A, P x) = ((P (@None A)) \/ (exists a : A, P (@Some A a))).
Axiom thm_option_DISTINCT : forall {A : Type'}, forall a : A, ~ ((@Some A a) = (@None A)).
Axiom thm_option_INJ : forall {A : Type'}, forall a : A, forall b : A, ((@Some A a) = (@Some A b)) = (a = b).
Axiom thm_ISO : forall {A B : Type'}, forall g : B -> A, forall f : A -> B, (@ExtensionalityFacts.is_inverse A B f g) = ((forall x : B, (f (g x)) = x) /\ (forall y : A, (g (f y)) = y)).
Axiom thm_ISO_REFL : forall {A : Type'}, @ExtensionalityFacts.is_inverse A A (fun x : A => x) (fun x : A => x).
Axiom thm_ISO_FUN : forall {A A' B B' : Type'} (g : B -> B') (f' : A' -> A) (g' : B' -> B) (f : A -> A'), ((@ExtensionalityFacts.is_inverse A A' f f') /\ (@ExtensionalityFacts.is_inverse B B' g g')) -> @ExtensionalityFacts.is_inverse (A -> B) (A' -> B') (fun h : A -> B => fun a' : A' => g (h (f' a'))) (fun h : A' -> B' => fun a : A => g' (h (f a))).
Axiom thm_ISO_USAGE : forall {_33436 _33439 : Type'} (g : _33436 -> _33439) (f : _33439 -> _33436), (@ExtensionalityFacts.is_inverse _33439 _33436 f g) -> (forall P : _33439 -> Prop, (forall x : _33439, P x) = (forall x : _33436, P (g x))) /\ ((forall P : _33439 -> Prop, (exists x : _33439, P x) = (exists x : _33436, P (g x))) /\ (forall a : _33439, forall b : _33436, (a = (g b)) = ((f a) = b))).
Axiom thm_HD : forall {A : Type'} (t : list A) (h : A), (@hd A (@cons A h t)) = h.
Axiom thm_TL : forall {A : Type'} (h : A) (t : list A), (@tl A (@cons A h t)) = t.
Axiom thm_APPEND : forall {A : Type'}, (forall l : list A, (@List.app A (@nil A) l) = l) /\ (forall h : A, forall t : list A, forall l : list A, (@List.app A (@cons A h t) l) = (@cons A h (@List.app A t l))).
Axiom thm_REVERSE : forall {A : Type'} (l : list A) (x : A), ((@List.rev A (@nil A)) = (@nil A)) /\ ((@List.rev A (@cons A x l)) = (@List.app A (@List.rev A l) (@cons A x (@nil A)))).
Axiom thm_LENGTH : forall {A : Type'}, ((@List.length A (@nil A)) = (NUMERAL 0)) /\ (forall h : A, forall t : list A, (@List.length A (@cons A h t)) = (S (@List.length A t))).
Axiom thm_MAP : forall {A B : Type'}, (forall f : A -> B, (@List.map A B f (@nil A)) = (@nil B)) /\ (forall f : A -> B, forall h : A, forall t : list A, (@List.map A B f (@cons A h t)) = (@cons B (f h) (@List.map A B f t))).
Axiom thm_LAST : forall {A : Type'} (h : A) (t : list A), (@LAST A (@cons A h t)) = (@COND A (t = (@nil A)) h (@LAST A t)).
Axiom thm_BUTLAST : forall {_34055 : Type'} (h : _34055) (t : list _34055), ((@List.removelast _34055 (@nil _34055)) = (@nil _34055)) /\ ((@List.removelast _34055 (@cons _34055 h t)) = (@COND (list _34055) (t = (@nil _34055)) (@nil _34055) (@cons _34055 h (@List.removelast _34055 t)))).
Axiom thm_REPLICATE : forall {_34076 : Type'} (n : nat) (x : _34076), ((@repeat_with_perm_args _34076 (NUMERAL 0) x) = (@nil _34076)) /\ ((@repeat_with_perm_args _34076 (S n) x) = (@cons _34076 x (@repeat_with_perm_args _34076 n x))).
Axiom thm_NULL : forall {_34091 : Type'} (h : _34091) (t : list _34091), ((@NULL _34091 (@nil _34091)) = True) /\ ((@NULL _34091 (@cons _34091 h t)) = False).
Axiom thm_ALL : forall {_34111 : Type'} (h : _34111) (P : _34111 -> Prop) (t : list _34111), ((@List.Forall _34111 P (@nil _34111)) = True) /\ ((@List.Forall _34111 P (@cons _34111 h t)) = ((P h) /\ (@List.Forall _34111 P t))).
Axiom thm_EX : forall {_34132 : Type'} (h : _34132) (P : _34132 -> Prop) (t : list _34132), ((@EX _34132 P (@nil _34132)) = False) /\ ((@EX _34132 P (@cons _34132 h t)) = ((P h) \/ (@EX _34132 P t))).
Axiom thm_ITLIST : forall {_34154 _34155 : Type'} (h : _34155) (f : _34155 -> _34154 -> _34154) (t : list _34155) (b : _34154), ((@fold_right_with_perm_args _34154 _34155 f (@nil _34155) b) = b) /\ ((@fold_right_with_perm_args _34154 _34155 f (@cons _34155 h t) b) = (f h (@fold_right_with_perm_args _34154 _34155 f t b))).
Axiom thm_MEM : forall {_34180 : Type'} (h : _34180) (x : _34180) (t : list _34180), ((@List.In _34180 x (@nil _34180)) = False) /\ ((@List.In _34180 x (@cons _34180 h t)) = ((x = h) \/ (@List.In _34180 x t))).
Axiom thm_ALL2_DEF : forall {_34213 _34220 : Type'} (h1' : _34213) (P : _34213 -> _34220 -> Prop) (t1 : list _34213) (l2 : list _34220), ((@ALL2 _34213 _34220 P (@nil _34213) l2) = (l2 = (@nil _34220))) /\ ((@ALL2 _34213 _34220 P (@cons _34213 h1' t1) l2) = (@COND Prop (l2 = (@nil _34220)) False ((P h1' (@hd _34220 l2)) /\ (@ALL2 _34213 _34220 P t1 (@tl _34220 l2))))).
Axiom thm_ALL2 : forall {_34274 _34275 : Type'} (h1' : _34275) (h2' : _34274) (P : _34275 -> _34274 -> Prop) (t1 : list _34275) (t2 : list _34274), ((@ALL2 _34275 _34274 P (@nil _34275) (@nil _34274)) = True) /\ (((@ALL2 _34275 _34274 P (@cons _34275 h1' t1) (@nil _34274)) = False) /\ (((@ALL2 _34275 _34274 P (@nil _34275) (@cons _34274 h2' t2)) = False) /\ ((@ALL2 _34275 _34274 P (@cons _34275 h1' t1) (@cons _34274 h2' t2)) = ((P h1' h2') /\ (@ALL2 _34275 _34274 P t1 t2))))).
Axiom thm_MAP2_DEF : forall {_34302 _34305 _34312 : Type'} (h1' : _34305) (f : _34305 -> _34312 -> _34302) (t1 : list _34305) (l : list _34312), ((@MAP2 _34302 _34305 _34312 f (@nil _34305) l) = (@nil _34302)) /\ ((@MAP2 _34302 _34305 _34312 f (@cons _34305 h1' t1) l) = (@cons _34302 (f h1' (@hd _34312 l)) (@MAP2 _34302 _34305 _34312 f t1 (@tl _34312 l)))).
Axiom thm_MAP2 : forall {_34346 _34347 _34353 : Type'} (h1' : _34347) (h2' : _34346) (f : _34347 -> _34346 -> _34353) (t1 : list _34347) (t2 : list _34346), ((@MAP2 _34353 _34347 _34346 f (@nil _34347) (@nil _34346)) = (@nil _34353)) /\ ((@MAP2 _34353 _34347 _34346 f (@cons _34347 h1' t1) (@cons _34346 h2' t2)) = (@cons _34353 (f h1' h2') (@MAP2 _34353 _34347 _34346 f t1 t2))).
Axiom thm_EL : forall {_34373 : Type'} (n : nat) (l : list _34373), ((@EL _34373 (NUMERAL 0) l) = (@hd _34373 l)) /\ ((@EL _34373 (S n) l) = (@EL _34373 n (@tl _34373 l))).
Axiom thm_FILTER : forall {_34398 : Type'} (h : _34398) (P : _34398 -> Prop) (t : list _34398), ((@FILTER _34398 P (@nil _34398)) = (@nil _34398)) /\ ((@FILTER _34398 P (@cons _34398 h t)) = (@COND (list _34398) (P h) (@cons _34398 h (@FILTER _34398 P t)) (@FILTER _34398 P t))).
Axiom thm_ASSOC : forall {_34421 _34427 : Type'} (h : prod _34427 _34421) (a : _34427) (t : list (prod _34427 _34421)), (@ASSOC _34421 _34427 a (@cons (prod _34427 _34421) h t)) = (@COND _34421 ((@fst _34427 _34421 h) = a) (@snd _34427 _34421 h) (@ASSOC _34421 _34427 a t)).
Axiom thm_ITLIST2_DEF : forall {_34449 _34451 _34459 : Type'} (h1' : _34451) (f : _34451 -> _34459 -> _34449 -> _34449) (t1 : list _34451) (l2 : list _34459) (b : _34449), ((@ITLIST2 _34449 _34451 _34459 f (@nil _34451) l2 b) = b) /\ ((@ITLIST2 _34449 _34451 _34459 f (@cons _34451 h1' t1) l2 b) = (f h1' (@hd _34459 l2) (@ITLIST2 _34449 _34451 _34459 f t1 (@tl _34459 l2) b))).
Axiom thm_ITLIST2 : forall {_34491 _34492 _34493 : Type'} (h1' : _34493) (h2' : _34492) (f : _34493 -> _34492 -> _34491 -> _34491) (t1 : list _34493) (t2 : list _34492) (b : _34491), ((@ITLIST2 _34491 _34493 _34492 f (@nil _34493) (@nil _34492) b) = b) /\ ((@ITLIST2 _34491 _34493 _34492 f (@cons _34493 h1' t1) (@cons _34492 h2' t2) b) = (f h1' h2' (@ITLIST2 _34491 _34493 _34492 f t1 t2 b))).
Axiom thm_ZIP_DEF : forall {_34523 _34531 : Type'} (h1' : _34523) (t1 : list _34523) (l2 : list _34531), ((@ZIP _34523 _34531 (@nil _34523) l2) = (@nil (prod _34523 _34531))) /\ ((@ZIP _34523 _34531 (@cons _34523 h1' t1) l2) = (@cons (prod _34523 _34531) (@pair _34523 _34531 h1' (@hd _34531 l2)) (@ZIP _34523 _34531 t1 (@tl _34531 l2)))).
Axiom thm_ZIP : forall {_34542 _34543 _34567 _34568 : Type'} (h1' : _34567) (h2' : _34568) (t1 : list _34567) (t2 : list _34568), ((@ZIP _34542 _34543 (@nil _34542) (@nil _34543)) = (@nil (prod _34542 _34543))) /\ ((@ZIP _34567 _34568 (@cons _34567 h1' t1) (@cons _34568 h2' t2)) = (@cons (prod _34567 _34568) (@pair _34567 _34568 h1' h2') (@ZIP _34567 _34568 t1 t2))).
Axiom thm_ALLPAIRS : forall {_34590 _34591 : Type'} (h : _34591) (f : _34591 -> _34590 -> Prop) (t : list _34591) (l : list _34590), ((@ALLPAIRS _34590 _34591 f (@nil _34591) l) = True) /\ ((@ALLPAIRS _34590 _34591 f (@cons _34591 h t) l) = ((@List.Forall _34590 (f h) l) /\ (@ALLPAIRS _34590 _34591 f t l))).
Axiom thm_PAIRWISE : forall {A : Type'} (h : A) (r : A -> A -> Prop) (t : list A), ((@List.ForallOrdPairs A r (@nil A)) = True) /\ ((@List.ForallOrdPairs A r (@cons A h t)) = ((@List.Forall A (r h) t) /\ (@List.ForallOrdPairs A r t))).
Axiom thm_list_of_seq : forall {A : Type'} (s : nat -> A) (n : nat), ((@list_of_seq A s (NUMERAL 0)) = (@nil A)) /\ ((@list_of_seq A s (S n)) = (@List.app A (@list_of_seq A s n) (@cons A (s n) (@nil A)))).
Axiom thm_NOT_CONS_NIL : forall {A : Type'}, forall h : A, forall t : list A, ~ ((@cons A h t) = (@nil A)).
Axiom thm_LAST_CLAUSES : forall {A : Type'} (h : A) (k : A) (t : list A), ((@LAST A (@cons A h (@nil A))) = h) /\ ((@LAST A (@cons A h (@cons A k t))) = (@LAST A (@cons A k t))).
Axiom thm_APPEND_NIL : forall {A : Type'}, forall l : list A, (@List.app A l (@nil A)) = l.
Axiom thm_APPEND_ASSOC : forall {A : Type'}, forall l : list A, forall m : list A, forall n : list A, (@List.app A l (@List.app A m n)) = (@List.app A (@List.app A l m) n).
Axiom thm_REVERSE_APPEND : forall {A : Type'}, forall l : list A, forall m : list A, (@List.rev A (@List.app A l m)) = (@List.app A (@List.rev A m) (@List.rev A l)).
Axiom thm_REVERSE_REVERSE : forall {A : Type'}, forall l : list A, (@List.rev A (@List.rev A l)) = l.
Axiom thm_REVERSE_EQ_EMPTY : forall {A : Type'}, forall l : list A, ((@List.rev A l) = (@nil A)) = (l = (@nil A)).
Axiom thm_CONS_11 : forall {A : Type'}, forall h1' : A, forall h2' : A, forall t1 : list A, forall t2 : list A, ((@cons A h1' t1) = (@cons A h2' t2)) = ((h1' = h2') /\ (t1 = t2)).
Axiom thm_list_CASES : forall {A : Type'}, forall l : list A, (l = (@nil A)) \/ (exists h : A, exists t : list A, l = (@cons A h t)).
Axiom thm_LIST_EQ : forall {A : Type'}, forall l1 : list A, forall l2 : list A, (l1 = l2) = (((@List.length A l1) = (@List.length A l2)) /\ (forall n : nat, (Peano.lt n (@List.length A l2)) -> (@EL A n l1) = (@EL A n l2))).
Axiom thm_LENGTH_APPEND : forall {A : Type'}, forall l : list A, forall m : list A, (@List.length A (@List.app A l m)) = (Nat.add (@List.length A l) (@List.length A m)).
Axiom thm_MAP_APPEND : forall {A B : Type'}, forall f : A -> B, forall l1 : list A, forall l2 : list A, (@List.map A B f (@List.app A l1 l2)) = (@List.app B (@List.map A B f l1) (@List.map A B f l2)).
Axiom thm_LENGTH_MAP : forall {A B : Type'}, forall l : list A, forall f : A -> B, (@List.length B (@List.map A B f l)) = (@List.length A l).
Axiom thm_LENGTH_EQ_NIL : forall {A : Type'}, forall l : list A, ((@List.length A l) = (NUMERAL 0)) = (l = (@nil A)).
Axiom thm_LENGTH_EQ_CONS : forall {_35025 : Type'}, forall l : list _35025, forall n : nat, ((@List.length _35025 l) = (S n)) = (exists h : _35025, exists t : list _35025, (l = (@cons _35025 h t)) /\ ((@List.length _35025 t) = n)).
Axiom thm_LENGTH_REVERSE : forall {A : Type'}, forall l : list A, (@List.length A (@List.rev A l)) = (@List.length A l).
Axiom thm_MAP_o : forall {A B C : Type'}, forall f : A -> B, forall g : B -> C, forall l : list A, (@List.map A C (@o A B C g f) l) = (@List.map B C g (@List.map A B f l)).
Axiom thm_MAP_EQ : forall {_35103 _35114 : Type'}, forall f : _35103 -> _35114, forall g : _35103 -> _35114, forall l : list _35103, (@List.Forall _35103 (fun x : _35103 => (f x) = (g x)) l) -> (@List.map _35103 _35114 f l) = (@List.map _35103 _35114 g l).
Axiom thm_ALL_IMP : forall {_35144 : Type'}, forall P : _35144 -> Prop, forall Q : _35144 -> Prop, forall l : list _35144, ((forall x : _35144, ((@List.In _35144 x l) /\ (P x)) -> Q x) /\ (@List.Forall _35144 P l)) -> @List.Forall _35144 Q l.
Axiom thm_NOT_EX : forall {_35172 : Type'}, forall P : _35172 -> Prop, forall l : list _35172, (~ (@EX _35172 P l)) = (@List.Forall _35172 (fun x : _35172 => ~ (P x)) l).
Axiom thm_NOT_ALL : forall {_35194 : Type'}, forall P : _35194 -> Prop, forall l : list _35194, (~ (@List.Forall _35194 P l)) = (@EX _35194 (fun x : _35194 => ~ (P x)) l).
Axiom thm_ALL_MAP : forall {_35215 _35216 : Type'}, forall P : _35216 -> Prop, forall f : _35215 -> _35216, forall l : list _35215, (@List.Forall _35216 P (@List.map _35215 _35216 f l)) = (@List.Forall _35215 (@o _35215 _35216 Prop P f) l).
Axiom thm_ALL_EQ : forall {_35247 : Type'} (R : _35247 -> Prop) (P : _35247 -> Prop) (Q : _35247 -> Prop), forall l : list _35247, ((@List.Forall _35247 R l) /\ (forall x : _35247, (R x) -> (P x) = (Q x))) -> (@List.Forall _35247 P l) = (@List.Forall _35247 Q l).
Axiom thm_ALL_T : forall {_35265 : Type'}, forall l : list _35265, @List.Forall _35265 (fun x : _35265 => True) l.
Axiom thm_MAP_EQ_ALL2 : forall {_35290 _35301 : Type'} (f : _35290 -> _35301), forall l : list _35290, forall m : list _35290, (@ALL2 _35290 _35290 (fun x : _35290 => fun y : _35290 => (f x) = (f y)) l m) -> (@List.map _35290 _35301 f l) = (@List.map _35290 _35301 f m).
Axiom thm_ALL2_MAP : forall {_35332 _35333 : Type'}, forall P : _35332 -> _35333 -> Prop, forall f : _35333 -> _35332, forall l : list _35333, (@ALL2 _35332 _35333 P (@List.map _35333 _35332 f l) l) = (@List.Forall _35333 (fun a : _35333 => P (f a) a) l).
Axiom thm_MAP_EQ_DEGEN : forall {_35350 : Type'}, forall l : list _35350, forall f : _35350 -> _35350, (@List.Forall _35350 (fun x : _35350 => (f x) = x) l) -> (@List.map _35350 _35350 f l) = l.
Axiom thm_ALL2_AND_RIGHT : forall {_35392 _35393 : Type'}, forall l : list _35393, forall m : list _35392, forall P : _35393 -> Prop, forall Q : _35393 -> _35392 -> Prop, (@ALL2 _35393 _35392 (fun x : _35393 => fun y : _35392 => (P x) /\ (Q x y)) l m) = ((@List.Forall _35393 P l) /\ (@ALL2 _35393 _35392 Q l m)).
Axiom thm_ITLIST_APPEND : forall {_35421 _35431 : Type'}, forall f : _35431 -> _35421 -> _35421, forall a : _35421, forall l1 : list _35431, forall l2 : list _35431, (@fold_right_with_perm_args _35421 _35431 f (@List.app _35431 l1 l2) a) = (@fold_right_with_perm_args _35421 _35431 f l1 (@fold_right_with_perm_args _35421 _35431 f l2 a)).
Axiom thm_ITLIST_EXTRA : forall {_35467 _35468 : Type'} (f : _35468 -> _35467 -> _35467) (a : _35468) (b : _35467), forall l : list _35468, (@fold_right_with_perm_args _35467 _35468 f (@List.app _35468 l (@cons _35468 a (@nil _35468))) b) = (@fold_right_with_perm_args _35467 _35468 f l (f a b)).
Axiom thm_ALL_MP : forall {_35494 : Type'}, forall P : _35494 -> Prop, forall Q : _35494 -> Prop, forall l : list _35494, ((@List.Forall _35494 (fun x : _35494 => (P x) -> Q x) l) /\ (@List.Forall _35494 P l)) -> @List.Forall _35494 Q l.
Axiom thm_AND_ALL : forall {_35524 : Type'} (P : _35524 -> Prop) (Q : _35524 -> Prop), forall l : list _35524, ((@List.Forall _35524 P l) /\ (@List.Forall _35524 Q l)) = (@List.Forall _35524 (fun x : _35524 => (P x) /\ (Q x)) l).
Axiom thm_EX_IMP : forall {_35554 : Type'}, forall P : _35554 -> Prop, forall Q : _35554 -> Prop, forall l : list _35554, ((forall x : _35554, ((@List.In _35554 x l) /\ (P x)) -> Q x) /\ (@EX _35554 P l)) -> @EX _35554 Q l.
Axiom thm_ALL_MEM : forall {_35581 : Type'}, forall P : _35581 -> Prop, forall l : list _35581, (forall x : _35581, (@List.In _35581 x l) -> P x) = (@List.Forall _35581 P l).
Axiom thm_LENGTH_REPLICATE : forall {_35599 : Type'}, forall n : nat, forall x : _35599, (@List.length _35599 (@repeat_with_perm_args _35599 n x)) = n.
Axiom thm_MEM_REPLICATE : forall {A : Type'}, forall n : nat, forall x : A, forall y : A, (@List.In A x (@repeat_with_perm_args A n y)) = ((x = y) /\ (~ (n = (NUMERAL 0)))).
Axiom thm_EX_MAP : forall {_35652 _35653 : Type'}, forall P : _35653 -> Prop, forall f : _35652 -> _35653, forall l : list _35652, (@EX _35653 P (@List.map _35652 _35653 f l)) = (@EX _35652 (@o _35652 _35653 Prop P f) l).
Axiom thm_EXISTS_EX : forall {_35690 _35691 : Type'}, forall P : _35691 -> _35690 -> Prop, forall l : list _35690, (exists x : _35691, @EX _35690 (P x) l) = (@EX _35690 (fun s : _35690 => exists x : _35691, P x s) l).
Axiom thm_FORALL_ALL : forall {_35720 _35721 : Type'}, forall P : _35721 -> _35720 -> Prop, forall l : list _35720, (forall x : _35721, @List.Forall _35720 (P x) l) = (@List.Forall _35720 (fun s : _35720 => forall x : _35721, P x s) l).
Axiom thm_MEM_APPEND : forall {_35749 : Type'}, forall x : _35749, forall l1 : list _35749, forall l2 : list _35749, (@List.In _35749 x (@List.app _35749 l1 l2)) = ((@List.In _35749 x l1) \/ (@List.In _35749 x l2)).
Axiom thm_MEM_MAP : forall {_35782 _35785 : Type'}, forall f : _35785 -> _35782, forall y : _35782, forall l : list _35785, (@List.In _35782 y (@List.map _35785 _35782 f l)) = (exists x : _35785, (@List.In _35785 x l) /\ (y = (f x))).
Axiom thm_FILTER_APPEND : forall {_35816 : Type'}, forall P : _35816 -> Prop, forall l1 : list _35816, forall l2 : list _35816, (@FILTER _35816 P (@List.app _35816 l1 l2)) = (@List.app _35816 (@FILTER _35816 P l1) (@FILTER _35816 P l2)).
Axiom thm_FILTER_MAP : forall {_35843 _35850 : Type'}, forall P : _35843 -> Prop, forall f : _35850 -> _35843, forall l : list _35850, (@FILTER _35843 P (@List.map _35850 _35843 f l)) = (@List.map _35850 _35843 f (@FILTER _35850 (@o _35850 _35843 Prop P f) l)).
Axiom thm_MEM_FILTER : forall {_35877 : Type'}, forall P : _35877 -> Prop, forall l : list _35877, forall x : _35877, (@List.In _35877 x (@FILTER _35877 P l)) = ((P x) /\ (@List.In _35877 x l)).
Axiom thm_EX_MEM : forall {_35898 : Type'}, forall P : _35898 -> Prop, forall l : list _35898, (exists x : _35898, (P x) /\ (@List.In _35898 x l)) = (@EX _35898 P l).
Axiom thm_MAP_FST_ZIP : forall {_35918 _35920 : Type'}, forall l1 : list _35918, forall l2 : list _35920, ((@List.length _35918 l1) = (@List.length _35920 l2)) -> (@List.map (prod _35918 _35920) _35918 (@fst _35918 _35920) (@ZIP _35918 _35920 l1 l2)) = l1.
Axiom thm_MAP_SND_ZIP : forall {_35949 _35951 : Type'}, forall l1 : list _35949, forall l2 : list _35951, ((@List.length _35949 l1) = (@List.length _35951 l2)) -> (@List.map (prod _35949 _35951) _35951 (@snd _35949 _35951) (@ZIP _35949 _35951 l1 l2)) = l2.
Axiom thm_LENGTH_ZIP : forall {_35980 _35982 : Type'}, forall l1 : list _35980, forall l2 : list _35982, ((@List.length _35980 l1) = (@List.length _35982 l2)) -> (@List.length (prod _35980 _35982) (@ZIP _35980 _35982 l1 l2)) = (@List.length _35982 l2).
Axiom thm_MEM_ASSOC : forall {_36008 _36024 : Type'}, forall l : list (prod _36024 _36008), forall x : _36024, (@List.In (prod _36024 _36008) (@pair _36024 _36008 x (@ASSOC _36008 _36024 x l)) l) = (@List.In _36024 x (@List.map (prod _36024 _36008) _36024 (@fst _36024 _36008) l)).
Axiom thm_ALL_APPEND : forall {_36045 : Type'}, forall P : _36045 -> Prop, forall l1 : list _36045, forall l2 : list _36045, (@List.Forall _36045 P (@List.app _36045 l1 l2)) = ((@List.Forall _36045 P l1) /\ (@List.Forall _36045 P l2)).
Axiom thm_MEM_EL : forall {_36068 : Type'}, forall l : list _36068, forall n : nat, (Peano.lt n (@List.length _36068 l)) -> @List.In _36068 (@EL _36068 n l) l.
Axiom thm_MEM_EXISTS_EL : forall {_36116 : Type'}, forall l : list _36116, forall x : _36116, (@List.In _36116 x l) = (exists i : nat, (Peano.lt i (@List.length _36116 l)) /\ (x = (@EL _36116 i l))).
Axiom thm_ALL_EL : forall {_36142 : Type'}, forall P : _36142 -> Prop, forall l : list _36142, (forall i : nat, (Peano.lt i (@List.length _36142 l)) -> P (@EL _36142 i l)) = (@List.Forall _36142 P l).
Axiom thm_ALL2_MAP2 : forall {_36184 _36185 _36186 _36187 : Type'} (P : _36185 -> _36184 -> Prop) (f : _36186 -> _36185) (g : _36187 -> _36184), forall l : list _36186, forall m : list _36187, (@ALL2 _36185 _36184 P (@List.map _36186 _36185 f l) (@List.map _36187 _36184 g m)) = (@ALL2 _36186 _36187 (fun x : _36186 => fun y : _36187 => P (f x) (g y)) l m).
Axiom thm_AND_ALL2 : forall {_36232 _36233 : Type'}, forall P : _36233 -> _36232 -> Prop, forall Q : _36233 -> _36232 -> Prop, forall l : list _36233, forall m : list _36232, ((@ALL2 _36233 _36232 P l m) /\ (@ALL2 _36233 _36232 Q l m)) = (@ALL2 _36233 _36232 (fun x : _36233 => fun y : _36232 => (P x y) /\ (Q x y)) l m).
Axiom thm_ALLPAIRS_SYM : forall {_36263 _36264 : Type'}, forall P : _36264 -> _36263 -> Prop, forall l : list _36264, forall m : list _36263, (@ALLPAIRS _36263 _36264 P l m) = (@ALLPAIRS _36264 _36263 (fun x : _36263 => fun y : _36264 => P y x) m l).
Axiom thm_ALLPAIRS_MEM : forall {_36298 _36299 : Type'}, forall P : _36299 -> _36298 -> Prop, forall l : list _36299, forall m : list _36298, (forall x : _36299, forall y : _36298, ((@List.In _36299 x l) /\ (@List.In _36298 y m)) -> P x y) = (@ALLPAIRS _36298 _36299 P l m).
Axiom thm_ALLPAIRS_MAP : forall {_36342 _36343 _36344 _36345 : Type'} (f : _36344 -> _36343) (g : _36345 -> _36342), forall P : _36343 -> _36342 -> Prop, forall l : list _36344, forall m : list _36345, (@ALLPAIRS _36342 _36343 P (@List.map _36344 _36343 f l) (@List.map _36345 _36342 g m)) = (@ALLPAIRS _36345 _36344 (fun x : _36344 => fun y : _36345 => P (f x) (g y)) l m).
Axiom thm_ALLPAIRS_EQ : forall {A B : Type'} (R : A -> B -> Prop) (R' : A -> B -> Prop), forall l : list A, forall m : list B, forall P : A -> Prop, forall Q : B -> Prop, ((@List.Forall A P l) /\ ((@List.Forall B Q m) /\ (forall p : A, forall q : B, ((P p) /\ (Q q)) -> (R p q) = (R' p q)))) -> (@ALLPAIRS B A R l m) = (@ALLPAIRS B A R' l m).
Axiom thm_ALL2_ALL : forall {_36428 : Type'}, forall P : _36428 -> _36428 -> Prop, forall l : list _36428, (@ALL2 _36428 _36428 P l l) = (@List.Forall _36428 (fun x : _36428 => P x x) l).
Axiom thm_APPEND_EQ_NIL : forall {_36457 : Type'}, forall l : list _36457, forall m : list _36457, ((@List.app _36457 l m) = (@nil _36457)) = ((l = (@nil _36457)) /\ (m = (@nil _36457))).
Axiom thm_APPEND_LCANCEL : forall {A : Type'}, forall l1 : list A, forall l2 : list A, forall l3 : list A, ((@List.app A l1 l2) = (@List.app A l1 l3)) = (l2 = l3).
Axiom thm_APPEND_RCANCEL : forall {A : Type'}, forall l1 : list A, forall l2 : list A, forall l3 : list A, ((@List.app A l1 l3) = (@List.app A l2 l3)) = (l1 = l2).
Axiom thm_LENGTH_MAP2 : forall {_36544 _36546 _36557 : Type'}, forall f : _36544 -> _36546 -> _36557, forall l : list _36544, forall m : list _36546, ((@List.length _36544 l) = (@List.length _36546 m)) -> (@List.length _36557 (@MAP2 _36557 _36544 _36546 f l m)) = (@List.length _36546 m).
Axiom thm_EL_MAP2 : forall {_36600 _36601 _36602 : Type'}, forall f : _36602 -> _36601 -> _36600, forall l : list _36602, forall m : list _36601, forall k : nat, ((Peano.lt k (@List.length _36602 l)) /\ (Peano.lt k (@List.length _36601 m))) -> (@EL _36600 k (@MAP2 _36600 _36602 _36601 f l m)) = (f (@EL _36602 k l) (@EL _36601 k m)).
Axiom thm_MAP_EQ_NIL : forall {_36627 _36631 : Type'}, forall f : _36631 -> _36627, forall l : list _36631, ((@List.map _36631 _36627 f l) = (@nil _36627)) = (l = (@nil _36631)).
Axiom thm_INJECTIVE_MAP : forall {A B : Type'}, forall f : A -> B, (forall l : list A, forall m : list A, ((@List.map A B f l) = (@List.map A B f m)) -> l = m) = (forall x : A, forall y : A, ((f x) = (f y)) -> x = y).
Axiom thm_SURJECTIVE_MAP : forall {A B : Type'}, forall f : A -> B, (forall m : list B, exists l : list A, (@List.map A B f l) = m) = (forall y : B, exists x : A, (f x) = y).
Axiom thm_MAP_ID : forall {_36745 : Type'}, forall l : list _36745, (@List.map _36745 _36745 (fun x : _36745 => x) l) = l.
Axiom thm_MAP_I : forall {_36754 : Type'}, (@List.map _36754 _36754 (@I _36754)) = (@I (list _36754)).
Axiom thm_BUTLAST_CLAUSES : forall {A : Type'}, ((@List.removelast A (@nil A)) = (@nil A)) /\ ((forall a : A, (@List.removelast A (@cons A a (@nil A))) = (@nil A)) /\ (forall a : A, forall h : A, forall t : list A, (@List.removelast A (@cons A a (@cons A h t))) = (@cons A a (@List.removelast A (@cons A h t))))).
Axiom thm_BUTLAST_APPEND : forall {A : Type'}, forall l : list A, forall m : list A, (@List.removelast A (@List.app A l m)) = (@COND (list A) (m = (@nil A)) (@List.removelast A l) (@List.app A l (@List.removelast A m))).
Axiom thm_APPEND_BUTLAST_LAST : forall {_36880 : Type'}, forall l : list _36880, (~ (l = (@nil _36880))) -> (@List.app _36880 (@List.removelast _36880 l) (@cons _36880 (@LAST _36880 l) (@nil _36880))) = l.
Axiom thm_LAST_APPEND : forall {_36905 : Type'}, forall p : list _36905, forall q : list _36905, (@LAST _36905 (@List.app _36905 p q)) = (@COND _36905 (q = (@nil _36905)) (@LAST _36905 p) (@LAST _36905 q)).
Axiom thm_LENGTH_TL : forall {_36921 : Type'}, forall l : list _36921, (~ (l = (@nil _36921))) -> (@List.length _36921 (@tl _36921 l)) = (Nat.sub (@List.length _36921 l) (NUMERAL (BIT1 0))).
Axiom thm_LAST_REVERSE : forall {A : Type'}, forall l : list A, (~ (l = (@nil A))) -> (@LAST A (@List.rev A l)) = (@hd A l).
Axiom thm_HD_REVERSE : forall {A : Type'}, forall l : list A, (~ (l = (@nil A))) -> (@hd A (@List.rev A l)) = (@LAST A l).
Axiom thm_EL_APPEND : forall {_36996 : Type'}, forall k : nat, forall l : list _36996, forall m : list _36996, (@EL _36996 k (@List.app _36996 l m)) = (@COND _36996 (Peano.lt k (@List.length _36996 l)) (@EL _36996 k l) (@EL _36996 (Nat.sub k (@List.length _36996 l)) m)).
Axiom thm_EL_TL : forall {_37027 : Type'} (l : list _37027), forall n : nat, (@EL _37027 n (@tl _37027 l)) = (@EL _37027 (Nat.add n (NUMERAL (BIT1 0))) l).
Axiom thm_EL_CONS : forall {_37053 : Type'}, forall n : nat, forall h : _37053, forall t : list _37053, (@EL _37053 n (@cons _37053 h t)) = (@COND _37053 (n = (NUMERAL 0)) h (@EL _37053 (Nat.sub n (NUMERAL (BIT1 0))) t)).
Axiom thm_LAST_EL : forall {_37078 : Type'}, forall l : list _37078, (~ (l = (@nil _37078))) -> (@LAST _37078 l) = (@EL _37078 (Nat.sub (@List.length _37078 l) (NUMERAL (BIT1 0))) l).
Axiom thm_HD_APPEND : forall {A : Type'}, forall l : list A, forall m : list A, (@hd A (@List.app A l m)) = (@COND A (l = (@nil A)) (@hd A m) (@hd A l)).
Axiom thm_CONS_HD_TL : forall {_37141 : Type'}, forall l : list _37141, (~ (l = (@nil _37141))) -> l = (@cons _37141 (@hd _37141 l) (@tl _37141 l)).
Axiom thm_EL_MAP : forall {_37170 _37171 : Type'}, forall f : _37171 -> _37170, forall n : nat, forall l : list _37171, (Peano.lt n (@List.length _37171 l)) -> (@EL _37170 n (@List.map _37171 _37170 f l)) = (f (@EL _37171 n l)).
Axiom thm_MAP_REVERSE : forall {_37196 _37198 : Type'}, forall f : _37198 -> _37196, forall l : list _37198, (@List.rev _37196 (@List.map _37198 _37196 f l)) = (@List.map _37198 _37196 f (@List.rev _37198 l)).
Axiom thm_ALL_FILTER : forall {A : Type'}, forall P : A -> Prop, forall Q : A -> Prop, forall l : list A, (@List.Forall A P (@FILTER A Q l)) = (@List.Forall A (fun x : A => (Q x) -> P x) l).
Axiom thm_APPEND_SING : forall {_37249 : Type'}, forall h : _37249, forall t : list _37249, (@List.app _37249 (@cons _37249 h (@nil _37249)) t) = (@cons _37249 h t).
Axiom thm_MEM_APPEND_DECOMPOSE_LEFT : forall {A : Type'}, forall x : A, forall l : list A, (@List.In A x l) = (exists l1 : list A, exists l2 : list A, (~ (@List.In A x l1)) /\ (l = (@List.app A l1 (@cons A x l2)))).
Axiom thm_MEM_APPEND_DECOMPOSE : forall {A : Type'}, forall x : A, forall l : list A, (@List.In A x l) = (exists l1 : list A, exists l2 : list A, l = (@List.app A l1 (@cons A x l2))).
Axiom thm_PAIRWISE_APPEND : forall {A : Type'}, forall R : A -> A -> Prop, forall l : list A, forall m : list A, (@List.ForallOrdPairs A R (@List.app A l m)) = ((@List.ForallOrdPairs A R l) /\ ((@List.ForallOrdPairs A R m) /\ (forall x : A, forall y : A, ((@List.In A x l) /\ (@List.In A y m)) -> R x y))).
Axiom thm_PAIRWISE_MAP : forall {A B : Type'}, forall R : B -> B -> Prop, forall f : A -> B, forall l : list A, (@List.ForallOrdPairs B R (@List.map A B f l)) = (@List.ForallOrdPairs A (fun x : A => fun y : A => R (f x) (f y)) l).
Axiom thm_PAIRWISE_IMPLIES : forall {A : Type'}, forall R : A -> A -> Prop, forall R' : A -> A -> Prop, forall l : list A, ((@List.ForallOrdPairs A R l) /\ (forall x : A, forall y : A, ((@List.In A x l) /\ ((@List.In A y l) /\ (R x y))) -> R' x y)) -> @List.ForallOrdPairs A R' l.
Axiom thm_PAIRWISE_TRANSITIVE : forall {A : Type'}, forall R : A -> A -> Prop, forall x : A, forall y : A, forall l : list A, (forall x' : A, forall y' : A, forall z : A, ((R x' y') /\ (R y' z)) -> R x' z) -> (@List.ForallOrdPairs A R (@cons A x (@cons A y l))) = ((R x y) /\ (@List.ForallOrdPairs A R (@cons A y l))).
Axiom thm_LENGTH_LIST_OF_SEQ : forall {A : Type'}, forall s : nat -> A, forall n : nat, (@List.length A (@list_of_seq A s n)) = n.
Axiom thm_EL_LIST_OF_SEQ : forall {A : Type'}, forall s : nat -> A, forall m : nat, forall n : nat, (Peano.lt m n) -> (@EL A m (@list_of_seq A s n)) = (s m).
Axiom thm_LIST_OF_SEQ_EQ_NIL : forall {A : Type'}, forall s : nat -> A, forall n : nat, ((@list_of_seq A s n) = (@nil A)) = (n = (NUMERAL 0)).
Axiom thm_MONO_ALL : forall {A : Type'} (P : A -> Prop) (Q : A -> Prop) (l : list A), (forall x : A, (P x) -> Q x) -> (@List.Forall A P l) -> @List.Forall A Q l.
Axiom thm_MONO_ALL2 : forall {A B : Type'} (P : A -> B -> Prop) (Q : A -> B -> Prop) (l : list A) (l' : list B), (forall x : A, forall y : B, (P x y) -> Q x y) -> (@ALL2 A B P l l') -> @ALL2 A B Q l l'.
Axiom thm_char_INDUCT : forall P : Ascii.ascii -> Prop, (forall a0 : Prop, forall a1 : Prop, forall a2 : Prop, forall a3 : Prop, forall a4 : Prop, forall a5 : Prop, forall a6 : Prop, forall a7 : Prop, P (ASCII a0 a1 a2 a3 a4 a5 a6 a7)) -> forall x : Ascii.ascii, P x.
Axiom thm_char_RECURSION : forall {Z : Type'}, forall f : Prop -> Prop -> Prop -> Prop -> Prop -> Prop -> Prop -> Prop -> Z, exists fn : Ascii.ascii -> Z, forall a0 : Prop, forall a1 : Prop, forall a2 : Prop, forall a3 : Prop, forall a4 : Prop, forall a5 : Prop, forall a6 : Prop, forall a7 : Prop, (fn (ASCII a0 a1 a2 a3 a4 a5 a6 a7)) = (f a0 a1 a2 a3 a4 a5 a6 a7).
