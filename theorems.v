Require Import HOLLight_Real.HOLLight_Real HOLLight Rdefinitions Rbasic_fun Raxioms.
Require Import HOLLight.theory_hol.
Require Import HOLLight.terms.
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
Axiom thm_FORALL_UNWIND_THM1 : forall {A : Type'}, forall P : A -> Prop, forall a : A, (forall x : A, (a = x) -> P x) = (P a).
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
Axiom thm_WLOG_RELATION : forall {A : Type'}, forall R' : A -> A -> Prop, forall P : A -> A -> Prop, ((forall x : A, forall y : A, (P x y) -> P y x) /\ ((forall x : A, forall y : A, (R' x y) \/ (R' y x)) /\ (forall x : A, forall y : A, (R' x y) -> P x y))) -> forall x : A, forall y : A, P x y.
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
Axiom thm_SKOLEM_THM_GEN : forall {A B : Type'}, forall P : A -> Prop, forall R' : A -> B -> Prop, (forall x : A, (P x) -> exists y : B, R' x y) = (exists f : A -> B, forall x : A, (P x) -> R' x (f x)).
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
Axiom thm_ETA_ONE : forall {A : Type'}, forall f : unit -> A, (fun x : unit => f tt) = f.
Axiom thm_LET_DEF : forall {A B : Type'}, forall f : A -> B, forall x : A, (@LET A B f x) = (f x).
Axiom thm_LET_END_DEF : forall {A : Type'}, forall t : A, (@LET_END A t) = t.
Axiom thm_GABS_DEF : forall {A : Type'}, forall P : A -> Prop, (@GABS A P) = (@ε A P).
Axiom thm_GEQ_DEF : forall {A : Type'}, forall a : A, forall b : A, (@GEQ A a b) = (a = b).
Axiom thm__SEQPATTERN : forall {A B : Type'}, (@_SEQPATTERN A B) = (fun r : A -> B -> Prop => fun s : A -> B -> Prop => fun x : A => @COND (B -> Prop) (exists y : B, r x y) (r x) (s x)).
Axiom thm__UNGUARDED_PATTERN : _UNGUARDED_PATTERN = (fun p : Prop => fun r : Prop => p /\ r).
Axiom thm__GUARDED_PATTERN : _GUARDED_PATTERN = (fun p : Prop => fun g : Prop => fun r : Prop => p /\ (g /\ r)).
Axiom thm__MATCH : forall {A B : Type'}, (@_MATCH A B) = (fun e : A => fun r : A -> B -> Prop => @COND B (@ex1 B (r e)) (@ε B (r e)) (@ε B (fun z : B => False))).
Axiom thm__FUNCTION : forall {A B : Type'}, (@_FUNCTION A B) = (fun r : A -> B -> Prop => fun x : A => @COND B (@ex1 B (r x)) (@ε B (r x)) (@ε B (fun z : B => False))).
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
Axiom thm_pair_INDUCT : forall {A B : Type'}, forall P : (prod A B) -> Prop, (forall x : A, forall y : B, P (@pair A B x y)) -> forall p : prod A B, P p.
Axiom thm_pair_RECURSION : forall {A B C : Type'}, forall PAIR' : A -> B -> C, exists fn : (prod A B) -> C, forall a0 : A, forall a1 : B, (fn (@pair A B a0 a1)) = (PAIR' a0 a1).
Axiom thm_CURRY_DEF : forall {A B C : Type'}, forall f : (prod A B) -> C, forall x : A, forall y : B, (@CURRY A B C f x y) = (f (@pair A B x y)).
Axiom thm_UNCURRY_DEF : forall {A B C : Type'}, forall f : A -> B -> C, forall x : A, forall y : B, (@UNCURRY A B C f (@pair A B x y)) = (f x y).
Axiom thm_PASSOC_DEF : forall {A B C D : Type'}, forall f : (prod (prod A B) C) -> D, forall x : A, forall y : B, forall z : C, (@PASSOC A B C D f (@pair A (prod B C) x (@pair B C y z))) = (f (@pair (prod A B) C (@pair A B x y) z)).
Axiom thm_FORALL_PAIR_THM : forall {A B : Type'}, forall P : (prod A B) -> Prop, (forall p : prod A B, P p) = (forall p1 : A, forall p2 : B, P (@pair A B p1 p2)).
Axiom thm_EXISTS_PAIR_THM : forall {A B : Type'}, forall P : (prod A B) -> Prop, (exists p : prod A B, P p) = (exists p1 : A, exists p2 : B, P (@pair A B p1 p2)).
Axiom thm_LAMBDA_PAIR_THM : forall {A B C : Type'}, forall t : (prod A B) -> C, (fun p : prod A B => t p) = (@GABS ((prod A B) -> C) (fun f : (prod A B) -> C => forall x : A, forall y : B, @GEQ C (f (@pair A B x y)) (t (@pair A B x y)))).
Axiom thm_LAMBDA_PAIR : forall {A B C : Type'}, forall f : A -> B -> C, (@GABS ((prod A B) -> C) (fun f' : (prod A B) -> C => forall x : A, forall y : B, @GEQ C (f' (@pair A B x y)) (f x y))) = (fun p : prod A B => f (@fst A B p) (@snd A B p)).
Axiom thm_LAMBDA_TRIPLE_THM : forall {A B C D : Type'}, forall f : (prod A (prod B C)) -> D, (fun t : prod A (prod B C) => f t) = (@GABS ((prod A (prod B C)) -> D) (fun f' : (prod A (prod B C)) -> D => forall x : A, forall y : B, forall z : C, @GEQ D (f' (@pair A (prod B C) x (@pair B C y z))) (f (@pair A (prod B C) x (@pair B C y z))))).
Axiom thm_LAMBDA_TRIPLE : forall {A B C D : Type'}, forall f : A -> B -> C -> D, (@GABS ((prod A (prod B C)) -> D) (fun f' : (prod A (prod B C)) -> D => forall x : A, forall y : B, forall z : C, @GEQ D (f' (@pair A (prod B C) x (@pair B C y z))) (f x y z))) = (fun t : prod A (prod B C) => f (@fst A (prod B C) t) (@fst B C (@snd A (prod B C) t)) (@snd B C (@snd A (prod B C) t))).
Axiom thm_PAIRED_ETA_THM : forall {A B C D E : Type'}, (forall f : (prod A B) -> C, (@GABS ((prod A B) -> C) (fun f' : (prod A B) -> C => forall x : A, forall y : B, @GEQ C (f' (@pair A B x y)) (f (@pair A B x y)))) = f) /\ ((forall f : (prod A (prod B C)) -> D, (@GABS ((prod A (prod B C)) -> D) (fun f' : (prod A (prod B C)) -> D => forall x : A, forall y : B, forall z : C, @GEQ D (f' (@pair A (prod B C) x (@pair B C y z))) (f (@pair A (prod B C) x (@pair B C y z))))) = f) /\ (forall f : (prod A (prod B (prod C D))) -> E, (@GABS ((prod A (prod B (prod C D))) -> E) (fun f' : (prod A (prod B (prod C D))) -> E => forall w : A, forall x : B, forall y : C, forall z : D, @GEQ E (f' (@pair A (prod B (prod C D)) w (@pair B (prod C D) x (@pair C D y z)))) (f (@pair A (prod B (prod C D)) w (@pair B (prod C D) x (@pair C D y z)))))) = f)).
Axiom thm_FORALL_UNCURRY : forall {A B C : Type'}, forall P : (A -> B -> C) -> Prop, (forall f : A -> B -> C, P f) = (forall f : (prod A B) -> C, P (fun a : A => fun b : B => f (@pair A B a b))).
Axiom thm_EXISTS_UNCURRY : forall {A B C : Type'}, forall P : (A -> B -> C) -> Prop, (exists f : A -> B -> C, P f) = (exists f : (prod A B) -> C, P (fun a : A => fun b : B => f (@pair A B a b))).
Axiom thm_EXISTS_CURRY : forall {A B C : Type'}, forall P : ((prod A B) -> C) -> Prop, (exists f : (prod A B) -> C, P f) = (exists f : A -> B -> C, P (@GABS ((prod A B) -> C) (fun f' : (prod A B) -> C => forall a : A, forall b : B, @GEQ C (f' (@pair A B a b)) (f a b)))).
Axiom thm_FORALL_CURRY : forall {A B C : Type'}, forall P : ((prod A B) -> C) -> Prop, (forall f : (prod A B) -> C, P f) = (forall f : A -> B -> C, P (@GABS ((prod A B) -> C) (fun f' : (prod A B) -> C => forall a : A, forall b : B, @GEQ C (f' (@pair A B a b)) (f a b)))).
Axiom thm_FORALL_UNPAIR_THM : forall {A B : Type'}, forall P : A -> B -> Prop, (forall x : A, forall y : B, P x y) = (forall z : prod A B, P (@fst A B z) (@snd A B z)).
Axiom thm_EXISTS_UNPAIR_THM : forall {A B : Type'}, forall P : A -> B -> Prop, (exists x : A, exists y : B, P x y) = (exists z : prod A B, P (@fst A B z) (@snd A B z)).
Axiom thm_FORALL_PAIR_FUN_THM : forall {A B C : Type'}, forall P : (A -> prod B C) -> Prop, (forall f : A -> prod B C, P f) = (forall g : A -> B, forall h : A -> C, P (fun a : A => @pair B C (g a) (h a))).
Axiom thm_EXISTS_PAIR_FUN_THM : forall {A B C : Type'}, forall P : (A -> prod B C) -> Prop, (exists f : A -> prod B C, P f) = (exists g : A -> B, exists h : A -> C, P (fun a : A => @pair B C (g a) (h a))).
Axiom thm_FORALL_UNPAIR_FUN_THM : forall {A B C : Type'}, forall P : (A -> B) -> (A -> C) -> Prop, (forall f : A -> B, forall g : A -> C, P f g) = (forall h : A -> prod B C, P (@o A (prod B C) B (@fst B C) h) (@o A (prod B C) C (@snd B C) h)).
Axiom thm_EXISTS_UNPAIR_FUN_THM : forall {A B C : Type'}, forall P : (A -> B) -> (A -> C) -> Prop, (exists f : A -> B, exists g : A -> C, P f g) = (exists h : A -> prod B C, P (@o A (prod B C) B (@fst B C) h) (@o A (prod B C) C (@snd B C) h)).
Axiom thm_EXISTS_SWAP_FUN_THM : forall {A B C : Type'}, forall P : (A -> B -> C) -> Prop, (exists f : A -> B -> C, P f) = (exists f : B -> A -> C, P (fun a : A => fun b : B => f b a)).
Axiom thm_FORALL_PAIRED_THM : forall {A B : Type'}, forall P : A -> B -> Prop, (all (@GABS ((prod A B) -> Prop) (fun f : (prod A B) -> Prop => forall x : A, forall y : B, @GEQ Prop (f (@pair A B x y)) (P x y)))) = (forall x : A, forall y : B, P x y).
Axiom thm_EXISTS_PAIRED_THM : forall {A B : Type'}, forall P : A -> B -> Prop, (ex (@GABS ((prod A B) -> Prop) (fun f : (prod A B) -> Prop => forall x : A, forall y : B, @GEQ Prop (f (@pair A B x y)) (P x y)))) = (exists x : A, exists y : B, P x y).
Axiom thm_FORALL_TRIPLED_THM : forall {A B C : Type'}, forall P : A -> B -> C -> Prop, (all (@GABS ((prod A (prod B C)) -> Prop) (fun f : (prod A (prod B C)) -> Prop => forall x : A, forall y : B, forall z : C, @GEQ Prop (f (@pair A (prod B C) x (@pair B C y z))) (P x y z)))) = (forall x : A, forall y : B, forall z : C, P x y z).
Axiom thm_EXISTS_TRIPLED_THM : forall {A B C : Type'}, forall P : A -> B -> C -> Prop, (ex (@GABS ((prod A (prod B C)) -> Prop) (fun f : (prod A (prod B C)) -> Prop => forall x : A, forall y : B, forall z : C, @GEQ Prop (f (@pair A (prod B C) x (@pair B C y z))) (P x y z)))) = (exists x : A, exists y : B, exists z : C, P x y z).
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
Axiom thm_TRANSITIVE_STEPWISE_LT_EQ : forall R' : nat -> nat -> Prop, (forall x : nat, forall y : nat, forall z : nat, ((R' x y) /\ (R' y z)) -> R' x z) -> (forall m : nat, forall n : nat, (Peano.lt m n) -> R' m n) = (forall n : nat, R' n (S n)).
Axiom thm_TRANSITIVE_STEPWISE_LT : forall R' : nat -> nat -> Prop, ((forall x : nat, forall y : nat, forall z : nat, ((R' x y) /\ (R' y z)) -> R' x z) /\ (forall n : nat, R' n (S n))) -> forall m : nat, forall n : nat, (Peano.lt m n) -> R' m n.
Axiom thm_TRANSITIVE_STEPWISE_LE_EQ : forall R' : nat -> nat -> Prop, ((forall x : nat, R' x x) /\ (forall x : nat, forall y : nat, forall z : nat, ((R' x y) /\ (R' y z)) -> R' x z)) -> (forall m : nat, forall n : nat, (Peano.le m n) -> R' m n) = (forall n : nat, R' n (S n)).
Axiom thm_TRANSITIVE_STEPWISE_LE : forall R' : nat -> nat -> Prop, ((forall x : nat, R' x x) /\ ((forall x : nat, forall y : nat, forall z : nat, ((R' x y) /\ (R' y z)) -> R' x z) /\ (forall n : nat, R' n (S n)))) -> forall m : nat, forall n : nat, (Peano.le m n) -> R' m n.
Axiom thm_DEPENDENT_CHOICE_FIXED : forall {A : Type'}, forall P : nat -> A -> Prop, forall R' : nat -> A -> A -> Prop, forall a : A, ((P (NUMERAL 0) a) /\ (forall n : nat, forall x : A, (P n x) -> exists y : A, (P (S n) y) /\ (R' n x y))) -> exists f : nat -> A, ((f (NUMERAL 0)) = a) /\ ((forall n : nat, P n (f n)) /\ (forall n : nat, R' n (f n) (f (S n)))).
Axiom thm_DEPENDENT_CHOICE : forall {A : Type'}, forall P : nat -> A -> Prop, forall R' : nat -> A -> A -> Prop, ((exists a : A, P (NUMERAL 0) a) /\ (forall n : nat, forall x : A, (P n x) -> exists y : A, (P (S n) y) /\ (R' n x y))) -> exists f : nat -> A, (forall n : nat, P n (f n)) /\ (forall n : nat, R' n (f n) (f (S n))).
Axiom thm_WF : forall {A : Type'}, forall lt2 : A -> A -> Prop, (@WF A lt2) = (forall P : A -> Prop, (exists x : A, P x) -> exists x : A, (P x) /\ (forall y : A, (lt2 y x) -> ~ (P y))).
Axiom thm_WF_EQ : forall {A : Type'} (lt2 : A -> A -> Prop), (@WF A lt2) = (forall P : A -> Prop, (exists x : A, P x) = (exists x : A, (P x) /\ (forall y : A, (lt2 y x) -> ~ (P y)))).
Axiom thm_WF_IND : forall {A : Type'} (lt2 : A -> A -> Prop), (@WF A lt2) = (forall P : A -> Prop, (forall x : A, (forall y : A, (lt2 y x) -> P y) -> P x) -> forall x : A, P x).
Axiom thm_WF_DCHAIN : forall {A : Type'} (lt2 : A -> A -> Prop), (@WF A lt2) = (~ (exists s : nat -> A, forall n : nat, lt2 (s (S n)) (s n))).
Axiom thm_WF_DHAIN_TRANSITIVE : forall {A : Type'}, forall lt2 : A -> A -> Prop, (forall x : A, forall y : A, forall z : A, ((lt2 x y) /\ (lt2 y z)) -> lt2 x z) -> (@WF A lt2) = (~ (exists s : nat -> A, forall i : nat, forall j : nat, (Peano.lt i j) -> lt2 (s j) (s i))).
Axiom thm_WF_UREC : forall {A B : Type'} (lt2 : A -> A -> Prop), (@WF A lt2) -> forall H : (A -> B) -> A -> B, (forall f : A -> B, forall g : A -> B, forall x : A, (forall z : A, (lt2 z x) -> (f z) = (g z)) -> (H f x) = (H g x)) -> forall f : A -> B, forall g : A -> B, ((forall x : A, (f x) = (H f x)) /\ (forall x : A, (g x) = (H g x))) -> f = g.
Axiom thm_WF_UREC_WF : forall {A : Type'} (lt2 : A -> A -> Prop), (forall H : (A -> Prop) -> A -> Prop, (forall f : A -> Prop, forall g : A -> Prop, forall x : A, (forall z : A, (lt2 z x) -> (f z) = (g z)) -> (H f x) = (H g x)) -> forall f : A -> Prop, forall g : A -> Prop, ((forall x : A, (f x) = (H f x)) /\ (forall x : A, (g x) = (H g x))) -> f = g) -> @WF A lt2.
Axiom thm_WF_REC_INVARIANT : forall {A B : Type'} (lt2 : A -> A -> Prop), (@WF A lt2) -> forall H : (A -> B) -> A -> B, forall S' : A -> B -> Prop, (forall f : A -> B, forall g : A -> B, forall x : A, (forall z : A, (lt2 z x) -> ((f z) = (g z)) /\ (S' z (f z))) -> ((H f x) = (H g x)) /\ (S' x (H f x))) -> exists f : A -> B, forall x : A, (f x) = (H f x).
Axiom thm_WF_REC : forall {A B : Type'} (lt2 : A -> A -> Prop), (@WF A lt2) -> forall H : (A -> B) -> A -> B, (forall f : A -> B, forall g : A -> B, forall x : A, (forall z : A, (lt2 z x) -> (f z) = (g z)) -> (H f x) = (H g x)) -> exists f : A -> B, forall x : A, (f x) = (H f x).
Axiom thm_WF_REC_WF : forall {A : Type'} (lt2 : A -> A -> Prop), (forall H : (A -> nat) -> A -> nat, (forall f : A -> nat, forall g : A -> nat, forall x : A, (forall z : A, (lt2 z x) -> (f z) = (g z)) -> (H f x) = (H g x)) -> exists f : A -> nat, forall x : A, (f x) = (H f x)) -> @WF A lt2.
Axiom thm_WF_EREC : forall {A B : Type'} (lt2 : A -> A -> Prop), (@WF A lt2) -> forall H : (A -> B) -> A -> B, (forall f : A -> B, forall g : A -> B, forall x : A, (forall z : A, (lt2 z x) -> (f z) = (g z)) -> (H f x) = (H g x)) -> @ex1 (A -> B) (fun f : A -> B => forall x : A, (f x) = (H f x)).
Axiom thm_WF_REC_EXISTS : forall {A B : Type'} (lt2 : A -> A -> Prop), (@WF A lt2) -> forall P : (A -> B) -> A -> B -> Prop, ((forall f : A -> B, forall g : A -> B, forall x : A, forall y : B, (forall z : A, (lt2 z x) -> (f z) = (g z)) -> (P f x y) = (P g x y)) /\ (forall f : A -> B, forall x : A, (forall z : A, (lt2 z x) -> P f z (f z)) -> exists y : B, P f x y)) -> exists f : A -> B, forall x : A, P f x (f x).
Axiom thm_WF_SUBSET : forall {A : Type'}, forall lt2 : A -> A -> Prop, forall lt3 : A -> A -> Prop, ((forall x : A, forall y : A, (lt2 x y) -> lt3 x y) /\ (@WF A lt3)) -> @WF A lt2.
Axiom thm_WF_RESTRICT : forall {A : Type'}, forall lt2 : A -> A -> Prop, forall P : A -> Prop, (@WF A lt2) -> @WF A (fun x : A => fun y : A => (P x) /\ ((P y) /\ (lt2 x y))).
Axiom thm_WF_MEASURE_GEN : forall {A B : Type'}, forall lt2 : B -> B -> Prop, forall m : A -> B, (@WF B lt2) -> @WF A (fun x : A => fun x' : A => lt2 (m x) (m x')).
Axiom thm_WF_LEX_DEPENDENT : forall {A B : Type'}, forall R' : A -> A -> Prop, forall S' : A -> B -> B -> Prop, ((@WF A R') /\ (forall a : A, @WF B (S' a))) -> @WF (prod A B) (@GABS ((prod A B) -> (prod A B) -> Prop) (fun f : (prod A B) -> (prod A B) -> Prop => forall r1 : A, forall s1 : B, @GEQ ((prod A B) -> Prop) (f (@pair A B r1 s1)) (@GABS ((prod A B) -> Prop) (fun f' : (prod A B) -> Prop => forall r2 : A, forall s2 : B, @GEQ Prop (f' (@pair A B r2 s2)) ((R' r1 r2) \/ ((r1 = r2) /\ (S' r1 s1 s2))))))).
Axiom thm_WF_LEX : forall {A B : Type'}, forall R' : A -> A -> Prop, forall S' : B -> B -> Prop, ((@WF A R') /\ (@WF B S')) -> @WF (prod A B) (@GABS ((prod A B) -> (prod A B) -> Prop) (fun f : (prod A B) -> (prod A B) -> Prop => forall r1 : A, forall s1 : B, @GEQ ((prod A B) -> Prop) (f (@pair A B r1 s1)) (@GABS ((prod A B) -> Prop) (fun f' : (prod A B) -> Prop => forall r2 : A, forall s2 : B, @GEQ Prop (f' (@pair A B r2 s2)) ((R' r1 r2) \/ ((r1 = r2) /\ (S' s1 s2))))))).
Axiom thm_WF_POINTWISE : forall {A B : Type'} (lt2 : A -> A -> Prop) (lt3 : B -> B -> Prop), ((@WF A lt2) /\ (@WF B lt3)) -> @WF (prod A B) (@GABS ((prod A B) -> (prod A B) -> Prop) (fun f : (prod A B) -> (prod A B) -> Prop => forall x1 : A, forall y1 : B, @GEQ ((prod A B) -> Prop) (f (@pair A B x1 y1)) (@GABS ((prod A B) -> Prop) (fun f' : (prod A B) -> Prop => forall x2 : A, forall y2 : B, @GEQ Prop (f' (@pair A B x2 y2)) ((lt2 x1 x2) /\ (lt3 y1 y2)))))).
Axiom thm_WF_num : @WF nat Peano.lt.
Axiom thm_WF_REC_num : forall {A : Type'}, forall H : (nat -> A) -> nat -> A, (forall f : nat -> A, forall g : nat -> A, forall n : nat, (forall m : nat, (Peano.lt m n) -> (f m) = (g m)) -> (H f n) = (H g n)) -> exists f : nat -> A, forall n : nat, (f n) = (H f n).
Axiom thm_MEASURE : forall {A : Type'}, forall m : A -> nat, (@MEASURE A m) = (fun x : A => fun y : A => Peano.lt (m x) (m y)).
Axiom thm_WF_MEASURE : forall {A : Type'}, forall m : A -> nat, @WF A (@MEASURE A m).
Axiom thm_MEASURE_LE : forall {A : Type'} (a : A) (b : A), forall m : A -> nat, (forall y : A, (@MEASURE A m y a) -> @MEASURE A m y b) = (Peano.le (m a) (m b)).
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
Axiom thm_sum_INDUCT : forall {A B : Type'}, forall P : (Datatypes.sum A B) -> Prop, ((forall a : A, P (@inl A B a)) /\ (forall a : B, P (@inr A B a))) -> forall x : Datatypes.sum A B, P x.
Axiom thm_sum_RECURSION : forall {A B Z : Type'}, forall INL' : A -> Z, forall INR' : B -> Z, exists fn : (Datatypes.sum A B) -> Z, (forall a : A, (fn (@inl A B a)) = (INL' a)) /\ (forall a : B, (fn (@inr A B a)) = (INR' a)).
Axiom thm_OUTL : forall {A B : Type'} (x : A), (@OUTL A B (@inl A B x)) = x.
Axiom thm_OUTR : forall {A B : Type'} (y : B), (@OUTR A B (@inr A B y)) = y.
Axiom thm_option_INDUCT : forall {A : Type'}, forall P : (option A) -> Prop, ((P (@None A)) /\ (forall a : A, P (@Some A a))) -> forall x : option A, P x.
Axiom thm_option_RECURSION : forall {A Z : Type'}, forall NONE' : Z, forall SOME' : A -> Z, exists fn : (option A) -> Z, ((fn (@None A)) = NONE') /\ (forall a : A, (fn (@Some A a)) = (SOME' a)).
Axiom thm_list_INDUCT : forall {A : Type'}, forall P : (list A) -> Prop, ((P (@nil A)) /\ (forall a0 : A, forall a1 : list A, (P a1) -> P (@cons A a0 a1))) -> forall x : list A, P x.
Axiom thm_list_RECURSION : forall {A Z : Type'}, forall NIL' : Z, forall CONS' : A -> (list A) -> Z -> Z, exists fn : (list A) -> Z, ((fn (@nil A)) = NIL') /\ (forall a0 : A, forall a1 : list A, (fn (@cons A a0 a1)) = (CONS' a0 a1 (fn a1))).
Axiom thm_FORALL_OPTION_THM : forall {A : Type'}, forall P : (option A) -> Prop, (forall x : option A, P x) = ((P (@None A)) /\ (forall a : A, P (@Some A a))).
Axiom thm_EXISTS_OPTION_THM : forall {A : Type'}, forall P : (option A) -> Prop, (exists x : option A, P x) = ((P (@None A)) \/ (exists a : A, P (@Some A a))).
Axiom thm_option_DISTINCT : forall {A : Type'}, forall a : A, ~ ((@Some A a) = (@None A)).
Axiom thm_option_INJ : forall {A : Type'}, forall a : A, forall b : A, ((@Some A a) = (@Some A b)) = (a = b).
Axiom thm_ISO : forall {A B : Type'}, forall g : B -> A, forall f : A -> B, (@ExtensionalityFacts.is_inverse A B f g) = ((forall x : B, (f (g x)) = x) /\ (forall y : A, (g (f y)) = y)).
Axiom thm_ISO_REFL : forall {A : Type'}, @ExtensionalityFacts.is_inverse A A (fun x : A => x) (fun x : A => x).
Axiom thm_ISO_FUN : forall {A A' B B' : Type'} (g : B -> B') (f' : A' -> A) (g' : B' -> B) (f : A -> A'), ((@ExtensionalityFacts.is_inverse A A' f f') /\ (@ExtensionalityFacts.is_inverse B B' g g')) -> @ExtensionalityFacts.is_inverse (A -> B) (A' -> B') (fun h : A -> B => fun a' : A' => g (h (f' a'))) (fun h : A' -> B' => fun a : A => g' (h (f a))).
Axiom thm_ISO_USAGE : forall {A B : Type'} (g : B -> A) (f : A -> B), (@ExtensionalityFacts.is_inverse A B f g) -> (forall P : A -> Prop, (forall x : A, P x) = (forall x : B, P (g x))) /\ ((forall P : A -> Prop, (exists x : A, P x) = (exists x : B, P (g x))) /\ (forall a : A, forall b : B, (a = (g b)) = ((f a) = b))).
Axiom thm_HD : forall {A : Type'} (t : list A) (h : A), (@hd A (@cons A h t)) = h.
Axiom thm_TL : forall {A : Type'} (h : A) (t : list A), (@tl A (@cons A h t)) = t.
Axiom thm_APPEND : forall {A : Type'}, (forall l : list A, (@List.app A (@nil A) l) = l) /\ (forall h : A, forall t : list A, forall l : list A, (@List.app A (@cons A h t) l) = (@cons A h (@List.app A t l))).
Axiom thm_REVERSE : forall {A : Type'} (l : list A) (x : A), ((@List.rev A (@nil A)) = (@nil A)) /\ ((@List.rev A (@cons A x l)) = (@List.app A (@List.rev A l) (@cons A x (@nil A)))).
Axiom thm_LENGTH : forall {A : Type'}, ((@List.length A (@nil A)) = (NUMERAL 0)) /\ (forall h : A, forall t : list A, (@List.length A (@cons A h t)) = (S (@List.length A t))).
Axiom thm_MAP : forall {A B : Type'}, (forall f : A -> B, (@List.map A B f (@nil A)) = (@nil B)) /\ (forall f : A -> B, forall h : A, forall t : list A, (@List.map A B f (@cons A h t)) = (@cons B (f h) (@List.map A B f t))).
Axiom thm_LAST : forall {A : Type'} (h : A) (t : list A), (@LAST A (@cons A h t)) = (@COND A (t = (@nil A)) h (@LAST A t)).
Axiom thm_BUTLAST : forall {A : Type'} (h : A) (t : list A), ((@List.removelast A (@nil A)) = (@nil A)) /\ ((@List.removelast A (@cons A h t)) = (@COND (list A) (t = (@nil A)) (@nil A) (@cons A h (@List.removelast A t)))).
Axiom thm_REPLICATE : forall {A : Type'} (n : nat) (x : A), ((@repeat_with_perm_args A (NUMERAL 0) x) = (@nil A)) /\ ((@repeat_with_perm_args A (S n) x) = (@cons A x (@repeat_with_perm_args A n x))).
Axiom thm_NULL : forall {A : Type'} (h : A) (t : list A), ((@NULL A (@nil A)) = True) /\ ((@NULL A (@cons A h t)) = False).
Axiom thm_ALL : forall {A : Type'} (h : A) (P : A -> Prop) (t : list A), ((@List.Forall A P (@nil A)) = True) /\ ((@List.Forall A P (@cons A h t)) = ((P h) /\ (@List.Forall A P t))).
Axiom thm_EX : forall {A : Type'} (h : A) (P : A -> Prop) (t : list A), ((@EX A P (@nil A)) = False) /\ ((@EX A P (@cons A h t)) = ((P h) \/ (@EX A P t))).
Axiom thm_ITLIST : forall {A B : Type'} (h : A) (f : A -> B -> B) (t : list A) (b : B), ((@ITLIST A B f (@nil A) b) = b) /\ ((@ITLIST A B f (@cons A h t) b) = (f h (@ITLIST A B f t b))).
Axiom thm_MEM : forall {A : Type'} (h : A) (x : A) (t : list A), ((@List.In A x (@nil A)) = False) /\ ((@List.In A x (@cons A h t)) = ((x = h) \/ (@List.In A x t))).
Axiom thm_ALL2_DEF : forall {A B : Type'} (h1' : A) (P : A -> B -> Prop) (t1 : list A) (l2 : list B), ((@ALL2 A B P (@nil A) l2) = (l2 = (@nil B))) /\ ((@ALL2 A B P (@cons A h1' t1) l2) = (@COND Prop (l2 = (@nil B)) False ((P h1' (@hd B l2)) /\ (@ALL2 A B P t1 (@tl B l2))))).
Axiom thm_ALL2 : forall {A B : Type'} (h1' : A) (h2' : B) (P : A -> B -> Prop) (t1 : list A) (t2 : list B), ((@ALL2 A B P (@nil A) (@nil B)) = True) /\ (((@ALL2 A B P (@cons A h1' t1) (@nil B)) = False) /\ (((@ALL2 A B P (@nil A) (@cons B h2' t2)) = False) /\ ((@ALL2 A B P (@cons A h1' t1) (@cons B h2' t2)) = ((P h1' h2') /\ (@ALL2 A B P t1 t2))))).
Axiom thm_MAP2_DEF : forall {A B C : Type'} (h1' : A) (f : A -> B -> C) (t1 : list A) (l : list B), ((@MAP2 A B C f (@nil A) l) = (@nil C)) /\ ((@MAP2 A B C f (@cons A h1' t1) l) = (@cons C (f h1' (@hd B l)) (@MAP2 A B C f t1 (@tl B l)))).
Axiom thm_MAP2 : forall {A B C : Type'} (h1' : A) (h2' : B) (f : A -> B -> C) (t1 : list A) (t2 : list B), ((@MAP2 A B C f (@nil A) (@nil B)) = (@nil C)) /\ ((@MAP2 A B C f (@cons A h1' t1) (@cons B h2' t2)) = (@cons C (f h1' h2') (@MAP2 A B C f t1 t2))).
Axiom thm_EL : forall {A : Type'} (n : nat) (l : list A), ((@EL A (NUMERAL 0) l) = (@hd A l)) /\ ((@EL A (S n) l) = (@EL A n (@tl A l))).
Axiom thm_FILTER : forall {A : Type'} (h : A) (P : A -> Prop) (t : list A), ((@FILTER A P (@nil A)) = (@nil A)) /\ ((@FILTER A P (@cons A h t)) = (@COND (list A) (P h) (@cons A h (@FILTER A P t)) (@FILTER A P t))).
Axiom thm_ASSOC : forall {A B : Type'} (h : prod A B) (a : A) (t : list (prod A B)), (@ASSOC A B a (@cons (prod A B) h t)) = (@COND B ((@fst A B h) = a) (@snd A B h) (@ASSOC A B a t)).
Axiom thm_ITLIST2_DEF : forall {A B C : Type'} (h1' : A) (f : A -> B -> C -> C) (t1 : list A) (l2 : list B) (b : C), ((@ITLIST2 A B C f (@nil A) l2 b) = b) /\ ((@ITLIST2 A B C f (@cons A h1' t1) l2 b) = (f h1' (@hd B l2) (@ITLIST2 A B C f t1 (@tl B l2) b))).
Axiom thm_ITLIST2 : forall {A B C : Type'} (h1' : A) (h2' : B) (f : A -> B -> C -> C) (t1 : list A) (t2 : list B) (b : C), ((@ITLIST2 A B C f (@nil A) (@nil B) b) = b) /\ ((@ITLIST2 A B C f (@cons A h1' t1) (@cons B h2' t2) b) = (f h1' h2' (@ITLIST2 A B C f t1 t2 b))).
Axiom thm_ZIP_DEF : forall {A B : Type'} (h1' : A) (t1 : list A) (l2 : list B), ((@ZIP A B (@nil A) l2) = (@nil (prod A B))) /\ ((@ZIP A B (@cons A h1' t1) l2) = (@cons (prod A B) (@pair A B h1' (@hd B l2)) (@ZIP A B t1 (@tl B l2)))).
Axiom thm_ZIP : forall {A B : Type'} (h1' : A) (h2' : B) (t1 : list A) (t2 : list B), ((@ZIP A B (@nil A) (@nil B)) = (@nil (prod A B))) /\ ((@ZIP A B (@cons A h1' t1) (@cons B h2' t2)) = (@cons (prod A B) (@pair A B h1' h2') (@ZIP A B t1 t2))).
Axiom thm_ALLPAIRS : forall {A B : Type'} (h : A) (f : A -> B -> Prop) (t : list A) (l : list B), ((@ALLPAIRS A B f (@nil A) l) = True) /\ ((@ALLPAIRS A B f (@cons A h t) l) = ((@List.Forall B (f h) l) /\ (@ALLPAIRS A B f t l))).
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
Axiom thm_LENGTH_EQ_CONS : forall {A : Type'}, forall l : list A, forall n : nat, ((@List.length A l) = (S n)) = (exists h : A, exists t : list A, (l = (@cons A h t)) /\ ((@List.length A t) = n)).
Axiom thm_LENGTH_REVERSE : forall {A : Type'}, forall l : list A, (@List.length A (@List.rev A l)) = (@List.length A l).
Axiom thm_MAP_o : forall {A B C : Type'}, forall f : A -> B, forall g : B -> C, forall l : list A, (@List.map A C (@o A B C g f) l) = (@List.map B C g (@List.map A B f l)).
Axiom thm_MAP_EQ : forall {A B : Type'}, forall f : A -> B, forall g : A -> B, forall l : list A, (@List.Forall A (fun x : A => (f x) = (g x)) l) -> (@List.map A B f l) = (@List.map A B g l).
Axiom thm_ALL_IMP : forall {A : Type'}, forall P : A -> Prop, forall Q : A -> Prop, forall l : list A, ((forall x : A, ((@List.In A x l) /\ (P x)) -> Q x) /\ (@List.Forall A P l)) -> @List.Forall A Q l.
Axiom thm_NOT_EX : forall {A : Type'}, forall P : A -> Prop, forall l : list A, (~ (@EX A P l)) = (@List.Forall A (fun x : A => ~ (P x)) l).
Axiom thm_NOT_ALL : forall {A : Type'}, forall P : A -> Prop, forall l : list A, (~ (@List.Forall A P l)) = (@EX A (fun x : A => ~ (P x)) l).
Axiom thm_ALL_MAP : forall {A B : Type'}, forall P : B -> Prop, forall f : A -> B, forall l : list A, (@List.Forall B P (@List.map A B f l)) = (@List.Forall A (@o A B Prop P f) l).
Axiom thm_ALL_EQ : forall {A : Type'} (R' : A -> Prop) (P : A -> Prop) (Q : A -> Prop), forall l : list A, ((@List.Forall A R' l) /\ (forall x : A, (R' x) -> (P x) = (Q x))) -> (@List.Forall A P l) = (@List.Forall A Q l).
Axiom thm_ALL_T : forall {A : Type'}, forall l : list A, @List.Forall A (fun x : A => True) l.
Axiom thm_MAP_EQ_ALL2 : forall {A B : Type'}, forall f : A -> B, forall l : list A, forall m : list A, (@ALL2 A A (fun x : A => fun y : A => (f x) = (f y)) l m) -> (@List.map A B f l) = (@List.map A B f m).
Axiom thm_ALL2_MAP : forall {A B : Type'}, forall P : B -> A -> Prop, forall f : A -> B, forall l : list A, (@ALL2 B A P (@List.map A B f l) l) = (@List.Forall A (fun a : A => P (f a) a) l).
Axiom thm_MAP_EQ_DEGEN : forall {A : Type'}, forall l : list A, forall f : A -> A, (@List.Forall A (fun x : A => (f x) = x) l) -> (@List.map A A f l) = l.
Axiom thm_ALL2_AND_RIGHT : forall {A B : Type'}, forall l : list A, forall m : list B, forall P : A -> Prop, forall Q : A -> B -> Prop, (@ALL2 A B (fun x : A => fun y : B => (P x) /\ (Q x y)) l m) = ((@List.Forall A P l) /\ (@ALL2 A B Q l m)).
Axiom thm_ITLIST_APPEND : forall {A B : Type'}, forall f : A -> B -> B, forall a : B, forall l1 : list A, forall l2 : list A, (@ITLIST A B f (@List.app A l1 l2) a) = (@ITLIST A B f l1 (@ITLIST A B f l2 a)).
Axiom thm_ITLIST_EXTRA : forall {A B : Type'} (a : A) (b : B), forall f : A -> B -> B, forall l : list A, (@ITLIST A B f (@List.app A l (@cons A a (@nil A))) b) = (@ITLIST A B f l (f a b)).
Axiom thm_ALL_MP : forall {A : Type'}, forall P : A -> Prop, forall Q : A -> Prop, forall l : list A, ((@List.Forall A (fun x : A => (P x) -> Q x) l) /\ (@List.Forall A P l)) -> @List.Forall A Q l.
Axiom thm_AND_ALL : forall {A : Type'} (P : A -> Prop) (Q : A -> Prop), forall l : list A, ((@List.Forall A P l) /\ (@List.Forall A Q l)) = (@List.Forall A (fun x : A => (P x) /\ (Q x)) l).
Axiom thm_EX_IMP : forall {A : Type'}, forall P : A -> Prop, forall Q : A -> Prop, forall l : list A, ((forall x : A, ((@List.In A x l) /\ (P x)) -> Q x) /\ (@EX A P l)) -> @EX A Q l.
Axiom thm_ALL_MEM : forall {A : Type'}, forall P : A -> Prop, forall l : list A, (forall x : A, (@List.In A x l) -> P x) = (@List.Forall A P l).
Axiom thm_LENGTH_REPLICATE : forall {A : Type'}, forall n : nat, forall x : A, (@List.length A (@repeat_with_perm_args A n x)) = n.
Axiom thm_MEM_REPLICATE : forall {A : Type'}, forall n : nat, forall x : A, forall y : A, (@List.In A x (@repeat_with_perm_args A n y)) = ((x = y) /\ (~ (n = (NUMERAL 0)))).
Axiom thm_EX_MAP : forall {A B : Type'}, forall P : B -> Prop, forall f : A -> B, forall l : list A, (@EX B P (@List.map A B f l)) = (@EX A (@o A B Prop P f) l).
Axiom thm_EXISTS_EX : forall {A B : Type'}, forall P : A -> B -> Prop, forall l : list B, (exists x : A, @EX B (P x) l) = (@EX B (fun s : B => exists x : A, P x s) l).
Axiom thm_FORALL_ALL : forall {A B : Type'}, forall P : A -> B -> Prop, forall l : list B, (forall x : A, @List.Forall B (P x) l) = (@List.Forall B (fun s : B => forall x : A, P x s) l).
Axiom thm_MEM_APPEND : forall {A : Type'}, forall x : A, forall l1 : list A, forall l2 : list A, (@List.In A x (@List.app A l1 l2)) = ((@List.In A x l1) \/ (@List.In A x l2)).
Axiom thm_MEM_MAP : forall {A B : Type'}, forall f : A -> B, forall y : B, forall l : list A, (@List.In B y (@List.map A B f l)) = (exists x : A, (@List.In A x l) /\ (y = (f x))).
Axiom thm_FILTER_APPEND : forall {A : Type'}, forall P : A -> Prop, forall l1 : list A, forall l2 : list A, (@FILTER A P (@List.app A l1 l2)) = (@List.app A (@FILTER A P l1) (@FILTER A P l2)).
Axiom thm_FILTER_MAP : forall {A B : Type'}, forall P : B -> Prop, forall f : A -> B, forall l : list A, (@FILTER B P (@List.map A B f l)) = (@List.map A B f (@FILTER A (@o A B Prop P f) l)).
Axiom thm_MEM_FILTER : forall {A : Type'}, forall P : A -> Prop, forall l : list A, forall x : A, (@List.In A x (@FILTER A P l)) = ((P x) /\ (@List.In A x l)).
Axiom thm_EX_MEM : forall {A : Type'}, forall P : A -> Prop, forall l : list A, (exists x : A, (P x) /\ (@List.In A x l)) = (@EX A P l).
Axiom thm_MAP_FST_ZIP : forall {A B : Type'}, forall l1 : list A, forall l2 : list B, ((@List.length A l1) = (@List.length B l2)) -> (@List.map (prod A B) A (@fst A B) (@ZIP A B l1 l2)) = l1.
Axiom thm_MAP_SND_ZIP : forall {A B : Type'}, forall l1 : list A, forall l2 : list B, ((@List.length A l1) = (@List.length B l2)) -> (@List.map (prod A B) B (@snd A B) (@ZIP A B l1 l2)) = l2.
Axiom thm_LENGTH_ZIP : forall {A B : Type'}, forall l1 : list A, forall l2 : list B, ((@List.length A l1) = (@List.length B l2)) -> (@List.length (prod A B) (@ZIP A B l1 l2)) = (@List.length B l2).
Axiom thm_MEM_ASSOC : forall {A B : Type'}, forall l : list (prod A B), forall x : A, (@List.In (prod A B) (@pair A B x (@ASSOC A B x l)) l) = (@List.In A x (@List.map (prod A B) A (@fst A B) l)).
Axiom thm_ALL_APPEND : forall {A : Type'}, forall P : A -> Prop, forall l1 : list A, forall l2 : list A, (@List.Forall A P (@List.app A l1 l2)) = ((@List.Forall A P l1) /\ (@List.Forall A P l2)).
Axiom thm_MEM_EL : forall {A : Type'}, forall l : list A, forall n : nat, (Peano.lt n (@List.length A l)) -> @List.In A (@EL A n l) l.
Axiom thm_MEM_EXISTS_EL : forall {A : Type'}, forall l : list A, forall x : A, (@List.In A x l) = (exists i : nat, (Peano.lt i (@List.length A l)) /\ (x = (@EL A i l))).
Axiom thm_ALL_EL : forall {A : Type'}, forall P : A -> Prop, forall l : list A, (forall i : nat, (Peano.lt i (@List.length A l)) -> P (@EL A i l)) = (@List.Forall A P l).
Axiom thm_ALL2_MAP2 : forall {A B C D : Type'} (P : B -> D -> Prop), forall f : A -> B, forall g : C -> D, forall l : list A, forall m : list C, (@ALL2 B D P (@List.map A B f l) (@List.map C D g m)) = (@ALL2 A C (fun x : A => fun y : C => P (f x) (g y)) l m).
Axiom thm_AND_ALL2 : forall {A B : Type'}, forall P : A -> B -> Prop, forall Q : A -> B -> Prop, forall l : list A, forall m : list B, ((@ALL2 A B P l m) /\ (@ALL2 A B Q l m)) = (@ALL2 A B (fun x : A => fun y : B => (P x y) /\ (Q x y)) l m).
Axiom thm_ALLPAIRS_SYM : forall {A B : Type'}, forall P : A -> B -> Prop, forall l : list A, forall m : list B, (@ALLPAIRS A B P l m) = (@ALLPAIRS B A (fun x : B => fun y : A => P y x) m l).
Axiom thm_ALLPAIRS_MEM : forall {A B : Type'}, forall P : A -> B -> Prop, forall l : list A, forall m : list B, (forall x : A, forall y : B, ((@List.In A x l) /\ (@List.In B y m)) -> P x y) = (@ALLPAIRS A B P l m).
Axiom thm_ALLPAIRS_MAP : forall {A B C D : Type'}, forall P : B -> D -> Prop, forall f : A -> B, forall g : C -> D, forall l : list A, forall m : list C, (@ALLPAIRS B D P (@List.map A B f l) (@List.map C D g m)) = (@ALLPAIRS A C (fun x : A => fun y : C => P (f x) (g y)) l m).
Axiom thm_ALLPAIRS_EQ : forall {A B : Type'} (R' : A -> B -> Prop) (R'' : A -> B -> Prop), forall l : list A, forall m : list B, forall P : A -> Prop, forall Q : B -> Prop, ((@List.Forall A P l) /\ ((@List.Forall B Q m) /\ (forall p : A, forall q : B, ((P p) /\ (Q q)) -> (R' p q) = (R'' p q)))) -> (@ALLPAIRS A B R' l m) = (@ALLPAIRS A B R'' l m).
Axiom thm_ALL2_ALL : forall {A : Type'}, forall P : A -> A -> Prop, forall l : list A, (@ALL2 A A P l l) = (@List.Forall A (fun x : A => P x x) l).
Axiom thm_APPEND_EQ_NIL : forall {A : Type'}, forall l : list A, forall m : list A, ((@List.app A l m) = (@nil A)) = ((l = (@nil A)) /\ (m = (@nil A))).
Axiom thm_APPEND_LCANCEL : forall {A : Type'}, forall l1 : list A, forall l2 : list A, forall l3 : list A, ((@List.app A l1 l2) = (@List.app A l1 l3)) = (l2 = l3).
Axiom thm_APPEND_RCANCEL : forall {A : Type'}, forall l1 : list A, forall l2 : list A, forall l3 : list A, ((@List.app A l1 l3) = (@List.app A l2 l3)) = (l1 = l2).
Axiom thm_LENGTH_MAP2 : forall {A B C : Type'}, forall f : A -> B -> C, forall l : list A, forall m : list B, ((@List.length A l) = (@List.length B m)) -> (@List.length C (@MAP2 A B C f l m)) = (@List.length B m).
Axiom thm_EL_MAP2 : forall {A B C : Type'}, forall f : A -> B -> C, forall l : list A, forall m : list B, forall k : nat, ((Peano.lt k (@List.length A l)) /\ (Peano.lt k (@List.length B m))) -> (@EL C k (@MAP2 A B C f l m)) = (f (@EL A k l) (@EL B k m)).
Axiom thm_MAP_EQ_NIL : forall {A B : Type'}, forall f : A -> B, forall l : list A, ((@List.map A B f l) = (@nil B)) = (l = (@nil A)).
Axiom thm_INJECTIVE_MAP : forall {A B : Type'}, forall f : A -> B, (forall l : list A, forall m : list A, ((@List.map A B f l) = (@List.map A B f m)) -> l = m) = (forall x : A, forall y : A, ((f x) = (f y)) -> x = y).
Axiom thm_SURJECTIVE_MAP : forall {A B : Type'}, forall f : A -> B, (forall m : list B, exists l : list A, (@List.map A B f l) = m) = (forall y : B, exists x : A, (f x) = y).
Axiom thm_MAP_ID : forall {A : Type'}, forall l : list A, (@List.map A A (fun x : A => x) l) = l.
Axiom thm_MAP_I : forall {A : Type'}, (@List.map A A (@I A)) = (@I (list A)).
Axiom thm_BUTLAST_CLAUSES : forall {A : Type'}, ((@List.removelast A (@nil A)) = (@nil A)) /\ ((forall a : A, (@List.removelast A (@cons A a (@nil A))) = (@nil A)) /\ (forall a : A, forall h : A, forall t : list A, (@List.removelast A (@cons A a (@cons A h t))) = (@cons A a (@List.removelast A (@cons A h t))))).
Axiom thm_BUTLAST_APPEND : forall {A : Type'}, forall l : list A, forall m : list A, (@List.removelast A (@List.app A l m)) = (@COND (list A) (m = (@nil A)) (@List.removelast A l) (@List.app A l (@List.removelast A m))).
Axiom thm_APPEND_BUTLAST_LAST : forall {A : Type'}, forall l : list A, (~ (l = (@nil A))) -> (@List.app A (@List.removelast A l) (@cons A (@LAST A l) (@nil A))) = l.
Axiom thm_LAST_APPEND : forall {A : Type'}, forall p : list A, forall q : list A, (@LAST A (@List.app A p q)) = (@COND A (q = (@nil A)) (@LAST A p) (@LAST A q)).
Axiom thm_LENGTH_TL : forall {A : Type'}, forall l : list A, (~ (l = (@nil A))) -> (@List.length A (@tl A l)) = (Nat.sub (@List.length A l) (NUMERAL (BIT1 0))).
Axiom thm_LAST_REVERSE : forall {A : Type'}, forall l : list A, (~ (l = (@nil A))) -> (@LAST A (@List.rev A l)) = (@hd A l).
Axiom thm_HD_REVERSE : forall {A : Type'}, forall l : list A, (~ (l = (@nil A))) -> (@hd A (@List.rev A l)) = (@LAST A l).
Axiom thm_EL_APPEND : forall {A : Type'}, forall k : nat, forall l : list A, forall m : list A, (@EL A k (@List.app A l m)) = (@COND A (Peano.lt k (@List.length A l)) (@EL A k l) (@EL A (Nat.sub k (@List.length A l)) m)).
Axiom thm_EL_TL : forall {A : Type'} (l : list A), forall n : nat, (@EL A n (@tl A l)) = (@EL A (Nat.add n (NUMERAL (BIT1 0))) l).
Axiom thm_EL_CONS : forall {A : Type'}, forall n : nat, forall h : A, forall t : list A, (@EL A n (@cons A h t)) = (@COND A (n = (NUMERAL 0)) h (@EL A (Nat.sub n (NUMERAL (BIT1 0))) t)).
Axiom thm_LAST_EL : forall {A : Type'}, forall l : list A, (~ (l = (@nil A))) -> (@LAST A l) = (@EL A (Nat.sub (@List.length A l) (NUMERAL (BIT1 0))) l).
Axiom thm_HD_APPEND : forall {A : Type'}, forall l : list A, forall m : list A, (@hd A (@List.app A l m)) = (@COND A (l = (@nil A)) (@hd A m) (@hd A l)).
Axiom thm_CONS_HD_TL : forall {A : Type'}, forall l : list A, (~ (l = (@nil A))) -> l = (@cons A (@hd A l) (@tl A l)).
Axiom thm_EL_MAP : forall {A B : Type'}, forall f : A -> B, forall n : nat, forall l : list A, (Peano.lt n (@List.length A l)) -> (@EL B n (@List.map A B f l)) = (f (@EL A n l)).
Axiom thm_MAP_REVERSE : forall {A B : Type'}, forall f : A -> B, forall l : list A, (@List.rev B (@List.map A B f l)) = (@List.map A B f (@List.rev A l)).
Axiom thm_ALL_FILTER : forall {A : Type'}, forall P : A -> Prop, forall Q : A -> Prop, forall l : list A, (@List.Forall A P (@FILTER A Q l)) = (@List.Forall A (fun x : A => (Q x) -> P x) l).
Axiom thm_APPEND_SING : forall {A : Type'}, forall h : A, forall t : list A, (@List.app A (@cons A h (@nil A)) t) = (@cons A h t).
Axiom thm_MEM_APPEND_DECOMPOSE_LEFT : forall {A : Type'}, forall x : A, forall l : list A, (@List.In A x l) = (exists l1 : list A, exists l2 : list A, (~ (@List.In A x l1)) /\ (l = (@List.app A l1 (@cons A x l2)))).
Axiom thm_MEM_APPEND_DECOMPOSE : forall {A : Type'}, forall x : A, forall l : list A, (@List.In A x l) = (exists l1 : list A, exists l2 : list A, l = (@List.app A l1 (@cons A x l2))).
Axiom thm_PAIRWISE_APPEND : forall {A : Type'}, forall R' : A -> A -> Prop, forall l : list A, forall m : list A, (@List.ForallOrdPairs A R' (@List.app A l m)) = ((@List.ForallOrdPairs A R' l) /\ ((@List.ForallOrdPairs A R' m) /\ (forall x : A, forall y : A, ((@List.In A x l) /\ (@List.In A y m)) -> R' x y))).
Axiom thm_PAIRWISE_MAP : forall {A B : Type'}, forall R' : B -> B -> Prop, forall f : A -> B, forall l : list A, (@List.ForallOrdPairs B R' (@List.map A B f l)) = (@List.ForallOrdPairs A (fun x : A => fun y : A => R' (f x) (f y)) l).
Axiom thm_PAIRWISE_IMPLIES : forall {A : Type'}, forall R' : A -> A -> Prop, forall R'' : A -> A -> Prop, forall l : list A, ((@List.ForallOrdPairs A R' l) /\ (forall x : A, forall y : A, ((@List.In A x l) /\ ((@List.In A y l) /\ (R' x y))) -> R'' x y)) -> @List.ForallOrdPairs A R'' l.
Axiom thm_PAIRWISE_TRANSITIVE : forall {A : Type'}, forall R' : A -> A -> Prop, forall x : A, forall y : A, forall l : list A, (forall x' : A, forall y' : A, forall z : A, ((R' x' y') /\ (R' y' z)) -> R' x' z) -> (@List.ForallOrdPairs A R' (@cons A x (@cons A y l))) = ((R' x y) /\ (@List.ForallOrdPairs A R' (@cons A y l))).
Axiom thm_LENGTH_LIST_OF_SEQ : forall {A : Type'}, forall s : nat -> A, forall n : nat, (@List.length A (@list_of_seq A s n)) = n.
Axiom thm_EL_LIST_OF_SEQ : forall {A : Type'}, forall s : nat -> A, forall m : nat, forall n : nat, (Peano.lt m n) -> (@EL A m (@list_of_seq A s n)) = (s m).
Axiom thm_LIST_OF_SEQ_EQ_NIL : forall {A : Type'}, forall s : nat -> A, forall n : nat, ((@list_of_seq A s n) = (@nil A)) = (n = (NUMERAL 0)).
Axiom thm_MONO_ALL : forall {A : Type'} (P : A -> Prop) (Q : A -> Prop) (l : list A), (forall x : A, (P x) -> Q x) -> (@List.Forall A P l) -> @List.Forall A Q l.
Axiom thm_MONO_ALL2 : forall {A B : Type'} (P : A -> B -> Prop) (Q : A -> B -> Prop) (l : list A) (l' : list B), (forall x : A, forall y : B, (P x y) -> Q x y) -> (@ALL2 A B P l l') -> @ALL2 A B Q l l'.
Axiom thm_char_INDUCT : forall P : Ascii.ascii -> Prop, (forall a0 : Prop, forall a1 : Prop, forall a2 : Prop, forall a3 : Prop, forall a4 : Prop, forall a5 : Prop, forall a6 : Prop, forall a7 : Prop, P (ASCII a0 a1 a2 a3 a4 a5 a6 a7)) -> forall x : Ascii.ascii, P x.
Axiom thm_char_RECURSION : forall {Z : Type'}, forall f : Prop -> Prop -> Prop -> Prop -> Prop -> Prop -> Prop -> Prop -> Z, exists fn : Ascii.ascii -> Z, forall a0 : Prop, forall a1 : Prop, forall a2 : Prop, forall a3 : Prop, forall a4 : Prop, forall a5 : Prop, forall a6 : Prop, forall a7 : Prop, (fn (ASCII a0 a1 a2 a3 a4 a5 a6 a7)) = (f a0 a1 a2 a3 a4 a5 a6 a7).
Axiom thm_dist : forall n : nat, forall m : nat, (dist (@pair nat nat m n)) = (Nat.add (Nat.sub m n) (Nat.sub n m)).
Axiom thm_DIST_REFL : forall n : nat, (dist (@pair nat nat n n)) = (NUMERAL 0).
Axiom thm_DIST_LZERO : forall n : nat, (dist (@pair nat nat (NUMERAL 0) n)) = n.
Axiom thm_DIST_RZERO : forall n : nat, (dist (@pair nat nat n (NUMERAL 0))) = n.
Axiom thm_DIST_SYM : forall m : nat, forall n : nat, (dist (@pair nat nat m n)) = (dist (@pair nat nat n m)).
Axiom thm_DIST_LADD : forall m : nat, forall p : nat, forall n : nat, (dist (@pair nat nat (Nat.add m n) (Nat.add m p))) = (dist (@pair nat nat n p)).
Axiom thm_DIST_RADD : forall m : nat, forall p : nat, forall n : nat, (dist (@pair nat nat (Nat.add m p) (Nat.add n p))) = (dist (@pair nat nat m n)).
Axiom thm_DIST_LADD_0 : forall m : nat, forall n : nat, (dist (@pair nat nat (Nat.add m n) m)) = n.
Axiom thm_DIST_RADD_0 : forall m : nat, forall n : nat, (dist (@pair nat nat m (Nat.add m n))) = n.
Axiom thm_DIST_LMUL : forall m : nat, forall n : nat, forall p : nat, (Nat.mul m (dist (@pair nat nat n p))) = (dist (@pair nat nat (Nat.mul m n) (Nat.mul m p))).
Axiom thm_DIST_RMUL : forall m : nat, forall n : nat, forall p : nat, (Nat.mul (dist (@pair nat nat m n)) p) = (dist (@pair nat nat (Nat.mul m p) (Nat.mul n p))).
Axiom thm_DIST_EQ_0 : forall m : nat, forall n : nat, ((dist (@pair nat nat m n)) = (NUMERAL 0)) = (m = n).
Axiom thm_DIST_ELIM_THM : forall (y : nat) (x : nat) (P : nat -> Prop), (P (dist (@pair nat nat x y))) = (forall d : nat, ((x = (Nat.add y d)) -> P d) /\ ((y = (Nat.add x d)) -> P d)).
Axiom thm_DIST_TRIANGLE_LE : forall m : nat, forall n : nat, forall p : nat, forall q : nat, (Peano.le (Nat.add (dist (@pair nat nat m n)) (dist (@pair nat nat n p))) q) -> Peano.le (dist (@pair nat nat m p)) q.
Axiom thm_DIST_TRIANGLES_LE : forall m : nat, forall n : nat, forall p : nat, forall q : nat, forall r : nat, forall s : nat, ((Peano.le (dist (@pair nat nat m n)) r) /\ (Peano.le (dist (@pair nat nat p q)) s)) -> Peano.le (dist (@pair nat nat m p)) (Nat.add (dist (@pair nat nat n q)) (Nat.add r s)).
Axiom thm_BOUNDS_LINEAR : forall A : nat, forall B : nat, forall C : nat, (forall n : nat, Peano.le (Nat.mul A n) (Nat.add (Nat.mul B n) C)) = (Peano.le A B).
Axiom thm_BOUNDS_LINEAR_0 : forall A : nat, forall B : nat, (forall n : nat, Peano.le (Nat.mul A n) B) = (A = (NUMERAL 0)).
Axiom thm_BOUNDS_DIVIDED : forall P : nat -> nat, (exists B : nat, forall n : nat, Peano.le (P n) B) = (exists A : nat, exists B : nat, forall n : nat, Peano.le (Nat.mul n (P n)) (Nat.add (Nat.mul A n) B)).
Axiom thm_BOUNDS_NOTZERO : forall P : nat -> nat -> nat, forall A : nat, forall B : nat, (((P (NUMERAL 0) (NUMERAL 0)) = (NUMERAL 0)) /\ (forall m : nat, forall n : nat, Peano.le (P m n) (Nat.add (Nat.mul A (Nat.add m n)) B))) -> exists B' : nat, forall m : nat, forall n : nat, Peano.le (P m n) (Nat.mul B' (Nat.add m n)).
Axiom thm_BOUNDS_IGNORE : forall P : nat -> nat, forall Q : nat -> nat, (exists B : nat, forall i : nat, Peano.le (P i) (Nat.add (Q i) B)) = (exists B : nat, exists N : nat, forall i : nat, (Peano.le N i) -> Peano.le (P i) (Nat.add (Q i) B)).
Axiom thm_is_nadd : forall x : nat -> nat, (is_nadd x) = (exists B : nat, forall m : nat, forall n : nat, Peano.le (dist (@pair nat nat (Nat.mul m (x n)) (Nat.mul n (x m)))) (Nat.mul B (Nat.add m n))).
Axiom thm_is_nadd_0 : is_nadd (fun n : nat => NUMERAL 0).
Axiom thm_NADD_CAUCHY : forall x : nadd, exists B : nat, forall m : nat, forall n : nat, Peano.le (dist (@pair nat nat (Nat.mul m (dest_nadd x n)) (Nat.mul n (dest_nadd x m)))) (Nat.mul B (Nat.add m n)).
Axiom thm_NADD_BOUND : forall x : nadd, exists A : nat, exists B : nat, forall n : nat, Peano.le (dest_nadd x n) (Nat.add (Nat.mul A n) B).
Axiom thm_NADD_MULTIPLICATIVE : forall x : nadd, exists B : nat, forall m : nat, forall n : nat, Peano.le (dist (@pair nat nat (dest_nadd x (Nat.mul m n)) (Nat.mul m (dest_nadd x n)))) (Nat.add (Nat.mul B m) B).
Axiom thm_NADD_ADDITIVE : forall x : nadd, exists B : nat, forall m : nat, forall n : nat, Peano.le (dist (@pair nat nat (dest_nadd x (Nat.add m n)) (Nat.add (dest_nadd x m) (dest_nadd x n)))) B.
Axiom thm_NADD_SUC : forall x : nadd, exists B : nat, forall n : nat, Peano.le (dist (@pair nat nat (dest_nadd x (S n)) (dest_nadd x n))) B.
Axiom thm_NADD_DIST_LEMMA : forall x : nadd, exists B : nat, forall m : nat, forall n : nat, Peano.le (dist (@pair nat nat (dest_nadd x (Nat.add m n)) (dest_nadd x m))) (Nat.mul B n).
Axiom thm_NADD_DIST : forall x : nadd, exists B : nat, forall m : nat, forall n : nat, Peano.le (dist (@pair nat nat (dest_nadd x m) (dest_nadd x n))) (Nat.mul B (dist (@pair nat nat m n))).
Axiom thm_NADD_ALTMUL : forall x : nadd, forall y : nadd, exists A : nat, exists B : nat, forall n : nat, Peano.le (dist (@pair nat nat (Nat.mul n (dest_nadd x (dest_nadd y n))) (Nat.mul (dest_nadd x n) (dest_nadd y n)))) (Nat.add (Nat.mul A n) B).
Axiom thm_nadd_eq : forall x : nadd, forall y : nadd, (nadd_eq x y) = (exists B : nat, forall n : nat, Peano.le (dist (@pair nat nat (dest_nadd x n) (dest_nadd y n))) B).
Axiom thm_NADD_EQ_REFL : forall x : nadd, nadd_eq x x.
Axiom thm_NADD_EQ_SYM : forall x : nadd, forall y : nadd, (nadd_eq x y) = (nadd_eq y x).
Axiom thm_NADD_EQ_TRANS : forall x : nadd, forall y : nadd, forall z : nadd, ((nadd_eq x y) /\ (nadd_eq y z)) -> nadd_eq x z.
Axiom thm_nadd_of_num : forall k : nat, (nadd_of_num k) = (mk_nadd (fun n : nat => Nat.mul k n)).
Axiom thm_NADD_OF_NUM : forall k : nat, (dest_nadd (nadd_of_num k)) = (fun n : nat => Nat.mul k n).
Axiom thm_NADD_OF_NUM_WELLDEF : forall m : nat, forall n : nat, (m = n) -> nadd_eq (nadd_of_num m) (nadd_of_num n).
Axiom thm_NADD_OF_NUM_EQ : forall m : nat, forall n : nat, (nadd_eq (nadd_of_num m) (nadd_of_num n)) = (m = n).
Axiom thm_nadd_le : forall x : nadd, forall y : nadd, (nadd_le x y) = (exists B : nat, forall n : nat, Peano.le (dest_nadd x n) (Nat.add (dest_nadd y n) B)).
Axiom thm_NADD_LE_WELLDEF_LEMMA : forall x : nadd, forall x' : nadd, forall y : nadd, forall y' : nadd, ((nadd_eq x x') /\ ((nadd_eq y y') /\ (nadd_le x y))) -> nadd_le x' y'.
Axiom thm_NADD_LE_WELLDEF : forall x : nadd, forall x' : nadd, forall y : nadd, forall y' : nadd, ((nadd_eq x x') /\ (nadd_eq y y')) -> (nadd_le x y) = (nadd_le x' y').
Axiom thm_NADD_LE_REFL : forall x : nadd, nadd_le x x.
Axiom thm_NADD_LE_TRANS : forall x : nadd, forall y : nadd, forall z : nadd, ((nadd_le x y) /\ (nadd_le y z)) -> nadd_le x z.
Axiom thm_NADD_LE_ANTISYM : forall x : nadd, forall y : nadd, ((nadd_le x y) /\ (nadd_le y x)) = (nadd_eq x y).
Axiom thm_NADD_LE_TOTAL_LEMMA : forall x : nadd, forall y : nadd, (~ (nadd_le x y)) -> forall B : nat, exists n : nat, (~ (n = (NUMERAL 0))) /\ (Peano.lt (Nat.add (dest_nadd y n) B) (dest_nadd x n)).
Axiom thm_NADD_LE_TOTAL : forall x : nadd, forall y : nadd, (nadd_le x y) \/ (nadd_le y x).
Axiom thm_NADD_ARCH : forall x : nadd, exists n : nat, nadd_le x (nadd_of_num n).
Axiom thm_NADD_OF_NUM_LE : forall m : nat, forall n : nat, (nadd_le (nadd_of_num m) (nadd_of_num n)) = (Peano.le m n).
Axiom thm_nadd_add : forall x : nadd, forall y : nadd, (nadd_add x y) = (mk_nadd (fun n : nat => Nat.add (dest_nadd x n) (dest_nadd y n))).
Axiom thm_NADD_ADD : forall x : nadd, forall y : nadd, (dest_nadd (nadd_add x y)) = (fun n : nat => Nat.add (dest_nadd x n) (dest_nadd y n)).
Axiom thm_NADD_ADD_WELLDEF : forall x : nadd, forall x' : nadd, forall y : nadd, forall y' : nadd, ((nadd_eq x x') /\ (nadd_eq y y')) -> nadd_eq (nadd_add x y) (nadd_add x' y').
Axiom thm_NADD_ADD_SYM : forall x : nadd, forall y : nadd, nadd_eq (nadd_add x y) (nadd_add y x).
Axiom thm_NADD_ADD_ASSOC : forall x : nadd, forall y : nadd, forall z : nadd, nadd_eq (nadd_add x (nadd_add y z)) (nadd_add (nadd_add x y) z).
Axiom thm_NADD_ADD_LID : forall x : nadd, nadd_eq (nadd_add (nadd_of_num (NUMERAL 0)) x) x.
Axiom thm_NADD_ADD_LCANCEL : forall x : nadd, forall y : nadd, forall z : nadd, (nadd_eq (nadd_add x y) (nadd_add x z)) -> nadd_eq y z.
Axiom thm_NADD_LE_ADD : forall x : nadd, forall y : nadd, nadd_le x (nadd_add x y).
Axiom thm_NADD_LE_EXISTS : forall x : nadd, forall y : nadd, (nadd_le x y) -> exists d : nadd, nadd_eq y (nadd_add x d).
Axiom thm_NADD_OF_NUM_ADD : forall m : nat, forall n : nat, nadd_eq (nadd_add (nadd_of_num m) (nadd_of_num n)) (nadd_of_num (Nat.add m n)).
Axiom thm_nadd_mul : forall x : nadd, forall y : nadd, (nadd_mul x y) = (mk_nadd (fun n : nat => dest_nadd x (dest_nadd y n))).
Axiom thm_NADD_MUL : forall x : nadd, forall y : nadd, (dest_nadd (nadd_mul x y)) = (fun n : nat => dest_nadd x (dest_nadd y n)).
Axiom thm_NADD_MUL_SYM : forall x : nadd, forall y : nadd, nadd_eq (nadd_mul x y) (nadd_mul y x).
Axiom thm_NADD_MUL_ASSOC : forall x : nadd, forall y : nadd, forall z : nadd, nadd_eq (nadd_mul x (nadd_mul y z)) (nadd_mul (nadd_mul x y) z).
Axiom thm_NADD_MUL_LID : forall x : nadd, nadd_eq (nadd_mul (nadd_of_num (NUMERAL (BIT1 0))) x) x.
Axiom thm_NADD_LDISTRIB : forall x : nadd, forall y : nadd, forall z : nadd, nadd_eq (nadd_mul x (nadd_add y z)) (nadd_add (nadd_mul x y) (nadd_mul x z)).
Axiom thm_NADD_MUL_WELLDEF_LEMMA : forall x : nadd, forall y : nadd, forall y' : nadd, (nadd_eq y y') -> nadd_eq (nadd_mul x y) (nadd_mul x y').
Axiom thm_NADD_MUL_WELLDEF : forall x : nadd, forall x' : nadd, forall y : nadd, forall y' : nadd, ((nadd_eq x x') /\ (nadd_eq y y')) -> nadd_eq (nadd_mul x y) (nadd_mul x' y').
Axiom thm_NADD_OF_NUM_MUL : forall m : nat, forall n : nat, nadd_eq (nadd_mul (nadd_of_num m) (nadd_of_num n)) (nadd_of_num (Nat.mul m n)).
Axiom thm_NADD_LE_0 : forall x : nadd, nadd_le (nadd_of_num (NUMERAL 0)) x.
Axiom thm_NADD_EQ_IMP_LE : forall x : nadd, forall y : nadd, (nadd_eq x y) -> nadd_le x y.
Axiom thm_NADD_LE_LMUL : forall x : nadd, forall y : nadd, forall z : nadd, (nadd_le y z) -> nadd_le (nadd_mul x y) (nadd_mul x z).
Axiom thm_NADD_LE_RMUL : forall x : nadd, forall y : nadd, forall z : nadd, (nadd_le x y) -> nadd_le (nadd_mul x z) (nadd_mul y z).
Axiom thm_NADD_LE_RADD : forall x : nadd, forall y : nadd, forall z : nadd, (nadd_le (nadd_add x z) (nadd_add y z)) = (nadd_le x y).
Axiom thm_NADD_LE_LADD : forall x : nadd, forall y : nadd, forall z : nadd, (nadd_le (nadd_add x y) (nadd_add x z)) = (nadd_le y z).
Axiom thm_NADD_RDISTRIB : forall x : nadd, forall y : nadd, forall z : nadd, nadd_eq (nadd_mul (nadd_add x y) z) (nadd_add (nadd_mul x z) (nadd_mul y z)).
Axiom thm_NADD_ARCH_MULT : forall x : nadd, forall k : nat, (~ (nadd_eq x (nadd_of_num (NUMERAL 0)))) -> exists N : nat, nadd_le (nadd_of_num k) (nadd_mul (nadd_of_num N) x).
Axiom thm_NADD_ARCH_ZERO : forall x : nadd, forall k : nadd, (forall n : nat, nadd_le (nadd_mul (nadd_of_num n) x) k) -> nadd_eq x (nadd_of_num (NUMERAL 0)).
Axiom thm_NADD_ARCH_LEMMA : forall x : nadd, forall y : nadd, forall z : nadd, (forall n : nat, nadd_le (nadd_mul (nadd_of_num n) x) (nadd_add (nadd_mul (nadd_of_num n) y) z)) -> nadd_le x y.
Axiom thm_NADD_COMPLETE : forall P : nadd -> Prop, ((exists x : nadd, P x) /\ (exists M : nadd, forall x : nadd, (P x) -> nadd_le x M)) -> exists M : nadd, (forall x : nadd, (P x) -> nadd_le x M) /\ (forall M' : nadd, (forall x : nadd, (P x) -> nadd_le x M') -> nadd_le M M').
Axiom thm_NADD_UBOUND : forall x : nadd, exists B : nat, exists N : nat, forall n : nat, (Peano.le N n) -> Peano.le (dest_nadd x n) (Nat.mul B n).
Axiom thm_NADD_NONZERO : forall x : nadd, (~ (nadd_eq x (nadd_of_num (NUMERAL 0)))) -> exists N : nat, forall n : nat, (Peano.le N n) -> ~ ((dest_nadd x n) = (NUMERAL 0)).
Axiom thm_NADD_LBOUND : forall x : nadd, (~ (nadd_eq x (nadd_of_num (NUMERAL 0)))) -> exists A : nat, exists N : nat, forall n : nat, (Peano.le N n) -> Peano.le n (Nat.mul A (dest_nadd x n)).
Axiom thm_nadd_rinv : forall x : nadd, (nadd_rinv x) = (fun n : nat => Nat.div (Nat.mul n n) (dest_nadd x n)).
Axiom thm_NADD_MUL_LINV_LEMMA0 : forall x : nadd, (~ (nadd_eq x (nadd_of_num (NUMERAL 0)))) -> exists A : nat, exists B : nat, forall n : nat, Peano.le (nadd_rinv x n) (Nat.add (Nat.mul A n) B).
Axiom thm_NADD_MUL_LINV_LEMMA1 : forall x : nadd, forall n : nat, (~ ((dest_nadd x n) = (NUMERAL 0))) -> Peano.le (dist (@pair nat nat (Nat.mul (dest_nadd x n) (nadd_rinv x n)) (Nat.mul n n))) (dest_nadd x n).
Axiom thm_NADD_MUL_LINV_LEMMA2 : forall x : nadd, (~ (nadd_eq x (nadd_of_num (NUMERAL 0)))) -> exists N : nat, forall n : nat, (Peano.le N n) -> Peano.le (dist (@pair nat nat (Nat.mul (dest_nadd x n) (nadd_rinv x n)) (Nat.mul n n))) (dest_nadd x n).
Axiom thm_NADD_MUL_LINV_LEMMA3 : forall x : nadd, (~ (nadd_eq x (nadd_of_num (NUMERAL 0)))) -> exists N : nat, forall m : nat, forall n : nat, (Peano.le N n) -> Peano.le (dist (@pair nat nat (Nat.mul m (Nat.mul (dest_nadd x m) (Nat.mul (dest_nadd x n) (nadd_rinv x n)))) (Nat.mul m (Nat.mul (dest_nadd x m) (Nat.mul n n))))) (Nat.mul m (Nat.mul (dest_nadd x m) (dest_nadd x n))).
Axiom thm_NADD_MUL_LINV_LEMMA4 : forall x : nadd, (~ (nadd_eq x (nadd_of_num (NUMERAL 0)))) -> exists N : nat, forall m : nat, forall n : nat, ((Peano.le N m) /\ (Peano.le N n)) -> Peano.le (Nat.mul (Nat.mul (dest_nadd x m) (dest_nadd x n)) (dist (@pair nat nat (Nat.mul m (nadd_rinv x n)) (Nat.mul n (nadd_rinv x m))))) (Nat.add (Nat.mul (Nat.mul m n) (dist (@pair nat nat (Nat.mul m (dest_nadd x n)) (Nat.mul n (dest_nadd x m))))) (Nat.mul (Nat.mul (dest_nadd x m) (dest_nadd x n)) (Nat.add m n))).
Axiom thm_NADD_MUL_LINV_LEMMA5 : forall x : nadd, (~ (nadd_eq x (nadd_of_num (NUMERAL 0)))) -> exists B : nat, exists N : nat, forall m : nat, forall n : nat, ((Peano.le N m) /\ (Peano.le N n)) -> Peano.le (Nat.mul (Nat.mul (dest_nadd x m) (dest_nadd x n)) (dist (@pair nat nat (Nat.mul m (nadd_rinv x n)) (Nat.mul n (nadd_rinv x m))))) (Nat.mul B (Nat.mul (Nat.mul m n) (Nat.add m n))).
Axiom thm_NADD_MUL_LINV_LEMMA6 : forall x : nadd, (~ (nadd_eq x (nadd_of_num (NUMERAL 0)))) -> exists B : nat, exists N : nat, forall m : nat, forall n : nat, ((Peano.le N m) /\ (Peano.le N n)) -> Peano.le (Nat.mul (Nat.mul m n) (dist (@pair nat nat (Nat.mul m (nadd_rinv x n)) (Nat.mul n (nadd_rinv x m))))) (Nat.mul B (Nat.mul (Nat.mul m n) (Nat.add m n))).
Axiom thm_NADD_MUL_LINV_LEMMA7 : forall x : nadd, (~ (nadd_eq x (nadd_of_num (NUMERAL 0)))) -> exists B : nat, exists N : nat, forall m : nat, forall n : nat, ((Peano.le N m) /\ (Peano.le N n)) -> Peano.le (dist (@pair nat nat (Nat.mul m (nadd_rinv x n)) (Nat.mul n (nadd_rinv x m)))) (Nat.mul B (Nat.add m n)).
Axiom thm_NADD_MUL_LINV_LEMMA7a : forall x : nadd, (~ (nadd_eq x (nadd_of_num (NUMERAL 0)))) -> forall N : nat, exists A : nat, exists B : nat, forall m : nat, forall n : nat, (Peano.le m N) -> Peano.le (dist (@pair nat nat (Nat.mul m (nadd_rinv x n)) (Nat.mul n (nadd_rinv x m)))) (Nat.add (Nat.mul A n) B).
Axiom thm_NADD_MUL_LINV_LEMMA8 : forall x : nadd, (~ (nadd_eq x (nadd_of_num (NUMERAL 0)))) -> exists B : nat, forall m : nat, forall n : nat, Peano.le (dist (@pair nat nat (Nat.mul m (nadd_rinv x n)) (Nat.mul n (nadd_rinv x m)))) (Nat.mul B (Nat.add m n)).
Axiom thm_nadd_inv : forall x : nadd, (nadd_inv x) = (@COND nadd (nadd_eq x (nadd_of_num (NUMERAL 0))) (nadd_of_num (NUMERAL 0)) (mk_nadd (nadd_rinv x))).
Axiom thm_NADD_INV : forall x : nadd, (dest_nadd (nadd_inv x)) = (@COND (nat -> nat) (nadd_eq x (nadd_of_num (NUMERAL 0))) (fun n : nat => NUMERAL 0) (nadd_rinv x)).
Axiom thm_NADD_MUL_LINV : forall x : nadd, (~ (nadd_eq x (nadd_of_num (NUMERAL 0)))) -> nadd_eq (nadd_mul (nadd_inv x) x) (nadd_of_num (NUMERAL (BIT1 0))).
Axiom thm_NADD_INV_0 : nadd_eq (nadd_inv (nadd_of_num (NUMERAL 0))) (nadd_of_num (NUMERAL 0)).
Axiom thm_NADD_INV_WELLDEF : forall x : nadd, forall y : nadd, (nadd_eq x y) -> nadd_eq (nadd_inv x) (nadd_inv y).
Axiom thm_HREAL_OF_NUM_EQ : forall m : nat, forall n : nat, ((hreal_of_num m) = (hreal_of_num n)) = (m = n).
Axiom thm_HREAL_OF_NUM_LE : forall m : nat, forall n : nat, (hreal_le (hreal_of_num m) (hreal_of_num n)) = (Peano.le m n).
Axiom thm_HREAL_OF_NUM_ADD : forall m : nat, forall n : nat, (hreal_add (hreal_of_num m) (hreal_of_num n)) = (hreal_of_num (Nat.add m n)).
Axiom thm_HREAL_OF_NUM_MUL : forall m : nat, forall n : nat, (hreal_mul (hreal_of_num m) (hreal_of_num n)) = (hreal_of_num (Nat.mul m n)).
Axiom thm_HREAL_LE_REFL : forall x : hreal, hreal_le x x.
Axiom thm_HREAL_LE_TRANS : forall x : hreal, forall y : hreal, forall z : hreal, ((hreal_le x y) /\ (hreal_le y z)) -> hreal_le x z.
Axiom thm_HREAL_LE_ANTISYM : forall x : hreal, forall y : hreal, ((hreal_le x y) /\ (hreal_le y x)) = (x = y).
Axiom thm_HREAL_LE_TOTAL : forall x : hreal, forall y : hreal, (hreal_le x y) \/ (hreal_le y x).
Axiom thm_HREAL_LE_ADD : forall x : hreal, forall y : hreal, hreal_le x (hreal_add x y).
Axiom thm_HREAL_LE_EXISTS : forall x : hreal, forall y : hreal, (hreal_le x y) -> exists d : hreal, y = (hreal_add x d).
Axiom thm_HREAL_ARCH : forall x : hreal, exists n : nat, hreal_le x (hreal_of_num n).
Axiom thm_HREAL_ADD_SYM : forall x : hreal, forall y : hreal, (hreal_add x y) = (hreal_add y x).
Axiom thm_HREAL_ADD_ASSOC : forall x : hreal, forall y : hreal, forall z : hreal, (hreal_add x (hreal_add y z)) = (hreal_add (hreal_add x y) z).
Axiom thm_HREAL_ADD_LID : forall x : hreal, (hreal_add (hreal_of_num (NUMERAL 0)) x) = x.
Axiom thm_HREAL_ADD_LCANCEL : forall x : hreal, forall y : hreal, forall z : hreal, ((hreal_add x y) = (hreal_add x z)) -> y = z.
Axiom thm_HREAL_MUL_SYM : forall x : hreal, forall y : hreal, (hreal_mul x y) = (hreal_mul y x).
Axiom thm_HREAL_MUL_ASSOC : forall x : hreal, forall y : hreal, forall z : hreal, (hreal_mul x (hreal_mul y z)) = (hreal_mul (hreal_mul x y) z).
Axiom thm_HREAL_MUL_LID : forall x : hreal, (hreal_mul (hreal_of_num (NUMERAL (BIT1 0))) x) = x.
Axiom thm_HREAL_ADD_LDISTRIB : forall x : hreal, forall y : hreal, forall z : hreal, (hreal_mul x (hreal_add y z)) = (hreal_add (hreal_mul x y) (hreal_mul x z)).
Axiom thm_HREAL_MUL_LINV : forall x : hreal, (~ (x = (hreal_of_num (NUMERAL 0)))) -> (hreal_mul (hreal_inv x) x) = (hreal_of_num (NUMERAL (BIT1 0))).
Axiom thm_HREAL_INV_0 : (hreal_inv (hreal_of_num (NUMERAL 0))) = (hreal_of_num (NUMERAL 0)).
Axiom thm_HREAL_LE_EXISTS_DEF : forall m : hreal, forall n : hreal, (hreal_le m n) = (exists d : hreal, n = (hreal_add m d)).
Axiom thm_HREAL_EQ_ADD_LCANCEL : forall m : hreal, forall n : hreal, forall p : hreal, ((hreal_add m n) = (hreal_add m p)) = (n = p).
Axiom thm_HREAL_EQ_ADD_RCANCEL : forall m : hreal, forall n : hreal, forall p : hreal, ((hreal_add m p) = (hreal_add n p)) = (m = n).
Axiom thm_HREAL_LE_ADD_LCANCEL : forall m : hreal, forall n : hreal, forall p : hreal, (hreal_le (hreal_add m n) (hreal_add m p)) = (hreal_le n p).
Axiom thm_HREAL_LE_ADD_RCANCEL : forall m : hreal, forall n : hreal, forall p : hreal, (hreal_le (hreal_add m p) (hreal_add n p)) = (hreal_le m n).
Axiom thm_HREAL_ADD_RID : forall n : hreal, (hreal_add n (hreal_of_num (NUMERAL 0))) = n.
Axiom thm_HREAL_ADD_RDISTRIB : forall m : hreal, forall n : hreal, forall p : hreal, (hreal_mul (hreal_add m n) p) = (hreal_add (hreal_mul m p) (hreal_mul n p)).
Axiom thm_HREAL_MUL_LZERO : forall m : hreal, (hreal_mul (hreal_of_num (NUMERAL 0)) m) = (hreal_of_num (NUMERAL 0)).
Axiom thm_HREAL_MUL_RZERO : forall m : hreal, (hreal_mul m (hreal_of_num (NUMERAL 0))) = (hreal_of_num (NUMERAL 0)).
Axiom thm_HREAL_ADD_AC : forall (n : hreal) (m : hreal) (p : hreal), ((hreal_add m n) = (hreal_add n m)) /\ (((hreal_add (hreal_add m n) p) = (hreal_add m (hreal_add n p))) /\ ((hreal_add m (hreal_add n p)) = (hreal_add n (hreal_add m p)))).
Axiom thm_HREAL_LE_ADD2 : forall a : hreal, forall b : hreal, forall c : hreal, forall d : hreal, ((hreal_le a b) /\ (hreal_le c d)) -> hreal_le (hreal_add a c) (hreal_add b d).
Axiom thm_HREAL_LE_MUL_RCANCEL_IMP : forall a : hreal, forall b : hreal, forall c : hreal, (hreal_le a b) -> hreal_le (hreal_mul a c) (hreal_mul b c).
Axiom thm_treal_of_num : forall n : nat, (treal_of_num n) = (@pair hreal hreal (hreal_of_num n) (hreal_of_num (NUMERAL 0))).
Axiom thm_treal_neg : forall y : hreal, forall x : hreal, (treal_neg (@pair hreal hreal x y)) = (@pair hreal hreal y x).
Axiom thm_treal_add : forall x1 : hreal, forall x2 : hreal, forall y1 : hreal, forall y2 : hreal, (treal_add (@pair hreal hreal x1 y1) (@pair hreal hreal x2 y2)) = (@pair hreal hreal (hreal_add x1 x2) (hreal_add y1 y2)).
Axiom thm_treal_mul : forall x1 : hreal, forall y2 : hreal, forall y1 : hreal, forall x2 : hreal, (treal_mul (@pair hreal hreal x1 y1) (@pair hreal hreal x2 y2)) = (@pair hreal hreal (hreal_add (hreal_mul x1 x2) (hreal_mul y1 y2)) (hreal_add (hreal_mul x1 y2) (hreal_mul y1 x2))).
Axiom thm_treal_le : forall x1 : hreal, forall y2 : hreal, forall x2 : hreal, forall y1 : hreal, (treal_le (@pair hreal hreal x1 y1) (@pair hreal hreal x2 y2)) = (hreal_le (hreal_add x1 y2) (hreal_add x2 y1)).
Axiom thm_treal_inv : forall y : hreal, forall x : hreal, (treal_inv (@pair hreal hreal x y)) = (@COND (prod hreal hreal) (x = y) (@pair hreal hreal (hreal_of_num (NUMERAL 0)) (hreal_of_num (NUMERAL 0))) (@COND (prod hreal hreal) (hreal_le y x) (@pair hreal hreal (hreal_inv (@ε hreal (fun d : hreal => x = (hreal_add y d)))) (hreal_of_num (NUMERAL 0))) (@pair hreal hreal (hreal_of_num (NUMERAL 0)) (hreal_inv (@ε hreal (fun d : hreal => y = (hreal_add x d))))))).
Axiom thm_treal_eq : forall x1 : hreal, forall y2 : hreal, forall x2 : hreal, forall y1 : hreal, (treal_eq (@pair hreal hreal x1 y1) (@pair hreal hreal x2 y2)) = ((hreal_add x1 y2) = (hreal_add x2 y1)).
Axiom thm_TREAL_EQ_REFL : forall x : prod hreal hreal, treal_eq x x.
Axiom thm_TREAL_EQ_SYM : forall x : prod hreal hreal, forall y : prod hreal hreal, (treal_eq x y) = (treal_eq y x).
Axiom thm_TREAL_EQ_TRANS : forall x : prod hreal hreal, forall y : prod hreal hreal, forall z : prod hreal hreal, ((treal_eq x y) /\ (treal_eq y z)) -> treal_eq x z.
Axiom thm_TREAL_EQ_AP : forall x : prod hreal hreal, forall y : prod hreal hreal, (x = y) -> treal_eq x y.
Axiom thm_TREAL_OF_NUM_EQ : forall m : nat, forall n : nat, (treal_eq (treal_of_num m) (treal_of_num n)) = (m = n).
Axiom thm_TREAL_OF_NUM_LE : forall m : nat, forall n : nat, (treal_le (treal_of_num m) (treal_of_num n)) = (Peano.le m n).
Axiom thm_TREAL_OF_NUM_ADD : forall m : nat, forall n : nat, treal_eq (treal_add (treal_of_num m) (treal_of_num n)) (treal_of_num (Nat.add m n)).
Axiom thm_TREAL_OF_NUM_MUL : forall m : nat, forall n : nat, treal_eq (treal_mul (treal_of_num m) (treal_of_num n)) (treal_of_num (Nat.mul m n)).
Axiom thm_TREAL_ADD_SYM_EQ : forall x : prod hreal hreal, forall y : prod hreal hreal, (treal_add x y) = (treal_add y x).
Axiom thm_TREAL_MUL_SYM_EQ : forall x : prod hreal hreal, forall y : prod hreal hreal, (treal_mul x y) = (treal_mul y x).
Axiom thm_TREAL_ADD_SYM : forall x : prod hreal hreal, forall y : prod hreal hreal, treal_eq (treal_add x y) (treal_add y x).
Axiom thm_TREAL_ADD_ASSOC : forall x : prod hreal hreal, forall y : prod hreal hreal, forall z : prod hreal hreal, treal_eq (treal_add x (treal_add y z)) (treal_add (treal_add x y) z).
Axiom thm_TREAL_ADD_LID : forall x : prod hreal hreal, treal_eq (treal_add (treal_of_num (NUMERAL 0)) x) x.
Axiom thm_TREAL_ADD_LINV : forall x : prod hreal hreal, treal_eq (treal_add (treal_neg x) x) (treal_of_num (NUMERAL 0)).
Axiom thm_TREAL_MUL_SYM : forall x : prod hreal hreal, forall y : prod hreal hreal, treal_eq (treal_mul x y) (treal_mul y x).
Axiom thm_TREAL_MUL_ASSOC : forall x : prod hreal hreal, forall y : prod hreal hreal, forall z : prod hreal hreal, treal_eq (treal_mul x (treal_mul y z)) (treal_mul (treal_mul x y) z).
Axiom thm_TREAL_MUL_LID : forall x : prod hreal hreal, treal_eq (treal_mul (treal_of_num (NUMERAL (BIT1 0))) x) x.
Axiom thm_TREAL_ADD_LDISTRIB : forall x : prod hreal hreal, forall y : prod hreal hreal, forall z : prod hreal hreal, treal_eq (treal_mul x (treal_add y z)) (treal_add (treal_mul x y) (treal_mul x z)).
Axiom thm_TREAL_LE_REFL : forall x : prod hreal hreal, treal_le x x.
Axiom thm_TREAL_LE_ANTISYM : forall x : prod hreal hreal, forall y : prod hreal hreal, ((treal_le x y) /\ (treal_le y x)) = (treal_eq x y).
Axiom thm_TREAL_LE_TRANS : forall x : prod hreal hreal, forall y : prod hreal hreal, forall z : prod hreal hreal, ((treal_le x y) /\ (treal_le y z)) -> treal_le x z.
Axiom thm_TREAL_LE_TOTAL : forall x : prod hreal hreal, forall y : prod hreal hreal, (treal_le x y) \/ (treal_le y x).
Axiom thm_TREAL_LE_LADD_IMP : forall x : prod hreal hreal, forall y : prod hreal hreal, forall z : prod hreal hreal, (treal_le y z) -> treal_le (treal_add x y) (treal_add x z).
Axiom thm_TREAL_LE_MUL : forall x : prod hreal hreal, forall y : prod hreal hreal, ((treal_le (treal_of_num (NUMERAL 0)) x) /\ (treal_le (treal_of_num (NUMERAL 0)) y)) -> treal_le (treal_of_num (NUMERAL 0)) (treal_mul x y).
Axiom thm_TREAL_INV_0 : treal_eq (treal_inv (treal_of_num (NUMERAL 0))) (treal_of_num (NUMERAL 0)).
Axiom thm_TREAL_MUL_LINV : forall x : prod hreal hreal, (~ (treal_eq x (treal_of_num (NUMERAL 0)))) -> treal_eq (treal_mul (treal_inv x) x) (treal_of_num (NUMERAL (BIT1 0))).
Axiom thm_TREAL_OF_NUM_WELLDEF : forall m : nat, forall n : nat, (m = n) -> treal_eq (treal_of_num m) (treal_of_num n).
Axiom thm_TREAL_NEG_WELLDEF : forall x1 : prod hreal hreal, forall x2 : prod hreal hreal, (treal_eq x1 x2) -> treal_eq (treal_neg x1) (treal_neg x2).
Axiom thm_TREAL_ADD_WELLDEFR : forall x1 : prod hreal hreal, forall x2 : prod hreal hreal, forall y : prod hreal hreal, (treal_eq x1 x2) -> treal_eq (treal_add x1 y) (treal_add x2 y).
Axiom thm_TREAL_ADD_WELLDEF : forall x1 : prod hreal hreal, forall x2 : prod hreal hreal, forall y1 : prod hreal hreal, forall y2 : prod hreal hreal, ((treal_eq x1 x2) /\ (treal_eq y1 y2)) -> treal_eq (treal_add x1 y1) (treal_add x2 y2).
Axiom thm_TREAL_MUL_WELLDEFR : forall x1 : prod hreal hreal, forall x2 : prod hreal hreal, forall y : prod hreal hreal, (treal_eq x1 x2) -> treal_eq (treal_mul x1 y) (treal_mul x2 y).
Axiom thm_TREAL_MUL_WELLDEF : forall x1 : prod hreal hreal, forall x2 : prod hreal hreal, forall y1 : prod hreal hreal, forall y2 : prod hreal hreal, ((treal_eq x1 x2) /\ (treal_eq y1 y2)) -> treal_eq (treal_mul x1 y1) (treal_mul x2 y2).
Axiom thm_TREAL_EQ_IMP_LE : forall x : prod hreal hreal, forall y : prod hreal hreal, (treal_eq x y) -> treal_le x y.
Axiom thm_TREAL_LE_WELLDEF : forall x1 : prod hreal hreal, forall x2 : prod hreal hreal, forall y1 : prod hreal hreal, forall y2 : prod hreal hreal, ((treal_eq x1 x2) /\ (treal_eq y1 y2)) -> (treal_le x1 y1) = (treal_le x2 y2).
Axiom thm_TREAL_INV_WELLDEF : forall x : prod hreal hreal, forall y : prod hreal hreal, (treal_eq x y) -> treal_eq (treal_inv x) (treal_inv y).
Axiom thm_REAL_ADD_SYM : forall x : R, forall y : R, (Rplus x y) = (Rplus y x).
Axiom thm_REAL_ADD_ASSOC : forall x : R, forall y : R, forall z : R, (Rplus x (Rplus y z)) = (Rplus (Rplus x y) z).
Axiom thm_REAL_ADD_LID : forall x : R, (Rplus (INR (NUMERAL 0)) x) = x.
Axiom thm_REAL_ADD_LINV : forall x : R, (Rplus (Ropp x) x) = (INR (NUMERAL 0)).
Axiom thm_REAL_MUL_SYM : forall x : R, forall y : R, (Rmult x y) = (Rmult y x).
Axiom thm_REAL_MUL_ASSOC : forall x : R, forall y : R, forall z : R, (Rmult x (Rmult y z)) = (Rmult (Rmult x y) z).
Axiom thm_REAL_MUL_LID : forall x : R, (Rmult (INR (NUMERAL (BIT1 0))) x) = x.
Axiom thm_REAL_ADD_LDISTRIB : forall x : R, forall y : R, forall z : R, (Rmult x (Rplus y z)) = (Rplus (Rmult x y) (Rmult x z)).
Axiom thm_REAL_LE_REFL : forall x : R, Rle x x.
Axiom thm_REAL_LE_ANTISYM : forall x : R, forall y : R, ((Rle x y) /\ (Rle y x)) = (x = y).
Axiom thm_REAL_LE_TRANS : forall x : R, forall y : R, forall z : R, ((Rle x y) /\ (Rle y z)) -> Rle x z.
Axiom thm_REAL_LE_TOTAL : forall x : R, forall y : R, (Rle x y) \/ (Rle y x).
Axiom thm_REAL_LE_LADD_IMP : forall x : R, forall y : R, forall z : R, (Rle y z) -> Rle (Rplus x y) (Rplus x z).
Axiom thm_REAL_LE_MUL : forall x : R, forall y : R, ((Rle (INR (NUMERAL 0)) x) /\ (Rle (INR (NUMERAL 0)) y)) -> Rle (INR (NUMERAL 0)) (Rmult x y).
Axiom thm_REAL_INV_0 : (Rinv (INR (NUMERAL 0))) = (INR (NUMERAL 0)).
Axiom thm_REAL_MUL_LINV : forall x : R, (~ (x = (INR (NUMERAL 0)))) -> (Rmult (Rinv x) x) = (INR (NUMERAL (BIT1 0))).
Axiom thm_REAL_OF_NUM_EQ : forall m : nat, forall n : nat, ((INR m) = (INR n)) = (m = n).
Axiom thm_REAL_OF_NUM_LE : forall m : nat, forall n : nat, (Rle (INR m) (INR n)) = (Peano.le m n).
Axiom thm_REAL_OF_NUM_ADD : forall m : nat, forall n : nat, (Rplus (INR m) (INR n)) = (INR (Nat.add m n)).
Axiom thm_REAL_OF_NUM_MUL : forall m : nat, forall n : nat, (Rmult (INR m) (INR n)) = (INR (Nat.mul m n)).
Axiom thm_real_sub : forall x : R, forall y : R, (Rminus x y) = (Rplus x (Ropp y)).
Axiom thm_real_lt : forall y : R, forall x : R, (Rlt x y) = (~ (Rle y x)).
Axiom thm_real_ge : forall y : R, forall x : R, (Rge x y) = (Rle y x).
Axiom thm_real_gt : forall y : R, forall x : R, (Rgt x y) = (Rlt y x).
Axiom thm_real_abs : forall x : R, (Rabs x) = (@COND R (Rle (INR (NUMERAL 0)) x) x (Ropp x)).
Axiom thm_real_pow : forall (x : R), ((Rpower_nat x (NUMERAL 0)) = (INR (NUMERAL (BIT1 0)))) /\ (forall n : nat, (Rpower_nat x (S n)) = (Rmult x (Rpower_nat x n))).
Axiom thm_real_div : forall x : R, forall y : R, (Rdiv x y) = (Rmult x (Rinv y)).
Axiom thm_real_max : forall n : R, forall m : R, (Rmax m n) = (@COND R (Rle m n) n m).
Axiom thm_real_min : forall m : R, forall n : R, (Rmin m n) = (@COND R (Rle m n) m n).
Axiom thm_REAL_HREAL_LEMMA1 : exists r : hreal -> R, (forall x : R, (Rle (INR (NUMERAL 0)) x) = (exists y : hreal, x = (r y))) /\ (forall y : hreal, forall z : hreal, (hreal_le y z) = (Rle (r y) (r z))).
Axiom thm_REAL_HREAL_LEMMA2 : exists h : R -> hreal, exists r : hreal -> R, (forall x : hreal, (h (r x)) = x) /\ ((forall x : R, (Rle (INR (NUMERAL 0)) x) -> (r (h x)) = x) /\ ((forall x : hreal, Rle (INR (NUMERAL 0)) (r x)) /\ (forall x : hreal, forall y : hreal, (hreal_le x y) = (Rle (r x) (r y))))).
Axiom thm_REAL_COMPLETE_SOMEPOS : forall P : R -> Prop, ((exists x : R, (P x) /\ (Rle (INR (NUMERAL 0)) x)) /\ (exists M : R, forall x : R, (P x) -> Rle x M)) -> exists M : R, (forall x : R, (P x) -> Rle x M) /\ (forall M' : R, (forall x : R, (P x) -> Rle x M') -> Rle M M').
Axiom thm_REAL_COMPLETE : forall P : R -> Prop, ((exists x : R, P x) /\ (exists M : R, forall x : R, (P x) -> Rle x M)) -> exists M : R, (forall x : R, (P x) -> Rle x M) /\ (forall M' : R, (forall x : R, (P x) -> Rle x M') -> Rle M M').
Axiom thm_REAL_ADD_AC : forall (n : R) (m : R) (p : R), ((Rplus m n) = (Rplus n m)) /\ (((Rplus (Rplus m n) p) = (Rplus m (Rplus n p))) /\ ((Rplus m (Rplus n p)) = (Rplus n (Rplus m p)))).
Axiom thm_REAL_ADD_RINV : forall x : R, (Rplus x (Ropp x)) = (INR (NUMERAL 0)).
Axiom thm_REAL_EQ_ADD_LCANCEL : forall x : R, forall y : R, forall z : R, ((Rplus x y) = (Rplus x z)) = (y = z).
Axiom thm_REAL_EQ_ADD_RCANCEL : forall x : R, forall y : R, forall z : R, ((Rplus x z) = (Rplus y z)) = (x = y).
Axiom thm_REAL_MUL_RZERO : forall x : R, (Rmult x (INR (NUMERAL 0))) = (INR (NUMERAL 0)).
Axiom thm_REAL_MUL_LZERO : forall x : R, (Rmult (INR (NUMERAL 0)) x) = (INR (NUMERAL 0)).
Axiom thm_REAL_NEG_NEG : forall x : R, (Ropp (Ropp x)) = x.
Axiom thm_REAL_MUL_RNEG : forall x : R, forall y : R, (Rmult x (Ropp y)) = (Ropp (Rmult x y)).
Axiom thm_REAL_MUL_LNEG : forall x : R, forall y : R, (Rmult (Ropp x) y) = (Ropp (Rmult x y)).
Axiom thm_REAL_NEG_ADD : forall x : R, forall y : R, (Ropp (Rplus x y)) = (Rplus (Ropp x) (Ropp y)).
Axiom thm_REAL_ADD_RID : forall x : R, (Rplus x (INR (NUMERAL 0))) = x.
Axiom thm_REAL_NEG_0 : (Ropp (INR (NUMERAL 0))) = (INR (NUMERAL 0)).
Axiom thm_REAL_LE_LNEG : forall x : R, forall y : R, (Rle (Ropp x) y) = (Rle (INR (NUMERAL 0)) (Rplus x y)).
Axiom thm_REAL_LE_NEG2 : forall x : R, forall y : R, (Rle (Ropp x) (Ropp y)) = (Rle y x).
Axiom thm_REAL_LE_RNEG : forall x : R, forall y : R, (Rle x (Ropp y)) = (Rle (Rplus x y) (INR (NUMERAL 0))).
Axiom thm_REAL_OF_NUM_POW : forall x : nat, forall n : nat, (Rpower_nat (INR x) n) = (INR (Nat.pow x n)).
Axiom thm_REAL_POW_NEG : forall x : R, forall n : nat, (Rpower_nat (Ropp x) n) = (@COND R (Coq.Arith.PeanoNat.Nat.Even n) (Rpower_nat x n) (Ropp (Rpower_nat x n))).
Axiom thm_REAL_ABS_NUM : forall n : nat, (Rabs (INR n)) = (INR n).
Axiom thm_REAL_ABS_NEG : forall x : R, (Rabs (Ropp x)) = (Rabs x).
Axiom thm_REAL_LTE_TOTAL : forall x : R, forall y : R, (Rlt x y) \/ (Rle y x).
Axiom thm_REAL_LET_TOTAL : forall x : R, forall y : R, (Rle x y) \/ (Rlt y x).
Axiom thm_REAL_LT_IMP_LE : forall x : R, forall y : R, (Rlt x y) -> Rle x y.
Axiom thm_REAL_LTE_TRANS : forall x : R, forall y : R, forall z : R, ((Rlt x y) /\ (Rle y z)) -> Rlt x z.
Axiom thm_REAL_LET_TRANS : forall x : R, forall y : R, forall z : R, ((Rle x y) /\ (Rlt y z)) -> Rlt x z.
Axiom thm_REAL_LT_TRANS : forall x : R, forall y : R, forall z : R, ((Rlt x y) /\ (Rlt y z)) -> Rlt x z.
Axiom thm_REAL_LE_ADD : forall x : R, forall y : R, ((Rle (INR (NUMERAL 0)) x) /\ (Rle (INR (NUMERAL 0)) y)) -> Rle (INR (NUMERAL 0)) (Rplus x y).
Axiom thm_REAL_LTE_ANTISYM : forall x : R, forall y : R, ~ ((Rlt x y) /\ (Rle y x)).
Axiom thm_REAL_SUB_LE : forall x : R, forall y : R, (Rle (INR (NUMERAL 0)) (Rminus x y)) = (Rle y x).
Axiom thm_REAL_NEG_SUB : forall x : R, forall y : R, (Ropp (Rminus x y)) = (Rminus y x).
Axiom thm_REAL_LE_LT : forall x : R, forall y : R, (Rle x y) = ((Rlt x y) \/ (x = y)).
Axiom thm_REAL_SUB_LT : forall x : R, forall y : R, (Rlt (INR (NUMERAL 0)) (Rminus x y)) = (Rlt y x).
Axiom thm_REAL_NOT_LT : forall x : R, forall y : R, (~ (Rlt x y)) = (Rle y x).
Axiom thm_REAL_SUB_0 : forall x : R, forall y : R, ((Rminus x y) = (INR (NUMERAL 0))) = (x = y).
Axiom thm_REAL_LT_LE : forall x : R, forall y : R, (Rlt x y) = ((Rle x y) /\ (~ (x = y))).
Axiom thm_REAL_LT_REFL : forall x : R, ~ (Rlt x x).
Axiom thm_REAL_LTE_ADD : forall x : R, forall y : R, ((Rlt (INR (NUMERAL 0)) x) /\ (Rle (INR (NUMERAL 0)) y)) -> Rlt (INR (NUMERAL 0)) (Rplus x y).
Axiom thm_REAL_LET_ADD : forall x : R, forall y : R, ((Rle (INR (NUMERAL 0)) x) /\ (Rlt (INR (NUMERAL 0)) y)) -> Rlt (INR (NUMERAL 0)) (Rplus x y).
Axiom thm_REAL_LT_ADD : forall x : R, forall y : R, ((Rlt (INR (NUMERAL 0)) x) /\ (Rlt (INR (NUMERAL 0)) y)) -> Rlt (INR (NUMERAL 0)) (Rplus x y).
Axiom thm_REAL_ENTIRE : forall x : R, forall y : R, ((Rmult x y) = (INR (NUMERAL 0))) = ((x = (INR (NUMERAL 0))) \/ (y = (INR (NUMERAL 0)))).
Axiom thm_REAL_LE_NEGTOTAL : forall x : R, (Rle (INR (NUMERAL 0)) x) \/ (Rle (INR (NUMERAL 0)) (Ropp x)).
Axiom thm_REAL_LE_SQUARE : forall x : R, Rle (INR (NUMERAL 0)) (Rmult x x).
Axiom thm_REAL_MUL_RID : forall x : R, (Rmult x (INR (NUMERAL (BIT1 0)))) = x.
Axiom thm_REAL_POW_2 : forall x : R, (Rpower_nat x (NUMERAL (BIT0 (BIT1 0)))) = (Rmult x x).
Axiom thm_REAL_POLY_CLAUSES : (forall x : R, forall y : R, forall z : R, (Rplus x (Rplus y z)) = (Rplus (Rplus x y) z)) /\ ((forall x : R, forall y : R, (Rplus x y) = (Rplus y x)) /\ ((forall x : R, (Rplus (INR (NUMERAL 0)) x) = x) /\ ((forall x : R, forall y : R, forall z : R, (Rmult x (Rmult y z)) = (Rmult (Rmult x y) z)) /\ ((forall x : R, forall y : R, (Rmult x y) = (Rmult y x)) /\ ((forall x : R, (Rmult (INR (NUMERAL (BIT1 0))) x) = x) /\ ((forall x : R, (Rmult (INR (NUMERAL 0)) x) = (INR (NUMERAL 0))) /\ ((forall x : R, forall y : R, forall z : R, (Rmult x (Rplus y z)) = (Rplus (Rmult x y) (Rmult x z))) /\ ((forall x : R, (Rpower_nat x (NUMERAL 0)) = (INR (NUMERAL (BIT1 0)))) /\ (forall x : R, forall n : nat, (Rpower_nat x (S n)) = (Rmult x (Rpower_nat x n))))))))))).
Axiom thm_REAL_POLY_NEG_CLAUSES : (forall x : R, (Ropp x) = (Rmult (Ropp (INR (NUMERAL (BIT1 0)))) x)) /\ (forall x : R, forall y : R, (Rminus x y) = (Rplus x (Rmult (Ropp (INR (NUMERAL (BIT1 0)))) y))).
Axiom thm_REAL_POS : forall n : nat, Rle (INR (NUMERAL 0)) (INR n).
Axiom thm_REAL_LT_NZ : forall n : nat, (~ ((INR n) = (INR (NUMERAL 0)))) = (Rlt (INR (NUMERAL 0)) (INR n)).
Axiom thm_REAL_POS_LT : forall n : nat, Rlt (INR (NUMERAL 0)) (INR (S n)).
Axiom thm_REAL_OF_NUM_LT : forall m : nat, forall n : nat, (Rlt (INR m) (INR n)) = (Peano.lt m n).
Axiom thm_REAL_OF_NUM_GE : forall m : nat, forall n : nat, (Rge (INR m) (INR n)) = (Peano.ge m n).
Axiom thm_REAL_OF_NUM_GT : forall m : nat, forall n : nat, (Rgt (INR m) (INR n)) = (Peano.gt m n).
Axiom thm_REAL_OF_NUM_MAX : forall m : nat, forall n : nat, (Rmax (INR m) (INR n)) = (INR (Nat.max m n)).
Axiom thm_REAL_OF_NUM_MIN : forall m : nat, forall n : nat, (Rmin (INR m) (INR n)) = (INR (Nat.min m n)).
Axiom thm_REAL_OF_NUM_SUC : forall n : nat, (Rplus (INR n) (INR (NUMERAL (BIT1 0)))) = (INR (S n)).
Axiom thm_REAL_OF_NUM_SUB : forall m : nat, forall n : nat, (Peano.le m n) -> (Rminus (INR n) (INR m)) = (INR (Nat.sub n m)).
Axiom thm_REAL_OF_NUM_SUB_CASES : forall m : nat, forall n : nat, (Rminus (INR m) (INR n)) = (@COND R (Peano.le n m) (INR (Nat.sub m n)) (Ropp (INR (Nat.sub n m)))).
Axiom thm_REAL_OF_NUM_CLAUSES : (forall m : nat, forall n : nat, ((INR m) = (INR n)) = (m = n)) /\ ((forall m : nat, forall n : nat, (Rge (INR m) (INR n)) = (Peano.ge m n)) /\ ((forall m : nat, forall n : nat, (Rgt (INR m) (INR n)) = (Peano.gt m n)) /\ ((forall m : nat, forall n : nat, (Rle (INR m) (INR n)) = (Peano.le m n)) /\ ((forall m : nat, forall n : nat, (Rlt (INR m) (INR n)) = (Peano.lt m n)) /\ ((forall m : nat, forall n : nat, (Rmax (INR m) (INR n)) = (INR (Nat.max m n))) /\ ((forall m : nat, forall n : nat, (Rmin (INR m) (INR n)) = (INR (Nat.min m n))) /\ ((forall m : nat, forall n : nat, (Rplus (INR m) (INR n)) = (INR (Nat.add m n))) /\ ((forall m : nat, forall n : nat, (Rmult (INR m) (INR n)) = (INR (Nat.mul m n))) /\ (forall x : nat, forall n : nat, (Rpower_nat (INR x) n) = (INR (Nat.pow x n))))))))))).
Axiom thm_REAL_MUL_AC : forall (n : R) (m : R) (p : R), ((Rmult m n) = (Rmult n m)) /\ (((Rmult (Rmult m n) p) = (Rmult m (Rmult n p))) /\ ((Rmult m (Rmult n p)) = (Rmult n (Rmult m p)))).
Axiom thm_REAL_ADD_RDISTRIB : forall x : R, forall y : R, forall z : R, (Rmult (Rplus x y) z) = (Rplus (Rmult x z) (Rmult y z)).
Axiom thm_REAL_LT_LADD_IMP : forall x : R, forall y : R, forall z : R, (Rlt y z) -> Rlt (Rplus x y) (Rplus x z).
Axiom thm_REAL_LT_MUL : forall x : R, forall y : R, ((Rlt (INR (NUMERAL 0)) x) /\ (Rlt (INR (NUMERAL 0)) y)) -> Rlt (INR (NUMERAL 0)) (Rmult x y).
Axiom thm_REAL_EQ_ADD_LCANCEL_0 : forall x : R, forall y : R, ((Rplus x y) = x) = (y = (INR (NUMERAL 0))).
Axiom thm_REAL_EQ_ADD_RCANCEL_0 : forall x : R, forall y : R, ((Rplus x y) = y) = (x = (INR (NUMERAL 0))).
Axiom thm_REAL_LNEG_UNIQ : forall x : R, forall y : R, ((Rplus x y) = (INR (NUMERAL 0))) = (x = (Ropp y)).
Axiom thm_REAL_RNEG_UNIQ : forall x : R, forall y : R, ((Rplus x y) = (INR (NUMERAL 0))) = (y = (Ropp x)).
Axiom thm_REAL_NEG_LMUL : forall x : R, forall y : R, (Ropp (Rmult x y)) = (Rmult (Ropp x) y).
Axiom thm_REAL_NEG_RMUL : forall x : R, forall y : R, (Ropp (Rmult x y)) = (Rmult x (Ropp y)).
Axiom thm_REAL_NEG_MUL2 : forall x : R, forall y : R, (Rmult (Ropp x) (Ropp y)) = (Rmult x y).
Axiom thm_REAL_LT_LADD : forall x : R, forall y : R, forall z : R, (Rlt (Rplus x y) (Rplus x z)) = (Rlt y z).
Axiom thm_REAL_LT_RADD : forall x : R, forall y : R, forall z : R, (Rlt (Rplus x z) (Rplus y z)) = (Rlt x y).
Axiom thm_REAL_LT_ANTISYM : forall x : R, forall y : R, ~ ((Rlt x y) /\ (Rlt y x)).
Axiom thm_REAL_LT_GT : forall x : R, forall y : R, (Rlt x y) -> ~ (Rlt y x).
Axiom thm_REAL_NOT_EQ : forall x : R, forall y : R, (~ (x = y)) = ((Rlt x y) \/ (Rlt y x)).
Axiom thm_REAL_NOT_LE : forall x : R, forall y : R, (~ (Rle x y)) = (Rlt y x).
Axiom thm_REAL_LET_ANTISYM : forall x : R, forall y : R, ~ ((Rle x y) /\ (Rlt y x)).
Axiom thm_REAL_NEG_LT0 : forall x : R, (Rlt (Ropp x) (INR (NUMERAL 0))) = (Rlt (INR (NUMERAL 0)) x).
Axiom thm_REAL_NEG_GT0 : forall x : R, (Rlt (INR (NUMERAL 0)) (Ropp x)) = (Rlt x (INR (NUMERAL 0))).
Axiom thm_REAL_NEG_LE0 : forall x : R, (Rle (Ropp x) (INR (NUMERAL 0))) = (Rle (INR (NUMERAL 0)) x).
Axiom thm_REAL_NEG_GE0 : forall x : R, (Rle (INR (NUMERAL 0)) (Ropp x)) = (Rle x (INR (NUMERAL 0))).
Axiom thm_REAL_LT_TOTAL : forall x : R, forall y : R, (x = y) \/ ((Rlt x y) \/ (Rlt y x)).
Axiom thm_REAL_LT_NEGTOTAL : forall x : R, (x = (INR (NUMERAL 0))) \/ ((Rlt (INR (NUMERAL 0)) x) \/ (Rlt (INR (NUMERAL 0)) (Ropp x))).
Axiom thm_REAL_LE_01 : Rle (INR (NUMERAL 0)) (INR (NUMERAL (BIT1 0))).
Axiom thm_REAL_LT_01 : Rlt (INR (NUMERAL 0)) (INR (NUMERAL (BIT1 0))).
Axiom thm_REAL_LE_LADD : forall x : R, forall y : R, forall z : R, (Rle (Rplus x y) (Rplus x z)) = (Rle y z).
Axiom thm_REAL_LE_RADD : forall x : R, forall y : R, forall z : R, (Rle (Rplus x z) (Rplus y z)) = (Rle x y).
Axiom thm_REAL_LT_ADD2 : forall w : R, forall x : R, forall y : R, forall z : R, ((Rlt w x) /\ (Rlt y z)) -> Rlt (Rplus w y) (Rplus x z).
Axiom thm_REAL_LE_ADD2 : forall w : R, forall x : R, forall y : R, forall z : R, ((Rle w x) /\ (Rle y z)) -> Rle (Rplus w y) (Rplus x z).
Axiom thm_REAL_LT_LNEG : forall x : R, forall y : R, (Rlt (Ropp x) y) = (Rlt (INR (NUMERAL 0)) (Rplus x y)).
Axiom thm_REAL_LT_RNEG : forall x : R, forall y : R, (Rlt x (Ropp y)) = (Rlt (Rplus x y) (INR (NUMERAL 0))).
Axiom thm_REAL_LT_ADDNEG : forall x : R, forall y : R, forall z : R, (Rlt y (Rplus x (Ropp z))) = (Rlt (Rplus y z) x).
Axiom thm_REAL_LT_ADDNEG2 : forall x : R, forall y : R, forall z : R, (Rlt (Rplus x (Ropp y)) z) = (Rlt x (Rplus z y)).
Axiom thm_REAL_LT_ADD1 : forall x : R, forall y : R, (Rle x y) -> Rlt x (Rplus y (INR (NUMERAL (BIT1 0)))).
Axiom thm_REAL_SUB_ADD : forall x : R, forall y : R, (Rplus (Rminus x y) y) = x.
Axiom thm_REAL_SUB_ADD2 : forall x : R, forall y : R, (Rplus y (Rminus x y)) = x.
Axiom thm_REAL_SUB_REFL : forall x : R, (Rminus x x) = (INR (NUMERAL 0)).
Axiom thm_REAL_LE_DOUBLE : forall x : R, (Rle (INR (NUMERAL 0)) (Rplus x x)) = (Rle (INR (NUMERAL 0)) x).
Axiom thm_REAL_LE_NEGL : forall x : R, (Rle (Ropp x) x) = (Rle (INR (NUMERAL 0)) x).
Axiom thm_REAL_LE_NEGR : forall x : R, (Rle x (Ropp x)) = (Rle x (INR (NUMERAL 0))).
Axiom thm_REAL_NEG_EQ_0 : forall x : R, ((Ropp x) = (INR (NUMERAL 0))) = (x = (INR (NUMERAL 0))).
Axiom thm_REAL_ADD_SUB : forall x : R, forall y : R, (Rminus (Rplus x y) x) = y.
Axiom thm_REAL_NEG_EQ : forall x : R, forall y : R, ((Ropp x) = y) = (x = (Ropp y)).
Axiom thm_REAL_NEG_MINUS1 : forall x : R, (Ropp x) = (Rmult (Ropp (INR (NUMERAL (BIT1 0)))) x).
Axiom thm_REAL_LT_IMP_NE : forall x : R, forall y : R, (Rlt x y) -> ~ (x = y).
Axiom thm_REAL_LE_ADDR : forall x : R, forall y : R, (Rle x (Rplus x y)) = (Rle (INR (NUMERAL 0)) y).
Axiom thm_REAL_LE_ADDL : forall x : R, forall y : R, (Rle y (Rplus x y)) = (Rle (INR (NUMERAL 0)) x).
Axiom thm_REAL_LT_ADDR : forall x : R, forall y : R, (Rlt x (Rplus x y)) = (Rlt (INR (NUMERAL 0)) y).
Axiom thm_REAL_LT_ADDL : forall x : R, forall y : R, (Rlt y (Rplus x y)) = (Rlt (INR (NUMERAL 0)) x).
Axiom thm_REAL_SUB_SUB : forall x : R, forall y : R, (Rminus (Rminus x y) x) = (Ropp y).
Axiom thm_REAL_LT_ADD_SUB : forall x : R, forall y : R, forall z : R, (Rlt (Rplus x y) z) = (Rlt x (Rminus z y)).
Axiom thm_REAL_LT_SUB_RADD : forall x : R, forall y : R, forall z : R, (Rlt (Rminus x y) z) = (Rlt x (Rplus z y)).
Axiom thm_REAL_LT_SUB_LADD : forall x : R, forall y : R, forall z : R, (Rlt x (Rminus y z)) = (Rlt (Rplus x z) y).
Axiom thm_REAL_LE_SUB_LADD : forall x : R, forall y : R, forall z : R, (Rle x (Rminus y z)) = (Rle (Rplus x z) y).
Axiom thm_REAL_LE_SUB_RADD : forall x : R, forall y : R, forall z : R, (Rle (Rminus x y) z) = (Rle x (Rplus z y)).
Axiom thm_REAL_ADD2_SUB2 : forall a : R, forall b : R, forall c : R, forall d : R, (Rminus (Rplus a b) (Rplus c d)) = (Rplus (Rminus a c) (Rminus b d)).
Axiom thm_REAL_SUB_LZERO : forall x : R, (Rminus (INR (NUMERAL 0)) x) = (Ropp x).
Axiom thm_REAL_SUB_RZERO : forall x : R, (Rminus x (INR (NUMERAL 0))) = x.
Axiom thm_REAL_LET_ADD2 : forall w : R, forall x : R, forall y : R, forall z : R, ((Rle w x) /\ (Rlt y z)) -> Rlt (Rplus w y) (Rplus x z).
Axiom thm_REAL_LTE_ADD2 : forall w : R, forall x : R, forall y : R, forall z : R, ((Rlt w x) /\ (Rle y z)) -> Rlt (Rplus w y) (Rplus x z).
Axiom thm_REAL_SUB_LNEG : forall x : R, forall y : R, (Rminus (Ropp x) y) = (Ropp (Rplus x y)).
Axiom thm_REAL_SUB_RNEG : forall x : R, forall y : R, (Rminus x (Ropp y)) = (Rplus x y).
Axiom thm_REAL_SUB_NEG2 : forall x : R, forall y : R, (Rminus (Ropp x) (Ropp y)) = (Rminus y x).
Axiom thm_REAL_SUB_TRIANGLE : forall a : R, forall b : R, forall c : R, (Rplus (Rminus a b) (Rminus b c)) = (Rminus a c).
Axiom thm_REAL_EQ_SUB_LADD : forall x : R, forall y : R, forall z : R, (x = (Rminus y z)) = ((Rplus x z) = y).
Axiom thm_REAL_EQ_SUB_RADD : forall x : R, forall y : R, forall z : R, ((Rminus x y) = z) = (x = (Rplus z y)).
Axiom thm_REAL_SUB_SUB2 : forall x : R, forall y : R, (Rminus x (Rminus x y)) = y.
Axiom thm_REAL_ADD_SUB2 : forall x : R, forall y : R, (Rminus x (Rplus x y)) = (Ropp y).
Axiom thm_REAL_EQ_IMP_LE : forall x : R, forall y : R, (x = y) -> Rle x y.
Axiom thm_REAL_LT_IMP_NZ : forall x : R, (Rlt (INR (NUMERAL 0)) x) -> ~ (x = (INR (NUMERAL 0))).
Axiom thm_REAL_DIFFSQ : forall x : R, forall y : R, (Rmult (Rplus x y) (Rminus x y)) = (Rminus (Rmult x x) (Rmult y y)).
Axiom thm_REAL_EQ_NEG2 : forall x : R, forall y : R, ((Ropp x) = (Ropp y)) = (x = y).
Axiom thm_REAL_LT_NEG2 : forall x : R, forall y : R, (Rlt (Ropp x) (Ropp y)) = (Rlt y x).
Axiom thm_REAL_SUB_LDISTRIB : forall x : R, forall y : R, forall z : R, (Rmult x (Rminus y z)) = (Rminus (Rmult x y) (Rmult x z)).
Axiom thm_REAL_SUB_RDISTRIB : forall x : R, forall y : R, forall z : R, (Rmult (Rminus x y) z) = (Rminus (Rmult x z) (Rmult y z)).
Axiom thm_REAL_ABS_ZERO : forall x : R, ((Rabs x) = (INR (NUMERAL 0))) = (x = (INR (NUMERAL 0))).
Axiom thm_REAL_ABS_0 : (Rabs (INR (NUMERAL 0))) = (INR (NUMERAL 0)).
Axiom thm_REAL_ABS_1 : (Rabs (INR (NUMERAL (BIT1 0)))) = (INR (NUMERAL (BIT1 0))).
Axiom thm_REAL_ABS_TRIANGLE : forall x : R, forall y : R, Rle (Rabs (Rplus x y)) (Rplus (Rabs x) (Rabs y)).
Axiom thm_REAL_ABS_TRIANGLE_LE : forall x : R, forall y : R, forall z : R, (Rle (Rplus (Rabs x) (Rabs (Rminus y x))) z) -> Rle (Rabs y) z.
Axiom thm_REAL_ABS_TRIANGLE_LT : forall x : R, forall y : R, forall z : R, (Rlt (Rplus (Rabs x) (Rabs (Rminus y x))) z) -> Rlt (Rabs y) z.
Axiom thm_REAL_ABS_POS : forall x : R, Rle (INR (NUMERAL 0)) (Rabs x).
Axiom thm_REAL_ABS_SUB : forall x : R, forall y : R, (Rabs (Rminus x y)) = (Rabs (Rminus y x)).
Axiom thm_REAL_ABS_NZ : forall x : R, (~ (x = (INR (NUMERAL 0)))) = (Rlt (INR (NUMERAL 0)) (Rabs x)).
Axiom thm_REAL_ABS_ABS : forall x : R, (Rabs (Rabs x)) = (Rabs x).
Axiom thm_REAL_ABS_LE : forall x : R, Rle x (Rabs x).
Axiom thm_REAL_ABS_REFL : forall x : R, ((Rabs x) = x) = (Rle (INR (NUMERAL 0)) x).
Axiom thm_REAL_ABS_BETWEEN : forall x : R, forall y : R, forall d : R, ((Rlt (INR (NUMERAL 0)) d) /\ ((Rlt (Rminus x d) y) /\ (Rlt y (Rplus x d)))) = (Rlt (Rabs (Rminus y x)) d).
Axiom thm_REAL_ABS_BOUND : forall x : R, forall y : R, forall d : R, (Rlt (Rabs (Rminus x y)) d) -> Rlt y (Rplus x d).
Axiom thm_REAL_ABS_STILLNZ : forall x : R, forall y : R, (Rlt (Rabs (Rminus x y)) (Rabs y)) -> ~ (x = (INR (NUMERAL 0))).
Axiom thm_REAL_ABS_CASES : forall x : R, (x = (INR (NUMERAL 0))) \/ (Rlt (INR (NUMERAL 0)) (Rabs x)).
Axiom thm_REAL_ABS_BETWEEN1 : forall x : R, forall y : R, forall z : R, ((Rlt x z) /\ (Rlt (Rabs (Rminus y x)) (Rminus z x))) -> Rlt y z.
Axiom thm_REAL_ABS_SIGN : forall x : R, forall y : R, (Rlt (Rabs (Rminus x y)) y) -> Rlt (INR (NUMERAL 0)) x.
Axiom thm_REAL_ABS_SIGN2 : forall x : R, forall y : R, (Rlt (Rabs (Rminus x y)) (Ropp y)) -> Rlt x (INR (NUMERAL 0)).
Axiom thm_REAL_ABS_CIRCLE : forall x : R, forall y : R, forall h : R, (Rlt (Rabs h) (Rminus (Rabs y) (Rabs x))) -> Rlt (Rabs (Rplus x h)) (Rabs y).
Axiom thm_REAL_SUB_ABS : forall x : R, forall y : R, Rle (Rminus (Rabs x) (Rabs y)) (Rabs (Rminus x y)).
Axiom thm_REAL_ABS_SUB_ABS : forall x : R, forall y : R, Rle (Rabs (Rminus (Rabs x) (Rabs y))) (Rabs (Rminus x y)).
Axiom thm_REAL_ABS_BETWEEN2 : forall x0 : R, forall x : R, forall y0 : R, forall y : R, ((Rlt x0 y0) /\ ((Rlt (Rmult (INR (NUMERAL (BIT0 (BIT1 0)))) (Rabs (Rminus x x0))) (Rminus y0 x0)) /\ (Rlt (Rmult (INR (NUMERAL (BIT0 (BIT1 0)))) (Rabs (Rminus y y0))) (Rminus y0 x0)))) -> Rlt x y.
Axiom thm_REAL_ABS_BOUNDS : forall x : R, forall k : R, (Rle (Rabs x) k) = ((Rle (Ropp k) x) /\ (Rle x k)).
Axiom thm_REAL_BOUNDS_LE : forall x : R, forall k : R, ((Rle (Ropp k) x) /\ (Rle x k)) = (Rle (Rabs x) k).
Axiom thm_REAL_BOUNDS_LT : forall x : R, forall k : R, ((Rlt (Ropp k) x) /\ (Rlt x k)) = (Rlt (Rabs x) k).
Axiom thm_REAL_MIN_MAX : forall x : R, forall y : R, (Rmin x y) = (Ropp (Rmax (Ropp x) (Ropp y))).
Axiom thm_REAL_MAX_MIN : forall x : R, forall y : R, (Rmax x y) = (Ropp (Rmin (Ropp x) (Ropp y))).
Axiom thm_REAL_MAX_MAX : forall x : R, forall y : R, (Rle x (Rmax x y)) /\ (Rle y (Rmax x y)).
Axiom thm_REAL_MIN_MIN : forall x : R, forall y : R, (Rle (Rmin x y) x) /\ (Rle (Rmin x y) y).
Axiom thm_REAL_MAX_SYM : forall x : R, forall y : R, (Rmax x y) = (Rmax y x).
Axiom thm_REAL_MIN_SYM : forall x : R, forall y : R, (Rmin x y) = (Rmin y x).
Axiom thm_REAL_LE_MAX : forall x : R, forall y : R, forall z : R, (Rle z (Rmax x y)) = ((Rle z x) \/ (Rle z y)).
Axiom thm_REAL_LE_MIN : forall x : R, forall y : R, forall z : R, (Rle z (Rmin x y)) = ((Rle z x) /\ (Rle z y)).
Axiom thm_REAL_LT_MAX : forall x : R, forall y : R, forall z : R, (Rlt z (Rmax x y)) = ((Rlt z x) \/ (Rlt z y)).
Axiom thm_REAL_LT_MIN : forall x : R, forall y : R, forall z : R, (Rlt z (Rmin x y)) = ((Rlt z x) /\ (Rlt z y)).
Axiom thm_REAL_MAX_LE : forall x : R, forall y : R, forall z : R, (Rle (Rmax x y) z) = ((Rle x z) /\ (Rle y z)).
Axiom thm_REAL_MIN_LE : forall x : R, forall y : R, forall z : R, (Rle (Rmin x y) z) = ((Rle x z) \/ (Rle y z)).
Axiom thm_REAL_MAX_LT : forall x : R, forall y : R, forall z : R, (Rlt (Rmax x y) z) = ((Rlt x z) /\ (Rlt y z)).
Axiom thm_REAL_MIN_LT : forall x : R, forall y : R, forall z : R, (Rlt (Rmin x y) z) = ((Rlt x z) \/ (Rlt y z)).
Axiom thm_REAL_MAX_ASSOC : forall x : R, forall y : R, forall z : R, (Rmax x (Rmax y z)) = (Rmax (Rmax x y) z).
Axiom thm_REAL_MIN_ASSOC : forall x : R, forall y : R, forall z : R, (Rmin x (Rmin y z)) = (Rmin (Rmin x y) z).
Axiom thm_REAL_MAX_ACI : forall (z : R) (x : R) (y : R), ((Rmax x y) = (Rmax y x)) /\ (((Rmax (Rmax x y) z) = (Rmax x (Rmax y z))) /\ (((Rmax x (Rmax y z)) = (Rmax y (Rmax x z))) /\ (((Rmax x x) = x) /\ ((Rmax x (Rmax x y)) = (Rmax x y))))).
Axiom thm_REAL_MIN_ACI : forall (z : R) (x : R) (y : R), ((Rmin x y) = (Rmin y x)) /\ (((Rmin (Rmin x y) z) = (Rmin x (Rmin y z))) /\ (((Rmin x (Rmin y z)) = (Rmin y (Rmin x z))) /\ (((Rmin x x) = x) /\ ((Rmin x (Rmin x y)) = (Rmin x y))))).
Axiom thm_REAL_ABS_MUL : forall x : R, forall y : R, (Rabs (Rmult x y)) = (Rmult (Rabs x) (Rabs y)).
Axiom thm_REAL_POW_LE : forall x : R, forall n : nat, (Rle (INR (NUMERAL 0)) x) -> Rle (INR (NUMERAL 0)) (Rpower_nat x n).
Axiom thm_REAL_POW_LT : forall x : R, forall n : nat, (Rlt (INR (NUMERAL 0)) x) -> Rlt (INR (NUMERAL 0)) (Rpower_nat x n).
Axiom thm_REAL_ABS_POW : forall x : R, forall n : nat, (Rabs (Rpower_nat x n)) = (Rpower_nat (Rabs x) n).
Axiom thm_REAL_LE_LMUL : forall x : R, forall y : R, forall z : R, ((Rle (INR (NUMERAL 0)) x) /\ (Rle y z)) -> Rle (Rmult x y) (Rmult x z).
Axiom thm_REAL_LE_RMUL : forall x : R, forall y : R, forall z : R, ((Rle x y) /\ (Rle (INR (NUMERAL 0)) z)) -> Rle (Rmult x z) (Rmult y z).
Axiom thm_REAL_LT_LMUL : forall x : R, forall y : R, forall z : R, ((Rlt (INR (NUMERAL 0)) x) /\ (Rlt y z)) -> Rlt (Rmult x y) (Rmult x z).
Axiom thm_REAL_LT_RMUL : forall x : R, forall y : R, forall z : R, ((Rlt x y) /\ (Rlt (INR (NUMERAL 0)) z)) -> Rlt (Rmult x z) (Rmult y z).
Axiom thm_REAL_EQ_MUL_LCANCEL : forall x : R, forall y : R, forall z : R, ((Rmult x y) = (Rmult x z)) = ((x = (INR (NUMERAL 0))) \/ (y = z)).
Axiom thm_REAL_EQ_MUL_RCANCEL : forall x : R, forall y : R, forall z : R, ((Rmult x z) = (Rmult y z)) = ((x = y) \/ (z = (INR (NUMERAL 0)))).
Axiom thm_REAL_MUL_LINV_UNIQ : forall x : R, forall y : R, ((Rmult x y) = (INR (NUMERAL (BIT1 0)))) -> (Rinv y) = x.
Axiom thm_REAL_MUL_RINV_UNIQ : forall x : R, forall y : R, ((Rmult x y) = (INR (NUMERAL (BIT1 0)))) -> (Rinv x) = y.
Axiom thm_REAL_INV_INV : forall x : R, (Rinv (Rinv x)) = x.
Axiom thm_REAL_EQ_INV2 : forall x : R, forall y : R, ((Rinv x) = (Rinv y)) = (x = y).
Axiom thm_REAL_INV_EQ_0 : forall x : R, ((Rinv x) = (INR (NUMERAL 0))) = (x = (INR (NUMERAL 0))).
Axiom thm_REAL_LT_INV : forall x : R, (Rlt (INR (NUMERAL 0)) x) -> Rlt (INR (NUMERAL 0)) (Rinv x).
Axiom thm_REAL_LT_INV_EQ : forall x : R, (Rlt (INR (NUMERAL 0)) (Rinv x)) = (Rlt (INR (NUMERAL 0)) x).
Axiom thm_REAL_INV_NEG : forall x : R, (Rinv (Ropp x)) = (Ropp (Rinv x)).
Axiom thm_REAL_LE_INV_EQ : forall x : R, (Rle (INR (NUMERAL 0)) (Rinv x)) = (Rle (INR (NUMERAL 0)) x).
Axiom thm_REAL_LE_INV : forall x : R, (Rle (INR (NUMERAL 0)) x) -> Rle (INR (NUMERAL 0)) (Rinv x).
Axiom thm_REAL_MUL_RINV : forall x : R, (~ (x = (INR (NUMERAL 0)))) -> (Rmult x (Rinv x)) = (INR (NUMERAL (BIT1 0))).
Axiom thm_REAL_INV_1 : (Rinv (INR (NUMERAL (BIT1 0)))) = (INR (NUMERAL (BIT1 0))).
Axiom thm_REAL_INV_EQ_1 : forall x : R, ((Rinv x) = (INR (NUMERAL (BIT1 0)))) = (x = (INR (NUMERAL (BIT1 0)))).
Axiom thm_REAL_DIV_1 : forall x : R, (Rdiv x (INR (NUMERAL (BIT1 0)))) = x.
Axiom thm_REAL_DIV_REFL : forall x : R, (~ (x = (INR (NUMERAL 0)))) -> (Rdiv x x) = (INR (NUMERAL (BIT1 0))).
Axiom thm_REAL_DIV_RMUL : forall x : R, forall y : R, (~ (y = (INR (NUMERAL 0)))) -> (Rmult (Rdiv x y) y) = x.
Axiom thm_REAL_DIV_LMUL : forall x : R, forall y : R, (~ (y = (INR (NUMERAL 0)))) -> (Rmult y (Rdiv x y)) = x.
Axiom thm_REAL_DIV_EQ_1 : forall x : R, forall y : R, ((Rdiv x y) = (INR (NUMERAL (BIT1 0)))) = ((x = y) /\ ((~ (x = (INR (NUMERAL 0)))) /\ (~ (y = (INR (NUMERAL 0)))))).
Axiom thm_REAL_ABS_INV : forall x : R, (Rabs (Rinv x)) = (Rinv (Rabs x)).
Axiom thm_REAL_ABS_DIV : forall x : R, forall y : R, (Rabs (Rdiv x y)) = (Rdiv (Rabs x) (Rabs y)).
Axiom thm_REAL_INV_MUL : forall x : R, forall y : R, (Rinv (Rmult x y)) = (Rmult (Rinv x) (Rinv y)).
Axiom thm_REAL_INV_DIV : forall x : R, forall y : R, (Rinv (Rdiv x y)) = (Rdiv y x).
Axiom thm_REAL_POW_MUL : forall x : R, forall y : R, forall n : nat, (Rpower_nat (Rmult x y) n) = (Rmult (Rpower_nat x n) (Rpower_nat y n)).
Axiom thm_REAL_POW_INV : forall x : R, forall n : nat, (Rpower_nat (Rinv x) n) = (Rinv (Rpower_nat x n)).
Axiom thm_REAL_INV_POW : forall x : R, forall n : nat, (Rinv (Rpower_nat x n)) = (Rpower_nat (Rinv x) n).
Axiom thm_REAL_POW_DIV : forall x : R, forall y : R, forall n : nat, (Rpower_nat (Rdiv x y) n) = (Rdiv (Rpower_nat x n) (Rpower_nat y n)).
Axiom thm_REAL_DIV_EQ_0 : forall x : R, forall y : R, ((Rdiv x y) = (INR (NUMERAL 0))) = ((x = (INR (NUMERAL 0))) \/ (y = (INR (NUMERAL 0)))).
Axiom thm_REAL_POW_ADD : forall x : R, forall m : nat, forall n : nat, (Rpower_nat x (Nat.add m n)) = (Rmult (Rpower_nat x m) (Rpower_nat x n)).
Axiom thm_REAL_POW_NZ : forall x : R, forall n : nat, (~ (x = (INR (NUMERAL 0)))) -> ~ ((Rpower_nat x n) = (INR (NUMERAL 0))).
Axiom thm_REAL_POW_SUB : forall x : R, forall m : nat, forall n : nat, ((~ (x = (INR (NUMERAL 0)))) /\ (Peano.le m n)) -> (Rpower_nat x (Nat.sub n m)) = (Rdiv (Rpower_nat x n) (Rpower_nat x m)).
Axiom thm_REAL_LT_LCANCEL_IMP : forall x : R, forall y : R, forall z : R, ((Rlt (INR (NUMERAL 0)) x) /\ (Rlt (Rmult x y) (Rmult x z))) -> Rlt y z.
Axiom thm_REAL_LT_RCANCEL_IMP : forall x : R, forall y : R, forall z : R, ((Rlt (INR (NUMERAL 0)) z) /\ (Rlt (Rmult x z) (Rmult y z))) -> Rlt x y.
Axiom thm_REAL_LE_LCANCEL_IMP : forall x : R, forall y : R, forall z : R, ((Rlt (INR (NUMERAL 0)) x) /\ (Rle (Rmult x y) (Rmult x z))) -> Rle y z.
Axiom thm_REAL_LE_RCANCEL_IMP : forall x : R, forall y : R, forall z : R, ((Rlt (INR (NUMERAL 0)) z) /\ (Rle (Rmult x z) (Rmult y z))) -> Rle x y.
Axiom thm_REAL_LE_RMUL_EQ : forall x : R, forall y : R, forall z : R, (Rlt (INR (NUMERAL 0)) z) -> (Rle (Rmult x z) (Rmult y z)) = (Rle x y).
Axiom thm_REAL_LE_LMUL_EQ : forall x : R, forall y : R, forall z : R, (Rlt (INR (NUMERAL 0)) z) -> (Rle (Rmult z x) (Rmult z y)) = (Rle x y).
Axiom thm_REAL_LT_RMUL_EQ : forall x : R, forall y : R, forall z : R, (Rlt (INR (NUMERAL 0)) z) -> (Rlt (Rmult x z) (Rmult y z)) = (Rlt x y).
Axiom thm_REAL_LT_LMUL_EQ : forall x : R, forall y : R, forall z : R, (Rlt (INR (NUMERAL 0)) z) -> (Rlt (Rmult z x) (Rmult z y)) = (Rlt x y).
Axiom thm_REAL_LE_MUL_EQ : (forall x : R, forall y : R, (Rlt (INR (NUMERAL 0)) x) -> (Rle (INR (NUMERAL 0)) (Rmult x y)) = (Rle (INR (NUMERAL 0)) y)) /\ (forall x : R, forall y : R, (Rlt (INR (NUMERAL 0)) y) -> (Rle (INR (NUMERAL 0)) (Rmult x y)) = (Rle (INR (NUMERAL 0)) x)).
Axiom thm_REAL_LT_MUL_EQ : (forall x : R, forall y : R, (Rlt (INR (NUMERAL 0)) x) -> (Rlt (INR (NUMERAL 0)) (Rmult x y)) = (Rlt (INR (NUMERAL 0)) y)) /\ (forall x : R, forall y : R, (Rlt (INR (NUMERAL 0)) y) -> (Rlt (INR (NUMERAL 0)) (Rmult x y)) = (Rlt (INR (NUMERAL 0)) x)).
Axiom thm_REAL_MUL_POS_LT : forall x : R, forall y : R, (Rlt (INR (NUMERAL 0)) (Rmult x y)) = (((Rlt (INR (NUMERAL 0)) x) /\ (Rlt (INR (NUMERAL 0)) y)) \/ ((Rlt x (INR (NUMERAL 0))) /\ (Rlt y (INR (NUMERAL 0))))).
Axiom thm_REAL_MUL_POS_LE : forall x : R, forall y : R, (Rle (INR (NUMERAL 0)) (Rmult x y)) = ((x = (INR (NUMERAL 0))) \/ ((y = (INR (NUMERAL 0))) \/ (((Rlt (INR (NUMERAL 0)) x) /\ (Rlt (INR (NUMERAL 0)) y)) \/ ((Rlt x (INR (NUMERAL 0))) /\ (Rlt y (INR (NUMERAL 0))))))).
Axiom thm_REAL_LE_RDIV_EQ : forall x : R, forall y : R, forall z : R, (Rlt (INR (NUMERAL 0)) z) -> (Rle x (Rdiv y z)) = (Rle (Rmult x z) y).
Axiom thm_REAL_LE_LDIV_EQ : forall x : R, forall y : R, forall z : R, (Rlt (INR (NUMERAL 0)) z) -> (Rle (Rdiv x z) y) = (Rle x (Rmult y z)).
Axiom thm_REAL_LT_RDIV_EQ : forall x : R, forall y : R, forall z : R, (Rlt (INR (NUMERAL 0)) z) -> (Rlt x (Rdiv y z)) = (Rlt (Rmult x z) y).
Axiom thm_REAL_LT_LDIV_EQ : forall x : R, forall y : R, forall z : R, (Rlt (INR (NUMERAL 0)) z) -> (Rlt (Rdiv x z) y) = (Rlt x (Rmult y z)).
Axiom thm_REAL_EQ_RDIV_EQ : forall x : R, forall y : R, forall z : R, (Rlt (INR (NUMERAL 0)) z) -> (x = (Rdiv y z)) = ((Rmult x z) = y).
Axiom thm_REAL_EQ_LDIV_EQ : forall x : R, forall y : R, forall z : R, (Rlt (INR (NUMERAL 0)) z) -> ((Rdiv x z) = y) = (x = (Rmult y z)).
Axiom thm_REAL_LT_DIV2_EQ : forall x : R, forall y : R, forall z : R, (Rlt (INR (NUMERAL 0)) z) -> (Rlt (Rdiv x z) (Rdiv y z)) = (Rlt x y).
Axiom thm_REAL_LE_DIV2_EQ : forall x : R, forall y : R, forall z : R, (Rlt (INR (NUMERAL 0)) z) -> (Rle (Rdiv x z) (Rdiv y z)) = (Rle x y).
Axiom thm_REAL_MUL_2 : forall x : R, (Rmult (INR (NUMERAL (BIT0 (BIT1 0)))) x) = (Rplus x x).
Axiom thm_REAL_POW_EQ_0 : forall x : R, forall n : nat, ((Rpower_nat x n) = (INR (NUMERAL 0))) = ((x = (INR (NUMERAL 0))) /\ (~ (n = (NUMERAL 0)))).
Axiom thm_REAL_LE_MUL2 : forall w : R, forall x : R, forall y : R, forall z : R, ((Rle (INR (NUMERAL 0)) w) /\ ((Rle w x) /\ ((Rle (INR (NUMERAL 0)) y) /\ (Rle y z)))) -> Rle (Rmult w y) (Rmult x z).
Axiom thm_REAL_LT_MUL2 : forall w : R, forall x : R, forall y : R, forall z : R, ((Rle (INR (NUMERAL 0)) w) /\ ((Rlt w x) /\ ((Rle (INR (NUMERAL 0)) y) /\ (Rlt y z)))) -> Rlt (Rmult w y) (Rmult x z).
Axiom thm_REAL_LT_SQUARE : forall x : R, (Rlt (INR (NUMERAL 0)) (Rmult x x)) = (~ (x = (INR (NUMERAL 0)))).
Axiom thm_REAL_POW_1 : forall x : R, (Rpower_nat x (NUMERAL (BIT1 0))) = x.
Axiom thm_REAL_POW_ONE : forall n : nat, (Rpower_nat (INR (NUMERAL (BIT1 0))) n) = (INR (NUMERAL (BIT1 0))).
Axiom thm_REAL_LT_INV2 : forall x : R, forall y : R, ((Rlt (INR (NUMERAL 0)) x) /\ (Rlt x y)) -> Rlt (Rinv y) (Rinv x).
Axiom thm_REAL_LE_INV2 : forall x : R, forall y : R, ((Rlt (INR (NUMERAL 0)) x) /\ (Rle x y)) -> Rle (Rinv y) (Rinv x).
Axiom thm_REAL_LT_LINV : forall x : R, forall y : R, ((Rlt (INR (NUMERAL 0)) y) /\ (Rlt (Rinv y) x)) -> Rlt (Rinv x) y.
Axiom thm_REAL_LT_RINV : forall x : R, forall y : R, ((Rlt (INR (NUMERAL 0)) x) /\ (Rlt x (Rinv y))) -> Rlt y (Rinv x).
Axiom thm_REAL_LE_LINV : forall x : R, forall y : R, ((Rlt (INR (NUMERAL 0)) y) /\ (Rle (Rinv y) x)) -> Rle (Rinv x) y.
Axiom thm_REAL_LE_RINV : forall x : R, forall y : R, ((Rlt (INR (NUMERAL 0)) x) /\ (Rle x (Rinv y))) -> Rle y (Rinv x).
Axiom thm_REAL_INV_LE_1 : forall x : R, (Rle (INR (NUMERAL (BIT1 0))) x) -> Rle (Rinv x) (INR (NUMERAL (BIT1 0))).
Axiom thm_REAL_INV_1_LE : forall x : R, ((Rlt (INR (NUMERAL 0)) x) /\ (Rle x (INR (NUMERAL (BIT1 0))))) -> Rle (INR (NUMERAL (BIT1 0))) (Rinv x).
Axiom thm_REAL_INV_LT_1 : forall x : R, (Rlt (INR (NUMERAL (BIT1 0))) x) -> Rlt (Rinv x) (INR (NUMERAL (BIT1 0))).
Axiom thm_REAL_INV_1_LT : forall x : R, ((Rlt (INR (NUMERAL 0)) x) /\ (Rlt x (INR (NUMERAL (BIT1 0))))) -> Rlt (INR (NUMERAL (BIT1 0))) (Rinv x).
Axiom thm_REAL_SUB_INV : forall x : R, forall y : R, ((~ (x = (INR (NUMERAL 0)))) /\ (~ (y = (INR (NUMERAL 0))))) -> (Rminus (Rinv x) (Rinv y)) = (Rdiv (Rminus y x) (Rmult x y)).
Axiom thm_REAL_DOWN : forall d : R, (Rlt (INR (NUMERAL 0)) d) -> exists e : R, (Rlt (INR (NUMERAL 0)) e) /\ (Rlt e d).
Axiom thm_REAL_DOWN2 : forall d1 : R, forall d2 : R, ((Rlt (INR (NUMERAL 0)) d1) /\ (Rlt (INR (NUMERAL 0)) d2)) -> exists e : R, (Rlt (INR (NUMERAL 0)) e) /\ ((Rlt e d1) /\ (Rlt e d2)).
Axiom thm_REAL_POW_LE2 : forall n : nat, forall x : R, forall y : R, ((Rle (INR (NUMERAL 0)) x) /\ (Rle x y)) -> Rle (Rpower_nat x n) (Rpower_nat y n).
Axiom thm_REAL_POW_LE_1 : forall n : nat, forall x : R, (Rle (INR (NUMERAL (BIT1 0))) x) -> Rle (INR (NUMERAL (BIT1 0))) (Rpower_nat x n).
Axiom thm_REAL_POW_1_LE : forall n : nat, forall x : R, ((Rle (INR (NUMERAL 0)) x) /\ (Rle x (INR (NUMERAL (BIT1 0))))) -> Rle (Rpower_nat x n) (INR (NUMERAL (BIT1 0))).
Axiom thm_REAL_POW_MONO : forall m : nat, forall n : nat, forall x : R, ((Rle (INR (NUMERAL (BIT1 0))) x) /\ (Peano.le m n)) -> Rle (Rpower_nat x m) (Rpower_nat x n).
Axiom thm_REAL_POW_LT2 : forall n : nat, forall x : R, forall y : R, ((~ (n = (NUMERAL 0))) /\ ((Rle (INR (NUMERAL 0)) x) /\ (Rlt x y))) -> Rlt (Rpower_nat x n) (Rpower_nat y n).
Axiom thm_REAL_POW_LT_1 : forall n : nat, forall x : R, ((~ (n = (NUMERAL 0))) /\ (Rlt (INR (NUMERAL (BIT1 0))) x)) -> Rlt (INR (NUMERAL (BIT1 0))) (Rpower_nat x n).
Axiom thm_REAL_POW_1_LT : forall n : nat, forall x : R, ((~ (n = (NUMERAL 0))) /\ ((Rle (INR (NUMERAL 0)) x) /\ (Rlt x (INR (NUMERAL (BIT1 0)))))) -> Rlt (Rpower_nat x n) (INR (NUMERAL (BIT1 0))).
Axiom thm_REAL_POW_MONO_LT : forall m : nat, forall n : nat, forall x : R, ((Rlt (INR (NUMERAL (BIT1 0))) x) /\ (Peano.lt m n)) -> Rlt (Rpower_nat x m) (Rpower_nat x n).
Axiom thm_REAL_POW_POW : forall x : R, forall m : nat, forall n : nat, (Rpower_nat (Rpower_nat x m) n) = (Rpower_nat x (Nat.mul m n)).
Axiom thm_REAL_EQ_RCANCEL_IMP : forall x : R, forall y : R, forall z : R, ((~ (z = (INR (NUMERAL 0)))) /\ ((Rmult x z) = (Rmult y z))) -> x = y.
Axiom thm_REAL_EQ_LCANCEL_IMP : forall x : R, forall y : R, forall z : R, ((~ (z = (INR (NUMERAL 0)))) /\ ((Rmult z x) = (Rmult z y))) -> x = y.
Axiom thm_REAL_LT_DIV : forall x : R, forall y : R, ((Rlt (INR (NUMERAL 0)) x) /\ (Rlt (INR (NUMERAL 0)) y)) -> Rlt (INR (NUMERAL 0)) (Rdiv x y).
Axiom thm_REAL_LE_DIV : forall x : R, forall y : R, ((Rle (INR (NUMERAL 0)) x) /\ (Rle (INR (NUMERAL 0)) y)) -> Rle (INR (NUMERAL 0)) (Rdiv x y).
Axiom thm_REAL_DIV_POW2 : forall x : R, forall m : nat, forall n : nat, (~ (x = (INR (NUMERAL 0)))) -> (Rdiv (Rpower_nat x m) (Rpower_nat x n)) = (@COND R (Peano.le n m) (Rpower_nat x (Nat.sub m n)) (Rinv (Rpower_nat x (Nat.sub n m)))).
Axiom thm_REAL_DIV_POW2_ALT : forall x : R, forall m : nat, forall n : nat, (~ (x = (INR (NUMERAL 0)))) -> (Rdiv (Rpower_nat x m) (Rpower_nat x n)) = (@COND R (Peano.lt n m) (Rpower_nat x (Nat.sub m n)) (Rinv (Rpower_nat x (Nat.sub n m)))).
Axiom thm_REAL_LT_POW2 : forall n : nat, Rlt (INR (NUMERAL 0)) (Rpower_nat (INR (NUMERAL (BIT0 (BIT1 0)))) n).
Axiom thm_REAL_LE_POW2 : forall n : nat, Rle (INR (NUMERAL (BIT1 0))) (Rpower_nat (INR (NUMERAL (BIT0 (BIT1 0)))) n).
Axiom thm_REAL_POW2_ABS : forall x : R, (Rpower_nat (Rabs x) (NUMERAL (BIT0 (BIT1 0)))) = (Rpower_nat x (NUMERAL (BIT0 (BIT1 0)))).
Axiom thm_REAL_LE_SQUARE_ABS : forall x : R, forall y : R, (Rle (Rabs x) (Rabs y)) = (Rle (Rpower_nat x (NUMERAL (BIT0 (BIT1 0)))) (Rpower_nat y (NUMERAL (BIT0 (BIT1 0))))).
Axiom thm_REAL_LT_SQUARE_ABS : forall x : R, forall y : R, (Rlt (Rabs x) (Rabs y)) = (Rlt (Rpower_nat x (NUMERAL (BIT0 (BIT1 0)))) (Rpower_nat y (NUMERAL (BIT0 (BIT1 0))))).
Axiom thm_REAL_EQ_SQUARE_ABS : forall x : R, forall y : R, ((Rabs x) = (Rabs y)) = ((Rpower_nat x (NUMERAL (BIT0 (BIT1 0)))) = (Rpower_nat y (NUMERAL (BIT0 (BIT1 0))))).
Axiom thm_REAL_LE_POW_2 : forall x : R, Rle (INR (NUMERAL 0)) (Rpower_nat x (NUMERAL (BIT0 (BIT1 0)))).
Axiom thm_REAL_LT_POW_2 : forall x : R, (Rlt (INR (NUMERAL 0)) (Rpower_nat x (NUMERAL (BIT0 (BIT1 0))))) = (~ (x = (INR (NUMERAL 0)))).
Axiom thm_REAL_SOS_EQ_0 : forall x : R, forall y : R, ((Rplus (Rpower_nat x (NUMERAL (BIT0 (BIT1 0)))) (Rpower_nat y (NUMERAL (BIT0 (BIT1 0))))) = (INR (NUMERAL 0))) = ((x = (INR (NUMERAL 0))) /\ (y = (INR (NUMERAL 0)))).
Axiom thm_REAL_POW_ZERO : forall n : nat, (Rpower_nat (INR (NUMERAL 0)) n) = (@COND R (n = (NUMERAL 0)) (INR (NUMERAL (BIT1 0))) (INR (NUMERAL 0))).
Axiom thm_REAL_POW_MONO_INV : forall m : nat, forall n : nat, forall x : R, ((Rle (INR (NUMERAL 0)) x) /\ ((Rle x (INR (NUMERAL (BIT1 0)))) /\ (Peano.le n m))) -> Rle (Rpower_nat x m) (Rpower_nat x n).
Axiom thm_REAL_POW_LE2_REV : forall n : nat, forall x : R, forall y : R, ((~ (n = (NUMERAL 0))) /\ ((Rle (INR (NUMERAL 0)) y) /\ (Rle (Rpower_nat x n) (Rpower_nat y n)))) -> Rle x y.
Axiom thm_REAL_POW_LT2_REV : forall n : nat, forall x : R, forall y : R, ((Rle (INR (NUMERAL 0)) y) /\ (Rlt (Rpower_nat x n) (Rpower_nat y n))) -> Rlt x y.
Axiom thm_REAL_POW_EQ : forall n : nat, forall x : R, forall y : R, ((~ (n = (NUMERAL 0))) /\ ((Rle (INR (NUMERAL 0)) x) /\ ((Rle (INR (NUMERAL 0)) y) /\ ((Rpower_nat x n) = (Rpower_nat y n))))) -> x = y.
Axiom thm_REAL_POW_EQ_ABS : forall n : nat, forall x : R, forall y : R, ((~ (n = (NUMERAL 0))) /\ ((Rpower_nat x n) = (Rpower_nat y n))) -> (Rabs x) = (Rabs y).
Axiom thm_REAL_POW_EQ_1_IMP : forall x : R, forall n : nat, ((~ (n = (NUMERAL 0))) /\ ((Rpower_nat x n) = (INR (NUMERAL (BIT1 0))))) -> (Rabs x) = (INR (NUMERAL (BIT1 0))).
Axiom thm_REAL_POW_EQ_1 : forall x : R, forall n : nat, ((Rpower_nat x n) = (INR (NUMERAL (BIT1 0)))) = ((((Rabs x) = (INR (NUMERAL (BIT1 0)))) /\ ((Rlt x (INR (NUMERAL 0))) -> Coq.Arith.PeanoNat.Nat.Even n)) \/ (n = (NUMERAL 0))).
Axiom thm_REAL_POW_LT2_ODD : forall n : nat, forall x : R, forall y : R, ((Rlt x y) /\ (Coq.Arith.PeanoNat.Nat.Odd n)) -> Rlt (Rpower_nat x n) (Rpower_nat y n).
Axiom thm_REAL_POW_LE2_ODD : forall n : nat, forall x : R, forall y : R, ((Rle x y) /\ (Coq.Arith.PeanoNat.Nat.Odd n)) -> Rle (Rpower_nat x n) (Rpower_nat y n).
Axiom thm_REAL_POW_LT2_ODD_EQ : forall n : nat, forall x : R, forall y : R, (Coq.Arith.PeanoNat.Nat.Odd n) -> (Rlt (Rpower_nat x n) (Rpower_nat y n)) = (Rlt x y).
Axiom thm_REAL_POW_LE2_ODD_EQ : forall n : nat, forall x : R, forall y : R, (Coq.Arith.PeanoNat.Nat.Odd n) -> (Rle (Rpower_nat x n) (Rpower_nat y n)) = (Rle x y).
Axiom thm_REAL_POW_EQ_ODD_EQ : forall n : nat, forall x : R, forall y : R, (Coq.Arith.PeanoNat.Nat.Odd n) -> ((Rpower_nat x n) = (Rpower_nat y n)) = (x = y).
Axiom thm_REAL_POW_EQ_ODD : forall n : nat, forall x : R, forall y : R, ((Coq.Arith.PeanoNat.Nat.Odd n) /\ ((Rpower_nat x n) = (Rpower_nat y n))) -> x = y.
Axiom thm_REAL_POW_EQ_EQ : forall n : nat, forall x : R, forall y : R, ((Rpower_nat x n) = (Rpower_nat y n)) = (@COND Prop (Coq.Arith.PeanoNat.Nat.Even n) ((n = (NUMERAL 0)) \/ ((Rabs x) = (Rabs y))) (x = y)).
Axiom thm_REAL_EVENPOW_ABS : forall x : R, forall n : nat, (Coq.Arith.PeanoNat.Nat.Even n) -> (Rpower_nat (Rabs x) n) = (Rpower_nat x n).
Axiom thm_REAL_OF_NUM_MOD : forall m : nat, forall n : nat, (INR (Nat.modulo m n)) = (Rminus (INR m) (Rmult (INR (Nat.div m n)) (INR n))).
Axiom thm_REAL_OF_NUM_DIV : forall m : nat, forall n : nat, (INR (Nat.div m n)) = (Rminus (Rdiv (INR m) (INR n)) (Rdiv (INR (Nat.modulo m n)) (INR n))).
Axiom thm_REAL_CONVEX_BOUND2_LT : forall (b : R), forall x : R, forall y : R, forall a : R, forall u : R, forall v : R, ((Rlt x a) /\ ((Rlt y b) /\ ((Rle (INR (NUMERAL 0)) u) /\ ((Rle (INR (NUMERAL 0)) v) /\ ((Rplus u v) = (INR (NUMERAL (BIT1 0)))))))) -> Rlt (Rplus (Rmult u x) (Rmult v y)) (Rplus (Rmult u a) (Rmult v b)).
Axiom thm_REAL_CONVEX_BOUND2_LE : forall (b : R), forall x : R, forall y : R, forall a : R, forall u : R, forall v : R, ((Rle x a) /\ ((Rle y b) /\ ((Rle (INR (NUMERAL 0)) u) /\ ((Rle (INR (NUMERAL 0)) v) /\ ((Rplus u v) = (INR (NUMERAL (BIT1 0)))))))) -> Rle (Rplus (Rmult u x) (Rmult v y)) (Rplus (Rmult u a) (Rmult v b)).
Axiom thm_REAL_CONVEX_BOUND_LT : forall x : R, forall y : R, forall a : R, forall u : R, forall v : R, ((Rlt x a) /\ ((Rlt y a) /\ ((Rle (INR (NUMERAL 0)) u) /\ ((Rle (INR (NUMERAL 0)) v) /\ ((Rplus u v) = (INR (NUMERAL (BIT1 0)))))))) -> Rlt (Rplus (Rmult u x) (Rmult v y)) a.
Axiom thm_REAL_CONVEX_BOUND_LE : forall x : R, forall y : R, forall a : R, forall u : R, forall v : R, ((Rle x a) /\ ((Rle y a) /\ ((Rle (INR (NUMERAL 0)) u) /\ ((Rle (INR (NUMERAL 0)) v) /\ ((Rplus u v) = (INR (NUMERAL (BIT1 0)))))))) -> Rle (Rplus (Rmult u x) (Rmult v y)) a.
Axiom thm_REAL_CONVEX_BOUND_GT : forall x : R, forall y : R, forall a : R, forall u : R, forall v : R, ((Rlt a x) /\ ((Rlt a y) /\ ((Rle (INR (NUMERAL 0)) u) /\ ((Rle (INR (NUMERAL 0)) v) /\ ((Rplus u v) = (INR (NUMERAL (BIT1 0)))))))) -> Rlt a (Rplus (Rmult u x) (Rmult v y)).
Axiom thm_REAL_CONVEX_BOUND_GE : forall x : R, forall y : R, forall a : R, forall u : R, forall v : R, ((Rle a x) /\ ((Rle a y) /\ ((Rle (INR (NUMERAL 0)) u) /\ ((Rle (INR (NUMERAL 0)) v) /\ ((Rplus u v) = (INR (NUMERAL (BIT1 0)))))))) -> Rle a (Rplus (Rmult u x) (Rmult v y)).
Axiom thm_REAL_CONVEX_BOUNDS_LE : forall x : R, forall y : R, forall a : R, forall b : R, forall u : R, forall v : R, ((Rle a x) /\ ((Rle x b) /\ ((Rle a y) /\ ((Rle y b) /\ ((Rle (INR (NUMERAL 0)) u) /\ ((Rle (INR (NUMERAL 0)) v) /\ ((Rplus u v) = (INR (NUMERAL (BIT1 0)))))))))) -> (Rle a (Rplus (Rmult u x) (Rmult v y))) /\ (Rle (Rplus (Rmult u x) (Rmult v y)) b).
Axiom thm_REAL_CONVEX_BOUNDS_LT : forall x : R, forall y : R, forall a : R, forall b : R, forall u : R, forall v : R, ((Rlt a x) /\ ((Rlt x b) /\ ((Rlt a y) /\ ((Rlt y b) /\ ((Rle (INR (NUMERAL 0)) u) /\ ((Rle (INR (NUMERAL 0)) v) /\ ((Rplus u v) = (INR (NUMERAL (BIT1 0)))))))))) -> (Rlt a (Rplus (Rmult u x) (Rmult v y))) /\ (Rlt (Rplus (Rmult u x) (Rmult v y)) b).
Axiom thm_REAL_ARCH_SIMPLE : forall x : R, exists n : nat, Rle x (INR n).
Axiom thm_REAL_ARCH_LT : forall x : R, exists n : nat, Rlt x (INR n).
Axiom thm_REAL_ARCH : forall x : R, (Rlt (INR (NUMERAL 0)) x) -> forall y : R, exists n : nat, Rlt y (Rmult (INR n) x).
Axiom thm_REAL_ARCH_INV : forall e : R, (Rlt (INR (NUMERAL 0)) e) = (exists n : nat, (~ (n = (NUMERAL 0))) /\ ((Rlt (INR (NUMERAL 0)) (Rinv (INR n))) /\ (Rlt (Rinv (INR n)) e))).
Axiom thm_REAL_POW_LBOUND : forall x : R, forall n : nat, (Rle (INR (NUMERAL 0)) x) -> Rle (Rplus (INR (NUMERAL (BIT1 0))) (Rmult (INR n) x)) (Rpower_nat (Rplus (INR (NUMERAL (BIT1 0))) x) n).
Axiom thm_REAL_ARCH_POW : forall x : R, forall y : R, (Rlt (INR (NUMERAL (BIT1 0))) x) -> exists n : nat, Rlt y (Rpower_nat x n).
Axiom thm_REAL_ARCH_POW2 : forall x : R, exists n : nat, Rlt x (Rpower_nat (INR (NUMERAL (BIT0 (BIT1 0)))) n).
Axiom thm_REAL_ARCH_POW_INV : forall x : R, forall y : R, ((Rlt (INR (NUMERAL 0)) y) /\ (Rlt x (INR (NUMERAL (BIT1 0))))) -> exists n : nat, Rlt (Rpower_nat x n) y.
Axiom thm_real_sgn : forall x : R, (real_sgn x) = (@COND R (Rlt (INR (NUMERAL 0)) x) (INR (NUMERAL (BIT1 0))) (@COND R (Rlt x (INR (NUMERAL 0))) (Ropp (INR (NUMERAL (BIT1 0)))) (INR (NUMERAL 0)))).
Axiom thm_REAL_SGN_0 : (real_sgn (INR (NUMERAL 0))) = (INR (NUMERAL 0)).
Axiom thm_REAL_SGN_NEG : forall x : R, (real_sgn (Ropp x)) = (Ropp (real_sgn x)).
Axiom thm_REAL_SGN_ABS : forall x : R, (Rmult (real_sgn x) (Rabs x)) = x.
Axiom thm_REAL_SGN_ABS_ALT : forall x : R, (Rmult (real_sgn x) x) = (Rabs x).
Axiom thm_REAL_EQ_SGN_ABS : forall x : R, forall y : R, (x = y) = (((real_sgn x) = (real_sgn y)) /\ ((Rabs x) = (Rabs y))).
Axiom thm_REAL_ABS_SGN : forall x : R, (Rabs (real_sgn x)) = (real_sgn (Rabs x)).
Axiom thm_REAL_SGN : forall x : R, (real_sgn x) = (Rdiv x (Rabs x)).
Axiom thm_REAL_SGN_MUL : forall x : R, forall y : R, (real_sgn (Rmult x y)) = (Rmult (real_sgn x) (real_sgn y)).
Axiom thm_REAL_SGN_INV : forall x : R, (real_sgn (Rinv x)) = (real_sgn x).
Axiom thm_REAL_SGN_DIV : forall x : R, forall y : R, (real_sgn (Rdiv x y)) = (Rdiv (real_sgn x) (real_sgn y)).
Axiom thm_REAL_SGN_EQ : (forall x : R, ((real_sgn x) = (INR (NUMERAL 0))) = (x = (INR (NUMERAL 0)))) /\ ((forall x : R, ((real_sgn x) = (INR (NUMERAL (BIT1 0)))) = (Rgt x (INR (NUMERAL 0)))) /\ (forall x : R, ((real_sgn x) = (Ropp (INR (NUMERAL (BIT1 0))))) = (Rlt x (INR (NUMERAL 0))))).
Axiom thm_REAL_SGN_CASES : forall x : R, ((real_sgn x) = (INR (NUMERAL 0))) \/ (((real_sgn x) = (INR (NUMERAL (BIT1 0)))) \/ ((real_sgn x) = (Ropp (INR (NUMERAL (BIT1 0)))))).
Axiom thm_REAL_SGN_INEQS : (forall x : R, (Rle (INR (NUMERAL 0)) (real_sgn x)) = (Rle (INR (NUMERAL 0)) x)) /\ ((forall x : R, (Rlt (INR (NUMERAL 0)) (real_sgn x)) = (Rlt (INR (NUMERAL 0)) x)) /\ ((forall x : R, (Rge (INR (NUMERAL 0)) (real_sgn x)) = (Rge (INR (NUMERAL 0)) x)) /\ ((forall x : R, (Rgt (INR (NUMERAL 0)) (real_sgn x)) = (Rgt (INR (NUMERAL 0)) x)) /\ ((forall x : R, ((INR (NUMERAL 0)) = (real_sgn x)) = ((INR (NUMERAL 0)) = x)) /\ ((forall x : R, (Rle (real_sgn x) (INR (NUMERAL 0))) = (Rle x (INR (NUMERAL 0)))) /\ ((forall x : R, (Rlt (real_sgn x) (INR (NUMERAL 0))) = (Rlt x (INR (NUMERAL 0)))) /\ ((forall x : R, (Rge (real_sgn x) (INR (NUMERAL 0))) = (Rge x (INR (NUMERAL 0)))) /\ ((forall x : R, (Rgt (real_sgn x) (INR (NUMERAL 0))) = (Rgt x (INR (NUMERAL 0)))) /\ (forall x : R, ((real_sgn x) = (INR (NUMERAL 0))) = (x = (INR (NUMERAL 0)))))))))))).
Axiom thm_REAL_SGN_POW : forall x : R, forall n : nat, (real_sgn (Rpower_nat x n)) = (Rpower_nat (real_sgn x) n).
Axiom thm_REAL_SGN_POW_2 : forall x : R, (real_sgn (Rpower_nat x (NUMERAL (BIT0 (BIT1 0))))) = (real_sgn (Rabs x)).
Axiom thm_REAL_SGN_REAL_SGN : forall x : R, (real_sgn (real_sgn x)) = (real_sgn x).
Axiom thm_REAL_INV_SGN : forall x : R, (Rinv (real_sgn x)) = (real_sgn x).
Axiom thm_REAL_SGN_EQ_INEQ : forall x : R, forall y : R, ((real_sgn x) = (real_sgn y)) = ((x = y) \/ (Rlt (Rabs (Rminus x y)) (Rmax (Rabs x) (Rabs y)))).
Axiom thm_REAL_SGNS_EQ : forall x : R, forall y : R, ((real_sgn x) = (real_sgn y)) = (((x = (INR (NUMERAL 0))) = (y = (INR (NUMERAL 0)))) /\ (((Rgt x (INR (NUMERAL 0))) = (Rgt y (INR (NUMERAL 0)))) /\ ((Rlt x (INR (NUMERAL 0))) = (Rlt y (INR (NUMERAL 0)))))).
Axiom thm_REAL_SGNS_EQ_ALT : forall x : R, forall y : R, ((real_sgn x) = (real_sgn y)) = (((x = (INR (NUMERAL 0))) -> y = (INR (NUMERAL 0))) /\ (((Rgt x (INR (NUMERAL 0))) -> Rgt y (INR (NUMERAL 0))) /\ ((Rlt x (INR (NUMERAL 0))) -> Rlt y (INR (NUMERAL 0))))).
Axiom thm_REAL_WLOG_LE : forall (P : R -> R -> Prop), ((forall x : R, forall y : R, (P x y) = (P y x)) /\ (forall x : R, forall y : R, (Rle x y) -> P x y)) -> forall x : R, forall y : R, P x y.
Axiom thm_REAL_WLOG_LT : forall (P : R -> R -> Prop), ((forall x : R, P x x) /\ ((forall x : R, forall y : R, (P x y) = (P y x)) /\ (forall x : R, forall y : R, (Rlt x y) -> P x y))) -> forall x : R, forall y : R, P x y.
Axiom thm_REAL_WLOG_LE_3 : forall P : R -> R -> R -> Prop, ((forall x : R, forall y : R, forall z : R, (P x y z) -> (P y x z) /\ (P x z y)) /\ (forall x : R, forall y : R, forall z : R, ((Rle x y) /\ (Rle y z)) -> P x y z)) -> forall x : R, forall y : R, forall z : R, P x y z.
Axiom thm_sqrt : forall x : R, (sqrt x) = (@ε R (fun y : R => ((real_sgn y) = (real_sgn x)) /\ ((Rpower_nat y (NUMERAL (BIT0 (BIT1 0)))) = (Rabs x)))).
Axiom thm_SQRT_UNIQUE : forall x : R, forall y : R, ((Rle (INR (NUMERAL 0)) y) /\ ((Rpower_nat y (NUMERAL (BIT0 (BIT1 0)))) = x)) -> (sqrt x) = y.
Axiom thm_POW_2_SQRT : forall x : R, (Rle (INR (NUMERAL 0)) x) -> (sqrt (Rpower_nat x (NUMERAL (BIT0 (BIT1 0))))) = x.
Axiom thm_SQRT_0 : (sqrt (INR (NUMERAL 0))) = (INR (NUMERAL 0)).
Axiom thm_SQRT_1 : (sqrt (INR (NUMERAL (BIT1 0)))) = (INR (NUMERAL (BIT1 0))).
Axiom thm_POW_2_SQRT_ABS : forall x : R, (sqrt (Rpower_nat x (NUMERAL (BIT0 (BIT1 0))))) = (Rabs x).
Axiom thm_SQRT_WORKS_GEN : forall x : R, ((real_sgn (sqrt x)) = (real_sgn x)) /\ ((Rpower_nat (sqrt x) (NUMERAL (BIT0 (BIT1 0)))) = (Rabs x)).
Axiom thm_SQRT_UNIQUE_GEN : forall x : R, forall y : R, (((real_sgn y) = (real_sgn x)) /\ ((Rpower_nat y (NUMERAL (BIT0 (BIT1 0)))) = (Rabs x))) -> (sqrt x) = y.
Axiom thm_SQRT_NEG : forall x : R, (sqrt (Ropp x)) = (Ropp (sqrt x)).
Axiom thm_REAL_SGN_SQRT : forall x : R, (real_sgn (sqrt x)) = (real_sgn x).
Axiom thm_SQRT_WORKS : forall x : R, (Rle (INR (NUMERAL 0)) x) -> (Rle (INR (NUMERAL 0)) (sqrt x)) /\ ((Rpower_nat (sqrt x) (NUMERAL (BIT0 (BIT1 0)))) = x).
Axiom thm_REAL_POS_EQ_SQUARE : forall x : R, (Rle (INR (NUMERAL 0)) x) = (exists y : R, (Rpower_nat y (NUMERAL (BIT0 (BIT1 0)))) = x).
Axiom thm_SQRT_POS_LE : forall x : R, (Rle (INR (NUMERAL 0)) x) -> Rle (INR (NUMERAL 0)) (sqrt x).
Axiom thm_SQRT_POW_2 : forall x : R, (Rle (INR (NUMERAL 0)) x) -> (Rpower_nat (sqrt x) (NUMERAL (BIT0 (BIT1 0)))) = x.
Axiom thm_SQRT_POW2 : forall x : R, ((Rpower_nat (sqrt x) (NUMERAL (BIT0 (BIT1 0)))) = x) = (Rle (INR (NUMERAL 0)) x).
Axiom thm_SQRT_MUL : forall x : R, forall y : R, (sqrt (Rmult x y)) = (Rmult (sqrt x) (sqrt y)).
Axiom thm_SQRT_INV : forall x : R, (sqrt (Rinv x)) = (Rinv (sqrt x)).
Axiom thm_SQRT_DIV : forall x : R, forall y : R, (sqrt (Rdiv x y)) = (Rdiv (sqrt x) (sqrt y)).
Axiom thm_SQRT_LT_0 : forall x : R, (Rlt (INR (NUMERAL 0)) (sqrt x)) = (Rlt (INR (NUMERAL 0)) x).
Axiom thm_SQRT_EQ_0 : forall x : R, ((sqrt x) = (INR (NUMERAL 0))) = (x = (INR (NUMERAL 0))).
Axiom thm_SQRT_LE_0 : forall x : R, (Rle (INR (NUMERAL 0)) (sqrt x)) = (Rle (INR (NUMERAL 0)) x).
Axiom thm_REAL_ABS_SQRT : forall x : R, (Rabs (sqrt x)) = (sqrt (Rabs x)).
Axiom thm_SQRT_MONO_LT : forall x : R, forall y : R, (Rlt x y) -> Rlt (sqrt x) (sqrt y).
Axiom thm_SQRT_MONO_LE : forall x : R, forall y : R, (Rle x y) -> Rle (sqrt x) (sqrt y).
Axiom thm_SQRT_MONO_LT_EQ : forall x : R, forall y : R, (Rlt (sqrt x) (sqrt y)) = (Rlt x y).
Axiom thm_SQRT_MONO_LE_EQ : forall x : R, forall y : R, (Rle (sqrt x) (sqrt y)) = (Rle x y).
Axiom thm_SQRT_INJ : forall x : R, forall y : R, ((sqrt x) = (sqrt y)) = (x = y).
Axiom thm_SQRT_EQ_1 : forall x : R, ((sqrt x) = (INR (NUMERAL (BIT1 0)))) = (x = (INR (NUMERAL (BIT1 0)))).
Axiom thm_SQRT_POS_LT : forall x : R, (Rlt (INR (NUMERAL 0)) x) -> Rlt (INR (NUMERAL 0)) (sqrt x).
Axiom thm_REAL_LE_LSQRT : forall x : R, forall y : R, ((Rle (INR (NUMERAL 0)) y) /\ (Rle x (Rpower_nat y (NUMERAL (BIT0 (BIT1 0)))))) -> Rle (sqrt x) y.
Axiom thm_REAL_LE_RSQRT : forall x : R, forall y : R, (Rle (Rpower_nat x (NUMERAL (BIT0 (BIT1 0)))) y) -> Rle x (sqrt y).
Axiom thm_REAL_LT_LSQRT : forall x : R, forall y : R, ((Rle (INR (NUMERAL 0)) y) /\ (Rlt x (Rpower_nat y (NUMERAL (BIT0 (BIT1 0)))))) -> Rlt (sqrt x) y.
Axiom thm_REAL_LT_RSQRT : forall x : R, forall y : R, (Rlt (Rpower_nat x (NUMERAL (BIT0 (BIT1 0)))) y) -> Rlt x (sqrt y).
Axiom thm_SQRT_EVEN_POW2 : forall n : nat, (Coq.Arith.PeanoNat.Nat.Even n) -> (sqrt (Rpower_nat (INR (NUMERAL (BIT0 (BIT1 0)))) n)) = (Rpower_nat (INR (NUMERAL (BIT0 (BIT1 0)))) (Nat.div n (NUMERAL (BIT0 (BIT1 0))))).
Axiom thm_REAL_DIV_SQRT : forall x : R, (Rle (INR (NUMERAL 0)) x) -> (Rdiv x (sqrt x)) = (sqrt x).
Axiom thm_REAL_RSQRT_LE : forall x : R, forall y : R, ((Rle (INR (NUMERAL 0)) x) /\ ((Rle (INR (NUMERAL 0)) y) /\ (Rle x (sqrt y)))) -> Rle (Rpower_nat x (NUMERAL (BIT0 (BIT1 0)))) y.
Axiom thm_REAL_LSQRT_LE : forall x : R, forall y : R, ((Rle (INR (NUMERAL 0)) x) /\ (Rle (sqrt x) y)) -> Rle x (Rpower_nat y (NUMERAL (BIT0 (BIT1 0)))).
Axiom thm_REAL_SQRT_POW_2 : forall x : R, (Rpower_nat (sqrt x) (NUMERAL (BIT0 (BIT1 0)))) = (Rabs x).
Axiom thm_REAL_ABS_LE_SQRT_POS : forall x : R, forall y : R, ((Rle (INR (NUMERAL 0)) x) /\ (Rle (INR (NUMERAL 0)) y)) -> Rle (Rabs (Rminus (sqrt x) (sqrt y))) (sqrt (Rabs (Rminus x y))).
Axiom thm_REAL_ABS_LE_SQRT : forall x : R, forall y : R, Rle (Rabs (Rminus (sqrt x) (sqrt y))) (sqrt (Rmult (INR (NUMERAL (BIT0 (BIT1 0)))) (Rabs (Rminus x y)))).
Axiom thm_DECIMAL : forall x : nat, forall y : nat, (DECIMAL x y) = (Rdiv (INR x) (INR y)).
Axiom thm_RAT_LEMMA1 : forall (x1 : R) (x2 : R) (y1 : R) (y2 : R), ((~ (y1 = (INR (NUMERAL 0)))) /\ (~ (y2 = (INR (NUMERAL 0))))) -> (Rplus (Rdiv x1 y1) (Rdiv x2 y2)) = (Rmult (Rplus (Rmult x1 y2) (Rmult x2 y1)) (Rmult (Rinv y1) (Rinv y2))).
Axiom thm_RAT_LEMMA2 : forall (x1 : R) (x2 : R) (y1 : R) (y2 : R), ((Rlt (INR (NUMERAL 0)) y1) /\ (Rlt (INR (NUMERAL 0)) y2)) -> (Rplus (Rdiv x1 y1) (Rdiv x2 y2)) = (Rmult (Rplus (Rmult x1 y2) (Rmult x2 y1)) (Rmult (Rinv y1) (Rinv y2))).
Axiom thm_RAT_LEMMA3 : forall (x1 : R) (x2 : R) (y1 : R) (y2 : R), ((Rlt (INR (NUMERAL 0)) y1) /\ (Rlt (INR (NUMERAL 0)) y2)) -> (Rminus (Rdiv x1 y1) (Rdiv x2 y2)) = (Rmult (Rminus (Rmult x1 y2) (Rmult x2 y1)) (Rmult (Rinv y1) (Rinv y2))).
Axiom thm_RAT_LEMMA4 : forall (x1 : R) (y2 : R) (x2 : R) (y1 : R), ((Rlt (INR (NUMERAL 0)) y1) /\ (Rlt (INR (NUMERAL 0)) y2)) -> (Rle (Rdiv x1 y1) (Rdiv x2 y2)) = (Rle (Rmult x1 y2) (Rmult x2 y1)).
Axiom thm_RAT_LEMMA5 : forall (x1 : R) (y2 : R) (x2 : R) (y1 : R), ((Rlt (INR (NUMERAL 0)) y1) /\ (Rlt (INR (NUMERAL 0)) y2)) -> ((Rdiv x1 y1) = (Rdiv x2 y2)) = ((Rmult x1 y2) = (Rmult x2 y1)).
Axiom thm_REAL_LE_TRANS_LE : forall x : R, forall y : R, (Rle x y) = (forall z : R, (Rle y z) -> Rle x z).
Axiom thm_REAL_LE_TRANS_LTE : forall x : R, forall y : R, (Rle x y) = (forall z : R, (Rlt y z) -> Rle x z).
Axiom thm_REAL_LE_TRANS_LT : forall x : R, forall y : R, (Rle x y) = (forall z : R, (Rlt y z) -> Rlt x z).
Axiom thm_REAL_SHRINK_RANGE : forall x : R, Rlt (Rabs (Rdiv x (Rplus (INR (NUMERAL (BIT1 0))) (Rabs x)))) (INR (NUMERAL (BIT1 0))).
Axiom thm_REAL_SHRINK_LT : forall x : R, forall y : R, (Rlt (Rdiv x (Rplus (INR (NUMERAL (BIT1 0))) (Rabs x))) (Rdiv y (Rplus (INR (NUMERAL (BIT1 0))) (Rabs y)))) = (Rlt x y).
Axiom thm_REAL_SHRINK_LE : forall x : R, forall y : R, (Rle (Rdiv x (Rplus (INR (NUMERAL (BIT1 0))) (Rabs x))) (Rdiv y (Rplus (INR (NUMERAL (BIT1 0))) (Rabs y)))) = (Rle x y).
Axiom thm_REAL_SHRINK_EQ : forall x : R, forall y : R, ((Rdiv x (Rplus (INR (NUMERAL (BIT1 0))) (Rabs x))) = (Rdiv y (Rplus (INR (NUMERAL (BIT1 0))) (Rabs y)))) = (x = y).
Axiom thm_REAL_SHRINK_GALOIS : forall x : R, forall y : R, ((Rdiv x (Rplus (INR (NUMERAL (BIT1 0))) (Rabs x))) = y) = ((Rlt (Rabs y) (INR (NUMERAL (BIT1 0)))) /\ ((Rdiv y (Rminus (INR (NUMERAL (BIT1 0))) (Rabs y))) = x)).
Axiom thm_REAL_GROW_SHRINK : forall x : R, (Rdiv (Rdiv x (Rplus (INR (NUMERAL (BIT1 0))) (Rabs x))) (Rminus (INR (NUMERAL (BIT1 0))) (Rabs (Rdiv x (Rplus (INR (NUMERAL (BIT1 0))) (Rabs x)))))) = x.
Axiom thm_REAL_SHRINK_GROW_EQ : forall x : R, ((Rdiv (Rdiv x (Rminus (INR (NUMERAL (BIT1 0))) (Rabs x))) (Rplus (INR (NUMERAL (BIT1 0))) (Rabs (Rdiv x (Rminus (INR (NUMERAL (BIT1 0))) (Rabs x)))))) = x) = (Rlt (Rabs x) (INR (NUMERAL (BIT1 0)))).
Axiom thm_REAL_SHRINK_GROW : forall x : R, (Rlt (Rabs x) (INR (NUMERAL (BIT1 0)))) -> (Rdiv (Rdiv x (Rminus (INR (NUMERAL (BIT1 0))) (Rabs x))) (Rplus (INR (NUMERAL (BIT1 0))) (Rabs (Rdiv x (Rminus (INR (NUMERAL (BIT1 0))) (Rabs x)))))) = x.
Axiom thm_integer : forall x : R, (integer x) = (exists n : nat, (Rabs x) = (INR n)).
Axiom thm_is_int : forall (x : R), (integer x) = (exists n : nat, (x = (INR n)) \/ (x = (Ropp (INR n)))).
Axiom thm_dest_int_rep : forall i : Z, exists n : nat, ((IZR i) = (INR n)) \/ ((IZR i) = (Ropp (INR n))).
Axiom thm_INTEGER_REAL_OF_INT : forall x : Z, integer (IZR x).
Axiom thm_int_eq : forall x : Z, forall y : Z, (x = y) = ((IZR x) = (IZR y)).
Axiom thm_int_le : forall x : Z, forall y : Z, (int_le x y) = (Rle (IZR x) (IZR y)).
Axiom thm_int_lt : forall x : Z, forall y : Z, (int_lt x y) = (Rlt (IZR x) (IZR y)).
Axiom thm_int_ge : forall x : Z, forall y : Z, (int_ge x y) = (Rge (IZR x) (IZR y)).
Axiom thm_int_gt : forall x : Z, forall y : Z, (int_gt x y) = (Rgt (IZR x) (IZR y)).
Axiom thm_int_of_num : forall n : nat, (int_of_num n) = (int_of_real (INR n)).
Axiom thm_int_of_num_th : forall n : nat, (IZR (int_of_num n)) = (INR n).
Axiom thm_int_neg : forall i : Z, (int_neg i) = (int_of_real (Ropp (IZR i))).
Axiom thm_int_neg_th : forall x : Z, (IZR (int_neg x)) = (Ropp (IZR x)).
Axiom thm_int_add : forall x : Z, forall y : Z, (int_add x y) = (int_of_real (Rplus (IZR x) (IZR y))).
Axiom thm_int_add_th : forall x : Z, forall y : Z, (IZR (int_add x y)) = (Rplus (IZR x) (IZR y)).
Axiom thm_int_sub : forall x : Z, forall y : Z, (int_sub x y) = (int_of_real (Rminus (IZR x) (IZR y))).
Axiom thm_int_sub_th : forall x : Z, forall y : Z, (IZR (int_sub x y)) = (Rminus (IZR x) (IZR y)).
Axiom thm_int_mul : forall x : Z, forall y : Z, (int_mul x y) = (int_of_real (Rmult (IZR x) (IZR y))).
Axiom thm_int_mul_th : forall x : Z, forall y : Z, (IZR (int_mul x y)) = (Rmult (IZR x) (IZR y)).
Axiom thm_int_abs : forall x : Z, (int_abs x) = (int_of_real (Rabs (IZR x))).
Axiom thm_int_abs_th : forall x : Z, (IZR (int_abs x)) = (Rabs (IZR x)).
Axiom thm_int_sgn : forall x : Z, (int_sgn x) = (int_of_real (real_sgn (IZR x))).
Axiom thm_int_sgn_th : forall x : Z, (IZR (int_sgn x)) = (real_sgn (IZR x)).
Axiom thm_int_max : forall x : Z, forall y : Z, (int_max x y) = (int_of_real (Rmax (IZR x) (IZR y))).
Axiom thm_int_max_th : forall x : Z, forall y : Z, (IZR (int_max x y)) = (Rmax (IZR x) (IZR y)).
Axiom thm_int_min : forall x : Z, forall y : Z, (int_min x y) = (int_of_real (Rmin (IZR x) (IZR y))).
Axiom thm_int_min_th : forall x : Z, forall y : Z, (IZR (int_min x y)) = (Rmin (IZR x) (IZR y)).
Axiom thm_int_pow : forall x : Z, forall n : nat, (int_pow x n) = (int_of_real (Rpower_nat (IZR x) n)).
Axiom thm_int_pow_th : forall x : Z, forall n : nat, (IZR (int_pow x n)) = (Rpower_nat (IZR x) n).
Axiom thm_REAL_OF_INT_CLAUSES : (forall x : Z, forall y : Z, ((IZR x) = (IZR y)) = (x = y)) /\ ((forall x : Z, forall y : Z, (Rge (IZR x) (IZR y)) = (int_ge x y)) /\ ((forall x : Z, forall y : Z, (Rgt (IZR x) (IZR y)) = (int_gt x y)) /\ ((forall x : Z, forall y : Z, (Rle (IZR x) (IZR y)) = (int_le x y)) /\ ((forall x : Z, forall y : Z, (Rlt (IZR x) (IZR y)) = (int_lt x y)) /\ ((forall x : Z, forall y : Z, (Rmax (IZR x) (IZR y)) = (IZR (int_max x y))) /\ ((forall x : Z, forall y : Z, (Rmin (IZR x) (IZR y)) = (IZR (int_min x y))) /\ ((forall n : nat, (INR n) = (IZR (int_of_num n))) /\ ((forall x : Z, (Ropp (IZR x)) = (IZR (int_neg x))) /\ ((forall x : Z, (Rabs (IZR x)) = (IZR (int_abs x))) /\ ((forall x : Z, forall y : Z, (Rmax (IZR x) (IZR y)) = (IZR (int_max x y))) /\ ((forall x : Z, forall y : Z, (Rmin (IZR x) (IZR y)) = (IZR (int_min x y))) /\ ((forall x : Z, (real_sgn (IZR x)) = (IZR (int_sgn x))) /\ ((forall x : Z, forall y : Z, (Rplus (IZR x) (IZR y)) = (IZR (int_add x y))) /\ ((forall x : Z, forall y : Z, (Rminus (IZR x) (IZR y)) = (IZR (int_sub x y))) /\ ((forall x : Z, forall y : Z, (Rmult (IZR x) (IZR y)) = (IZR (int_mul x y))) /\ (forall x : Z, forall n : nat, (Rpower_nat (IZR x) n) = (IZR (int_pow x n)))))))))))))))))).
Axiom thm_INT_IMAGE : forall x : Z, (exists n : nat, x = (int_of_num n)) \/ (exists n : nat, x = (int_neg (int_of_num n))).
Axiom thm_FORALL_INT_CASES : forall P : Z -> Prop, (forall x : Z, P x) = ((forall n : nat, P (int_of_num n)) /\ (forall n : nat, P (int_neg (int_of_num n)))).
Axiom thm_EXISTS_INT_CASES : forall P : Z -> Prop, (exists x : Z, P x) = ((exists n : nat, P (int_of_num n)) \/ (exists n : nat, P (int_neg (int_of_num n)))).
Axiom thm_INT_LT_DISCRETE : forall x : Z, forall y : Z, (int_lt x y) = (int_le (int_add x (int_of_num (NUMERAL (BIT1 0)))) y).
Axiom thm_INT_GT_DISCRETE : forall x : Z, forall y : Z, (int_gt x y) = (int_ge x (int_add y (int_of_num (NUMERAL (BIT1 0))))).
Axiom thm_INT_ABS_0 : (int_abs (int_of_num (NUMERAL 0))) = (int_of_num (NUMERAL 0)).
Axiom thm_INT_ABS_1 : (int_abs (int_of_num (NUMERAL (BIT1 0)))) = (int_of_num (NUMERAL (BIT1 0))).
Axiom thm_INT_ABS_ABS : forall x : Z, (int_abs (int_abs x)) = (int_abs x).
Axiom thm_INT_ABS_BETWEEN : forall x : Z, forall y : Z, forall d : Z, ((int_lt (int_of_num (NUMERAL 0)) d) /\ ((int_lt (int_sub x d) y) /\ (int_lt y (int_add x d)))) = (int_lt (int_abs (int_sub y x)) d).
Axiom thm_INT_ABS_BETWEEN1 : forall x : Z, forall y : Z, forall z : Z, ((int_lt x z) /\ (int_lt (int_abs (int_sub y x)) (int_sub z x))) -> int_lt y z.
Axiom thm_INT_ABS_BETWEEN2 : forall x0 : Z, forall x : Z, forall y0 : Z, forall y : Z, ((int_lt x0 y0) /\ ((int_lt (int_mul (int_of_num (NUMERAL (BIT0 (BIT1 0)))) (int_abs (int_sub x x0))) (int_sub y0 x0)) /\ (int_lt (int_mul (int_of_num (NUMERAL (BIT0 (BIT1 0)))) (int_abs (int_sub y y0))) (int_sub y0 x0)))) -> int_lt x y.
Axiom thm_INT_ABS_BOUND : forall x : Z, forall y : Z, forall d : Z, (int_lt (int_abs (int_sub x y)) d) -> int_lt y (int_add x d).
Axiom thm_INT_ABS_BOUNDS : forall x : Z, forall k : Z, (int_le (int_abs x) k) = ((int_le (int_neg k) x) /\ (int_le x k)).
Axiom thm_INT_ABS_CASES : forall x : Z, (x = (int_of_num (NUMERAL 0))) \/ (int_lt (int_of_num (NUMERAL 0)) (int_abs x)).
Axiom thm_INT_ABS_CIRCLE : forall x : Z, forall y : Z, forall h : Z, (int_lt (int_abs h) (int_sub (int_abs y) (int_abs x))) -> int_lt (int_abs (int_add x h)) (int_abs y).
Axiom thm_INT_ABS_LE : forall x : Z, int_le x (int_abs x).
Axiom thm_INT_ABS_MUL : forall x : Z, forall y : Z, (int_abs (int_mul x y)) = (int_mul (int_abs x) (int_abs y)).
Axiom thm_INT_ABS_NEG : forall x : Z, (int_abs (int_neg x)) = (int_abs x).
Axiom thm_INT_ABS_NUM : forall n : nat, (int_abs (int_of_num n)) = (int_of_num n).
Axiom thm_INT_ABS_NZ : forall x : Z, (~ (x = (int_of_num (NUMERAL 0)))) = (int_lt (int_of_num (NUMERAL 0)) (int_abs x)).
Axiom thm_INT_ABS_POS : forall x : Z, int_le (int_of_num (NUMERAL 0)) (int_abs x).
Axiom thm_INT_ABS_POW : forall x : Z, forall n : nat, (int_abs (int_pow x n)) = (int_pow (int_abs x) n).
Axiom thm_INT_ABS_REFL : forall x : Z, ((int_abs x) = x) = (int_le (int_of_num (NUMERAL 0)) x).
Axiom thm_INT_ABS_SGN : forall x : Z, (int_abs (int_sgn x)) = (int_sgn (int_abs x)).
Axiom thm_INT_ABS_SIGN : forall x : Z, forall y : Z, (int_lt (int_abs (int_sub x y)) y) -> int_lt (int_of_num (NUMERAL 0)) x.
Axiom thm_INT_ABS_SIGN2 : forall x : Z, forall y : Z, (int_lt (int_abs (int_sub x y)) (int_neg y)) -> int_lt x (int_of_num (NUMERAL 0)).
Axiom thm_INT_ABS_STILLNZ : forall x : Z, forall y : Z, (int_lt (int_abs (int_sub x y)) (int_abs y)) -> ~ (x = (int_of_num (NUMERAL 0))).
Axiom thm_INT_ABS_SUB : forall x : Z, forall y : Z, (int_abs (int_sub x y)) = (int_abs (int_sub y x)).
Axiom thm_INT_ABS_SUB_ABS : forall x : Z, forall y : Z, int_le (int_abs (int_sub (int_abs x) (int_abs y))) (int_abs (int_sub x y)).
Axiom thm_INT_ABS_TRIANGLE : forall x : Z, forall y : Z, int_le (int_abs (int_add x y)) (int_add (int_abs x) (int_abs y)).
Axiom thm_INT_ABS_ZERO : forall x : Z, ((int_abs x) = (int_of_num (NUMERAL 0))) = (x = (int_of_num (NUMERAL 0))).
Axiom thm_INT_ADD2_SUB2 : forall a : Z, forall b : Z, forall c : Z, forall d : Z, (int_sub (int_add a b) (int_add c d)) = (int_add (int_sub a c) (int_sub b d)).
Axiom thm_INT_ADD_AC : forall (n : Z) (m : Z) (p : Z), ((int_add m n) = (int_add n m)) /\ (((int_add (int_add m n) p) = (int_add m (int_add n p))) /\ ((int_add m (int_add n p)) = (int_add n (int_add m p)))).
Axiom thm_INT_ADD_ASSOC : forall x : Z, forall y : Z, forall z : Z, (int_add x (int_add y z)) = (int_add (int_add x y) z).
Axiom thm_INT_ADD_LDISTRIB : forall x : Z, forall y : Z, forall z : Z, (int_mul x (int_add y z)) = (int_add (int_mul x y) (int_mul x z)).
Axiom thm_INT_ADD_LID : forall x : Z, (int_add (int_of_num (NUMERAL 0)) x) = x.
Axiom thm_INT_ADD_LINV : forall x : Z, (int_add (int_neg x) x) = (int_of_num (NUMERAL 0)).
Axiom thm_INT_ADD_RDISTRIB : forall x : Z, forall y : Z, forall z : Z, (int_mul (int_add x y) z) = (int_add (int_mul x z) (int_mul y z)).
Axiom thm_INT_ADD_RID : forall x : Z, (int_add x (int_of_num (NUMERAL 0))) = x.
Axiom thm_INT_ADD_RINV : forall x : Z, (int_add x (int_neg x)) = (int_of_num (NUMERAL 0)).
Axiom thm_INT_ADD_SUB : forall x : Z, forall y : Z, (int_sub (int_add x y) x) = y.
Axiom thm_INT_ADD_SUB2 : forall x : Z, forall y : Z, (int_sub x (int_add x y)) = (int_neg y).
Axiom thm_INT_ADD_SYM : forall x : Z, forall y : Z, (int_add x y) = (int_add y x).
Axiom thm_INT_BOUNDS_LE : forall x : Z, forall k : Z, ((int_le (int_neg k) x) /\ (int_le x k)) = (int_le (int_abs x) k).
Axiom thm_INT_BOUNDS_LT : forall x : Z, forall k : Z, ((int_lt (int_neg k) x) /\ (int_lt x k)) = (int_lt (int_abs x) k).
Axiom thm_INT_DIFFSQ : forall x : Z, forall y : Z, (int_mul (int_add x y) (int_sub x y)) = (int_sub (int_mul x x) (int_mul y y)).
Axiom thm_INT_ENTIRE : forall x : Z, forall y : Z, ((int_mul x y) = (int_of_num (NUMERAL 0))) = ((x = (int_of_num (NUMERAL 0))) \/ (y = (int_of_num (NUMERAL 0)))).
Axiom thm_INT_EQ_ADD_LCANCEL : forall x : Z, forall y : Z, forall z : Z, ((int_add x y) = (int_add x z)) = (y = z).
Axiom thm_INT_EQ_ADD_LCANCEL_0 : forall x : Z, forall y : Z, ((int_add x y) = x) = (y = (int_of_num (NUMERAL 0))).
Axiom thm_INT_EQ_ADD_RCANCEL : forall x : Z, forall y : Z, forall z : Z, ((int_add x z) = (int_add y z)) = (x = y).
Axiom thm_INT_EQ_ADD_RCANCEL_0 : forall x : Z, forall y : Z, ((int_add x y) = y) = (x = (int_of_num (NUMERAL 0))).
Axiom thm_INT_EQ_IMP_LE : forall x : Z, forall y : Z, (x = y) -> int_le x y.
Axiom thm_INT_EQ_LCANCEL_IMP : forall x : Z, forall y : Z, forall z : Z, ((~ (z = (int_of_num (NUMERAL 0)))) /\ ((int_mul z x) = (int_mul z y))) -> x = y.
Axiom thm_INT_EQ_MUL_LCANCEL : forall x : Z, forall y : Z, forall z : Z, ((int_mul x y) = (int_mul x z)) = ((x = (int_of_num (NUMERAL 0))) \/ (y = z)).
Axiom thm_INT_EQ_MUL_RCANCEL : forall x : Z, forall y : Z, forall z : Z, ((int_mul x z) = (int_mul y z)) = ((x = y) \/ (z = (int_of_num (NUMERAL 0)))).
Axiom thm_INT_EQ_NEG2 : forall x : Z, forall y : Z, ((int_neg x) = (int_neg y)) = (x = y).
Axiom thm_INT_EQ_RCANCEL_IMP : forall x : Z, forall y : Z, forall z : Z, ((~ (z = (int_of_num (NUMERAL 0)))) /\ ((int_mul x z) = (int_mul y z))) -> x = y.
Axiom thm_INT_EQ_SGN_ABS : forall x : Z, forall y : Z, (x = y) = (((int_sgn x) = (int_sgn y)) /\ ((int_abs x) = (int_abs y))).
Axiom thm_INT_EQ_SQUARE_ABS : forall x : Z, forall y : Z, ((int_abs x) = (int_abs y)) = ((int_pow x (NUMERAL (BIT0 (BIT1 0)))) = (int_pow y (NUMERAL (BIT0 (BIT1 0))))).
Axiom thm_INT_EQ_SUB_LADD : forall x : Z, forall y : Z, forall z : Z, (x = (int_sub y z)) = ((int_add x z) = y).
Axiom thm_INT_EQ_SUB_RADD : forall x : Z, forall y : Z, forall z : Z, ((int_sub x y) = z) = (x = (int_add z y)).
Axiom thm_INT_EVENPOW_ABS : forall x : Z, forall n : nat, (Coq.Arith.PeanoNat.Nat.Even n) -> (int_pow (int_abs x) n) = (int_pow x n).
Axiom thm_INT_LET_ADD : forall x : Z, forall y : Z, ((int_le (int_of_num (NUMERAL 0)) x) /\ (int_lt (int_of_num (NUMERAL 0)) y)) -> int_lt (int_of_num (NUMERAL 0)) (int_add x y).
Axiom thm_INT_LET_ADD2 : forall w : Z, forall x : Z, forall y : Z, forall z : Z, ((int_le w x) /\ (int_lt y z)) -> int_lt (int_add w y) (int_add x z).
Axiom thm_INT_LET_ANTISYM : forall x : Z, forall y : Z, ~ ((int_le x y) /\ (int_lt y x)).
Axiom thm_INT_LET_TOTAL : forall x : Z, forall y : Z, (int_le x y) \/ (int_lt y x).
Axiom thm_INT_LET_TRANS : forall x : Z, forall y : Z, forall z : Z, ((int_le x y) /\ (int_lt y z)) -> int_lt x z.
Axiom thm_INT_LE_01 : int_le (int_of_num (NUMERAL 0)) (int_of_num (NUMERAL (BIT1 0))).
Axiom thm_INT_LE_ADD : forall x : Z, forall y : Z, ((int_le (int_of_num (NUMERAL 0)) x) /\ (int_le (int_of_num (NUMERAL 0)) y)) -> int_le (int_of_num (NUMERAL 0)) (int_add x y).
Axiom thm_INT_LE_ADD2 : forall w : Z, forall x : Z, forall y : Z, forall z : Z, ((int_le w x) /\ (int_le y z)) -> int_le (int_add w y) (int_add x z).
Axiom thm_INT_LE_ADDL : forall x : Z, forall y : Z, (int_le y (int_add x y)) = (int_le (int_of_num (NUMERAL 0)) x).
Axiom thm_INT_LE_ADDR : forall x : Z, forall y : Z, (int_le x (int_add x y)) = (int_le (int_of_num (NUMERAL 0)) y).
Axiom thm_INT_LE_ANTISYM : forall x : Z, forall y : Z, ((int_le x y) /\ (int_le y x)) = (x = y).
Axiom thm_INT_LE_DOUBLE : forall x : Z, (int_le (int_of_num (NUMERAL 0)) (int_add x x)) = (int_le (int_of_num (NUMERAL 0)) x).
Axiom thm_INT_LE_LADD : forall x : Z, forall y : Z, forall z : Z, (int_le (int_add x y) (int_add x z)) = (int_le y z).
Axiom thm_INT_LE_LADD_IMP : forall x : Z, forall y : Z, forall z : Z, (int_le y z) -> int_le (int_add x y) (int_add x z).
Axiom thm_INT_LE_LCANCEL_IMP : forall x : Z, forall y : Z, forall z : Z, ((int_lt (int_of_num (NUMERAL 0)) x) /\ (int_le (int_mul x y) (int_mul x z))) -> int_le y z.
Axiom thm_INT_LE_LMUL : forall x : Z, forall y : Z, forall z : Z, ((int_le (int_of_num (NUMERAL 0)) x) /\ (int_le y z)) -> int_le (int_mul x y) (int_mul x z).
Axiom thm_INT_LE_LMUL_EQ : forall x : Z, forall y : Z, forall z : Z, (int_lt (int_of_num (NUMERAL 0)) z) -> (int_le (int_mul z x) (int_mul z y)) = (int_le x y).
Axiom thm_INT_LE_LNEG : forall x : Z, forall y : Z, (int_le (int_neg x) y) = (int_le (int_of_num (NUMERAL 0)) (int_add x y)).
Axiom thm_INT_LE_LT : forall x : Z, forall y : Z, (int_le x y) = ((int_lt x y) \/ (x = y)).
Axiom thm_INT_LE_MAX : forall x : Z, forall y : Z, forall z : Z, (int_le z (int_max x y)) = ((int_le z x) \/ (int_le z y)).
Axiom thm_INT_LE_MIN : forall x : Z, forall y : Z, forall z : Z, (int_le z (int_min x y)) = ((int_le z x) /\ (int_le z y)).
Axiom thm_INT_LE_MUL : forall x : Z, forall y : Z, ((int_le (int_of_num (NUMERAL 0)) x) /\ (int_le (int_of_num (NUMERAL 0)) y)) -> int_le (int_of_num (NUMERAL 0)) (int_mul x y).
Axiom thm_INT_LE_MUL2 : forall w : Z, forall x : Z, forall y : Z, forall z : Z, ((int_le (int_of_num (NUMERAL 0)) w) /\ ((int_le w x) /\ ((int_le (int_of_num (NUMERAL 0)) y) /\ (int_le y z)))) -> int_le (int_mul w y) (int_mul x z).
Axiom thm_INT_LE_MUL_EQ : (forall x : Z, forall y : Z, (int_lt (int_of_num (NUMERAL 0)) x) -> (int_le (int_of_num (NUMERAL 0)) (int_mul x y)) = (int_le (int_of_num (NUMERAL 0)) y)) /\ (forall x : Z, forall y : Z, (int_lt (int_of_num (NUMERAL 0)) y) -> (int_le (int_of_num (NUMERAL 0)) (int_mul x y)) = (int_le (int_of_num (NUMERAL 0)) x)).
Axiom thm_INT_LE_NEG2 : forall x : Z, forall y : Z, (int_le (int_neg x) (int_neg y)) = (int_le y x).
Axiom thm_INT_LE_NEGL : forall x : Z, (int_le (int_neg x) x) = (int_le (int_of_num (NUMERAL 0)) x).
Axiom thm_INT_LE_NEGR : forall x : Z, (int_le x (int_neg x)) = (int_le x (int_of_num (NUMERAL 0))).
Axiom thm_INT_LE_NEGTOTAL : forall x : Z, (int_le (int_of_num (NUMERAL 0)) x) \/ (int_le (int_of_num (NUMERAL 0)) (int_neg x)).
Axiom thm_INT_LE_POW2 : forall n : nat, int_le (int_of_num (NUMERAL (BIT1 0))) (int_pow (int_of_num (NUMERAL (BIT0 (BIT1 0)))) n).
Axiom thm_INT_LE_POW_2 : forall x : Z, int_le (int_of_num (NUMERAL 0)) (int_pow x (NUMERAL (BIT0 (BIT1 0)))).
Axiom thm_INT_LE_RADD : forall x : Z, forall y : Z, forall z : Z, (int_le (int_add x z) (int_add y z)) = (int_le x y).
Axiom thm_INT_LE_RCANCEL_IMP : forall x : Z, forall y : Z, forall z : Z, ((int_lt (int_of_num (NUMERAL 0)) z) /\ (int_le (int_mul x z) (int_mul y z))) -> int_le x y.
Axiom thm_INT_LE_REFL : forall x : Z, int_le x x.
Axiom thm_INT_LE_RMUL : forall x : Z, forall y : Z, forall z : Z, ((int_le x y) /\ (int_le (int_of_num (NUMERAL 0)) z)) -> int_le (int_mul x z) (int_mul y z).
Axiom thm_INT_LE_RMUL_EQ : forall x : Z, forall y : Z, forall z : Z, (int_lt (int_of_num (NUMERAL 0)) z) -> (int_le (int_mul x z) (int_mul y z)) = (int_le x y).
Axiom thm_INT_LE_RNEG : forall x : Z, forall y : Z, (int_le x (int_neg y)) = (int_le (int_add x y) (int_of_num (NUMERAL 0))).
Axiom thm_INT_LE_SQUARE : forall x : Z, int_le (int_of_num (NUMERAL 0)) (int_mul x x).
Axiom thm_INT_LE_SQUARE_ABS : forall x : Z, forall y : Z, (int_le (int_abs x) (int_abs y)) = (int_le (int_pow x (NUMERAL (BIT0 (BIT1 0)))) (int_pow y (NUMERAL (BIT0 (BIT1 0))))).
Axiom thm_INT_LE_SUB_LADD : forall x : Z, forall y : Z, forall z : Z, (int_le x (int_sub y z)) = (int_le (int_add x z) y).
Axiom thm_INT_LE_SUB_RADD : forall x : Z, forall y : Z, forall z : Z, (int_le (int_sub x y) z) = (int_le x (int_add z y)).
Axiom thm_INT_LE_TOTAL : forall x : Z, forall y : Z, (int_le x y) \/ (int_le y x).
Axiom thm_INT_LE_TRANS : forall x : Z, forall y : Z, forall z : Z, ((int_le x y) /\ (int_le y z)) -> int_le x z.
Axiom thm_INT_LNEG_UNIQ : forall x : Z, forall y : Z, ((int_add x y) = (int_of_num (NUMERAL 0))) = (x = (int_neg y)).
Axiom thm_INT_LTE_ADD : forall x : Z, forall y : Z, ((int_lt (int_of_num (NUMERAL 0)) x) /\ (int_le (int_of_num (NUMERAL 0)) y)) -> int_lt (int_of_num (NUMERAL 0)) (int_add x y).
Axiom thm_INT_LTE_ADD2 : forall w : Z, forall x : Z, forall y : Z, forall z : Z, ((int_lt w x) /\ (int_le y z)) -> int_lt (int_add w y) (int_add x z).
Axiom thm_INT_LTE_ANTISYM : forall x : Z, forall y : Z, ~ ((int_lt x y) /\ (int_le y x)).
Axiom thm_INT_LTE_TOTAL : forall x : Z, forall y : Z, (int_lt x y) \/ (int_le y x).
Axiom thm_INT_LTE_TRANS : forall x : Z, forall y : Z, forall z : Z, ((int_lt x y) /\ (int_le y z)) -> int_lt x z.
Axiom thm_INT_LT_01 : int_lt (int_of_num (NUMERAL 0)) (int_of_num (NUMERAL (BIT1 0))).
Axiom thm_INT_LT_ADD : forall x : Z, forall y : Z, ((int_lt (int_of_num (NUMERAL 0)) x) /\ (int_lt (int_of_num (NUMERAL 0)) y)) -> int_lt (int_of_num (NUMERAL 0)) (int_add x y).
Axiom thm_INT_LT_ADD1 : forall x : Z, forall y : Z, (int_le x y) -> int_lt x (int_add y (int_of_num (NUMERAL (BIT1 0)))).
Axiom thm_INT_LT_ADD2 : forall w : Z, forall x : Z, forall y : Z, forall z : Z, ((int_lt w x) /\ (int_lt y z)) -> int_lt (int_add w y) (int_add x z).
Axiom thm_INT_LT_ADDL : forall x : Z, forall y : Z, (int_lt y (int_add x y)) = (int_lt (int_of_num (NUMERAL 0)) x).
Axiom thm_INT_LT_ADDNEG : forall x : Z, forall y : Z, forall z : Z, (int_lt y (int_add x (int_neg z))) = (int_lt (int_add y z) x).
Axiom thm_INT_LT_ADDNEG2 : forall x : Z, forall y : Z, forall z : Z, (int_lt (int_add x (int_neg y)) z) = (int_lt x (int_add z y)).
Axiom thm_INT_LT_ADDR : forall x : Z, forall y : Z, (int_lt x (int_add x y)) = (int_lt (int_of_num (NUMERAL 0)) y).
Axiom thm_INT_LT_ADD_SUB : forall x : Z, forall y : Z, forall z : Z, (int_lt (int_add x y) z) = (int_lt x (int_sub z y)).
Axiom thm_INT_LT_ANTISYM : forall x : Z, forall y : Z, ~ ((int_lt x y) /\ (int_lt y x)).
Axiom thm_INT_LT_GT : forall x : Z, forall y : Z, (int_lt x y) -> ~ (int_lt y x).
Axiom thm_INT_LT_IMP_LE : forall x : Z, forall y : Z, (int_lt x y) -> int_le x y.
Axiom thm_INT_LT_IMP_NE : forall x : Z, forall y : Z, (int_lt x y) -> ~ (x = y).
Axiom thm_INT_LT_LADD : forall x : Z, forall y : Z, forall z : Z, (int_lt (int_add x y) (int_add x z)) = (int_lt y z).
Axiom thm_INT_LT_LADD_IMP : forall x : Z, forall y : Z, forall z : Z, (int_lt y z) -> int_lt (int_add x y) (int_add x z).
Axiom thm_INT_LT_LCANCEL_IMP : forall x : Z, forall y : Z, forall z : Z, ((int_lt (int_of_num (NUMERAL 0)) x) /\ (int_lt (int_mul x y) (int_mul x z))) -> int_lt y z.
Axiom thm_INT_LT_LE : forall x : Z, forall y : Z, (int_lt x y) = ((int_le x y) /\ (~ (x = y))).
Axiom thm_INT_LT_LMUL : forall x : Z, forall y : Z, forall z : Z, ((int_lt (int_of_num (NUMERAL 0)) x) /\ (int_lt y z)) -> int_lt (int_mul x y) (int_mul x z).
Axiom thm_INT_LT_LMUL_EQ : forall x : Z, forall y : Z, forall z : Z, (int_lt (int_of_num (NUMERAL 0)) z) -> (int_lt (int_mul z x) (int_mul z y)) = (int_lt x y).
Axiom thm_INT_LT_LNEG : forall x : Z, forall y : Z, (int_lt (int_neg x) y) = (int_lt (int_of_num (NUMERAL 0)) (int_add x y)).
Axiom thm_INT_LT_MAX : forall x : Z, forall y : Z, forall z : Z, (int_lt z (int_max x y)) = ((int_lt z x) \/ (int_lt z y)).
Axiom thm_INT_LT_MIN : forall x : Z, forall y : Z, forall z : Z, (int_lt z (int_min x y)) = ((int_lt z x) /\ (int_lt z y)).
Axiom thm_INT_LT_MUL : forall x : Z, forall y : Z, ((int_lt (int_of_num (NUMERAL 0)) x) /\ (int_lt (int_of_num (NUMERAL 0)) y)) -> int_lt (int_of_num (NUMERAL 0)) (int_mul x y).
Axiom thm_INT_LT_MUL2 : forall w : Z, forall x : Z, forall y : Z, forall z : Z, ((int_le (int_of_num (NUMERAL 0)) w) /\ ((int_lt w x) /\ ((int_le (int_of_num (NUMERAL 0)) y) /\ (int_lt y z)))) -> int_lt (int_mul w y) (int_mul x z).
Axiom thm_INT_LT_MUL_EQ : (forall x : Z, forall y : Z, (int_lt (int_of_num (NUMERAL 0)) x) -> (int_lt (int_of_num (NUMERAL 0)) (int_mul x y)) = (int_lt (int_of_num (NUMERAL 0)) y)) /\ (forall x : Z, forall y : Z, (int_lt (int_of_num (NUMERAL 0)) y) -> (int_lt (int_of_num (NUMERAL 0)) (int_mul x y)) = (int_lt (int_of_num (NUMERAL 0)) x)).
Axiom thm_INT_LT_NEG2 : forall x : Z, forall y : Z, (int_lt (int_neg x) (int_neg y)) = (int_lt y x).
Axiom thm_INT_LT_NEGTOTAL : forall x : Z, (x = (int_of_num (NUMERAL 0))) \/ ((int_lt (int_of_num (NUMERAL 0)) x) \/ (int_lt (int_of_num (NUMERAL 0)) (int_neg x))).
Axiom thm_INT_LT_POW2 : forall n : nat, int_lt (int_of_num (NUMERAL 0)) (int_pow (int_of_num (NUMERAL (BIT0 (BIT1 0)))) n).
Axiom thm_INT_LT_POW_2 : forall x : Z, (int_lt (int_of_num (NUMERAL 0)) (int_pow x (NUMERAL (BIT0 (BIT1 0))))) = (~ (x = (int_of_num (NUMERAL 0)))).
Axiom thm_INT_LT_RADD : forall x : Z, forall y : Z, forall z : Z, (int_lt (int_add x z) (int_add y z)) = (int_lt x y).
Axiom thm_INT_LT_RCANCEL_IMP : forall x : Z, forall y : Z, forall z : Z, ((int_lt (int_of_num (NUMERAL 0)) z) /\ (int_lt (int_mul x z) (int_mul y z))) -> int_lt x y.
Axiom thm_INT_LT_REFL : forall x : Z, ~ (int_lt x x).
Axiom thm_INT_LT_RMUL : forall x : Z, forall y : Z, forall z : Z, ((int_lt x y) /\ (int_lt (int_of_num (NUMERAL 0)) z)) -> int_lt (int_mul x z) (int_mul y z).
Axiom thm_INT_LT_RMUL_EQ : forall x : Z, forall y : Z, forall z : Z, (int_lt (int_of_num (NUMERAL 0)) z) -> (int_lt (int_mul x z) (int_mul y z)) = (int_lt x y).
Axiom thm_INT_LT_RNEG : forall x : Z, forall y : Z, (int_lt x (int_neg y)) = (int_lt (int_add x y) (int_of_num (NUMERAL 0))).
Axiom thm_INT_LT_SQUARE : forall x : Z, (int_lt (int_of_num (NUMERAL 0)) (int_mul x x)) = (~ (x = (int_of_num (NUMERAL 0)))).
Axiom thm_INT_LT_SQUARE_ABS : forall x : Z, forall y : Z, (int_lt (int_abs x) (int_abs y)) = (int_lt (int_pow x (NUMERAL (BIT0 (BIT1 0)))) (int_pow y (NUMERAL (BIT0 (BIT1 0))))).
Axiom thm_INT_LT_SUB_LADD : forall x : Z, forall y : Z, forall z : Z, (int_lt x (int_sub y z)) = (int_lt (int_add x z) y).
Axiom thm_INT_LT_SUB_RADD : forall x : Z, forall y : Z, forall z : Z, (int_lt (int_sub x y) z) = (int_lt x (int_add z y)).
Axiom thm_INT_LT_TOTAL : forall x : Z, forall y : Z, (x = y) \/ ((int_lt x y) \/ (int_lt y x)).
Axiom thm_INT_LT_TRANS : forall x : Z, forall y : Z, forall z : Z, ((int_lt x y) /\ (int_lt y z)) -> int_lt x z.
Axiom thm_INT_MAX_ACI : forall (z : Z) (x : Z) (y : Z), ((int_max x y) = (int_max y x)) /\ (((int_max (int_max x y) z) = (int_max x (int_max y z))) /\ (((int_max x (int_max y z)) = (int_max y (int_max x z))) /\ (((int_max x x) = x) /\ ((int_max x (int_max x y)) = (int_max x y))))).
Axiom thm_INT_MAX_ASSOC : forall x : Z, forall y : Z, forall z : Z, (int_max x (int_max y z)) = (int_max (int_max x y) z).
Axiom thm_INT_MAX_LE : forall x : Z, forall y : Z, forall z : Z, (int_le (int_max x y) z) = ((int_le x z) /\ (int_le y z)).
Axiom thm_INT_MAX_LT : forall x : Z, forall y : Z, forall z : Z, (int_lt (int_max x y) z) = ((int_lt x z) /\ (int_lt y z)).
Axiom thm_INT_MAX_MAX : forall x : Z, forall y : Z, (int_le x (int_max x y)) /\ (int_le y (int_max x y)).
Axiom thm_INT_MAX_MIN : forall x : Z, forall y : Z, (int_max x y) = (int_neg (int_min (int_neg x) (int_neg y))).
Axiom thm_INT_MAX_SYM : forall x : Z, forall y : Z, (int_max x y) = (int_max y x).
Axiom thm_INT_MIN_ACI : forall (z : Z) (x : Z) (y : Z), ((int_min x y) = (int_min y x)) /\ (((int_min (int_min x y) z) = (int_min x (int_min y z))) /\ (((int_min x (int_min y z)) = (int_min y (int_min x z))) /\ (((int_min x x) = x) /\ ((int_min x (int_min x y)) = (int_min x y))))).
Axiom thm_INT_MIN_ASSOC : forall x : Z, forall y : Z, forall z : Z, (int_min x (int_min y z)) = (int_min (int_min x y) z).
Axiom thm_INT_MIN_LE : forall x : Z, forall y : Z, forall z : Z, (int_le (int_min x y) z) = ((int_le x z) \/ (int_le y z)).
Axiom thm_INT_MIN_LT : forall x : Z, forall y : Z, forall z : Z, (int_lt (int_min x y) z) = ((int_lt x z) \/ (int_lt y z)).
Axiom thm_INT_MIN_MAX : forall x : Z, forall y : Z, (int_min x y) = (int_neg (int_max (int_neg x) (int_neg y))).
Axiom thm_INT_MIN_MIN : forall x : Z, forall y : Z, (int_le (int_min x y) x) /\ (int_le (int_min x y) y).
Axiom thm_INT_MIN_SYM : forall x : Z, forall y : Z, (int_min x y) = (int_min y x).
Axiom thm_INT_MUL_2 : forall x : Z, (int_mul (int_of_num (NUMERAL (BIT0 (BIT1 0)))) x) = (int_add x x).
Axiom thm_INT_MUL_AC : forall (n : Z) (m : Z) (p : Z), ((int_mul m n) = (int_mul n m)) /\ (((int_mul (int_mul m n) p) = (int_mul m (int_mul n p))) /\ ((int_mul m (int_mul n p)) = (int_mul n (int_mul m p)))).
Axiom thm_INT_MUL_ASSOC : forall x : Z, forall y : Z, forall z : Z, (int_mul x (int_mul y z)) = (int_mul (int_mul x y) z).
Axiom thm_INT_MUL_LID : forall x : Z, (int_mul (int_of_num (NUMERAL (BIT1 0))) x) = x.
Axiom thm_INT_MUL_LNEG : forall x : Z, forall y : Z, (int_mul (int_neg x) y) = (int_neg (int_mul x y)).
Axiom thm_INT_MUL_LZERO : forall x : Z, (int_mul (int_of_num (NUMERAL 0)) x) = (int_of_num (NUMERAL 0)).
Axiom thm_INT_MUL_POS_LE : forall x : Z, forall y : Z, (int_le (int_of_num (NUMERAL 0)) (int_mul x y)) = ((x = (int_of_num (NUMERAL 0))) \/ ((y = (int_of_num (NUMERAL 0))) \/ (((int_lt (int_of_num (NUMERAL 0)) x) /\ (int_lt (int_of_num (NUMERAL 0)) y)) \/ ((int_lt x (int_of_num (NUMERAL 0))) /\ (int_lt y (int_of_num (NUMERAL 0))))))).
Axiom thm_INT_MUL_POS_LT : forall x : Z, forall y : Z, (int_lt (int_of_num (NUMERAL 0)) (int_mul x y)) = (((int_lt (int_of_num (NUMERAL 0)) x) /\ (int_lt (int_of_num (NUMERAL 0)) y)) \/ ((int_lt x (int_of_num (NUMERAL 0))) /\ (int_lt y (int_of_num (NUMERAL 0))))).
Axiom thm_INT_MUL_RID : forall x : Z, (int_mul x (int_of_num (NUMERAL (BIT1 0)))) = x.
Axiom thm_INT_MUL_RNEG : forall x : Z, forall y : Z, (int_mul x (int_neg y)) = (int_neg (int_mul x y)).
Axiom thm_INT_MUL_RZERO : forall x : Z, (int_mul x (int_of_num (NUMERAL 0))) = (int_of_num (NUMERAL 0)).
Axiom thm_INT_MUL_SYM : forall x : Z, forall y : Z, (int_mul x y) = (int_mul y x).
Axiom thm_INT_NEG_0 : (int_neg (int_of_num (NUMERAL 0))) = (int_of_num (NUMERAL 0)).
Axiom thm_INT_NEG_ADD : forall x : Z, forall y : Z, (int_neg (int_add x y)) = (int_add (int_neg x) (int_neg y)).
Axiom thm_INT_NEG_EQ : forall x : Z, forall y : Z, ((int_neg x) = y) = (x = (int_neg y)).
Axiom thm_INT_NEG_EQ_0 : forall x : Z, ((int_neg x) = (int_of_num (NUMERAL 0))) = (x = (int_of_num (NUMERAL 0))).
Axiom thm_INT_NEG_GE0 : forall x : Z, (int_le (int_of_num (NUMERAL 0)) (int_neg x)) = (int_le x (int_of_num (NUMERAL 0))).
Axiom thm_INT_NEG_GT0 : forall x : Z, (int_lt (int_of_num (NUMERAL 0)) (int_neg x)) = (int_lt x (int_of_num (NUMERAL 0))).
Axiom thm_INT_NEG_LE0 : forall x : Z, (int_le (int_neg x) (int_of_num (NUMERAL 0))) = (int_le (int_of_num (NUMERAL 0)) x).
Axiom thm_INT_NEG_LMUL : forall x : Z, forall y : Z, (int_neg (int_mul x y)) = (int_mul (int_neg x) y).
Axiom thm_INT_NEG_LT0 : forall x : Z, (int_lt (int_neg x) (int_of_num (NUMERAL 0))) = (int_lt (int_of_num (NUMERAL 0)) x).
Axiom thm_INT_NEG_MINUS1 : forall x : Z, (int_neg x) = (int_mul (int_neg (int_of_num (NUMERAL (BIT1 0)))) x).
Axiom thm_INT_NEG_MUL2 : forall x : Z, forall y : Z, (int_mul (int_neg x) (int_neg y)) = (int_mul x y).
Axiom thm_INT_NEG_NEG : forall x : Z, (int_neg (int_neg x)) = x.
Axiom thm_INT_NEG_RMUL : forall x : Z, forall y : Z, (int_neg (int_mul x y)) = (int_mul x (int_neg y)).
Axiom thm_INT_NEG_SUB : forall x : Z, forall y : Z, (int_neg (int_sub x y)) = (int_sub y x).
Axiom thm_INT_NOT_EQ : forall x : Z, forall y : Z, (~ (x = y)) = ((int_lt x y) \/ (int_lt y x)).
Axiom thm_INT_NOT_LE : forall x : Z, forall y : Z, (~ (int_le x y)) = (int_lt y x).
Axiom thm_INT_NOT_LT : forall x : Z, forall y : Z, (~ (int_lt x y)) = (int_le y x).
Axiom thm_INT_OF_NUM_ADD : forall m : nat, forall n : nat, (int_add (int_of_num m) (int_of_num n)) = (int_of_num (Nat.add m n)).
Axiom thm_INT_OF_NUM_CLAUSES : (forall m : nat, forall n : nat, ((int_of_num m) = (int_of_num n)) = (m = n)) /\ ((forall m : nat, forall n : nat, (int_ge (int_of_num m) (int_of_num n)) = (Peano.ge m n)) /\ ((forall m : nat, forall n : nat, (int_gt (int_of_num m) (int_of_num n)) = (Peano.gt m n)) /\ ((forall m : nat, forall n : nat, (int_le (int_of_num m) (int_of_num n)) = (Peano.le m n)) /\ ((forall m : nat, forall n : nat, (int_lt (int_of_num m) (int_of_num n)) = (Peano.lt m n)) /\ ((forall m : nat, forall n : nat, (int_max (int_of_num m) (int_of_num n)) = (int_of_num (Nat.max m n))) /\ ((forall m : nat, forall n : nat, (int_min (int_of_num m) (int_of_num n)) = (int_of_num (Nat.min m n))) /\ ((forall m : nat, forall n : nat, (int_add (int_of_num m) (int_of_num n)) = (int_of_num (Nat.add m n))) /\ ((forall m : nat, forall n : nat, (int_mul (int_of_num m) (int_of_num n)) = (int_of_num (Nat.mul m n))) /\ (forall x : nat, forall n : nat, (int_pow (int_of_num x) n) = (int_of_num (Nat.pow x n))))))))))).
Axiom thm_INT_OF_NUM_EQ : forall m : nat, forall n : nat, ((int_of_num m) = (int_of_num n)) = (m = n).
Axiom thm_INT_OF_NUM_GE : forall m : nat, forall n : nat, (int_ge (int_of_num m) (int_of_num n)) = (Peano.ge m n).
Axiom thm_INT_OF_NUM_GT : forall m : nat, forall n : nat, (int_gt (int_of_num m) (int_of_num n)) = (Peano.gt m n).
Axiom thm_INT_OF_NUM_LE : forall m : nat, forall n : nat, (int_le (int_of_num m) (int_of_num n)) = (Peano.le m n).
Axiom thm_INT_OF_NUM_LT : forall m : nat, forall n : nat, (int_lt (int_of_num m) (int_of_num n)) = (Peano.lt m n).
Axiom thm_INT_OF_NUM_MAX : forall m : nat, forall n : nat, (int_max (int_of_num m) (int_of_num n)) = (int_of_num (Nat.max m n)).
Axiom thm_INT_OF_NUM_MIN : forall m : nat, forall n : nat, (int_min (int_of_num m) (int_of_num n)) = (int_of_num (Nat.min m n)).
Axiom thm_INT_OF_NUM_MOD : forall m : nat, forall n : nat, (int_of_num (Nat.modulo m n)) = (int_sub (int_of_num m) (int_mul (int_of_num (Nat.div m n)) (int_of_num n))).
Axiom thm_INT_OF_NUM_MUL : forall m : nat, forall n : nat, (int_mul (int_of_num m) (int_of_num n)) = (int_of_num (Nat.mul m n)).
Axiom thm_INT_OF_NUM_POW : forall x : nat, forall n : nat, (int_pow (int_of_num x) n) = (int_of_num (Nat.pow x n)).
Axiom thm_INT_OF_NUM_SUB : forall m : nat, forall n : nat, (Peano.le m n) -> (int_sub (int_of_num n) (int_of_num m)) = (int_of_num (Nat.sub n m)).
Axiom thm_INT_OF_NUM_SUB_CASES : forall m : nat, forall n : nat, (int_sub (int_of_num m) (int_of_num n)) = (@COND Z (Peano.le n m) (int_of_num (Nat.sub m n)) (int_neg (int_of_num (Nat.sub n m)))).
Axiom thm_INT_OF_NUM_SUC : forall n : nat, (int_add (int_of_num n) (int_of_num (NUMERAL (BIT1 0)))) = (int_of_num (S n)).
Axiom thm_INT_POS : forall n : nat, int_le (int_of_num (NUMERAL 0)) (int_of_num n).
Axiom thm_INT_POS_EQ_SQUARE : forall x : Z, (int_le (int_of_num (NUMERAL 0)) x) = (exists y : R, (Rpower_nat y (NUMERAL (BIT0 (BIT1 0)))) = (IZR x)).
Axiom thm_INT_POS_NZ : forall x : Z, (int_lt (int_of_num (NUMERAL 0)) x) -> ~ (x = (int_of_num (NUMERAL 0))).
Axiom thm_INT_POW2_ABS : forall x : Z, (int_pow (int_abs x) (NUMERAL (BIT0 (BIT1 0)))) = (int_pow x (NUMERAL (BIT0 (BIT1 0)))).
Axiom thm_INT_POW_1 : forall x : Z, (int_pow x (NUMERAL (BIT1 0))) = x.
Axiom thm_INT_POW_1_LE : forall n : nat, forall x : Z, ((int_le (int_of_num (NUMERAL 0)) x) /\ (int_le x (int_of_num (NUMERAL (BIT1 0))))) -> int_le (int_pow x n) (int_of_num (NUMERAL (BIT1 0))).
Axiom thm_INT_POW_1_LT : forall n : nat, forall x : Z, ((~ (n = (NUMERAL 0))) /\ ((int_le (int_of_num (NUMERAL 0)) x) /\ (int_lt x (int_of_num (NUMERAL (BIT1 0)))))) -> int_lt (int_pow x n) (int_of_num (NUMERAL (BIT1 0))).
Axiom thm_INT_POW_2 : forall x : Z, (int_pow x (NUMERAL (BIT0 (BIT1 0)))) = (int_mul x x).
Axiom thm_INT_POW_ADD : forall x : Z, forall m : nat, forall n : nat, (int_pow x (Nat.add m n)) = (int_mul (int_pow x m) (int_pow x n)).
Axiom thm_INT_POW_EQ : forall n : nat, forall x : Z, forall y : Z, ((~ (n = (NUMERAL 0))) /\ ((int_le (int_of_num (NUMERAL 0)) x) /\ ((int_le (int_of_num (NUMERAL 0)) y) /\ ((int_pow x n) = (int_pow y n))))) -> x = y.
Axiom thm_INT_POW_EQ_0 : forall x : Z, forall n : nat, ((int_pow x n) = (int_of_num (NUMERAL 0))) = ((x = (int_of_num (NUMERAL 0))) /\ (~ (n = (NUMERAL 0)))).
Axiom thm_INT_POW_EQ_1 : forall x : Z, forall n : nat, ((int_pow x n) = (int_of_num (NUMERAL (BIT1 0)))) = ((((int_abs x) = (int_of_num (NUMERAL (BIT1 0)))) /\ ((int_lt x (int_of_num (NUMERAL 0))) -> Coq.Arith.PeanoNat.Nat.Even n)) \/ (n = (NUMERAL 0))).
Axiom thm_INT_POW_EQ_1_IMP : forall x : Z, forall n : nat, ((~ (n = (NUMERAL 0))) /\ ((int_pow x n) = (int_of_num (NUMERAL (BIT1 0))))) -> (int_abs x) = (int_of_num (NUMERAL (BIT1 0))).
Axiom thm_INT_POW_EQ_ABS : forall n : nat, forall x : Z, forall y : Z, ((~ (n = (NUMERAL 0))) /\ ((int_pow x n) = (int_pow y n))) -> (int_abs x) = (int_abs y).
Axiom thm_INT_POW_EQ_EQ : forall n : nat, forall x : Z, forall y : Z, ((int_pow x n) = (int_pow y n)) = (@COND Prop (Coq.Arith.PeanoNat.Nat.Even n) ((n = (NUMERAL 0)) \/ ((int_abs x) = (int_abs y))) (x = y)).
Axiom thm_INT_POW_EQ_ODD : forall n : nat, forall x : Z, forall y : Z, ((Coq.Arith.PeanoNat.Nat.Odd n) /\ ((int_pow x n) = (int_pow y n))) -> x = y.
Axiom thm_INT_POW_EQ_ODD_EQ : forall n : nat, forall x : Z, forall y : Z, (Coq.Arith.PeanoNat.Nat.Odd n) -> ((int_pow x n) = (int_pow y n)) = (x = y).
Axiom thm_INT_POW_LBOUND : forall x : Z, forall n : nat, (int_le (int_of_num (NUMERAL 0)) x) -> int_le (int_add (int_of_num (NUMERAL (BIT1 0))) (int_mul (int_of_num n) x)) (int_pow (int_add (int_of_num (NUMERAL (BIT1 0))) x) n).
Axiom thm_INT_POW_LE : forall x : Z, forall n : nat, (int_le (int_of_num (NUMERAL 0)) x) -> int_le (int_of_num (NUMERAL 0)) (int_pow x n).
Axiom thm_INT_POW_LE2 : forall n : nat, forall x : Z, forall y : Z, ((int_le (int_of_num (NUMERAL 0)) x) /\ (int_le x y)) -> int_le (int_pow x n) (int_pow y n).
Axiom thm_INT_POW_LE2_ODD : forall n : nat, forall x : Z, forall y : Z, ((int_le x y) /\ (Coq.Arith.PeanoNat.Nat.Odd n)) -> int_le (int_pow x n) (int_pow y n).
Axiom thm_INT_POW_LE2_ODD_EQ : forall n : nat, forall x : Z, forall y : Z, (Coq.Arith.PeanoNat.Nat.Odd n) -> (int_le (int_pow x n) (int_pow y n)) = (int_le x y).
Axiom thm_INT_POW_LE2_REV : forall n : nat, forall x : Z, forall y : Z, ((~ (n = (NUMERAL 0))) /\ ((int_le (int_of_num (NUMERAL 0)) y) /\ (int_le (int_pow x n) (int_pow y n)))) -> int_le x y.
Axiom thm_INT_POW_LE_1 : forall n : nat, forall x : Z, (int_le (int_of_num (NUMERAL (BIT1 0))) x) -> int_le (int_of_num (NUMERAL (BIT1 0))) (int_pow x n).
Axiom thm_INT_POW_LT : forall x : Z, forall n : nat, (int_lt (int_of_num (NUMERAL 0)) x) -> int_lt (int_of_num (NUMERAL 0)) (int_pow x n).
Axiom thm_INT_POW_LT2 : forall n : nat, forall x : Z, forall y : Z, ((~ (n = (NUMERAL 0))) /\ ((int_le (int_of_num (NUMERAL 0)) x) /\ (int_lt x y))) -> int_lt (int_pow x n) (int_pow y n).
Axiom thm_INT_POW_LT2_ODD : forall n : nat, forall x : Z, forall y : Z, ((int_lt x y) /\ (Coq.Arith.PeanoNat.Nat.Odd n)) -> int_lt (int_pow x n) (int_pow y n).
Axiom thm_INT_POW_LT2_ODD_EQ : forall n : nat, forall x : Z, forall y : Z, (Coq.Arith.PeanoNat.Nat.Odd n) -> (int_lt (int_pow x n) (int_pow y n)) = (int_lt x y).
Axiom thm_INT_POW_LT2_REV : forall n : nat, forall x : Z, forall y : Z, ((int_le (int_of_num (NUMERAL 0)) y) /\ (int_lt (int_pow x n) (int_pow y n))) -> int_lt x y.
Axiom thm_INT_POW_LT_1 : forall n : nat, forall x : Z, ((~ (n = (NUMERAL 0))) /\ (int_lt (int_of_num (NUMERAL (BIT1 0))) x)) -> int_lt (int_of_num (NUMERAL (BIT1 0))) (int_pow x n).
Axiom thm_INT_POW_MONO : forall m : nat, forall n : nat, forall x : Z, ((int_le (int_of_num (NUMERAL (BIT1 0))) x) /\ (Peano.le m n)) -> int_le (int_pow x m) (int_pow x n).
Axiom thm_INT_POW_MONO_LT : forall m : nat, forall n : nat, forall x : Z, ((int_lt (int_of_num (NUMERAL (BIT1 0))) x) /\ (Peano.lt m n)) -> int_lt (int_pow x m) (int_pow x n).
Axiom thm_INT_POW_MUL : forall x : Z, forall y : Z, forall n : nat, (int_pow (int_mul x y) n) = (int_mul (int_pow x n) (int_pow y n)).
Axiom thm_INT_POW_NEG : forall x : Z, forall n : nat, (int_pow (int_neg x) n) = (@COND Z (Coq.Arith.PeanoNat.Nat.Even n) (int_pow x n) (int_neg (int_pow x n))).
Axiom thm_INT_POW_NZ : forall x : Z, forall n : nat, (~ (x = (int_of_num (NUMERAL 0)))) -> ~ ((int_pow x n) = (int_of_num (NUMERAL 0))).
Axiom thm_INT_POW_ONE : forall n : nat, (int_pow (int_of_num (NUMERAL (BIT1 0))) n) = (int_of_num (NUMERAL (BIT1 0))).
Axiom thm_INT_POW_POW : forall x : Z, forall m : nat, forall n : nat, (int_pow (int_pow x m) n) = (int_pow x (Nat.mul m n)).
Axiom thm_INT_POW_ZERO : forall n : nat, (int_pow (int_of_num (NUMERAL 0)) n) = (@COND Z (n = (NUMERAL 0)) (int_of_num (NUMERAL (BIT1 0))) (int_of_num (NUMERAL 0))).
Axiom thm_INT_RNEG_UNIQ : forall x : Z, forall y : Z, ((int_add x y) = (int_of_num (NUMERAL 0))) = (y = (int_neg x)).
Axiom thm_INT_SGN : forall x : Z, (int_sgn x) = (@COND Z (int_lt (int_of_num (NUMERAL 0)) x) (int_of_num (NUMERAL (BIT1 0))) (@COND Z (int_lt x (int_of_num (NUMERAL 0))) (int_neg (int_of_num (NUMERAL (BIT1 0)))) (int_of_num (NUMERAL 0)))).
Axiom thm_INT_SGNS_EQ : forall x : Z, forall y : Z, ((int_sgn x) = (int_sgn y)) = (((x = (int_of_num (NUMERAL 0))) = (y = (int_of_num (NUMERAL 0)))) /\ (((int_gt x (int_of_num (NUMERAL 0))) = (int_gt y (int_of_num (NUMERAL 0)))) /\ ((int_lt x (int_of_num (NUMERAL 0))) = (int_lt y (int_of_num (NUMERAL 0)))))).
Axiom thm_INT_SGNS_EQ_ALT : forall x : Z, forall y : Z, ((int_sgn x) = (int_sgn y)) = (((x = (int_of_num (NUMERAL 0))) -> y = (int_of_num (NUMERAL 0))) /\ (((int_gt x (int_of_num (NUMERAL 0))) -> int_gt y (int_of_num (NUMERAL 0))) /\ ((int_lt x (int_of_num (NUMERAL 0))) -> int_lt y (int_of_num (NUMERAL 0))))).
Axiom thm_INT_SGN_0 : (int_sgn (int_of_num (NUMERAL 0))) = (int_of_num (NUMERAL 0)).
Axiom thm_INT_SGN_ABS : forall x : Z, (int_mul (int_sgn x) (int_abs x)) = x.
Axiom thm_INT_SGN_ABS_ALT : forall x : Z, (int_mul (int_sgn x) x) = (int_abs x).
Axiom thm_INT_SGN_CASES : forall x : Z, ((int_sgn x) = (int_of_num (NUMERAL 0))) \/ (((int_sgn x) = (int_of_num (NUMERAL (BIT1 0)))) \/ ((int_sgn x) = (int_neg (int_of_num (NUMERAL (BIT1 0)))))).
Axiom thm_INT_SGN_EQ : (forall x : Z, ((int_sgn x) = (int_of_num (NUMERAL 0))) = (x = (int_of_num (NUMERAL 0)))) /\ ((forall x : Z, ((int_sgn x) = (int_of_num (NUMERAL (BIT1 0)))) = (int_gt x (int_of_num (NUMERAL 0)))) /\ (forall x : Z, ((int_sgn x) = (int_neg (int_of_num (NUMERAL (BIT1 0))))) = (int_lt x (int_of_num (NUMERAL 0))))).
Axiom thm_INT_SGN_EQ_INEQ : forall x : Z, forall y : Z, ((int_sgn x) = (int_sgn y)) = ((x = y) \/ (int_lt (int_abs (int_sub x y)) (int_max (int_abs x) (int_abs y)))).
Axiom thm_INT_SGN_INEQS : (forall x : Z, (int_le (int_of_num (NUMERAL 0)) (int_sgn x)) = (int_le (int_of_num (NUMERAL 0)) x)) /\ ((forall x : Z, (int_lt (int_of_num (NUMERAL 0)) (int_sgn x)) = (int_lt (int_of_num (NUMERAL 0)) x)) /\ ((forall x : Z, (int_ge (int_of_num (NUMERAL 0)) (int_sgn x)) = (int_ge (int_of_num (NUMERAL 0)) x)) /\ ((forall x : Z, (int_gt (int_of_num (NUMERAL 0)) (int_sgn x)) = (int_gt (int_of_num (NUMERAL 0)) x)) /\ ((forall x : Z, ((int_of_num (NUMERAL 0)) = (int_sgn x)) = ((int_of_num (NUMERAL 0)) = x)) /\ ((forall x : Z, (int_le (int_sgn x) (int_of_num (NUMERAL 0))) = (int_le x (int_of_num (NUMERAL 0)))) /\ ((forall x : Z, (int_lt (int_sgn x) (int_of_num (NUMERAL 0))) = (int_lt x (int_of_num (NUMERAL 0)))) /\ ((forall x : Z, (int_ge (int_sgn x) (int_of_num (NUMERAL 0))) = (int_ge x (int_of_num (NUMERAL 0)))) /\ ((forall x : Z, (int_gt (int_sgn x) (int_of_num (NUMERAL 0))) = (int_gt x (int_of_num (NUMERAL 0)))) /\ (forall x : Z, ((int_sgn x) = (int_of_num (NUMERAL 0))) = (x = (int_of_num (NUMERAL 0)))))))))))).
Axiom thm_INT_SGN_INT_SGN : forall x : Z, (int_sgn (int_sgn x)) = (int_sgn x).
Axiom thm_INT_SGN_MUL : forall x : Z, forall y : Z, (int_sgn (int_mul x y)) = (int_mul (int_sgn x) (int_sgn y)).
Axiom thm_INT_SGN_NEG : forall x : Z, (int_sgn (int_neg x)) = (int_neg (int_sgn x)).
Axiom thm_INT_SGN_POW : forall x : Z, forall n : nat, (int_sgn (int_pow x n)) = (int_pow (int_sgn x) n).
Axiom thm_INT_SGN_POW_2 : forall x : Z, (int_sgn (int_pow x (NUMERAL (BIT0 (BIT1 0))))) = (int_sgn (int_abs x)).
Axiom thm_INT_SOS_EQ_0 : forall x : Z, forall y : Z, ((int_add (int_pow x (NUMERAL (BIT0 (BIT1 0)))) (int_pow y (NUMERAL (BIT0 (BIT1 0))))) = (int_of_num (NUMERAL 0))) = ((x = (int_of_num (NUMERAL 0))) /\ (y = (int_of_num (NUMERAL 0)))).
Axiom thm_INT_SUB_0 : forall x : Z, forall y : Z, ((int_sub x y) = (int_of_num (NUMERAL 0))) = (x = y).
Axiom thm_INT_SUB_ABS : forall x : Z, forall y : Z, int_le (int_sub (int_abs x) (int_abs y)) (int_abs (int_sub x y)).
Axiom thm_INT_SUB_ADD : forall x : Z, forall y : Z, (int_add (int_sub x y) y) = x.
Axiom thm_INT_SUB_ADD2 : forall x : Z, forall y : Z, (int_add y (int_sub x y)) = x.
Axiom thm_INT_SUB_LDISTRIB : forall x : Z, forall y : Z, forall z : Z, (int_mul x (int_sub y z)) = (int_sub (int_mul x y) (int_mul x z)).
Axiom thm_INT_SUB_LE : forall x : Z, forall y : Z, (int_le (int_of_num (NUMERAL 0)) (int_sub x y)) = (int_le y x).
Axiom thm_INT_SUB_LNEG : forall x : Z, forall y : Z, (int_sub (int_neg x) y) = (int_neg (int_add x y)).
Axiom thm_INT_SUB_LT : forall x : Z, forall y : Z, (int_lt (int_of_num (NUMERAL 0)) (int_sub x y)) = (int_lt y x).
Axiom thm_INT_SUB_LZERO : forall x : Z, (int_sub (int_of_num (NUMERAL 0)) x) = (int_neg x).
Axiom thm_INT_SUB_NEG2 : forall x : Z, forall y : Z, (int_sub (int_neg x) (int_neg y)) = (int_sub y x).
Axiom thm_INT_SUB_RDISTRIB : forall x : Z, forall y : Z, forall z : Z, (int_mul (int_sub x y) z) = (int_sub (int_mul x z) (int_mul y z)).
Axiom thm_INT_SUB_REFL : forall x : Z, (int_sub x x) = (int_of_num (NUMERAL 0)).
Axiom thm_INT_SUB_RNEG : forall x : Z, forall y : Z, (int_sub x (int_neg y)) = (int_add x y).
Axiom thm_INT_SUB_RZERO : forall x : Z, (int_sub x (int_of_num (NUMERAL 0))) = x.
Axiom thm_INT_SUB_SUB : forall x : Z, forall y : Z, (int_sub (int_sub x y) x) = (int_neg y).
Axiom thm_INT_SUB_SUB2 : forall x : Z, forall y : Z, (int_sub x (int_sub x y)) = y.
Axiom thm_INT_SUB_TRIANGLE : forall a : Z, forall b : Z, forall c : Z, (int_add (int_sub a b) (int_sub b c)) = (int_sub a c).
Axiom thm_INT_WLOG_LE : forall (P : Z -> Z -> Prop), ((forall x : Z, forall y : Z, (P x y) = (P y x)) /\ (forall x : Z, forall y : Z, (int_le x y) -> P x y)) -> forall x : Z, forall y : Z, P x y.
Axiom thm_INT_WLOG_LT : forall (P : Z -> Z -> Prop), ((forall x : Z, P x x) /\ ((forall x : Z, forall y : Z, (P x y) = (P y x)) /\ (forall x : Z, forall y : Z, (int_lt x y) -> P x y))) -> forall x : Z, forall y : Z, P x y.
Axiom thm_INT_WLOG_LE_3 : forall P : Z -> Z -> Z -> Prop, ((forall x : Z, forall y : Z, forall z : Z, (P x y z) -> (P y x z) /\ (P x z y)) /\ (forall x : Z, forall y : Z, forall z : Z, ((int_le x y) /\ (int_le y z)) -> P x y z)) -> forall x : Z, forall y : Z, forall z : Z, P x y z.
Axiom thm_INT_FORALL_POS : forall P : Z -> Prop, (forall n : nat, P (int_of_num n)) = (forall i : Z, (int_le (int_of_num (NUMERAL 0)) i) -> P i).
Axiom thm_INT_EXISTS_POS : forall P : Z -> Prop, (exists n : nat, P (int_of_num n)) = (exists i : Z, (int_le (int_of_num (NUMERAL 0)) i) /\ (P i)).
Axiom thm_INT_FORALL_ABS : forall P : Z -> Prop, (forall n : nat, P (int_of_num n)) = (forall x : Z, P (int_abs x)).
Axiom thm_INT_EXISTS_ABS : forall P : Z -> Prop, (exists n : nat, P (int_of_num n)) = (exists x : Z, P (int_abs x)).
Axiom thm_INT_POW : forall (x : Z), ((int_pow x (NUMERAL 0)) = (int_of_num (NUMERAL (BIT1 0)))) /\ (forall n : nat, (int_pow x (S n)) = (int_mul x (int_pow x n))).
Axiom thm_INT_ABS : forall x : Z, (int_abs x) = (@COND Z (int_le (int_of_num (NUMERAL 0)) x) x (int_neg x)).
Axiom thm_INT_GE : forall x : Z, forall y : Z, (int_ge x y) = (int_le y x).
Axiom thm_INT_GT : forall x : Z, forall y : Z, (int_gt x y) = (int_lt y x).
Axiom thm_INT_LT : forall x : Z, forall y : Z, (int_lt x y) = (~ (int_le y x)).
Axiom thm_INT_SUB : forall x : Z, forall y : Z, (int_sub x y) = (int_add x (int_neg y)).
Axiom thm_INT_MAX : forall x : Z, forall y : Z, (int_max x y) = (@COND Z (int_le x y) y x).
Axiom thm_INT_MIN : forall x : Z, forall y : Z, (int_min x y) = (@COND Z (int_le x y) x y).
Axiom thm_INT_OF_NUM_EXISTS : forall x : Z, (exists n : nat, x = (int_of_num n)) = (int_le (int_of_num (NUMERAL 0)) x).
Axiom thm_INT_LE_DISCRETE : forall x : Z, forall y : Z, (int_le x y) = (int_lt x (int_add y (int_of_num (NUMERAL (BIT1 0))))).
Axiom thm_INT_LE_TRANS_LE : forall x : Z, forall y : Z, (int_le x y) = (forall z : Z, (int_le y z) -> int_le x z).
Axiom thm_INT_LE_TRANS_LT : forall x : Z, forall y : Z, (int_le x y) = (forall z : Z, (int_lt y z) -> int_lt x z).
Axiom thm_INT_MUL_EQ_1 : forall x : Z, forall y : Z, ((int_mul x y) = (int_of_num (NUMERAL (BIT1 0)))) = (((x = (int_of_num (NUMERAL (BIT1 0)))) /\ (y = (int_of_num (NUMERAL (BIT1 0))))) \/ ((x = (int_neg (int_of_num (NUMERAL (BIT1 0))))) /\ (y = (int_neg (int_of_num (NUMERAL (BIT1 0))))))).
Axiom thm_INT_ABS_MUL_1 : forall x : Z, forall y : Z, ((int_abs (int_mul x y)) = (int_of_num (NUMERAL (BIT1 0)))) = (((int_abs x) = (int_of_num (NUMERAL (BIT1 0)))) /\ ((int_abs y) = (int_of_num (NUMERAL (BIT1 0))))).
Axiom thm_INT_WOP : forall (P : Z -> Prop), (exists x : Z, (int_le (int_of_num (NUMERAL 0)) x) /\ (P x)) = (exists x : Z, (int_le (int_of_num (NUMERAL 0)) x) /\ ((P x) /\ (forall y : Z, ((int_le (int_of_num (NUMERAL 0)) y) /\ (P y)) -> int_le x y))).
Axiom thm_INT_ARCH : forall x : Z, forall d : Z, (~ (d = (int_of_num (NUMERAL 0)))) -> exists c : Z, int_lt x (int_mul c d).
Axiom thm_INT_DIVMOD_EXIST_0 : forall m : Z, forall n : Z, exists q : Z, exists r : Z, @COND Prop (n = (int_of_num (NUMERAL 0))) ((q = (int_of_num (NUMERAL 0))) /\ (r = m)) ((int_le (int_of_num (NUMERAL 0)) r) /\ ((int_lt r (int_abs n)) /\ (m = (int_add (int_mul q n) r)))).
Axiom thm_INT_DIVISION : forall m : Z, forall n : Z, (~ (n = (int_of_num (NUMERAL 0)))) -> (m = (int_add (int_mul (div m n) n) (rem m n))) /\ ((int_le (int_of_num (NUMERAL 0)) (rem m n)) /\ (int_lt (rem m n) (int_abs n))).
Axiom thm_INT_DIVISION_SIMP : forall m : Z, forall n : Z, (int_add (int_mul (div m n) n) (rem m n)) = m.
Axiom thm_INT_REM_POS : forall a : Z, forall b : Z, (~ (b = (int_of_num (NUMERAL 0)))) -> int_le (int_of_num (NUMERAL 0)) (rem a b).
Axiom thm_INT_DIV_0 : forall m : Z, (div m (int_of_num (NUMERAL 0))) = (int_of_num (NUMERAL 0)).
Axiom thm_INT_REM_0 : forall m : Z, (rem m (int_of_num (NUMERAL 0))) = m.
Axiom thm_INT_REM_POS_EQ : forall m : Z, forall n : Z, (int_le (int_of_num (NUMERAL 0)) (rem m n)) = ((n = (int_of_num (NUMERAL 0))) -> int_le (int_of_num (NUMERAL 0)) m).
Axiom thm_INT_REM_DIV : forall m : Z, forall n : Z, (rem m n) = (int_sub m (int_mul (div m n) n)).
Axiom thm_INT_LT_REM : forall x : Z, forall n : Z, (int_lt (int_of_num (NUMERAL 0)) n) -> int_lt (rem x n) n.
Axiom thm_INT_LT_REM_EQ : forall m : Z, forall n : Z, (int_lt (rem m n) n) = ((int_lt (int_of_num (NUMERAL 0)) n) \/ ((n = (int_of_num (NUMERAL 0))) /\ (int_lt m (int_of_num (NUMERAL 0))))).
Axiom thm_cong : forall {A : Type'}, forall rel : A -> A -> Prop, forall x : A, forall y : A, (@eq2 A x y rel) = (rel x y).
Axiom thm_real_mod : forall x : R, forall y : R, forall n : R, (real_mod n x y) = (exists q : R, (integer q) /\ ((Rminus x y) = (Rmult q n))).
Axiom thm_int_divides : forall b : Z, forall a : Z, (int_divides a b) = (exists x : Z, b = (int_mul a x)).
Axiom thm_INT_DIVIDES_LE : forall x : Z, forall y : Z, (int_divides x y) -> (int_le (int_abs x) (int_abs y)) \/ (y = (int_of_num (NUMERAL 0))).
Axiom thm_int_mod : forall n : Z, forall x : Z, forall y : Z, (int_mod n x y) = (int_divides n (int_sub x y)).
Axiom thm_int_congruent : forall x : Z, forall y : Z, forall n : Z, (@eq2 Z x y (int_mod n)) = (exists d : Z, (int_sub x y) = (int_mul n d)).
Axiom thm_INT_CONG_IMP_EQ : forall x : Z, forall y : Z, forall n : Z, ((int_lt (int_abs (int_sub x y)) n) /\ (@eq2 Z x y (int_mod n))) -> x = y.
Axiom thm_int_coprime : forall a : Z, forall b : Z, (int_coprime (@pair Z Z a b)) = (exists x : Z, exists y : Z, (int_add (int_mul a x) (int_mul b y)) = (int_of_num (NUMERAL (BIT1 0)))).
Axiom thm_INT_DIVMOD_UNIQ : forall m : Z, forall n : Z, forall q : Z, forall r : Z, ((m = (int_add (int_mul q n) r)) /\ ((int_le (int_of_num (NUMERAL 0)) r) /\ (int_lt r (int_abs n)))) -> ((div m n) = q) /\ ((rem m n) = r).
Axiom thm_INT_DIV_UNIQ : forall m : Z, forall n : Z, forall q : Z, forall r : Z, ((m = (int_add (int_mul q n) r)) /\ ((int_le (int_of_num (NUMERAL 0)) r) /\ (int_lt r (int_abs n)))) -> (div m n) = q.
Axiom thm_INT_REM_UNIQ : forall m : Z, forall n : Z, forall q : Z, forall r : Z, ((m = (int_add (int_mul q n) r)) /\ ((int_le (int_of_num (NUMERAL 0)) r) /\ (int_lt r (int_abs n)))) -> (rem m n) = r.
Axiom thm_INT_REM_LT : forall m : Z, forall n : Z, (((~ (n = (int_of_num (NUMERAL 0)))) -> int_le (int_of_num (NUMERAL 0)) m) /\ (int_lt m n)) -> (rem m n) = m.
Axiom thm_INT_DIV_LT : forall m : Z, forall n : Z, (((~ (n = (int_of_num (NUMERAL 0)))) -> int_le (int_of_num (NUMERAL 0)) m) /\ (int_lt m n)) -> (div m n) = (int_of_num (NUMERAL 0)).
Axiom thm_INT_REM_RNEG : forall m : Z, forall n : Z, (rem m (int_neg n)) = (rem m n).
Axiom thm_INT_DIV_RNEG : forall m : Z, forall n : Z, (div m (int_neg n)) = (int_neg (div m n)).
Axiom thm_INT_REM_RABS : forall x : Z, forall y : Z, (rem x (int_abs y)) = (rem x y).
Axiom thm_INT_REM_REM : forall m : Z, forall n : Z, (rem (rem m n) n) = (rem m n).
Axiom thm_INT_REM_EQ : forall m : Z, forall n : Z, forall p : Z, ((rem m p) = (rem n p)) = (@eq2 Z m n (int_mod p)).
Axiom thm_INT_REM_ZERO : forall n : Z, (rem (int_of_num (NUMERAL 0)) n) = (int_of_num (NUMERAL 0)).
Axiom thm_INT_DIV_ZERO : forall n : Z, (div (int_of_num (NUMERAL 0)) n) = (int_of_num (NUMERAL 0)).
Axiom thm_INT_REM_EQ_0 : forall m : Z, forall n : Z, ((rem m n) = (int_of_num (NUMERAL 0))) = (int_divides n m).
Axiom thm_INT_MUL_DIV_EQ : (forall m : Z, forall n : Z, ((int_mul n (div m n)) = m) = (int_divides n m)) /\ (forall m : Z, forall n : Z, ((int_mul (div m n) n) = m) = (int_divides n m)).
Axiom thm_INT_CONG_LREM : forall x : Z, forall y : Z, forall n : Z, (@eq2 Z (rem x n) y (int_mod n)) = (@eq2 Z x y (int_mod n)).
Axiom thm_INT_CONG_RREM : forall x : Z, forall y : Z, forall n : Z, (@eq2 Z x (rem y n) (int_mod n)) = (@eq2 Z x y (int_mod n)).
Axiom thm_INT_REM_MOD_SELF : forall m : Z, forall n : Z, @eq2 Z (rem m n) m (int_mod n).
Axiom thm_INT_REM_REM_MUL : (forall m : Z, forall n : Z, forall p : Z, (rem (rem m (int_mul n p)) n) = (rem m n)) /\ (forall m : Z, forall n : Z, forall p : Z, (rem (rem m (int_mul n p)) p) = (rem m p)).
Axiom thm_INT_CONG_SOLVE_BOUNDS : forall a : Z, forall n : Z, (~ (n = (int_of_num (NUMERAL 0)))) -> exists x : Z, (int_le (int_of_num (NUMERAL 0)) x) /\ ((int_lt x (int_abs n)) /\ (@eq2 Z x a (int_mod n))).
Axiom thm_INT_NEG_REM : forall n : Z, forall p : Z, (rem (int_neg (rem n p)) p) = (rem (int_neg n) p).
Axiom thm_INT_ADD_REM : forall m : Z, forall n : Z, forall p : Z, (rem (int_add (rem m p) (rem n p)) p) = (rem (int_add m n) p).
Axiom thm_INT_SUB_REM : forall m : Z, forall n : Z, forall p : Z, (rem (int_sub (rem m p) (rem n p)) p) = (rem (int_sub m n) p).
Axiom thm_INT_MUL_REM : forall m : Z, forall n : Z, forall p : Z, (rem (int_mul (rem m p) (rem n p)) p) = (rem (int_mul m n) p).
Axiom thm_INT_POW_REM : forall m : Z, forall n : nat, forall p : Z, (rem (int_pow (rem m p) n) p) = (rem (int_pow m n) p).
Axiom thm_INT_OF_NUM_REM : forall m : nat, forall n : nat, (rem (int_of_num m) (int_of_num n)) = (int_of_num (Nat.modulo m n)).
Axiom thm_INT_OF_NUM_DIV : forall m : nat, forall n : nat, (div (int_of_num m) (int_of_num n)) = (int_of_num (Nat.div m n)).
Axiom thm_INT_REM_REFL : forall n : Z, (rem n n) = (int_of_num (NUMERAL 0)).
Axiom thm_INT_DIV_REFL : forall n : Z, (div n n) = (@COND Z (n = (int_of_num (NUMERAL 0))) (int_of_num (NUMERAL 0)) (int_of_num (NUMERAL (BIT1 0)))).
Axiom thm_INT_REM_LNEG : forall m : Z, forall n : Z, (rem (int_neg m) n) = (@COND Z ((rem m n) = (int_of_num (NUMERAL 0))) (int_of_num (NUMERAL 0)) (int_sub (int_abs n) (rem m n))).
Axiom thm_INT_DIV_LNEG : forall m : Z, forall n : Z, (div (int_neg m) n) = (@COND Z ((rem m n) = (int_of_num (NUMERAL 0))) (int_neg (div m n)) (int_sub (int_neg (div m n)) (int_sgn n))).
Axiom thm_INT_DIV_NEG2 : forall m : Z, forall n : Z, (div (int_neg m) (int_neg n)) = (@COND Z ((rem m n) = (int_of_num (NUMERAL 0))) (div m n) (int_add (div m n) (int_sgn n))).
Axiom thm_INT_REM_NEG2 : forall m : Z, forall n : Z, (rem (int_neg m) (int_neg n)) = (@COND Z ((rem m n) = (int_of_num (NUMERAL 0))) (int_of_num (NUMERAL 0)) (int_sub (int_abs n) (rem m n))).
Axiom thm_INT_REM_1 : forall n : Z, (rem n (int_of_num (NUMERAL (BIT1 0)))) = (int_of_num (NUMERAL 0)).
Axiom thm_INT_DIV_1 : forall n : Z, (div n (int_of_num (NUMERAL (BIT1 0)))) = n.
Axiom thm_INT_REM_MUL : (forall m : Z, forall n : Z, (rem (int_mul m n) n) = (int_of_num (NUMERAL 0))) /\ (forall m : Z, forall n : Z, (rem (int_mul m n) m) = (int_of_num (NUMERAL 0))).
Axiom thm_INT_DIV_MUL : (forall m : Z, forall n : Z, (~ (n = (int_of_num (NUMERAL 0)))) -> (div (int_mul m n) n) = m) /\ (forall m : Z, forall n : Z, (~ (m = (int_of_num (NUMERAL 0)))) -> (div (int_mul m n) m) = n).
Axiom thm_INT_DIV_LT_EQ : forall a : Z, forall b : Z, forall c : Z, (int_lt (int_of_num (NUMERAL 0)) a) -> (int_lt (div b a) c) = (int_lt b (int_mul a c)).
Axiom thm_INT_LE_DIV_EQ : forall a : Z, forall b : Z, forall c : Z, (int_lt (int_of_num (NUMERAL 0)) a) -> (int_le c (div b a)) = (int_le (int_mul a c) b).
Axiom thm_INT_DIV_LE_EQ : forall a : Z, forall b : Z, forall c : Z, (int_lt (int_of_num (NUMERAL 0)) a) -> (int_le (div b a) c) = (int_lt b (int_mul a (int_add c (int_of_num (NUMERAL (BIT1 0)))))).
Axiom thm_INT_LT_DIV_EQ : forall a : Z, forall b : Z, forall c : Z, (int_lt (int_of_num (NUMERAL 0)) a) -> (int_lt c (div b a)) = (int_le (int_mul a (int_add c (int_of_num (NUMERAL (BIT1 0))))) b).
Axiom thm_INT_DIV_LE : forall m : Z, forall n : Z, int_le (int_abs (div m n)) (int_abs m).
Axiom thm_INT_REM_MUL_REM : forall m : Z, forall n : Z, forall p : Z, (int_le (int_of_num (NUMERAL 0)) n) -> (rem m (int_mul n p)) = (int_add (int_mul n (rem (div m n) p)) (rem m n)).
Axiom thm_INT_DIV_DIV : forall m : Z, forall n : Z, forall p : Z, (int_le (int_of_num (NUMERAL 0)) n) -> (div (div m n) p) = (div m (int_mul n p)).
Axiom thm_INT_DIV_EQ_0 : forall m : Z, forall n : Z, ((div m n) = (int_of_num (NUMERAL 0))) = ((n = (int_of_num (NUMERAL 0))) \/ ((int_le (int_of_num (NUMERAL 0)) m) /\ (int_lt m (int_abs n)))).
Axiom thm_INT_REM_EQ_SELF : forall m : Z, forall n : Z, ((rem m n) = m) = ((n = (int_of_num (NUMERAL 0))) \/ ((int_le (int_of_num (NUMERAL 0)) m) /\ (int_lt m (int_abs n)))).
Axiom thm_INT_REM_UNIQUE : forall m : Z, forall n : Z, forall p : Z, ((rem m n) = p) = ((((n = (int_of_num (NUMERAL 0))) /\ (m = p)) \/ ((int_le (int_of_num (NUMERAL 0)) p) /\ (int_lt p (int_abs n)))) /\ (@eq2 Z m p (int_mod n))).
Axiom thm_INT_DIV_REM : forall m : Z, forall n : Z, forall p : Z, (int_le (int_of_num (NUMERAL 0)) n) -> (rem (div m n) p) = (div (rem m (int_mul n p)) n).
Axiom thm_INT_REM_REM_LE : forall m : Z, forall n : Z, forall p : Z, ((~ (n = (int_of_num (NUMERAL 0)))) /\ (int_le (int_abs n) (int_abs p))) -> (rem (rem m n) p) = (rem m n).
Axiom thm_INT_LE_DIV : forall m : Z, forall n : Z, ((int_le (int_of_num (NUMERAL 0)) m) /\ (int_le (int_of_num (NUMERAL 0)) n)) -> int_le (int_of_num (NUMERAL 0)) (div m n).
Axiom thm_INT_LT_DIV : forall m : Z, forall n : Z, ((int_lt (int_of_num (NUMERAL 0)) n) /\ (int_le n m)) -> int_lt (int_of_num (NUMERAL 0)) (div m n).
Axiom thm_INT_REM_LE_EQ : forall m : Z, forall n : Z, (int_le (rem m n) m) = ((n = (int_of_num (NUMERAL 0))) \/ (int_le (int_of_num (NUMERAL 0)) m)).
Axiom thm_INT_REM_LE : forall m : Z, forall n : Z, forall p : Z, (((n = (int_of_num (NUMERAL 0))) \/ (int_le (int_of_num (NUMERAL 0)) m)) /\ (int_le m p)) -> int_le (rem m n) p.
Axiom thm_INT_REM_MUL_ADD : (forall m : Z, forall n : Z, forall p : Z, (rem (int_add (int_mul m n) p) n) = (rem p n)) /\ ((forall m : Z, forall n : Z, forall p : Z, (rem (int_add (int_mul n m) p) n) = (rem p n)) /\ ((forall m : Z, forall n : Z, forall p : Z, (rem (int_add p (int_mul m n)) n) = (rem p n)) /\ (forall m : Z, forall n : Z, forall p : Z, (rem (int_add p (int_mul n m)) n) = (rem p n)))).
Axiom thm_INT_DIV_MUL_ADD : (forall m : Z, forall n : Z, forall p : Z, (~ (n = (int_of_num (NUMERAL 0)))) -> (div (int_add (int_mul m n) p) n) = (int_add m (div p n))) /\ ((forall m : Z, forall n : Z, forall p : Z, (~ (n = (int_of_num (NUMERAL 0)))) -> (div (int_add (int_mul n m) p) n) = (int_add m (div p n))) /\ ((forall m : Z, forall n : Z, forall p : Z, (~ (n = (int_of_num (NUMERAL 0)))) -> (div (int_add p (int_mul m n)) n) = (int_add (div p n) m)) /\ (forall m : Z, forall n : Z, forall p : Z, (~ (n = (int_of_num (NUMERAL 0)))) -> (div (int_add p (int_mul n m)) n) = (int_add (div p n) m)))).
Axiom thm_INT_CONG_DIV2 : forall a : Z, forall b : Z, forall m : Z, forall n : Z, (@eq2 Z a b (int_mod (int_mul m n))) -> @eq2 Z (div a m) (div b m) (int_mod n).
Axiom thm_INT_REM_2_CASES : forall n : Z, ((rem n (int_of_num (NUMERAL (BIT0 (BIT1 0))))) = (int_of_num (NUMERAL 0))) \/ ((rem n (int_of_num (NUMERAL (BIT0 (BIT1 0))))) = (int_of_num (NUMERAL (BIT1 0)))).
Axiom thm_NOT_INT_REM_2 : (forall n : Z, (~ ((rem n (int_of_num (NUMERAL (BIT0 (BIT1 0))))) = (int_of_num (NUMERAL 0)))) = ((rem n (int_of_num (NUMERAL (BIT0 (BIT1 0))))) = (int_of_num (NUMERAL (BIT1 0))))) /\ (forall n : Z, (~ ((rem n (int_of_num (NUMERAL (BIT0 (BIT1 0))))) = (int_of_num (NUMERAL (BIT1 0))))) = ((rem n (int_of_num (NUMERAL (BIT0 (BIT1 0))))) = (int_of_num (NUMERAL 0)))).
Axiom thm_INT_REM_2_DIVIDES : (forall n : Z, ((rem n (int_of_num (NUMERAL (BIT0 (BIT1 0))))) = (int_of_num (NUMERAL 0))) = (int_divides (int_of_num (NUMERAL (BIT0 (BIT1 0)))) n)) /\ (forall n : Z, ((rem n (int_of_num (NUMERAL (BIT0 (BIT1 0))))) = (int_of_num (NUMERAL (BIT1 0)))) = (~ (int_divides (int_of_num (NUMERAL (BIT0 (BIT1 0)))) n))).
Axiom thm_INT_REM_2_EXPAND : forall x : Z, (rem x (int_of_num (NUMERAL (BIT0 (BIT1 0))))) = (@COND Z (int_divides (int_of_num (NUMERAL (BIT0 (BIT1 0)))) x) (int_of_num (NUMERAL 0)) (int_of_num (NUMERAL (BIT1 0)))).
Axiom thm_INT_REM_2_NEG : forall x : Z, (rem (int_neg x) (int_of_num (NUMERAL (BIT0 (BIT1 0))))) = (rem x (int_of_num (NUMERAL (BIT0 (BIT1 0))))).
Axiom thm_INT_DIVIDES_DIV_SELF : forall n : Z, forall d : Z, (int_divides d n) -> int_divides (div n d) n.
Axiom thm_INT_DIV_BY_DIV : forall m : Z, forall n : Z, ((~ (n = (int_of_num (NUMERAL 0)))) /\ (int_divides m n)) -> (div n (div n m)) = m.
Axiom thm_INT_DIVIDES_DIV_DIVIDES : forall n : Z, forall d : Z, forall e : Z, ((int_divides d n) /\ ((n = (int_of_num (NUMERAL 0))) -> e = (int_of_num (NUMERAL 0)))) -> (int_divides (div n d) e) = (int_divides n (int_mul d e)).
Axiom thm_INT_DIVIDES_DIVIDES_DIV : forall n : Z, forall d : Z, forall e : Z, (int_divides d n) -> (int_divides e (div n d)) = (int_divides (int_mul d e) n).
Axiom thm_INT_DIVIDES_DIVIDES_DIV_EQ : forall n : Z, forall d : Z, forall e : Z, ((int_divides d n) /\ (int_divides e (div n d))) = (int_divides (int_mul d e) n).
Axiom thm_INT_2_DIVIDES_ADD : forall m : Z, forall n : Z, (int_divides (int_of_num (NUMERAL (BIT0 (BIT1 0)))) (int_add m n)) = ((int_divides (int_of_num (NUMERAL (BIT0 (BIT1 0)))) m) = (int_divides (int_of_num (NUMERAL (BIT0 (BIT1 0)))) n)).
Axiom thm_INT_2_DIVIDES_SUB : forall m : Z, forall n : Z, (int_divides (int_of_num (NUMERAL (BIT0 (BIT1 0)))) (int_sub m n)) = ((int_divides (int_of_num (NUMERAL (BIT0 (BIT1 0)))) m) = (int_divides (int_of_num (NUMERAL (BIT0 (BIT1 0)))) n)).
Axiom thm_INT_2_DIVIDES_MUL : forall m : Z, forall n : Z, (int_divides (int_of_num (NUMERAL (BIT0 (BIT1 0)))) (int_mul m n)) = ((int_divides (int_of_num (NUMERAL (BIT0 (BIT1 0)))) m) \/ (int_divides (int_of_num (NUMERAL (BIT0 (BIT1 0)))) n)).
Axiom thm_INT_2_DIVIDES_POW : forall n : Z, forall k : nat, (int_divides (int_of_num (NUMERAL (BIT0 (BIT1 0)))) (int_pow n k)) = ((int_divides (int_of_num (NUMERAL (BIT0 (BIT1 0)))) n) /\ (~ (k = (NUMERAL 0)))).
Axiom thm_WF_INT_MEASURE : forall {A : Type'}, forall P : A -> Prop, forall m : A -> Z, ((forall x : A, int_le (int_of_num (NUMERAL 0)) (m x)) /\ (forall x : A, (forall y : A, (int_lt (m y) (m x)) -> P y) -> P x)) -> forall x : A, P x.
Axiom thm_WF_INT_MEASURE_2 : forall {A B : Type'}, forall P : A -> B -> Prop, forall m : A -> B -> Z, ((forall x : A, forall y : B, int_le (int_of_num (NUMERAL 0)) (m x y)) /\ (forall x : A, forall y : B, (forall x' : A, forall y' : B, (int_lt (m x' y') (m x y)) -> P x' y') -> P x y)) -> forall x : A, forall y : B, P x y.
Axiom thm_INT_GCD_EXISTS : forall a : Z, forall b : Z, exists d : Z, (int_divides d a) /\ ((int_divides d b) /\ (exists x : Z, exists y : Z, d = (int_add (int_mul a x) (int_mul b y)))).
Axiom thm_INT_GCD_EXISTS_POS : forall a : Z, forall b : Z, exists d : Z, (int_le (int_of_num (NUMERAL 0)) d) /\ ((int_divides d a) /\ ((int_divides d b) /\ (exists x : Z, exists y : Z, d = (int_add (int_mul a x) (int_mul b y))))).
Axiom thm_int_lcm : forall m : Z, forall n : Z, (int_lcm (@pair Z Z m n)) = (@COND Z ((int_mul m n) = (int_of_num (NUMERAL 0))) (int_of_num (NUMERAL 0)) (div (int_abs (int_mul m n)) (int_gcd (@pair Z Z m n)))).
Axiom thm_INT_DIVIDES_LABS : forall d : Z, forall n : Z, (int_divides (int_abs d) n) = (int_divides d n).
Axiom thm_INT_DIVIDES_RABS : forall d : Z, forall n : Z, (int_divides d (int_abs n)) = (int_divides d n).
Axiom thm_INT_DIVIDES_ABS : (forall d : Z, forall n : Z, (int_divides (int_abs d) n) = (int_divides d n)) /\ (forall d : Z, forall n : Z, (int_divides d (int_abs n)) = (int_divides d n)).
Axiom thm_INT_LCM_POS : forall m : Z, forall n : Z, int_le (int_of_num (NUMERAL 0)) (int_lcm (@pair Z Z m n)).
Axiom thm_INT_MUL_GCD_LCM : forall m : Z, forall n : Z, (int_mul (int_gcd (@pair Z Z m n)) (int_lcm (@pair Z Z m n))) = (int_abs (int_mul m n)).
Axiom thm_INT_MUL_LCM_GCD : forall m : Z, forall n : Z, (int_mul (int_lcm (@pair Z Z m n)) (int_gcd (@pair Z Z m n))) = (int_abs (int_mul m n)).
Axiom thm_INT_DIVIDES_LCM_GCD : forall m : Z, forall n : Z, forall d : Z, (int_divides d (int_lcm (@pair Z Z m n))) = (int_divides (int_mul d (int_gcd (@pair Z Z m n))) (int_mul m n)).
Axiom thm_INT_LCM_DIVIDES : forall m : Z, forall n : Z, forall d : Z, (int_divides (int_lcm (@pair Z Z m n)) d) = ((int_divides m d) /\ (int_divides n d)).
Axiom thm_INT_LCM : forall m : Z, forall n : Z, (int_divides m (int_lcm (@pair Z Z m n))) /\ ((int_divides n (int_lcm (@pair Z Z m n))) /\ (forall d : Z, ((int_divides m d) /\ (int_divides n d)) -> int_divides (int_lcm (@pair Z Z m n)) d)).
Axiom thm_num_of_int : forall x : Z, (num_of_int x) = (@ε nat (fun n : nat => (int_of_num n) = x)).
Axiom thm_NUM_OF_INT_OF_NUM : forall n : nat, (num_of_int (int_of_num n)) = n.
Axiom thm_INT_OF_NUM_OF_INT : forall x : Z, (int_le (int_of_num (NUMERAL 0)) x) -> (int_of_num (num_of_int x)) = x.
Axiom thm_NUM_OF_INT : forall x : Z, (int_le (int_of_num (NUMERAL 0)) x) = ((int_of_num (num_of_int x)) = x).
Axiom thm_NUM_OF_INT_ADD : forall x : Z, forall y : Z, ((int_le (int_of_num (NUMERAL 0)) x) /\ (int_le (int_of_num (NUMERAL 0)) y)) -> (num_of_int (int_add x y)) = (Nat.add (num_of_int x) (num_of_int y)).
Axiom thm_NUM_OF_INT_MUL : forall x : Z, forall y : Z, ((int_le (int_of_num (NUMERAL 0)) x) /\ (int_le (int_of_num (NUMERAL 0)) y)) -> (num_of_int (int_mul x y)) = (Nat.mul (num_of_int x) (num_of_int y)).
Axiom thm_NUM_OF_INT_POW : forall x : Z, forall n : nat, (int_le (int_of_num (NUMERAL 0)) x) -> (num_of_int (int_pow x n)) = (Nat.pow (num_of_int x) n).
Axiom thm_num_divides : forall a : nat, forall b : nat, (num_divides a b) = (int_divides (int_of_num a) (int_of_num b)).
Axiom thm_num_mod : forall n : nat, forall x : nat, forall y : nat, (num_mod n x y) = (int_mod (int_of_num n) (int_of_num x) (int_of_num y)).
Axiom thm_num_congruent : forall x : nat, forall y : nat, forall n : nat, (@eq2 nat x y (num_mod n)) = (@eq2 Z (int_of_num x) (int_of_num y) (int_mod (int_of_num n))).
Axiom thm_num_coprime : forall a : nat, forall b : nat, (num_coprime (@pair nat nat a b)) = (int_coprime (@pair Z Z (int_of_num a) (int_of_num b))).
Axiom thm_num_gcd : forall a : nat, forall b : nat, (num_gcd (@pair nat nat a b)) = (num_of_int (int_gcd (@pair Z Z (int_of_num a) (int_of_num b)))).
Axiom thm_num_lcm : forall a : nat, forall b : nat, (num_lcm (@pair nat nat a b)) = (num_of_int (int_lcm (@pair Z Z (int_of_num a) (int_of_num b)))).
Axiom thm_BINARY_INDUCT : forall P : nat -> Prop, ((P (NUMERAL 0)) /\ (forall n : nat, (P n) -> (P (Nat.mul (NUMERAL (BIT0 (BIT1 0))) n)) /\ (P (Nat.add (Nat.mul (NUMERAL (BIT0 (BIT1 0))) n) (NUMERAL (BIT1 0)))))) -> forall n : nat, P n.
Axiom thm_NUM_CASES_BINARY : forall P : nat -> Prop, (forall n : nat, P n) = ((forall n : nat, P (Nat.mul (NUMERAL (BIT0 (BIT1 0))) n)) /\ (forall n : nat, P (Nat.add (Nat.mul (NUMERAL (BIT0 (BIT1 0))) n) (NUMERAL (BIT1 0))))).
Axiom thm_num_WF_DOWN : forall P : nat -> Prop, forall m : nat, ((forall n : nat, (Peano.le m n) -> P n) /\ (forall n : nat, ((Peano.lt n m) /\ (forall p : nat, (Peano.lt n p) -> P p)) -> P n)) -> forall n : nat, P n.
Axiom thm_INT_REM_REM_POW_MIN : forall x : Z, forall p : Z, forall m : nat, forall n : nat, (rem (rem x (int_pow p m)) (int_pow p n)) = (rem x (int_pow p (Nat.min m n))).
Axiom thm_NUM_GCD : forall a : nat, forall b : nat, (int_of_num (num_gcd (@pair nat nat a b))) = (int_gcd (@pair Z Z (int_of_num a) (int_of_num b))).
Axiom thm_NUM_LCM : forall a : nat, forall b : nat, (int_of_num (num_lcm (@pair nat nat a b))) = (int_lcm (@pair Z Z (int_of_num a) (int_of_num b))).
Axiom thm_CONG : forall x : nat, forall y : nat, forall n : nat, (@eq2 nat x y (num_mod n)) = ((Nat.modulo x n) = (Nat.modulo y n)).
Axiom thm_CONG_LMOD : forall x : nat, forall y : nat, forall n : nat, (@eq2 nat (Nat.modulo x n) y (num_mod n)) = (@eq2 nat x y (num_mod n)).
Axiom thm_CONG_RMOD : forall x : nat, forall y : nat, forall n : nat, (@eq2 nat x (Nat.modulo y n) (num_mod n)) = (@eq2 nat x y (num_mod n)).
Axiom thm_CONG_DIV2 : forall a : nat, forall b : nat, forall m : nat, forall n : nat, (@eq2 nat a b (num_mod (Nat.mul m n))) -> @eq2 nat (Nat.div a m) (Nat.div b m) (num_mod n).
Axiom thm_divides : forall (b : nat) (a : nat), (num_divides a b) = (exists x : nat, b = (Nat.mul a x)).
Axiom thm_DIVIDES_LE : forall m : nat, forall n : nat, (num_divides m n) -> (Peano.le m n) \/ (n = (NUMERAL 0)).
Axiom thm_DIVIDES_LE_STRONG : forall m : nat, forall n : nat, (num_divides m n) -> ((Peano.le (NUMERAL (BIT1 0)) m) /\ (Peano.le m n)) \/ (n = (NUMERAL 0)).
Axiom thm_DIVIDES_LE_IMP : forall m : nat, forall n : nat, ((num_divides m n) /\ ((n = (NUMERAL 0)) -> m = (NUMERAL 0))) -> Peano.le m n.
Axiom thm_PROPERLY_DIVIDES_LE_IMP : forall m : nat, forall n : nat, ((num_divides m n) /\ ((~ (n = (NUMERAL 0))) /\ (~ (m = n)))) -> Peano.le (Nat.mul (NUMERAL (BIT0 (BIT1 0))) m) n.
Axiom thm_DIVIDES_ANTISYM : forall m : nat, forall n : nat, ((num_divides m n) /\ (num_divides n m)) = (m = n).
Axiom thm_DIVIDES_ONE : forall n : nat, (num_divides n (NUMERAL (BIT1 0))) = (n = (NUMERAL (BIT1 0))).
Axiom thm_DIV_ADD : forall d : nat, forall a : nat, forall b : nat, ((num_divides d a) \/ (num_divides d b)) -> (Nat.div (Nat.add a b) d) = (Nat.add (Nat.div a d) (Nat.div b d)).
Axiom thm_DIVIDES_MOD : forall m : nat, forall n : nat, (num_divides m n) = ((Nat.modulo n m) = (NUMERAL 0)).
Axiom thm_DIVIDES_DIV_MULT : forall m : nat, forall n : nat, (num_divides m n) = ((Nat.mul (Nat.div n m) m) = n).
Axiom thm_DIV_BY_DIV : forall m : nat, forall n : nat, ((~ (n = (NUMERAL 0))) /\ (num_divides m n)) -> (Nat.div n (Nat.div n m)) = m.
Axiom thm_DIVIDES_DIV_DIVIDES : forall n : nat, forall d : nat, forall e : nat, ((num_divides d n) /\ ((n = (NUMERAL 0)) -> e = (NUMERAL 0))) -> (num_divides (Nat.div n d) e) = (num_divides n (Nat.mul d e)).
Axiom thm_DIVIDES_DIV_SELF : forall n : nat, forall d : nat, (num_divides d n) -> num_divides (Nat.div n d) n.
Axiom thm_DIVIDES_DIVIDES_DIV : forall n : nat, forall d : nat, forall e : nat, (num_divides d n) -> (num_divides e (Nat.div n d)) = (num_divides (Nat.mul d e) n).
Axiom thm_DIVIDES_DIVIDES_DIV_EQ : forall n : nat, forall d : nat, forall e : nat, ((num_divides d n) /\ (num_divides e (Nat.div n d))) = (num_divides (Nat.mul d e) n).
Axiom thm_DIVIDES_DIVIDES_DIV_IMP : forall n : nat, forall d : nat, forall e : nat, (num_divides (Nat.mul d e) n) -> num_divides e (Nat.div n d).
Axiom thm_MULT_DIV : (forall m : nat, forall n : nat, forall p : nat, (num_divides p m) -> (Nat.div (Nat.mul m n) p) = (Nat.mul (Nat.div m p) n)) /\ (forall m : nat, forall n : nat, forall p : nat, (num_divides p n) -> (Nat.div (Nat.mul m n) p) = (Nat.mul m (Nat.div n p))).
Axiom thm_COPRIME_LMOD : forall a : nat, forall n : nat, (num_coprime (@pair nat nat (Nat.modulo a n) n)) = (num_coprime (@pair nat nat a n)).
Axiom thm_COPRIME_RMOD : forall a : nat, forall n : nat, (num_coprime (@pair nat nat n (Nat.modulo a n))) = (num_coprime (@pair nat nat n a)).
Axiom thm_INT_CONG_NUM_EXISTS : forall x : Z, forall y : Z, ((y = (int_of_num (NUMERAL 0))) -> int_le (int_of_num (NUMERAL 0)) x) -> exists n : nat, @eq2 Z (int_of_num n) x (int_mod y).
Axiom thm_GCD : forall a : nat, forall b : nat, ((num_divides (num_gcd (@pair nat nat a b)) a) /\ (num_divides (num_gcd (@pair nat nat a b)) b)) /\ (forall e : nat, ((num_divides e a) /\ (num_divides e b)) -> num_divides e (num_gcd (@pair nat nat a b))).
Axiom thm_coprime : forall (a : nat) (b : nat), (num_coprime (@pair nat nat a b)) = (forall d : nat, ((num_divides d a) /\ (num_divides d b)) -> d = (NUMERAL (BIT1 0))).
Axiom thm_prime : forall p : nat, (prime p) = ((~ (p = (NUMERAL (BIT1 0)))) /\ (forall x : nat, (num_divides x p) -> (x = (NUMERAL (BIT1 0))) \/ (x = p))).
Axiom thm_ONE_OR_PRIME : forall p : nat, ((p = (NUMERAL (BIT1 0))) \/ (prime p)) = (forall n : nat, (num_divides n p) -> (n = (NUMERAL (BIT1 0))) \/ (n = p)).
Axiom thm_ONE_OR_PRIME_DIVIDES_OR_COPRIME : forall p : nat, ((p = (NUMERAL (BIT1 0))) \/ (prime p)) = (forall n : nat, (num_divides p n) \/ (num_coprime (@pair nat nat p n))).
Axiom thm_PRIME_COPRIME_EQ_NONDIVISIBLE : forall p : nat, (prime p) = (forall n : nat, (num_coprime (@pair nat nat p n)) = (~ (num_divides p n))).
Axiom thm_ZERO_ONE_OR_PRIME_DIVPROD : forall p : nat, forall a : nat, forall b : nat, ((p = (NUMERAL 0)) \/ ((p = (NUMERAL (BIT1 0))) \/ (prime p))) -> (num_divides p (Nat.mul a b)) = ((num_divides p a) \/ (num_divides p b)).
Axiom thm_ZERO_ONE_OR_PRIME : forall p : nat, ((p = (NUMERAL 0)) \/ ((p = (NUMERAL (BIT1 0))) \/ (prime p))) = (forall a : nat, forall b : nat, (num_divides p (Nat.mul a b)) -> (num_divides p a) \/ (num_divides p b)).
Axiom thm_real_zpow : forall z : R, forall i : Z, (real_zpow z i) = (@COND R (int_le (int_of_num (NUMERAL 0)) i) (Rpower_nat z (num_of_int i)) (Rinv (Rpower_nat z (num_of_int (int_neg i))))).
Axiom thm_REAL_POW_ZPOW : forall x : R, forall n : nat, (Rpower_nat x n) = (real_zpow x (int_of_num n)).
Axiom thm_REAL_ZPOW_NUM : forall x : R, forall n : nat, (real_zpow x (int_of_num n)) = (Rpower_nat x n).
Axiom thm_REAL_ZPOW_0 : forall x : R, (real_zpow x (int_of_num (NUMERAL 0))) = (INR (NUMERAL (BIT1 0))).
Axiom thm_REAL_ZPOW_1 : forall x : R, (real_zpow x (int_of_num (NUMERAL (BIT1 0)))) = x.
Axiom thm_REAL_ZPOW_2 : forall x : R, (real_zpow x (int_of_num (NUMERAL (BIT0 (BIT1 0))))) = (Rmult x x).
Axiom thm_REAL_ZPOW_ONE : forall n : Z, (real_zpow (INR (NUMERAL (BIT1 0))) n) = (INR (NUMERAL (BIT1 0))).
Axiom thm_REAL_ZPOW_NEG : forall x : R, forall n : Z, (real_zpow x (int_neg n)) = (Rinv (real_zpow x n)).
Axiom thm_REAL_ZPOW_MINUS1 : forall x : R, (real_zpow x (int_neg (int_of_num (NUMERAL (BIT1 0))))) = (Rinv x).
Axiom thm_REAL_ZPOW_ZERO : forall n : Z, (real_zpow (INR (NUMERAL 0)) n) = (@COND R (n = (int_of_num (NUMERAL 0))) (INR (NUMERAL (BIT1 0))) (INR (NUMERAL 0))).
Axiom thm_REAL_ZPOW_POW : (forall x : R, forall n : nat, (real_zpow x (int_of_num n)) = (Rpower_nat x n)) /\ (forall x : R, forall n : nat, (real_zpow x (int_neg (int_of_num n))) = (Rinv (Rpower_nat x n))).
Axiom thm_REAL_INV_ZPOW : forall x : R, forall n : Z, (Rinv (real_zpow x n)) = (real_zpow (Rinv x) n).
Axiom thm_REAL_ZPOW_INV : forall x : R, forall n : Z, (real_zpow (Rinv x) n) = (Rinv (real_zpow x n)).
Axiom thm_REAL_ZPOW_ZPOW : forall x : R, forall m : Z, forall n : Z, (real_zpow (real_zpow x m) n) = (real_zpow x (int_mul m n)).
Axiom thm_REAL_ZPOW_MUL : forall x : R, forall y : R, forall n : Z, (real_zpow (Rmult x y) n) = (Rmult (real_zpow x n) (real_zpow y n)).
Axiom thm_REAL_ZPOW_DIV : forall x : R, forall y : R, forall n : Z, (real_zpow (Rdiv x y) n) = (Rdiv (real_zpow x n) (real_zpow y n)).
Axiom thm_REAL_ZPOW_ADD : forall x : R, forall m : Z, forall n : Z, (~ (x = (INR (NUMERAL 0)))) -> (real_zpow x (int_add m n)) = (Rmult (real_zpow x m) (real_zpow x n)).
Axiom thm_REAL_ZPOW_SUB : forall x : R, forall m : Z, forall n : Z, (~ (x = (INR (NUMERAL 0)))) -> (real_zpow x (int_sub m n)) = (Rdiv (real_zpow x m) (real_zpow x n)).
Axiom thm_REAL_ZPOW_LE : forall x : R, forall n : Z, (Rle (INR (NUMERAL 0)) x) -> Rle (INR (NUMERAL 0)) (real_zpow x n).
Axiom thm_REAL_ZPOW_LT : forall x : R, forall n : Z, (Rlt (INR (NUMERAL 0)) x) -> Rlt (INR (NUMERAL 0)) (real_zpow x n).
Axiom thm_REAL_ZPOW_EQ_0 : forall x : R, forall n : Z, ((real_zpow x n) = (INR (NUMERAL 0))) = ((x = (INR (NUMERAL 0))) /\ (~ (n = (int_of_num (NUMERAL 0))))).
Axiom thm_REAL_ABS_ZPOW : forall x : R, forall n : Z, (Rabs (real_zpow x n)) = (real_zpow (Rabs x) n).
Axiom thm_REAL_SGN_ZPOW : forall x : R, forall n : Z, (real_sgn (real_zpow x n)) = (real_zpow (real_sgn x) n).
Axiom thm_IN : forall {A : Type'}, forall P : A -> Prop, forall x : A, (@IN A x P) = (P x).
Axiom thm_EXTENSION : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, (s = t) = (forall x : A, (@IN A x s) = (@IN A x t)).
Axiom thm_GSPEC : forall {A : Type'}, forall p : A -> Prop, (@GSPEC A p) = p.
Axiom thm_SETSPEC : forall {A : Type'}, forall P : Prop, forall v : A, forall t : A, (@SETSPEC A v P t) = (P /\ (v = t)).
Axiom thm_IN_ELIM_THM : forall {A : Type'}, (forall P : (Prop -> A -> Prop) -> Prop, forall x : A, (@IN A x (@GSPEC A (fun v : A => P (@SETSPEC A v)))) = (P (fun p : Prop => fun t : A => p /\ (x = t)))) /\ ((forall p : A -> Prop, forall x : A, (@IN A x (@GSPEC A (fun v : A => exists y : A, @SETSPEC A v (p y) y))) = (p x)) /\ ((forall P : (Prop -> A -> Prop) -> Prop, forall x : A, (@GSPEC A (fun v : A => P (@SETSPEC A v)) x) = (P (fun p : Prop => fun t : A => p /\ (x = t)))) /\ ((forall p : A -> Prop, forall x : A, (@GSPEC A (fun v : A => exists y : A, @SETSPEC A v (p y) y) x) = (p x)) /\ (forall p : A -> Prop, forall x : A, (@IN A x (fun y : A => p y)) = (p x))))).
Axiom thm_EMPTY : forall {A : Type'}, (@EMPTY A) = (fun x : A => False).
Axiom thm_INSERT_DEF : forall {A : Type'}, forall s : A -> Prop, forall x : A, (@INSERT A x s) = (fun y : A => (@IN A y s) \/ (y = x)).
Axiom thm_UNIV : forall {A : Type'}, (@UNIV A) = (fun x : A => True).
Axiom thm_UNION : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, (@UNION A s t) = (@GSPEC A (fun GEN_PVAR_0 : A => exists x : A, @SETSPEC A GEN_PVAR_0 ((@IN A x s) \/ (@IN A x t)) x)).
Axiom thm_UNIONS : forall {A : Type'}, forall s : (A -> Prop) -> Prop, (@UNIONS A s) = (@GSPEC A (fun GEN_PVAR_1 : A => exists x : A, @SETSPEC A GEN_PVAR_1 (exists u : A -> Prop, (@IN (A -> Prop) u s) /\ (@IN A x u)) x)).
Axiom thm_INTER : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, (@INTER A s t) = (@GSPEC A (fun GEN_PVAR_2 : A => exists x : A, @SETSPEC A GEN_PVAR_2 ((@IN A x s) /\ (@IN A x t)) x)).
Axiom thm_INTERS : forall {A : Type'}, forall s : (A -> Prop) -> Prop, (@INTERS A s) = (@GSPEC A (fun GEN_PVAR_3 : A => exists x : A, @SETSPEC A GEN_PVAR_3 (forall u : A -> Prop, (@IN (A -> Prop) u s) -> @IN A x u) x)).
Axiom thm_DIFF : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, (@DIFF A s t) = (@GSPEC A (fun GEN_PVAR_4 : A => exists x : A, @SETSPEC A GEN_PVAR_4 ((@IN A x s) /\ (~ (@IN A x t))) x)).
Axiom thm_INSERT : forall {A : Type'} (s : A -> Prop) (x : A), (@INSERT A x s) = (@GSPEC A (fun GEN_PVAR_5 : A => exists y : A, @SETSPEC A GEN_PVAR_5 ((@IN A y s) \/ (y = x)) y)).
Axiom thm_DELETE : forall {A : Type'}, forall s : A -> Prop, forall x : A, (@DELETE A s x) = (@GSPEC A (fun GEN_PVAR_6 : A => exists y : A, @SETSPEC A GEN_PVAR_6 ((@IN A y s) /\ (~ (y = x))) y)).
Axiom thm_SUBSET : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, (@SUBSET A s t) = (forall x : A, (@IN A x s) -> @IN A x t).
Axiom thm_PSUBSET : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, (@PSUBSET A s t) = ((@SUBSET A s t) /\ (~ (s = t))).
Axiom thm_DISJOINT : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, (@DISJOINT A s t) = ((@INTER A s t) = (@EMPTY A)).
Axiom thm_SING : forall {A : Type'}, forall s : A -> Prop, (@SING A s) = (exists x : A, s = (@INSERT A x (@EMPTY A))).
Axiom thm_FINITE_RULES : forall {A : Type'}, (@FINITE A (@EMPTY A)) /\ (forall x : A, forall s : A -> Prop, (@FINITE A s) -> @FINITE A (@INSERT A x s)).
Axiom thm_FINITE_CASES : forall {A : Type'}, forall a : A -> Prop, (@FINITE A a) = ((a = (@EMPTY A)) \/ (exists x : A, exists s : A -> Prop, (a = (@INSERT A x s)) /\ (@FINITE A s))).
Axiom thm_FINITE_INDUCT : forall {A : Type'}, forall FINITE' : (A -> Prop) -> Prop, ((FINITE' (@EMPTY A)) /\ (forall x : A, forall s : A -> Prop, (FINITE' s) -> FINITE' (@INSERT A x s))) -> forall a : A -> Prop, (@FINITE A a) -> FINITE' a.
Axiom thm_INFINITE : forall {A : Type'}, forall s : A -> Prop, (@INFINITE A s) = (~ (@FINITE A s)).
Axiom thm_IMAGE : forall {A B : Type'}, forall s : A -> Prop, forall f : A -> B, (@IMAGE A B f s) = (@GSPEC B (fun GEN_PVAR_7 : B => exists y : B, @SETSPEC B GEN_PVAR_7 (exists x : A, (@IN A x s) /\ (y = (f x))) y)).
Axiom thm_INJ : forall {A B : Type'}, forall t : B -> Prop, forall s : A -> Prop, forall f : A -> B, (@INJ A B f s t) = ((forall x : A, (@IN A x s) -> @IN B (f x) t) /\ (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y)).
Axiom thm_SURJ : forall {A B : Type'}, forall t : B -> Prop, forall s : A -> Prop, forall f : A -> B, (@SURJ A B f s t) = ((forall x : A, (@IN A x s) -> @IN B (f x) t) /\ (forall x : B, (@IN B x t) -> exists y : A, (@IN A y s) /\ ((f y) = x))).
Axiom thm_BIJ : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall t : B -> Prop, (@BIJ A B f s t) = ((@INJ A B f s t) /\ (@SURJ A B f s t)).
Axiom thm_CHOICE : forall {A : Type'}, forall s : A -> Prop, (@CHOICE A s) = (@ε A (fun x : A => @IN A x s)).
Axiom thm_REST : forall {A : Type'}, forall s : A -> Prop, (@REST A s) = (@DELETE A s (@CHOICE A s)).
Axiom thm_NOT_IN_EMPTY : forall {A : Type'}, forall x : A, ~ (@IN A x (@EMPTY A)).
Axiom thm_IN_UNIV : forall {A : Type'}, forall x : A, @IN A x (@UNIV A).
Axiom thm_IN_UNION : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall x : A, (@IN A x (@UNION A s t)) = ((@IN A x s) \/ (@IN A x t)).
Axiom thm_IN_UNIONS : forall {A : Type'}, forall s : (A -> Prop) -> Prop, forall x : A, (@IN A x (@UNIONS A s)) = (exists t : A -> Prop, (@IN (A -> Prop) t s) /\ (@IN A x t)).
Axiom thm_IN_INTER : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall x : A, (@IN A x (@INTER A s t)) = ((@IN A x s) /\ (@IN A x t)).
Axiom thm_IN_INTERS : forall {A : Type'}, forall s : (A -> Prop) -> Prop, forall x : A, (@IN A x (@INTERS A s)) = (forall t : A -> Prop, (@IN (A -> Prop) t s) -> @IN A x t).
Axiom thm_IN_DIFF : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall x : A, (@IN A x (@DIFF A s t)) = ((@IN A x s) /\ (~ (@IN A x t))).
Axiom thm_IN_INSERT : forall {A : Type'}, forall x : A, forall y : A, forall s : A -> Prop, (@IN A x (@INSERT A y s)) = ((x = y) \/ (@IN A x s)).
Axiom thm_IN_DELETE : forall {A : Type'}, forall s : A -> Prop, forall x : A, forall y : A, (@IN A x (@DELETE A s y)) = ((@IN A x s) /\ (~ (x = y))).
Axiom thm_IN_SING : forall {A : Type'}, forall x : A, forall y : A, (@IN A x (@INSERT A y (@EMPTY A))) = (x = y).
Axiom thm_IN_IMAGE : forall {A B : Type'}, forall y : B, forall s : A -> Prop, forall f : A -> B, (@IN B y (@IMAGE A B f s)) = (exists x : A, (y = (f x)) /\ (@IN A x s)).
Axiom thm_IN_REST : forall {A : Type'}, forall x : A, forall s : A -> Prop, (@IN A x (@REST A s)) = ((@IN A x s) /\ (~ (x = (@CHOICE A s)))).
Axiom thm_FORALL_IN_INSERT : forall {A : Type'}, forall P : A -> Prop, forall a : A, forall s : A -> Prop, (forall x : A, (@IN A x (@INSERT A a s)) -> P x) = ((P a) /\ (forall x : A, (@IN A x s) -> P x)).
Axiom thm_EXISTS_IN_INSERT : forall {A : Type'}, forall P : A -> Prop, forall a : A, forall s : A -> Prop, (exists x : A, (@IN A x (@INSERT A a s)) /\ (P x)) = ((P a) \/ (exists x : A, (@IN A x s) /\ (P x))).
Axiom thm_FORALL_IN_UNION : forall {A : Type'}, forall P : A -> Prop, forall s : A -> Prop, forall t : A -> Prop, (forall x : A, (@IN A x (@UNION A s t)) -> P x) = ((forall x : A, (@IN A x s) -> P x) /\ (forall x : A, (@IN A x t) -> P x)).
Axiom thm_EXISTS_IN_UNION : forall {A : Type'}, forall P : A -> Prop, forall s : A -> Prop, forall t : A -> Prop, (exists x : A, (@IN A x (@UNION A s t)) /\ (P x)) = ((exists x : A, (@IN A x s) /\ (P x)) \/ (exists x : A, (@IN A x t) /\ (P x))).
Axiom thm_FORALL_IN_IMAGE : forall {A B : Type'} (P : B -> Prop), forall f : A -> B, forall s : A -> Prop, (forall y : B, (@IN B y (@IMAGE A B f s)) -> P y) = (forall x : A, (@IN A x s) -> P (f x)).
Axiom thm_EXISTS_IN_IMAGE : forall {A B : Type'} (P : B -> Prop), forall f : A -> B, forall s : A -> Prop, (exists y : B, (@IN B y (@IMAGE A B f s)) /\ (P y)) = (exists x : A, (@IN A x s) /\ (P (f x))).
Axiom thm_FORALL_IN_GSPEC : forall {A B C D E : Type'}, (forall P : A -> Prop, forall Q : B -> Prop, forall f : A -> B, (forall z : B, (@IN B z (@GSPEC B (fun GEN_PVAR_8 : B => exists x : A, @SETSPEC B GEN_PVAR_8 (P x) (f x)))) -> Q z) = (forall x : A, (P x) -> Q (f x))) /\ ((forall P : A -> B -> Prop, forall Q : C -> Prop, forall f : A -> B -> C, (forall z : C, (@IN C z (@GSPEC C (fun GEN_PVAR_9 : C => exists x : A, exists y : B, @SETSPEC C GEN_PVAR_9 (P x y) (f x y)))) -> Q z) = (forall x : A, forall y : B, (P x y) -> Q (f x y))) /\ ((forall P : A -> B -> C -> Prop, forall Q : D -> Prop, forall f : A -> B -> C -> D, (forall z : D, (@IN D z (@GSPEC D (fun GEN_PVAR_10 : D => exists w : A, exists x : B, exists y : C, @SETSPEC D GEN_PVAR_10 (P w x y) (f w x y)))) -> Q z) = (forall w : A, forall x : B, forall y : C, (P w x y) -> Q (f w x y))) /\ (forall P : A -> B -> C -> D -> Prop, forall Q : E -> Prop, forall f : A -> B -> C -> D -> E, (forall z : E, (@IN E z (@GSPEC E (fun GEN_PVAR_11 : E => exists v : A, exists w : B, exists x : C, exists y : D, @SETSPEC E GEN_PVAR_11 (P v w x y) (f v w x y)))) -> Q z) = (forall v : A, forall w : B, forall x : C, forall y : D, (P v w x y) -> Q (f v w x y))))).
Axiom thm_EXISTS_IN_GSPEC : forall {A B C D E : Type'}, (forall P : A -> Prop, forall Q : B -> Prop, forall f : A -> B, (exists z : B, (@IN B z (@GSPEC B (fun GEN_PVAR_12 : B => exists x : A, @SETSPEC B GEN_PVAR_12 (P x) (f x)))) /\ (Q z)) = (exists x : A, (P x) /\ (Q (f x)))) /\ ((forall P : A -> B -> Prop, forall Q : C -> Prop, forall f : A -> B -> C, (exists z : C, (@IN C z (@GSPEC C (fun GEN_PVAR_13 : C => exists x : A, exists y : B, @SETSPEC C GEN_PVAR_13 (P x y) (f x y)))) /\ (Q z)) = (exists x : A, exists y : B, (P x y) /\ (Q (f x y)))) /\ ((forall P : A -> B -> C -> Prop, forall Q : D -> Prop, forall f : A -> B -> C -> D, (exists z : D, (@IN D z (@GSPEC D (fun GEN_PVAR_14 : D => exists w : A, exists x : B, exists y : C, @SETSPEC D GEN_PVAR_14 (P w x y) (f w x y)))) /\ (Q z)) = (exists w : A, exists x : B, exists y : C, (P w x y) /\ (Q (f w x y)))) /\ (forall P : A -> B -> C -> D -> Prop, forall Q : E -> Prop, forall f : A -> B -> C -> D -> E, (exists z : E, (@IN E z (@GSPEC E (fun GEN_PVAR_15 : E => exists v : A, exists w : B, exists x : C, exists y : D, @SETSPEC E GEN_PVAR_15 (P v w x y) (f v w x y)))) /\ (Q z)) = (exists v : A, exists w : B, exists x : C, exists y : D, (P v w x y) /\ (Q (f v w x y)))))).
Axiom thm_UNIONS_IMAGE : forall {A B : Type'}, forall f : A -> B -> Prop, forall s : A -> Prop, (@UNIONS B (@IMAGE A (B -> Prop) f s)) = (@GSPEC B (fun GEN_PVAR_16 : B => exists y : B, @SETSPEC B GEN_PVAR_16 (exists x : A, (@IN A x s) /\ (@IN B y (f x))) y)).
Axiom thm_INTERS_IMAGE : forall {A B : Type'}, forall f : A -> B -> Prop, forall s : A -> Prop, (@INTERS B (@IMAGE A (B -> Prop) f s)) = (@GSPEC B (fun GEN_PVAR_17 : B => exists y : B, @SETSPEC B GEN_PVAR_17 (forall x : A, (@IN A x s) -> @IN B y (f x)) y)).
Axiom thm_UNIONS_GSPEC : forall {A B C D : Type'}, (forall P : A -> Prop, forall f : A -> B -> Prop, (@UNIONS B (@GSPEC (B -> Prop) (fun GEN_PVAR_18 : B -> Prop => exists x : A, @SETSPEC (B -> Prop) GEN_PVAR_18 (P x) (f x)))) = (@GSPEC B (fun GEN_PVAR_19 : B => exists a : B, @SETSPEC B GEN_PVAR_19 (exists x : A, (P x) /\ (@IN B a (f x))) a))) /\ ((forall P : A -> B -> Prop, forall f : A -> B -> C -> Prop, (@UNIONS C (@GSPEC (C -> Prop) (fun GEN_PVAR_20 : C -> Prop => exists x : A, exists y : B, @SETSPEC (C -> Prop) GEN_PVAR_20 (P x y) (f x y)))) = (@GSPEC C (fun GEN_PVAR_21 : C => exists a : C, @SETSPEC C GEN_PVAR_21 (exists x : A, exists y : B, (P x y) /\ (@IN C a (f x y))) a))) /\ (forall P : A -> B -> C -> Prop, forall f : A -> B -> C -> D -> Prop, (@UNIONS D (@GSPEC (D -> Prop) (fun GEN_PVAR_22 : D -> Prop => exists x : A, exists y : B, exists z : C, @SETSPEC (D -> Prop) GEN_PVAR_22 (P x y z) (f x y z)))) = (@GSPEC D (fun GEN_PVAR_23 : D => exists a : D, @SETSPEC D GEN_PVAR_23 (exists x : A, exists y : B, exists z : C, (P x y z) /\ (@IN D a (f x y z))) a)))).
Axiom thm_INTERS_GSPEC : forall {A B C D : Type'}, (forall P : A -> Prop, forall f : A -> B -> Prop, (@INTERS B (@GSPEC (B -> Prop) (fun GEN_PVAR_24 : B -> Prop => exists x : A, @SETSPEC (B -> Prop) GEN_PVAR_24 (P x) (f x)))) = (@GSPEC B (fun GEN_PVAR_25 : B => exists a : B, @SETSPEC B GEN_PVAR_25 (forall x : A, (P x) -> @IN B a (f x)) a))) /\ ((forall P : A -> B -> Prop, forall f : A -> B -> C -> Prop, (@INTERS C (@GSPEC (C -> Prop) (fun GEN_PVAR_26 : C -> Prop => exists x : A, exists y : B, @SETSPEC (C -> Prop) GEN_PVAR_26 (P x y) (f x y)))) = (@GSPEC C (fun GEN_PVAR_27 : C => exists a : C, @SETSPEC C GEN_PVAR_27 (forall x : A, forall y : B, (P x y) -> @IN C a (f x y)) a))) /\ (forall P : A -> B -> C -> Prop, forall f : A -> B -> C -> D -> Prop, (@INTERS D (@GSPEC (D -> Prop) (fun GEN_PVAR_28 : D -> Prop => exists x : A, exists y : B, exists z : C, @SETSPEC (D -> Prop) GEN_PVAR_28 (P x y z) (f x y z)))) = (@GSPEC D (fun GEN_PVAR_29 : D => exists a : D, @SETSPEC D GEN_PVAR_29 (forall x : A, forall y : B, forall z : C, (P x y z) -> @IN D a (f x y z)) a)))).
Axiom thm_CHOICE_DEF : forall {A : Type'}, forall s : A -> Prop, (~ (s = (@EMPTY A))) -> @IN A (@CHOICE A s) s.
Axiom thm_NOT_EQUAL_SETS : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, (~ (s = t)) = (exists x : A, (@IN A x t) = (~ (@IN A x s))).
Axiom thm_INSERT_RESTRICT : forall {A : Type'}, forall P : A -> Prop, forall s : A -> Prop, forall a : A, (@GSPEC A (fun GEN_PVAR_30 : A => exists x : A, @SETSPEC A GEN_PVAR_30 ((@IN A x (@INSERT A a s)) /\ (P x)) x)) = (@COND (A -> Prop) (P a) (@INSERT A a (@GSPEC A (fun GEN_PVAR_31 : A => exists x : A, @SETSPEC A GEN_PVAR_31 ((@IN A x s) /\ (P x)) x))) (@GSPEC A (fun GEN_PVAR_32 : A => exists x : A, @SETSPEC A GEN_PVAR_32 ((@IN A x s) /\ (P x)) x))).
Axiom thm_UNIV_1 : (@UNIV unit) = (@INSERT unit tt (@EMPTY unit)).
Axiom thm_MEMBER_NOT_EMPTY : forall {A : Type'}, forall s : A -> Prop, (exists x : A, @IN A x s) = (~ (s = (@EMPTY A))).
Axiom thm_UNIV_NOT_EMPTY : forall {A : Type'}, ~ ((@UNIV A) = (@EMPTY A)).
Axiom thm_EMPTY_NOT_UNIV : forall {A : Type'}, ~ ((@EMPTY A) = (@UNIV A)).
Axiom thm_EQ_UNIV : forall {A : Type'} (s : A -> Prop), (forall x : A, @IN A x s) = (s = (@UNIV A)).
Axiom thm_SUBSET_TRANS : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall u : A -> Prop, ((@SUBSET A s t) /\ (@SUBSET A t u)) -> @SUBSET A s u.
Axiom thm_SUBSET_REFL : forall {A : Type'}, forall s : A -> Prop, @SUBSET A s s.
Axiom thm_SUBSET_ANTISYM : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, ((@SUBSET A s t) /\ (@SUBSET A t s)) -> s = t.
Axiom thm_SUBSET_ANTISYM_EQ : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, ((@SUBSET A s t) /\ (@SUBSET A t s)) = (s = t).
Axiom thm_EMPTY_SUBSET : forall {A : Type'}, forall s : A -> Prop, @SUBSET A (@EMPTY A) s.
Axiom thm_SUBSET_EMPTY : forall {A : Type'}, forall s : A -> Prop, (@SUBSET A s (@EMPTY A)) = (s = (@EMPTY A)).
Axiom thm_SUBSET_UNIV : forall {A : Type'}, forall s : A -> Prop, @SUBSET A s (@UNIV A).
Axiom thm_UNIV_SUBSET : forall {A : Type'}, forall s : A -> Prop, (@SUBSET A (@UNIV A) s) = (s = (@UNIV A)).
Axiom thm_SING_SUBSET : forall {A : Type'}, forall s : A -> Prop, forall x : A, (@SUBSET A (@INSERT A x (@EMPTY A)) s) = (@IN A x s).
Axiom thm_SUBSET_RESTRICT : forall {A : Type'}, forall s : A -> Prop, forall P : A -> Prop, @SUBSET A (@GSPEC A (fun GEN_PVAR_33 : A => exists x : A, @SETSPEC A GEN_PVAR_33 ((@IN A x s) /\ (P x)) x)) s.
Axiom thm_PSUBSET_TRANS : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall u : A -> Prop, ((@PSUBSET A s t) /\ (@PSUBSET A t u)) -> @PSUBSET A s u.
Axiom thm_PSUBSET_SUBSET_TRANS : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall u : A -> Prop, ((@PSUBSET A s t) /\ (@SUBSET A t u)) -> @PSUBSET A s u.
Axiom thm_SUBSET_PSUBSET_TRANS : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall u : A -> Prop, ((@SUBSET A s t) /\ (@PSUBSET A t u)) -> @PSUBSET A s u.
Axiom thm_PSUBSET_IRREFL : forall {A : Type'}, forall s : A -> Prop, ~ (@PSUBSET A s s).
Axiom thm_NOT_PSUBSET_EMPTY : forall {A : Type'}, forall s : A -> Prop, ~ (@PSUBSET A s (@EMPTY A)).
Axiom thm_NOT_UNIV_PSUBSET : forall {A : Type'}, forall s : A -> Prop, ~ (@PSUBSET A (@UNIV A) s).
Axiom thm_PSUBSET_UNIV : forall {A : Type'}, forall s : A -> Prop, (@PSUBSET A s (@UNIV A)) = (exists x : A, ~ (@IN A x s)).
Axiom thm_PSUBSET_ALT : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, (@PSUBSET A s t) = ((@SUBSET A s t) /\ (exists a : A, (@IN A a t) /\ (~ (@IN A a s)))).
Axiom thm_UNION_ASSOC : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall u : A -> Prop, (@UNION A (@UNION A s t) u) = (@UNION A s (@UNION A t u)).
Axiom thm_UNION_IDEMPOT : forall {A : Type'}, forall s : A -> Prop, (@UNION A s s) = s.
Axiom thm_UNION_COMM : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, (@UNION A s t) = (@UNION A t s).
Axiom thm_SUBSET_UNION : forall {A : Type'}, (forall s : A -> Prop, forall t : A -> Prop, @SUBSET A s (@UNION A s t)) /\ (forall s : A -> Prop, forall t : A -> Prop, @SUBSET A s (@UNION A t s)).
Axiom thm_SUBSET_UNION_ABSORPTION : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, (@SUBSET A s t) = ((@UNION A s t) = t).
Axiom thm_UNION_EMPTY : forall {A : Type'}, (forall s : A -> Prop, (@UNION A (@EMPTY A) s) = s) /\ (forall s : A -> Prop, (@UNION A s (@EMPTY A)) = s).
Axiom thm_UNION_UNIV : forall {A : Type'}, (forall s : A -> Prop, (@UNION A (@UNIV A) s) = (@UNIV A)) /\ (forall s : A -> Prop, (@UNION A s (@UNIV A)) = (@UNIV A)).
Axiom thm_EMPTY_UNION : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, ((@UNION A s t) = (@EMPTY A)) = ((s = (@EMPTY A)) /\ (t = (@EMPTY A))).
Axiom thm_UNION_SUBSET : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall u : A -> Prop, (@SUBSET A (@UNION A s t) u) = ((@SUBSET A s u) /\ (@SUBSET A t u)).
Axiom thm_UNION_RESTRICT : forall {A : Type'}, forall P : A -> Prop, forall s : A -> Prop, forall t : A -> Prop, (@GSPEC A (fun GEN_PVAR_34 : A => exists x : A, @SETSPEC A GEN_PVAR_34 ((@IN A x (@UNION A s t)) /\ (P x)) x)) = (@UNION A (@GSPEC A (fun GEN_PVAR_35 : A => exists x : A, @SETSPEC A GEN_PVAR_35 ((@IN A x s) /\ (P x)) x)) (@GSPEC A (fun GEN_PVAR_36 : A => exists x : A, @SETSPEC A GEN_PVAR_36 ((@IN A x t) /\ (P x)) x))).
Axiom thm_FORALL_SUBSET_UNION : forall {A : Type'} (P : (A -> Prop) -> Prop), forall t : A -> Prop, forall u : A -> Prop, (forall s : A -> Prop, (@SUBSET A s (@UNION A t u)) -> P s) = (forall t' : A -> Prop, forall u' : A -> Prop, ((@SUBSET A t' t) /\ (@SUBSET A u' u)) -> P (@UNION A t' u')).
Axiom thm_EXISTS_SUBSET_UNION : forall {A : Type'} (P : (A -> Prop) -> Prop), forall t : A -> Prop, forall u : A -> Prop, (exists s : A -> Prop, (@SUBSET A s (@UNION A t u)) /\ (P s)) = (exists t' : A -> Prop, exists u' : A -> Prop, (@SUBSET A t' t) /\ ((@SUBSET A u' u) /\ (P (@UNION A t' u')))).
Axiom thm_FORALL_SUBSET_INSERT : forall {A : Type'} (P : (A -> Prop) -> Prop), forall a : A, forall t : A -> Prop, (forall s : A -> Prop, (@SUBSET A s (@INSERT A a t)) -> P s) = (forall s : A -> Prop, (@SUBSET A s t) -> (P s) /\ (P (@INSERT A a s))).
Axiom thm_EXISTS_SUBSET_INSERT : forall {A : Type'} (P : (A -> Prop) -> Prop), forall a : A, forall t : A -> Prop, (exists s : A -> Prop, (@SUBSET A s (@INSERT A a t)) /\ (P s)) = (exists s : A -> Prop, (@SUBSET A s t) /\ ((P s) \/ (P (@INSERT A a s)))).
Axiom thm_INTER_ASSOC : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall u : A -> Prop, (@INTER A (@INTER A s t) u) = (@INTER A s (@INTER A t u)).
Axiom thm_INTER_IDEMPOT : forall {A : Type'}, forall s : A -> Prop, (@INTER A s s) = s.
Axiom thm_INTER_COMM : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, (@INTER A s t) = (@INTER A t s).
Axiom thm_INTER_SUBSET : forall {A : Type'}, (forall s : A -> Prop, forall t : A -> Prop, @SUBSET A (@INTER A s t) s) /\ (forall s : A -> Prop, forall t : A -> Prop, @SUBSET A (@INTER A t s) s).
Axiom thm_SUBSET_INTER_ABSORPTION : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, (@SUBSET A s t) = ((@INTER A s t) = s).
Axiom thm_INTER_EMPTY : forall {A : Type'}, (forall s : A -> Prop, (@INTER A (@EMPTY A) s) = (@EMPTY A)) /\ (forall s : A -> Prop, (@INTER A s (@EMPTY A)) = (@EMPTY A)).
Axiom thm_INTER_UNIV : forall {A : Type'}, (forall s : A -> Prop, (@INTER A (@UNIV A) s) = s) /\ (forall s : A -> Prop, (@INTER A s (@UNIV A)) = s).
Axiom thm_SUBSET_INTER : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall u : A -> Prop, (@SUBSET A s (@INTER A t u)) = ((@SUBSET A s t) /\ (@SUBSET A s u)).
Axiom thm_INTER_RESTRICT : forall {A : Type'}, forall P : A -> Prop, forall s : A -> Prop, forall t : A -> Prop, (@GSPEC A (fun GEN_PVAR_37 : A => exists x : A, @SETSPEC A GEN_PVAR_37 ((@IN A x (@INTER A s t)) /\ (P x)) x)) = (@INTER A (@GSPEC A (fun GEN_PVAR_38 : A => exists x : A, @SETSPEC A GEN_PVAR_38 ((@IN A x s) /\ (P x)) x)) (@GSPEC A (fun GEN_PVAR_39 : A => exists x : A, @SETSPEC A GEN_PVAR_39 ((@IN A x t) /\ (P x)) x))).
Axiom thm_UNION_OVER_INTER : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall u : A -> Prop, (@INTER A s (@UNION A t u)) = (@UNION A (@INTER A s t) (@INTER A s u)).
Axiom thm_INTER_OVER_UNION : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall u : A -> Prop, (@UNION A s (@INTER A t u)) = (@INTER A (@UNION A s t) (@UNION A s u)).
Axiom thm_IN_DISJOINT : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, (@DISJOINT A s t) = (~ (exists x : A, (@IN A x s) /\ (@IN A x t))).
Axiom thm_DISJOINT_SYM : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, (@DISJOINT A s t) = (@DISJOINT A t s).
Axiom thm_DISJOINT_EMPTY : forall {A : Type'}, forall s : A -> Prop, (@DISJOINT A (@EMPTY A) s) /\ (@DISJOINT A s (@EMPTY A)).
Axiom thm_DISJOINT_EMPTY_REFL : forall {A : Type'}, forall s : A -> Prop, (s = (@EMPTY A)) = (@DISJOINT A s s).
Axiom thm_DISJOINT_UNION : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall u : A -> Prop, (@DISJOINT A (@UNION A s t) u) = ((@DISJOINT A s u) /\ (@DISJOINT A t u)).
Axiom thm_DISJOINT_SING : forall {A : Type'}, (forall s : A -> Prop, forall a : A, (@DISJOINT A s (@INSERT A a (@EMPTY A))) = (~ (@IN A a s))) /\ (forall s : A -> Prop, forall a : A, (@DISJOINT A (@INSERT A a (@EMPTY A)) s) = (~ (@IN A a s))).
Axiom thm_DIFF_EMPTY : forall {A : Type'}, forall s : A -> Prop, (@DIFF A s (@EMPTY A)) = s.
Axiom thm_EMPTY_DIFF : forall {A : Type'}, forall s : A -> Prop, (@DIFF A (@EMPTY A) s) = (@EMPTY A).
Axiom thm_DIFF_UNIV : forall {A : Type'}, forall s : A -> Prop, (@DIFF A s (@UNIV A)) = (@EMPTY A).
Axiom thm_DIFF_DIFF : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, (@DIFF A (@DIFF A s t) t) = (@DIFF A s t).
Axiom thm_DIFF_EQ_EMPTY : forall {A : Type'}, forall s : A -> Prop, (@DIFF A s s) = (@EMPTY A).
Axiom thm_SUBSET_DIFF : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, @SUBSET A (@DIFF A s t) s.
Axiom thm_COMPL_COMPL : forall {A : Type'}, forall s : A -> Prop, (@DIFF A (@UNIV A) (@DIFF A (@UNIV A) s)) = s.
Axiom thm_DIFF_RESTRICT : forall {A : Type'}, forall P : A -> Prop, forall s : A -> Prop, forall t : A -> Prop, (@GSPEC A (fun GEN_PVAR_40 : A => exists x : A, @SETSPEC A GEN_PVAR_40 ((@IN A x (@DIFF A s t)) /\ (P x)) x)) = (@DIFF A (@GSPEC A (fun GEN_PVAR_41 : A => exists x : A, @SETSPEC A GEN_PVAR_41 ((@IN A x s) /\ (P x)) x)) (@GSPEC A (fun GEN_PVAR_42 : A => exists x : A, @SETSPEC A GEN_PVAR_42 ((@IN A x t) /\ (P x)) x))).
Axiom thm_COMPONENT : forall {A : Type'}, forall x : A, forall s : A -> Prop, @IN A x (@INSERT A x s).
Axiom thm_DECOMPOSITION : forall {A : Type'}, forall s : A -> Prop, forall x : A, (@IN A x s) = (exists t : A -> Prop, (s = (@INSERT A x t)) /\ (~ (@IN A x t))).
Axiom thm_SET_CASES : forall {A : Type'}, forall s : A -> Prop, (s = (@EMPTY A)) \/ (exists x : A, exists t : A -> Prop, (s = (@INSERT A x t)) /\ (~ (@IN A x t))).
Axiom thm_ABSORPTION : forall {A : Type'}, forall x : A, forall s : A -> Prop, (@IN A x s) = ((@INSERT A x s) = s).
Axiom thm_INSERT_INSERT : forall {A : Type'}, forall x : A, forall s : A -> Prop, (@INSERT A x (@INSERT A x s)) = (@INSERT A x s).
Axiom thm_INSERT_COMM : forall {A : Type'}, forall x : A, forall y : A, forall s : A -> Prop, (@INSERT A x (@INSERT A y s)) = (@INSERT A y (@INSERT A x s)).
Axiom thm_INSERT_UNIV : forall {A : Type'}, forall x : A, (@INSERT A x (@UNIV A)) = (@UNIV A).
Axiom thm_NOT_INSERT_EMPTY : forall {A : Type'}, forall x : A, forall s : A -> Prop, ~ ((@INSERT A x s) = (@EMPTY A)).
Axiom thm_NOT_EMPTY_INSERT : forall {A : Type'}, forall x : A, forall s : A -> Prop, ~ ((@EMPTY A) = (@INSERT A x s)).
Axiom thm_INSERT_UNION : forall {A : Type'}, forall x : A, forall s : A -> Prop, forall t : A -> Prop, (@UNION A (@INSERT A x s) t) = (@COND (A -> Prop) (@IN A x t) (@UNION A s t) (@INSERT A x (@UNION A s t))).
Axiom thm_INSERT_UNION_EQ : forall {A : Type'}, forall x : A, forall s : A -> Prop, forall t : A -> Prop, (@UNION A (@INSERT A x s) t) = (@INSERT A x (@UNION A s t)).
Axiom thm_INSERT_INTER : forall {A : Type'}, forall x : A, forall s : A -> Prop, forall t : A -> Prop, (@INTER A (@INSERT A x s) t) = (@COND (A -> Prop) (@IN A x t) (@INSERT A x (@INTER A s t)) (@INTER A s t)).
Axiom thm_DISJOINT_INSERT : forall {A : Type'}, forall x : A, forall s : A -> Prop, forall t : A -> Prop, (@DISJOINT A (@INSERT A x s) t) = ((@DISJOINT A s t) /\ (~ (@IN A x t))).
Axiom thm_INSERT_SUBSET : forall {A : Type'}, forall x : A, forall s : A -> Prop, forall t : A -> Prop, (@SUBSET A (@INSERT A x s) t) = ((@IN A x t) /\ (@SUBSET A s t)).
Axiom thm_SUBSET_INSERT : forall {A : Type'}, forall x : A, forall s : A -> Prop, (~ (@IN A x s)) -> forall t : A -> Prop, (@SUBSET A s (@INSERT A x t)) = (@SUBSET A s t).
Axiom thm_INSERT_DIFF : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall x : A, (@DIFF A (@INSERT A x s) t) = (@COND (A -> Prop) (@IN A x t) (@DIFF A s t) (@INSERT A x (@DIFF A s t))).
Axiom thm_INSERT_AC : forall {A : Type'} (y : A) (x : A) (s : A -> Prop), ((@INSERT A x (@INSERT A y s)) = (@INSERT A y (@INSERT A x s))) /\ ((@INSERT A x (@INSERT A x s)) = (@INSERT A x s)).
Axiom thm_INTER_ACI : forall {A : Type'} (r : A -> Prop) (p : A -> Prop) (q : A -> Prop), ((@INTER A p q) = (@INTER A q p)) /\ (((@INTER A (@INTER A p q) r) = (@INTER A p (@INTER A q r))) /\ (((@INTER A p (@INTER A q r)) = (@INTER A q (@INTER A p r))) /\ (((@INTER A p p) = p) /\ ((@INTER A p (@INTER A p q)) = (@INTER A p q))))).
Axiom thm_UNION_ACI : forall {A : Type'} (r : A -> Prop) (p : A -> Prop) (q : A -> Prop), ((@UNION A p q) = (@UNION A q p)) /\ (((@UNION A (@UNION A p q) r) = (@UNION A p (@UNION A q r))) /\ (((@UNION A p (@UNION A q r)) = (@UNION A q (@UNION A p r))) /\ (((@UNION A p p) = p) /\ ((@UNION A p (@UNION A p q)) = (@UNION A p q))))).
Axiom thm_DELETE_NON_ELEMENT : forall {A : Type'}, forall x : A, forall s : A -> Prop, (~ (@IN A x s)) = ((@DELETE A s x) = s).
Axiom thm_IN_DELETE_EQ : forall {A : Type'}, forall s : A -> Prop, forall x : A, forall x' : A, ((@IN A x s) = (@IN A x' s)) = ((@IN A x (@DELETE A s x')) = (@IN A x' (@DELETE A s x))).
Axiom thm_EMPTY_DELETE : forall {A : Type'}, forall x : A, (@DELETE A (@EMPTY A) x) = (@EMPTY A).
Axiom thm_DELETE_DELETE : forall {A : Type'}, forall x : A, forall s : A -> Prop, (@DELETE A (@DELETE A s x) x) = (@DELETE A s x).
Axiom thm_DELETE_COMM : forall {A : Type'}, forall x : A, forall y : A, forall s : A -> Prop, (@DELETE A (@DELETE A s x) y) = (@DELETE A (@DELETE A s y) x).
Axiom thm_DELETE_SUBSET : forall {A : Type'}, forall x : A, forall s : A -> Prop, @SUBSET A (@DELETE A s x) s.
Axiom thm_SUBSET_DELETE : forall {A : Type'}, forall x : A, forall s : A -> Prop, forall t : A -> Prop, (@SUBSET A s (@DELETE A t x)) = ((~ (@IN A x s)) /\ (@SUBSET A s t)).
Axiom thm_SUBSET_INSERT_DELETE : forall {A : Type'}, forall x : A, forall s : A -> Prop, forall t : A -> Prop, (@SUBSET A s (@INSERT A x t)) = (@SUBSET A (@DELETE A s x) t).
Axiom thm_DIFF_INSERT : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall x : A, (@DIFF A s (@INSERT A x t)) = (@DIFF A (@DELETE A s x) t).
Axiom thm_PSUBSET_INSERT_SUBSET : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, (@PSUBSET A s t) = (exists x : A, (~ (@IN A x s)) /\ (@SUBSET A (@INSERT A x s) t)).
Axiom thm_DELETE_INSERT : forall {A : Type'}, forall x : A, forall y : A, forall s : A -> Prop, (@DELETE A (@INSERT A x s) y) = (@COND (A -> Prop) (x = y) (@DELETE A s y) (@INSERT A x (@DELETE A s y))).
Axiom thm_INSERT_DELETE : forall {A : Type'}, forall x : A, forall s : A -> Prop, (@IN A x s) -> (@INSERT A x (@DELETE A s x)) = s.
Axiom thm_DELETE_INTER : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall x : A, (@INTER A (@DELETE A s x) t) = (@DELETE A (@INTER A s t) x).
Axiom thm_DISJOINT_DELETE_SYM : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall x : A, (@DISJOINT A (@DELETE A s x) t) = (@DISJOINT A (@DELETE A t x) s).
Axiom thm_UNIONS_0 : forall {A : Type'}, (@UNIONS A (@EMPTY (A -> Prop))) = (@EMPTY A).
Axiom thm_UNIONS_1 : forall {A : Type'}, forall s : A -> Prop, (@UNIONS A (@INSERT (A -> Prop) s (@EMPTY (A -> Prop)))) = s.
Axiom thm_UNIONS_2 : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, (@UNIONS A (@INSERT (A -> Prop) s (@INSERT (A -> Prop) t (@EMPTY (A -> Prop))))) = (@UNION A s t).
Axiom thm_UNIONS_INSERT : forall {A : Type'}, forall s : A -> Prop, forall u : (A -> Prop) -> Prop, (@UNIONS A (@INSERT (A -> Prop) s u)) = (@UNION A s (@UNIONS A u)).
Axiom thm_FORALL_IN_UNIONS : forall {A : Type'}, forall P : A -> Prop, forall s : (A -> Prop) -> Prop, (forall x : A, (@IN A x (@UNIONS A s)) -> P x) = (forall t : A -> Prop, forall x : A, ((@IN (A -> Prop) t s) /\ (@IN A x t)) -> P x).
Axiom thm_EXISTS_IN_UNIONS : forall {A : Type'}, forall P : A -> Prop, forall s : (A -> Prop) -> Prop, (exists x : A, (@IN A x (@UNIONS A s)) /\ (P x)) = (exists t : A -> Prop, exists x : A, (@IN (A -> Prop) t s) /\ ((@IN A x t) /\ (P x))).
Axiom thm_EMPTY_UNIONS : forall {A : Type'}, forall s : (A -> Prop) -> Prop, ((@UNIONS A s) = (@EMPTY A)) = (forall t : A -> Prop, (@IN (A -> Prop) t s) -> t = (@EMPTY A)).
Axiom thm_INTER_UNIONS : forall {A : Type'}, (forall s : (A -> Prop) -> Prop, forall t : A -> Prop, (@INTER A (@UNIONS A s) t) = (@UNIONS A (@GSPEC (A -> Prop) (fun GEN_PVAR_43 : A -> Prop => exists x : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_43 (@IN (A -> Prop) x s) (@INTER A x t))))) /\ (forall s : (A -> Prop) -> Prop, forall t : A -> Prop, (@INTER A t (@UNIONS A s)) = (@UNIONS A (@GSPEC (A -> Prop) (fun GEN_PVAR_44 : A -> Prop => exists x : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_44 (@IN (A -> Prop) x s) (@INTER A t x))))).
Axiom thm_UNIONS_SUBSET : forall {A : Type'}, forall f : (A -> Prop) -> Prop, forall t : A -> Prop, (@SUBSET A (@UNIONS A f) t) = (forall s : A -> Prop, (@IN (A -> Prop) s f) -> @SUBSET A s t).
Axiom thm_SUBSET_UNIONS : forall {A : Type'}, forall f : (A -> Prop) -> Prop, forall g : (A -> Prop) -> Prop, (@SUBSET (A -> Prop) f g) -> @SUBSET A (@UNIONS A f) (@UNIONS A g).
Axiom thm_UNIONS_UNION : forall {A : Type'}, forall s : (A -> Prop) -> Prop, forall t : (A -> Prop) -> Prop, (@UNIONS A (@UNION (A -> Prop) s t)) = (@UNION A (@UNIONS A s) (@UNIONS A t)).
Axiom thm_INTERS_UNION : forall {A : Type'}, forall s : (A -> Prop) -> Prop, forall t : (A -> Prop) -> Prop, (@INTERS A (@UNION (A -> Prop) s t)) = (@INTER A (@INTERS A s) (@INTERS A t)).
Axiom thm_UNIONS_MONO : forall {A : Type'}, forall s : (A -> Prop) -> Prop, forall t : (A -> Prop) -> Prop, (forall x : A -> Prop, (@IN (A -> Prop) x s) -> exists y : A -> Prop, (@IN (A -> Prop) y t) /\ (@SUBSET A x y)) -> @SUBSET A (@UNIONS A s) (@UNIONS A t).
Axiom thm_UNIONS_MONO_IMAGE : forall {A B : Type'}, forall f : A -> B -> Prop, forall g : A -> B -> Prop, forall s : A -> Prop, (forall x : A, (@IN A x s) -> @SUBSET B (f x) (g x)) -> @SUBSET B (@UNIONS B (@IMAGE A (B -> Prop) f s)) (@UNIONS B (@IMAGE A (B -> Prop) g s)).
Axiom thm_UNIONS_UNIV : forall {A : Type'}, (@UNIONS A (@UNIV (A -> Prop))) = (@UNIV A).
Axiom thm_UNIONS_INSERT_EMPTY : forall {A : Type'}, forall s : (A -> Prop) -> Prop, (@UNIONS A (@INSERT (A -> Prop) (@EMPTY A) s)) = (@UNIONS A s).
Axiom thm_UNIONS_DELETE_EMPTY : forall {A : Type'}, forall s : (A -> Prop) -> Prop, (@UNIONS A (@DELETE (A -> Prop) s (@EMPTY A))) = (@UNIONS A s).
Axiom thm_INTERS_0 : forall {A : Type'}, (@INTERS A (@EMPTY (A -> Prop))) = (@UNIV A).
Axiom thm_INTERS_1 : forall {A : Type'}, forall s : A -> Prop, (@INTERS A (@INSERT (A -> Prop) s (@EMPTY (A -> Prop)))) = s.
Axiom thm_INTERS_2 : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, (@INTERS A (@INSERT (A -> Prop) s (@INSERT (A -> Prop) t (@EMPTY (A -> Prop))))) = (@INTER A s t).
Axiom thm_INTERS_INSERT : forall {A : Type'}, forall s : A -> Prop, forall u : (A -> Prop) -> Prop, (@INTERS A (@INSERT (A -> Prop) s u)) = (@INTER A s (@INTERS A u)).
Axiom thm_SUBSET_INTERS : forall {A : Type'}, forall s : A -> Prop, forall f : (A -> Prop) -> Prop, (@SUBSET A s (@INTERS A f)) = (forall t : A -> Prop, (@IN (A -> Prop) t f) -> @SUBSET A s t).
Axiom thm_INTERS_SUBSET : forall {A : Type'}, forall u : (A -> Prop) -> Prop, forall s : A -> Prop, ((~ (u = (@EMPTY (A -> Prop)))) /\ (forall t : A -> Prop, (@IN (A -> Prop) t u) -> @SUBSET A t s)) -> @SUBSET A (@INTERS A u) s.
Axiom thm_INTERS_SUBSET_STRONG : forall {A : Type'}, forall u : (A -> Prop) -> Prop, forall s : A -> Prop, (exists t : A -> Prop, (@IN (A -> Prop) t u) /\ (@SUBSET A t s)) -> @SUBSET A (@INTERS A u) s.
Axiom thm_INTERS_ANTIMONO : forall {A : Type'}, forall f : (A -> Prop) -> Prop, forall g : (A -> Prop) -> Prop, (@SUBSET (A -> Prop) g f) -> @SUBSET A (@INTERS A f) (@INTERS A g).
Axiom thm_INTERS_EQ_UNIV : forall {A : Type'}, forall f : (A -> Prop) -> Prop, ((@INTERS A f) = (@UNIV A)) = (forall s : A -> Prop, (@IN (A -> Prop) s f) -> s = (@UNIV A)).
Axiom thm_INTERS_ANTIMONO_GEN : forall {A : Type'}, forall s : (A -> Prop) -> Prop, forall t : (A -> Prop) -> Prop, (forall y : A -> Prop, (@IN (A -> Prop) y t) -> exists x : A -> Prop, (@IN (A -> Prop) x s) /\ (@SUBSET A x y)) -> @SUBSET A (@INTERS A s) (@INTERS A t).
Axiom thm_IMAGE_CLAUSES : forall {A B : Type'} (x : A) (f : A -> B) (s : A -> Prop), ((@IMAGE A B f (@EMPTY A)) = (@EMPTY B)) /\ ((@IMAGE A B f (@INSERT A x s)) = (@INSERT B (f x) (@IMAGE A B f s))).
Axiom thm_IMAGE_UNION : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall t : A -> Prop, (@IMAGE A B f (@UNION A s t)) = (@UNION B (@IMAGE A B f s) (@IMAGE A B f t)).
Axiom thm_IMAGE_ID : forall {A : Type'}, forall s : A -> Prop, (@IMAGE A A (fun x : A => x) s) = s.
Axiom thm_IMAGE_I : forall {A : Type'}, forall s : A -> Prop, (@IMAGE A A (@I A) s) = s.
Axiom thm_IMAGE_o : forall {A B C : Type'}, forall f : B -> C, forall g : A -> B, forall s : A -> Prop, (@IMAGE A C (@o A B C f g) s) = (@IMAGE B C f (@IMAGE A B g s)).
Axiom thm_IMAGE_SUBSET : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall t : A -> Prop, (@SUBSET A s t) -> @SUBSET B (@IMAGE A B f s) (@IMAGE A B f t).
Axiom thm_IMAGE_INTER_INJ : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall t : A -> Prop, (forall x : A, forall y : A, ((f x) = (f y)) -> x = y) -> (@IMAGE A B f (@INTER A s t)) = (@INTER B (@IMAGE A B f s) (@IMAGE A B f t)).
Axiom thm_IMAGE_DIFF_INJ : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall t : A -> Prop, (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y t) /\ ((f x) = (f y)))) -> x = y) -> (@IMAGE A B f (@DIFF A s t)) = (@DIFF B (@IMAGE A B f s) (@IMAGE A B f t)).
Axiom thm_IMAGE_DIFF_INJ_ALT : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall t : A -> Prop, ((forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y) /\ (@SUBSET A t s)) -> (@IMAGE A B f (@DIFF A s t)) = (@DIFF B (@IMAGE A B f s) (@IMAGE A B f t)).
Axiom thm_IMAGE_DELETE_INJ : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall a : A, (forall x : A, ((@IN A x s) /\ ((f x) = (f a))) -> x = a) -> (@IMAGE A B f (@DELETE A s a)) = (@DELETE B (@IMAGE A B f s) (f a)).
Axiom thm_IMAGE_DELETE_INJ_ALT : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall a : A, ((forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y) /\ (@IN A a s)) -> (@IMAGE A B f (@DELETE A s a)) = (@DELETE B (@IMAGE A B f s) (f a)).
Axiom thm_IMAGE_EQ_EMPTY : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, ((@IMAGE A B f s) = (@EMPTY B)) = (s = (@EMPTY A)).
Axiom thm_FORALL_IN_IMAGE_2 : forall {A B : Type'}, forall f : A -> B, forall P : B -> B -> Prop, forall s : A -> Prop, (forall x : B, forall y : B, ((@IN B x (@IMAGE A B f s)) /\ (@IN B y (@IMAGE A B f s))) -> P x y) = (forall x : A, forall y : A, ((@IN A x s) /\ (@IN A y s)) -> P (f x) (f y)).
Axiom thm_IMAGE_CONST : forall {A B : Type'}, forall s : A -> Prop, forall c : B, (@IMAGE A B (fun x : A => c) s) = (@COND (B -> Prop) (s = (@EMPTY A)) (@EMPTY B) (@INSERT B c (@EMPTY B))).
Axiom thm_SIMPLE_IMAGE : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, (@GSPEC B (fun GEN_PVAR_45 : B => exists x : A, @SETSPEC B GEN_PVAR_45 (@IN A x s) (f x))) = (@IMAGE A B f s).
Axiom thm_SIMPLE_IMAGE_GEN : forall {A B : Type'}, forall f : A -> B, forall P : A -> Prop, (@GSPEC B (fun GEN_PVAR_46 : B => exists x : A, @SETSPEC B GEN_PVAR_46 (P x) (f x))) = (@IMAGE A B f (@GSPEC A (fun GEN_PVAR_47 : A => exists x : A, @SETSPEC A GEN_PVAR_47 (P x) x))).
Axiom thm_IMAGE_UNIONS : forall {A B : Type'}, forall f : A -> B, forall s : (A -> Prop) -> Prop, (@IMAGE A B f (@UNIONS A s)) = (@UNIONS B (@IMAGE (A -> Prop) (B -> Prop) (@IMAGE A B f) s)).
Axiom thm_FUN_IN_IMAGE : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall x : A, (@IN A x s) -> @IN B (f x) (@IMAGE A B f s).
Axiom thm_SURJECTIVE_IMAGE_EQ : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall t : B -> Prop, ((forall y : B, (@IN B y t) -> exists x : A, (f x) = y) /\ (forall x : A, (@IN B (f x) t) = (@IN A x s))) -> (@IMAGE A B f s) = t.
Axiom thm_IMAGE_EQ : forall {A B : Type'}, forall f : A -> B, forall g : A -> B, forall s : A -> Prop, (forall x : A, (@IN A x s) -> (f x) = (g x)) -> (@IMAGE A B f s) = (@IMAGE A B g s).
Axiom thm_EMPTY_GSPEC : forall {A : Type'}, (@GSPEC A (fun GEN_PVAR_48 : A => exists x : A, @SETSPEC A GEN_PVAR_48 False x)) = (@EMPTY A).
Axiom thm_UNIV_GSPEC : forall {A : Type'}, (@GSPEC A (fun GEN_PVAR_49 : A => exists x : A, @SETSPEC A GEN_PVAR_49 True x)) = (@UNIV A).
Axiom thm_SING_GSPEC : forall {A : Type'}, (forall a : A, (@GSPEC A (fun GEN_PVAR_50 : A => exists x : A, @SETSPEC A GEN_PVAR_50 (x = a) x)) = (@INSERT A a (@EMPTY A))) /\ (forall a : A, (@GSPEC A (fun GEN_PVAR_51 : A => exists x : A, @SETSPEC A GEN_PVAR_51 (a = x) x)) = (@INSERT A a (@EMPTY A))).
Axiom thm_SING_ALT : forall {A : Type'}, forall s : A -> Prop, (exists x : A, s = (@INSERT A x (@EMPTY A))) = (@ex1 A (fun x : A => @IN A x s)).
Axiom thm_IN_GSPEC : forall {A : Type'}, forall s : A -> Prop, (@GSPEC A (fun GEN_PVAR_52 : A => exists x : A, @SETSPEC A GEN_PVAR_52 (@IN A x s) x)) = s.
Axiom thm_IN_ELIM_PAIR_THM : forall {A B : Type'}, forall P : A -> B -> Prop, forall a : A, forall b : B, (@IN (prod A B) (@pair A B a b) (@GSPEC (prod A B) (fun GEN_PVAR_53 : prod A B => exists x : A, exists y : B, @SETSPEC (prod A B) GEN_PVAR_53 (P x y) (@pair A B x y)))) = (P a b).
Axiom thm_IN_ELIM_TRIPLE_THM : forall {A B C : Type'}, (forall P : A -> B -> C -> Prop, forall a : A, forall b : B, forall c : C, (@IN (prod A (prod B C)) (@pair A (prod B C) a (@pair B C b c)) (@GSPEC (prod A (prod B C)) (fun GEN_PVAR_54 : prod A (prod B C) => exists x : A, exists y : B, exists z : C, @SETSPEC (prod A (prod B C)) GEN_PVAR_54 (P x y z) (@pair A (prod B C) x (@pair B C y z))))) = (P a b c)) /\ (forall P : A -> B -> C -> Prop, forall a : A, forall b : B, forall c : C, (@IN (prod (prod A B) C) (@pair (prod A B) C (@pair A B a b) c) (@GSPEC (prod (prod A B) C) (fun GEN_PVAR_55 : prod (prod A B) C => exists x : A, exists y : B, exists z : C, @SETSPEC (prod (prod A B) C) GEN_PVAR_55 (P x y z) (@pair (prod A B) C (@pair A B x y) z)))) = (P a b c)).
Axiom thm_IN_ELIM_QUAD_THM : forall {A B C D : Type'}, (forall P : A -> B -> C -> D -> Prop, forall a : A, forall b : B, forall c : C, forall d : D, (@IN (prod A (prod B (prod C D))) (@pair A (prod B (prod C D)) a (@pair B (prod C D) b (@pair C D c d))) (@GSPEC (prod A (prod B (prod C D))) (fun GEN_PVAR_56 : prod A (prod B (prod C D)) => exists w : A, exists x : B, exists y : C, exists z : D, @SETSPEC (prod A (prod B (prod C D))) GEN_PVAR_56 (P w x y z) (@pair A (prod B (prod C D)) w (@pair B (prod C D) x (@pair C D y z)))))) = (P a b c d)) /\ ((forall P : A -> B -> C -> D -> Prop, forall a : A, forall b : B, forall c : C, forall d : D, (@IN (prod (prod A B) (prod C D)) (@pair (prod A B) (prod C D) (@pair A B a b) (@pair C D c d)) (@GSPEC (prod (prod A B) (prod C D)) (fun GEN_PVAR_57 : prod (prod A B) (prod C D) => exists w : A, exists x : B, exists y : C, exists z : D, @SETSPEC (prod (prod A B) (prod C D)) GEN_PVAR_57 (P w x y z) (@pair (prod A B) (prod C D) (@pair A B w x) (@pair C D y z))))) = (P a b c d)) /\ (forall P : A -> B -> C -> D -> Prop, forall a : A, forall b : B, forall c : C, forall d : D, (@IN (prod (prod (prod A B) C) D) (@pair (prod (prod A B) C) D (@pair (prod A B) C (@pair A B a b) c) d) (@GSPEC (prod (prod (prod A B) C) D) (fun GEN_PVAR_58 : prod (prod (prod A B) C) D => exists w : A, exists x : B, exists y : C, exists z : D, @SETSPEC (prod (prod (prod A B) C) D) GEN_PVAR_58 (P w x y z) (@pair (prod (prod A B) C) D (@pair (prod A B) C (@pair A B w x) y) z)))) = (P a b c d))).
Axiom thm_SET_PAIR_THM : forall {A B : Type'}, forall P : (prod A B) -> Prop, (@GSPEC (prod A B) (fun GEN_PVAR_59 : prod A B => exists p : prod A B, @SETSPEC (prod A B) GEN_PVAR_59 (P p) p)) = (@GSPEC (prod A B) (fun GEN_PVAR_60 : prod A B => exists a : A, exists b : B, @SETSPEC (prod A B) GEN_PVAR_60 (P (@pair A B a b)) (@pair A B a b))).
Axiom thm_SET_PROVE_CASES : forall {A : Type'}, forall P : (A -> Prop) -> Prop, ((P (@EMPTY A)) /\ (forall a : A, forall s : A -> Prop, (~ (@IN A a s)) -> P (@INSERT A a s))) -> forall s : A -> Prop, P s.
Axiom thm_UNIONS_SINGS_GEN : forall {A : Type'}, forall P : A -> Prop, (@UNIONS A (@GSPEC (A -> Prop) (fun GEN_PVAR_61 : A -> Prop => exists x : A, @SETSPEC (A -> Prop) GEN_PVAR_61 (P x) (@INSERT A x (@EMPTY A))))) = (@GSPEC A (fun GEN_PVAR_62 : A => exists x : A, @SETSPEC A GEN_PVAR_62 (P x) x)).
Axiom thm_UNIONS_SINGS : forall {A : Type'}, forall s : A -> Prop, (@UNIONS A (@GSPEC (A -> Prop) (fun GEN_PVAR_63 : A -> Prop => exists x : A, @SETSPEC (A -> Prop) GEN_PVAR_63 (@IN A x s) (@INSERT A x (@EMPTY A))))) = s.
Axiom thm_IMAGE_INTERS : forall {A B : Type'}, forall f : A -> B, forall s : (A -> Prop) -> Prop, ((~ (s = (@EMPTY (A -> Prop)))) /\ (forall x : A, forall y : A, ((@IN A x (@UNIONS A s)) /\ ((@IN A y (@UNIONS A s)) /\ ((f x) = (f y)))) -> x = y)) -> (@IMAGE A B f (@INTERS A s)) = (@INTERS B (@IMAGE (A -> Prop) (B -> Prop) (@IMAGE A B f) s)).
Axiom thm_DIFF_INTERS : forall {A : Type'}, forall u : A -> Prop, forall s : (A -> Prop) -> Prop, (@DIFF A u (@INTERS A s)) = (@UNIONS A (@GSPEC (A -> Prop) (fun GEN_PVAR_64 : A -> Prop => exists t : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_64 (@IN (A -> Prop) t s) (@DIFF A u t)))).
Axiom thm_INTERS_UNIONS : forall {A : Type'}, forall s : (A -> Prop) -> Prop, (@INTERS A s) = (@DIFF A (@UNIV A) (@UNIONS A (@GSPEC (A -> Prop) (fun GEN_PVAR_65 : A -> Prop => exists t : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_65 (@IN (A -> Prop) t s) (@DIFF A (@UNIV A) t))))).
Axiom thm_UNIONS_INTERS : forall {A : Type'}, forall s : (A -> Prop) -> Prop, (@UNIONS A s) = (@DIFF A (@UNIV A) (@INTERS A (@GSPEC (A -> Prop) (fun GEN_PVAR_66 : A -> Prop => exists t : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_66 (@IN (A -> Prop) t s) (@DIFF A (@UNIV A) t))))).
Axiom thm_UNIONS_DIFF : forall {A : Type'}, forall s : (A -> Prop) -> Prop, forall t : A -> Prop, (@DIFF A (@UNIONS A s) t) = (@UNIONS A (@GSPEC (A -> Prop) (fun GEN_PVAR_67 : A -> Prop => exists x : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_67 (@IN (A -> Prop) x s) (@DIFF A x t)))).
Axiom thm_DIFF_UNIONS : forall {A : Type'}, forall u : A -> Prop, forall s : (A -> Prop) -> Prop, (@DIFF A u (@UNIONS A s)) = (@INTER A u (@INTERS A (@GSPEC (A -> Prop) (fun GEN_PVAR_68 : A -> Prop => exists t : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_68 (@IN (A -> Prop) t s) (@DIFF A u t))))).
Axiom thm_DIFF_UNIONS_NONEMPTY : forall {A : Type'}, forall u : A -> Prop, forall s : (A -> Prop) -> Prop, (~ (s = (@EMPTY (A -> Prop)))) -> (@DIFF A u (@UNIONS A s)) = (@INTERS A (@GSPEC (A -> Prop) (fun GEN_PVAR_69 : A -> Prop => exists t : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_69 (@IN (A -> Prop) t s) (@DIFF A u t)))).
Axiom thm_INTERS_OVER_UNIONS : forall {A B : Type'}, forall f : A -> (B -> Prop) -> Prop, forall s : A -> Prop, (@INTERS B (@GSPEC (B -> Prop) (fun GEN_PVAR_70 : B -> Prop => exists x : A, @SETSPEC (B -> Prop) GEN_PVAR_70 (@IN A x s) (@UNIONS B (f x))))) = (@UNIONS B (@GSPEC (B -> Prop) (fun GEN_PVAR_74 : B -> Prop => exists g : A -> B -> Prop, @SETSPEC (B -> Prop) GEN_PVAR_74 (forall x : A, (@IN A x s) -> @IN (B -> Prop) (g x) (f x)) (@INTERS B (@GSPEC (B -> Prop) (fun GEN_PVAR_73 : B -> Prop => exists x : A, @SETSPEC (B -> Prop) GEN_PVAR_73 (@IN A x s) (g x))))))).
Axiom thm_INTER_INTERS : forall {A : Type'}, (forall f : (A -> Prop) -> Prop, forall s : A -> Prop, (@INTER A s (@INTERS A f)) = (@COND (A -> Prop) (f = (@EMPTY (A -> Prop))) s (@INTERS A (@GSPEC (A -> Prop) (fun GEN_PVAR_75 : A -> Prop => exists t : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_75 (@IN (A -> Prop) t f) (@INTER A s t)))))) /\ (forall f : (A -> Prop) -> Prop, forall s : A -> Prop, (@INTER A (@INTERS A f) s) = (@COND (A -> Prop) (f = (@EMPTY (A -> Prop))) s (@INTERS A (@GSPEC (A -> Prop) (fun GEN_PVAR_76 : A -> Prop => exists t : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_76 (@IN (A -> Prop) t f) (@INTER A t s)))))).
Axiom thm_UNIONS_OVER_INTERS : forall {A B : Type'}, forall f : A -> (B -> Prop) -> Prop, forall s : A -> Prop, (@UNIONS B (@GSPEC (B -> Prop) (fun GEN_PVAR_77 : B -> Prop => exists x : A, @SETSPEC (B -> Prop) GEN_PVAR_77 (@IN A x s) (@INTERS B (f x))))) = (@INTERS B (@GSPEC (B -> Prop) (fun GEN_PVAR_81 : B -> Prop => exists g : A -> B -> Prop, @SETSPEC (B -> Prop) GEN_PVAR_81 (forall x : A, (@IN A x s) -> @IN (B -> Prop) (g x) (f x)) (@UNIONS B (@GSPEC (B -> Prop) (fun GEN_PVAR_80 : B -> Prop => exists x : A, @SETSPEC (B -> Prop) GEN_PVAR_80 (@IN A x s) (g x))))))).
Axiom thm_UNIONS_EQ_INTERS : forall {A : Type'}, forall f : (A -> Prop) -> Prop, ((@UNIONS A f) = (@INTERS A f)) = (exists s : A -> Prop, f = (@INSERT (A -> Prop) s (@EMPTY (A -> Prop)))).
Axiom thm_EXISTS_UNIQUE_UNIONS_INTERS : forall {A : Type'}, forall P : (A -> Prop) -> Prop, (@ex1 (A -> Prop) (fun s : A -> Prop => P s)) = ((@UNIONS A (@GSPEC (A -> Prop) (fun GEN_PVAR_82 : A -> Prop => exists s : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_82 (P s) s))) = (@INTERS A (@GSPEC (A -> Prop) (fun GEN_PVAR_83 : A -> Prop => exists s : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_83 (P s) s)))).
Axiom thm_IMAGE_INTERS_SUBSET : forall {A B : Type'}, forall f : A -> B, forall g : (A -> Prop) -> Prop, @SUBSET B (@IMAGE A B f (@INTERS A g)) (@INTERS B (@IMAGE (A -> Prop) (B -> Prop) (@IMAGE A B f) g)).
Axiom thm_IMAGE_INTER_SUBSET : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall t : A -> Prop, @SUBSET B (@IMAGE A B f (@INTER A s t)) (@INTER B (@IMAGE A B f s) (@IMAGE A B f t)).
Axiom thm_IMAGE_INTER_SATURATED_GEN : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall t : A -> Prop, forall u : A -> Prop, (((@SUBSET A (@GSPEC A (fun GEN_PVAR_84 : A => exists x : A, @SETSPEC A GEN_PVAR_84 ((@IN A x u) /\ (@IN B (f x) (@IMAGE A B f s))) x)) s) /\ (@SUBSET A t u)) \/ ((@SUBSET A (@GSPEC A (fun GEN_PVAR_85 : A => exists x : A, @SETSPEC A GEN_PVAR_85 ((@IN A x u) /\ (@IN B (f x) (@IMAGE A B f t))) x)) t) /\ (@SUBSET A s u))) -> (@IMAGE A B f (@INTER A s t)) = (@INTER B (@IMAGE A B f s) (@IMAGE A B f t)).
Axiom thm_IMAGE_INTERS_SATURATED_GEN : forall {A B : Type'}, forall f : A -> B, forall g : (A -> Prop) -> Prop, forall s : A -> Prop, forall u : A -> Prop, ((~ (g = (@EMPTY (A -> Prop)))) /\ ((forall t : A -> Prop, (@IN (A -> Prop) t g) -> @SUBSET A t u) /\ (forall t : A -> Prop, (@IN (A -> Prop) t (@DELETE (A -> Prop) g s)) -> @SUBSET A (@GSPEC A (fun GEN_PVAR_87 : A => exists x : A, @SETSPEC A GEN_PVAR_87 ((@IN A x u) /\ (@IN B (f x) (@IMAGE A B f t))) x)) t))) -> (@IMAGE A B f (@INTERS A g)) = (@INTERS B (@IMAGE (A -> Prop) (B -> Prop) (@IMAGE A B f) g)).
Axiom thm_IMAGE_INTER_SATURATED : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall t : A -> Prop, ((@SUBSET A (@GSPEC A (fun GEN_PVAR_88 : A => exists x : A, @SETSPEC A GEN_PVAR_88 (@IN B (f x) (@IMAGE A B f s)) x)) s) \/ (@SUBSET A (@GSPEC A (fun GEN_PVAR_89 : A => exists x : A, @SETSPEC A GEN_PVAR_89 (@IN B (f x) (@IMAGE A B f t)) x)) t)) -> (@IMAGE A B f (@INTER A s t)) = (@INTER B (@IMAGE A B f s) (@IMAGE A B f t)).
Axiom thm_IMAGE_INTERS_SATURATED : forall {A B : Type'}, forall f : A -> B, forall g : (A -> Prop) -> Prop, forall s : A -> Prop, ((~ (g = (@EMPTY (A -> Prop)))) /\ (forall t : A -> Prop, (@IN (A -> Prop) t (@DELETE (A -> Prop) g s)) -> @SUBSET A (@GSPEC A (fun GEN_PVAR_90 : A => exists x : A, @SETSPEC A GEN_PVAR_90 (@IN B (f x) (@IMAGE A B f t)) x)) t)) -> (@IMAGE A B f (@INTERS A g)) = (@INTERS B (@IMAGE (A -> Prop) (B -> Prop) (@IMAGE A B f) g)).
Axiom thm_FINITE_INDUCT_STRONG : forall {A : Type'}, forall P : (A -> Prop) -> Prop, ((P (@EMPTY A)) /\ (forall x : A, forall s : A -> Prop, ((P s) /\ ((~ (@IN A x s)) /\ (@FINITE A s))) -> P (@INSERT A x s))) -> forall s : A -> Prop, (@FINITE A s) -> P s.
Axiom thm_INJECTIVE_ON_ALT : forall {A B : Type'}, forall P : A -> Prop, forall f : A -> B, (forall x : A, forall y : A, ((P x) /\ ((P y) /\ ((f x) = (f y)))) -> x = y) = (forall x : A, forall y : A, ((P x) /\ (P y)) -> ((f x) = (f y)) = (x = y)).
Axiom thm_INJECTIVE_ALT : forall {A B : Type'}, forall f : A -> B, (forall x : A, forall y : A, ((f x) = (f y)) -> x = y) = (forall x : A, forall y : A, ((f x) = (f y)) = (x = y)).
Axiom thm_SURJECTIVE_ON_RIGHT_INVERSE : forall {A B : Type'} (s : A -> Prop), forall f : A -> B, forall t : B -> Prop, (forall y : B, (@IN B y t) -> exists x : A, (@IN A x s) /\ ((f x) = y)) = (exists g : B -> A, forall y : B, (@IN B y t) -> (@IN A (g y) s) /\ ((f (g y)) = y)).
Axiom thm_INJECTIVE_ON_LEFT_INVERSE : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y) = (exists g : B -> A, forall x : A, (@IN A x s) -> (g (f x)) = x).
Axiom thm_BIJECTIVE_ON_LEFT_RIGHT_INVERSE : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall t : B -> Prop, (forall x : A, (@IN A x s) -> @IN B (f x) t) -> ((forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y) /\ (forall y : B, (@IN B y t) -> exists x : A, (@IN A x s) /\ ((f x) = y))) = (exists g : B -> A, (forall y : B, (@IN B y t) -> @IN A (g y) s) /\ ((forall y : B, (@IN B y t) -> (f (g y)) = y) /\ (forall x : A, (@IN A x s) -> (g (f x)) = x))).
Axiom thm_SURJECTIVE_RIGHT_INVERSE : forall {A B : Type'}, forall f : A -> B, (forall y : B, exists x : A, (f x) = y) = (exists g : B -> A, forall y : B, (f (g y)) = y).
Axiom thm_INJECTIVE_LEFT_INVERSE : forall {A B : Type'}, forall f : A -> B, (forall x : A, forall y : A, ((f x) = (f y)) -> x = y) = (exists g : B -> A, forall x : A, (g (f x)) = x).
Axiom thm_BIJECTIVE_LEFT_RIGHT_INVERSE : forall {A B : Type'}, forall f : A -> B, ((forall x : A, forall y : A, ((f x) = (f y)) -> x = y) /\ (forall y : B, exists x : A, (f x) = y)) = (exists g : B -> A, (forall y : B, (f (g y)) = y) /\ (forall x : A, (g (f x)) = x)).
Axiom thm_FUNCTION_FACTORS_LEFT_GEN : forall {A B C : Type'}, forall P : A -> Prop, forall f : A -> B, forall g : A -> C, (forall x : A, forall y : A, ((P x) /\ ((P y) /\ ((g x) = (g y)))) -> (f x) = (f y)) = (exists h : C -> B, forall x : A, (P x) -> (f x) = (h (g x))).
Axiom thm_FUNCTION_FACTORS_LEFT : forall {A B C : Type'}, forall f : A -> B, forall g : A -> C, (forall x : A, forall y : A, ((g x) = (g y)) -> (f x) = (f y)) = (exists h : C -> B, f = (@o A C B h g)).
Axiom thm_FUNCTION_FACTORS_RIGHT_GEN : forall {A B C : Type'}, forall P : A -> Prop, forall f : A -> C, forall g : B -> C, (forall x : A, (P x) -> exists y : B, (g y) = (f x)) = (exists h : A -> B, forall x : A, (P x) -> (f x) = (g (h x))).
Axiom thm_FUNCTION_FACTORS_RIGHT : forall {A B C : Type'}, forall f : A -> C, forall g : B -> C, (forall x : A, exists y : B, (g y) = (f x)) = (exists h : A -> B, f = (@o A B C g h)).
Axiom thm_SURJECTIVE_FORALL_THM : forall {A B : Type'}, forall f : A -> B, (forall y : B, exists x : A, (f x) = y) = (forall P : B -> Prop, (forall x : A, P (f x)) = (forall y : B, P y)).
Axiom thm_SURJECTIVE_EXISTS_THM : forall {A B : Type'}, forall f : A -> B, (forall y : B, exists x : A, (f x) = y) = (forall P : B -> Prop, (exists x : A, P (f x)) = (exists y : B, P y)).
Axiom thm_SURJECTIVE_IMAGE_THM : forall {A B : Type'}, forall f : A -> B, (forall y : B, exists x : A, (f x) = y) = (forall P : B -> Prop, (@IMAGE A B f (@GSPEC A (fun GEN_PVAR_91 : A => exists x : A, @SETSPEC A GEN_PVAR_91 (P (f x)) x))) = (@GSPEC B (fun GEN_PVAR_92 : B => exists x : B, @SETSPEC B GEN_PVAR_92 (P x) x))).
Axiom thm_IMAGE_INJECTIVE_IMAGE_OF_SUBSET : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, exists t : A -> Prop, (@SUBSET A t s) /\ (((@IMAGE A B f s) = (@IMAGE A B f t)) /\ (forall x : A, forall y : A, ((@IN A x t) /\ ((@IN A y t) /\ ((f x) = (f y)))) -> x = y)).
Axiom thm_FINITE_EMPTY : forall {A : Type'}, @FINITE A (@EMPTY A).
Axiom thm_FINITE_SUBSET : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A t) /\ (@SUBSET A s t)) -> @FINITE A s.
Axiom thm_FINITE_RESTRICT : forall {A : Type'}, forall s : A -> Prop, forall P : A -> Prop, (@FINITE A s) -> @FINITE A (@GSPEC A (fun GEN_PVAR_93 : A => exists x : A, @SETSPEC A GEN_PVAR_93 ((@IN A x s) /\ (P x)) x)).
Axiom thm_FINITE_UNION_IMP : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A s) /\ (@FINITE A t)) -> @FINITE A (@UNION A s t).
Axiom thm_FINITE_UNION : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, (@FINITE A (@UNION A s t)) = ((@FINITE A s) /\ (@FINITE A t)).
Axiom thm_FINITE_INTER : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A s) \/ (@FINITE A t)) -> @FINITE A (@INTER A s t).
Axiom thm_FINITE_INSERT : forall {A : Type'}, forall s : A -> Prop, forall x : A, (@FINITE A (@INSERT A x s)) = (@FINITE A s).
Axiom thm_FINITE_SING : forall {A : Type'}, forall a : A, @FINITE A (@INSERT A a (@EMPTY A)).
Axiom thm_FINITE_DELETE_IMP : forall {A : Type'}, forall s : A -> Prop, forall x : A, (@FINITE A s) -> @FINITE A (@DELETE A s x).
Axiom thm_FINITE_DELETE : forall {A : Type'}, forall s : A -> Prop, forall x : A, (@FINITE A (@DELETE A s x)) = (@FINITE A s).
Axiom thm_FINITE_FINITE_UNIONS : forall {A : Type'}, forall s : (A -> Prop) -> Prop, (@FINITE (A -> Prop) s) -> (@FINITE A (@UNIONS A s)) = (forall t : A -> Prop, (@IN (A -> Prop) t s) -> @FINITE A t).
Axiom thm_FINITE_IMAGE_EXPAND : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, (@FINITE A s) -> @FINITE B (@GSPEC B (fun GEN_PVAR_96 : B => exists y : B, @SETSPEC B GEN_PVAR_96 (exists x : A, (@IN A x s) /\ (y = (f x))) y)).
Axiom thm_FINITE_IMAGE : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, (@FINITE A s) -> @FINITE B (@IMAGE A B f s).
Axiom thm_FINITE_IMAGE_INJ_GENERAL : forall {A B : Type'}, forall f : A -> B, forall A' : B -> Prop, forall s : A -> Prop, ((forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y) /\ (@FINITE B A')) -> @FINITE A (@GSPEC A (fun GEN_PVAR_97 : A => exists x : A, @SETSPEC A GEN_PVAR_97 ((@IN A x s) /\ (@IN B (f x) A')) x)).
Axiom thm_FINITE_FINITE_PREIMAGE_GENERAL : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall t : B -> Prop, ((@FINITE B t) /\ (forall y : B, (@IN B y t) -> @FINITE A (@GSPEC A (fun GEN_PVAR_100 : A => exists x : A, @SETSPEC A GEN_PVAR_100 ((@IN A x s) /\ ((f x) = y)) x)))) -> @FINITE A (@GSPEC A (fun GEN_PVAR_101 : A => exists x : A, @SETSPEC A GEN_PVAR_101 ((@IN A x s) /\ (@IN B (f x) t)) x)).
Axiom thm_FINITE_FINITE_PREIMAGE : forall {A B : Type'}, forall f : A -> B, forall t : B -> Prop, ((@FINITE B t) /\ (forall y : B, (@IN B y t) -> @FINITE A (@GSPEC A (fun GEN_PVAR_102 : A => exists x : A, @SETSPEC A GEN_PVAR_102 ((f x) = y) x)))) -> @FINITE A (@GSPEC A (fun GEN_PVAR_103 : A => exists x : A, @SETSPEC A GEN_PVAR_103 (@IN B (f x) t) x)).
Axiom thm_FINITE_IMAGE_INJ_EQ : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y) -> (@FINITE B (@IMAGE A B f s)) = (@FINITE A s).
Axiom thm_FINITE_IMAGE_INJ : forall {A B : Type'}, forall f : A -> B, forall A' : B -> Prop, ((forall x : A, forall y : A, ((f x) = (f y)) -> x = y) /\ (@FINITE B A')) -> @FINITE A (@GSPEC A (fun GEN_PVAR_104 : A => exists x : A, @SETSPEC A GEN_PVAR_104 (@IN B (f x) A') x)).
Axiom thm_FINITE_IMAGE_GEN : forall {A B C : Type'}, forall f : A -> B, forall g : A -> C, forall s : A -> Prop, forall t : B -> Prop, ((@SUBSET B (@IMAGE A B f s) t) /\ ((@FINITE B t) /\ (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> (g x) = (g y)))) -> @FINITE C (@IMAGE A C g s).
Axiom thm_INFINITE_IMAGE : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, ((@INFINITE A s) /\ (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y)) -> @INFINITE B (@IMAGE A B f s).
Axiom thm_INFINITE_IMAGE_INJ : forall {A B : Type'}, forall f : A -> B, (forall x : A, forall y : A, ((f x) = (f y)) -> x = y) -> forall s : A -> Prop, (@INFINITE A s) -> @INFINITE B (@IMAGE A B f s).
Axiom thm_INFINITE_NONEMPTY : forall {A : Type'}, forall s : A -> Prop, (@INFINITE A s) -> ~ (s = (@EMPTY A)).
Axiom thm_INFINITE_DIFF_FINITE : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, ((@INFINITE A s) /\ (@FINITE A t)) -> @INFINITE A (@DIFF A s t).
Axiom thm_SUBSET_IMAGE_INJ : forall {A B : Type'}, forall f : A -> B, forall s : B -> Prop, forall t : A -> Prop, (@SUBSET B s (@IMAGE A B f t)) = (exists u : A -> Prop, (@SUBSET A u t) /\ ((forall x : A, forall y : A, ((@IN A x u) /\ (@IN A y u)) -> ((f x) = (f y)) = (x = y)) /\ (s = (@IMAGE A B f u)))).
Axiom thm_SUBSET_IMAGE : forall {A B : Type'}, forall f : A -> B, forall s : B -> Prop, forall t : A -> Prop, (@SUBSET B s (@IMAGE A B f t)) = (exists u : A -> Prop, (@SUBSET A u t) /\ (s = (@IMAGE A B f u))).
Axiom thm_EXISTS_SUBSET_IMAGE : forall {A B : Type'}, forall P : (B -> Prop) -> Prop, forall f : A -> B, forall s : A -> Prop, (exists t : B -> Prop, (@SUBSET B t (@IMAGE A B f s)) /\ (P t)) = (exists t : A -> Prop, (@SUBSET A t s) /\ (P (@IMAGE A B f t))).
Axiom thm_FORALL_SUBSET_IMAGE : forall {A B : Type'}, forall P : (B -> Prop) -> Prop, forall f : A -> B, forall s : A -> Prop, (forall t : B -> Prop, (@SUBSET B t (@IMAGE A B f s)) -> P t) = (forall t : A -> Prop, (@SUBSET A t s) -> P (@IMAGE A B f t)).
Axiom thm_EXISTS_SUBSET_IMAGE_INJ : forall {A B : Type'}, forall P : (B -> Prop) -> Prop, forall f : A -> B, forall s : A -> Prop, (exists t : B -> Prop, (@SUBSET B t (@IMAGE A B f s)) /\ (P t)) = (exists t : A -> Prop, (@SUBSET A t s) /\ ((forall x : A, forall y : A, ((@IN A x t) /\ (@IN A y t)) -> ((f x) = (f y)) = (x = y)) /\ (P (@IMAGE A B f t)))).
Axiom thm_FORALL_SUBSET_IMAGE_INJ : forall {A B : Type'}, forall P : (B -> Prop) -> Prop, forall f : A -> B, forall s : A -> Prop, (forall t : B -> Prop, (@SUBSET B t (@IMAGE A B f s)) -> P t) = (forall t : A -> Prop, ((@SUBSET A t s) /\ (forall x : A, forall y : A, ((@IN A x t) /\ (@IN A y t)) -> ((f x) = (f y)) = (x = y))) -> P (@IMAGE A B f t)).
Axiom thm_EXISTS_FINITE_SUBSET_IMAGE_INJ : forall {A B : Type'}, forall P : (B -> Prop) -> Prop, forall f : A -> B, forall s : A -> Prop, (exists t : B -> Prop, (@FINITE B t) /\ ((@SUBSET B t (@IMAGE A B f s)) /\ (P t))) = (exists t : A -> Prop, (@FINITE A t) /\ ((@SUBSET A t s) /\ ((forall x : A, forall y : A, ((@IN A x t) /\ (@IN A y t)) -> ((f x) = (f y)) = (x = y)) /\ (P (@IMAGE A B f t))))).
Axiom thm_FORALL_FINITE_SUBSET_IMAGE_INJ : forall {A B : Type'}, forall P : (B -> Prop) -> Prop, forall f : A -> B, forall s : A -> Prop, (forall t : B -> Prop, ((@FINITE B t) /\ (@SUBSET B t (@IMAGE A B f s))) -> P t) = (forall t : A -> Prop, ((@FINITE A t) /\ ((@SUBSET A t s) /\ (forall x : A, forall y : A, ((@IN A x t) /\ (@IN A y t)) -> ((f x) = (f y)) = (x = y)))) -> P (@IMAGE A B f t)).
Axiom thm_EXISTS_FINITE_SUBSET_IMAGE : forall {A B : Type'}, forall P : (B -> Prop) -> Prop, forall f : A -> B, forall s : A -> Prop, (exists t : B -> Prop, (@FINITE B t) /\ ((@SUBSET B t (@IMAGE A B f s)) /\ (P t))) = (exists t : A -> Prop, (@FINITE A t) /\ ((@SUBSET A t s) /\ (P (@IMAGE A B f t)))).
Axiom thm_FORALL_FINITE_SUBSET_IMAGE : forall {A B : Type'}, forall P : (B -> Prop) -> Prop, forall f : A -> B, forall s : A -> Prop, (forall t : B -> Prop, ((@FINITE B t) /\ (@SUBSET B t (@IMAGE A B f s))) -> P t) = (forall t : A -> Prop, ((@FINITE A t) /\ (@SUBSET A t s)) -> P (@IMAGE A B f t)).
Axiom thm_FINITE_SUBSET_IMAGE : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall t : B -> Prop, ((@FINITE B t) /\ (@SUBSET B t (@IMAGE A B f s))) = (exists s' : A -> Prop, (@FINITE A s') /\ ((@SUBSET A s' s) /\ (t = (@IMAGE A B f s')))).
Axiom thm_FINITE_SUBSET_IMAGE_IMP : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall t : B -> Prop, ((@FINITE B t) /\ (@SUBSET B t (@IMAGE A B f s))) -> exists s' : A -> Prop, (@FINITE A s') /\ ((@SUBSET A s' s) /\ (@SUBSET B t (@IMAGE A B f s'))).
Axiom thm_FINITE_IMAGE_EQ : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, (@FINITE B (@IMAGE A B f s)) = (exists t : A -> Prop, (@FINITE A t) /\ ((@SUBSET A t s) /\ ((@IMAGE A B f s) = (@IMAGE A B f t)))).
Axiom thm_FINITE_IMAGE_EQ_INJ : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, (@FINITE B (@IMAGE A B f s)) = (exists t : A -> Prop, (@FINITE A t) /\ ((@SUBSET A t s) /\ (((@IMAGE A B f s) = (@IMAGE A B f t)) /\ (forall x : A, forall y : A, ((@IN A x t) /\ (@IN A y t)) -> ((f x) = (f y)) = (x = y))))).
Axiom thm_FINITE_DIFF : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, (@FINITE A s) -> @FINITE A (@DIFF A s t).
Axiom thm_INFINITE_SUPERSET : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, ((@INFINITE A s) /\ (@SUBSET A s t)) -> @INFINITE A t.
Axiom thm_FINITE_TRANSITIVITY_CHAIN : forall {A : Type'}, forall R' : A -> A -> Prop, forall s : A -> Prop, ((@FINITE A s) /\ ((forall x : A, ~ (R' x x)) /\ ((forall x : A, forall y : A, forall z : A, ((R' x y) /\ (R' y z)) -> R' x z) /\ (forall x : A, (@IN A x s) -> exists y : A, (@IN A y s) /\ (R' x y))))) -> s = (@EMPTY A).
Axiom thm_UNIONS_MAXIMAL_SETS : forall {A : Type'}, forall f : (A -> Prop) -> Prop, (@FINITE (A -> Prop) f) -> (@UNIONS A (@GSPEC (A -> Prop) (fun GEN_PVAR_106 : A -> Prop => exists t : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_106 ((@IN (A -> Prop) t f) /\ (forall u : A -> Prop, (@IN (A -> Prop) u f) -> ~ (@PSUBSET A t u))) t))) = (@UNIONS A f).
Axiom thm_FINITE_SUBSET_UNIONS : forall {A : Type'}, forall f : (A -> Prop) -> Prop, forall s : A -> Prop, ((@FINITE A s) /\ (@SUBSET A s (@UNIONS A f))) -> exists f' : (A -> Prop) -> Prop, (@FINITE (A -> Prop) f') /\ ((@SUBSET (A -> Prop) f' f) /\ (@SUBSET A s (@UNIONS A f'))).
Axiom thm_UNIONS_IN_CHAIN : forall {A : Type'}, forall f : (A -> Prop) -> Prop, ((@FINITE (A -> Prop) f) /\ ((~ (f = (@EMPTY (A -> Prop)))) /\ (forall s : A -> Prop, forall t : A -> Prop, ((@IN (A -> Prop) s f) /\ (@IN (A -> Prop) t f)) -> (@SUBSET A s t) \/ (@SUBSET A t s)))) -> @IN (A -> Prop) (@UNIONS A f) f.
Axiom thm_INTERS_IN_CHAIN : forall {A : Type'}, forall f : (A -> Prop) -> Prop, ((@FINITE (A -> Prop) f) /\ ((~ (f = (@EMPTY (A -> Prop)))) /\ (forall s : A -> Prop, forall t : A -> Prop, ((@IN (A -> Prop) s f) /\ (@IN (A -> Prop) t f)) -> (@SUBSET A s t) \/ (@SUBSET A t s)))) -> @IN (A -> Prop) (@INTERS A f) f.
Axiom thm_FINITE_SUBSET_UNIONS_DIRECTED_EQ : forall {A : Type'}, forall f : (A -> Prop) -> Prop, forall s : A -> Prop, ((~ (f = (@EMPTY (A -> Prop)))) /\ ((forall t : A -> Prop, forall u : A -> Prop, ((@IN (A -> Prop) t f) /\ (@IN (A -> Prop) u f)) -> exists v : A -> Prop, (@IN (A -> Prop) v f) /\ ((@SUBSET A t v) /\ (@SUBSET A u v))) /\ (@FINITE A s))) -> (@SUBSET A s (@UNIONS A f)) = (exists t : A -> Prop, (@IN (A -> Prop) t f) /\ (@SUBSET A s t)).
Axiom thm_FINITE_SUBSET_UNIONS_CHAIN : forall {A : Type'}, forall f : (A -> Prop) -> Prop, forall s : A -> Prop, ((@FINITE A s) /\ ((@SUBSET A s (@UNIONS A f)) /\ ((~ (f = (@EMPTY (A -> Prop)))) /\ (forall t : A -> Prop, forall u : A -> Prop, ((@IN (A -> Prop) t f) /\ (@IN (A -> Prop) u f)) -> (@SUBSET A t u) \/ (@SUBSET A u t))))) -> exists t : A -> Prop, (@IN (A -> Prop) t f) /\ (@SUBSET A s t).
Axiom thm_FINREC : forall {A B : Type'} (b : B) (s : A -> Prop) (n : nat) (a : B) (f : A -> B -> B), ((@FINREC A B f b s a (NUMERAL 0)) = ((s = (@EMPTY A)) /\ (a = b))) /\ ((@FINREC A B f b s a (S n)) = (exists x : A, exists c : B, (@IN A x s) /\ ((@FINREC A B f b (@DELETE A s x) c n) /\ (a = (f x c))))).
Axiom thm_FINREC_1_LEMMA : forall {A B : Type'}, forall f : A -> B -> B, forall b : B, forall s : A -> Prop, forall a : B, (@FINREC A B f b s a (S (NUMERAL 0))) = (exists x : A, (s = (@INSERT A x (@EMPTY A))) /\ (a = (f x b))).
Axiom thm_FINREC_SUC_LEMMA : forall {A B : Type'}, forall f : A -> B -> B, forall b : B, (forall x : A, forall y : A, forall s : B, (~ (x = y)) -> (f x (f y s)) = (f y (f x s))) -> forall n : nat, forall s : A -> Prop, forall z : B, (@FINREC A B f b s z (S n)) -> forall x : A, (@IN A x s) -> exists w : B, (@FINREC A B f b (@DELETE A s x) w n) /\ (z = (f x w)).
Axiom thm_FINREC_UNIQUE_LEMMA : forall {A B : Type'}, forall f : A -> B -> B, forall b : B, (forall x : A, forall y : A, forall s : B, (~ (x = y)) -> (f x (f y s)) = (f y (f x s))) -> forall n1 : nat, forall n2 : nat, forall s : A -> Prop, forall a1 : B, forall a2 : B, ((@FINREC A B f b s a1 n1) /\ (@FINREC A B f b s a2 n2)) -> (a1 = a2) /\ (n1 = n2).
Axiom thm_FINREC_EXISTS_LEMMA : forall {A B : Type'}, forall f : A -> B -> B, forall b : B, forall s : A -> Prop, (@FINITE A s) -> exists a : B, exists n : nat, @FINREC A B f b s a n.
Axiom thm_FINREC_FUN_LEMMA : forall {A B C : Type'}, forall P : A -> Prop, forall R' : A -> B -> C -> Prop, ((forall s : A, (P s) -> exists a : B, exists n : C, R' s a n) /\ (forall n1 : C, forall n2 : C, forall s : A, forall a1 : B, forall a2 : B, ((R' s a1 n1) /\ (R' s a2 n2)) -> (a1 = a2) /\ (n1 = n2))) -> exists f : A -> B, forall s : A, forall a : B, (P s) -> (exists n : C, R' s a n) = ((f s) = a).
Axiom thm_FINREC_FUN : forall {A B : Type'}, forall f : A -> B -> B, forall b : B, (forall x : A, forall y : A, forall s : B, (~ (x = y)) -> (f x (f y s)) = (f y (f x s))) -> exists g : (A -> Prop) -> B, ((g (@EMPTY A)) = b) /\ (forall s : A -> Prop, forall x : A, ((@FINITE A s) /\ (@IN A x s)) -> (g s) = (f x (g (@DELETE A s x)))).
Axiom thm_SET_RECURSION_LEMMA : forall {A B : Type'}, forall f : A -> B -> B, forall b : B, (forall x : A, forall y : A, forall s : B, (~ (x = y)) -> (f x (f y s)) = (f y (f x s))) -> exists g : (A -> Prop) -> B, ((g (@EMPTY A)) = b) /\ (forall x : A, forall s : A -> Prop, (@FINITE A s) -> (g (@INSERT A x s)) = (@COND B (@IN A x s) (g s) (f x (g s)))).
Axiom thm_ITSET : forall {A B : Type'}, forall b : B, forall f : A -> B -> B, forall s : A -> Prop, (@ITSET A B f s b) = (@ε ((A -> Prop) -> B) (fun g : (A -> Prop) -> B => ((g (@EMPTY A)) = b) /\ (forall x : A, forall s' : A -> Prop, (@FINITE A s') -> (g (@INSERT A x s')) = (@COND B (@IN A x s') (g s') (f x (g s'))))) s).
Axiom thm_FINITE_RECURSION : forall {A B : Type'}, forall f : A -> B -> B, forall b : B, (forall x : A, forall y : A, forall s : B, (~ (x = y)) -> (f x (f y s)) = (f y (f x s))) -> ((@ITSET A B f (@EMPTY A) b) = b) /\ (forall x : A, forall s : A -> Prop, (@FINITE A s) -> (@ITSET A B f (@INSERT A x s) b) = (@COND B (@IN A x s) (@ITSET A B f s b) (f x (@ITSET A B f s b)))).
Axiom thm_FINITE_RECURSION_DELETE : forall {A B : Type'}, forall f : A -> B -> B, forall b : B, (forall x : A, forall y : A, forall s : B, (~ (x = y)) -> (f x (f y s)) = (f y (f x s))) -> ((@ITSET A B f (@EMPTY A) b) = b) /\ (forall x : A, forall s : A -> Prop, (@FINITE A s) -> (@ITSET A B f s b) = (@COND B (@IN A x s) (f x (@ITSET A B f (@DELETE A s x) b)) (@ITSET A B f (@DELETE A s x) b))).
Axiom thm_ITSET_EQ : forall {A B : Type'}, forall s : A -> Prop, forall f : A -> B -> B, forall g : A -> B -> B, forall b : B, ((@FINITE A s) /\ ((forall x : A, (@IN A x s) -> (f x) = (g x)) /\ ((forall x : A, forall y : A, forall s' : B, (~ (x = y)) -> (f x (f y s')) = (f y (f x s'))) /\ (forall x : A, forall y : A, forall s' : B, (~ (x = y)) -> (g x (g y s')) = (g y (g x s')))))) -> (@ITSET A B f s b) = (@ITSET A B g s b).
Axiom thm_CARD : forall {A : Type'}, forall s : A -> Prop, (@CARD A s) = (@ITSET A nat (fun x : A => fun n : nat => S n) s (NUMERAL 0)).
Axiom thm_CARD_CLAUSES : forall {A : Type'}, ((@CARD A (@EMPTY A)) = (NUMERAL 0)) /\ (forall x : A, forall s : A -> Prop, (@FINITE A s) -> (@CARD A (@INSERT A x s)) = (@COND nat (@IN A x s) (@CARD A s) (S (@CARD A s)))).
Axiom thm_CARD_UNION : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A s) /\ ((@FINITE A t) /\ ((@INTER A s t) = (@EMPTY A)))) -> (@CARD A (@UNION A s t)) = (Nat.add (@CARD A s) (@CARD A t)).
Axiom thm_CARD_DELETE : forall {A : Type'}, forall x : A, forall s : A -> Prop, (@FINITE A s) -> (@CARD A (@DELETE A s x)) = (@COND nat (@IN A x s) (Nat.sub (@CARD A s) (NUMERAL (BIT1 0))) (@CARD A s)).
Axiom thm_CARD_UNION_EQ : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall u : A -> Prop, ((@FINITE A u) /\ (((@INTER A s t) = (@EMPTY A)) /\ ((@UNION A s t) = u))) -> (Nat.add (@CARD A s) (@CARD A t)) = (@CARD A u).
Axiom thm_CARD_DIFF : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A s) /\ (@SUBSET A t s)) -> (@CARD A (@DIFF A s t)) = (Nat.sub (@CARD A s) (@CARD A t)).
Axiom thm_CARD_EQ_0 : forall {A : Type'}, forall s : A -> Prop, (@FINITE A s) -> ((@CARD A s) = (NUMERAL 0)) = (s = (@EMPTY A)).
Axiom thm_CARD_SING : forall {A : Type'}, forall a : A, (@CARD A (@INSERT A a (@EMPTY A))) = (NUMERAL (BIT1 0)).
Axiom thm_FINITE_INDUCT_DELETE : forall {A : Type'}, forall P : (A -> Prop) -> Prop, ((P (@EMPTY A)) /\ (forall s : A -> Prop, ((@FINITE A s) /\ (~ (s = (@EMPTY A)))) -> exists x : A, (@IN A x s) /\ ((P (@DELETE A s x)) -> P s))) -> forall s : A -> Prop, (@FINITE A s) -> P s.
Axiom thm_HAS_SIZE : forall {A : Type'}, forall s : A -> Prop, forall n : nat, (@HAS_SIZE A s n) = ((@FINITE A s) /\ ((@CARD A s) = n)).
Axiom thm_HAS_SIZE_CARD : forall {A : Type'}, forall s : A -> Prop, forall n : nat, (@HAS_SIZE A s n) -> (@CARD A s) = n.
Axiom thm_HAS_SIZE_0 : forall {A : Type'}, forall s : A -> Prop, (@HAS_SIZE A s (NUMERAL 0)) = (s = (@EMPTY A)).
Axiom thm_HAS_SIZE_SUC : forall {A : Type'}, forall s : A -> Prop, forall n : nat, (@HAS_SIZE A s (S n)) = ((~ (s = (@EMPTY A))) /\ (forall a : A, (@IN A a s) -> @HAS_SIZE A (@DELETE A s a) n)).
Axiom thm_HAS_SIZE_UNION : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall m : nat, forall n : nat, ((@HAS_SIZE A s m) /\ ((@HAS_SIZE A t n) /\ (@DISJOINT A s t))) -> @HAS_SIZE A (@UNION A s t) (Nat.add m n).
Axiom thm_HAS_SIZE_DIFF : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall m : nat, forall n : nat, ((@HAS_SIZE A s m) /\ ((@HAS_SIZE A t n) /\ (@SUBSET A t s))) -> @HAS_SIZE A (@DIFF A s t) (Nat.sub m n).
Axiom thm_HAS_SIZE_UNIONS : forall {A B : Type'}, forall s : A -> Prop, forall t : A -> B -> Prop, forall m : nat, forall n : nat, ((@HAS_SIZE A s m) /\ ((forall x : A, (@IN A x s) -> @HAS_SIZE B (t x) n) /\ (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ (~ (x = y)))) -> @DISJOINT B (t x) (t y)))) -> @HAS_SIZE B (@UNIONS B (@GSPEC (B -> Prop) (fun GEN_PVAR_109 : B -> Prop => exists x : A, @SETSPEC (B -> Prop) GEN_PVAR_109 (@IN A x s) (t x)))) (Nat.mul m n).
Axiom thm_FINITE_HAS_SIZE : forall {A : Type'}, forall s : A -> Prop, (@FINITE A s) = (@HAS_SIZE A s (@CARD A s)).
Axiom thm_HAS_SIZE_CLAUSES : forall {A : Type'} (n : nat) (s : A -> Prop), ((@HAS_SIZE A s (NUMERAL 0)) = (s = (@EMPTY A))) /\ ((@HAS_SIZE A s (S n)) = (exists a : A, exists t : A -> Prop, (@HAS_SIZE A t n) /\ ((~ (@IN A a t)) /\ (s = (@INSERT A a t))))).
Axiom thm_CARD_SUBSET_EQ : forall {A : Type'}, forall a : A -> Prop, forall b : A -> Prop, ((@FINITE A b) /\ ((@SUBSET A a b) /\ ((@CARD A a) = (@CARD A b)))) -> a = b.
Axiom thm_CARD_SUBSET : forall {A : Type'}, forall a : A -> Prop, forall b : A -> Prop, ((@SUBSET A a b) /\ (@FINITE A b)) -> Peano.le (@CARD A a) (@CARD A b).
Axiom thm_CARD_SUBSET_LE : forall {A : Type'}, forall a : A -> Prop, forall b : A -> Prop, ((@FINITE A b) /\ ((@SUBSET A a b) /\ (Peano.le (@CARD A b) (@CARD A a)))) -> a = b.
Axiom thm_SUBSET_CARD_EQ : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A t) /\ (@SUBSET A s t)) -> ((@CARD A s) = (@CARD A t)) = (s = t).
Axiom thm_FINITE_CARD_LE_SUBSET : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall n : nat, ((@SUBSET A s t) /\ ((@FINITE A t) /\ (Peano.le (@CARD A t) n))) -> (@FINITE A s) /\ (Peano.le (@CARD A s) n).
Axiom thm_CARD_PSUBSET : forall {A : Type'}, forall a : A -> Prop, forall b : A -> Prop, ((@PSUBSET A a b) /\ (@FINITE A b)) -> Peano.lt (@CARD A a) (@CARD A b).
Axiom thm_CARD_PSUBSET_IMP : forall {A : Type'}, forall a : A -> Prop, forall b : A -> Prop, ((@SUBSET A a b) /\ (~ ((@CARD A a) = (@CARD A b)))) -> @PSUBSET A a b.
Axiom thm_CARD_PSUBSET_EQ : forall {A : Type'}, forall a : A -> Prop, forall b : A -> Prop, ((@FINITE A b) /\ (@SUBSET A a b)) -> (@PSUBSET A a b) = (Peano.lt (@CARD A a) (@CARD A b)).
Axiom thm_CARD_UNION_LE : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A s) /\ (@FINITE A t)) -> Peano.le (@CARD A (@UNION A s t)) (Nat.add (@CARD A s) (@CARD A t)).
Axiom thm_FINITE_CARD_LE_UNION : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall m : nat, forall n : nat, (((@FINITE A s) /\ (Peano.le (@CARD A s) m)) /\ ((@FINITE A t) /\ (Peano.le (@CARD A t) n))) -> (@FINITE A (@UNION A s t)) /\ (Peano.le (@CARD A (@UNION A s t)) (Nat.add m n)).
Axiom thm_CARD_UNIONS_LE : forall {A B : Type'}, forall s : A -> Prop, forall t : A -> B -> Prop, forall m : nat, forall n : nat, ((@HAS_SIZE A s m) /\ (forall x : A, (@IN A x s) -> (@FINITE B (t x)) /\ (Peano.le (@CARD B (t x)) n))) -> Peano.le (@CARD B (@UNIONS B (@GSPEC (B -> Prop) (fun GEN_PVAR_115 : B -> Prop => exists x : A, @SETSPEC (B -> Prop) GEN_PVAR_115 (@IN A x s) (t x))))) (Nat.mul m n).
Axiom thm_CARD_UNION_GEN : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A s) /\ (@FINITE A t)) -> (@CARD A (@UNION A s t)) = (Nat.sub (Nat.add (@CARD A s) (@CARD A t)) (@CARD A (@INTER A s t))).
Axiom thm_CARD_UNION_OVERLAP_EQ : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A s) /\ (@FINITE A t)) -> ((@CARD A (@UNION A s t)) = (Nat.add (@CARD A s) (@CARD A t))) = ((@INTER A s t) = (@EMPTY A)).
Axiom thm_CARD_UNION_OVERLAP : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A s) /\ ((@FINITE A t) /\ (Peano.lt (@CARD A (@UNION A s t)) (Nat.add (@CARD A s) (@CARD A t))))) -> ~ ((@INTER A s t) = (@EMPTY A)).
Axiom thm_CARD_IMAGE_INJ : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, ((forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y) /\ (@FINITE A s)) -> (@CARD B (@IMAGE A B f s)) = (@CARD A s).
Axiom thm_HAS_SIZE_IMAGE_INJ : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall n : nat, ((forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y) /\ (@HAS_SIZE A s n)) -> @HAS_SIZE B (@IMAGE A B f s) n.
Axiom thm_CARD_IMAGE_LE : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, (@FINITE A s) -> Peano.le (@CARD B (@IMAGE A B f s)) (@CARD A s).
Axiom thm_FINITE_CARD_LE_IMAGE : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall n : nat, ((@FINITE A s) /\ (Peano.le (@CARD A s) n)) -> (@FINITE B (@IMAGE A B f s)) /\ (Peano.le (@CARD B (@IMAGE A B f s)) n).
Axiom thm_CARD_IMAGE_INJ_EQ : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall t : B -> Prop, ((@FINITE A s) /\ ((forall x : A, (@IN A x s) -> @IN B (f x) t) /\ (forall y : B, (@IN B y t) -> @ex1 A (fun x : A => (@IN A x s) /\ ((f x) = y))))) -> (@CARD B t) = (@CARD A s).
Axiom thm_CARD_SUBSET_IMAGE : forall {A B : Type'}, forall f : A -> B, forall s : B -> Prop, forall t : A -> Prop, ((@FINITE A t) /\ (@SUBSET B s (@IMAGE A B f t))) -> Peano.le (@CARD B s) (@CARD A t).
Axiom thm_HAS_SIZE_IMAGE_INJ_EQ : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall n : nat, (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y) -> (@HAS_SIZE B (@IMAGE A B f s) n) = (@HAS_SIZE A s n).
Axiom thm_CARD_IMAGE_EQ_INJ : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, (@FINITE A s) -> ((@CARD B (@IMAGE A B f s)) = (@CARD A s)) = (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y).
Axiom thm_EXISTS_SMALL_SUBSET_IMAGE_INJ : forall {A B : Type'}, forall P : (B -> Prop) -> Prop, forall f : A -> B, forall s : A -> Prop, forall n : nat, (exists t : B -> Prop, (@FINITE B t) /\ ((Peano.lt (@CARD B t) n) /\ ((@SUBSET B t (@IMAGE A B f s)) /\ (P t)))) = (exists t : A -> Prop, (@FINITE A t) /\ ((Peano.lt (@CARD A t) n) /\ ((@SUBSET A t s) /\ ((forall x : A, forall y : A, ((@IN A x t) /\ (@IN A y t)) -> ((f x) = (f y)) = (x = y)) /\ (P (@IMAGE A B f t)))))).
Axiom thm_FORALL_SMALL_SUBSET_IMAGE_INJ : forall {A B : Type'}, forall P : (B -> Prop) -> Prop, forall f : A -> B, forall s : A -> Prop, forall n : nat, (forall t : B -> Prop, ((@FINITE B t) /\ ((Peano.lt (@CARD B t) n) /\ (@SUBSET B t (@IMAGE A B f s)))) -> P t) = (forall t : A -> Prop, ((@FINITE A t) /\ ((Peano.lt (@CARD A t) n) /\ ((@SUBSET A t s) /\ (forall x : A, forall y : A, ((@IN A x t) /\ (@IN A y t)) -> ((f x) = (f y)) = (x = y))))) -> P (@IMAGE A B f t)).
Axiom thm_EXISTS_SMALL_SUBSET_IMAGE : forall {A B : Type'}, forall P : (B -> Prop) -> Prop, forall f : A -> B, forall s : A -> Prop, forall n : nat, (exists t : B -> Prop, (@FINITE B t) /\ ((Peano.lt (@CARD B t) n) /\ ((@SUBSET B t (@IMAGE A B f s)) /\ (P t)))) = (exists t : A -> Prop, (@FINITE A t) /\ ((Peano.lt (@CARD A t) n) /\ ((@SUBSET A t s) /\ (P (@IMAGE A B f t))))).
Axiom thm_FORALL_SMALL_SUBSET_IMAGE : forall {A B : Type'}, forall P : (B -> Prop) -> Prop, forall f : A -> B, forall s : A -> Prop, forall n : nat, (forall t : B -> Prop, ((@FINITE B t) /\ ((Peano.lt (@CARD B t) n) /\ (@SUBSET B t (@IMAGE A B f s)))) -> P t) = (forall t : A -> Prop, ((@FINITE A t) /\ ((Peano.lt (@CARD A t) n) /\ (@SUBSET A t s))) -> P (@IMAGE A B f t)).
Axiom thm_CARD_IMAGE_LE2 : forall {A B C : Type'}, forall f : A -> B, forall g : A -> C, forall s : A -> Prop, ((@FINITE A s) /\ (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((g x) = (g y)))) -> (f x) = (f y))) -> Peano.le (@CARD B (@IMAGE A B f s)) (@CARD C (@IMAGE A C g s)).
Axiom thm_CARD_IMAGE_LT2 : forall {A B C : Type'}, forall f : A -> B, forall g : A -> C, forall s : A -> Prop, ((@FINITE A s) /\ ((forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((g x) = (g y)))) -> (f x) = (f y)) /\ (~ (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> (g x) = (g y))))) -> Peano.lt (@CARD B (@IMAGE A B f s)) (@CARD C (@IMAGE A C g s)).
Axiom thm_CHOOSE_SUBSET_STRONG : forall {A : Type'}, forall n : nat, forall s : A -> Prop, ((@FINITE A s) -> Peano.le n (@CARD A s)) -> exists t : A -> Prop, (@SUBSET A t s) /\ (@HAS_SIZE A t n).
Axiom thm_CHOOSE_SUBSET_EQ : forall {A : Type'}, forall n : nat, forall s : A -> Prop, ((@FINITE A s) -> Peano.le n (@CARD A s)) = (exists t : A -> Prop, (@SUBSET A t s) /\ (@HAS_SIZE A t n)).
Axiom thm_CHOOSE_SUBSET : forall {A : Type'}, forall s : A -> Prop, (@FINITE A s) -> forall n : nat, (Peano.le n (@CARD A s)) -> exists t : A -> Prop, (@SUBSET A t s) /\ (@HAS_SIZE A t n).
Axiom thm_CHOOSE_SUBSET_BETWEEN : forall {A : Type'}, forall n : nat, forall s : A -> Prop, forall u : A -> Prop, ((@SUBSET A s u) /\ ((@FINITE A s) /\ ((Peano.le (@CARD A s) n) /\ ((@FINITE A u) -> Peano.le n (@CARD A u))))) -> exists t : A -> Prop, (@SUBSET A s t) /\ ((@SUBSET A t u) /\ (@HAS_SIZE A t n)).
Axiom thm_CARD_LE_UNIONS_CHAIN : forall {A : Type'}, forall f : (A -> Prop) -> Prop, forall n : nat, ((forall t : A -> Prop, forall u : A -> Prop, ((@IN (A -> Prop) t f) /\ (@IN (A -> Prop) u f)) -> (@SUBSET A t u) \/ (@SUBSET A u t)) /\ (forall t : A -> Prop, (@IN (A -> Prop) t f) -> (@FINITE A t) /\ (Peano.le (@CARD A t) n))) -> (@FINITE A (@UNIONS A f)) /\ (Peano.le (@CARD A (@UNIONS A f)) n).
Axiom thm_CARD_LE_1 : forall {A : Type'}, forall s : A -> Prop, ((@FINITE A s) /\ (Peano.le (@CARD A s) (NUMERAL (BIT1 0)))) = (exists a : A, @SUBSET A s (@INSERT A a (@EMPTY A))).
Axiom thm_INVOLUTION_EVEN_NOFIXPOINTS : forall {A : Type'}, forall f : A -> A, forall s : A -> Prop, ((@FINITE A s) /\ (forall x : A, (@IN A x s) -> (@IN A (f x) s) /\ ((~ ((f x) = x)) /\ ((f (f x)) = x)))) -> Coq.Arith.PeanoNat.Nat.Even (@CARD A s).
Axiom thm_INVOLUTION_EVEN_FIXPOINTS : forall {A : Type'}, forall f : A -> A, forall s : A -> Prop, ((@FINITE A s) /\ (forall x : A, (@IN A x s) -> (@IN A (f x) s) /\ ((f (f x)) = x))) -> (Coq.Arith.PeanoNat.Nat.Even (@CARD A (@GSPEC A (fun GEN_PVAR_120 : A => exists x : A, @SETSPEC A GEN_PVAR_120 ((@IN A x s) /\ ((f x) = x)) x)))) = (Coq.Arith.PeanoNat.Nat.Even (@CARD A s)).
Axiom thm_HAS_SIZE_PRODUCT_DEPENDENT : forall {A B : Type'}, forall s : A -> Prop, forall m : nat, forall t : A -> B -> Prop, forall n : nat, ((@HAS_SIZE A s m) /\ (forall x : A, (@IN A x s) -> @HAS_SIZE B (t x) n)) -> @HAS_SIZE (prod A B) (@GSPEC (prod A B) (fun GEN_PVAR_123 : prod A B => exists x : A, exists y : B, @SETSPEC (prod A B) GEN_PVAR_123 ((@IN A x s) /\ (@IN B y (t x))) (@pair A B x y))) (Nat.mul m n).
Axiom thm_FINITE_PRODUCT_DEPENDENT : forall {A B C : Type'}, forall f : A -> B -> C, forall s : A -> Prop, forall t : A -> B -> Prop, ((@FINITE A s) /\ (forall x : A, (@IN A x s) -> @FINITE B (t x))) -> @FINITE C (@GSPEC C (fun GEN_PVAR_128 : C => exists x : A, exists y : B, @SETSPEC C GEN_PVAR_128 ((@IN A x s) /\ (@IN B y (t x))) (f x y))).
Axiom thm_FINITE_PRODUCT : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, ((@FINITE A s) /\ (@FINITE B t)) -> @FINITE (prod A B) (@GSPEC (prod A B) (fun GEN_PVAR_129 : prod A B => exists x : A, exists y : B, @SETSPEC (prod A B) GEN_PVAR_129 ((@IN A x s) /\ (@IN B y t)) (@pair A B x y))).
Axiom thm_CARD_PRODUCT : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, ((@FINITE A s) /\ (@FINITE B t)) -> (@CARD (prod A B) (@GSPEC (prod A B) (fun GEN_PVAR_130 : prod A B => exists x : A, exists y : B, @SETSPEC (prod A B) GEN_PVAR_130 ((@IN A x s) /\ (@IN B y t)) (@pair A B x y)))) = (Nat.mul (@CARD A s) (@CARD B t)).
Axiom thm_HAS_SIZE_PRODUCT : forall {A B : Type'}, forall s : A -> Prop, forall m : nat, forall t : B -> Prop, forall n : nat, ((@HAS_SIZE A s m) /\ (@HAS_SIZE B t n)) -> @HAS_SIZE (prod A B) (@GSPEC (prod A B) (fun GEN_PVAR_131 : prod A B => exists x : A, exists y : B, @SETSPEC (prod A B) GEN_PVAR_131 ((@IN A x s) /\ (@IN B y t)) (@pair A B x y))) (Nat.mul m n).
Axiom thm_CROSS : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, (@CROSS A B s t) = (@GSPEC (prod A B) (fun GEN_PVAR_132 : prod A B => exists x : A, exists y : B, @SETSPEC (prod A B) GEN_PVAR_132 ((@IN A x s) /\ (@IN B y t)) (@pair A B x y))).
Axiom thm_IN_CROSS : forall {A B : Type'}, forall x : A, forall y : B, forall s : A -> Prop, forall t : B -> Prop, (@IN (prod A B) (@pair A B x y) (@CROSS A B s t)) = ((@IN A x s) /\ (@IN B y t)).
Axiom thm_HAS_SIZE_CROSS : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, forall m : nat, forall n : nat, ((@HAS_SIZE A s m) /\ (@HAS_SIZE B t n)) -> @HAS_SIZE (prod A B) (@CROSS A B s t) (Nat.mul m n).
Axiom thm_FINITE_CROSS : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, ((@FINITE A s) /\ (@FINITE B t)) -> @FINITE (prod A B) (@CROSS A B s t).
Axiom thm_CARD_CROSS : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, ((@FINITE A s) /\ (@FINITE B t)) -> (@CARD (prod A B) (@CROSS A B s t)) = (Nat.mul (@CARD A s) (@CARD B t)).
Axiom thm_CROSS_EQ_EMPTY : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, ((@CROSS A B s t) = (@EMPTY (prod A B))) = ((s = (@EMPTY A)) \/ (t = (@EMPTY B))).
Axiom thm_CROSS_EMPTY : forall {_98484 _98497 A B : Type'}, (forall s : A -> Prop, (@CROSS A _98484 s (@EMPTY _98484)) = (@EMPTY (prod A _98484))) /\ (forall t : B -> Prop, (@CROSS _98497 B (@EMPTY _98497) t) = (@EMPTY (prod _98497 B))).
Axiom thm_CROSS_SING : forall {A B : Type'}, forall x : A, forall y : B, (@CROSS A B (@INSERT A x (@EMPTY A)) (@INSERT B y (@EMPTY B))) = (@INSERT (prod A B) (@pair A B x y) (@EMPTY (prod A B))).
Axiom thm_CROSS_UNIV : forall {A B : Type'}, (@CROSS A B (@UNIV A) (@UNIV B)) = (@UNIV (prod A B)).
Axiom thm_FINITE_CROSS_EQ : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, (@FINITE (prod A B) (@CROSS A B s t)) = ((s = (@EMPTY A)) \/ ((t = (@EMPTY B)) \/ ((@FINITE A s) /\ (@FINITE B t)))).
Axiom thm_INFINITE_CROSS_EQ : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, (@INFINITE (prod A B) (@CROSS A B s t)) = (((~ (s = (@EMPTY A))) /\ (@INFINITE B t)) \/ ((@INFINITE A s) /\ (~ (t = (@EMPTY B))))).
Axiom thm_FINITE_CROSS_UNIV : forall {A B : Type'}, (@FINITE (prod A B) (@UNIV (prod A B))) = ((@FINITE A (@UNIV A)) /\ (@FINITE B (@UNIV B))).
Axiom thm_INFINITE_CROSS_UNIV : forall {A B : Type'}, (@INFINITE (prod A B) (@UNIV (prod A B))) = ((@INFINITE A (@UNIV A)) \/ (@INFINITE B (@UNIV B))).
Axiom thm_FINITE_UNIV_PAIR : forall {A : Type'}, (@FINITE (prod A A) (@UNIV (prod A A))) = (@FINITE A (@UNIV A)).
Axiom thm_INFINITE_UNIV_PAIR : forall {A : Type'}, (@INFINITE (prod A A) (@UNIV (prod A A))) = (@INFINITE A (@UNIV A)).
Axiom thm_FORALL_IN_CROSS : forall {A B : Type'}, forall P : (prod A B) -> Prop, forall s : A -> Prop, forall t : B -> Prop, (forall z : prod A B, (@IN (prod A B) z (@CROSS A B s t)) -> P z) = (forall x : A, forall y : B, ((@IN A x s) /\ (@IN B y t)) -> P (@pair A B x y)).
Axiom thm_EXISTS_IN_CROSS : forall {A B : Type'}, forall P : (prod A B) -> Prop, forall s : A -> Prop, forall t : B -> Prop, (exists z : prod A B, (@IN (prod A B) z (@CROSS A B s t)) /\ (P z)) = (exists x : A, exists y : B, (@IN A x s) /\ ((@IN B y t) /\ (P (@pair A B x y)))).
Axiom thm_SUBSET_CROSS : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, forall s' : A -> Prop, forall t' : B -> Prop, (@SUBSET (prod A B) (@CROSS A B s t) (@CROSS A B s' t')) = ((s = (@EMPTY A)) \/ ((t = (@EMPTY B)) \/ ((@SUBSET A s s') /\ (@SUBSET B t t')))).
Axiom thm_CROSS_MONO : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, forall s' : A -> Prop, forall t' : B -> Prop, ((@SUBSET A s s') /\ (@SUBSET B t t')) -> @SUBSET (prod A B) (@CROSS A B s t) (@CROSS A B s' t').
Axiom thm_CROSS_EQ : forall {A B : Type'}, forall s : A -> Prop, forall s' : A -> Prop, forall t : B -> Prop, forall t' : B -> Prop, ((@CROSS A B s t) = (@CROSS A B s' t')) = ((((s = (@EMPTY A)) \/ (t = (@EMPTY B))) /\ ((s' = (@EMPTY A)) \/ (t' = (@EMPTY B)))) \/ ((s = s') /\ (t = t'))).
Axiom thm_IMAGE_FST_CROSS : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, (@IMAGE (prod A B) A (@fst A B) (@CROSS A B s t)) = (@COND (A -> Prop) (t = (@EMPTY B)) (@EMPTY A) s).
Axiom thm_IMAGE_SND_CROSS : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, (@IMAGE (prod A B) B (@snd A B) (@CROSS A B s t)) = (@COND (B -> Prop) (s = (@EMPTY A)) (@EMPTY B) t).
Axiom thm_IMAGE_PAIRED_CROSS : forall {A B C D : Type'}, forall f : A -> B, forall g : C -> D, forall s : A -> Prop, forall t : C -> Prop, (@IMAGE (prod A C) (prod B D) (@GABS ((prod A C) -> prod B D) (fun f' : (prod A C) -> prod B D => forall x : A, forall y : C, @GEQ (prod B D) (f' (@pair A C x y)) (@pair B D (f x) (g y)))) (@CROSS A C s t)) = (@CROSS B D (@IMAGE A B f s) (@IMAGE C D g t)).
Axiom thm_CROSS_INTER : forall {A B : Type'}, (forall s : A -> Prop, forall t : B -> Prop, forall u : B -> Prop, (@CROSS A B s (@INTER B t u)) = (@INTER (prod A B) (@CROSS A B s t) (@CROSS A B s u))) /\ (forall s : A -> Prop, forall t : A -> Prop, forall u : B -> Prop, (@CROSS A B (@INTER A s t) u) = (@INTER (prod A B) (@CROSS A B s u) (@CROSS A B t u))).
Axiom thm_CROSS_UNION : forall {A B : Type'}, (forall s : A -> Prop, forall t : B -> Prop, forall u : B -> Prop, (@CROSS A B s (@UNION B t u)) = (@UNION (prod A B) (@CROSS A B s t) (@CROSS A B s u))) /\ (forall s : A -> Prop, forall t : A -> Prop, forall u : B -> Prop, (@CROSS A B (@UNION A s t) u) = (@UNION (prod A B) (@CROSS A B s u) (@CROSS A B t u))).
Axiom thm_CROSS_DIFF : forall {A B : Type'}, (forall s : A -> Prop, forall t : B -> Prop, forall u : B -> Prop, (@CROSS A B s (@DIFF B t u)) = (@DIFF (prod A B) (@CROSS A B s t) (@CROSS A B s u))) /\ (forall s : A -> Prop, forall t : A -> Prop, forall u : B -> Prop, (@CROSS A B (@DIFF A s t) u) = (@DIFF (prod A B) (@CROSS A B s u) (@CROSS A B t u))).
Axiom thm_INTER_CROSS : forall {A B : Type'}, forall s : A -> Prop, forall s' : A -> Prop, forall t : B -> Prop, forall t' : B -> Prop, (@INTER (prod A B) (@CROSS A B s t) (@CROSS A B s' t')) = (@CROSS A B (@INTER A s s') (@INTER B t t')).
Axiom thm_CROSS_UNIONS : forall {A B : Type'}, (forall s : A -> Prop, forall f : (A -> Prop) -> Prop, (@CROSS A A s (@UNIONS A f)) = (@UNIONS (prod A A) (@GSPEC ((prod A A) -> Prop) (fun GEN_PVAR_134 : (prod A A) -> Prop => exists t : A -> Prop, @SETSPEC ((prod A A) -> Prop) GEN_PVAR_134 (@IN (A -> Prop) t f) (@CROSS A A s t))))) /\ (forall f : (A -> Prop) -> Prop, forall t : B -> Prop, (@CROSS A B (@UNIONS A f) t) = (@UNIONS (prod A B) (@GSPEC ((prod A B) -> Prop) (fun GEN_PVAR_135 : (prod A B) -> Prop => exists s : A -> Prop, @SETSPEC ((prod A B) -> Prop) GEN_PVAR_135 (@IN (A -> Prop) s f) (@CROSS A B s t))))).
Axiom thm_CROSS_UNIONS_UNIONS : forall {A B : Type'}, forall f : (A -> Prop) -> Prop, forall g : (B -> Prop) -> Prop, (@CROSS A B (@UNIONS A f) (@UNIONS B g)) = (@UNIONS (prod A B) (@GSPEC ((prod A B) -> Prop) (fun GEN_PVAR_133 : (prod A B) -> Prop => exists s : A -> Prop, exists t : B -> Prop, @SETSPEC ((prod A B) -> Prop) GEN_PVAR_133 ((@IN (A -> Prop) s f) /\ (@IN (B -> Prop) t g)) (@CROSS A B s t)))).
Axiom thm_CROSS_INTERS : forall {A B : Type'}, (forall s : A -> Prop, forall f : (A -> Prop) -> Prop, (@CROSS A A s (@INTERS A f)) = (@COND ((prod A A) -> Prop) (f = (@EMPTY (A -> Prop))) (@CROSS A A s (@UNIV A)) (@INTERS (prod A A) (@GSPEC ((prod A A) -> Prop) (fun GEN_PVAR_139 : (prod A A) -> Prop => exists t : A -> Prop, @SETSPEC ((prod A A) -> Prop) GEN_PVAR_139 (@IN (A -> Prop) t f) (@CROSS A A s t)))))) /\ (forall f : (A -> Prop) -> Prop, forall t : B -> Prop, (@CROSS A B (@INTERS A f) t) = (@COND ((prod A B) -> Prop) (f = (@EMPTY (A -> Prop))) (@CROSS A B (@UNIV A) t) (@INTERS (prod A B) (@GSPEC ((prod A B) -> Prop) (fun GEN_PVAR_140 : (prod A B) -> Prop => exists s : A -> Prop, @SETSPEC ((prod A B) -> Prop) GEN_PVAR_140 (@IN (A -> Prop) s f) (@CROSS A B s t)))))).
Axiom thm_CROSS_INTERS_INTERS : forall {A B : Type'}, forall f : (A -> Prop) -> Prop, forall g : (B -> Prop) -> Prop, (@CROSS A B (@INTERS A f) (@INTERS B g)) = (@COND ((prod A B) -> Prop) (f = (@EMPTY (A -> Prop))) (@INTERS (prod A B) (@GSPEC ((prod A B) -> Prop) (fun GEN_PVAR_136 : (prod A B) -> Prop => exists t : B -> Prop, @SETSPEC ((prod A B) -> Prop) GEN_PVAR_136 (@IN (B -> Prop) t g) (@CROSS A B (@UNIV A) t)))) (@COND ((prod A B) -> Prop) (g = (@EMPTY (B -> Prop))) (@INTERS (prod A B) (@GSPEC ((prod A B) -> Prop) (fun GEN_PVAR_137 : (prod A B) -> Prop => exists s : A -> Prop, @SETSPEC ((prod A B) -> Prop) GEN_PVAR_137 (@IN (A -> Prop) s f) (@CROSS A B s (@UNIV B))))) (@INTERS (prod A B) (@GSPEC ((prod A B) -> Prop) (fun GEN_PVAR_138 : (prod A B) -> Prop => exists s : A -> Prop, exists t : B -> Prop, @SETSPEC ((prod A B) -> Prop) GEN_PVAR_138 ((@IN (A -> Prop) s f) /\ (@IN (B -> Prop) t g)) (@CROSS A B s t)))))).
Axiom thm_DISJOINT_CROSS : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, forall s' : A -> Prop, forall t' : B -> Prop, (@DISJOINT (prod A B) (@CROSS A B s t) (@CROSS A B s' t')) = ((@DISJOINT A s s') \/ (@DISJOINT B t t')).
Axiom thm_ARB : forall {A : Type'}, (@ARB A) = (@ε A (fun x : A => False)).
Axiom thm_EXTENSIONAL : forall {A B : Type'}, forall s : A -> Prop, (@EXTENSIONAL A B s) = (@GSPEC (A -> B) (fun GEN_PVAR_141 : A -> B => exists f : A -> B, @SETSPEC (A -> B) GEN_PVAR_141 (forall x : A, (~ (@IN A x s)) -> (f x) = (@ARB B)) f)).
Axiom thm_IN_EXTENSIONAL : forall {A B : Type'}, forall s : A -> Prop, forall f : A -> B, (@IN (A -> B) f (@EXTENSIONAL A B s)) = (forall x : A, (~ (@IN A x s)) -> (f x) = (@ARB B)).
Axiom thm_IN_EXTENSIONAL_UNDEFINED : forall {A B : Type'}, forall s : A -> Prop, forall f : A -> B, forall x : A, ((@IN (A -> B) f (@EXTENSIONAL A B s)) /\ (~ (@IN A x s))) -> (f x) = (@ARB B).
Axiom thm_EXTENSIONAL_EMPTY : forall {A B : Type'}, (@EXTENSIONAL A B (@EMPTY A)) = (@INSERT (A -> B) (fun x : A => @ARB B) (@EMPTY (A -> B))).
Axiom thm_EXTENSIONAL_UNIV : forall {A B : Type'}, forall f : A -> B, @EXTENSIONAL A B (@UNIV A) f.
Axiom thm_EXTENSIONAL_EQ : forall {A B : Type'}, forall s : A -> Prop, forall f : A -> B, forall g : A -> B, ((@IN (A -> B) f (@EXTENSIONAL A B s)) /\ ((@IN (A -> B) g (@EXTENSIONAL A B s)) /\ (forall x : A, (@IN A x s) -> (f x) = (g x)))) -> f = g.
Axiom thm_RESTRICTION : forall {A B : Type'}, forall s : A -> Prop, forall f : A -> B, forall x : A, (@RESTRICTION A B s f x) = (@COND B (@IN A x s) (f x) (@ARB B)).
Axiom thm_RESTRICTION_THM : forall {A B : Type'}, forall s : A -> Prop, forall f : A -> B, (@RESTRICTION A B s f) = (fun x : A => @COND B (@IN A x s) (f x) (@ARB B)).
Axiom thm_RESTRICTION_DEFINED : forall {A B : Type'}, forall s : A -> Prop, forall f : A -> B, forall x : A, (@IN A x s) -> (@RESTRICTION A B s f x) = (f x).
Axiom thm_RESTRICTION_UNDEFINED : forall {A B : Type'}, forall s : A -> Prop, forall f : A -> B, forall x : A, (~ (@IN A x s)) -> (@RESTRICTION A B s f x) = (@ARB B).
Axiom thm_RESTRICTION_EQ : forall {A B : Type'}, forall s : A -> Prop, forall f : A -> B, forall x : A, forall y : B, ((@IN A x s) /\ ((f x) = y)) -> (@RESTRICTION A B s f x) = y.
Axiom thm_RESTRICTION_IN_EXTENSIONAL : forall {A B : Type'}, forall s : A -> Prop, forall f : A -> B, @IN (A -> B) (@RESTRICTION A B s f) (@EXTENSIONAL A B s).
Axiom thm_RESTRICTION_EXTENSION : forall {A B : Type'}, forall s : A -> Prop, forall f : A -> B, forall g : A -> B, ((@RESTRICTION A B s f) = (@RESTRICTION A B s g)) = (forall x : A, (@IN A x s) -> (f x) = (g x)).
Axiom thm_RESTRICTION_FIXPOINT : forall {A B : Type'}, forall s : A -> Prop, forall f : A -> B, ((@RESTRICTION A B s f) = f) = (@IN (A -> B) f (@EXTENSIONAL A B s)).
Axiom thm_RESTRICTION_UNIV : forall {A B : Type'}, forall f : A -> B, (@RESTRICTION A B (@UNIV A) f) = f.
Axiom thm_RESTRICTION_RESTRICTION : forall {A B : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall f : A -> B, (@SUBSET A s t) -> (@RESTRICTION A B s (@RESTRICTION A B t f)) = (@RESTRICTION A B s f).
Axiom thm_RESTRICTION_IDEMP : forall {A B : Type'}, forall s : A -> Prop, forall f : A -> B, (@RESTRICTION A B s (@RESTRICTION A B s f)) = (@RESTRICTION A B s f).
Axiom thm_IMAGE_RESTRICTION : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall t : A -> Prop, (@SUBSET A s t) -> (@IMAGE A B (@RESTRICTION A B t f) s) = (@IMAGE A B f s).
Axiom thm_RESTRICTION_COMPOSE_RIGHT : forall {A B C : Type'}, forall f : A -> B, forall g : B -> C, forall s : A -> Prop, (@RESTRICTION A C s (@o A B C g (@RESTRICTION A B s f))) = (@RESTRICTION A C s (@o A B C g f)).
Axiom thm_RESTRICTION_COMPOSE_LEFT : forall {A B C : Type'}, forall f : A -> B, forall g : B -> C, forall s : A -> Prop, forall t : B -> Prop, (@SUBSET B (@IMAGE A B f s) t) -> (@RESTRICTION A C s (@o A B C (@RESTRICTION B C t g) f)) = (@RESTRICTION A C s (@o A B C g f)).
Axiom thm_RESTRICTION_COMPOSE : forall {A B C : Type'}, forall f : A -> B, forall g : B -> C, forall s : A -> Prop, forall t : B -> Prop, (@SUBSET B (@IMAGE A B f s) t) -> (@RESTRICTION A C s (@o A B C (@RESTRICTION B C t g) (@RESTRICTION A B s f))) = (@RESTRICTION A C s (@o A B C g f)).
Axiom thm_RESTRICTION_UNIQUE : forall {A B : Type'}, forall s : A -> Prop, forall f : A -> B, forall g : A -> B, ((@RESTRICTION A B s f) = g) = ((@EXTENSIONAL A B s g) /\ (forall x : A, (@IN A x s) -> (f x) = (g x))).
Axiom thm_RESTRICTION_UNIQUE_ALT : forall {A B : Type'}, forall s : A -> Prop, forall f : A -> B, forall g : A -> B, (f = (@RESTRICTION A B s g)) = ((@EXTENSIONAL A B s f) /\ (forall x : A, (@IN A x s) -> (f x) = (g x))).
Axiom thm_cartesian_product : forall {A K : Type'}, forall k : K -> Prop, forall s : K -> A -> Prop, (@cartesian_product A K k s) = (@GSPEC (K -> A) (fun GEN_PVAR_142 : K -> A => exists f : K -> A, @SETSPEC (K -> A) GEN_PVAR_142 ((@EXTENSIONAL K A k f) /\ (forall i : K, (@IN K i k) -> @IN A (f i) (s i))) f)).
Axiom thm_IN_CARTESIAN_PRODUCT : forall {A K : Type'}, forall k : K -> Prop, forall s : K -> A -> Prop, forall x : K -> A, (@IN (K -> A) x (@cartesian_product A K k s)) = ((@EXTENSIONAL K A k x) /\ (forall i : K, (@IN K i k) -> @IN A (x i) (s i))).
Axiom thm_CARTESIAN_PRODUCT : forall {A K : Type'}, forall k : K -> Prop, forall s : K -> A -> Prop, (@cartesian_product A K k s) = (@GSPEC (K -> A) (fun GEN_PVAR_143 : K -> A => exists f : K -> A, @SETSPEC (K -> A) GEN_PVAR_143 (forall i : K, @IN A (f i) (@COND (A -> Prop) (@IN K i k) (s i) (@INSERT A (@ARB A) (@EMPTY A)))) f)).
Axiom thm_RESTRICTION_IN_CARTESIAN_PRODUCT : forall {A K : Type'}, forall k : K -> Prop, forall s : K -> A -> Prop, forall f : K -> A, (@IN (K -> A) (@RESTRICTION K A k f) (@cartesian_product A K k s)) = (forall i : K, (@IN K i k) -> @IN A (f i) (s i)).
Axiom thm_CARTESIAN_PRODUCT_AS_RESTRICTIONS : forall {A K : Type'}, forall k : K -> Prop, forall s : K -> A -> Prop, (@cartesian_product A K k s) = (@GSPEC (K -> A) (fun GEN_PVAR_144 : K -> A => exists f : K -> A, @SETSPEC (K -> A) GEN_PVAR_144 (forall i : K, (@IN K i k) -> @IN A (f i) (s i)) (@RESTRICTION K A k f))).
Axiom thm_CARTESIAN_PRODUCT_EQ_EMPTY : forall {A K : Type'}, forall k : K -> Prop, forall s : K -> A -> Prop, ((@cartesian_product A K k s) = (@EMPTY (K -> A))) = (exists i : K, (@IN K i k) /\ ((s i) = (@EMPTY A))).
Axiom thm_CARTESIAN_PRODUCT_EMPTY : forall {A K : Type'}, forall s : K -> A -> Prop, (@cartesian_product A K (@EMPTY K) s) = (@INSERT (K -> A) (fun i : K => @ARB A) (@EMPTY (K -> A))).
Axiom thm_CARTESIAN_PRODUCT_EQ_MEMBERS : forall {A K : Type'}, forall k : K -> Prop, forall s : K -> A -> Prop, forall x : K -> A, forall y : K -> A, ((@IN (K -> A) x (@cartesian_product A K k s)) /\ ((@IN (K -> A) y (@cartesian_product A K k s)) /\ (forall i : K, (@IN K i k) -> (x i) = (y i)))) -> x = y.
Axiom thm_CARTESIAN_PRODUCT_EQ_MEMBERS_EQ : forall {A K : Type'}, forall k : K -> Prop, forall s : K -> A -> Prop, forall x : K -> A, forall y : K -> A, ((@IN (K -> A) x (@cartesian_product A K k s)) /\ (@IN (K -> A) y (@cartesian_product A K k s))) -> (x = y) = (forall i : K, (@IN K i k) -> (x i) = (y i)).
Axiom thm_SUBSET_CARTESIAN_PRODUCT : forall {A K : Type'}, forall k : K -> Prop, forall s : K -> A -> Prop, forall t : K -> A -> Prop, (@SUBSET (K -> A) (@cartesian_product A K k s) (@cartesian_product A K k t)) = (((@cartesian_product A K k s) = (@EMPTY (K -> A))) \/ (forall i : K, (@IN K i k) -> @SUBSET A (s i) (t i))).
Axiom thm_CARTESIAN_PRODUCT_EQ : forall {A K : Type'}, forall k : K -> Prop, forall s : K -> A -> Prop, forall t : K -> A -> Prop, ((@cartesian_product A K k s) = (@cartesian_product A K k t)) = ((((@cartesian_product A K k s) = (@EMPTY (K -> A))) /\ ((@cartesian_product A K k t) = (@EMPTY (K -> A)))) \/ (forall i : K, (@IN K i k) -> (s i) = (t i))).
Axiom thm_INTER_CARTESIAN_PRODUCT : forall {A K : Type'}, forall k : K -> Prop, forall s : K -> A -> Prop, forall t : K -> A -> Prop, (@INTER (K -> A) (@cartesian_product A K k s) (@cartesian_product A K k t)) = (@cartesian_product A K k (fun i : K => @INTER A (s i) (t i))).
Axiom thm_CARTESIAN_PRODUCT_UNIV : forall {A K : Type'}, (@cartesian_product A K (@UNIV K) (fun i : K => @UNIV A)) = (@UNIV (K -> A)).
Axiom thm_CARTESIAN_PRODUCT_SINGS : forall {A K : Type'}, forall k : K -> Prop, forall x : K -> A, (@EXTENSIONAL K A k x) -> (@cartesian_product A K k (fun i : K => @INSERT A (x i) (@EMPTY A))) = (@INSERT (K -> A) x (@EMPTY (K -> A))).
Axiom thm_CARTESIAN_PRODUCT_SINGS_GEN : forall {A K : Type'}, forall k : K -> Prop, forall x : K -> A, (@cartesian_product A K k (fun i : K => @INSERT A (x i) (@EMPTY A))) = (@INSERT (K -> A) (@RESTRICTION K A k x) (@EMPTY (K -> A))).
Axiom thm_IMAGE_PROJECTION_CARTESIAN_PRODUCT : forall {A K : Type'}, forall k : K -> Prop, forall s : K -> A -> Prop, forall i : K, (@IMAGE (K -> A) A (fun x : K -> A => x i) (@cartesian_product A K k s)) = (@COND (A -> Prop) ((@cartesian_product A K k s) = (@EMPTY (K -> A))) (@EMPTY A) (@COND (A -> Prop) (@IN K i k) (s i) (@INSERT A (@ARB A) (@EMPTY A)))).
Axiom thm_FORALL_CARTESIAN_PRODUCT_ELEMENTS : forall {A K : Type'}, forall P : K -> A -> Prop, forall k : K -> Prop, forall s : K -> A -> Prop, (forall z : K -> A, forall i : K, ((@IN (K -> A) z (@cartesian_product A K k s)) /\ (@IN K i k)) -> P i (z i)) = (((@cartesian_product A K k s) = (@EMPTY (K -> A))) \/ (forall i : K, forall x : A, ((@IN K i k) /\ (@IN A x (s i))) -> P i x)).
Axiom thm_FORALL_CARTESIAN_PRODUCT_ELEMENTS_EQ : forall {A K : Type'}, forall P : K -> A -> Prop, forall k : K -> Prop, forall s : K -> A -> Prop, (~ ((@cartesian_product A K k s) = (@EMPTY (K -> A)))) -> (forall i : K, forall x : A, ((@IN K i k) /\ (@IN A x (s i))) -> P i x) = (forall z : K -> A, forall i : K, ((@IN (K -> A) z (@cartesian_product A K k s)) /\ (@IN K i k)) -> P i (z i)).
Axiom thm_EXISTS_CARTESIAN_PRODUCT_ELEMENT : forall {A K : Type'}, forall P : K -> A -> Prop, forall k : K -> Prop, forall s : K -> A -> Prop, (exists z : K -> A, (@IN (K -> A) z (@cartesian_product A K k s)) /\ (forall i : K, (@IN K i k) -> P i (z i))) = (forall i : K, (@IN K i k) -> exists x : A, (@IN A x (s i)) /\ (P i x)).
Axiom thm_product_map : forall {A B K : Type'}, forall k : K -> Prop, forall f : K -> A -> B, (@product_map A B K k f) = (fun x : K -> A => @RESTRICTION K B k (fun i : K => f i (x i))).
Axiom thm_PRODUCT_MAP_RESTRICTION : forall {A B K : Type'}, forall f : K -> A -> B, forall k : K -> Prop, forall x : K -> A, (@product_map A B K k f (@RESTRICTION K A k x)) = (@RESTRICTION K B k (fun i : K => f i (x i))).
Axiom thm_IMAGE_PRODUCT_MAP : forall {A B K : Type'}, forall f : K -> A -> B, forall k : K -> Prop, forall s : K -> A -> Prop, (@IMAGE (K -> A) (K -> B) (@product_map A B K k f) (@cartesian_product A K k s)) = (@cartesian_product B K k (fun i : K => @IMAGE A B (f i) (s i))).
Axiom thm_disjoint_union : forall {A K : Type'}, forall k : K -> Prop, forall s : K -> A -> Prop, (@disjoint_union A K k s) = (@GSPEC (prod K A) (fun GEN_PVAR_145 : prod K A => exists i : K, exists x : A, @SETSPEC (prod K A) GEN_PVAR_145 ((@IN K i k) /\ (@IN A x (s i))) (@pair K A i x))).
Axiom thm_SUBSET_DISJOINT_UNION : forall {A K : Type'}, forall k : K -> Prop, forall s : K -> A -> Prop, forall t : K -> A -> Prop, (@SUBSET (prod K A) (@disjoint_union A K k s) (@disjoint_union A K k t)) = (forall i : K, (@IN K i k) -> @SUBSET A (s i) (t i)).
Axiom thm_DISJOINT_UNION_EQ : forall {A K : Type'}, forall k : K -> Prop, forall s : K -> A -> Prop, forall t : K -> A -> Prop, ((@disjoint_union A K k s) = (@disjoint_union A K k t)) = (forall i : K, (@IN K i k) -> (s i) = (t i)).
Axiom thm_SUBSET_DISJOINT_UNION_EXISTS : forall {A K : Type'}, forall k : K -> Prop, forall s : K -> A -> Prop, forall u : (prod K A) -> Prop, (@SUBSET (prod K A) u (@disjoint_union A K k s)) = (exists t : K -> A -> Prop, (u = (@disjoint_union A K k t)) /\ (forall i : K, (@IN K i k) -> @SUBSET A (t i) (s i))).
Axiom thm_INTER_DISJOINT_UNION : forall {A K : Type'}, forall k : K -> Prop, forall s : K -> A -> Prop, forall t : K -> A -> Prop, (@INTER (prod K A) (@disjoint_union A K k s) (@disjoint_union A K k t)) = (@disjoint_union A K k (fun i : K => @INTER A (s i) (t i))).
Axiom thm_UNION_DISJOINT_UNION : forall {A K : Type'}, forall k : K -> Prop, forall s : K -> A -> Prop, forall t : K -> A -> Prop, (@UNION (prod K A) (@disjoint_union A K k s) (@disjoint_union A K k t)) = (@disjoint_union A K k (fun i : K => @UNION A (s i) (t i))).
Axiom thm_DISJOINT_UNION_EQ_EMPTY : forall {A K : Type'}, forall k : K -> Prop, forall s : K -> A -> Prop, ((@disjoint_union A K k s) = (@EMPTY (prod K A))) = (forall i : K, (@IN K i k) -> (s i) = (@EMPTY A)).
Axiom thm_DISJOINT_DISJOINT_UNION : forall {A K : Type'}, forall k : K -> Prop, forall s : K -> A -> Prop, forall t : K -> A -> Prop, (@DISJOINT (prod K A) (@disjoint_union A K k s) (@disjoint_union A K k t)) = (forall i : K, (@IN K i k) -> @DISJOINT A (s i) (t i)).
Axiom thm_HAS_SIZE_FUNSPACE : forall {A B : Type'}, forall d : B, forall n : nat, forall t : B -> Prop, forall m : nat, forall s : A -> Prop, ((@HAS_SIZE A s m) /\ (@HAS_SIZE B t n)) -> @HAS_SIZE (A -> B) (@GSPEC (A -> B) (fun GEN_PVAR_150 : A -> B => exists f : A -> B, @SETSPEC (A -> B) GEN_PVAR_150 ((forall x : A, (@IN A x s) -> @IN B (f x) t) /\ (forall x : A, (~ (@IN A x s)) -> (f x) = d)) f)) (Nat.pow n m).
Axiom thm_CARD_FUNSPACE : forall {A B : Type'} (d : B), forall s : A -> Prop, forall t : B -> Prop, ((@FINITE A s) /\ (@FINITE B t)) -> (@CARD (A -> B) (@GSPEC (A -> B) (fun GEN_PVAR_151 : A -> B => exists f : A -> B, @SETSPEC (A -> B) GEN_PVAR_151 ((forall x : A, (@IN A x s) -> @IN B (f x) t) /\ (forall x : A, (~ (@IN A x s)) -> (f x) = d)) f))) = (Nat.pow (@CARD B t) (@CARD A s)).
Axiom thm_FINITE_FUNSPACE : forall {A B : Type'} (d : B), forall s : A -> Prop, forall t : B -> Prop, ((@FINITE A s) /\ (@FINITE B t)) -> @FINITE (A -> B) (@GSPEC (A -> B) (fun GEN_PVAR_152 : A -> B => exists f : A -> B, @SETSPEC (A -> B) GEN_PVAR_152 ((forall x : A, (@IN A x s) -> @IN B (f x) t) /\ (forall x : A, (~ (@IN A x s)) -> (f x) = d)) f)).
Axiom thm_HAS_SIZE_FUNSPACE_UNIV : forall {A B : Type'}, forall m : nat, forall n : nat, ((@HAS_SIZE A (@UNIV A) m) /\ (@HAS_SIZE B (@UNIV B) n)) -> @HAS_SIZE (A -> B) (@UNIV (A -> B)) (Nat.pow n m).
Axiom thm_CARD_FUNSPACE_UNIV : forall {A B : Type'}, ((@FINITE A (@UNIV A)) /\ (@FINITE B (@UNIV B))) -> (@CARD (A -> B) (@UNIV (A -> B))) = (Nat.pow (@CARD B (@UNIV B)) (@CARD A (@UNIV A))).
Axiom thm_FINITE_FUNSPACE_UNIV : forall {A B : Type'}, ((@FINITE A (@UNIV A)) /\ (@FINITE B (@UNIV B))) -> @FINITE (A -> B) (@UNIV (A -> B)).
Axiom thm_HAS_SIZE_BOOL : @HAS_SIZE Prop (@UNIV Prop) (NUMERAL (BIT0 (BIT1 0))).
Axiom thm_CARD_BOOL : (@CARD Prop (@UNIV Prop)) = (NUMERAL (BIT0 (BIT1 0))).
Axiom thm_FINITE_BOOL : @FINITE Prop (@UNIV Prop).
Axiom thm_HAS_SIZE_POWERSET : forall {A : Type'}, forall s : A -> Prop, forall n : nat, (@HAS_SIZE A s n) -> @HAS_SIZE (A -> Prop) (@GSPEC (A -> Prop) (fun GEN_PVAR_155 : A -> Prop => exists t : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_155 (@SUBSET A t s) t)) (Nat.pow (NUMERAL (BIT0 (BIT1 0))) n).
Axiom thm_CARD_POWERSET : forall {A : Type'}, forall s : A -> Prop, (@FINITE A s) -> (@CARD (A -> Prop) (@GSPEC (A -> Prop) (fun GEN_PVAR_156 : A -> Prop => exists t : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_156 (@SUBSET A t s) t))) = (Nat.pow (NUMERAL (BIT0 (BIT1 0))) (@CARD A s)).
Axiom thm_FINITE_POWERSET : forall {A : Type'}, forall s : A -> Prop, (@FINITE A s) -> @FINITE (A -> Prop) (@GSPEC (A -> Prop) (fun GEN_PVAR_157 : A -> Prop => exists t : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_157 (@SUBSET A t s) t)).
Axiom thm_FINITE_POWERSET_EQ : forall {A : Type'}, forall s : A -> Prop, (@FINITE (A -> Prop) (@GSPEC (A -> Prop) (fun GEN_PVAR_158 : A -> Prop => exists t : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_158 (@SUBSET A t s) t))) = (@FINITE A s).
Axiom thm_FINITE_RESTRICTED_SUBSETS : forall {A : Type'}, forall P : (A -> Prop) -> Prop, forall s : A -> Prop, (@FINITE A s) -> @FINITE (A -> Prop) (@GSPEC (A -> Prop) (fun GEN_PVAR_160 : A -> Prop => exists t : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_160 ((@SUBSET A t s) /\ (P t)) t)).
Axiom thm_FINITE_UNIONS : forall {A : Type'}, forall s : (A -> Prop) -> Prop, (@FINITE A (@UNIONS A s)) = ((@FINITE (A -> Prop) s) /\ (forall t : A -> Prop, (@IN (A -> Prop) t s) -> @FINITE A t)).
Axiom thm_FINITE_CARD_LE_UNIONS : forall {A B : Type'}, forall s : A -> Prop, forall t : A -> B -> Prop, forall m : nat, forall n : nat, ((forall x : A, (@IN A x s) -> (@FINITE B (t x)) /\ (Peano.le (@CARD B (t x)) n)) /\ ((@FINITE A s) /\ (Peano.le (@CARD A s) m))) -> (@FINITE B (@UNIONS B (@GSPEC (B -> Prop) (fun GEN_PVAR_161 : B -> Prop => exists x : A, @SETSPEC (B -> Prop) GEN_PVAR_161 (@IN A x s) (t x))))) /\ (Peano.le (@CARD B (@UNIONS B (@GSPEC (B -> Prop) (fun GEN_PVAR_162 : B -> Prop => exists x : A, @SETSPEC (B -> Prop) GEN_PVAR_162 (@IN A x s) (t x))))) (Nat.mul m n)).
Axiom thm_POWERSET_CLAUSES : forall {A : Type'}, ((@GSPEC (A -> Prop) (fun GEN_PVAR_163 : A -> Prop => exists s : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_163 (@SUBSET A s (@EMPTY A)) s)) = (@INSERT (A -> Prop) (@EMPTY A) (@EMPTY (A -> Prop)))) /\ (forall a : A, forall t : A -> Prop, (@GSPEC (A -> Prop) (fun GEN_PVAR_164 : A -> Prop => exists s : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_164 (@SUBSET A s (@INSERT A a t)) s)) = (@UNION (A -> Prop) (@GSPEC (A -> Prop) (fun GEN_PVAR_165 : A -> Prop => exists s : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_165 (@SUBSET A s t) s)) (@IMAGE (A -> Prop) (A -> Prop) (fun s : A -> Prop => @INSERT A a s) (@GSPEC (A -> Prop) (fun GEN_PVAR_166 : A -> Prop => exists s : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_166 (@SUBSET A s t) s))))).
Axiom thm_FINITE_IMAGE_INFINITE : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, ((@INFINITE A s) /\ (@FINITE B (@IMAGE A B f s))) -> exists a : A, (@IN A a s) /\ (@INFINITE A (@GSPEC A (fun GEN_PVAR_171 : A => exists x : A, @SETSPEC A GEN_PVAR_171 ((@IN A x s) /\ ((f x) = (f a))) x))).
Axiom thm_FINITE_RESTRICTED_POWERSET : forall {A : Type'}, forall s : A -> Prop, forall n : nat, (@FINITE (A -> Prop) (@GSPEC (A -> Prop) (fun GEN_PVAR_176 : A -> Prop => exists t : A -> Prop, @SETSPEC (A -> Prop) GEN_PVAR_176 ((@SUBSET A t s) /\ (@HAS_SIZE A t n)) t))) = ((@FINITE A s) \/ (n = (NUMERAL 0))).
Axiom thm_FINITE_RESTRICTED_FUNSPACE : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, forall k : A -> B, ((@FINITE A s) /\ (@FINITE B t)) -> @FINITE (A -> B) (@GSPEC (A -> B) (fun GEN_PVAR_180 : A -> B => exists f : A -> B, @SETSPEC (A -> B) GEN_PVAR_180 ((@SUBSET B (@IMAGE A B f s) t) /\ (@SUBSET A (@GSPEC A (fun GEN_PVAR_179 : A => exists x : A, @SETSPEC A GEN_PVAR_179 (~ ((f x) = (k x))) x)) s)) f)).
Axiom thm_NUMSEG_CLAUSES_LT : ((@GSPEC nat (fun GEN_PVAR_181 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_181 (Peano.lt i (NUMERAL 0)) i)) = (@EMPTY nat)) /\ (forall k : nat, (@GSPEC nat (fun GEN_PVAR_182 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_182 (Peano.lt i (S k)) i)) = (@INSERT nat k (@GSPEC nat (fun GEN_PVAR_183 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_183 (Peano.lt i k) i)))).
Axiom thm_HAS_SIZE_NUMSEG_LT : forall n : nat, @HAS_SIZE nat (@GSPEC nat (fun GEN_PVAR_184 : nat => exists m : nat, @SETSPEC nat GEN_PVAR_184 (Peano.lt m n) m)) n.
Axiom thm_CARD_NUMSEG_LT : forall n : nat, (@CARD nat (@GSPEC nat (fun GEN_PVAR_185 : nat => exists m : nat, @SETSPEC nat GEN_PVAR_185 (Peano.lt m n) m))) = n.
Axiom thm_FINITE_NUMSEG_LT : forall n : nat, @FINITE nat (@GSPEC nat (fun GEN_PVAR_186 : nat => exists m : nat, @SETSPEC nat GEN_PVAR_186 (Peano.lt m n) m)).
Axiom thm_NUMSEG_CLAUSES_LE : ((@GSPEC nat (fun GEN_PVAR_187 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_187 (Peano.le i (NUMERAL 0)) i)) = (@INSERT nat (NUMERAL 0) (@EMPTY nat))) /\ (forall k : nat, (@GSPEC nat (fun GEN_PVAR_188 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_188 (Peano.le i (S k)) i)) = (@INSERT nat (S k) (@GSPEC nat (fun GEN_PVAR_189 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_189 (Peano.le i k) i)))).
Axiom thm_HAS_SIZE_NUMSEG_LE : forall n : nat, @HAS_SIZE nat (@GSPEC nat (fun GEN_PVAR_190 : nat => exists m : nat, @SETSPEC nat GEN_PVAR_190 (Peano.le m n) m)) (Nat.add n (NUMERAL (BIT1 0))).
Axiom thm_FINITE_NUMSEG_LE : forall n : nat, @FINITE nat (@GSPEC nat (fun GEN_PVAR_191 : nat => exists m : nat, @SETSPEC nat GEN_PVAR_191 (Peano.le m n) m)).
Axiom thm_CARD_NUMSEG_LE : forall n : nat, (@CARD nat (@GSPEC nat (fun GEN_PVAR_192 : nat => exists m : nat, @SETSPEC nat GEN_PVAR_192 (Peano.le m n) m))) = (Nat.add n (NUMERAL (BIT1 0))).
Axiom thm_num_FINITE : forall s : nat -> Prop, (@FINITE nat s) = (exists a : nat, forall x : nat, (@IN nat x s) -> Peano.le x a).
Axiom thm_num_FINITE_AVOID : forall s : nat -> Prop, (@FINITE nat s) -> exists a : nat, ~ (@IN nat a s).
Axiom thm_num_INFINITE_EQ : forall s : nat -> Prop, (@INFINITE nat s) = (forall N : nat, exists n : nat, (Peano.le N n) /\ (@IN nat n s)).
Axiom thm_num_INFINITE : @INFINITE nat (@UNIV nat).
Axiom thm_string_INFINITE : @INFINITE (list Ascii.ascii) (@UNIV (list Ascii.ascii)).
Axiom thm_FINITE_REAL_INTERVAL : (forall a : R, ~ (@FINITE R (@GSPEC R (fun GEN_PVAR_202 : R => exists x : R, @SETSPEC R GEN_PVAR_202 (Rlt a x) x)))) /\ ((forall a : R, ~ (@FINITE R (@GSPEC R (fun GEN_PVAR_203 : R => exists x : R, @SETSPEC R GEN_PVAR_203 (Rle a x) x)))) /\ ((forall b : R, ~ (@FINITE R (@GSPEC R (fun GEN_PVAR_204 : R => exists x : R, @SETSPEC R GEN_PVAR_204 (Rlt x b) x)))) /\ ((forall b : R, ~ (@FINITE R (@GSPEC R (fun GEN_PVAR_205 : R => exists x : R, @SETSPEC R GEN_PVAR_205 (Rle x b) x)))) /\ ((forall a : R, forall b : R, (@FINITE R (@GSPEC R (fun GEN_PVAR_206 : R => exists x : R, @SETSPEC R GEN_PVAR_206 ((Rlt a x) /\ (Rlt x b)) x))) = (Rle b a)) /\ ((forall a : R, forall b : R, (@FINITE R (@GSPEC R (fun GEN_PVAR_207 : R => exists x : R, @SETSPEC R GEN_PVAR_207 ((Rle a x) /\ (Rlt x b)) x))) = (Rle b a)) /\ ((forall a : R, forall b : R, (@FINITE R (@GSPEC R (fun GEN_PVAR_208 : R => exists x : R, @SETSPEC R GEN_PVAR_208 ((Rlt a x) /\ (Rle x b)) x))) = (Rle b a)) /\ (forall a : R, forall b : R, (@FINITE R (@GSPEC R (fun GEN_PVAR_209 : R => exists x : R, @SETSPEC R GEN_PVAR_209 ((Rle a x) /\ (Rle x b)) x))) = (Rle b a)))))))).
Axiom thm_real_INFINITE : @INFINITE R (@UNIV R).
Axiom thm_HAS_SIZE_INDEX : forall {A : Type'}, forall s : A -> Prop, forall n : nat, (@HAS_SIZE A s n) -> exists f : nat -> A, (forall m : nat, (Peano.lt m n) -> @IN A (f m) s) /\ (forall x : A, (@IN A x s) -> @ex1 nat (fun m : nat => (Peano.lt m n) /\ ((f m) = x))).
Axiom thm_INFINITE_ENUMERATE : forall s : nat -> Prop, (@INFINITE nat s) -> exists r : nat -> nat, (forall m : nat, forall n : nat, (Peano.lt m n) -> Peano.lt (r m) (r n)) /\ ((@IMAGE nat nat r (@UNIV nat)) = s).
Axiom thm_INFINITE_ENUMERATE_EQ : forall s : nat -> Prop, (@INFINITE nat s) = (exists r : nat -> nat, (forall m : nat, forall n : nat, (Peano.lt m n) -> Peano.lt (r m) (r n)) /\ ((@IMAGE nat nat r (@UNIV nat)) = s)).
Axiom thm_INFINITE_ENUMERATE_SUBSET : forall {A : Type'}, forall s : A -> Prop, (@INFINITE A s) = (exists f : nat -> A, (forall x : nat, @IN A (f x) s) /\ (forall x : nat, forall y : nat, ((f x) = (f y)) -> x = y)).
Axiom thm_set_of_list : forall {A : Type'} (h : A) (t : list A), ((@set_of_list A (@nil A)) = (@EMPTY A)) /\ ((@set_of_list A (@cons A h t)) = (@INSERT A h (@set_of_list A t))).
Axiom thm_list_of_set : forall {A : Type'}, forall s : A -> Prop, (@list_of_set A s) = (@ε (list A) (fun l : list A => ((@set_of_list A l) = s) /\ ((@List.length A l) = (@CARD A s)))).
Axiom thm_LIST_OF_SET_PROPERTIES : forall {A : Type'}, forall s : A -> Prop, (@FINITE A s) -> ((@set_of_list A (@list_of_set A s)) = s) /\ ((@List.length A (@list_of_set A s)) = (@CARD A s)).
Axiom thm_SET_OF_LIST_OF_SET : forall {A : Type'}, forall s : A -> Prop, (@FINITE A s) -> (@set_of_list A (@list_of_set A s)) = s.
Axiom thm_LENGTH_LIST_OF_SET : forall {A : Type'}, forall s : A -> Prop, (@FINITE A s) -> (@List.length A (@list_of_set A s)) = (@CARD A s).
Axiom thm_MEM_LIST_OF_SET : forall {A : Type'}, forall s : A -> Prop, (@FINITE A s) -> forall x : A, (@List.In A x (@list_of_set A s)) = (@IN A x s).
Axiom thm_FINITE_SET_OF_LIST : forall {A : Type'}, forall l : list A, @FINITE A (@set_of_list A l).
Axiom thm_IN_SET_OF_LIST : forall {A : Type'}, forall x : A, forall l : list A, (@IN A x (@set_of_list A l)) = (@List.In A x l).
Axiom thm_SET_OF_LIST_APPEND : forall {A : Type'}, forall l1 : list A, forall l2 : list A, (@set_of_list A (@List.app A l1 l2)) = (@UNION A (@set_of_list A l1) (@set_of_list A l2)).
Axiom thm_SET_OF_LIST_MAP : forall {A B : Type'}, forall f : A -> B, forall l : list A, (@set_of_list B (@List.map A B f l)) = (@IMAGE A B f (@set_of_list A l)).
Axiom thm_SET_OF_LIST_EQ_EMPTY : forall {A : Type'}, forall l : list A, ((@set_of_list A l) = (@EMPTY A)) = (l = (@nil A)).
Axiom thm_LIST_OF_SET_EMPTY : forall {A : Type'}, (@list_of_set A (@EMPTY A)) = (@nil A).
Axiom thm_LIST_OF_SET_SING : forall {A : Type'}, forall a : A, (@list_of_set A (@INSERT A a (@EMPTY A))) = (@cons A a (@nil A)).
Axiom thm_pairwise : forall {A : Type'}, forall s : A -> Prop, forall r : A -> A -> Prop, (@pairwise A r s) = (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ (~ (x = y)))) -> r x y).
Axiom thm_PAIRWISE_EMPTY : forall {A : Type'}, forall r : A -> A -> Prop, (@pairwise A r (@EMPTY A)) = True.
Axiom thm_PAIRWISE_SING : forall {A : Type'}, forall r : A -> A -> Prop, forall x : A, (@pairwise A r (@INSERT A x (@EMPTY A))) = True.
Axiom thm_PAIRWISE_IMP : forall {A : Type'}, forall P : A -> A -> Prop, forall Q : A -> A -> Prop, forall s : A -> Prop, ((@pairwise A P s) /\ (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((P x y) /\ (~ (x = y))))) -> Q x y)) -> @pairwise A Q s.
Axiom thm_PAIRWISE_MONO : forall {A : Type'}, forall r : A -> A -> Prop, forall s : A -> Prop, forall t : A -> Prop, ((@pairwise A r s) /\ (@SUBSET A t s)) -> @pairwise A r t.
Axiom thm_PAIRWISE_AND : forall {A : Type'}, forall R' : A -> A -> Prop, forall R'' : A -> A -> Prop, forall s : A -> Prop, ((@pairwise A R' s) /\ (@pairwise A R'' s)) = (@pairwise A (fun x : A => fun y : A => (R' x y) /\ (R'' x y)) s).
Axiom thm_PAIRWISE_INSERT : forall {A : Type'}, forall r : A -> A -> Prop, forall x : A, forall s : A -> Prop, (@pairwise A r (@INSERT A x s)) = ((forall y : A, ((@IN A y s) /\ (~ (y = x))) -> (r x y) /\ (r y x)) /\ (@pairwise A r s)).
Axiom thm_PAIRWISE_INSERT_SYMMETRIC : forall {A : Type'}, forall r : A -> A -> Prop, forall x : A, forall s : A -> Prop, (forall y : A, (@IN A y s) -> (r x y) = (r y x)) -> (@pairwise A r (@INSERT A x s)) = ((forall y : A, ((@IN A y s) /\ (~ (y = x))) -> r x y) /\ (@pairwise A r s)).
Axiom thm_PAIRWISE_IMAGE : forall {A B : Type'} (s : A -> Prop), forall r : B -> B -> Prop, forall f : A -> B, (@pairwise B r (@IMAGE A B f s)) = (@pairwise A (fun x : A => fun y : A => (~ ((f x) = (f y))) -> r (f x) (f y)) s).
Axiom thm_PAIRWISE_UNION : forall {A : Type'}, forall R' : A -> A -> Prop, forall s : A -> Prop, forall t : A -> Prop, (@pairwise A R' (@UNION A s t)) = ((@pairwise A R' s) /\ ((@pairwise A R' t) /\ (forall x : A, forall y : A, ((@IN A x (@DIFF A s t)) /\ (@IN A y (@DIFF A t s))) -> (R' x y) /\ (R' y x)))).
Axiom thm_PAIRWISE_CHAIN_UNIONS : forall {A : Type'}, forall R' : A -> A -> Prop, forall c : (A -> Prop) -> Prop, ((forall s : A -> Prop, (@IN (A -> Prop) s c) -> @pairwise A R' s) /\ (forall s : A -> Prop, forall t : A -> Prop, ((@IN (A -> Prop) s c) /\ (@IN (A -> Prop) t c)) -> (@SUBSET A s t) \/ (@SUBSET A t s))) -> @pairwise A R' (@UNIONS A c).
Axiom thm_DIFF_UNIONS_PAIRWISE_DISJOINT : forall {A : Type'}, forall s : (A -> Prop) -> Prop, forall t : (A -> Prop) -> Prop, ((@pairwise (A -> Prop) (@DISJOINT A) s) /\ (@SUBSET (A -> Prop) t s)) -> (@DIFF A (@UNIONS A s) (@UNIONS A t)) = (@UNIONS A (@DIFF (A -> Prop) s t)).
Axiom thm_INTER_UNIONS_PAIRWISE_DISJOINT : forall {A : Type'}, forall s : (A -> Prop) -> Prop, forall t : (A -> Prop) -> Prop, (@pairwise (A -> Prop) (@DISJOINT A) (@UNION (A -> Prop) s t)) -> (@INTER A (@UNIONS A s) (@UNIONS A t)) = (@UNIONS A (@INTER (A -> Prop) s t)).
Axiom thm_PSUBSET_UNIONS_PAIRWISE_DISJOINT : forall {A : Type'}, forall u : (A -> Prop) -> Prop, forall v : (A -> Prop) -> Prop, ((@pairwise (A -> Prop) (@DISJOINT A) v) /\ (@PSUBSET (A -> Prop) u (@DELETE (A -> Prop) v (@EMPTY A)))) -> @PSUBSET A (@UNIONS A u) (@UNIONS A v).
Axiom thm_UNION_OF : forall {A : Type'}, forall P : ((A -> Prop) -> Prop) -> Prop, forall Q : (A -> Prop) -> Prop, (@UNION_OF A P Q) = (fun s : A -> Prop => exists u : (A -> Prop) -> Prop, (P u) /\ ((forall c : A -> Prop, (@IN (A -> Prop) c u) -> Q c) /\ ((@UNIONS A u) = s))).
Axiom thm_INTERSECTION_OF : forall {A : Type'}, forall P : ((A -> Prop) -> Prop) -> Prop, forall Q : (A -> Prop) -> Prop, (@INTERSECTION_OF A P Q) = (fun s : A -> Prop => exists u : (A -> Prop) -> Prop, (P u) /\ ((forall c : A -> Prop, (@IN (A -> Prop) c u) -> Q c) /\ ((@INTERS A u) = s))).
Axiom thm_UNION_OF_INC : forall {A : Type'}, forall P : ((A -> Prop) -> Prop) -> Prop, forall Q : (A -> Prop) -> Prop, forall s : A -> Prop, ((P (@INSERT (A -> Prop) s (@EMPTY (A -> Prop)))) /\ (Q s)) -> @UNION_OF A P Q s.
Axiom thm_INTERSECTION_OF_INC : forall {A : Type'}, forall P : ((A -> Prop) -> Prop) -> Prop, forall Q : (A -> Prop) -> Prop, forall s : A -> Prop, ((P (@INSERT (A -> Prop) s (@EMPTY (A -> Prop)))) /\ (Q s)) -> @INTERSECTION_OF A P Q s.
Axiom thm_UNION_OF_MONO : forall {A : Type'}, forall P : ((A -> Prop) -> Prop) -> Prop, forall Q : (A -> Prop) -> Prop, forall Q' : (A -> Prop) -> Prop, forall s : A -> Prop, ((@UNION_OF A P Q s) /\ (forall x : A -> Prop, (Q x) -> Q' x)) -> @UNION_OF A P Q' s.
Axiom thm_INTERSECTION_OF_MONO : forall {A : Type'}, forall P : ((A -> Prop) -> Prop) -> Prop, forall Q : (A -> Prop) -> Prop, forall Q' : (A -> Prop) -> Prop, forall s : A -> Prop, ((@INTERSECTION_OF A P Q s) /\ (forall x : A -> Prop, (Q x) -> Q' x)) -> @INTERSECTION_OF A P Q' s.
Axiom thm_FORALL_UNION_OF : forall {A : Type'} (P : ((A -> Prop) -> Prop) -> Prop) (Q : (A -> Prop) -> Prop) (R' : (A -> Prop) -> Prop), (forall s : A -> Prop, (@UNION_OF A P Q s) -> R' s) = (forall t : (A -> Prop) -> Prop, ((P t) /\ (forall c : A -> Prop, (@IN (A -> Prop) c t) -> Q c)) -> R' (@UNIONS A t)).
Axiom thm_FORALL_INTERSECTION_OF : forall {A : Type'} (P : ((A -> Prop) -> Prop) -> Prop) (Q : (A -> Prop) -> Prop) (R' : (A -> Prop) -> Prop), (forall s : A -> Prop, (@INTERSECTION_OF A P Q s) -> R' s) = (forall t : (A -> Prop) -> Prop, ((P t) /\ (forall c : A -> Prop, (@IN (A -> Prop) c t) -> Q c)) -> R' (@INTERS A t)).
Axiom thm_UNION_OF_EMPTY : forall {A : Type'}, forall P : ((A -> Prop) -> Prop) -> Prop, forall Q : (A -> Prop) -> Prop, (P (@EMPTY (A -> Prop))) -> @UNION_OF A P Q (@EMPTY A).
Axiom thm_INTERSECTION_OF_EMPTY : forall {A : Type'}, forall P : ((A -> Prop) -> Prop) -> Prop, forall Q : (A -> Prop) -> Prop, (P (@EMPTY (A -> Prop))) -> @INTERSECTION_OF A P Q (@UNIV A).
Axiom thm_ARBITRARY : forall {A : Type'}, forall s : (A -> Prop) -> Prop, (@ARBITRARY A s) = True.
Axiom thm_ARBITRARY_UNION_OF_ALT : forall {A : Type'}, forall B : (A -> Prop) -> Prop, forall s : A -> Prop, (@UNION_OF A (@ARBITRARY A) B s) = (forall x : A, (@IN A x s) -> exists u : A -> Prop, (@IN (A -> Prop) u B) /\ ((@IN A x u) /\ (@SUBSET A u s))).
Axiom thm_ARBITRARY_UNION_OF_EMPTY : forall {A : Type'}, forall P : (A -> Prop) -> Prop, @UNION_OF A (@ARBITRARY A) P (@EMPTY A).
Axiom thm_ARBITRARY_INTERSECTION_OF_EMPTY : forall {A : Type'}, forall P : (A -> Prop) -> Prop, @INTERSECTION_OF A (@ARBITRARY A) P (@UNIV A).
Axiom thm_ARBITRARY_UNION_OF_INC : forall {A : Type'}, forall P : (A -> Prop) -> Prop, forall s : A -> Prop, (P s) -> @UNION_OF A (@ARBITRARY A) P s.
Axiom thm_ARBITRARY_INTERSECTION_OF_INC : forall {A : Type'}, forall P : (A -> Prop) -> Prop, forall s : A -> Prop, (P s) -> @INTERSECTION_OF A (@ARBITRARY A) P s.
Axiom thm_ARBITRARY_UNION_OF_COMPLEMENT : forall {A : Type'}, forall P : (A -> Prop) -> Prop, forall s : A -> Prop, (@UNION_OF A (@ARBITRARY A) P s) = (@INTERSECTION_OF A (@ARBITRARY A) (fun s' : A -> Prop => P (@DIFF A (@UNIV A) s')) (@DIFF A (@UNIV A) s)).
Axiom thm_ARBITRARY_INTERSECTION_OF_COMPLEMENT : forall {A : Type'}, forall P : (A -> Prop) -> Prop, forall s : A -> Prop, (@INTERSECTION_OF A (@ARBITRARY A) P s) = (@UNION_OF A (@ARBITRARY A) (fun s' : A -> Prop => P (@DIFF A (@UNIV A) s')) (@DIFF A (@UNIV A) s)).
Axiom thm_ARBITRARY_UNION_OF_IDEMPOT : forall {A : Type'}, forall P : (A -> Prop) -> Prop, (@UNION_OF A (@ARBITRARY A) (@UNION_OF A (@ARBITRARY A) P)) = (@UNION_OF A (@ARBITRARY A) P).
Axiom thm_ARBITRARY_INTERSECTION_OF_IDEMPOT : forall {A : Type'}, forall P : (A -> Prop) -> Prop, (@INTERSECTION_OF A (@ARBITRARY A) (@INTERSECTION_OF A (@ARBITRARY A) P)) = (@INTERSECTION_OF A (@ARBITRARY A) P).
Axiom thm_ARBITRARY_UNION_OF_UNIONS : forall {A : Type'}, forall P : (A -> Prop) -> Prop, forall u : (A -> Prop) -> Prop, (forall s : A -> Prop, (@IN (A -> Prop) s u) -> @UNION_OF A (@ARBITRARY A) P s) -> @UNION_OF A (@ARBITRARY A) P (@UNIONS A u).
Axiom thm_ARBITRARY_UNION_OF_UNION : forall {A : Type'}, forall P : (A -> Prop) -> Prop, forall s : A -> Prop, forall t : A -> Prop, ((@UNION_OF A (@ARBITRARY A) P s) /\ (@UNION_OF A (@ARBITRARY A) P t)) -> @UNION_OF A (@ARBITRARY A) P (@UNION A s t).
Axiom thm_ARBITRARY_INTERSECTION_OF_INTERS : forall {A : Type'}, forall P : (A -> Prop) -> Prop, forall u : (A -> Prop) -> Prop, (forall s : A -> Prop, (@IN (A -> Prop) s u) -> @INTERSECTION_OF A (@ARBITRARY A) P s) -> @INTERSECTION_OF A (@ARBITRARY A) P (@INTERS A u).
Axiom thm_ARBITRARY_INTERSECTION_OF_INTER : forall {A : Type'}, forall P : (A -> Prop) -> Prop, forall s : A -> Prop, forall t : A -> Prop, ((@INTERSECTION_OF A (@ARBITRARY A) P s) /\ (@INTERSECTION_OF A (@ARBITRARY A) P t)) -> @INTERSECTION_OF A (@ARBITRARY A) P (@INTER A s t).
Axiom thm_ARBITRARY_UNION_OF_INTER_EQ : forall {A : Type'}, forall P : (A -> Prop) -> Prop, (forall s : A -> Prop, forall t : A -> Prop, ((@UNION_OF A (@ARBITRARY A) P s) /\ (@UNION_OF A (@ARBITRARY A) P t)) -> @UNION_OF A (@ARBITRARY A) P (@INTER A s t)) = (forall s : A -> Prop, forall t : A -> Prop, ((P s) /\ (P t)) -> @UNION_OF A (@ARBITRARY A) P (@INTER A s t)).
Axiom thm_ARBITRARY_UNION_OF_INTER : forall {A : Type'}, forall P : (A -> Prop) -> Prop, (forall s : A -> Prop, forall t : A -> Prop, ((P s) /\ (P t)) -> P (@INTER A s t)) -> forall s : A -> Prop, forall t : A -> Prop, ((@UNION_OF A (@ARBITRARY A) P s) /\ (@UNION_OF A (@ARBITRARY A) P t)) -> @UNION_OF A (@ARBITRARY A) P (@INTER A s t).
Axiom thm_ARBITRARY_INTERSECTION_OF_UNION_EQ : forall {A : Type'}, forall P : (A -> Prop) -> Prop, (forall s : A -> Prop, forall t : A -> Prop, ((@INTERSECTION_OF A (@ARBITRARY A) P s) /\ (@INTERSECTION_OF A (@ARBITRARY A) P t)) -> @INTERSECTION_OF A (@ARBITRARY A) P (@UNION A s t)) = (forall s : A -> Prop, forall t : A -> Prop, ((P s) /\ (P t)) -> @INTERSECTION_OF A (@ARBITRARY A) P (@UNION A s t)).
Axiom thm_ARBITRARY_INTERSECTION_OF_UNION : forall {A : Type'}, forall P : (A -> Prop) -> Prop, (forall s : A -> Prop, forall t : A -> Prop, ((P s) /\ (P t)) -> P (@UNION A s t)) -> forall s : A -> Prop, forall t : A -> Prop, ((@INTERSECTION_OF A (@ARBITRARY A) P s) /\ (@INTERSECTION_OF A (@ARBITRARY A) P t)) -> @INTERSECTION_OF A (@ARBITRARY A) P (@UNION A s t).
Axiom thm_FINITE_UNION_OF_EMPTY : forall {A : Type'}, forall P : (A -> Prop) -> Prop, @UNION_OF A (@FINITE (A -> Prop)) P (@EMPTY A).
Axiom thm_FINITE_INTERSECTION_OF_EMPTY : forall {A : Type'}, forall P : (A -> Prop) -> Prop, @INTERSECTION_OF A (@FINITE (A -> Prop)) P (@UNIV A).
Axiom thm_FINITE_UNION_OF_INC : forall {A : Type'}, forall P : (A -> Prop) -> Prop, forall s : A -> Prop, (P s) -> @UNION_OF A (@FINITE (A -> Prop)) P s.
Axiom thm_FINITE_INTERSECTION_OF_INC : forall {A : Type'}, forall P : (A -> Prop) -> Prop, forall s : A -> Prop, (P s) -> @INTERSECTION_OF A (@FINITE (A -> Prop)) P s.
Axiom thm_FINITE_UNION_OF_COMPLEMENT : forall {A : Type'}, forall P : (A -> Prop) -> Prop, forall s : A -> Prop, (@UNION_OF A (@FINITE (A -> Prop)) P s) = (@INTERSECTION_OF A (@FINITE (A -> Prop)) (fun s' : A -> Prop => P (@DIFF A (@UNIV A) s')) (@DIFF A (@UNIV A) s)).
Axiom thm_FINITE_INTERSECTION_OF_COMPLEMENT : forall {A : Type'}, forall P : (A -> Prop) -> Prop, forall s : A -> Prop, (@INTERSECTION_OF A (@FINITE (A -> Prop)) P s) = (@UNION_OF A (@FINITE (A -> Prop)) (fun s' : A -> Prop => P (@DIFF A (@UNIV A) s')) (@DIFF A (@UNIV A) s)).
Axiom thm_FINITE_UNION_OF_IDEMPOT : forall {A : Type'}, forall P : (A -> Prop) -> Prop, (@UNION_OF A (@FINITE (A -> Prop)) (@UNION_OF A (@FINITE (A -> Prop)) P)) = (@UNION_OF A (@FINITE (A -> Prop)) P).
Axiom thm_FINITE_INTERSECTION_OF_IDEMPOT : forall {A : Type'}, forall P : (A -> Prop) -> Prop, (@INTERSECTION_OF A (@FINITE (A -> Prop)) (@INTERSECTION_OF A (@FINITE (A -> Prop)) P)) = (@INTERSECTION_OF A (@FINITE (A -> Prop)) P).
Axiom thm_FINITE_UNION_OF_UNIONS : forall {A : Type'}, forall P : (A -> Prop) -> Prop, forall u : (A -> Prop) -> Prop, ((@FINITE (A -> Prop) u) /\ (forall s : A -> Prop, (@IN (A -> Prop) s u) -> @UNION_OF A (@FINITE (A -> Prop)) P s)) -> @UNION_OF A (@FINITE (A -> Prop)) P (@UNIONS A u).
Axiom thm_FINITE_UNION_OF_UNION : forall {A : Type'}, forall P : (A -> Prop) -> Prop, forall s : A -> Prop, forall t : A -> Prop, ((@UNION_OF A (@FINITE (A -> Prop)) P s) /\ (@UNION_OF A (@FINITE (A -> Prop)) P t)) -> @UNION_OF A (@FINITE (A -> Prop)) P (@UNION A s t).
Axiom thm_FINITE_INTERSECTION_OF_INTERS : forall {A : Type'}, forall P : (A -> Prop) -> Prop, forall u : (A -> Prop) -> Prop, ((@FINITE (A -> Prop) u) /\ (forall s : A -> Prop, (@IN (A -> Prop) s u) -> @INTERSECTION_OF A (@FINITE (A -> Prop)) P s)) -> @INTERSECTION_OF A (@FINITE (A -> Prop)) P (@INTERS A u).
Axiom thm_FINITE_INTERSECTION_OF_INTER : forall {A : Type'}, forall P : (A -> Prop) -> Prop, forall s : A -> Prop, forall t : A -> Prop, ((@INTERSECTION_OF A (@FINITE (A -> Prop)) P s) /\ (@INTERSECTION_OF A (@FINITE (A -> Prop)) P t)) -> @INTERSECTION_OF A (@FINITE (A -> Prop)) P (@INTER A s t).
Axiom thm_FINITE_UNION_OF_INTER_EQ : forall {A : Type'}, forall P : (A -> Prop) -> Prop, (forall s : A -> Prop, forall t : A -> Prop, ((@UNION_OF A (@FINITE (A -> Prop)) P s) /\ (@UNION_OF A (@FINITE (A -> Prop)) P t)) -> @UNION_OF A (@FINITE (A -> Prop)) P (@INTER A s t)) = (forall s : A -> Prop, forall t : A -> Prop, ((P s) /\ (P t)) -> @UNION_OF A (@FINITE (A -> Prop)) P (@INTER A s t)).
Axiom thm_FINITE_UNION_OF_INTER : forall {A : Type'}, forall P : (A -> Prop) -> Prop, (forall s : A -> Prop, forall t : A -> Prop, ((P s) /\ (P t)) -> P (@INTER A s t)) -> forall s : A -> Prop, forall t : A -> Prop, ((@UNION_OF A (@FINITE (A -> Prop)) P s) /\ (@UNION_OF A (@FINITE (A -> Prop)) P t)) -> @UNION_OF A (@FINITE (A -> Prop)) P (@INTER A s t).
Axiom thm_FINITE_INTERSECTION_OF_UNION_EQ : forall {A : Type'}, forall P : (A -> Prop) -> Prop, (forall s : A -> Prop, forall t : A -> Prop, ((@INTERSECTION_OF A (@FINITE (A -> Prop)) P s) /\ (@INTERSECTION_OF A (@FINITE (A -> Prop)) P t)) -> @INTERSECTION_OF A (@FINITE (A -> Prop)) P (@UNION A s t)) = (forall s : A -> Prop, forall t : A -> Prop, ((P s) /\ (P t)) -> @INTERSECTION_OF A (@FINITE (A -> Prop)) P (@UNION A s t)).
Axiom thm_FINITE_INTERSECTION_OF_UNION : forall {A : Type'}, forall P : (A -> Prop) -> Prop, (forall s : A -> Prop, forall t : A -> Prop, ((P s) /\ (P t)) -> P (@UNION A s t)) -> forall s : A -> Prop, forall t : A -> Prop, ((@INTERSECTION_OF A (@FINITE (A -> Prop)) P s) /\ (@INTERSECTION_OF A (@FINITE (A -> Prop)) P t)) -> @INTERSECTION_OF A (@FINITE (A -> Prop)) P (@UNION A s t).
Axiom thm_CARD_SET_OF_LIST_LE : forall {A : Type'}, forall l : list A, Peano.le (@CARD A (@set_of_list A l)) (@List.length A l).
Axiom thm_HAS_SIZE_SET_OF_LIST : forall {A : Type'}, forall l : list A, (@HAS_SIZE A (@set_of_list A l) (@List.length A l)) = (@List.ForallOrdPairs A (fun x : A => fun y : A => ~ (x = y)) l).
Axiom thm_SURJECTIVE_IFF_INJECTIVE_GEN : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, forall f : A -> B, ((@FINITE A s) /\ ((@FINITE B t) /\ (((@CARD A s) = (@CARD B t)) /\ (@SUBSET B (@IMAGE A B f s) t)))) -> (forall y : B, (@IN B y t) -> exists x : A, (@IN A x s) /\ ((f x) = y)) = (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y).
Axiom thm_SURJECTIVE_IFF_INJECTIVE : forall {A : Type'}, forall s : A -> Prop, forall f : A -> A, ((@FINITE A s) /\ (@SUBSET A (@IMAGE A A f s) s)) -> (forall y : A, (@IN A y s) -> exists x : A, (@IN A x s) /\ ((f x) = y)) = (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y).
Axiom thm_IMAGE_IMP_INJECTIVE_GEN : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, forall f : A -> B, ((@FINITE A s) /\ (((@CARD A s) = (@CARD B t)) /\ ((@IMAGE A B f s) = t))) -> forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y.
Axiom thm_IMAGE_IMP_INJECTIVE : forall {A : Type'}, forall s : A -> Prop, forall f : A -> A, ((@FINITE A s) /\ ((@IMAGE A A f s) = s)) -> forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y.
Axiom thm_HAS_SIZE_IMAGE_INJ_RESTRICT : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall t : B -> Prop, forall P : B -> Prop, forall n : nat, ((@FINITE A s) /\ ((@FINITE B t) /\ (((@CARD A s) = (@CARD B t)) /\ ((@SUBSET B (@IMAGE A B f s) t) /\ ((forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y) /\ (@HAS_SIZE A (@GSPEC A (fun GEN_PVAR_219 : A => exists x : A, @SETSPEC A GEN_PVAR_219 ((@IN A x s) /\ (P (f x))) x)) n)))))) -> @HAS_SIZE B (@GSPEC B (fun GEN_PVAR_220 : B => exists x : B, @SETSPEC B GEN_PVAR_220 ((@IN B x t) /\ (P x)) x)) n.
Axiom thm_CARD_LE_INJ : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, ((@FINITE A s) /\ ((@FINITE B t) /\ (Peano.le (@CARD A s) (@CARD B t)))) -> exists f : A -> B, (@SUBSET B (@IMAGE A B f s) t) /\ (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y).
Axiom thm_FORALL_IN_CLAUSES : forall {A : Type'}, (forall P : A -> Prop, (forall x : A, (@IN A x (@EMPTY A)) -> P x) = True) /\ (forall P : A -> Prop, forall a : A, forall s : A -> Prop, (forall x : A, (@IN A x (@INSERT A a s)) -> P x) = ((P a) /\ (forall x : A, (@IN A x s) -> P x))).
Axiom thm_EXISTS_IN_CLAUSES : forall {A : Type'}, (forall P : A -> Prop, (exists x : A, (@IN A x (@EMPTY A)) /\ (P x)) = False) /\ (forall P : A -> Prop, forall a : A, forall s : A -> Prop, (exists x : A, (@IN A x (@INSERT A a s)) /\ (P x)) = ((P a) \/ (exists x : A, (@IN A x s) /\ (P x)))).
Axiom thm_INJECTIVE_ON_IMAGE : forall {A B : Type'}, forall f : A -> B, forall u : A -> Prop, (forall s : A -> Prop, forall t : A -> Prop, ((@SUBSET A s u) /\ ((@SUBSET A t u) /\ ((@IMAGE A B f s) = (@IMAGE A B f t)))) -> s = t) = (forall x : A, forall y : A, ((@IN A x u) /\ ((@IN A y u) /\ ((f x) = (f y)))) -> x = y).
Axiom thm_INJECTIVE_IMAGE : forall {A B : Type'}, forall f : A -> B, (forall s : A -> Prop, forall t : A -> Prop, ((@IMAGE A B f s) = (@IMAGE A B f t)) -> s = t) = (forall x : A, forall y : A, ((f x) = (f y)) -> x = y).
Axiom thm_SURJECTIVE_ON_IMAGE : forall {A B : Type'}, forall f : A -> B, forall u : A -> Prop, forall v : B -> Prop, (forall t : B -> Prop, (@SUBSET B t v) -> exists s : A -> Prop, (@SUBSET A s u) /\ ((@IMAGE A B f s) = t)) = (forall y : B, (@IN B y v) -> exists x : A, (@IN A x u) /\ ((f x) = y)).
Axiom thm_SURJECTIVE_IMAGE : forall {A B : Type'}, forall f : A -> B, (forall t : B -> Prop, exists s : A -> Prop, (@IMAGE A B f s) = t) = (forall y : B, exists x : A, (f x) = y).
Axiom thm_INJECTIVE_ON_PREIMAGE : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall u : B -> Prop, (forall t : B -> Prop, forall t' : B -> Prop, ((@SUBSET B t u) /\ ((@SUBSET B t' u) /\ ((@GSPEC A (fun GEN_PVAR_222 : A => exists x : A, @SETSPEC A GEN_PVAR_222 ((@IN A x s) /\ (@IN B (f x) t)) x)) = (@GSPEC A (fun GEN_PVAR_223 : A => exists x : A, @SETSPEC A GEN_PVAR_223 ((@IN A x s) /\ (@IN B (f x) t')) x))))) -> t = t') = (@SUBSET B u (@IMAGE A B f s)).
Axiom thm_SURJECTIVE_ON_PREIMAGE : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall u : B -> Prop, (forall k : A -> Prop, (@SUBSET A k s) -> exists t : B -> Prop, (@SUBSET B t u) /\ ((@GSPEC A (fun GEN_PVAR_224 : A => exists x : A, @SETSPEC A GEN_PVAR_224 ((@IN A x s) /\ (@IN B (f x) t)) x)) = k)) = ((@SUBSET B (@IMAGE A B f s) u) /\ (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y)).
Axiom thm_INJECTIVE_PREIMAGE : forall {A B : Type'}, forall f : A -> B, (forall t : B -> Prop, forall t' : B -> Prop, ((@GSPEC A (fun GEN_PVAR_225 : A => exists x : A, @SETSPEC A GEN_PVAR_225 (@IN B (f x) t) x)) = (@GSPEC A (fun GEN_PVAR_226 : A => exists x : A, @SETSPEC A GEN_PVAR_226 (@IN B (f x) t') x))) -> t = t') = ((@IMAGE A B f (@UNIV A)) = (@UNIV B)).
Axiom thm_SURJECTIVE_PREIMAGE : forall {A B : Type'}, forall f : A -> B, (forall k : A -> Prop, exists t : B -> Prop, (@GSPEC A (fun GEN_PVAR_227 : A => exists x : A, @SETSPEC A GEN_PVAR_227 (@IN B (f x) t) x)) = k) = (forall x : A, forall y : A, ((f x) = (f y)) -> x = y).
Axiom thm_CARD_EQ_BIJECTION : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, ((@FINITE A s) /\ ((@FINITE B t) /\ ((@CARD A s) = (@CARD B t)))) -> exists f : A -> B, (forall x : A, (@IN A x s) -> @IN B (f x) t) /\ ((forall y : B, (@IN B y t) -> exists x : A, (@IN A x s) /\ ((f x) = y)) /\ (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y)).
Axiom thm_CARD_EQ_BIJECTIONS : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, ((@FINITE A s) /\ ((@FINITE B t) /\ ((@CARD A s) = (@CARD B t)))) -> exists f : A -> B, exists g : B -> A, (forall x : A, (@IN A x s) -> (@IN B (f x) t) /\ ((g (f x)) = x)) /\ (forall y : B, (@IN B y t) -> (@IN A (g y) s) /\ ((f (g y)) = y)).
Axiom thm_CARD_EQ_BIJECTIONS_SPECIAL : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, forall a : A, forall b : B, ((@FINITE A s) /\ ((@FINITE B t) /\ (((@CARD A s) = (@CARD B t)) /\ ((@IN A a s) /\ (@IN B b t))))) -> exists f : A -> B, exists g : B -> A, ((f a) = b) /\ (((g b) = a) /\ ((forall x : A, (@IN A x s) -> (@IN B (f x) t) /\ ((g (f x)) = x)) /\ (forall y : B, (@IN B y t) -> (@IN A (g y) s) /\ ((f (g y)) = y)))).
Axiom thm_BIJECTIONS_HAS_SIZE : forall {A B : Type'} (n : nat), forall s : A -> Prop, forall t : B -> Prop, forall f : A -> B, forall g : B -> A, ((forall x : A, (@IN A x s) -> (@IN B (f x) t) /\ ((g (f x)) = x)) /\ ((forall y : B, (@IN B y t) -> (@IN A (g y) s) /\ ((f (g y)) = y)) /\ (@HAS_SIZE A s n))) -> @HAS_SIZE B t n.
Axiom thm_BIJECTIONS_HAS_SIZE_EQ : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, forall f : A -> B, forall g : B -> A, ((forall x : A, (@IN A x s) -> (@IN B (f x) t) /\ ((g (f x)) = x)) /\ (forall y : B, (@IN B y t) -> (@IN A (g y) s) /\ ((f (g y)) = y))) -> forall n : nat, (@HAS_SIZE A s n) = (@HAS_SIZE B t n).
Axiom thm_BIJECTIONS_CARD_EQ : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, forall f : A -> B, forall g : B -> A, (((@FINITE A s) \/ (@FINITE B t)) /\ ((forall x : A, (@IN A x s) -> (@IN B (f x) t) /\ ((g (f x)) = x)) /\ (forall y : B, (@IN B y t) -> (@IN A (g y) s) /\ ((f (g y)) = y)))) -> (@CARD A s) = (@CARD B t).
Axiom thm_WF_FINITE : forall {A : Type'}, forall lt2 : A -> A -> Prop, ((forall x : A, ~ (lt2 x x)) /\ ((forall x : A, forall y : A, forall z : A, ((lt2 x y) /\ (lt2 y z)) -> lt2 x z) /\ (forall x : A, @FINITE A (@GSPEC A (fun GEN_PVAR_229 : A => exists y : A, @SETSPEC A GEN_PVAR_229 (lt2 y x) y))))) -> @WF A lt2.
Axiom thm_WF_PSUBSET : forall {A : Type'}, forall s : A -> Prop, (@FINITE A s) -> @WF (A -> Prop) (fun t1 : A -> Prop => fun t2 : A -> Prop => (@PSUBSET A t1 t2) /\ (@SUBSET A t2 s)).
Axiom thm_le_c : forall {A B : Type'}, forall t : B -> Prop, forall s : A -> Prop, (@le_c A B s t) = (exists f : A -> B, (forall x : A, (@IN A x s) -> @IN B (f x) t) /\ (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y)).
Axiom thm_lt_c : forall {A B : Type'}, forall t : B -> Prop, forall s : A -> Prop, (@lt_c A B s t) = ((@le_c A B s t) /\ (~ (@le_c B A t s))).
Axiom thm_eq_c : forall {A B : Type'}, forall t : B -> Prop, forall s : A -> Prop, (@eq_c A B s t) = (exists f : A -> B, (forall x : A, (@IN A x s) -> @IN B (f x) t) /\ (forall y : B, (@IN B y t) -> @ex1 A (fun x : A => (@IN A x s) /\ ((f x) = y)))).
Axiom thm_ge_c : forall {A B : Type'}, forall t : B -> Prop, forall s : A -> Prop, (@ge_c A B s t) = (@le_c B A t s).
Axiom thm_gt_c : forall {A B : Type'}, forall t : B -> Prop, forall s : A -> Prop, (@gt_c A B s t) = (@lt_c B A t s).
Axiom thm_LE_C : forall {A B : Type'}, forall s : B -> Prop, forall t : A -> Prop, (@le_c B A s t) = (exists g : A -> B, forall x : B, (@IN B x s) -> exists y : A, (@IN A y t) /\ ((g y) = x)).
Axiom thm_GE_C : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, (@ge_c A B s t) = (exists f : A -> B, forall y : B, (@IN B y t) -> exists x : A, (@IN A x s) /\ (y = (f x))).
Axiom thm_COUNTABLE : forall {A : Type'}, forall t : A -> Prop, (@COUNTABLE A t) = (@ge_c nat A (@UNIV nat) t).
Axiom thm_sup : forall s : R -> Prop, (sup s) = (@ε R (fun a : R => (forall x : R, (@IN R x s) -> Rle x a) /\ (forall b : R, (forall x : R, (@IN R x s) -> Rle x b) -> Rle a b))).
Axiom thm_SUP_EQ : forall s : R -> Prop, forall t : R -> Prop, (forall b : R, (forall x : R, (@IN R x s) -> Rle x b) = (forall x : R, (@IN R x t) -> Rle x b)) -> (sup s) = (sup t).
Axiom thm_SUP : forall s : R -> Prop, ((~ (s = (@EMPTY R))) /\ (exists b : R, forall x : R, (@IN R x s) -> Rle x b)) -> (forall x : R, (@IN R x s) -> Rle x (sup s)) /\ (forall b : R, (forall x : R, (@IN R x s) -> Rle x b) -> Rle (sup s) b).
Axiom thm_SUP_FINITE_LEMMA : forall s : R -> Prop, ((@FINITE R s) /\ (~ (s = (@EMPTY R)))) -> exists b : R, (@IN R b s) /\ (forall x : R, (@IN R x s) -> Rle x b).
Axiom thm_SUP_FINITE : forall s : R -> Prop, ((@FINITE R s) /\ (~ (s = (@EMPTY R)))) -> (@IN R (sup s) s) /\ (forall x : R, (@IN R x s) -> Rle x (sup s)).
Axiom thm_REAL_LE_SUP_FINITE : forall s : R -> Prop, forall a : R, ((@FINITE R s) /\ (~ (s = (@EMPTY R)))) -> (Rle a (sup s)) = (exists x : R, (@IN R x s) /\ (Rle a x)).
Axiom thm_REAL_SUP_LE_FINITE : forall s : R -> Prop, forall a : R, ((@FINITE R s) /\ (~ (s = (@EMPTY R)))) -> (Rle (sup s) a) = (forall x : R, (@IN R x s) -> Rle x a).
Axiom thm_REAL_LT_SUP_FINITE : forall s : R -> Prop, forall a : R, ((@FINITE R s) /\ (~ (s = (@EMPTY R)))) -> (Rlt a (sup s)) = (exists x : R, (@IN R x s) /\ (Rlt a x)).
Axiom thm_REAL_SUP_LT_FINITE : forall s : R -> Prop, forall a : R, ((@FINITE R s) /\ (~ (s = (@EMPTY R)))) -> (Rlt (sup s) a) = (forall x : R, (@IN R x s) -> Rlt x a).
Axiom thm_REAL_SUP_UNIQUE : forall s : R -> Prop, forall b : R, ((forall x : R, (@IN R x s) -> Rle x b) /\ (forall b' : R, (Rlt b' b) -> exists x : R, (@IN R x s) /\ (Rlt b' x))) -> (sup s) = b.
Axiom thm_REAL_SUP_LE : forall (s : R -> Prop), forall b : R, ((~ (s = (@EMPTY R))) /\ (forall x : R, (@IN R x s) -> Rle x b)) -> Rle (sup s) b.
Axiom thm_REAL_SUP_LE_SUBSET : forall s : R -> Prop, forall t : R -> Prop, ((~ (s = (@EMPTY R))) /\ ((@SUBSET R s t) /\ (exists b : R, forall x : R, (@IN R x t) -> Rle x b))) -> Rle (sup s) (sup t).
Axiom thm_REAL_SUP_BOUNDS : forall s : R -> Prop, forall a : R, forall b : R, ((~ (s = (@EMPTY R))) /\ (forall x : R, (@IN R x s) -> (Rle a x) /\ (Rle x b))) -> (Rle a (sup s)) /\ (Rle (sup s) b).
Axiom thm_REAL_ABS_SUP_LE : forall s : R -> Prop, forall a : R, ((~ (s = (@EMPTY R))) /\ (forall x : R, (@IN R x s) -> Rle (Rabs x) a)) -> Rle (Rabs (sup s)) a.
Axiom thm_REAL_SUP_ASCLOSE : forall s : R -> Prop, forall l : R, forall e : R, ((~ (s = (@EMPTY R))) /\ (forall x : R, (@IN R x s) -> Rle (Rabs (Rminus x l)) e)) -> Rle (Rabs (Rminus (sup s) l)) e.
Axiom thm_SUP_UNIQUE_FINITE : forall (a : R), forall s : R -> Prop, ((@FINITE R s) /\ (~ (s = (@EMPTY R)))) -> ((sup s) = a) = ((@IN R a s) /\ (forall y : R, (@IN R y s) -> Rle y a)).
Axiom thm_SUP_INSERT_FINITE : forall x : R, forall s : R -> Prop, (@FINITE R s) -> (sup (@INSERT R x s)) = (@COND R (s = (@EMPTY R)) x (Rmax x (sup s))).
Axiom thm_SUP_SING : forall a : R, (sup (@INSERT R a (@EMPTY R))) = a.
Axiom thm_SUP_INSERT_INSERT : forall a : R, forall b : R, forall s : R -> Prop, (sup (@INSERT R b (@INSERT R a s))) = (sup (@INSERT R (Rmax a b) s)).
Axiom thm_REAL_LE_SUP : forall s : R -> Prop, forall a : R, forall b : R, forall y : R, ((@IN R y s) /\ ((Rle a y) /\ (forall x : R, (@IN R x s) -> Rle x b))) -> Rle a (sup s).
Axiom thm_REAL_SUP_LE_EQ : forall s : R -> Prop, forall y : R, ((~ (s = (@EMPTY R))) /\ (exists b : R, forall x : R, (@IN R x s) -> Rle x b)) -> (Rle (sup s) y) = (forall x : R, (@IN R x s) -> Rle x y).
Axiom thm_SUP_UNIQUE : forall s : R -> Prop, forall b : R, (forall c : R, (forall x : R, (@IN R x s) -> Rle x c) = (Rle b c)) -> (sup s) = b.
Axiom thm_SUP_UNION : forall s : R -> Prop, forall t : R -> Prop, ((~ (s = (@EMPTY R))) /\ ((~ (t = (@EMPTY R))) /\ ((exists b : R, forall x : R, (@IN R x s) -> Rle x b) /\ (exists c : R, forall x : R, (@IN R x t) -> Rle x c)))) -> (sup (@UNION R s t)) = (Rmax (sup s) (sup t)).
Axiom thm_ELEMENT_LE_SUP : forall s : R -> Prop, forall a : R, ((exists b : R, forall x : R, (@IN R x s) -> Rle x b) /\ (@IN R a s)) -> Rle a (sup s).
Axiom thm_SUP_APPROACH : forall s : R -> Prop, forall c : R, ((~ (s = (@EMPTY R))) /\ ((exists b : R, forall x : R, (@IN R x s) -> Rle x b) /\ (Rlt c (sup s)))) -> exists x : R, (@IN R x s) /\ (Rlt c x).
Axiom thm_REAL_MAX_SUP : forall x : R, forall y : R, (Rmax x y) = (sup (@INSERT R x (@INSERT R y (@EMPTY R)))).
Axiom thm_inf : forall s : R -> Prop, (inf s) = (@ε R (fun a : R => (forall x : R, (@IN R x s) -> Rle a x) /\ (forall b : R, (forall x : R, (@IN R x s) -> Rle b x) -> Rle b a))).
Axiom thm_INF_EQ : forall s : R -> Prop, forall t : R -> Prop, (forall a : R, (forall x : R, (@IN R x s) -> Rle a x) = (forall x : R, (@IN R x t) -> Rle a x)) -> (inf s) = (inf t).
Axiom thm_INF : forall s : R -> Prop, ((~ (s = (@EMPTY R))) /\ (exists b : R, forall x : R, (@IN R x s) -> Rle b x)) -> (forall x : R, (@IN R x s) -> Rle (inf s) x) /\ (forall b : R, (forall x : R, (@IN R x s) -> Rle b x) -> Rle b (inf s)).
Axiom thm_INF_FINITE_LEMMA : forall s : R -> Prop, ((@FINITE R s) /\ (~ (s = (@EMPTY R)))) -> exists b : R, (@IN R b s) /\ (forall x : R, (@IN R x s) -> Rle b x).
Axiom thm_INF_FINITE : forall s : R -> Prop, ((@FINITE R s) /\ (~ (s = (@EMPTY R)))) -> (@IN R (inf s) s) /\ (forall x : R, (@IN R x s) -> Rle (inf s) x).
Axiom thm_REAL_LE_INF_FINITE : forall s : R -> Prop, forall a : R, ((@FINITE R s) /\ (~ (s = (@EMPTY R)))) -> (Rle a (inf s)) = (forall x : R, (@IN R x s) -> Rle a x).
Axiom thm_REAL_INF_LE_FINITE : forall s : R -> Prop, forall a : R, ((@FINITE R s) /\ (~ (s = (@EMPTY R)))) -> (Rle (inf s) a) = (exists x : R, (@IN R x s) /\ (Rle x a)).
Axiom thm_REAL_LT_INF_FINITE : forall s : R -> Prop, forall a : R, ((@FINITE R s) /\ (~ (s = (@EMPTY R)))) -> (Rlt a (inf s)) = (forall x : R, (@IN R x s) -> Rlt a x).
Axiom thm_REAL_INF_LT_FINITE : forall s : R -> Prop, forall a : R, ((@FINITE R s) /\ (~ (s = (@EMPTY R)))) -> (Rlt (inf s) a) = (exists x : R, (@IN R x s) /\ (Rlt x a)).
Axiom thm_REAL_INF_UNIQUE : forall s : R -> Prop, forall b : R, ((forall x : R, (@IN R x s) -> Rle b x) /\ (forall b' : R, (Rlt b b') -> exists x : R, (@IN R x s) /\ (Rlt x b'))) -> (inf s) = b.
Axiom thm_REAL_LE_INF : forall (s : R -> Prop), forall b : R, ((~ (s = (@EMPTY R))) /\ (forall x : R, (@IN R x s) -> Rle b x)) -> Rle b (inf s).
Axiom thm_REAL_LE_INF_SUBSET : forall s : R -> Prop, forall t : R -> Prop, ((~ (t = (@EMPTY R))) /\ ((@SUBSET R t s) /\ (exists b : R, forall x : R, (@IN R x s) -> Rle b x))) -> Rle (inf s) (inf t).
Axiom thm_REAL_INF_BOUNDS : forall s : R -> Prop, forall a : R, forall b : R, ((~ (s = (@EMPTY R))) /\ (forall x : R, (@IN R x s) -> (Rle a x) /\ (Rle x b))) -> (Rle a (inf s)) /\ (Rle (inf s) b).
Axiom thm_REAL_ABS_INF_LE : forall s : R -> Prop, forall a : R, ((~ (s = (@EMPTY R))) /\ (forall x : R, (@IN R x s) -> Rle (Rabs x) a)) -> Rle (Rabs (inf s)) a.
Axiom thm_REAL_INF_ASCLOSE : forall s : R -> Prop, forall l : R, forall e : R, ((~ (s = (@EMPTY R))) /\ (forall x : R, (@IN R x s) -> Rle (Rabs (Rminus x l)) e)) -> Rle (Rabs (Rminus (inf s) l)) e.
Axiom thm_INF_UNIQUE_FINITE : forall (a : R), forall s : R -> Prop, ((@FINITE R s) /\ (~ (s = (@EMPTY R)))) -> ((inf s) = a) = ((@IN R a s) /\ (forall y : R, (@IN R y s) -> Rle a y)).
Axiom thm_INF_INSERT_FINITE : forall x : R, forall s : R -> Prop, (@FINITE R s) -> (inf (@INSERT R x s)) = (@COND R (s = (@EMPTY R)) x (Rmin x (inf s))).
Axiom thm_INF_SING : forall a : R, (inf (@INSERT R a (@EMPTY R))) = a.
Axiom thm_INF_INSERT_INSERT : forall a : R, forall b : R, forall s : R -> Prop, (inf (@INSERT R b (@INSERT R a s))) = (inf (@INSERT R (Rmin a b) s)).
Axiom thm_REAL_SUP_EQ_INF : forall s : R -> Prop, ((~ (s = (@EMPTY R))) /\ (exists B : R, forall x : R, (@IN R x s) -> Rle (Rabs x) B)) -> ((sup s) = (inf s)) = (exists a : R, s = (@INSERT R a (@EMPTY R))).
Axiom thm_REAL_INF_LE : forall s : R -> Prop, forall a : R, forall b : R, forall y : R, ((@IN R y s) /\ ((Rle y b) /\ (forall x : R, (@IN R x s) -> Rle a x))) -> Rle (inf s) b.
Axiom thm_REAL_LE_INF_EQ : forall s : R -> Prop, forall y : R, ((~ (s = (@EMPTY R))) /\ (exists b : R, forall x : R, (@IN R x s) -> Rle b x)) -> (Rle y (inf s)) = (forall x : R, (@IN R x s) -> Rle y x).
Axiom thm_INF_UNIQUE : forall s : R -> Prop, forall b : R, (forall c : R, (forall x : R, (@IN R x s) -> Rle c x) = (Rle c b)) -> (inf s) = b.
Axiom thm_INF_UNION : forall s : R -> Prop, forall t : R -> Prop, ((~ (s = (@EMPTY R))) /\ ((~ (t = (@EMPTY R))) /\ ((exists b : R, forall x : R, (@IN R x s) -> Rle b x) /\ (exists c : R, forall x : R, (@IN R x t) -> Rle c x)))) -> (inf (@UNION R s t)) = (Rmin (inf s) (inf t)).
Axiom thm_INF_LE_ELEMENT : forall s : R -> Prop, forall a : R, ((exists b : R, forall x : R, (@IN R x s) -> Rle b x) /\ (@IN R a s)) -> Rle (inf s) a.
Axiom thm_INF_APPROACH : forall s : R -> Prop, forall c : R, ((~ (s = (@EMPTY R))) /\ ((exists b : R, forall x : R, (@IN R x s) -> Rle b x) /\ (Rlt (inf s) c))) -> exists x : R, (@IN R x s) /\ (Rlt x c).
Axiom thm_REAL_MIN_INF : forall x : R, forall y : R, (Rmin x y) = (inf (@INSERT R x (@INSERT R y (@EMPTY R)))).
Axiom thm_has_inf : forall s : R -> Prop, forall b : R, (has_inf s b) = (forall c : R, (forall x : R, (@IN R x s) -> Rle c x) = (Rle c b)).
Axiom thm_has_sup : forall s : R -> Prop, forall b : R, (has_sup s b) = (forall c : R, (forall x : R, (@IN R x s) -> Rle x c) = (Rle b c)).
Axiom thm_HAS_INF_LBOUND : forall s : R -> Prop, forall b : R, forall x : R, ((has_inf s b) /\ (@IN R x s)) -> Rle b x.
Axiom thm_HAS_SUP_UBOUND : forall s : R -> Prop, forall b : R, forall x : R, ((has_sup s b) /\ (@IN R x s)) -> Rle x b.
Axiom thm_HAS_INF_INF : forall s : R -> Prop, forall l : R, (has_inf s l) = ((~ (s = (@EMPTY R))) /\ ((exists b : R, forall x : R, (@IN R x s) -> Rle b x) /\ ((inf s) = l))).
Axiom thm_HAS_SUP_SUP : forall s : R -> Prop, forall l : R, (has_sup s l) = ((~ (s = (@EMPTY R))) /\ ((exists b : R, forall x : R, (@IN R x s) -> Rle x b) /\ ((sup s) = l))).
Axiom thm_INF_EXISTS : forall s : R -> Prop, (exists l : R, has_inf s l) = ((~ (s = (@EMPTY R))) /\ (exists b : R, forall x : R, (@IN R x s) -> Rle b x)).
Axiom thm_SUP_EXISTS : forall s : R -> Prop, (exists l : R, has_sup s l) = ((~ (s = (@EMPTY R))) /\ (exists b : R, forall x : R, (@IN R x s) -> Rle x b)).
Axiom thm_HAS_INF_APPROACH : forall s : R -> Prop, forall l : R, forall c : R, ((has_inf s l) /\ (Rlt l c)) -> exists x : R, (@IN R x s) /\ (Rlt x c).
Axiom thm_HAS_SUP_APPROACH : forall s : R -> Prop, forall l : R, forall c : R, ((has_sup s l) /\ (Rlt c l)) -> exists x : R, (@IN R x s) /\ (Rlt c x).
Axiom thm_HAS_INF : forall s : R -> Prop, forall l : R, (has_inf s l) = ((~ (s = (@EMPTY R))) /\ ((forall x : R, (@IN R x s) -> Rle l x) /\ (forall c : R, (Rlt l c) -> exists x : R, (@IN R x s) /\ (Rlt x c)))).
Axiom thm_HAS_SUP : forall s : R -> Prop, forall l : R, (has_sup s l) = ((~ (s = (@EMPTY R))) /\ ((forall x : R, (@IN R x s) -> Rle x l) /\ (forall c : R, (Rlt c l) -> exists x : R, (@IN R x s) /\ (Rlt c x)))).
Axiom thm_HAS_INF_LE : forall s : R -> Prop, forall t : R -> Prop, forall l : R, forall m : R, ((has_inf s l) /\ ((has_inf t m) /\ (forall y : R, (@IN R y t) -> exists x : R, (@IN R x s) /\ (Rle x y)))) -> Rle l m.
Axiom thm_HAS_SUP_LE : forall s : R -> Prop, forall t : R -> Prop, forall l : R, forall m : R, ((has_sup s l) /\ ((has_sup t m) /\ (forall y : R, (@IN R y t) -> exists x : R, (@IN R x s) /\ (Rle y x)))) -> Rle m l.
Axiom thm_numseg : forall m : nat, forall n : nat, (dotdot m n) = (@GSPEC nat (fun GEN_PVAR_231 : nat => exists x : nat, @SETSPEC nat GEN_PVAR_231 ((Peano.le m x) /\ (Peano.le x n)) x)).
Axiom thm_FINITE_NUMSEG : forall m : nat, forall n : nat, @FINITE nat (dotdot m n).
Axiom thm_NUMSEG_COMBINE_R : forall m : nat, forall p : nat, forall n : nat, ((Peano.le m (Nat.add p (NUMERAL (BIT1 0)))) /\ (Peano.le p n)) -> (@UNION nat (dotdot m p) (dotdot (Nat.add p (NUMERAL (BIT1 0))) n)) = (dotdot m n).
Axiom thm_NUMSEG_COMBINE_L : forall m : nat, forall p : nat, forall n : nat, ((Peano.le m p) /\ (Peano.le p (Nat.add n (NUMERAL (BIT1 0))))) -> (@UNION nat (dotdot m (Nat.sub p (NUMERAL (BIT1 0)))) (dotdot p n)) = (dotdot m n).
Axiom thm_NUMSEG_LREC : forall m : nat, forall n : nat, (Peano.le m n) -> (@INSERT nat m (dotdot (Nat.add m (NUMERAL (BIT1 0))) n)) = (dotdot m n).
Axiom thm_NUMSEG_RREC : forall m : nat, forall n : nat, (Peano.le m n) -> (@INSERT nat n (dotdot m (Nat.sub n (NUMERAL (BIT1 0))))) = (dotdot m n).
Axiom thm_NUMSEG_REC : forall m : nat, forall n : nat, (Peano.le m (S n)) -> (dotdot m (S n)) = (@INSERT nat (S n) (dotdot m n)).
Axiom thm_IN_NUMSEG : forall m : nat, forall n : nat, forall p : nat, (@IN nat p (dotdot m n)) = ((Peano.le m p) /\ (Peano.le p n)).
Axiom thm_IN_NUMSEG_0 : forall m : nat, forall n : nat, (@IN nat m (dotdot (NUMERAL 0) n)) = (Peano.le m n).
Axiom thm_NUMSEG_SING : forall n : nat, (dotdot n n) = (@INSERT nat n (@EMPTY nat)).
Axiom thm_NUMSEG_EMPTY : forall m : nat, forall n : nat, ((dotdot m n) = (@EMPTY nat)) = (Peano.lt n m).
Axiom thm_EMPTY_NUMSEG : forall m : nat, forall n : nat, (Peano.lt n m) -> (dotdot m n) = (@EMPTY nat).
Axiom thm_FINITE_SUBSET_NUMSEG : forall s : nat -> Prop, (@FINITE nat s) = (exists n : nat, @SUBSET nat s (dotdot (NUMERAL 0) n)).
Axiom thm_CARD_NUMSEG_LEMMA : forall m : nat, forall d : nat, (@CARD nat (dotdot m (Nat.add m d))) = (Nat.add d (NUMERAL (BIT1 0))).
Axiom thm_CARD_NUMSEG : forall m : nat, forall n : nat, (@CARD nat (dotdot m n)) = (Nat.sub (Nat.add n (NUMERAL (BIT1 0))) m).
Axiom thm_HAS_SIZE_NUMSEG : forall m : nat, forall n : nat, @HAS_SIZE nat (dotdot m n) (Nat.sub (Nat.add n (NUMERAL (BIT1 0))) m).
Axiom thm_CARD_NUMSEG_1 : forall n : nat, (@CARD nat (dotdot (NUMERAL (BIT1 0)) n)) = n.
Axiom thm_HAS_SIZE_NUMSEG_1 : forall n : nat, @HAS_SIZE nat (dotdot (NUMERAL (BIT1 0)) n) n.
Axiom thm_NUMSEG_CLAUSES : (forall m : nat, (dotdot m (NUMERAL 0)) = (@COND (nat -> Prop) (m = (NUMERAL 0)) (@INSERT nat (NUMERAL 0) (@EMPTY nat)) (@EMPTY nat))) /\ (forall m : nat, forall n : nat, (dotdot m (S n)) = (@COND (nat -> Prop) (Peano.le m (S n)) (@INSERT nat (S n) (dotdot m n)) (dotdot m n))).
Axiom thm_FINITE_INDEX_NUMSEG : forall {A : Type'}, forall s : A -> Prop, (@FINITE A s) = (exists f : nat -> A, (forall i : nat, forall j : nat, ((@IN nat i (dotdot (NUMERAL (BIT1 0)) (@CARD A s))) /\ ((@IN nat j (dotdot (NUMERAL (BIT1 0)) (@CARD A s))) /\ ((f i) = (f j)))) -> i = j) /\ (s = (@IMAGE nat A f (dotdot (NUMERAL (BIT1 0)) (@CARD A s))))).
Axiom thm_FINITE_INDEX_NUMBERS : forall {A : Type'}, forall s : A -> Prop, (@FINITE A s) = (exists k : nat -> Prop, exists f : nat -> A, (forall i : nat, forall j : nat, ((@IN nat i k) /\ ((@IN nat j k) /\ ((f i) = (f j)))) -> i = j) /\ ((@FINITE nat k) /\ (s = (@IMAGE nat A f k)))).
Axiom thm_INTER_NUMSEG : forall m : nat, forall n : nat, forall p : nat, forall q : nat, (@INTER nat (dotdot m n) (dotdot p q)) = (dotdot (Nat.max m p) (Nat.min n q)).
Axiom thm_DISJOINT_NUMSEG : forall m : nat, forall n : nat, forall p : nat, forall q : nat, (@DISJOINT nat (dotdot m n) (dotdot p q)) = ((Peano.lt n p) \/ ((Peano.lt q m) \/ ((Peano.lt n m) \/ (Peano.lt q p)))).
Axiom thm_NUMSEG_ADD_SPLIT : forall m : nat, forall n : nat, forall p : nat, (Peano.le m (Nat.add n (NUMERAL (BIT1 0)))) -> (dotdot m (Nat.add n p)) = (@UNION nat (dotdot m n) (dotdot (Nat.add n (NUMERAL (BIT1 0))) (Nat.add n p))).
Axiom thm_NUMSEG_OFFSET_IMAGE : forall m : nat, forall n : nat, forall p : nat, (dotdot (Nat.add m p) (Nat.add n p)) = (@IMAGE nat nat (fun i : nat => Nat.add i p) (dotdot m n)).
Axiom thm_SUBSET_NUMSEG : forall m : nat, forall n : nat, forall p : nat, forall q : nat, (@SUBSET nat (dotdot m n) (dotdot p q)) = ((Peano.lt n m) \/ ((Peano.le p m) /\ (Peano.le n q))).
Axiom thm_NUMSEG_LE : forall n : nat, (@GSPEC nat (fun GEN_PVAR_233 : nat => exists x : nat, @SETSPEC nat GEN_PVAR_233 (Peano.le x n) x)) = (dotdot (NUMERAL 0) n).
Axiom thm_NUMSEG_LT : forall n : nat, (@GSPEC nat (fun GEN_PVAR_234 : nat => exists x : nat, @SETSPEC nat GEN_PVAR_234 (Peano.lt x n) x)) = (@COND (nat -> Prop) (n = (NUMERAL 0)) (@EMPTY nat) (dotdot (NUMERAL 0) (Nat.sub n (NUMERAL (BIT1 0))))).
Axiom thm_TOPOLOGICAL_SORT : forall {A : Type'}, forall lt2 : A -> A -> Prop, ((forall x : A, forall y : A, ((lt2 x y) /\ (lt2 y x)) -> x = y) /\ (forall x : A, forall y : A, forall z : A, ((lt2 x y) /\ (lt2 y z)) -> lt2 x z)) -> forall n : nat, forall s : A -> Prop, (@HAS_SIZE A s n) -> exists f : nat -> A, (s = (@IMAGE nat A f (dotdot (NUMERAL (BIT1 0)) n))) /\ (forall j : nat, forall k : nat, ((@IN nat j (dotdot (NUMERAL (BIT1 0)) n)) /\ ((@IN nat k (dotdot (NUMERAL (BIT1 0)) n)) /\ (Peano.lt j k))) -> ~ (lt2 (f k) (f j))).
Axiom thm_FINITE_INT_SEG : (forall l : Z, forall r : Z, @FINITE Z (@GSPEC Z (fun GEN_PVAR_235 : Z => exists x : Z, @SETSPEC Z GEN_PVAR_235 ((int_le l x) /\ (int_le x r)) x))) /\ ((forall l : Z, forall r : Z, @FINITE Z (@GSPEC Z (fun GEN_PVAR_236 : Z => exists x : Z, @SETSPEC Z GEN_PVAR_236 ((int_le l x) /\ (int_lt x r)) x))) /\ ((forall l : Z, forall r : Z, @FINITE Z (@GSPEC Z (fun GEN_PVAR_237 : Z => exists x : Z, @SETSPEC Z GEN_PVAR_237 ((int_lt l x) /\ (int_le x r)) x))) /\ (forall l : Z, forall r : Z, @FINITE Z (@GSPEC Z (fun GEN_PVAR_238 : Z => exists x : Z, @SETSPEC Z GEN_PVAR_238 ((int_lt l x) /\ (int_lt x r)) x))))).
Axiom thm_neutral : forall {A : Type'}, forall op : A -> A -> A, (@neutral A op) = (@ε A (fun x : A => forall y : A, ((op x y) = y) /\ ((op y x) = y))).
Axiom thm_monoidal : forall {A : Type'}, forall op : A -> A -> A, (@monoidal A op) = ((forall x : A, forall y : A, (op x y) = (op y x)) /\ ((forall x : A, forall y : A, forall z : A, (op x (op y z)) = (op (op x y) z)) /\ (forall x : A, (op (@neutral A op) x) = x))).
Axiom thm_MONOIDAL_AC : forall {A : Type'}, forall op : A -> A -> A, (@monoidal A op) -> (forall a : A, (op (@neutral A op) a) = a) /\ ((forall a : A, (op a (@neutral A op)) = a) /\ ((forall a : A, forall b : A, (op a b) = (op b a)) /\ ((forall a : A, forall b : A, forall c : A, (op (op a b) c) = (op a (op b c))) /\ (forall a : A, forall b : A, forall c : A, (op a (op b c)) = (op b (op a c)))))).
Axiom thm_support : forall {A B : Type'}, forall s : A -> Prop, forall f : A -> B, forall op : B -> B -> B, (@support A B op f s) = (@GSPEC A (fun GEN_PVAR_239 : A => exists x : A, @SETSPEC A GEN_PVAR_239 ((@IN A x s) /\ (~ ((f x) = (@neutral B op)))) x)).
Axiom thm_iterate : forall {A B : Type'}, forall f : A -> B, forall s : A -> Prop, forall op : B -> B -> B, (@iterate A B op s f) = (@COND B (@FINITE A (@support A B op f s)) (@ITSET A B (fun x : A => fun a : B => op (f x) a) (@support A B op f s) (@neutral B op)) (@neutral B op)).
Axiom thm_IN_SUPPORT : forall {A B : Type'}, forall op : B -> B -> B, forall f : A -> B, forall x : A, forall s : A -> Prop, (@IN A x (@support A B op f s)) = ((@IN A x s) /\ (~ ((f x) = (@neutral B op)))).
Axiom thm_SUPPORT_SUPPORT : forall {A B : Type'}, forall op : B -> B -> B, forall f : A -> B, forall s : A -> Prop, (@support A B op f (@support A B op f s)) = (@support A B op f s).
Axiom thm_SUPPORT_EMPTY : forall {A B : Type'}, forall op : B -> B -> B, forall f : A -> B, forall s : A -> Prop, (forall x : A, (@IN A x s) -> (f x) = (@neutral B op)) = ((@support A B op f s) = (@EMPTY A)).
Axiom thm_SUPPORT_SUBSET : forall {A B : Type'}, forall op : B -> B -> B, forall f : A -> B, forall s : A -> Prop, @SUBSET A (@support A B op f s) s.
Axiom thm_FINITE_SUPPORT : forall {A B : Type'}, forall op : B -> B -> B, forall f : A -> B, forall s : A -> Prop, (@FINITE A s) -> @FINITE A (@support A B op f s).
Axiom thm_SUPPORT_CLAUSES : forall {A B C : Type'} (op : C -> C -> C), (forall f : A -> C, (@support A C op f (@EMPTY A)) = (@EMPTY A)) /\ ((forall f : A -> C, forall x : A, forall s : A -> Prop, (@support A C op f (@INSERT A x s)) = (@COND (A -> Prop) ((f x) = (@neutral C op)) (@support A C op f s) (@INSERT A x (@support A C op f s)))) /\ ((forall f : A -> C, forall x : A, forall s : A -> Prop, (@support A C op f (@DELETE A s x)) = (@DELETE A (@support A C op f s) x)) /\ ((forall f : A -> C, forall s : A -> Prop, forall t : A -> Prop, (@support A C op f (@UNION A s t)) = (@UNION A (@support A C op f s) (@support A C op f t))) /\ ((forall f : A -> C, forall s : A -> Prop, forall t : A -> Prop, (@support A C op f (@INTER A s t)) = (@INTER A (@support A C op f s) (@support A C op f t))) /\ ((forall f : A -> C, forall s : A -> Prop, forall t : A -> Prop, (@support A C op f (@DIFF A s t)) = (@DIFF A (@support A C op f s) (@support A C op f t))) /\ (forall f : A -> B, forall g : B -> C, forall s : A -> Prop, (@support B C op g (@IMAGE A B f s)) = (@IMAGE A B f (@support A C op (@o A B C g f) s)))))))).
Axiom thm_SUPPORT_DELTA : forall {A B : Type'}, forall op : B -> B -> B, forall s : A -> Prop, forall f : A -> B, forall a : A, (@support A B op (fun x : A => @COND B (x = a) (f x) (@neutral B op)) s) = (@COND (A -> Prop) (@IN A a s) (@support A B op f (@INSERT A a (@EMPTY A))) (@EMPTY A)).
Axiom thm_FINITE_SUPPORT_DELTA : forall {A B : Type'} (s : A -> Prop), forall op : B -> B -> B, forall f : A -> B, forall a : A, @FINITE A (@support A B op (fun x : A => @COND B (x = a) (f x) (@neutral B op)) s).
Axiom thm_ITERATE_SUPPORT : forall {A B : Type'}, forall op : B -> B -> B, forall f : A -> B, forall s : A -> Prop, (@iterate A B op (@support A B op f s) f) = (@iterate A B op s f).
Axiom thm_ITERATE_EXPAND_CASES : forall {A B : Type'}, forall op : B -> B -> B, forall f : A -> B, forall s : A -> Prop, (@iterate A B op s f) = (@COND B (@FINITE A (@support A B op f s)) (@iterate A B op (@support A B op f s) f) (@neutral B op)).
Axiom thm_ITERATE_CLAUSES_GEN : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> (forall f : A -> B, (@iterate A B op (@EMPTY A) f) = (@neutral B op)) /\ (forall f : A -> B, forall x : A, forall s : A -> Prop, (@FINITE A (@support A B op f s)) -> (@iterate A B op (@INSERT A x s) f) = (@COND B (@IN A x s) (@iterate A B op s f) (op (f x) (@iterate A B op s f)))).
Axiom thm_ITERATE_CLAUSES : forall {A B C : Type'}, forall op : C -> C -> C, (@monoidal C op) -> (forall f : A -> C, (@iterate A C op (@EMPTY A) f) = (@neutral C op)) /\ (forall f : B -> C, forall x : B, forall s : B -> Prop, (@FINITE B s) -> (@iterate B C op (@INSERT B x s) f) = (@COND C (@IN B x s) (@iterate B C op s f) (op (f x) (@iterate B C op s f)))).
Axiom thm_ITERATE_UNION : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall f : A -> B, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A s) /\ ((@FINITE A t) /\ (@DISJOINT A s t))) -> (@iterate A B op (@UNION A s t) f) = (op (@iterate A B op s f) (@iterate A B op t f)).
Axiom thm_ITERATE_UNION_GEN : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall f : A -> B, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A (@support A B op f s)) /\ ((@FINITE A (@support A B op f t)) /\ (@DISJOINT A (@support A B op f s) (@support A B op f t)))) -> (@iterate A B op (@UNION A s t) f) = (op (@iterate A B op s f) (@iterate A B op t f)).
Axiom thm_ITERATE_DIFF : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall f : A -> B, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A s) /\ (@SUBSET A t s)) -> (op (@iterate A B op (@DIFF A s t) f) (@iterate A B op t f)) = (@iterate A B op s f).
Axiom thm_ITERATE_DIFF_GEN : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall f : A -> B, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A (@support A B op f s)) /\ (@SUBSET A (@support A B op f t) (@support A B op f s))) -> (op (@iterate A B op (@DIFF A s t) f) (@iterate A B op t f)) = (@iterate A B op s f).
Axiom thm_ITERATE_INCL_EXCL : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall s : A -> Prop, forall t : A -> Prop, forall f : A -> B, ((@FINITE A s) /\ (@FINITE A t)) -> (op (@iterate A B op s f) (@iterate A B op t f)) = (op (@iterate A B op (@UNION A s t) f) (@iterate A B op (@INTER A s t) f)).
Axiom thm_ITERATE_CLOSED : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall P : B -> Prop, ((P (@neutral B op)) /\ (forall x : B, forall y : B, ((P x) /\ (P y)) -> P (op x y))) -> forall f : A -> B, forall s : A -> Prop, (forall x : A, ((@IN A x s) /\ (~ ((f x) = (@neutral B op)))) -> P (f x)) -> P (@iterate A B op s f).
Axiom thm_ITERATE_RELATED : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall R' : B -> B -> Prop, ((R' (@neutral B op) (@neutral B op)) /\ (forall x1 : B, forall y1 : B, forall x2 : B, forall y2 : B, ((R' x1 x2) /\ (R' y1 y2)) -> R' (op x1 y1) (op x2 y2))) -> forall f : A -> B, forall g : A -> B, forall s : A -> Prop, ((@FINITE A s) /\ (forall x : A, (@IN A x s) -> R' (f x) (g x))) -> R' (@iterate A B op s f) (@iterate A B op s g).
Axiom thm_ITERATE_EQ_NEUTRAL : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall f : A -> B, forall s : A -> Prop, (forall x : A, (@IN A x s) -> (f x) = (@neutral B op)) -> (@iterate A B op s f) = (@neutral B op).
Axiom thm_ITERATE_SING : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall f : A -> B, forall x : A, (@iterate A B op (@INSERT A x (@EMPTY A)) f) = (f x).
Axiom thm_ITERATE_CLOSED_NONEMPTY : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall P : B -> Prop, (forall x : B, forall y : B, ((P x) /\ (P y)) -> P (op x y)) -> forall f : A -> B, forall s : A -> Prop, ((@FINITE A s) /\ ((~ (s = (@EMPTY A))) /\ (forall x : A, (@IN A x s) -> P (f x)))) -> P (@iterate A B op s f).
Axiom thm_ITERATE_RELATED_NONEMPTY : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall R' : B -> B -> Prop, (forall x1 : B, forall y1 : B, forall x2 : B, forall y2 : B, ((R' x1 x2) /\ (R' y1 y2)) -> R' (op x1 y1) (op x2 y2)) -> forall f : A -> B, forall g : A -> B, forall s : A -> Prop, ((@FINITE A s) /\ ((~ (s = (@EMPTY A))) /\ (forall x : A, (@IN A x s) -> R' (f x) (g x)))) -> R' (@iterate A B op s f) (@iterate A B op s g).
Axiom thm_ITERATE_DELETE : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall f : A -> B, forall s : A -> Prop, forall a : A, ((@FINITE A s) /\ (@IN A a s)) -> (op (f a) (@iterate A B op (@DELETE A s a) f)) = (@iterate A B op s f).
Axiom thm_ITERATE_DELTA : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall f : A -> B, forall a : A, forall s : A -> Prop, (@iterate A B op s (fun x : A => @COND B (x = a) (f x) (@neutral B op))) = (@COND B (@IN A a s) (f a) (@neutral B op)).
Axiom thm_ITERATE_IMAGE : forall {A B C : Type'}, forall op : C -> C -> C, (@monoidal C op) -> forall f : A -> B, forall g : B -> C, forall s : A -> Prop, (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y) -> (@iterate B C op (@IMAGE A B f s) g) = (@iterate A C op s (@o A B C g f)).
Axiom thm_ITERATE_BIJECTION : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall f : A -> B, forall p : A -> A, forall s : A -> Prop, ((forall x : A, (@IN A x s) -> @IN A (p x) s) /\ (forall y : A, (@IN A y s) -> @ex1 A (fun x : A => (@IN A x s) /\ ((p x) = y)))) -> (@iterate A B op s f) = (@iterate A B op s (@o A A B f p)).
Axiom thm_ITERATE_ITERATE_PRODUCT : forall {A B C : Type'}, forall op : C -> C -> C, (@monoidal C op) -> forall s : A -> Prop, forall t : A -> B -> Prop, forall x : A -> B -> C, ((@FINITE A s) /\ (forall i : A, (@IN A i s) -> @FINITE B (t i))) -> (@iterate A C op s (fun i : A => @iterate B C op (t i) (x i))) = (@iterate (prod A B) C op (@GSPEC (prod A B) (fun GEN_PVAR_243 : prod A B => exists i : A, exists j : B, @SETSPEC (prod A B) GEN_PVAR_243 ((@IN A i s) /\ (@IN B j (t i))) (@pair A B i j))) (@GABS ((prod A B) -> C) (fun f : (prod A B) -> C => forall i : A, forall j : B, @GEQ C (f (@pair A B i j)) (x i j)))).
Axiom thm_ITERATE_EQ : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall f : A -> B, forall g : A -> B, forall s : A -> Prop, (forall x : A, (@IN A x s) -> (f x) = (g x)) -> (@iterate A B op s f) = (@iterate A B op s g).
Axiom thm_ITERATE_RESTRICT_SET : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall P : A -> Prop, forall s : A -> Prop, forall f : A -> B, (@iterate A B op (@GSPEC A (fun GEN_PVAR_244 : A => exists x : A, @SETSPEC A GEN_PVAR_244 ((@IN A x s) /\ (P x)) x)) f) = (@iterate A B op s (fun x : A => @COND B (P x) (f x) (@neutral B op))).
Axiom thm_ITERATE_EQ_GENERAL : forall {A B C : Type'}, forall op : C -> C -> C, (@monoidal C op) -> forall s : A -> Prop, forall t : B -> Prop, forall f : A -> C, forall g : B -> C, forall h : A -> B, ((forall y : B, (@IN B y t) -> @ex1 A (fun x : A => (@IN A x s) /\ ((h x) = y))) /\ (forall x : A, (@IN A x s) -> (@IN B (h x) t) /\ ((g (h x)) = (f x)))) -> (@iterate A C op s f) = (@iterate B C op t g).
Axiom thm_ITERATE_EQ_GENERAL_INVERSES : forall {A B C : Type'}, forall op : C -> C -> C, (@monoidal C op) -> forall s : A -> Prop, forall t : B -> Prop, forall f : A -> C, forall g : B -> C, forall h : A -> B, forall k : B -> A, ((forall y : B, (@IN B y t) -> (@IN A (k y) s) /\ ((h (k y)) = y)) /\ (forall x : A, (@IN A x s) -> (@IN B (h x) t) /\ (((k (h x)) = x) /\ ((g (h x)) = (f x))))) -> (@iterate A C op s f) = (@iterate B C op t g).
Axiom thm_ITERATE_INJECTION : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall f : A -> B, forall p : A -> A, forall s : A -> Prop, ((@FINITE A s) /\ ((forall x : A, (@IN A x s) -> @IN A (p x) s) /\ (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((p x) = (p y)))) -> x = y))) -> (@iterate A B op s (@o A A B f p)) = (@iterate A B op s f).
Axiom thm_ITERATE_UNION_NONZERO : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall f : A -> B, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A s) /\ ((@FINITE A t) /\ (forall x : A, (@IN A x (@INTER A s t)) -> (f x) = (@neutral B op)))) -> (@iterate A B op (@UNION A s t) f) = (op (@iterate A B op s f) (@iterate A B op t f)).
Axiom thm_ITERATE_OP : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall f : A -> B, forall g : A -> B, forall s : A -> Prop, (@FINITE A s) -> (@iterate A B op s (fun x : A => op (f x) (g x))) = (op (@iterate A B op s f) (@iterate A B op s g)).
Axiom thm_ITERATE_SUPERSET : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall f : A -> B, forall u : A -> Prop, forall v : A -> Prop, ((@SUBSET A u v) /\ (forall x : A, ((@IN A x v) /\ (~ (@IN A x u))) -> (f x) = (@neutral B op))) -> (@iterate A B op v f) = (@iterate A B op u f).
Axiom thm_ITERATE_UNIV : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall f : A -> B, forall s : A -> Prop, (@SUBSET A (@support A B op f (@UNIV A)) s) -> (@iterate A B op s f) = (@iterate A B op (@UNIV A) f).
Axiom thm_ITERATE_SWAP : forall {A B C : Type'}, forall op : C -> C -> C, (@monoidal C op) -> forall f : A -> B -> C, forall s : A -> Prop, forall t : B -> Prop, ((@FINITE A s) /\ (@FINITE B t)) -> (@iterate A C op s (fun i : A => @iterate B C op t (f i))) = (@iterate B C op t (fun j : B => @iterate A C op s (fun i : A => f i j))).
Axiom thm_ITERATE_IMAGE_NONZERO : forall {A B C : Type'}, forall op : C -> C -> C, (@monoidal C op) -> forall g : B -> C, forall f : A -> B, forall s : A -> Prop, ((@FINITE A s) /\ (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((~ (x = y)) /\ ((f x) = (f y))))) -> (g (f x)) = (@neutral C op))) -> (@iterate B C op (@IMAGE A B f s) g) = (@iterate A C op s (@o A B C g f)).
Axiom thm_ITERATE_IMAGE_GEN : forall {A B C : Type'}, forall op : C -> C -> C, (@monoidal C op) -> forall f : A -> B, forall g : A -> C, forall s : A -> Prop, (@FINITE A s) -> (@iterate A C op s g) = (@iterate B C op (@IMAGE A B f s) (fun y : B => @iterate A C op (@GSPEC A (fun GEN_PVAR_247 : A => exists x : A, @SETSPEC A GEN_PVAR_247 ((@IN A x s) /\ ((f x) = y)) x)) g)).
Axiom thm_ITERATE_CASES : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall s : A -> Prop, forall P : A -> Prop, forall f : A -> B, forall g : A -> B, (@FINITE A s) -> (@iterate A B op s (fun x : A => @COND B (P x) (f x) (g x))) = (op (@iterate A B op (@GSPEC A (fun GEN_PVAR_250 : A => exists x : A, @SETSPEC A GEN_PVAR_250 ((@IN A x s) /\ (P x)) x)) f) (@iterate A B op (@GSPEC A (fun GEN_PVAR_251 : A => exists x : A, @SETSPEC A GEN_PVAR_251 ((@IN A x s) /\ (~ (P x))) x)) g)).
Axiom thm_ITERATE_OP_GEN : forall {A B : Type'}, forall op : B -> B -> B, (@monoidal B op) -> forall f : A -> B, forall g : A -> B, forall s : A -> Prop, ((@FINITE A (@support A B op f s)) /\ (@FINITE A (@support A B op g s))) -> (@iterate A B op s (fun x : A => op (f x) (g x))) = (op (@iterate A B op s f) (@iterate A B op s g)).
Axiom thm_ITERATE_CLAUSES_NUMSEG : forall {A : Type'} (f : nat -> A), forall op : A -> A -> A, (@monoidal A op) -> (forall m : nat, (@iterate nat A op (dotdot m (NUMERAL 0)) f) = (@COND A (m = (NUMERAL 0)) (f (NUMERAL 0)) (@neutral A op))) /\ (forall m : nat, forall n : nat, (@iterate nat A op (dotdot m (S n)) f) = (@COND A (Peano.le m (S n)) (op (@iterate nat A op (dotdot m n) f) (f (S n))) (@iterate nat A op (dotdot m n) f))).
Axiom thm_ITERATE_CLAUSES_NUMSEG_LT : forall {A : Type'} (f : nat -> A), forall op : A -> A -> A, (@monoidal A op) -> ((@iterate nat A op (@GSPEC nat (fun GEN_PVAR_256 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_256 (Peano.lt i (NUMERAL 0)) i)) f) = (@neutral A op)) /\ (forall k : nat, (@iterate nat A op (@GSPEC nat (fun GEN_PVAR_257 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_257 (Peano.lt i (S k)) i)) f) = (op (@iterate nat A op (@GSPEC nat (fun GEN_PVAR_258 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_258 (Peano.lt i k) i)) f) (f k))).
Axiom thm_ITERATE_CLAUSES_NUMSEG_LE : forall {A : Type'} (f : nat -> A), forall op : A -> A -> A, (@monoidal A op) -> ((@iterate nat A op (@GSPEC nat (fun GEN_PVAR_259 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_259 (Peano.le i (NUMERAL 0)) i)) f) = (f (NUMERAL 0))) /\ (forall k : nat, (@iterate nat A op (@GSPEC nat (fun GEN_PVAR_260 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_260 (Peano.le i (S k)) i)) f) = (op (@iterate nat A op (@GSPEC nat (fun GEN_PVAR_261 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_261 (Peano.le i k) i)) f) (f (S k)))).
Axiom thm_ITERATE_PAIR : forall {A : Type'}, forall op : A -> A -> A, (@monoidal A op) -> forall f : nat -> A, forall m : nat, forall n : nat, (@iterate nat A op (dotdot (Nat.mul (NUMERAL (BIT0 (BIT1 0))) m) (Nat.add (Nat.mul (NUMERAL (BIT0 (BIT1 0))) n) (NUMERAL (BIT1 0)))) f) = (@iterate nat A op (dotdot m n) (fun i : nat => op (f (Nat.mul (NUMERAL (BIT0 (BIT1 0))) i)) (f (Nat.add (Nat.mul (NUMERAL (BIT0 (BIT1 0))) i) (NUMERAL (BIT1 0)))))).
Axiom thm_ITERATE_REFLECT : forall {A : Type'}, forall op : A -> A -> A, (@monoidal A op) -> forall x : nat -> A, forall m : nat, forall n : nat, (@iterate nat A op (dotdot m n) x) = (@COND A (Peano.lt n m) (@neutral A op) (@iterate nat A op (dotdot (NUMERAL 0) (Nat.sub n m)) (fun i : nat => x (Nat.sub n i)))).
Axiom thm_ITERATO_SUPPORT : forall {A K : Type'}, forall dom : A -> Prop, forall neut : A, forall op : A -> A -> A, forall ltle : K -> K -> Prop, forall k : K -> Prop, forall f : K -> A, (@iterato A K dom neut op ltle (@GSPEC K (fun GEN_PVAR_270 : K => exists i : K, @SETSPEC K GEN_PVAR_270 ((@IN K i k) /\ (@IN A (f i) (@DIFF A dom (@INSERT A neut (@EMPTY A))))) i)) f) = (@iterato A K dom neut op ltle k f).
Axiom thm_ITERATO_EXPAND_CASES : forall {A K : Type'}, forall dom : A -> Prop, forall neut : A, forall op : A -> A -> A, forall ltle : K -> K -> Prop, forall k : K -> Prop, forall f : K -> A, (@iterato A K dom neut op ltle k f) = (@COND A (@FINITE K (@GSPEC K (fun GEN_PVAR_271 : K => exists i : K, @SETSPEC K GEN_PVAR_271 ((@IN K i k) /\ (@IN A (f i) (@DIFF A dom (@INSERT A neut (@EMPTY A))))) i))) (@iterato A K dom neut op ltle (@GSPEC K (fun GEN_PVAR_272 : K => exists i : K, @SETSPEC K GEN_PVAR_272 ((@IN K i k) /\ (@IN A (f i) (@DIFF A dom (@INSERT A neut (@EMPTY A))))) i)) f) neut).
Axiom thm_ITERATO_CLAUSES_GEN : forall {A K : Type'}, forall dom : A -> Prop, forall neut : A, forall op : A -> A -> A, forall ltle : K -> K -> Prop, forall f : K -> A, ((@iterato A K dom neut op ltle (@EMPTY K) f) = neut) /\ (forall i : K, forall k : K -> Prop, ((@FINITE K (@GSPEC K (fun GEN_PVAR_274 : K => exists j : K, @SETSPEC K GEN_PVAR_274 ((@IN K j k) /\ (@IN A (f j) (@DIFF A dom (@INSERT A neut (@EMPTY A))))) j))) /\ ((forall j : K, (@IN K j k) -> (i = j) \/ ((ltle i j) \/ (ltle j i))) /\ (forall j : K, ((ltle j i) /\ ((@IN K j k) /\ (@IN A (f j) (@DIFF A dom (@INSERT A neut (@EMPTY A)))))) -> j = i))) -> (@iterato A K dom neut op ltle (@INSERT K i k) f) = (@COND A ((@IN A (f i) dom) -> ((f i) = neut) \/ (@IN K i k)) (@iterato A K dom neut op ltle k f) (op (f i) (@iterato A K dom neut op ltle k f)))).
Axiom thm_ITERATO_CLAUSES : forall {A K : Type'}, forall dom : A -> Prop, forall neut : A, forall op : A -> A -> A, forall ltle : K -> K -> Prop, forall f : K -> A, ((@iterato A K dom neut op ltle (@EMPTY K) f) = neut) /\ (forall i : K, forall k : K -> Prop, ((@FINITE K (@GSPEC K (fun GEN_PVAR_275 : K => exists i' : K, @SETSPEC K GEN_PVAR_275 ((@IN K i' k) /\ (@IN A (f i') (@DIFF A dom (@INSERT A neut (@EMPTY A))))) i'))) /\ (forall j : K, (@IN K j k) -> (ltle i j) /\ (~ (ltle j i)))) -> (@iterato A K dom neut op ltle (@INSERT K i k) f) = (@COND A ((@IN A (f i) dom) -> ((f i) = neut) \/ (@IN K i k)) (@iterato A K dom neut op ltle k f) (op (f i) (@iterato A K dom neut op ltle k f)))).
Axiom thm_ITERATO_CLAUSES_EXISTS : forall {A K : Type'}, forall dom : A -> Prop, forall neut : A, forall op : A -> A -> A, forall ltle : K -> K -> Prop, forall f : K -> A, ((@iterato A K dom neut op ltle (@EMPTY K) f) = neut) /\ (forall k : K -> Prop, ((@FINITE K (@GSPEC K (fun GEN_PVAR_276 : K => exists i : K, @SETSPEC K GEN_PVAR_276 ((@IN K i k) /\ (@IN A (f i) (@DIFF A dom (@INSERT A neut (@EMPTY A))))) i))) /\ (~ ((@GSPEC K (fun GEN_PVAR_277 : K => exists i : K, @SETSPEC K GEN_PVAR_277 ((@IN K i k) /\ (@IN A (f i) (@DIFF A dom (@INSERT A neut (@EMPTY A))))) i)) = (@EMPTY K)))) -> exists i : K, (@IN K i k) /\ ((@IN A (f i) (@DIFF A dom (@INSERT A neut (@EMPTY A)))) /\ ((@iterato A K dom neut op ltle k f) = (op (f i) (@iterato A K dom neut op ltle (@DELETE K k i) f))))).
Axiom thm_ITERATO_EQ : forall {A K : Type'}, forall dom : A -> Prop, forall neut : A, forall op : A -> A -> A, forall ltle : K -> K -> Prop, forall k : K -> Prop, forall f : K -> A, forall g : K -> A, (forall i : K, (@IN K i k) -> (f i) = (g i)) -> (@iterato A K dom neut op ltle k f) = (@iterato A K dom neut op ltle k g).
Axiom thm_ITERATO_INDUCT : forall {A K : Type'}, forall dom : A -> Prop, forall neut : A, forall op : A -> A -> A, forall ltle : K -> K -> Prop, forall k : K -> Prop, forall f : K -> A, forall P : A -> Prop, ((P neut) /\ (forall i : K, forall x : A, ((@IN K i k) /\ ((@IN A (f i) dom) /\ ((~ ((f i) = neut)) /\ (P x)))) -> P (op (f i) x))) -> P (@iterato A K dom neut op ltle k f).
Axiom thm_ITERATO_CLOSED : forall {A K : Type'}, forall dom : A -> Prop, forall neut : A, forall op : A -> A -> A, forall ltle : K -> K -> Prop, forall k : K -> Prop, forall f : K -> A, forall P : A -> Prop, ((P neut) /\ ((forall x : A, forall y : A, ((P x) /\ (P y)) -> P (op x y)) /\ (forall i : K, ((@IN K i k) /\ ((@IN A (f i) dom) /\ (~ ((f i) = neut)))) -> P (f i)))) -> P (@iterato A K dom neut op ltle k f).
Axiom thm_ITERATO_ITERATE : forall {A K : Type'}, forall op : A -> A -> A, forall ltle : K -> K -> Prop, (@monoidal A op) -> (@iterato A K (@UNIV A) (@neutral A op) op ltle) = (@iterate K A op).
Axiom thm_ITERATO_CLAUSES_NUMSEG_LEFT : forall {A : Type'}, forall dom : A -> Prop, forall neut : A, forall op : A -> A -> A, forall f : nat -> A, forall m : nat, forall n : nat, (@iterato A nat dom neut op Peano.le (dotdot m n) f) = (@COND A (Peano.le m n) (@COND A ((@IN A (f m) dom) -> (f m) = neut) (@iterato A nat dom neut op Peano.le (dotdot (Nat.add m (NUMERAL (BIT1 0))) n) f) (op (f m) (@iterato A nat dom neut op Peano.le (dotdot (Nat.add m (NUMERAL (BIT1 0))) n) f))) neut).
Axiom thm_nproduct : forall {A : Type'}, (@nproduct A) = (@iterate A nat Nat.mul).
Axiom thm_NEUTRAL_MUL : (@neutral nat Nat.mul) = (NUMERAL (BIT1 0)).
Axiom thm_MONOIDAL_MUL : @monoidal nat Nat.mul.
Axiom thm_NPRODUCT_CLAUSES : forall {A B : Type'}, (forall f : A -> nat, (@nproduct A (@EMPTY A) f) = (NUMERAL (BIT1 0))) /\ (forall x : B, forall f : B -> nat, forall s : B -> Prop, (@FINITE B s) -> (@nproduct B (@INSERT B x s) f) = (@COND nat (@IN B x s) (@nproduct B s f) (Nat.mul (f x) (@nproduct B s f)))).
Axiom thm_iproduct : forall {A : Type'}, (@iproduct A) = (@iterate A Z int_mul).
Axiom thm_NEUTRAL_INT_MUL : (@neutral Z int_mul) = (int_of_num (NUMERAL (BIT1 0))).
Axiom thm_MONOIDAL_INT_MUL : @monoidal Z int_mul.
Axiom thm_IPRODUCT_CLAUSES : forall {A B : Type'}, (forall f : A -> Z, (@iproduct A (@EMPTY A) f) = (int_of_num (NUMERAL (BIT1 0)))) /\ (forall x : B, forall f : B -> Z, forall s : B -> Prop, (@FINITE B s) -> (@iproduct B (@INSERT B x s) f) = (@COND Z (@IN B x s) (@iproduct B s f) (int_mul (f x) (@iproduct B s f)))).
Axiom thm_product : forall {A : Type'}, (@product A) = (@iterate A R Rmult).
Axiom thm_NEUTRAL_REAL_MUL : (@neutral R Rmult) = (INR (NUMERAL (BIT1 0))).
Axiom thm_MONOIDAL_REAL_MUL : @monoidal R Rmult.
Axiom thm_PRODUCT_CLAUSES : forall {A B : Type'}, (forall f : A -> R, (@product A (@EMPTY A) f) = (INR (NUMERAL (BIT1 0)))) /\ (forall x : B, forall f : B -> R, forall s : B -> Prop, (@FINITE B s) -> (@product B (@INSERT B x s) f) = (@COND R (@IN B x s) (@product B s f) (Rmult (f x) (@product B s f)))).
Axiom thm_isum : forall {A : Type'}, (@isum A) = (@iterate A Z int_add).
Axiom thm_NEUTRAL_INT_ADD : (@neutral Z int_add) = (int_of_num (NUMERAL 0)).
Axiom thm_MONOIDAL_INT_ADD : @monoidal Z int_add.
Axiom thm_ISUM_CLAUSES : forall {A B : Type'}, (forall f : A -> Z, (@isum A (@EMPTY A) f) = (int_of_num (NUMERAL 0))) /\ (forall x : B, forall f : B -> Z, forall s : B -> Prop, (@FINITE B s) -> (@isum B (@INSERT B x s) f) = (@COND Z (@IN B x s) (@isum B s f) (int_add (f x) (@isum B s f)))).
Axiom thm_nsum : forall {A : Type'}, (@nsum A) = (@iterate A nat Nat.add).
Axiom thm_NEUTRAL_ADD : (@neutral nat Nat.add) = (NUMERAL 0).
Axiom thm_MONOIDAL_ADD : @monoidal nat Nat.add.
Axiom thm_NSUM_DEGENERATE : forall {A : Type'}, forall f : A -> nat, forall s : A -> Prop, (~ (@FINITE A (@GSPEC A (fun GEN_PVAR_286 : A => exists x : A, @SETSPEC A GEN_PVAR_286 ((@IN A x s) /\ (~ ((f x) = (NUMERAL 0)))) x)))) -> (@nsum A s f) = (NUMERAL 0).
Axiom thm_NSUM_CLAUSES : forall {A B : Type'}, (forall f : A -> nat, (@nsum A (@EMPTY A) f) = (NUMERAL 0)) /\ (forall x : B, forall f : B -> nat, forall s : B -> Prop, (@FINITE B s) -> (@nsum B (@INSERT B x s) f) = (@COND nat (@IN B x s) (@nsum B s f) (Nat.add (f x) (@nsum B s f)))).
Axiom thm_NSUM_UNION : forall {A : Type'}, forall f : A -> nat, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A s) /\ ((@FINITE A t) /\ (@DISJOINT A s t))) -> (@nsum A (@UNION A s t) f) = (Nat.add (@nsum A s f) (@nsum A t f)).
Axiom thm_NSUM_DIFF : forall {A : Type'}, forall f : A -> nat, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A s) /\ (@SUBSET A t s)) -> (@nsum A (@DIFF A s t) f) = (Nat.sub (@nsum A s f) (@nsum A t f)).
Axiom thm_NSUM_INCL_EXCL : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall f : A -> nat, ((@FINITE A s) /\ (@FINITE A t)) -> (Nat.add (@nsum A s f) (@nsum A t f)) = (Nat.add (@nsum A (@UNION A s t) f) (@nsum A (@INTER A s t) f)).
Axiom thm_NSUM_SUPPORT : forall {A : Type'}, forall f : A -> nat, forall s : A -> Prop, (@nsum A (@support A nat Nat.add f s) f) = (@nsum A s f).
Axiom thm_NSUM_ADD : forall {A : Type'}, forall f : A -> nat, forall g : A -> nat, forall s : A -> Prop, (@FINITE A s) -> (@nsum A s (fun x : A => Nat.add (f x) (g x))) = (Nat.add (@nsum A s f) (@nsum A s g)).
Axiom thm_NSUM_ADD_GEN : forall {A : Type'}, forall f : A -> nat, forall g : A -> nat, forall s : A -> Prop, ((@FINITE A (@GSPEC A (fun GEN_PVAR_287 : A => exists x : A, @SETSPEC A GEN_PVAR_287 ((@IN A x s) /\ (~ ((f x) = (NUMERAL 0)))) x))) /\ (@FINITE A (@GSPEC A (fun GEN_PVAR_288 : A => exists x : A, @SETSPEC A GEN_PVAR_288 ((@IN A x s) /\ (~ ((g x) = (NUMERAL 0)))) x)))) -> (@nsum A s (fun x : A => Nat.add (f x) (g x))) = (Nat.add (@nsum A s f) (@nsum A s g)).
Axiom thm_NSUM_EQ_0 : forall {A : Type'}, forall f : A -> nat, forall s : A -> Prop, (forall x : A, (@IN A x s) -> (f x) = (NUMERAL 0)) -> (@nsum A s f) = (NUMERAL 0).
Axiom thm_NSUM_0 : forall {A : Type'}, forall s : A -> Prop, (@nsum A s (fun n : A => NUMERAL 0)) = (NUMERAL 0).
Axiom thm_NSUM_LMUL : forall {A : Type'}, forall f : A -> nat, forall c : nat, forall s : A -> Prop, (@nsum A s (fun x : A => Nat.mul c (f x))) = (Nat.mul c (@nsum A s f)).
Axiom thm_NSUM_RMUL : forall {A : Type'}, forall f : A -> nat, forall c : nat, forall s : A -> Prop, (@nsum A s (fun x : A => Nat.mul (f x) c)) = (Nat.mul (@nsum A s f) c).
Axiom thm_NSUM_LE : forall {A : Type'}, forall f : A -> nat, forall g : A -> nat, forall s : A -> Prop, ((@FINITE A s) /\ (forall x : A, (@IN A x s) -> Peano.le (f x) (g x))) -> Peano.le (@nsum A s f) (@nsum A s g).
Axiom thm_NSUM_LT : forall {A : Type'}, forall f : A -> nat, forall g : A -> nat, forall s : A -> Prop, ((@FINITE A s) /\ ((forall x : A, (@IN A x s) -> Peano.le (f x) (g x)) /\ (exists x : A, (@IN A x s) /\ (Peano.lt (f x) (g x))))) -> Peano.lt (@nsum A s f) (@nsum A s g).
Axiom thm_NSUM_LT_ALL : forall {A : Type'}, forall f : A -> nat, forall g : A -> nat, forall s : A -> Prop, ((@FINITE A s) /\ ((~ (s = (@EMPTY A))) /\ (forall x : A, (@IN A x s) -> Peano.lt (f x) (g x)))) -> Peano.lt (@nsum A s f) (@nsum A s g).
Axiom thm_NSUM_EQ : forall {A : Type'}, forall f : A -> nat, forall g : A -> nat, forall s : A -> Prop, (forall x : A, (@IN A x s) -> (f x) = (g x)) -> (@nsum A s f) = (@nsum A s g).
Axiom thm_NSUM_CONST : forall {A : Type'}, forall c : nat, forall s : A -> Prop, (@FINITE A s) -> (@nsum A s (fun n : A => c)) = (Nat.mul (@CARD A s) c).
Axiom thm_NSUM_POS_BOUND : forall {A : Type'}, forall f : A -> nat, forall b : nat, forall s : A -> Prop, ((@FINITE A s) /\ (Peano.le (@nsum A s f) b)) -> forall x : A, (@IN A x s) -> Peano.le (f x) b.
Axiom thm_NSUM_EQ_0_IFF : forall {A : Type'} (f : A -> nat), forall s : A -> Prop, (@FINITE A s) -> ((@nsum A s f) = (NUMERAL 0)) = (forall x : A, (@IN A x s) -> (f x) = (NUMERAL 0)).
Axiom thm_NSUM_POS_LT : forall {A : Type'}, forall f : A -> nat, forall s : A -> Prop, ((@FINITE A s) /\ (exists x : A, (@IN A x s) /\ (Peano.lt (NUMERAL 0) (f x)))) -> Peano.lt (NUMERAL 0) (@nsum A s f).
Axiom thm_NSUM_POS_LT_ALL : forall {A : Type'}, forall s : A -> Prop, forall f : A -> nat, ((@FINITE A s) /\ ((~ (s = (@EMPTY A))) /\ (forall i : A, (@IN A i s) -> Peano.lt (NUMERAL 0) (f i)))) -> Peano.lt (NUMERAL 0) (@nsum A s f).
Axiom thm_NSUM_DELETE : forall {A : Type'}, forall f : A -> nat, forall s : A -> Prop, forall a : A, ((@FINITE A s) /\ (@IN A a s)) -> (Nat.add (f a) (@nsum A (@DELETE A s a) f)) = (@nsum A s f).
Axiom thm_NSUM_SING : forall {A : Type'}, forall f : A -> nat, forall x : A, (@nsum A (@INSERT A x (@EMPTY A)) f) = (f x).
Axiom thm_NSUM_DELTA : forall {A : Type'} (b : nat), forall s : A -> Prop, forall a : A, (@nsum A s (fun x : A => @COND nat (x = a) b (NUMERAL 0))) = (@COND nat (@IN A a s) b (NUMERAL 0)).
Axiom thm_NSUM_SWAP : forall {A B : Type'}, forall f : A -> B -> nat, forall s : A -> Prop, forall t : B -> Prop, ((@FINITE A s) /\ (@FINITE B t)) -> (@nsum A s (fun i : A => @nsum B t (f i))) = (@nsum B t (fun j : B => @nsum A s (fun i : A => f i j))).
Axiom thm_NSUM_IMAGE : forall {A B : Type'}, forall f : A -> B, forall g : B -> nat, forall s : A -> Prop, (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y) -> (@nsum B (@IMAGE A B f s) g) = (@nsum A s (@o A B nat g f)).
Axiom thm_NSUM_SUPERSET : forall {A : Type'}, forall f : A -> nat, forall u : A -> Prop, forall v : A -> Prop, ((@SUBSET A u v) /\ (forall x : A, ((@IN A x v) /\ (~ (@IN A x u))) -> (f x) = (NUMERAL 0))) -> (@nsum A v f) = (@nsum A u f).
Axiom thm_NSUM_UNIV : forall {A : Type'}, forall f : A -> nat, forall s : A -> Prop, (@SUBSET A (@support A nat Nat.add f (@UNIV A)) s) -> (@nsum A s f) = (@nsum A (@UNIV A) f).
Axiom thm_NSUM_UNION_RZERO : forall {A : Type'}, forall f : A -> nat, forall u : A -> Prop, forall v : A -> Prop, ((@FINITE A u) /\ (forall x : A, ((@IN A x v) /\ (~ (@IN A x u))) -> (f x) = (NUMERAL 0))) -> (@nsum A (@UNION A u v) f) = (@nsum A u f).
Axiom thm_NSUM_UNION_LZERO : forall {A : Type'}, forall f : A -> nat, forall u : A -> Prop, forall v : A -> Prop, ((@FINITE A v) /\ (forall x : A, ((@IN A x u) /\ (~ (@IN A x v))) -> (f x) = (NUMERAL 0))) -> (@nsum A (@UNION A u v) f) = (@nsum A v f).
Axiom thm_NSUM_RESTRICT : forall {A : Type'}, forall f : A -> nat, forall s : A -> Prop, (@FINITE A s) -> (@nsum A s (fun x : A => @COND nat (@IN A x s) (f x) (NUMERAL 0))) = (@nsum A s f).
Axiom thm_NSUM_BOUND : forall {A : Type'}, forall s : A -> Prop, forall f : A -> nat, forall b : nat, ((@FINITE A s) /\ (forall x : A, (@IN A x s) -> Peano.le (f x) b)) -> Peano.le (@nsum A s f) (Nat.mul (@CARD A s) b).
Axiom thm_NSUM_BOUND_GEN : forall {A : Type'}, forall s : A -> Prop, forall f : A -> nat, forall b : nat, ((@FINITE A s) /\ ((~ (s = (@EMPTY A))) /\ (forall x : A, (@IN A x s) -> Peano.le (f x) (Nat.div b (@CARD A s))))) -> Peano.le (@nsum A s f) b.
Axiom thm_NSUM_BOUND_LT : forall {A : Type'}, forall s : A -> Prop, forall f : A -> nat, forall b : nat, ((@FINITE A s) /\ ((forall x : A, (@IN A x s) -> Peano.le (f x) b) /\ (exists x : A, (@IN A x s) /\ (Peano.lt (f x) b)))) -> Peano.lt (@nsum A s f) (Nat.mul (@CARD A s) b).
Axiom thm_NSUM_BOUND_LT_ALL : forall {A : Type'}, forall s : A -> Prop, forall f : A -> nat, forall b : nat, ((@FINITE A s) /\ ((~ (s = (@EMPTY A))) /\ (forall x : A, (@IN A x s) -> Peano.lt (f x) b))) -> Peano.lt (@nsum A s f) (Nat.mul (@CARD A s) b).
Axiom thm_NSUM_BOUND_LT_GEN : forall {A : Type'}, forall s : A -> Prop, forall f : A -> nat, forall b : nat, ((@FINITE A s) /\ ((~ (s = (@EMPTY A))) /\ (forall x : A, (@IN A x s) -> Peano.lt (f x) (Nat.div b (@CARD A s))))) -> Peano.lt (@nsum A s f) b.
Axiom thm_NSUM_UNION_EQ : forall {A : Type'} (f : A -> nat), forall s : A -> Prop, forall t : A -> Prop, forall u : A -> Prop, ((@FINITE A u) /\ (((@INTER A s t) = (@EMPTY A)) /\ ((@UNION A s t) = u))) -> (Nat.add (@nsum A s f) (@nsum A t f)) = (@nsum A u f).
Axiom thm_NSUM_EQ_SUPERSET : forall {A : Type'} (g : A -> nat), forall f : A -> nat, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A t) /\ ((@SUBSET A t s) /\ ((forall x : A, (@IN A x t) -> (f x) = (g x)) /\ (forall x : A, ((@IN A x s) /\ (~ (@IN A x t))) -> (f x) = (NUMERAL 0))))) -> (@nsum A s f) = (@nsum A t g).
Axiom thm_NSUM_RESTRICT_SET : forall {A : Type'}, forall P : A -> Prop, forall s : A -> Prop, forall f : A -> nat, (@nsum A (@GSPEC A (fun GEN_PVAR_289 : A => exists x : A, @SETSPEC A GEN_PVAR_289 ((@IN A x s) /\ (P x)) x)) f) = (@nsum A s (fun x : A => @COND nat (P x) (f x) (NUMERAL 0))).
Axiom thm_NSUM_NSUM_RESTRICT : forall {A B : Type'}, forall R' : A -> B -> Prop, forall f : A -> B -> nat, forall s : A -> Prop, forall t : B -> Prop, ((@FINITE A s) /\ (@FINITE B t)) -> (@nsum A s (fun x : A => @nsum B (@GSPEC B (fun GEN_PVAR_290 : B => exists y : B, @SETSPEC B GEN_PVAR_290 ((@IN B y t) /\ (R' x y)) y)) (fun y : B => f x y))) = (@nsum B t (fun y : B => @nsum A (@GSPEC A (fun GEN_PVAR_291 : A => exists x : A, @SETSPEC A GEN_PVAR_291 ((@IN A x s) /\ (R' x y)) x)) (fun x : A => f x y))).
Axiom thm_CARD_EQ_NSUM : forall {A : Type'}, forall s : A -> Prop, (@FINITE A s) -> (@CARD A s) = (@nsum A s (fun x : A => NUMERAL (BIT1 0))).
Axiom thm_NSUM_MULTICOUNT_GEN : forall {A B : Type'}, forall R' : A -> B -> Prop, forall s : A -> Prop, forall t : B -> Prop, forall k : B -> nat, ((@FINITE A s) /\ ((@FINITE B t) /\ (forall j : B, (@IN B j t) -> (@CARD A (@GSPEC A (fun GEN_PVAR_293 : A => exists i : A, @SETSPEC A GEN_PVAR_293 ((@IN A i s) /\ (R' i j)) i))) = (k j)))) -> (@nsum A s (fun i : A => @CARD B (@GSPEC B (fun GEN_PVAR_294 : B => exists j : B, @SETSPEC B GEN_PVAR_294 ((@IN B j t) /\ (R' i j)) j)))) = (@nsum B t (fun i : B => k i)).
Axiom thm_NSUM_MULTICOUNT : forall {A B : Type'}, forall R' : A -> B -> Prop, forall s : A -> Prop, forall t : B -> Prop, forall k : nat, ((@FINITE A s) /\ ((@FINITE B t) /\ (forall j : B, (@IN B j t) -> (@CARD A (@GSPEC A (fun GEN_PVAR_295 : A => exists i : A, @SETSPEC A GEN_PVAR_295 ((@IN A i s) /\ (R' i j)) i))) = k))) -> (@nsum A s (fun i : A => @CARD B (@GSPEC B (fun GEN_PVAR_296 : B => exists j : B, @SETSPEC B GEN_PVAR_296 ((@IN B j t) /\ (R' i j)) j)))) = (Nat.mul k (@CARD B t)).
Axiom thm_NSUM_IMAGE_GEN : forall {A B : Type'}, forall f : A -> B, forall g : A -> nat, forall s : A -> Prop, (@FINITE A s) -> (@nsum A s g) = (@nsum B (@IMAGE A B f s) (fun y : B => @nsum A (@GSPEC A (fun GEN_PVAR_297 : A => exists x : A, @SETSPEC A GEN_PVAR_297 ((@IN A x s) /\ ((f x) = y)) x)) g)).
Axiom thm_NSUM_GROUP : forall {A B : Type'}, forall f : A -> B, forall g : A -> nat, forall s : A -> Prop, forall t : B -> Prop, ((@FINITE A s) /\ (@SUBSET B (@IMAGE A B f s) t)) -> (@nsum B t (fun y : B => @nsum A (@GSPEC A (fun GEN_PVAR_298 : A => exists x : A, @SETSPEC A GEN_PVAR_298 ((@IN A x s) /\ ((f x) = y)) x)) g)) = (@nsum A s g).
Axiom thm_NSUM_GROUP_RELATION : forall {A B : Type'}, forall R' : A -> B -> Prop, forall g : A -> nat, forall s : A -> Prop, forall t : B -> Prop, ((@FINITE A s) /\ (forall x : A, (@IN A x s) -> @ex1 B (fun y : B => (@IN B y t) /\ (R' x y)))) -> (@nsum B t (fun y : B => @nsum A (@GSPEC A (fun GEN_PVAR_299 : A => exists x : A, @SETSPEC A GEN_PVAR_299 ((@IN A x s) /\ (R' x y)) x)) g)) = (@nsum A s g).
Axiom thm_NSUM_SUBSET : forall {A : Type'}, forall u : A -> Prop, forall v : A -> Prop, forall f : A -> nat, ((@FINITE A u) /\ ((@FINITE A v) /\ (forall x : A, (@IN A x (@DIFF A u v)) -> (f x) = (NUMERAL 0)))) -> Peano.le (@nsum A u f) (@nsum A v f).
Axiom thm_NSUM_SUBSET_SIMPLE : forall {A : Type'}, forall u : A -> Prop, forall v : A -> Prop, forall f : A -> nat, ((@FINITE A v) /\ (@SUBSET A u v)) -> Peano.le (@nsum A u f) (@nsum A v f).
Axiom thm_NSUM_LE_GEN : forall {A : Type'}, forall f : A -> nat, forall g : A -> nat, forall s : A -> Prop, ((forall x : A, (@IN A x s) -> Peano.le (f x) (g x)) /\ (@FINITE A (@GSPEC A (fun GEN_PVAR_301 : A => exists x : A, @SETSPEC A GEN_PVAR_301 ((@IN A x s) /\ (~ ((g x) = (NUMERAL 0)))) x)))) -> Peano.le (@nsum A s f) (@nsum A s g).
Axiom thm_NSUM_MUL_BOUND : forall {A : Type'}, forall a : A -> nat, forall b : A -> nat, forall s : A -> Prop, (@FINITE A s) -> Peano.le (@nsum A s (fun i : A => Nat.mul (a i) (b i))) (Nat.mul (@nsum A s a) (@nsum A s b)).
Axiom thm_NSUM_IMAGE_NONZERO : forall {A B : Type'}, forall d : B -> nat, forall i : A -> B, forall s : A -> Prop, ((@FINITE A s) /\ (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((~ (x = y)) /\ ((i x) = (i y))))) -> (d (i x)) = (NUMERAL 0))) -> (@nsum B (@IMAGE A B i s) d) = (@nsum A s (@o A B nat d i)).
Axiom thm_NSUM_BIJECTION : forall {A : Type'}, forall f : A -> nat, forall p : A -> A, forall s : A -> Prop, ((forall x : A, (@IN A x s) -> @IN A (p x) s) /\ (forall y : A, (@IN A y s) -> @ex1 A (fun x : A => (@IN A x s) /\ ((p x) = y)))) -> (@nsum A s f) = (@nsum A s (@o A A nat f p)).
Axiom thm_NSUM_NSUM_PRODUCT : forall {A B : Type'}, forall s : A -> Prop, forall t : A -> B -> Prop, forall x : A -> B -> nat, ((@FINITE A s) /\ (forall i : A, (@IN A i s) -> @FINITE B (t i))) -> (@nsum A s (fun i : A => @nsum B (t i) (x i))) = (@nsum (prod A B) (@GSPEC (prod A B) (fun GEN_PVAR_302 : prod A B => exists i : A, exists j : B, @SETSPEC (prod A B) GEN_PVAR_302 ((@IN A i s) /\ (@IN B j (t i))) (@pair A B i j))) (@GABS ((prod A B) -> nat) (fun f : (prod A B) -> nat => forall i : A, forall j : B, @GEQ nat (f (@pair A B i j)) (x i j)))).
Axiom thm_NSUM_EQ_GENERAL : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, forall f : A -> nat, forall g : B -> nat, forall h : A -> B, ((forall y : B, (@IN B y t) -> @ex1 A (fun x : A => (@IN A x s) /\ ((h x) = y))) /\ (forall x : A, (@IN A x s) -> (@IN B (h x) t) /\ ((g (h x)) = (f x)))) -> (@nsum A s f) = (@nsum B t g).
Axiom thm_NSUM_EQ_GENERAL_INVERSES : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, forall f : A -> nat, forall g : B -> nat, forall h : A -> B, forall k : B -> A, ((forall y : B, (@IN B y t) -> (@IN A (k y) s) /\ ((h (k y)) = y)) /\ (forall x : A, (@IN A x s) -> (@IN B (h x) t) /\ (((k (h x)) = x) /\ ((g (h x)) = (f x))))) -> (@nsum A s f) = (@nsum B t g).
Axiom thm_NSUM_INJECTION : forall {A : Type'}, forall f : A -> nat, forall p : A -> A, forall s : A -> Prop, ((@FINITE A s) /\ ((forall x : A, (@IN A x s) -> @IN A (p x) s) /\ (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((p x) = (p y)))) -> x = y))) -> (@nsum A s (@o A A nat f p)) = (@nsum A s f).
Axiom thm_NSUM_UNION_NONZERO : forall {A : Type'}, forall f : A -> nat, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A s) /\ ((@FINITE A t) /\ (forall x : A, (@IN A x (@INTER A s t)) -> (f x) = (NUMERAL 0)))) -> (@nsum A (@UNION A s t) f) = (Nat.add (@nsum A s f) (@nsum A t f)).
Axiom thm_NSUM_UNIONS_NONZERO : forall {A : Type'}, forall f : A -> nat, forall s : (A -> Prop) -> Prop, ((@FINITE (A -> Prop) s) /\ ((forall t : A -> Prop, (@IN (A -> Prop) t s) -> @FINITE A t) /\ (forall t1 : A -> Prop, forall t2 : A -> Prop, forall x : A, ((@IN (A -> Prop) t1 s) /\ ((@IN (A -> Prop) t2 s) /\ ((~ (t1 = t2)) /\ ((@IN A x t1) /\ (@IN A x t2))))) -> (f x) = (NUMERAL 0)))) -> (@nsum A (@UNIONS A s) f) = (@nsum (A -> Prop) s (fun t : A -> Prop => @nsum A t f)).
Axiom thm_NSUM_CASES : forall {A : Type'}, forall s : A -> Prop, forall P : A -> Prop, forall f : A -> nat, forall g : A -> nat, (@FINITE A s) -> (@nsum A s (fun x : A => @COND nat (P x) (f x) (g x))) = (Nat.add (@nsum A (@GSPEC A (fun GEN_PVAR_303 : A => exists x : A, @SETSPEC A GEN_PVAR_303 ((@IN A x s) /\ (P x)) x)) f) (@nsum A (@GSPEC A (fun GEN_PVAR_304 : A => exists x : A, @SETSPEC A GEN_PVAR_304 ((@IN A x s) /\ (~ (P x))) x)) g)).
Axiom thm_NSUM_CLOSED : forall {A : Type'}, forall P : nat -> Prop, forall f : A -> nat, forall s : A -> Prop, ((P (NUMERAL 0)) /\ ((forall x : nat, forall y : nat, ((P x) /\ (P y)) -> P (Nat.add x y)) /\ (forall a : A, (@IN A a s) -> P (f a)))) -> P (@nsum A s f).
Axiom thm_NSUM_RELATED : forall {A : Type'}, forall R' : nat -> nat -> Prop, forall f : A -> nat, forall g : A -> nat, forall s : A -> Prop, ((R' (NUMERAL 0) (NUMERAL 0)) /\ ((forall m : nat, forall n : nat, forall m' : nat, forall n' : nat, ((R' m n) /\ (R' m' n')) -> R' (Nat.add m m') (Nat.add n n')) /\ ((@FINITE A s) /\ (forall x : A, (@IN A x s) -> R' (f x) (g x))))) -> R' (@nsum A s f) (@nsum A s g).
Axiom thm_NSUM_CLOSED_NONEMPTY : forall {A : Type'}, forall P : nat -> Prop, forall f : A -> nat, forall s : A -> Prop, ((@FINITE A s) /\ ((~ (s = (@EMPTY A))) /\ ((forall x : nat, forall y : nat, ((P x) /\ (P y)) -> P (Nat.add x y)) /\ (forall a : A, (@IN A a s) -> P (f a))))) -> P (@nsum A s f).
Axiom thm_NSUM_RELATED_NONEMPTY : forall {A : Type'}, forall R' : nat -> nat -> Prop, forall f : A -> nat, forall g : A -> nat, forall s : A -> Prop, ((forall m : nat, forall n : nat, forall m' : nat, forall n' : nat, ((R' m n) /\ (R' m' n')) -> R' (Nat.add m m') (Nat.add n n')) /\ ((@FINITE A s) /\ ((~ (s = (@EMPTY A))) /\ (forall x : A, (@IN A x s) -> R' (f x) (g x))))) -> R' (@nsum A s f) (@nsum A s g).
Axiom thm_NSUM_ADD_NUMSEG : forall f : nat -> nat, forall g : nat -> nat, forall m : nat, forall n : nat, (@nsum nat (dotdot m n) (fun i : nat => Nat.add (f i) (g i))) = (Nat.add (@nsum nat (dotdot m n) f) (@nsum nat (dotdot m n) g)).
Axiom thm_NSUM_LE_NUMSEG : forall f : nat -> nat, forall g : nat -> nat, forall m : nat, forall n : nat, (forall i : nat, ((Peano.le m i) /\ (Peano.le i n)) -> Peano.le (f i) (g i)) -> Peano.le (@nsum nat (dotdot m n) f) (@nsum nat (dotdot m n) g).
Axiom thm_NSUM_EQ_NUMSEG : forall f : nat -> nat, forall g : nat -> nat, forall m : nat, forall n : nat, (forall i : nat, ((Peano.le m i) /\ (Peano.le i n)) -> (f i) = (g i)) -> (@nsum nat (dotdot m n) f) = (@nsum nat (dotdot m n) g).
Axiom thm_NSUM_CONST_NUMSEG : forall c : nat, forall m : nat, forall n : nat, (@nsum nat (dotdot m n) (fun n' : nat => c)) = (Nat.mul (Nat.sub (Nat.add n (NUMERAL (BIT1 0))) m) c).
Axiom thm_NSUM_EQ_0_NUMSEG : forall f : nat -> nat, forall m : nat, forall n : nat, (forall i : nat, ((Peano.le m i) /\ (Peano.le i n)) -> (f i) = (NUMERAL 0)) -> (@nsum nat (dotdot m n) f) = (NUMERAL 0).
Axiom thm_NSUM_EQ_0_IFF_NUMSEG : forall f : nat -> nat, forall m : nat, forall n : nat, ((@nsum nat (dotdot m n) f) = (NUMERAL 0)) = (forall i : nat, ((Peano.le m i) /\ (Peano.le i n)) -> (f i) = (NUMERAL 0)).
Axiom thm_NSUM_TRIV_NUMSEG : forall f : nat -> nat, forall m : nat, forall n : nat, (Peano.lt n m) -> (@nsum nat (dotdot m n) f) = (NUMERAL 0).
Axiom thm_NSUM_SING_NUMSEG : forall f : nat -> nat, forall n : nat, (@nsum nat (dotdot n n) f) = (f n).
Axiom thm_NSUM_CLAUSES_NUMSEG : forall (f : nat -> nat), (forall m : nat, (@nsum nat (dotdot m (NUMERAL 0)) f) = (@COND nat (m = (NUMERAL 0)) (f (NUMERAL 0)) (NUMERAL 0))) /\ (forall m : nat, forall n : nat, (@nsum nat (dotdot m (S n)) f) = (@COND nat (Peano.le m (S n)) (Nat.add (@nsum nat (dotdot m n) f) (f (S n))) (@nsum nat (dotdot m n) f))).
Axiom thm_NSUM_CLAUSES_NUMSEG_LT : forall (f : nat -> nat), ((@nsum nat (@GSPEC nat (fun GEN_PVAR_305 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_305 (Peano.lt i (NUMERAL 0)) i)) f) = (NUMERAL 0)) /\ (forall k : nat, (@nsum nat (@GSPEC nat (fun GEN_PVAR_306 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_306 (Peano.lt i (S k)) i)) f) = (Nat.add (@nsum nat (@GSPEC nat (fun GEN_PVAR_307 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_307 (Peano.lt i k) i)) f) (f k))).
Axiom thm_NSUM_CLAUSES_NUMSEG_LE : forall (f : nat -> nat), ((@nsum nat (@GSPEC nat (fun GEN_PVAR_308 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_308 (Peano.le i (NUMERAL 0)) i)) f) = (f (NUMERAL 0))) /\ (forall k : nat, (@nsum nat (@GSPEC nat (fun GEN_PVAR_309 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_309 (Peano.le i (S k)) i)) f) = (Nat.add (@nsum nat (@GSPEC nat (fun GEN_PVAR_310 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_310 (Peano.le i k) i)) f) (f (S k)))).
Axiom thm_NSUM_SWAP_NUMSEG : forall a : nat, forall b : nat, forall c : nat, forall d : nat, forall f : nat -> nat -> nat, (@nsum nat (dotdot a b) (fun i : nat => @nsum nat (dotdot c d) (f i))) = (@nsum nat (dotdot c d) (fun j : nat => @nsum nat (dotdot a b) (fun i : nat => f i j))).
Axiom thm_NSUM_ADD_SPLIT : forall f : nat -> nat, forall m : nat, forall n : nat, forall p : nat, (Peano.le m (Nat.add n (NUMERAL (BIT1 0)))) -> (@nsum nat (dotdot m (Nat.add n p)) f) = (Nat.add (@nsum nat (dotdot m n) f) (@nsum nat (dotdot (Nat.add n (NUMERAL (BIT1 0))) (Nat.add n p)) f)).
Axiom thm_NSUM_OFFSET : forall p : nat, forall f : nat -> nat, forall m : nat, forall n : nat, (@nsum nat (dotdot (Nat.add m p) (Nat.add n p)) f) = (@nsum nat (dotdot m n) (fun i : nat => f (Nat.add i p))).
Axiom thm_NSUM_OFFSET_0 : forall f : nat -> nat, forall m : nat, forall n : nat, (Peano.le m n) -> (@nsum nat (dotdot m n) f) = (@nsum nat (dotdot (NUMERAL 0) (Nat.sub n m)) (fun i : nat => f (Nat.add i m))).
Axiom thm_NSUM_CLAUSES_LEFT : forall f : nat -> nat, forall m : nat, forall n : nat, (Peano.le m n) -> (@nsum nat (dotdot m n) f) = (Nat.add (f m) (@nsum nat (dotdot (Nat.add m (NUMERAL (BIT1 0))) n) f)).
Axiom thm_NSUM_CLAUSES_RIGHT : forall f : nat -> nat, forall m : nat, forall n : nat, ((Peano.lt (NUMERAL 0) n) /\ (Peano.le m n)) -> (@nsum nat (dotdot m n) f) = (Nat.add (@nsum nat (dotdot m (Nat.sub n (NUMERAL (BIT1 0)))) f) (f n)).
Axiom thm_NSUM_PAIR : forall f : nat -> nat, forall m : nat, forall n : nat, (@nsum nat (dotdot (Nat.mul (NUMERAL (BIT0 (BIT1 0))) m) (Nat.add (Nat.mul (NUMERAL (BIT0 (BIT1 0))) n) (NUMERAL (BIT1 0)))) f) = (@nsum nat (dotdot m n) (fun i : nat => Nat.add (f (Nat.mul (NUMERAL (BIT0 (BIT1 0))) i)) (f (Nat.add (Nat.mul (NUMERAL (BIT0 (BIT1 0))) i) (NUMERAL (BIT1 0)))))).
Axiom thm_NSUM_REFLECT : forall x : nat -> nat, forall m : nat, forall n : nat, (@nsum nat (dotdot m n) x) = (@COND nat (Peano.lt n m) (NUMERAL 0) (@nsum nat (dotdot (NUMERAL 0) (Nat.sub n m)) (fun i : nat => x (Nat.sub n i)))).
Axiom thm_MOD_NSUM_MOD : forall {A : Type'}, forall f : A -> nat, forall n : nat, forall s : A -> Prop, (@FINITE A s) -> (Nat.modulo (@nsum A s f) n) = (Nat.modulo (@nsum A s (fun i : A => Nat.modulo (f i) n)) n).
Axiom thm_MOD_NSUM_MOD_NUMSEG : forall f : nat -> nat, forall a : nat, forall b : nat, forall n : nat, (Nat.modulo (@nsum nat (dotdot a b) f) n) = (Nat.modulo (@nsum nat (dotdot a b) (fun i : nat => Nat.modulo (f i) n)) n).
Axiom thm_CONG_NSUM : forall {A : Type'}, forall n : nat, forall f : A -> nat, forall g : A -> nat, forall s : A -> Prop, ((@FINITE A s) /\ (forall x : A, (@IN A x s) -> @eq2 nat (f x) (g x) (num_mod n))) -> @eq2 nat (@nsum A s f) (@nsum A s g) (num_mod n).
Axiom thm_CARD_UNIONS : forall {A : Type'}, forall s : (A -> Prop) -> Prop, ((@FINITE (A -> Prop) s) /\ ((forall t : A -> Prop, (@IN (A -> Prop) t s) -> @FINITE A t) /\ (forall t : A -> Prop, forall u : A -> Prop, ((@IN (A -> Prop) t s) /\ ((@IN (A -> Prop) u s) /\ (~ (t = u)))) -> (@INTER A t u) = (@EMPTY A)))) -> (@CARD A (@UNIONS A s)) = (@nsum (A -> Prop) s (@CARD A)).
Axiom thm_sum : forall {A : Type'}, (@sum A) = (@iterate A R Rplus).
Axiom thm_NEUTRAL_REAL_ADD : (@neutral R Rplus) = (INR (NUMERAL 0)).
Axiom thm_MONOIDAL_REAL_ADD : @monoidal R Rplus.
Axiom thm_SUM_DEGENERATE : forall {A : Type'}, forall f : A -> R, forall s : A -> Prop, (~ (@FINITE A (@GSPEC A (fun GEN_PVAR_313 : A => exists x : A, @SETSPEC A GEN_PVAR_313 ((@IN A x s) /\ (~ ((f x) = (INR (NUMERAL 0))))) x)))) -> (@sum A s f) = (INR (NUMERAL 0)).
Axiom thm_SUM_CLAUSES : forall {A B : Type'}, (forall f : A -> R, (@sum A (@EMPTY A) f) = (INR (NUMERAL 0))) /\ (forall x : B, forall f : B -> R, forall s : B -> Prop, (@FINITE B s) -> (@sum B (@INSERT B x s) f) = (@COND R (@IN B x s) (@sum B s f) (Rplus (f x) (@sum B s f)))).
Axiom thm_SUM_UNION : forall {A : Type'}, forall f : A -> R, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A s) /\ ((@FINITE A t) /\ (@DISJOINT A s t))) -> (@sum A (@UNION A s t) f) = (Rplus (@sum A s f) (@sum A t f)).
Axiom thm_SUM_DIFF : forall {A : Type'}, forall f : A -> R, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A s) /\ (@SUBSET A t s)) -> (@sum A (@DIFF A s t) f) = (Rminus (@sum A s f) (@sum A t f)).
Axiom thm_SUM_INCL_EXCL : forall {A : Type'}, forall s : A -> Prop, forall t : A -> Prop, forall f : A -> R, ((@FINITE A s) /\ (@FINITE A t)) -> (Rplus (@sum A s f) (@sum A t f)) = (Rplus (@sum A (@UNION A s t) f) (@sum A (@INTER A s t) f)).
Axiom thm_SUM_SUPPORT : forall {A : Type'}, forall f : A -> R, forall s : A -> Prop, (@sum A (@support A R Rplus f s) f) = (@sum A s f).
Axiom thm_SUM_ADD : forall {A : Type'}, forall f : A -> R, forall g : A -> R, forall s : A -> Prop, (@FINITE A s) -> (@sum A s (fun x : A => Rplus (f x) (g x))) = (Rplus (@sum A s f) (@sum A s g)).
Axiom thm_SUM_ADD_GEN : forall {A : Type'}, forall f : A -> R, forall g : A -> R, forall s : A -> Prop, ((@FINITE A (@GSPEC A (fun GEN_PVAR_314 : A => exists x : A, @SETSPEC A GEN_PVAR_314 ((@IN A x s) /\ (~ ((f x) = (INR (NUMERAL 0))))) x))) /\ (@FINITE A (@GSPEC A (fun GEN_PVAR_315 : A => exists x : A, @SETSPEC A GEN_PVAR_315 ((@IN A x s) /\ (~ ((g x) = (INR (NUMERAL 0))))) x)))) -> (@sum A s (fun x : A => Rplus (f x) (g x))) = (Rplus (@sum A s f) (@sum A s g)).
Axiom thm_SUM_EQ_0 : forall {A : Type'}, forall f : A -> R, forall s : A -> Prop, (forall x : A, (@IN A x s) -> (f x) = (INR (NUMERAL 0))) -> (@sum A s f) = (INR (NUMERAL 0)).
Axiom thm_SUM_0 : forall {A : Type'}, forall s : A -> Prop, (@sum A s (fun n : A => INR (NUMERAL 0))) = (INR (NUMERAL 0)).
Axiom thm_SUM_LMUL : forall {A : Type'}, forall f : A -> R, forall c : R, forall s : A -> Prop, (@sum A s (fun x : A => Rmult c (f x))) = (Rmult c (@sum A s f)).
Axiom thm_SUM_RMUL : forall {A : Type'}, forall f : A -> R, forall c : R, forall s : A -> Prop, (@sum A s (fun x : A => Rmult (f x) c)) = (Rmult (@sum A s f) c).
Axiom thm_SUM_NEG : forall {A : Type'}, forall f : A -> R, forall s : A -> Prop, (@sum A s (fun x : A => Ropp (f x))) = (Ropp (@sum A s f)).
Axiom thm_SUM_SUB : forall {A : Type'}, forall f : A -> R, forall g : A -> R, forall s : A -> Prop, (@FINITE A s) -> (@sum A s (fun x : A => Rminus (f x) (g x))) = (Rminus (@sum A s f) (@sum A s g)).
Axiom thm_SUM_LE : forall {A : Type'}, forall f : A -> R, forall g : A -> R, forall s : A -> Prop, ((@FINITE A s) /\ (forall x : A, (@IN A x s) -> Rle (f x) (g x))) -> Rle (@sum A s f) (@sum A s g).
Axiom thm_SUM_LT : forall {A : Type'}, forall f : A -> R, forall g : A -> R, forall s : A -> Prop, ((@FINITE A s) /\ ((forall x : A, (@IN A x s) -> Rle (f x) (g x)) /\ (exists x : A, (@IN A x s) /\ (Rlt (f x) (g x))))) -> Rlt (@sum A s f) (@sum A s g).
Axiom thm_SUM_LT_ALL : forall {A : Type'}, forall f : A -> R, forall g : A -> R, forall s : A -> Prop, ((@FINITE A s) /\ ((~ (s = (@EMPTY A))) /\ (forall x : A, (@IN A x s) -> Rlt (f x) (g x)))) -> Rlt (@sum A s f) (@sum A s g).
Axiom thm_SUM_POS_LT : forall {A : Type'}, forall f : A -> R, forall s : A -> Prop, ((@FINITE A s) /\ ((forall x : A, (@IN A x s) -> Rle (INR (NUMERAL 0)) (f x)) /\ (exists x : A, (@IN A x s) /\ (Rlt (INR (NUMERAL 0)) (f x))))) -> Rlt (INR (NUMERAL 0)) (@sum A s f).
Axiom thm_SUM_POS_LT_ALL : forall {A : Type'}, forall s : A -> Prop, forall f : A -> R, ((@FINITE A s) /\ ((~ (s = (@EMPTY A))) /\ (forall i : A, (@IN A i s) -> Rlt (INR (NUMERAL 0)) (f i)))) -> Rlt (INR (NUMERAL 0)) (@sum A s f).
Axiom thm_SUM_EQ : forall {A : Type'}, forall f : A -> R, forall g : A -> R, forall s : A -> Prop, (forall x : A, (@IN A x s) -> (f x) = (g x)) -> (@sum A s f) = (@sum A s g).
Axiom thm_SUM_ABS : forall {A : Type'}, forall f : A -> R, forall s : A -> Prop, (@FINITE A s) -> Rle (Rabs (@sum A s f)) (@sum A s (fun x : A => Rabs (f x))).
Axiom thm_SUM_ABS_LE : forall {A : Type'}, forall f : A -> R, forall g : A -> R, forall s : A -> Prop, ((@FINITE A s) /\ (forall x : A, (@IN A x s) -> Rle (Rabs (f x)) (g x))) -> Rle (Rabs (@sum A s f)) (@sum A s g).
Axiom thm_SUM_CONST : forall {A : Type'}, forall c : R, forall s : A -> Prop, (@FINITE A s) -> (@sum A s (fun n : A => c)) = (Rmult (INR (@CARD A s)) c).
Axiom thm_SUM_POS_LE : forall {A : Type'} (f : A -> R), forall s : A -> Prop, (forall x : A, (@IN A x s) -> Rle (INR (NUMERAL 0)) (f x)) -> Rle (INR (NUMERAL 0)) (@sum A s f).
Axiom thm_SUM_POS_BOUND : forall {A : Type'}, forall f : A -> R, forall b : R, forall s : A -> Prop, ((@FINITE A s) /\ ((forall x : A, (@IN A x s) -> Rle (INR (NUMERAL 0)) (f x)) /\ (Rle (@sum A s f) b))) -> forall x : A, (@IN A x s) -> Rle (f x) b.
Axiom thm_SUM_POS_EQ_0 : forall {A : Type'}, forall f : A -> R, forall s : A -> Prop, ((@FINITE A s) /\ ((forall x : A, (@IN A x s) -> Rle (INR (NUMERAL 0)) (f x)) /\ ((@sum A s f) = (INR (NUMERAL 0))))) -> forall x : A, (@IN A x s) -> (f x) = (INR (NUMERAL 0)).
Axiom thm_SUM_ZERO_EXISTS : forall {A : Type'}, forall u : A -> R, forall s : A -> Prop, ((@FINITE A s) /\ ((@sum A s u) = (INR (NUMERAL 0)))) -> (forall i : A, (@IN A i s) -> (u i) = (INR (NUMERAL 0))) \/ (exists j : A, exists k : A, (@IN A j s) /\ ((Rlt (u j) (INR (NUMERAL 0))) /\ ((@IN A k s) /\ (Rgt (u k) (INR (NUMERAL 0)))))).
Axiom thm_SUM_DELETE : forall {A : Type'}, forall f : A -> R, forall s : A -> Prop, forall a : A, ((@FINITE A s) /\ (@IN A a s)) -> (@sum A (@DELETE A s a) f) = (Rminus (@sum A s f) (f a)).
Axiom thm_SUM_DELETE_CASES : forall {A : Type'}, forall f : A -> R, forall s : A -> Prop, forall a : A, (@FINITE A s) -> (@sum A (@DELETE A s a) f) = (@COND R (@IN A a s) (Rminus (@sum A s f) (f a)) (@sum A s f)).
Axiom thm_SUM_SING : forall {A : Type'}, forall f : A -> R, forall x : A, (@sum A (@INSERT A x (@EMPTY A)) f) = (f x).
Axiom thm_SUM_DELTA : forall {A : Type'} (b : R), forall s : A -> Prop, forall a : A, (@sum A s (fun x : A => @COND R (x = a) b (INR (NUMERAL 0)))) = (@COND R (@IN A a s) b (INR (NUMERAL 0))).
Axiom thm_SUM_SWAP : forall {A B : Type'}, forall f : A -> B -> R, forall s : A -> Prop, forall t : B -> Prop, ((@FINITE A s) /\ (@FINITE B t)) -> (@sum A s (fun i : A => @sum B t (f i))) = (@sum B t (fun j : B => @sum A s (fun i : A => f i j))).
Axiom thm_SUM_IMAGE : forall {A B : Type'}, forall f : A -> B, forall g : B -> R, forall s : A -> Prop, (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((f x) = (f y)))) -> x = y) -> (@sum B (@IMAGE A B f s) g) = (@sum A s (@o A B R g f)).
Axiom thm_SUM_SUPERSET : forall {A : Type'}, forall f : A -> R, forall u : A -> Prop, forall v : A -> Prop, ((@SUBSET A u v) /\ (forall x : A, ((@IN A x v) /\ (~ (@IN A x u))) -> (f x) = (INR (NUMERAL 0)))) -> (@sum A v f) = (@sum A u f).
Axiom thm_SUM_UNIV : forall {A : Type'}, forall f : A -> R, forall s : A -> Prop, (@SUBSET A (@support A R Rplus f (@UNIV A)) s) -> (@sum A s f) = (@sum A (@UNIV A) f).
Axiom thm_SUM_UNION_RZERO : forall {A : Type'}, forall f : A -> R, forall u : A -> Prop, forall v : A -> Prop, ((@FINITE A u) /\ (forall x : A, ((@IN A x v) /\ (~ (@IN A x u))) -> (f x) = (INR (NUMERAL 0)))) -> (@sum A (@UNION A u v) f) = (@sum A u f).
Axiom thm_SUM_UNION_LZERO : forall {A : Type'}, forall f : A -> R, forall u : A -> Prop, forall v : A -> Prop, ((@FINITE A v) /\ (forall x : A, ((@IN A x u) /\ (~ (@IN A x v))) -> (f x) = (INR (NUMERAL 0)))) -> (@sum A (@UNION A u v) f) = (@sum A v f).
Axiom thm_SUM_RESTRICT : forall {A : Type'}, forall f : A -> R, forall s : A -> Prop, (@FINITE A s) -> (@sum A s (fun x : A => @COND R (@IN A x s) (f x) (INR (NUMERAL 0)))) = (@sum A s f).
Axiom thm_SUM_BOUND : forall {A : Type'}, forall s : A -> Prop, forall f : A -> R, forall b : R, ((@FINITE A s) /\ (forall x : A, (@IN A x s) -> Rle (f x) b)) -> Rle (@sum A s f) (Rmult (INR (@CARD A s)) b).
Axiom thm_SUM_BOUND_GEN : forall {A : Type'}, forall s : A -> Prop, forall f : A -> R, forall b : R, ((@FINITE A s) /\ ((~ (s = (@EMPTY A))) /\ (forall x : A, (@IN A x s) -> Rle (f x) (Rdiv b (INR (@CARD A s)))))) -> Rle (@sum A s f) b.
Axiom thm_SUM_ABS_BOUND : forall {A : Type'}, forall s : A -> Prop, forall f : A -> R, forall b : R, ((@FINITE A s) /\ (forall x : A, (@IN A x s) -> Rle (Rabs (f x)) b)) -> Rle (Rabs (@sum A s f)) (Rmult (INR (@CARD A s)) b).
Axiom thm_SUM_BOUND_LT : forall {A : Type'}, forall s : A -> Prop, forall f : A -> R, forall b : R, ((@FINITE A s) /\ ((forall x : A, (@IN A x s) -> Rle (f x) b) /\ (exists x : A, (@IN A x s) /\ (Rlt (f x) b)))) -> Rlt (@sum A s f) (Rmult (INR (@CARD A s)) b).
Axiom thm_SUM_BOUND_LT_ALL : forall {A : Type'}, forall s : A -> Prop, forall f : A -> R, forall b : R, ((@FINITE A s) /\ ((~ (s = (@EMPTY A))) /\ (forall x : A, (@IN A x s) -> Rlt (f x) b))) -> Rlt (@sum A s f) (Rmult (INR (@CARD A s)) b).
Axiom thm_SUM_BOUND_LT_GEN : forall {A : Type'}, forall s : A -> Prop, forall f : A -> R, forall b : R, ((@FINITE A s) /\ ((~ (s = (@EMPTY A))) /\ (forall x : A, (@IN A x s) -> Rlt (f x) (Rdiv b (INR (@CARD A s)))))) -> Rlt (@sum A s f) b.
Axiom thm_SUM_UNION_EQ : forall {A : Type'} (f : A -> R), forall s : A -> Prop, forall t : A -> Prop, forall u : A -> Prop, ((@FINITE A u) /\ (((@INTER A s t) = (@EMPTY A)) /\ ((@UNION A s t) = u))) -> (Rplus (@sum A s f) (@sum A t f)) = (@sum A u f).
Axiom thm_SUM_EQ_SUPERSET : forall {A : Type'} (g : A -> R), forall f : A -> R, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A t) /\ ((@SUBSET A t s) /\ ((forall x : A, (@IN A x t) -> (f x) = (g x)) /\ (forall x : A, ((@IN A x s) /\ (~ (@IN A x t))) -> (f x) = (INR (NUMERAL 0)))))) -> (@sum A s f) = (@sum A t g).
Axiom thm_SUM_RESTRICT_SET : forall {A : Type'}, forall P : A -> Prop, forall s : A -> Prop, forall f : A -> R, (@sum A (@GSPEC A (fun GEN_PVAR_318 : A => exists x : A, @SETSPEC A GEN_PVAR_318 ((@IN A x s) /\ (P x)) x)) f) = (@sum A s (fun x : A => @COND R (P x) (f x) (INR (NUMERAL 0)))).
Axiom thm_SUM_SUM_RESTRICT : forall {A B : Type'}, forall R' : A -> B -> Prop, forall f : A -> B -> R, forall s : A -> Prop, forall t : B -> Prop, ((@FINITE A s) /\ (@FINITE B t)) -> (@sum A s (fun x : A => @sum B (@GSPEC B (fun GEN_PVAR_319 : B => exists y : B, @SETSPEC B GEN_PVAR_319 ((@IN B y t) /\ (R' x y)) y)) (fun y : B => f x y))) = (@sum B t (fun y : B => @sum A (@GSPEC A (fun GEN_PVAR_320 : A => exists x : A, @SETSPEC A GEN_PVAR_320 ((@IN A x s) /\ (R' x y)) x)) (fun x : A => f x y))).
Axiom thm_CARD_EQ_SUM : forall {A : Type'}, forall s : A -> Prop, (@FINITE A s) -> (INR (@CARD A s)) = (@sum A s (fun x : A => INR (NUMERAL (BIT1 0)))).
Axiom thm_SUM_MULTICOUNT_GEN : forall {A B : Type'}, forall R' : A -> B -> Prop, forall s : A -> Prop, forall t : B -> Prop, forall k : B -> nat, ((@FINITE A s) /\ ((@FINITE B t) /\ (forall j : B, (@IN B j t) -> (@CARD A (@GSPEC A (fun GEN_PVAR_322 : A => exists i : A, @SETSPEC A GEN_PVAR_322 ((@IN A i s) /\ (R' i j)) i))) = (k j)))) -> (@sum A s (fun i : A => INR (@CARD B (@GSPEC B (fun GEN_PVAR_323 : B => exists j : B, @SETSPEC B GEN_PVAR_323 ((@IN B j t) /\ (R' i j)) j))))) = (@sum B t (fun i : B => INR (k i))).
Axiom thm_SUM_MULTICOUNT : forall {A B : Type'}, forall R' : A -> B -> Prop, forall s : A -> Prop, forall t : B -> Prop, forall k : nat, ((@FINITE A s) /\ ((@FINITE B t) /\ (forall j : B, (@IN B j t) -> (@CARD A (@GSPEC A (fun GEN_PVAR_324 : A => exists i : A, @SETSPEC A GEN_PVAR_324 ((@IN A i s) /\ (R' i j)) i))) = k))) -> (@sum A s (fun i : A => INR (@CARD B (@GSPEC B (fun GEN_PVAR_325 : B => exists j : B, @SETSPEC B GEN_PVAR_325 ((@IN B j t) /\ (R' i j)) j))))) = (INR (Nat.mul k (@CARD B t))).
Axiom thm_SUM_IMAGE_GEN : forall {A B : Type'}, forall f : A -> B, forall g : A -> R, forall s : A -> Prop, (@FINITE A s) -> (@sum A s g) = (@sum B (@IMAGE A B f s) (fun y : B => @sum A (@GSPEC A (fun GEN_PVAR_326 : A => exists x : A, @SETSPEC A GEN_PVAR_326 ((@IN A x s) /\ ((f x) = y)) x)) g)).
Axiom thm_SUM_GROUP : forall {A B : Type'}, forall f : A -> B, forall g : A -> R, forall s : A -> Prop, forall t : B -> Prop, ((@FINITE A s) /\ (@SUBSET B (@IMAGE A B f s) t)) -> (@sum B t (fun y : B => @sum A (@GSPEC A (fun GEN_PVAR_327 : A => exists x : A, @SETSPEC A GEN_PVAR_327 ((@IN A x s) /\ ((f x) = y)) x)) g)) = (@sum A s g).
Axiom thm_SUM_GROUP_RELATION : forall {A B : Type'}, forall R' : A -> B -> Prop, forall g : A -> R, forall s : A -> Prop, forall t : B -> Prop, ((@FINITE A s) /\ (forall x : A, (@IN A x s) -> @ex1 B (fun y : B => (@IN B y t) /\ (R' x y)))) -> (@sum B t (fun y : B => @sum A (@GSPEC A (fun GEN_PVAR_328 : A => exists x : A, @SETSPEC A GEN_PVAR_328 ((@IN A x s) /\ (R' x y)) x)) g)) = (@sum A s g).
Axiom thm_REAL_OF_NUM_SUM : forall {A : Type'}, forall f : A -> nat, forall s : A -> Prop, (@FINITE A s) -> (INR (@nsum A s f)) = (@sum A s (fun x : A => INR (f x))).
Axiom thm_SUM_SUBSET : forall {A : Type'}, forall u : A -> Prop, forall v : A -> Prop, forall f : A -> R, ((@FINITE A u) /\ ((@FINITE A v) /\ ((forall x : A, (@IN A x (@DIFF A u v)) -> Rle (f x) (INR (NUMERAL 0))) /\ (forall x : A, (@IN A x (@DIFF A v u)) -> Rle (INR (NUMERAL 0)) (f x))))) -> Rle (@sum A u f) (@sum A v f).
Axiom thm_SUM_SUBSET_SIMPLE : forall {A : Type'}, forall u : A -> Prop, forall v : A -> Prop, forall f : A -> R, ((@FINITE A v) /\ ((@SUBSET A u v) /\ (forall x : A, (@IN A x (@DIFF A v u)) -> Rle (INR (NUMERAL 0)) (f x)))) -> Rle (@sum A u f) (@sum A v f).
Axiom thm_SUM_MUL_BOUND : forall {A : Type'}, forall a : A -> R, forall b : A -> R, forall s : A -> Prop, ((@FINITE A s) /\ (forall i : A, (@IN A i s) -> (Rle (INR (NUMERAL 0)) (a i)) /\ (Rle (INR (NUMERAL 0)) (b i)))) -> Rle (@sum A s (fun i : A => Rmult (a i) (b i))) (Rmult (@sum A s a) (@sum A s b)).
Axiom thm_SUM_IMAGE_NONZERO : forall {A B : Type'}, forall d : B -> R, forall i : A -> B, forall s : A -> Prop, ((@FINITE A s) /\ (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((~ (x = y)) /\ ((i x) = (i y))))) -> (d (i x)) = (INR (NUMERAL 0)))) -> (@sum B (@IMAGE A B i s) d) = (@sum A s (@o A B R d i)).
Axiom thm_SUM_BIJECTION : forall {A : Type'}, forall f : A -> R, forall p : A -> A, forall s : A -> Prop, ((forall x : A, (@IN A x s) -> @IN A (p x) s) /\ (forall y : A, (@IN A y s) -> @ex1 A (fun x : A => (@IN A x s) /\ ((p x) = y)))) -> (@sum A s f) = (@sum A s (@o A A R f p)).
Axiom thm_SUM_SUM_PRODUCT : forall {A B : Type'}, forall s : A -> Prop, forall t : A -> B -> Prop, forall x : A -> B -> R, ((@FINITE A s) /\ (forall i : A, (@IN A i s) -> @FINITE B (t i))) -> (@sum A s (fun i : A => @sum B (t i) (x i))) = (@sum (prod A B) (@GSPEC (prod A B) (fun GEN_PVAR_329 : prod A B => exists i : A, exists j : B, @SETSPEC (prod A B) GEN_PVAR_329 ((@IN A i s) /\ (@IN B j (t i))) (@pair A B i j))) (@GABS ((prod A B) -> R) (fun f : (prod A B) -> R => forall i : A, forall j : B, @GEQ R (f (@pair A B i j)) (x i j)))).
Axiom thm_SUM_EQ_GENERAL : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, forall f : A -> R, forall g : B -> R, forall h : A -> B, ((forall y : B, (@IN B y t) -> @ex1 A (fun x : A => (@IN A x s) /\ ((h x) = y))) /\ (forall x : A, (@IN A x s) -> (@IN B (h x) t) /\ ((g (h x)) = (f x)))) -> (@sum A s f) = (@sum B t g).
Axiom thm_SUM_EQ_GENERAL_INVERSES : forall {A B : Type'}, forall s : A -> Prop, forall t : B -> Prop, forall f : A -> R, forall g : B -> R, forall h : A -> B, forall k : B -> A, ((forall y : B, (@IN B y t) -> (@IN A (k y) s) /\ ((h (k y)) = y)) /\ (forall x : A, (@IN A x s) -> (@IN B (h x) t) /\ (((k (h x)) = x) /\ ((g (h x)) = (f x))))) -> (@sum A s f) = (@sum B t g).
Axiom thm_SUM_INJECTION : forall {A : Type'}, forall f : A -> R, forall p : A -> A, forall s : A -> Prop, ((@FINITE A s) /\ ((forall x : A, (@IN A x s) -> @IN A (p x) s) /\ (forall x : A, forall y : A, ((@IN A x s) /\ ((@IN A y s) /\ ((p x) = (p y)))) -> x = y))) -> (@sum A s (@o A A R f p)) = (@sum A s f).
Axiom thm_SUM_UNION_NONZERO : forall {A : Type'}, forall f : A -> R, forall s : A -> Prop, forall t : A -> Prop, ((@FINITE A s) /\ ((@FINITE A t) /\ (forall x : A, (@IN A x (@INTER A s t)) -> (f x) = (INR (NUMERAL 0))))) -> (@sum A (@UNION A s t) f) = (Rplus (@sum A s f) (@sum A t f)).
Axiom thm_SUM_UNIONS_NONZERO : forall {A : Type'}, forall f : A -> R, forall s : (A -> Prop) -> Prop, ((@FINITE (A -> Prop) s) /\ ((forall t : A -> Prop, (@IN (A -> Prop) t s) -> @FINITE A t) /\ (forall t1 : A -> Prop, forall t2 : A -> Prop, forall x : A, ((@IN (A -> Prop) t1 s) /\ ((@IN (A -> Prop) t2 s) /\ ((~ (t1 = t2)) /\ ((@IN A x t1) /\ (@IN A x t2))))) -> (f x) = (INR (NUMERAL 0))))) -> (@sum A (@UNIONS A s) f) = (@sum (A -> Prop) s (fun t : A -> Prop => @sum A t f)).
Axiom thm_SUM_CASES : forall {A : Type'}, forall s : A -> Prop, forall P : A -> Prop, forall f : A -> R, forall g : A -> R, (@FINITE A s) -> (@sum A s (fun x : A => @COND R (P x) (f x) (g x))) = (Rplus (@sum A (@GSPEC A (fun GEN_PVAR_330 : A => exists x : A, @SETSPEC A GEN_PVAR_330 ((@IN A x s) /\ (P x)) x)) f) (@sum A (@GSPEC A (fun GEN_PVAR_331 : A => exists x : A, @SETSPEC A GEN_PVAR_331 ((@IN A x s) /\ (~ (P x))) x)) g)).
Axiom thm_SUM_CASES_1 : forall {A : Type'} (y : R) (f : A -> R), forall s : A -> Prop, forall a : A, ((@FINITE A s) /\ (@IN A a s)) -> (@sum A s (fun x : A => @COND R (x = a) y (f x))) = (Rplus (@sum A s f) (Rminus y (f a))).
Axiom thm_SUM_LE_INCLUDED : forall {A B : Type'}, forall f : A -> R, forall g : B -> R, forall s : A -> Prop, forall t : B -> Prop, forall i : B -> A, ((@FINITE A s) /\ ((@FINITE B t) /\ ((forall y : B, (@IN B y t) -> Rle (INR (NUMERAL 0)) (g y)) /\ (forall x : A, (@IN A x s) -> exists y : B, (@IN B y t) /\ (((i y) = x) /\ (Rle (f x) (g y))))))) -> Rle (@sum A s f) (@sum B t g).
Axiom thm_SUM_IMAGE_LE : forall {A B : Type'}, forall f : A -> B, forall g : B -> R, forall s : A -> Prop, ((@FINITE A s) /\ (forall x : A, (@IN A x s) -> Rle (INR (NUMERAL 0)) (g (f x)))) -> Rle (@sum B (@IMAGE A B f s) g) (@sum A s (@o A B R g f)).
Axiom thm_SUM_CLOSED : forall {A : Type'}, forall P : R -> Prop, forall f : A -> R, forall s : A -> Prop, ((P (INR (NUMERAL 0))) /\ ((forall x : R, forall y : R, ((P x) /\ (P y)) -> P (Rplus x y)) /\ (forall a : A, (@IN A a s) -> P (f a)))) -> P (@sum A s f).
Axiom thm_SUM_RELATED : forall {A : Type'}, forall R' : R -> R -> Prop, forall f : A -> R, forall g : A -> R, forall s : A -> Prop, ((R' (INR (NUMERAL 0)) (INR (NUMERAL 0))) /\ ((forall m : R, forall n : R, forall m' : R, forall n' : R, ((R' m n) /\ (R' m' n')) -> R' (Rplus m m') (Rplus n n')) /\ ((@FINITE A s) /\ (forall x : A, (@IN A x s) -> R' (f x) (g x))))) -> R' (@sum A s f) (@sum A s g).
Axiom thm_SUM_CLOSED_NONEMPTY : forall {A : Type'}, forall P : R -> Prop, forall f : A -> R, forall s : A -> Prop, ((@FINITE A s) /\ ((~ (s = (@EMPTY A))) /\ ((forall x : R, forall y : R, ((P x) /\ (P y)) -> P (Rplus x y)) /\ (forall a : A, (@IN A a s) -> P (f a))))) -> P (@sum A s f).
Axiom thm_SUM_RELATED_NONEMPTY : forall {A : Type'}, forall R' : R -> R -> Prop, forall f : A -> R, forall g : A -> R, forall s : A -> Prop, ((forall m : R, forall n : R, forall m' : R, forall n' : R, ((R' m n) /\ (R' m' n')) -> R' (Rplus m m') (Rplus n n')) /\ ((@FINITE A s) /\ ((~ (s = (@EMPTY A))) /\ (forall x : A, (@IN A x s) -> R' (f x) (g x))))) -> R' (@sum A s f) (@sum A s g).
Axiom thm_REAL_OF_NUM_SUM_GEN : forall {A : Type'}, forall f : A -> nat, forall s : A -> Prop, (@FINITE A (@GSPEC A (fun GEN_PVAR_335 : A => exists i : A, @SETSPEC A GEN_PVAR_335 ((@IN A i s) /\ (~ ((f i) = (NUMERAL 0)))) i))) -> (INR (@nsum A s f)) = (@sum A s (fun x : A => INR (f x))).
Axiom thm_SUM_ADD_NUMSEG : forall f : nat -> R, forall g : nat -> R, forall m : nat, forall n : nat, (@sum nat (dotdot m n) (fun i : nat => Rplus (f i) (g i))) = (Rplus (@sum nat (dotdot m n) f) (@sum nat (dotdot m n) g)).
Axiom thm_SUM_SUB_NUMSEG : forall f : nat -> R, forall g : nat -> R, forall m : nat, forall n : nat, (@sum nat (dotdot m n) (fun i : nat => Rminus (f i) (g i))) = (Rminus (@sum nat (dotdot m n) f) (@sum nat (dotdot m n) g)).
Axiom thm_SUM_LE_NUMSEG : forall f : nat -> R, forall g : nat -> R, forall m : nat, forall n : nat, (forall i : nat, ((Peano.le m i) /\ (Peano.le i n)) -> Rle (f i) (g i)) -> Rle (@sum nat (dotdot m n) f) (@sum nat (dotdot m n) g).
Axiom thm_SUM_EQ_NUMSEG : forall f : nat -> R, forall g : nat -> R, forall m : nat, forall n : nat, (forall i : nat, ((Peano.le m i) /\ (Peano.le i n)) -> (f i) = (g i)) -> (@sum nat (dotdot m n) f) = (@sum nat (dotdot m n) g).
Axiom thm_SUM_ABS_NUMSEG : forall f : nat -> R, forall m : nat, forall n : nat, Rle (Rabs (@sum nat (dotdot m n) f)) (@sum nat (dotdot m n) (fun i : nat => Rabs (f i))).
Axiom thm_SUM_CONST_NUMSEG : forall c : R, forall m : nat, forall n : nat, (@sum nat (dotdot m n) (fun n' : nat => c)) = (Rmult (INR (Nat.sub (Nat.add n (NUMERAL (BIT1 0))) m)) c).
Axiom thm_SUM_EQ_0_NUMSEG : forall f : nat -> R, forall m : nat, forall n : nat, (forall i : nat, ((Peano.le m i) /\ (Peano.le i n)) -> (f i) = (INR (NUMERAL 0))) -> (@sum nat (dotdot m n) f) = (INR (NUMERAL 0)).
Axiom thm_SUM_TRIV_NUMSEG : forall f : nat -> R, forall m : nat, forall n : nat, (Peano.lt n m) -> (@sum nat (dotdot m n) f) = (INR (NUMERAL 0)).
Axiom thm_SUM_POS_LE_NUMSEG : forall m : nat, forall n : nat, forall f : nat -> R, (forall p : nat, ((Peano.le m p) /\ (Peano.le p n)) -> Rle (INR (NUMERAL 0)) (f p)) -> Rle (INR (NUMERAL 0)) (@sum nat (dotdot m n) f).
Axiom thm_SUM_POS_EQ_0_NUMSEG : forall f : nat -> R, forall m : nat, forall n : nat, ((forall p : nat, ((Peano.le m p) /\ (Peano.le p n)) -> Rle (INR (NUMERAL 0)) (f p)) /\ ((@sum nat (dotdot m n) f) = (INR (NUMERAL 0)))) -> forall p : nat, ((Peano.le m p) /\ (Peano.le p n)) -> (f p) = (INR (NUMERAL 0)).
Axiom thm_SUM_SING_NUMSEG : forall f : nat -> R, forall n : nat, (@sum nat (dotdot n n) f) = (f n).
Axiom thm_SUM_CLAUSES_NUMSEG : forall (f : nat -> R), (forall m : nat, (@sum nat (dotdot m (NUMERAL 0)) f) = (@COND R (m = (NUMERAL 0)) (f (NUMERAL 0)) (INR (NUMERAL 0)))) /\ (forall m : nat, forall n : nat, (@sum nat (dotdot m (S n)) f) = (@COND R (Peano.le m (S n)) (Rplus (@sum nat (dotdot m n) f) (f (S n))) (@sum nat (dotdot m n) f))).
Axiom thm_SUM_CLAUSES_NUMSEG_LT : forall (f : nat -> R), ((@sum nat (@GSPEC nat (fun GEN_PVAR_336 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_336 (Peano.lt i (NUMERAL 0)) i)) f) = (INR (NUMERAL 0))) /\ (forall k : nat, (@sum nat (@GSPEC nat (fun GEN_PVAR_337 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_337 (Peano.lt i (S k)) i)) f) = (Rplus (@sum nat (@GSPEC nat (fun GEN_PVAR_338 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_338 (Peano.lt i k) i)) f) (f k))).
Axiom thm_SUM_CLAUSES_NUMSEG_LE : forall (f : nat -> R), ((@sum nat (@GSPEC nat (fun GEN_PVAR_339 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_339 (Peano.le i (NUMERAL 0)) i)) f) = (f (NUMERAL 0))) /\ (forall k : nat, (@sum nat (@GSPEC nat (fun GEN_PVAR_340 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_340 (Peano.le i (S k)) i)) f) = (Rplus (@sum nat (@GSPEC nat (fun GEN_PVAR_341 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_341 (Peano.le i k) i)) f) (f (S k)))).
Axiom thm_SUM_SWAP_NUMSEG : forall a : nat, forall b : nat, forall c : nat, forall d : nat, forall f : nat -> nat -> R, (@sum nat (dotdot a b) (fun i : nat => @sum nat (dotdot c d) (f i))) = (@sum nat (dotdot c d) (fun j : nat => @sum nat (dotdot a b) (fun i : nat => f i j))).
Axiom thm_SUM_ADD_SPLIT : forall f : nat -> R, forall m : nat, forall n : nat, forall p : nat, (Peano.le m (Nat.add n (NUMERAL (BIT1 0)))) -> (@sum nat (dotdot m (Nat.add n p)) f) = (Rplus (@sum nat (dotdot m n) f) (@sum nat (dotdot (Nat.add n (NUMERAL (BIT1 0))) (Nat.add n p)) f)).
Axiom thm_SUM_OFFSET : forall p : nat, forall f : nat -> R, forall m : nat, forall n : nat, (@sum nat (dotdot (Nat.add m p) (Nat.add n p)) f) = (@sum nat (dotdot m n) (fun i : nat => f (Nat.add i p))).
Axiom thm_SUM_OFFSET_0 : forall f : nat -> R, forall m : nat, forall n : nat, (Peano.le m n) -> (@sum nat (dotdot m n) f) = (@sum nat (dotdot (NUMERAL 0) (Nat.sub n m)) (fun i : nat => f (Nat.add i m))).
Axiom thm_SUM_CLAUSES_LEFT : forall f : nat -> R, forall m : nat, forall n : nat, (Peano.le m n) -> (@sum nat (dotdot m n) f) = (Rplus (f m) (@sum nat (dotdot (Nat.add m (NUMERAL (BIT1 0))) n) f)).
Axiom thm_SUM_CLAUSES_RIGHT : forall f : nat -> R, forall m : nat, forall n : nat, ((Peano.lt (NUMERAL 0) n) /\ (Peano.le m n)) -> (@sum nat (dotdot m n) f) = (Rplus (@sum nat (dotdot m (Nat.sub n (NUMERAL (BIT1 0)))) f) (f n)).
Axiom thm_SUM_PAIR : forall f : nat -> R, forall m : nat, forall n : nat, (@sum nat (dotdot (Nat.mul (NUMERAL (BIT0 (BIT1 0))) m) (Nat.add (Nat.mul (NUMERAL (BIT0 (BIT1 0))) n) (NUMERAL (BIT1 0)))) f) = (@sum nat (dotdot m n) (fun i : nat => Rplus (f (Nat.mul (NUMERAL (BIT0 (BIT1 0))) i)) (f (Nat.add (Nat.mul (NUMERAL (BIT0 (BIT1 0))) i) (NUMERAL (BIT1 0)))))).
Axiom thm_SUM_REFLECT : forall x : nat -> R, forall m : nat, forall n : nat, (@sum nat (dotdot m n) x) = (@COND R (Peano.lt n m) (INR (NUMERAL 0)) (@sum nat (dotdot (NUMERAL 0) (Nat.sub n m)) (fun i : nat => x (Nat.sub n i)))).
Axiom thm_REAL_OF_NUM_SUM_NUMSEG : forall f : nat -> nat, forall m : nat, forall n : nat, (INR (@nsum nat (dotdot m n) f)) = (@sum nat (dotdot m n) (fun i : nat => INR (f i))).
Axiom thm_SUM_PARTIAL_SUC : forall f : nat -> R, forall g : nat -> R, forall m : nat, forall n : nat, (@sum nat (dotdot m n) (fun k : nat => Rmult (f k) (Rminus (g (Nat.add k (NUMERAL (BIT1 0)))) (g k)))) = (@COND R (Peano.le m n) (Rminus (Rminus (Rmult (f (Nat.add n (NUMERAL (BIT1 0)))) (g (Nat.add n (NUMERAL (BIT1 0))))) (Rmult (f m) (g m))) (@sum nat (dotdot m n) (fun k : nat => Rmult (g (Nat.add k (NUMERAL (BIT1 0)))) (Rminus (f (Nat.add k (NUMERAL (BIT1 0)))) (f k))))) (INR (NUMERAL 0))).
Axiom thm_SUM_PARTIAL_PRE : forall f : nat -> R, forall g : nat -> R, forall m : nat, forall n : nat, (@sum nat (dotdot m n) (fun k : nat => Rmult (f k) (Rminus (g k) (g (Nat.sub k (NUMERAL (BIT1 0))))))) = (@COND R (Peano.le m n) (Rminus (Rminus (Rmult (f (Nat.add n (NUMERAL (BIT1 0)))) (g n)) (Rmult (f m) (g (Nat.sub m (NUMERAL (BIT1 0)))))) (@sum nat (dotdot m n) (fun k : nat => Rmult (g k) (Rminus (f (Nat.add k (NUMERAL (BIT1 0)))) (f k))))) (INR (NUMERAL 0))).
Axiom thm_SUM_DIFFS : forall (f : nat -> R), forall m : nat, forall n : nat, (@sum nat (dotdot m n) (fun k : nat => Rminus (f k) (f (Nat.add k (NUMERAL (BIT1 0)))))) = (@COND R (Peano.le m n) (Rminus (f m) (f (Nat.add n (NUMERAL (BIT1 0))))) (INR (NUMERAL 0))).
Axiom thm_SUM_DIFFS_ALT : forall (f : nat -> R), forall m : nat, forall n : nat, (@sum nat (dotdot m n) (fun k : nat => Rminus (f (Nat.add k (NUMERAL (BIT1 0)))) (f k))) = (@COND R (Peano.le m n) (Rminus (f (Nat.add n (NUMERAL (BIT1 0)))) (f m)) (INR (NUMERAL 0))).
Axiom thm_SUM_COMBINE_R : forall f : nat -> R, forall m : nat, forall n : nat, forall p : nat, ((Peano.le m (Nat.add n (NUMERAL (BIT1 0)))) /\ (Peano.le n p)) -> (Rplus (@sum nat (dotdot m n) f) (@sum nat (dotdot (Nat.add n (NUMERAL (BIT1 0))) p) f)) = (@sum nat (dotdot m p) f).
Axiom thm_SUM_COMBINE_L : forall f : nat -> R, forall m : nat, forall n : nat, forall p : nat, ((Peano.lt (NUMERAL 0) n) /\ ((Peano.le m n) /\ (Peano.le n (Nat.add p (NUMERAL (BIT1 0)))))) -> (Rplus (@sum nat (dotdot m (Nat.sub n (NUMERAL (BIT1 0)))) f) (@sum nat (dotdot n p) f)) = (@sum nat (dotdot m p) f).
Axiom thm_REAL_SUB_POW : forall x : R, forall y : R, forall n : nat, (Peano.le (NUMERAL (BIT1 0)) n) -> (Rminus (Rpower_nat x n) (Rpower_nat y n)) = (Rmult (Rminus x y) (@sum nat (dotdot (NUMERAL 0) (Nat.sub n (NUMERAL (BIT1 0)))) (fun i : nat => Rmult (Rpower_nat x i) (Rpower_nat y (Nat.sub (Nat.sub n (NUMERAL (BIT1 0))) i))))).
Axiom thm_REAL_SUB_POW_R1 : forall x : R, forall n : nat, (Peano.le (NUMERAL (BIT1 0)) n) -> (Rminus (Rpower_nat x n) (INR (NUMERAL (BIT1 0)))) = (Rmult (Rminus x (INR (NUMERAL (BIT1 0)))) (@sum nat (dotdot (NUMERAL 0) (Nat.sub n (NUMERAL (BIT1 0)))) (fun i : nat => Rpower_nat x i))).
Axiom thm_REAL_SUB_POW_L1 : forall x : R, forall n : nat, (Peano.le (NUMERAL (BIT1 0)) n) -> (Rminus (INR (NUMERAL (BIT1 0))) (Rpower_nat x n)) = (Rmult (Rminus (INR (NUMERAL (BIT1 0))) x) (@sum nat (dotdot (NUMERAL 0) (Nat.sub n (NUMERAL (BIT1 0)))) (fun i : nat => Rpower_nat x i))).
Axiom thm_REAL_SUB_POLYFUN : forall a : nat -> R, forall x : R, forall y : R, forall n : nat, (Peano.le (NUMERAL (BIT1 0)) n) -> (Rminus (@sum nat (dotdot (NUMERAL 0) n) (fun i : nat => Rmult (a i) (Rpower_nat x i))) (@sum nat (dotdot (NUMERAL 0) n) (fun i : nat => Rmult (a i) (Rpower_nat y i)))) = (Rmult (Rminus x y) (@sum nat (dotdot (NUMERAL 0) (Nat.sub n (NUMERAL (BIT1 0)))) (fun j : nat => Rmult (@sum nat (dotdot (Nat.add j (NUMERAL (BIT1 0))) n) (fun i : nat => Rmult (a i) (Rpower_nat y (Nat.sub (Nat.sub i j) (NUMERAL (BIT1 0)))))) (Rpower_nat x j)))).
Axiom thm_REAL_SUB_POLYFUN_ALT : forall a : nat -> R, forall x : R, forall y : R, forall n : nat, (Peano.le (NUMERAL (BIT1 0)) n) -> (Rminus (@sum nat (dotdot (NUMERAL 0) n) (fun i : nat => Rmult (a i) (Rpower_nat x i))) (@sum nat (dotdot (NUMERAL 0) n) (fun i : nat => Rmult (a i) (Rpower_nat y i)))) = (Rmult (Rminus x y) (@sum nat (dotdot (NUMERAL 0) (Nat.sub n (NUMERAL (BIT1 0)))) (fun j : nat => Rmult (@sum nat (dotdot (NUMERAL 0) (Nat.sub (Nat.sub n j) (NUMERAL (BIT1 0)))) (fun k : nat => Rmult (a (Nat.add j (Nat.add k (NUMERAL (BIT1 0))))) (Rpower_nat y k))) (Rpower_nat x j)))).
Axiom thm_REAL_POLYFUN_ROOTBOUND : forall n : nat, forall c : nat -> R, (~ (forall i : nat, (@IN nat i (dotdot (NUMERAL 0) n)) -> (c i) = (INR (NUMERAL 0)))) -> (@FINITE R (@GSPEC R (fun GEN_PVAR_347 : R => exists x : R, @SETSPEC R GEN_PVAR_347 ((@sum nat (dotdot (NUMERAL 0) n) (fun i : nat => Rmult (c i) (Rpower_nat x i))) = (INR (NUMERAL 0))) x))) /\ (Peano.le (@CARD R (@GSPEC R (fun GEN_PVAR_348 : R => exists x : R, @SETSPEC R GEN_PVAR_348 ((@sum nat (dotdot (NUMERAL 0) n) (fun i : nat => Rmult (c i) (Rpower_nat x i))) = (INR (NUMERAL 0))) x))) n).
Axiom thm_REAL_POLYFUN_FINITE_ROOTS : forall n : nat, forall c : nat -> R, (@FINITE R (@GSPEC R (fun GEN_PVAR_350 : R => exists x : R, @SETSPEC R GEN_PVAR_350 ((@sum nat (dotdot (NUMERAL 0) n) (fun i : nat => Rmult (c i) (Rpower_nat x i))) = (INR (NUMERAL 0))) x))) = (exists i : nat, (@IN nat i (dotdot (NUMERAL 0) n)) /\ (~ ((c i) = (INR (NUMERAL 0))))).
Axiom thm_REAL_POLYFUN_EQ_0 : forall n : nat, forall c : nat -> R, (forall x : R, (@sum nat (dotdot (NUMERAL 0) n) (fun i : nat => Rmult (c i) (Rpower_nat x i))) = (INR (NUMERAL 0))) = (forall i : nat, (@IN nat i (dotdot (NUMERAL 0) n)) -> (c i) = (INR (NUMERAL 0))).
Axiom thm_REAL_POLYFUN_EQ_CONST : forall n : nat, forall c : nat -> R, forall k : R, (forall x : R, (@sum nat (dotdot (NUMERAL 0) n) (fun i : nat => Rmult (c i) (Rpower_nat x i))) = k) = (((c (NUMERAL 0)) = k) /\ (forall i : nat, (@IN nat i (dotdot (NUMERAL (BIT1 0)) n)) -> (c i) = (INR (NUMERAL 0)))).
Axiom thm_polynomial_function : forall p : R -> R, (polynomial_function p) = (exists m : nat, exists c : nat -> R, forall x : R, (p x) = (@sum nat (dotdot (NUMERAL 0) m) (fun i : nat => Rmult (c i) (Rpower_nat x i)))).
Axiom thm_POLYNOMIAL_FUNCTION_CONST : forall c : R, polynomial_function (fun x : R => c).
Axiom thm_POLYNOMIAL_FUNCTION_ID : polynomial_function (fun x : R => x).
Axiom thm_POLYNOMIAL_FUNCTION_I : polynomial_function (@I R).
Axiom thm_POLYNOMIAL_FUNCTION_ADD : forall p : R -> R, forall q : R -> R, ((polynomial_function p) /\ (polynomial_function q)) -> polynomial_function (fun x : R => Rplus (p x) (q x)).
Axiom thm_POLYNOMIAL_FUNCTION_LMUL : forall p : R -> R, forall c : R, (polynomial_function p) -> polynomial_function (fun x : R => Rmult c (p x)).
Axiom thm_POLYNOMIAL_FUNCTION_RMUL : forall p : R -> R, forall c : R, (polynomial_function p) -> polynomial_function (fun x : R => Rmult (p x) c).
Axiom thm_POLYNOMIAL_FUNCTION_NEG : forall p : R -> R, (polynomial_function (fun x : R => Ropp (p x))) = (polynomial_function p).
Axiom thm_POLYNOMIAL_FUNCTION_SUB : forall p : R -> R, forall q : R -> R, ((polynomial_function p) /\ (polynomial_function q)) -> polynomial_function (fun x : R => Rminus (p x) (q x)).
Axiom thm_POLYNOMIAL_FUNCTION_MUL : forall p : R -> R, forall q : R -> R, ((polynomial_function p) /\ (polynomial_function q)) -> polynomial_function (fun x : R => Rmult (p x) (q x)).
Axiom thm_POLYNOMIAL_FUNCTION_SUM : forall {A : Type'}, forall s : A -> Prop, forall p : R -> A -> R, ((@FINITE A s) /\ (forall i : A, (@IN A i s) -> polynomial_function (fun x : R => p x i))) -> polynomial_function (fun x : R => @sum A s (p x)).
Axiom thm_POLYNOMIAL_FUNCTION_POW : forall p : R -> R, forall n : nat, (polynomial_function p) -> polynomial_function (fun x : R => Rpower_nat (p x) n).
Axiom thm_POLYNOMIAL_FUNCTION_INDUCT : forall P : (R -> R) -> Prop, ((P (fun x : R => x)) /\ ((forall c : R, P (fun x : R => c)) /\ ((forall p : R -> R, forall q : R -> R, ((P p) /\ (P q)) -> P (fun x : R => Rplus (p x) (q x))) /\ (forall p : R -> R, forall q : R -> R, ((P p) /\ (P q)) -> P (fun x : R => Rmult (p x) (q x)))))) -> forall p : R -> R, (polynomial_function p) -> P p.
Axiom thm_POLYNOMIAL_FUNCTION_o : forall p : R -> R, forall q : R -> R, ((polynomial_function p) /\ (polynomial_function q)) -> polynomial_function (@o R R R p q).
Axiom thm_POLYNOMIAL_FUNCTION_FINITE_ROOTS : forall p : R -> R, forall a : R, (polynomial_function p) -> (@FINITE R (@GSPEC R (fun GEN_PVAR_353 : R => exists x : R, @SETSPEC R GEN_PVAR_353 ((p x) = a) x))) = (~ (forall x : R, (p x) = a)).
Axiom thm_dimindex : forall {A : Type'}, forall s : A -> Prop, (@dimindex A s) = (@COND nat (@FINITE A (@UNIV A)) (@CARD A (@UNIV A)) (NUMERAL (BIT1 0))).
Axiom thm_DIMINDEX_NONZERO : forall {A : Type'}, forall s : A -> Prop, ~ ((@dimindex A s) = (NUMERAL 0)).
Axiom thm_DIMINDEX_GE_1 : forall {A : Type'}, forall s : A -> Prop, Peano.le (NUMERAL (BIT1 0)) (@dimindex A s).
Axiom thm_DIMINDEX_UNIV : forall {A : Type'}, forall s : A -> Prop, (@dimindex A s) = (@dimindex A (@UNIV A)).
Axiom thm_DIMINDEX_UNIQUE : forall {A : Type'} (n : nat), (@HAS_SIZE A (@UNIV A) n) -> (@dimindex A (@UNIV A)) = n.
Axiom thm_UNIV_HAS_SIZE_DIMINDEX : forall {N : Type'}, (@HAS_SIZE N (@UNIV N) (@dimindex N (@UNIV N))) = (@FINITE N (@UNIV N)).
Axiom thm_HAS_SIZE_1 : @HAS_SIZE unit (@UNIV unit) (NUMERAL (BIT1 0)).
Axiom thm_NUMSEG_LT_DIMINDEX : forall {N : Type'}, (@GSPEC nat (fun GEN_PVAR_354 : nat => exists i : nat, @SETSPEC nat GEN_PVAR_354 (Peano.lt i (@dimindex N (@UNIV N))) i)) = (dotdot (NUMERAL 0) (Nat.sub (@dimindex N (@UNIV N)) (NUMERAL (BIT1 0)))).
Axiom thm_FINITE_IMAGE_IMAGE : forall {A : Type'}, (@UNIV (finite_image A)) = (@IMAGE nat (finite_image A) (@finite_index A) (dotdot (NUMERAL (BIT1 0)) (@dimindex A (@UNIV A)))).
Axiom thm_HAS_SIZE_FINITE_IMAGE : forall {A : Type'}, forall s : A -> Prop, @HAS_SIZE (finite_image A) (@UNIV (finite_image A)) (@dimindex A s).
Axiom thm_CARD_FINITE_IMAGE : forall {A : Type'}, forall s : A -> Prop, (@CARD (finite_image A) (@UNIV (finite_image A))) = (@dimindex A s).
Axiom thm_FINITE_FINITE_IMAGE : forall {A : Type'}, @FINITE (finite_image A) (@UNIV (finite_image A)).
Axiom thm_DIMINDEX_FINITE_IMAGE : forall {A : Type'}, forall s : (finite_image A) -> Prop, forall t : A -> Prop, (@dimindex (finite_image A) s) = (@dimindex A t).
Axiom thm_FINITE_INDEX_WORKS : forall {A : Type'}, forall i : finite_image A, @ex1 nat (fun n : nat => (Peano.le (NUMERAL (BIT1 0)) n) /\ ((Peano.le n (@dimindex A (@UNIV A))) /\ ((@finite_index A n) = i))).
Axiom thm_FINITE_INDEX_INJ : forall {A : Type'}, forall i : nat, forall j : nat, ((Peano.le (NUMERAL (BIT1 0)) i) /\ ((Peano.le i (@dimindex A (@UNIV A))) /\ ((Peano.le (NUMERAL (BIT1 0)) j) /\ (Peano.le j (@dimindex A (@UNIV A)))))) -> ((@finite_index A i) = (@finite_index A j)) = (i = j).
Axiom thm_FORALL_FINITE_INDEX : forall {N : Type'} (P : (finite_image N) -> Prop), (forall k : finite_image N, P k) = (forall i : nat, ((Peano.le (NUMERAL (BIT1 0)) i) /\ (Peano.le i (@dimindex N (@UNIV N)))) -> P (@finite_index N i)).
Axiom thm_finite_index : forall {A N : Type'}, forall x : cart A N, forall i : nat, (@dollar A N x i) = (@dest_cart A N x (@finite_index N i)).
Axiom thm_CART_EQ : forall {A B : Type'}, forall x : cart A B, forall y : cart A B, (x = y) = (forall i : nat, ((Peano.le (NUMERAL (BIT1 0)) i) /\ (Peano.le i (@dimindex B (@UNIV B)))) -> (@dollar A B x i) = (@dollar A B y i)).
Axiom thm_lambda : forall {A B : Type'}, forall g : nat -> A, (@lambda A B g) = (@ε (cart A B) (fun f : cart A B => forall i : nat, ((Peano.le (NUMERAL (BIT1 0)) i) /\ (Peano.le i (@dimindex B (@UNIV B)))) -> (@dollar A B f i) = (g i))).
Axiom thm_LAMBDA_BETA : forall {A B : Type'} (g : nat -> A), forall i : nat, ((Peano.le (NUMERAL (BIT1 0)) i) /\ (Peano.le i (@dimindex B (@UNIV B)))) -> (@dollar A B (@lambda A B g) i) = (g i).
Axiom thm_LAMBDA_UNIQUE : forall {A B : Type'}, forall f : cart A B, forall g : nat -> A, (forall i : nat, ((Peano.le (NUMERAL (BIT1 0)) i) /\ (Peano.le i (@dimindex B (@UNIV B)))) -> (@dollar A B f i) = (g i)) = ((@lambda A B g) = f).
Axiom thm_LAMBDA_ETA : forall {A B : Type'}, forall g : cart A B, (@lambda A B (fun i : nat => @dollar A B g i)) = g.
Axiom thm_FINITE_INDEX_INRANGE : forall {A N : Type'}, forall i : nat, exists k : nat, (Peano.le (NUMERAL (BIT1 0)) k) /\ ((Peano.le k (@dimindex N (@UNIV N))) /\ (forall x : cart A N, (@dollar A N x i) = (@dollar A N x k))).
Axiom thm_FINITE_INDEX_INRANGE_2 : forall {A B N : Type'}, forall i : nat, exists k : nat, (Peano.le (NUMERAL (BIT1 0)) k) /\ ((Peano.le k (@dimindex N (@UNIV N))) /\ ((forall x : cart A N, (@dollar A N x i) = (@dollar A N x k)) /\ (forall y : cart B N, (@dollar B N y i) = (@dollar B N y k)))).
Axiom thm_CART_EQ_FULL : forall {A N : Type'}, forall x : cart A N, forall y : cart A N, (x = y) = (forall i : nat, (@dollar A N x i) = (@dollar A N y i)).
Axiom thm_pastecart : forall {A M N : Type'}, forall f : cart A M, forall g : cart A N, (@pastecart A M N f g) = (@lambda A (finite_sum M N) (fun i : nat => @COND A (Peano.le i (@dimindex M (@UNIV M))) (@dollar A M f i) (@dollar A N g (Nat.sub i (@dimindex M (@UNIV M)))))).
Axiom thm_fstcart : forall {A M N : Type'}, forall f : cart A (finite_sum M N), (@fstcart A M N f) = (@lambda A M (fun i : nat => @dollar A (finite_sum M N) f i)).
Axiom thm_sndcart : forall {A M N : Type'}, forall f : cart A (finite_sum M N), (@sndcart A M N f) = (@lambda A N (fun i : nat => @dollar A (finite_sum M N) f (Nat.add i (@dimindex M (@UNIV M))))).
Axiom thm_FINITE_SUM_IMAGE : forall {A B : Type'}, (@UNIV (finite_sum A B)) = (@IMAGE nat (finite_sum A B) (@mk_finite_sum A B) (dotdot (NUMERAL (BIT1 0)) (Nat.add (@dimindex A (@UNIV A)) (@dimindex B (@UNIV B))))).
Axiom thm_DIMINDEX_HAS_SIZE_FINITE_SUM : forall {M N : Type'}, @HAS_SIZE (finite_sum M N) (@UNIV (finite_sum M N)) (Nat.add (@dimindex M (@UNIV M)) (@dimindex N (@UNIV N))).
Axiom thm_DIMINDEX_FINITE_SUM : forall {M N : Type'}, (@dimindex (finite_sum M N) (@UNIV (finite_sum M N))) = (Nat.add (@dimindex M (@UNIV M)) (@dimindex N (@UNIV N))).
Axiom thm_FSTCART_PASTECART : forall {A M N : Type'}, forall x : cart A M, forall y : cart A N, (@fstcart A M N (@pastecart A M N x y)) = x.
Axiom thm_SNDCART_PASTECART : forall {A M N : Type'}, forall x : cart A M, forall y : cart A N, (@sndcart A M N (@pastecart A M N x y)) = y.
Axiom thm_PASTECART_FST_SND : forall {A M N : Type'}, forall z : cart A (finite_sum M N), (@pastecart A M N (@fstcart A M N z) (@sndcart A M N z)) = z.
Axiom thm_PASTECART_EQ : forall {A M N : Type'}, forall x : cart A (finite_sum M N), forall y : cart A (finite_sum M N), (x = y) = (((@fstcart A M N x) = (@fstcart A M N y)) /\ ((@sndcart A M N x) = (@sndcart A M N y))).
Axiom thm_FORALL_PASTECART : forall {A M N : Type'} (P : (cart A (finite_sum M N)) -> Prop), (forall p : cart A (finite_sum M N), P p) = (forall x : cart A M, forall y : cart A N, P (@pastecart A M N x y)).
Axiom thm_EXISTS_PASTECART : forall {A M N : Type'} (P : (cart A (finite_sum M N)) -> Prop), (exists p : cart A (finite_sum M N), P p) = (exists x : cart A M, exists y : cart A N, P (@pastecart A M N x y)).
Axiom thm_PASTECART_INJ : forall {A M N : Type'}, forall x : cart A M, forall y : cart A N, forall w : cart A M, forall z : cart A N, ((@pastecart A M N x y) = (@pastecart A M N w z)) = ((x = w) /\ (y = z)).
Axiom thm_FSTCART_COMPONENT : forall {A M N : Type'}, forall x : cart A (finite_sum M N), forall i : nat, ((Peano.le (NUMERAL (BIT1 0)) i) /\ (Peano.le i (@dimindex M (@UNIV M)))) -> (@dollar A M (@fstcart A M N x) i) = (@dollar A (finite_sum M N) x i).
Axiom thm_SNDCART_COMPONENT : forall {A M N : Type'}, forall x : cart A (finite_sum M N), forall i : nat, ((Peano.le (NUMERAL (BIT1 0)) i) /\ (Peano.le i (@dimindex N (@UNIV N)))) -> (@dollar A N (@sndcart A M N x) i) = (@dollar A (finite_sum M N) x (Nat.add i (@dimindex M (@UNIV M)))).
Axiom thm_PASTECART_COMPONENT : forall {A M N : Type'}, (forall u : cart A M, forall v : cart A N, forall i : nat, ((Peano.le (NUMERAL (BIT1 0)) i) /\ (Peano.le i (@dimindex M (@UNIV M)))) -> (@dollar A (finite_sum M N) (@pastecart A M N u v) i) = (@dollar A M u i)) /\ (forall u : cart A M, forall v : cart A N, forall i : nat, ((Peano.le (Nat.add (@dimindex M (@UNIV M)) (NUMERAL (BIT1 0))) i) /\ (Peano.le i (Nat.add (@dimindex M (@UNIV M)) (@dimindex N (@UNIV N))))) -> (@dollar A (finite_sum M N) (@pastecart A M N u v) i) = (@dollar A N v (Nat.sub i (@dimindex M (@UNIV M))))).
Axiom thm_FINITE_DIFF_IMAGE : forall {A B : Type'}, (@UNIV (finite_diff A B)) = (@IMAGE nat (finite_diff A B) (@mk_finite_diff A B) (dotdot (NUMERAL (BIT1 0)) (@COND nat (Peano.lt (@dimindex B (@UNIV B)) (@dimindex A (@UNIV A))) (Nat.sub (@dimindex A (@UNIV A)) (@dimindex B (@UNIV B))) (NUMERAL (BIT1 0))))).
Axiom thm_DIMINDEX_HAS_SIZE_FINITE_DIFF : forall {M N : Type'}, @HAS_SIZE (finite_diff M N) (@UNIV (finite_diff M N)) (@COND nat (Peano.lt (@dimindex N (@UNIV N)) (@dimindex M (@UNIV M))) (Nat.sub (@dimindex M (@UNIV M)) (@dimindex N (@UNIV N))) (NUMERAL (BIT1 0))).
Axiom thm_DIMINDEX_FINITE_DIFF : forall {M N : Type'}, (@dimindex (finite_diff M N) (@UNIV (finite_diff M N))) = (@COND nat (Peano.lt (@dimindex N (@UNIV N)) (@dimindex M (@UNIV M))) (Nat.sub (@dimindex M (@UNIV M)) (@dimindex N (@UNIV N))) (NUMERAL (BIT1 0))).
Axiom thm_FINITE_PROD_IMAGE : forall {A B : Type'}, (@UNIV (finite_prod A B)) = (@IMAGE nat (finite_prod A B) (@mk_finite_prod A B) (dotdot (NUMERAL (BIT1 0)) (Nat.mul (@dimindex A (@UNIV A)) (@dimindex B (@UNIV B))))).
Axiom thm_DIMINDEX_HAS_SIZE_FINITE_PROD : forall {M N : Type'}, @HAS_SIZE (finite_prod M N) (@UNIV (finite_prod M N)) (Nat.mul (@dimindex M (@UNIV M)) (@dimindex N (@UNIV N))).
Axiom thm_DIMINDEX_FINITE_PROD : forall {M N : Type'}, (@dimindex (finite_prod M N) (@UNIV (finite_prod M N))) = (Nat.mul (@dimindex M (@UNIV M)) (@dimindex N (@UNIV N))).
Axiom thm_tybit0_INDUCT : forall {A : Type'}, forall P : (tybit0 A) -> Prop, (forall a : finite_sum A A, P (@mktybit0 A a)) -> forall x : tybit0 A, P x.
Axiom thm_tybit0_RECURSION : forall {A Z : Type'}, forall f : (finite_sum A A) -> Z, exists fn : (tybit0 A) -> Z, forall a : finite_sum A A, (fn (@mktybit0 A a)) = (f a).
Axiom thm_tybit1_INDUCT : forall {A : Type'}, forall P : (tybit1 A) -> Prop, (forall a : finite_sum (finite_sum A A) unit, P (@mktybit1 A a)) -> forall x : tybit1 A, P x.
Axiom thm_tybit1_RECURSION : forall {A Z : Type'}, forall f : (finite_sum (finite_sum A A) unit) -> Z, exists fn : (tybit1 A) -> Z, forall a : finite_sum (finite_sum A A) unit, (fn (@mktybit1 A a)) = (f a).
Axiom thm_HAS_SIZE_TYBIT0 : forall {A : Type'}, @HAS_SIZE (tybit0 A) (@UNIV (tybit0 A)) (Nat.mul (NUMERAL (BIT0 (BIT1 0))) (@dimindex A (@UNIV A))).
Axiom thm_HAS_SIZE_TYBIT1 : forall {A : Type'}, @HAS_SIZE (tybit1 A) (@UNIV (tybit1 A)) (Nat.add (Nat.mul (NUMERAL (BIT0 (BIT1 0))) (@dimindex A (@UNIV A))) (NUMERAL (BIT1 0))).
Axiom thm_DIMINDEX_TYBIT0 : forall {A : Type'}, (@dimindex (tybit0 A) (@UNIV (tybit0 A))) = (Nat.mul (NUMERAL (BIT0 (BIT1 0))) (@dimindex A (@UNIV A))).
Axiom thm_DIMINDEX_TYBIT1 : forall {A : Type'}, (@dimindex (tybit1 A) (@UNIV (tybit1 A))) = (Nat.add (Nat.mul (NUMERAL (BIT0 (BIT1 0))) (@dimindex A (@UNIV A))) (NUMERAL (BIT1 0))).
Axiom thm_DIMINDEX_CLAUSES : forall {A : Type'}, ((@dimindex unit (@UNIV unit)) = (NUMERAL (BIT1 0))) /\ (((@dimindex (tybit0 A) (@UNIV (tybit0 A))) = (Nat.mul (NUMERAL (BIT0 (BIT1 0))) (@dimindex A (@UNIV A)))) /\ ((@dimindex (tybit1 A) (@UNIV (tybit1 A))) = (Nat.add (Nat.mul (NUMERAL (BIT0 (BIT1 0))) (@dimindex A (@UNIV A))) (NUMERAL (BIT1 0))))).
Axiom thm_FINITE_1 : @FINITE unit (@UNIV unit).
Axiom thm_FINITE_TYBIT0 : forall {A : Type'}, @FINITE (tybit0 A) (@UNIV (tybit0 A)).
Axiom thm_FINITE_TYBIT1 : forall {A : Type'}, @FINITE (tybit1 A) (@UNIV (tybit1 A)).
Axiom thm_FINITE_CLAUSES : forall {A : Type'}, (@FINITE unit (@UNIV unit)) /\ ((@FINITE (tybit0 A) (@UNIV (tybit0 A))) /\ (@FINITE (tybit1 A) (@UNIV (tybit1 A)))).
Axiom thm_DIMINDEX_2 : (@dimindex (tybit0 unit) (@UNIV (tybit0 unit))) = (NUMERAL (BIT0 (BIT1 0))).
Axiom thm_DIMINDEX_3 : (@dimindex (tybit1 unit) (@UNIV (tybit1 unit))) = (NUMERAL (BIT1 (BIT1 0))).
Axiom thm_DIMINDEX_4 : (@dimindex (tybit0 (tybit0 unit)) (@UNIV (tybit0 (tybit0 unit)))) = (NUMERAL (BIT0 (BIT0 (BIT1 0)))).
Axiom thm_FINITE_CART : forall {A N : Type'}, forall P : nat -> A -> Prop, (forall i : nat, ((Peano.le (NUMERAL (BIT1 0)) i) /\ (Peano.le i (@dimindex N (@UNIV N)))) -> @FINITE A (@GSPEC A (fun GEN_PVAR_360 : A => exists x : A, @SETSPEC A GEN_PVAR_360 (P i x) x))) -> @FINITE (cart A N) (@GSPEC (cart A N) (fun GEN_PVAR_361 : cart A N => exists v : cart A N, @SETSPEC (cart A N) GEN_PVAR_361 (forall i : nat, ((Peano.le (NUMERAL (BIT1 0)) i) /\ (Peano.le i (@dimindex N (@UNIV N)))) -> P i (@dollar A N v i)) v)).
Axiom thm_HAS_SIZE_CART_UNIV : forall {A N : Type'}, forall m : nat, (@HAS_SIZE A (@UNIV A) m) -> @HAS_SIZE (cart A N) (@UNIV (cart A N)) (Nat.pow m (@dimindex N (@UNIV N))).
Axiom thm_CARD_CART_UNIV : forall {A N : Type'}, (@FINITE A (@UNIV A)) -> (@CARD (cart A N) (@UNIV (cart A N))) = (Nat.pow (@CARD A (@UNIV A)) (@dimindex N (@UNIV N))).
Axiom thm_FINITE_CART_UNIV : forall {A N : Type'}, (@FINITE A (@UNIV A)) -> @FINITE (cart A N) (@UNIV (cart A N)).
Axiom thm_vector : forall {A N : Type'}, forall l : list A, (@vector A N l) = (@lambda A N (fun i : nat => @EL A (Nat.sub i (NUMERAL (BIT1 0))) l)).
Axiom thm_IN_ELIM_PASTECART_THM : forall {A M N : Type'}, forall P : (cart A M) -> (cart A N) -> Prop, forall a : cart A M, forall b : cart A N, (@IN (cart A (finite_sum M N)) (@pastecart A M N a b) (@GSPEC (cart A (finite_sum M N)) (fun GEN_PVAR_362 : cart A (finite_sum M N) => exists x : cart A M, exists y : cart A N, @SETSPEC (cart A (finite_sum M N)) GEN_PVAR_362 (P x y) (@pastecart A M N x y)))) = (P a b).
Axiom thm_PCROSS : forall {A M N : Type'}, forall s : (cart A M) -> Prop, forall t : (cart A N) -> Prop, (@PCROSS A M N s t) = (@GSPEC (cart A (finite_sum M N)) (fun GEN_PVAR_363 : cart A (finite_sum M N) => exists x : cart A M, exists y : cart A N, @SETSPEC (cart A (finite_sum M N)) GEN_PVAR_363 ((@IN (cart A M) x s) /\ (@IN (cart A N) y t)) (@pastecart A M N x y))).
Axiom thm_FORALL_IN_PCROSS : forall {A M N : Type'} (s : (cart A M) -> Prop) (t : (cart A N) -> Prop) (P : (cart A (finite_sum M N)) -> Prop), (forall z : cart A (finite_sum M N), (@IN (cart A (finite_sum M N)) z (@PCROSS A M N s t)) -> P z) = (forall x : cart A M, forall y : cart A N, ((@IN (cart A M) x s) /\ (@IN (cart A N) y t)) -> P (@pastecart A M N x y)).
Axiom thm_EXISTS_IN_PCROSS : forall {A M N : Type'} (s : (cart A M) -> Prop) (t : (cart A N) -> Prop) (P : (cart A (finite_sum M N)) -> Prop), (exists z : cart A (finite_sum M N), (@IN (cart A (finite_sum M N)) z (@PCROSS A M N s t)) /\ (P z)) = (exists x : cart A M, exists y : cart A N, (@IN (cart A M) x s) /\ ((@IN (cart A N) y t) /\ (P (@pastecart A M N x y)))).
Axiom thm_PASTECART_IN_PCROSS : forall {A M N : Type'}, forall s : (cart A M) -> Prop, forall t : (cart A N) -> Prop, forall x : cart A M, forall y : cart A N, (@IN (cart A (finite_sum M N)) (@pastecart A M N x y) (@PCROSS A M N s t)) = ((@IN (cart A M) x s) /\ (@IN (cart A N) y t)).
Axiom thm_PCROSS_EQ_EMPTY : forall {A M N : Type'}, forall s : (cart A M) -> Prop, forall t : (cart A N) -> Prop, ((@PCROSS A M N s t) = (@EMPTY (cart A (finite_sum M N)))) = ((s = (@EMPTY (cart A M))) \/ (t = (@EMPTY (cart A N)))).
Axiom thm_PCROSS_EMPTY : forall {A M N : Type'}, (forall s : (cart A M) -> Prop, (@PCROSS A M N s (@EMPTY (cart A N))) = (@EMPTY (cart A (finite_sum M N)))) /\ (forall t : (cart A N) -> Prop, (@PCROSS A M N (@EMPTY (cart A M)) t) = (@EMPTY (cart A (finite_sum M N)))).
Axiom thm_PCROSS_SING : forall {A M N : Type'}, forall x : cart A M, forall y : cart A N, (@PCROSS A M N (@INSERT (cart A M) x (@EMPTY (cart A M))) (@INSERT (cart A N) y (@EMPTY (cart A N)))) = (@INSERT (cart A (finite_sum M N)) (@pastecart A M N x y) (@EMPTY (cart A (finite_sum M N)))).
Axiom thm_SUBSET_PCROSS : forall {A M N : Type'}, forall s : (cart A M) -> Prop, forall t : (cart A N) -> Prop, forall s' : (cart A M) -> Prop, forall t' : (cart A N) -> Prop, (@SUBSET (cart A (finite_sum M N)) (@PCROSS A M N s t) (@PCROSS A M N s' t')) = ((s = (@EMPTY (cart A M))) \/ ((t = (@EMPTY (cart A N))) \/ ((@SUBSET (cart A M) s s') /\ (@SUBSET (cart A N) t t')))).
Axiom thm_PCROSS_MONO : forall {A M N : Type'}, forall s : (cart A M) -> Prop, forall t : (cart A N) -> Prop, forall s' : (cart A M) -> Prop, forall t' : (cart A N) -> Prop, ((@SUBSET (cart A M) s s') /\ (@SUBSET (cart A N) t t')) -> @SUBSET (cart A (finite_sum M N)) (@PCROSS A M N s t) (@PCROSS A M N s' t').
Axiom thm_PCROSS_EQ : forall {M N : Type'}, forall s : (cart R M) -> Prop, forall s' : (cart R M) -> Prop, forall t : (cart R N) -> Prop, forall t' : (cart R N) -> Prop, ((@PCROSS R M N s t) = (@PCROSS R M N s' t')) = ((((s = (@EMPTY (cart R M))) \/ (t = (@EMPTY (cart R N)))) /\ ((s' = (@EMPTY (cart R M))) \/ (t' = (@EMPTY (cart R N))))) \/ ((s = s') /\ (t = t'))).
Axiom thm_UNIV_PCROSS_UNIV : forall {A M N : Type'}, (@PCROSS A M N (@UNIV (cart A M)) (@UNIV (cart A N))) = (@UNIV (cart A (finite_sum M N))).
Axiom thm_HAS_SIZE_PCROSS : forall {A M N : Type'}, forall s : (cart A M) -> Prop, forall t : (cart A N) -> Prop, forall m : nat, forall n : nat, ((@HAS_SIZE (cart A M) s m) /\ (@HAS_SIZE (cart A N) t n)) -> @HAS_SIZE (cart A (finite_sum M N)) (@PCROSS A M N s t) (Nat.mul m n).
Axiom thm_FINITE_PCROSS : forall {A M N : Type'}, forall s : (cart A M) -> Prop, forall t : (cart A N) -> Prop, ((@FINITE (cart A M) s) /\ (@FINITE (cart A N) t)) -> @FINITE (cart A (finite_sum M N)) (@PCROSS A M N s t).
Axiom thm_FINITE_PCROSS_EQ : forall {A M N : Type'}, forall s : (cart A M) -> Prop, forall t : (cart A N) -> Prop, (@FINITE (cart A (finite_sum M N)) (@PCROSS A M N s t)) = ((s = (@EMPTY (cart A M))) \/ ((t = (@EMPTY (cart A N))) \/ ((@FINITE (cart A M) s) /\ (@FINITE (cart A N) t)))).
Axiom thm_IMAGE_FSTCART_PCROSS : forall {M N : Type'}, forall s : (cart R M) -> Prop, forall t : (cart R N) -> Prop, (@IMAGE (cart R (finite_sum M N)) (cart R M) (@fstcart R M N) (@PCROSS R M N s t)) = (@COND ((cart R M) -> Prop) (t = (@EMPTY (cart R N))) (@EMPTY (cart R M)) s).
Axiom thm_IMAGE_SNDCART_PCROSS : forall {M N : Type'}, forall s : (cart R M) -> Prop, forall t : (cart R N) -> Prop, (@IMAGE (cart R (finite_sum M N)) (cart R N) (@sndcart R M N) (@PCROSS R M N s t)) = (@COND ((cart R N) -> Prop) (s = (@EMPTY (cart R M))) (@EMPTY (cart R N)) t).
Axiom thm_PCROSS_INTER : forall {A M N : Type'}, (forall s : (cart A M) -> Prop, forall t : (cart A N) -> Prop, forall u : (cart A N) -> Prop, (@PCROSS A M N s (@INTER (cart A N) t u)) = (@INTER (cart A (finite_sum M N)) (@PCROSS A M N s t) (@PCROSS A M N s u))) /\ (forall s : (cart A M) -> Prop, forall t : (cart A M) -> Prop, forall u : (cart A N) -> Prop, (@PCROSS A M N (@INTER (cart A M) s t) u) = (@INTER (cart A (finite_sum M N)) (@PCROSS A M N s u) (@PCROSS A M N t u))).
Axiom thm_PCROSS_UNION : forall {A M N : Type'}, (forall s : (cart A M) -> Prop, forall t : (cart A N) -> Prop, forall u : (cart A N) -> Prop, (@PCROSS A M N s (@UNION (cart A N) t u)) = (@UNION (cart A (finite_sum M N)) (@PCROSS A M N s t) (@PCROSS A M N s u))) /\ (forall s : (cart A M) -> Prop, forall t : (cart A M) -> Prop, forall u : (cart A N) -> Prop, (@PCROSS A M N (@UNION (cart A M) s t) u) = (@UNION (cart A (finite_sum M N)) (@PCROSS A M N s u) (@PCROSS A M N t u))).
Axiom thm_PCROSS_DIFF : forall {A M N : Type'}, (forall s : (cart A M) -> Prop, forall t : (cart A N) -> Prop, forall u : (cart A N) -> Prop, (@PCROSS A M N s (@DIFF (cart A N) t u)) = (@DIFF (cart A (finite_sum M N)) (@PCROSS A M N s t) (@PCROSS A M N s u))) /\ (forall s : (cart A M) -> Prop, forall t : (cart A M) -> Prop, forall u : (cart A N) -> Prop, (@PCROSS A M N (@DIFF (cart A M) s t) u) = (@DIFF (cart A (finite_sum M N)) (@PCROSS A M N s u) (@PCROSS A M N t u))).
Axiom thm_INTER_PCROSS : forall {A M N : Type'}, forall s : (cart A M) -> Prop, forall s' : (cart A M) -> Prop, forall t : (cart A N) -> Prop, forall t' : (cart A N) -> Prop, (@INTER (cart A (finite_sum M N)) (@PCROSS A M N s t) (@PCROSS A M N s' t')) = (@PCROSS A M N (@INTER (cart A M) s s') (@INTER (cart A N) t t')).
Axiom thm_PCROSS_UNIONS : forall {A M N : Type'}, (forall s : (cart A M) -> Prop, forall f : ((cart A N) -> Prop) -> Prop, (@PCROSS A M N s (@UNIONS (cart A N) f)) = (@UNIONS (cart A (finite_sum M N)) (@GSPEC ((cart A (finite_sum M N)) -> Prop) (fun GEN_PVAR_365 : (cart A (finite_sum M N)) -> Prop => exists t : (cart A N) -> Prop, @SETSPEC ((cart A (finite_sum M N)) -> Prop) GEN_PVAR_365 (@IN ((cart A N) -> Prop) t f) (@PCROSS A M N s t))))) /\ (forall f : ((cart A M) -> Prop) -> Prop, forall t : (cart A N) -> Prop, (@PCROSS A M N (@UNIONS (cart A M) f) t) = (@UNIONS (cart A (finite_sum M N)) (@GSPEC ((cart A (finite_sum M N)) -> Prop) (fun GEN_PVAR_366 : (cart A (finite_sum M N)) -> Prop => exists s : (cart A M) -> Prop, @SETSPEC ((cart A (finite_sum M N)) -> Prop) GEN_PVAR_366 (@IN ((cart A M) -> Prop) s f) (@PCROSS A M N s t))))).
Axiom thm_PCROSS_UNIONS_UNIONS : forall {A M N : Type'}, forall f : ((cart A M) -> Prop) -> Prop, forall g : ((cart A N) -> Prop) -> Prop, (@PCROSS A M N (@UNIONS (cart A M) f) (@UNIONS (cart A N) g)) = (@UNIONS (cart A (finite_sum M N)) (@GSPEC ((cart A (finite_sum M N)) -> Prop) (fun GEN_PVAR_364 : (cart A (finite_sum M N)) -> Prop => exists s : (cart A M) -> Prop, exists t : (cart A N) -> Prop, @SETSPEC ((cart A (finite_sum M N)) -> Prop) GEN_PVAR_364 ((@IN ((cart A M) -> Prop) s f) /\ (@IN ((cart A N) -> Prop) t g)) (@PCROSS A M N s t)))).
Axiom thm_PCROSS_INTERS : forall {A M N : Type'}, (forall s : (cart A M) -> Prop, forall f : ((cart A N) -> Prop) -> Prop, (@PCROSS A M N s (@INTERS (cart A N) f)) = (@COND ((cart A (finite_sum M N)) -> Prop) (f = (@EMPTY ((cart A N) -> Prop))) (@PCROSS A M N s (@UNIV (cart A N))) (@INTERS (cart A (finite_sum M N)) (@GSPEC ((cart A (finite_sum M N)) -> Prop) (fun GEN_PVAR_370 : (cart A (finite_sum M N)) -> Prop => exists t : (cart A N) -> Prop, @SETSPEC ((cart A (finite_sum M N)) -> Prop) GEN_PVAR_370 (@IN ((cart A N) -> Prop) t f) (@PCROSS A M N s t)))))) /\ (forall f : ((cart A M) -> Prop) -> Prop, forall t : (cart A N) -> Prop, (@PCROSS A M N (@INTERS (cart A M) f) t) = (@COND ((cart A (finite_sum M N)) -> Prop) (f = (@EMPTY ((cart A M) -> Prop))) (@PCROSS A M N (@UNIV (cart A M)) t) (@INTERS (cart A (finite_sum M N)) (@GSPEC ((cart A (finite_sum M N)) -> Prop) (fun GEN_PVAR_371 : (cart A (finite_sum M N)) -> Prop => exists s : (cart A M) -> Prop, @SETSPEC ((cart A (finite_sum M N)) -> Prop) GEN_PVAR_371 (@IN ((cart A M) -> Prop) s f) (@PCROSS A M N s t)))))).
Axiom thm_PCROSS_INTERS_INTERS : forall {A M N : Type'}, forall f : ((cart A M) -> Prop) -> Prop, forall g : ((cart A N) -> Prop) -> Prop, (@PCROSS A M N (@INTERS (cart A M) f) (@INTERS (cart A N) g)) = (@COND ((cart A (finite_sum M N)) -> Prop) (f = (@EMPTY ((cart A M) -> Prop))) (@INTERS (cart A (finite_sum M N)) (@GSPEC ((cart A (finite_sum M N)) -> Prop) (fun GEN_PVAR_367 : (cart A (finite_sum M N)) -> Prop => exists t : (cart A N) -> Prop, @SETSPEC ((cart A (finite_sum M N)) -> Prop) GEN_PVAR_367 (@IN ((cart A N) -> Prop) t g) (@PCROSS A M N (@UNIV (cart A M)) t)))) (@COND ((cart A (finite_sum M N)) -> Prop) (g = (@EMPTY ((cart A N) -> Prop))) (@INTERS (cart A (finite_sum M N)) (@GSPEC ((cart A (finite_sum M N)) -> Prop) (fun GEN_PVAR_368 : (cart A (finite_sum M N)) -> Prop => exists s : (cart A M) -> Prop, @SETSPEC ((cart A (finite_sum M N)) -> Prop) GEN_PVAR_368 (@IN ((cart A M) -> Prop) s f) (@PCROSS A M N s (@UNIV (cart A N)))))) (@INTERS (cart A (finite_sum M N)) (@GSPEC ((cart A (finite_sum M N)) -> Prop) (fun GEN_PVAR_369 : (cart A (finite_sum M N)) -> Prop => exists s : (cart A M) -> Prop, exists t : (cart A N) -> Prop, @SETSPEC ((cart A (finite_sum M N)) -> Prop) GEN_PVAR_369 ((@IN ((cart A M) -> Prop) s f) /\ (@IN ((cart A N) -> Prop) t g)) (@PCROSS A M N s t)))))).
Axiom thm_DISJOINT_PCROSS : forall {A M N : Type'}, forall s : (cart A M) -> Prop, forall t : (cart A N) -> Prop, forall s' : (cart A M) -> Prop, forall t' : (cart A N) -> Prop, (@DISJOINT (cart A (finite_sum M N)) (@PCROSS A M N s t) (@PCROSS A M N s' t')) = ((@DISJOINT (cart A M) s s') \/ (@DISJOINT (cart A N) t t')).
Axiom thm_CASEWISE_DEF : forall {_137714 _137750 _137754 _137755 : Type'} (h : prod (_137750 -> _137754) (_137755 -> _137750 -> _137714)) (t : list (prod (_137750 -> _137754) (_137755 -> _137750 -> _137714))) (f : _137755) (x : _137754), ((@CASEWISE _137714 _137750 _137754 _137755 (@nil (prod (_137750 -> _137754) (_137755 -> _137750 -> _137714))) f x) = (@ε _137714 (fun y : _137714 => True))) /\ ((@CASEWISE _137714 _137750 _137754 _137755 (@cons (prod (_137750 -> _137754) (_137755 -> _137750 -> _137714)) h t) f x) = (@COND _137714 (exists y : _137750, (@fst (_137750 -> _137754) (_137755 -> _137750 -> _137714) h y) = x) (@snd (_137750 -> _137754) (_137755 -> _137750 -> _137714) h f (@ε _137750 (fun y : _137750 => (@fst (_137750 -> _137754) (_137755 -> _137750 -> _137714) h y) = x))) (@CASEWISE _137714 _137750 _137754 _137755 t f x))).
Axiom thm_CASEWISE : forall {_137766 _137774 _137775 _137814 _137815 _137817 : Type'} (t : _137815 -> _137817 -> _137775) (s : _137817 -> _137814) (clauses : list (prod (_137817 -> _137814) (_137815 -> _137817 -> _137775))) (f : _137815) (x : _137814), ((@CASEWISE _137774 _137766 _137814 _137815 (@nil (prod (_137766 -> _137814) (_137815 -> _137766 -> _137774))) f x) = (@ε _137774 (fun y : _137774 => True))) /\ ((@CASEWISE _137775 _137817 _137814 _137815 (@cons (prod (_137817 -> _137814) (_137815 -> _137817 -> _137775)) (@pair (_137817 -> _137814) (_137815 -> _137817 -> _137775) s t) clauses) f x) = (@COND _137775 (exists y : _137817, (s y) = x) (t f (@ε _137817 (fun y : _137817 => (s y) = x))) (@CASEWISE _137775 _137817 _137814 _137815 clauses f x))).
Axiom thm_CASEWISE_CASES : forall {_137906 _137907 _137909 _137916 : Type'}, forall clauses : list (prod (_137909 -> _137906) (_137907 -> _137909 -> _137916)), forall c : _137907, forall x : _137906, (exists s : _137909 -> _137906, exists t : _137907 -> _137909 -> _137916, exists a : _137909, (@List.In (prod (_137909 -> _137906) (_137907 -> _137909 -> _137916)) (@pair (_137909 -> _137906) (_137907 -> _137909 -> _137916) s t) clauses) /\ (((s a) = x) /\ ((@CASEWISE _137916 _137909 _137906 _137907 clauses c x) = (t c a)))) \/ ((~ (exists s : _137909 -> _137906, exists t : _137907 -> _137909 -> _137916, exists a : _137909, (@List.In (prod (_137909 -> _137906) (_137907 -> _137909 -> _137916)) (@pair (_137909 -> _137906) (_137907 -> _137909 -> _137916) s t) clauses) /\ ((s a) = x))) /\ ((@CASEWISE _137916 _137909 _137906 _137907 clauses c x) = (@ε _137916 (fun y : _137916 => True)))).
Axiom thm_CASEWISE_WORKS : forall {A B C P : Type'}, forall clauses : list (prod (P -> A) (C -> P -> B)), forall c : C, (forall s : P -> A, forall t : C -> P -> B, forall s' : P -> A, forall t' : C -> P -> B, forall x : P, forall y : P, ((@List.In (prod (P -> A) (C -> P -> B)) (@pair (P -> A) (C -> P -> B) s t) clauses) /\ ((@List.In (prod (P -> A) (C -> P -> B)) (@pair (P -> A) (C -> P -> B) s' t') clauses) /\ ((s x) = (s' y)))) -> (t c x) = (t' c y)) -> @List.Forall (prod (P -> A) (C -> P -> B)) (@GABS ((prod (P -> A) (C -> P -> B)) -> Prop) (fun f : (prod (P -> A) (C -> P -> B)) -> Prop => forall s : P -> A, forall t : C -> P -> B, @GEQ Prop (f (@pair (P -> A) (C -> P -> B) s t)) (forall x : P, (@CASEWISE B P A C clauses c (s x)) = (t c x)))) clauses.
Axiom thm_admissible : forall {_138045 _138048 _138052 _138053 _138058 : Type'}, forall p : (_138052 -> _138048) -> _138058 -> Prop, forall lt2 : _138052 -> _138045 -> Prop, forall s : _138058 -> _138045, forall t : (_138052 -> _138048) -> _138058 -> _138053, (@admissible _138045 _138048 _138052 _138053 _138058 lt2 p s t) = (forall f : _138052 -> _138048, forall g : _138052 -> _138048, forall a : _138058, ((p f a) /\ ((p g a) /\ (forall z : _138052, (lt2 z (s a)) -> (f z) = (g z)))) -> (t f a) = (t g a)).
Axiom thm_tailadmissible : forall {A B P : Type'}, forall lt2 : A -> A -> Prop, forall s : P -> A, forall p : (A -> B) -> P -> Prop, forall t : (A -> B) -> P -> B, (@tailadmissible A B P lt2 p s t) = (exists P' : (A -> B) -> P -> Prop, exists G : (A -> B) -> P -> A, exists H : (A -> B) -> P -> B, (forall f : A -> B, forall a : P, forall y : A, ((P' f a) /\ (lt2 y (G f a))) -> lt2 y (s a)) /\ ((forall f : A -> B, forall g : A -> B, forall a : P, (forall z : A, (lt2 z (s a)) -> (f z) = (g z)) -> ((P' f a) = (P' g a)) /\ (((G f a) = (G g a)) /\ ((H f a) = (H g a)))) /\ (forall f : A -> B, forall a : P, (p f a) -> (t f a) = (@COND B (P' f a) (f (G f a)) (H f a))))).
Axiom thm_superadmissible : forall {_138202 _138204 _138210 : Type'}, forall lt2 : _138202 -> _138202 -> Prop, forall p : (_138202 -> _138204) -> _138210 -> Prop, forall s : _138210 -> _138202, forall t : (_138202 -> _138204) -> _138210 -> _138204, (@superadmissible _138202 _138204 _138210 lt2 p s t) = ((@admissible _138202 _138204 _138202 Prop _138210 lt2 (fun f : _138202 -> _138204 => fun a : _138210 => True) s p) -> @tailadmissible _138202 _138204 _138210 lt2 p s t).
Axiom thm_MATCH_SEQPATTERN : forall {_138238 _138245 : Type'} (r : _138245 -> _138238 -> Prop) (x : _138245) (s : _138245 -> _138238 -> Prop), (@_MATCH _138245 _138238 x (@_SEQPATTERN _138245 _138238 r s)) = (@COND _138238 (exists y : _138238, r x y) (@_MATCH _138245 _138238 x r) (@_MATCH _138245 _138238 x s)).
Axiom thm_ADMISSIBLE_CONST : forall {_138265 _138266 _138267 _138268 _138269 : Type'} (lt2 : _138266 -> _138265 -> Prop), forall p : (_138266 -> _138267) -> _138268 -> Prop, forall s : _138268 -> _138265, forall c : _138268 -> _138269, @admissible _138265 _138267 _138266 _138269 _138268 lt2 p s (fun f : _138266 -> _138267 => c).
Axiom thm_ADMISSIBLE_BASE : forall {A B P : Type'}, forall lt2 : A -> A -> Prop, forall p : (A -> B) -> P -> Prop, forall s : P -> A, forall t : P -> A, (forall f : A -> B, forall a : P, (p f a) -> lt2 (t a) (s a)) -> @admissible A B A B P lt2 p s (fun f : A -> B => fun x : P => f (t x)).
Axiom thm_ADMISSIBLE_COMB : forall {A B C D P : Type'}, forall lt2 : A -> A -> Prop, forall p : (A -> B) -> P -> Prop, forall s : P -> A, forall g : (A -> B) -> P -> C -> D, forall y : (A -> B) -> P -> C, ((@admissible A B A (C -> D) P lt2 p s g) /\ (@admissible A B A C P lt2 p s y)) -> @admissible A B A D P lt2 p s (fun f : A -> B => fun x : P => g f x (y f x)).
Axiom thm_ADMISSIBLE_RAND : forall {A B C D P : Type'}, forall lt2 : A -> A -> Prop, forall p : (A -> B) -> P -> Prop, forall s : P -> A, forall g : P -> C -> D, forall y : (A -> B) -> P -> C, (@admissible A B A C P lt2 p s y) -> @admissible A B A D P lt2 p s (fun f : A -> B => fun x : P => g x (y f x)).
Axiom thm_ADMISSIBLE_LAMBDA : forall {A B C P : Type'}, forall lt2 : A -> A -> Prop, forall p : (A -> B) -> P -> Prop, forall s : P -> A, forall t : (A -> B) -> C -> P -> Prop, (@admissible A B A Prop (prod C P) lt2 (fun f : A -> B => @GABS ((prod C P) -> Prop) (fun f' : (prod C P) -> Prop => forall u : C, forall x : P, @GEQ Prop (f' (@pair C P u x)) (p f x))) (@GABS ((prod C P) -> A) (fun f : (prod C P) -> A => forall u : C, forall x : P, @GEQ A (f (@pair C P u x)) (s x))) (fun f : A -> B => @GABS ((prod C P) -> Prop) (fun f' : (prod C P) -> Prop => forall u : C, forall x : P, @GEQ Prop (f' (@pair C P u x)) (t f u x)))) -> @admissible A B A (C -> Prop) P lt2 p s (fun f : A -> B => fun x : P => fun u : C => t f u x).
Axiom thm_ADMISSIBLE_NEST : forall {A B P : Type'}, forall lt2 : A -> A -> Prop, forall p : (A -> B) -> P -> Prop, forall s : P -> A, forall t : (A -> B) -> P -> A, ((@admissible A B A A P lt2 p s t) /\ (forall f : A -> B, forall a : P, (p f a) -> lt2 (t f a) (s a))) -> @admissible A B A B P lt2 p s (fun f : A -> B => fun x : P => f (t f x)).
Axiom thm_ADMISSIBLE_COND : forall {_138602 _138603 _138634 _138659 P : Type'}, forall lt2 : _138603 -> _138602 -> Prop, forall p : (_138603 -> _138634) -> P -> Prop, forall P' : (_138603 -> _138634) -> P -> Prop, forall s : P -> _138602, forall h : (_138603 -> _138634) -> P -> _138659, forall k : (_138603 -> _138634) -> P -> _138659, ((@admissible _138602 _138634 _138603 Prop P lt2 p s P') /\ ((@admissible _138602 _138634 _138603 _138659 P lt2 (fun f : _138603 -> _138634 => fun x : P => (p f x) /\ (P' f x)) s h) /\ (@admissible _138602 _138634 _138603 _138659 P lt2 (fun f : _138603 -> _138634 => fun x : P => (p f x) /\ (~ (P' f x))) s k))) -> @admissible _138602 _138634 _138603 _138659 P lt2 p s (fun f : _138603 -> _138634 => fun x : P => @COND _138659 (P' f x) (h f x) (k f x)).
Axiom thm_ADMISSIBLE_MATCH : forall {_138700 _138701 _138702 _138734 _138737 P : Type'}, forall lt2 : _138701 -> _138700 -> Prop, forall p : (_138701 -> _138702) -> P -> Prop, forall s : P -> _138700, forall e : (_138701 -> _138702) -> P -> _138737, forall c : (_138701 -> _138702) -> P -> _138737 -> _138734 -> Prop, ((@admissible _138700 _138702 _138701 _138737 P lt2 p s e) /\ (@admissible _138700 _138702 _138701 (_138734 -> Prop) P lt2 p s (fun f : _138701 -> _138702 => fun x : P => c f x (e f x)))) -> @admissible _138700 _138702 _138701 _138734 P lt2 p s (fun f : _138701 -> _138702 => fun x : P => @_MATCH _138737 _138734 (e f x) (c f x)).
Axiom thm_ADMISSIBLE_SEQPATTERN : forall {_138777 _138778 _138840 _138856 _138866 P : Type'}, forall lt2 : _138778 -> _138777 -> Prop, forall p : (_138778 -> _138840) -> P -> Prop, forall s : P -> _138777, forall c1 : (_138778 -> _138840) -> P -> _138866 -> _138856 -> Prop, forall c2 : (_138778 -> _138840) -> P -> _138866 -> _138856 -> Prop, forall e : (_138778 -> _138840) -> P -> _138866, ((@admissible _138777 _138840 _138778 Prop P lt2 p s (fun f : _138778 -> _138840 => fun x : P => exists y : _138856, c1 f x (e f x) y)) /\ ((@admissible _138777 _138840 _138778 (_138856 -> Prop) P lt2 (fun f : _138778 -> _138840 => fun x : P => (p f x) /\ (exists y : _138856, c1 f x (e f x) y)) s (fun f : _138778 -> _138840 => fun x : P => c1 f x (e f x))) /\ (@admissible _138777 _138840 _138778 (_138856 -> Prop) P lt2 (fun f : _138778 -> _138840 => fun x : P => (p f x) /\ (~ (exists y : _138856, c1 f x (e f x) y))) s (fun f : _138778 -> _138840 => fun x : P => c2 f x (e f x))))) -> @admissible _138777 _138840 _138778 (_138856 -> Prop) P lt2 p s (fun f : _138778 -> _138840 => fun x : P => @_SEQPATTERN _138866 _138856 (c1 f x) (c2 f x) (e f x)).
Axiom thm_ADMISSIBLE_UNGUARDED_PATTERN : forall {_138951 _138952 _138999 _139032 _139039 P : Type'}, forall lt2 : _138952 -> _138951 -> Prop, forall p : (_138952 -> _138999) -> P -> Prop, forall s : P -> _138951, forall pat : (_138952 -> _138999) -> P -> _139032, forall e : (_138952 -> _138999) -> P -> _139032, forall t : (_138952 -> _138999) -> P -> _139039, forall y : (_138952 -> _138999) -> P -> _139039, ((@admissible _138951 _138999 _138952 _139032 P lt2 p s pat) /\ ((@admissible _138951 _138999 _138952 _139032 P lt2 p s e) /\ ((@admissible _138951 _138999 _138952 _139039 P lt2 (fun f : _138952 -> _138999 => fun x : P => (p f x) /\ ((pat f x) = (e f x))) s t) /\ (@admissible _138951 _138999 _138952 _139039 P lt2 (fun f : _138952 -> _138999 => fun x : P => (p f x) /\ ((pat f x) = (e f x))) s y)))) -> @admissible _138951 _138999 _138952 Prop P lt2 p s (fun f : _138952 -> _138999 => fun x : P => _UNGUARDED_PATTERN (@GEQ _139032 (pat f x) (e f x)) (@GEQ _139039 (t f x) (y f x))).
Axiom thm_ADMISSIBLE_GUARDED_PATTERN : forall {_139125 _139126 _139203 _139241 _139250 P : Type'}, forall lt2 : _139126 -> _139125 -> Prop, forall p : (_139126 -> _139203) -> P -> Prop, forall s : P -> _139125, forall pat : (_139126 -> _139203) -> P -> _139241, forall q : (_139126 -> _139203) -> P -> Prop, forall e : (_139126 -> _139203) -> P -> _139241, forall t : (_139126 -> _139203) -> P -> _139250, forall y : (_139126 -> _139203) -> P -> _139250, ((@admissible _139125 _139203 _139126 _139241 P lt2 p s pat) /\ ((@admissible _139125 _139203 _139126 _139241 P lt2 p s e) /\ ((@admissible _139125 _139203 _139126 _139250 P lt2 (fun f : _139126 -> _139203 => fun x : P => (p f x) /\ (((pat f x) = (e f x)) /\ (q f x))) s t) /\ ((@admissible _139125 _139203 _139126 Prop P lt2 (fun f : _139126 -> _139203 => fun x : P => (p f x) /\ ((pat f x) = (e f x))) s q) /\ (@admissible _139125 _139203 _139126 _139250 P lt2 (fun f : _139126 -> _139203 => fun x : P => (p f x) /\ (((pat f x) = (e f x)) /\ (q f x))) s y))))) -> @admissible _139125 _139203 _139126 Prop P lt2 p s (fun f : _139126 -> _139203 => fun x : P => _GUARDED_PATTERN (@GEQ _139241 (pat f x) (e f x)) (q f x) (@GEQ _139250 (t f x) (y f x))).
Axiom thm_ADMISSIBLE_NSUM : forall {A B C P : Type'}, forall lt2 : B -> A -> Prop, forall p : (B -> C) -> P -> Prop, forall s : P -> A, forall h : (B -> C) -> P -> nat -> nat, forall a : P -> nat, forall b : P -> nat, (@admissible A C B nat (prod nat P) lt2 (fun f : B -> C => @GABS ((prod nat P) -> Prop) (fun f' : (prod nat P) -> Prop => forall k : nat, forall x : P, @GEQ Prop (f' (@pair nat P k x)) ((Peano.le (a x) k) /\ ((Peano.le k (b x)) /\ (p f x))))) (@GABS ((prod nat P) -> A) (fun f : (prod nat P) -> A => forall k : nat, forall x : P, @GEQ A (f (@pair nat P k x)) (s x))) (fun f : B -> C => @GABS ((prod nat P) -> nat) (fun f' : (prod nat P) -> nat => forall k : nat, forall x : P, @GEQ nat (f' (@pair nat P k x)) (h f x k)))) -> @admissible A C B nat P lt2 p s (fun f : B -> C => fun x : P => @nsum nat (dotdot (a x) (b x)) (h f x)).
Axiom thm_ADMISSIBLE_SUM : forall {A B C P : Type'}, forall lt2 : B -> A -> Prop, forall p : (B -> C) -> P -> Prop, forall s : P -> A, forall h : (B -> C) -> P -> nat -> R, forall a : P -> nat, forall b : P -> nat, (@admissible A C B R (prod nat P) lt2 (fun f : B -> C => @GABS ((prod nat P) -> Prop) (fun f' : (prod nat P) -> Prop => forall k : nat, forall x : P, @GEQ Prop (f' (@pair nat P k x)) ((Peano.le (a x) k) /\ ((Peano.le k (b x)) /\ (p f x))))) (@GABS ((prod nat P) -> A) (fun f : (prod nat P) -> A => forall k : nat, forall x : P, @GEQ A (f (@pair nat P k x)) (s x))) (fun f : B -> C => @GABS ((prod nat P) -> R) (fun f' : (prod nat P) -> R => forall k : nat, forall x : P, @GEQ R (f' (@pair nat P k x)) (h f x k)))) -> @admissible A C B R P lt2 p s (fun f : B -> C => fun x : P => @sum nat (dotdot (a x) (b x)) (h f x)).
Axiom thm_ADMISSIBLE_MAP : forall {_139543 _139552 _139558 A B P : Type'}, forall lt2 : A -> _139543 -> Prop, forall p : (A -> B) -> P -> Prop, forall s : P -> _139543, forall h : (A -> B) -> P -> _139558 -> _139552, forall l : (A -> B) -> P -> list _139558, ((@admissible _139543 B A (list _139558) P lt2 p s l) /\ (@admissible _139543 B A _139552 (prod _139558 P) lt2 (fun f : A -> B => @GABS ((prod _139558 P) -> Prop) (fun f' : (prod _139558 P) -> Prop => forall y : _139558, forall x : P, @GEQ Prop (f' (@pair _139558 P y x)) ((p f x) /\ (@List.In _139558 y (l f x))))) (@GABS ((prod _139558 P) -> _139543) (fun f : (prod _139558 P) -> _139543 => forall y : _139558, forall x : P, @GEQ _139543 (f (@pair _139558 P y x)) (s x))) (fun f : A -> B => @GABS ((prod _139558 P) -> _139552) (fun f' : (prod _139558 P) -> _139552 => forall y : _139558, forall x : P, @GEQ _139552 (f' (@pair _139558 P y x)) (h f x y))))) -> @admissible _139543 B A (list _139552) P lt2 p s (fun f : A -> B => fun x : P => @List.map _139558 _139552 (h f x) (l f x)).
Axiom thm_ADMISSIBLE_MATCH_SEQPATTERN : forall {_139615 _139616 _139681 _139705 _139736 P : Type'}, forall lt2 : _139616 -> _139615 -> Prop, forall p : (_139616 -> _139681) -> P -> Prop, forall s : P -> _139615, forall c1 : (_139616 -> _139681) -> P -> _139736 -> _139705 -> Prop, forall c2 : (_139616 -> _139681) -> P -> _139736 -> _139705 -> Prop, forall e : (_139616 -> _139681) -> P -> _139736, ((@admissible _139615 _139681 _139616 Prop P lt2 p s (fun f : _139616 -> _139681 => fun x : P => exists y : _139705, c1 f x (e f x) y)) /\ ((@admissible _139615 _139681 _139616 _139705 P lt2 (fun f : _139616 -> _139681 => fun x : P => (p f x) /\ (exists y : _139705, c1 f x (e f x) y)) s (fun f : _139616 -> _139681 => fun x : P => @_MATCH _139736 _139705 (e f x) (c1 f x))) /\ (@admissible _139615 _139681 _139616 _139705 P lt2 (fun f : _139616 -> _139681 => fun x : P => (p f x) /\ (~ (exists y : _139705, c1 f x (e f x) y))) s (fun f : _139616 -> _139681 => fun x : P => @_MATCH _139736 _139705 (e f x) (c2 f x))))) -> @admissible _139615 _139681 _139616 _139705 P lt2 p s (fun f : _139616 -> _139681 => fun x : P => @_MATCH _139736 _139705 (e f x) (@_SEQPATTERN _139736 _139705 (c1 f x) (c2 f x))).
Axiom thm_ADMISSIBLE_IMP_SUPERADMISSIBLE : forall {A B P : Type'}, forall lt2 : A -> A -> Prop, forall p : (A -> B) -> P -> Prop, forall s : P -> A, forall t : (A -> B) -> P -> B, (@admissible A B A B P lt2 p s t) -> @superadmissible A B P lt2 p s t.
Axiom thm_SUPERADMISSIBLE_CONST : forall {_139815 _139816 _139817 : Type'} (lt2 : _139815 -> _139815 -> Prop), forall p : (_139815 -> _139817) -> _139816 -> Prop, forall s : _139816 -> _139815, forall c : _139816 -> _139817, @superadmissible _139815 _139817 _139816 lt2 p s (fun f : _139815 -> _139817 => c).
Axiom thm_SUPERADMISSIBLE_TAIL : forall {A B P : Type'}, forall lt2 : A -> A -> Prop, forall p : (A -> B) -> P -> Prop, forall s : P -> A, forall t : (A -> B) -> P -> A, ((@admissible A B A A P lt2 p s t) /\ (forall f : A -> B, forall a : P, (p f a) -> forall y : A, (lt2 y (t f a)) -> lt2 y (s a))) -> @superadmissible A B P lt2 p s (fun f : A -> B => fun x : P => f (t f x)).
Axiom thm_SUPERADMISSIBLE_COND : forall {A B P : Type'}, forall lt2 : A -> A -> Prop, forall p : (A -> B) -> P -> Prop, forall P' : (A -> B) -> P -> Prop, forall s : P -> A, forall h : (A -> B) -> P -> B, forall k : (A -> B) -> P -> B, ((@admissible A B A Prop P lt2 p s P') /\ ((@superadmissible A B P lt2 (fun f : A -> B => fun x : P => (p f x) /\ (P' f x)) s h) /\ (@superadmissible A B P lt2 (fun f : A -> B => fun x : P => (p f x) /\ (~ (P' f x))) s k))) -> @superadmissible A B P lt2 p s (fun f : A -> B => fun x : P => @COND B (P' f x) (h f x) (k f x)).
Axiom thm_SUPERADMISSIBLE_MATCH_SEQPATTERN : forall {_140136 _140251 _140252 P : Type'}, forall lt2 : _140136 -> _140136 -> Prop, forall p : (_140136 -> _140252) -> P -> Prop, forall s : P -> _140136, forall c1 : (_140136 -> _140252) -> P -> _140251 -> _140252 -> Prop, forall c2 : (_140136 -> _140252) -> P -> _140251 -> _140252 -> Prop, forall e : (_140136 -> _140252) -> P -> _140251, ((@admissible _140136 _140252 _140136 Prop P lt2 p s (fun f : _140136 -> _140252 => fun x : P => exists y : _140252, c1 f x (e f x) y)) /\ ((@superadmissible _140136 _140252 P lt2 (fun f : _140136 -> _140252 => fun x : P => (p f x) /\ (exists y : _140252, c1 f x (e f x) y)) s (fun f : _140136 -> _140252 => fun x : P => @_MATCH _140251 _140252 (e f x) (c1 f x))) /\ (@superadmissible _140136 _140252 P lt2 (fun f : _140136 -> _140252 => fun x : P => (p f x) /\ (~ (exists y : _140252, c1 f x (e f x) y))) s (fun f : _140136 -> _140252 => fun x : P => @_MATCH _140251 _140252 (e f x) (c2 f x))))) -> @superadmissible _140136 _140252 P lt2 p s (fun f : _140136 -> _140252 => fun x : P => @_MATCH _140251 _140252 (e f x) (@_SEQPATTERN _140251 _140252 (c1 f x) (c2 f x))).
Axiom thm_SUPERADMISSIBLE_MATCH_UNGUARDED_PATTERN : forall {A B D P Q : Type'}, forall lt2 : A -> A -> Prop, forall p : (A -> B) -> P -> Prop, forall s : P -> A, forall e : P -> D, forall pat : Q -> D, forall arg : P -> Q -> A, ((forall f : A -> B, forall a : P, forall t : Q, forall u : Q, ((p f a) /\ (((pat t) = (e a)) /\ ((pat u) = (e a)))) -> (arg a t) = (arg a u)) /\ (forall f : A -> B, forall a : P, forall t : Q, ((p f a) /\ ((pat t) = (e a))) -> forall y : A, (lt2 y (arg a t)) -> lt2 y (s a))) -> @superadmissible A B P lt2 p s (fun f : A -> B => fun x : P => @_MATCH D B (e x) (fun u : D => fun v : B => exists t : Q, _UNGUARDED_PATTERN (@GEQ D (pat t) u) (@GEQ B (f (arg x t)) v))).
Axiom thm_SUPERADMISSIBLE_MATCH_GUARDED_PATTERN : forall {A B D P Q : Type'}, forall lt2 : A -> A -> Prop, forall p : (A -> B) -> P -> Prop, forall s : P -> A, forall e : P -> D, forall pat : Q -> D, forall q : P -> Q -> Prop, forall arg : P -> Q -> A, ((forall f : A -> B, forall a : P, forall t : Q, forall u : Q, ((p f a) /\ (((pat t) = (e a)) /\ ((q a t) /\ (((pat u) = (e a)) /\ (q a u))))) -> (arg a t) = (arg a u)) /\ (forall f : A -> B, forall a : P, forall t : Q, ((p f a) /\ ((q a t) /\ ((pat t) = (e a)))) -> forall y : A, (lt2 y (arg a t)) -> lt2 y (s a))) -> @superadmissible A B P lt2 p s (fun f : A -> B => fun x : P => @_MATCH D B (e x) (fun u : D => fun v : B => exists t : Q, _GUARDED_PATTERN (@GEQ D (pat t) u) (q x t) (@GEQ B (f (arg x t)) v))).
Axiom thm_WF_REC_CASES : forall {A B P : Type'}, forall lt2 : A -> A -> Prop, forall clauses : list (prod (P -> A) ((A -> B) -> P -> B)), ((@WF A lt2) /\ (@List.Forall (prod (P -> A) ((A -> B) -> P -> B)) (@GABS ((prod (P -> A) ((A -> B) -> P -> B)) -> Prop) (fun f : (prod (P -> A) ((A -> B) -> P -> B)) -> Prop => forall s : P -> A, forall t : (A -> B) -> P -> B, @GEQ Prop (f (@pair (P -> A) ((A -> B) -> P -> B) s t)) (exists P' : (A -> B) -> P -> Prop, exists G : (A -> B) -> P -> A, exists H : (A -> B) -> P -> B, (forall f' : A -> B, forall a : P, forall y : A, ((P' f' a) /\ (lt2 y (G f' a))) -> lt2 y (s a)) /\ ((forall f' : A -> B, forall g : A -> B, forall a : P, (forall z : A, (lt2 z (s a)) -> (f' z) = (g z)) -> ((P' f' a) = (P' g a)) /\ (((G f' a) = (G g a)) /\ ((H f' a) = (H g a)))) /\ (forall f' : A -> B, forall a : P, (t f' a) = (@COND B (P' f' a) (f' (G f' a)) (H f' a))))))) clauses)) -> exists f : A -> B, forall x : A, (f x) = (@CASEWISE B P A (A -> B) clauses f x).
Axiom thm_RECURSION_CASEWISE : forall {A B P : Type'}, forall clauses : list (prod (P -> A) ((A -> B) -> P -> B)), ((exists lt2 : A -> A -> Prop, (@WF A lt2) /\ (@List.Forall (prod (P -> A) ((A -> B) -> P -> B)) (@GABS ((prod (P -> A) ((A -> B) -> P -> B)) -> Prop) (fun f : (prod (P -> A) ((A -> B) -> P -> B)) -> Prop => forall s : P -> A, forall t : (A -> B) -> P -> B, @GEQ Prop (f (@pair (P -> A) ((A -> B) -> P -> B) s t)) (@tailadmissible A B P lt2 (fun f' : A -> B => fun a : P => True) s t))) clauses)) /\ (forall s : P -> A, forall t : (A -> B) -> P -> B, forall s' : P -> A, forall t' : (A -> B) -> P -> B, forall f : A -> B, forall x : P, forall y : P, ((@List.In (prod (P -> A) ((A -> B) -> P -> B)) (@pair (P -> A) ((A -> B) -> P -> B) s t) clauses) /\ (@List.In (prod (P -> A) ((A -> B) -> P -> B)) (@pair (P -> A) ((A -> B) -> P -> B) s' t') clauses)) -> ((s x) = (s' y)) -> (t f x) = (t' f y))) -> exists f : A -> B, @List.Forall (prod (P -> A) ((A -> B) -> P -> B)) (@GABS ((prod (P -> A) ((A -> B) -> P -> B)) -> Prop) (fun f' : (prod (P -> A) ((A -> B) -> P -> B)) -> Prop => forall s : P -> A, forall t : (A -> B) -> P -> B, @GEQ Prop (f' (@pair (P -> A) ((A -> B) -> P -> B) s t)) (forall x : P, (f (s x)) = (t f x)))) clauses.
Axiom thm_RECURSION_CASEWISE_PAIRWISE : forall {_141475 _141491 _141495 : Type'}, forall clauses : list (prod (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491)), ((exists lt2 : _141475 -> _141475 -> Prop, (@WF _141475 lt2) /\ (@List.Forall (prod (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491)) (@GABS ((prod (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491)) -> Prop) (fun f : (prod (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491)) -> Prop => forall s : _141495 -> _141475, forall t : (_141475 -> _141491) -> _141495 -> _141491, @GEQ Prop (f (@pair (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491) s t)) (@tailadmissible _141475 _141491 _141495 lt2 (fun f' : _141475 -> _141491 => fun a : _141495 => True) s t))) clauses)) /\ ((@List.Forall (prod (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491)) (@GABS ((prod (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491)) -> Prop) (fun f : (prod (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491)) -> Prop => forall s : _141495 -> _141475, forall t : (_141475 -> _141491) -> _141495 -> _141491, @GEQ Prop (f (@pair (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491) s t)) (forall f' : _141475 -> _141491, forall x : _141495, forall y : _141495, ((s x) = (s y)) -> (t f' x) = (t f' y)))) clauses) /\ (@List.ForallOrdPairs (prod (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491)) (@GABS ((prod (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491)) -> (prod (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491)) -> Prop) (fun f : (prod (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491)) -> (prod (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491)) -> Prop => forall s : _141495 -> _141475, forall t : (_141475 -> _141491) -> _141495 -> _141491, @GEQ ((prod (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491)) -> Prop) (f (@pair (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491) s t)) (@GABS ((prod (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491)) -> Prop) (fun f' : (prod (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491)) -> Prop => forall s' : _141495 -> _141475, forall t' : (_141475 -> _141491) -> _141495 -> _141491, @GEQ Prop (f' (@pair (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491) s' t')) (forall f'' : _141475 -> _141491, forall x : _141495, forall y : _141495, ((s x) = (s' y)) -> (t f'' x) = (t' f'' y)))))) clauses))) -> exists f : _141475 -> _141491, @List.Forall (prod (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491)) (@GABS ((prod (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491)) -> Prop) (fun f' : (prod (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491)) -> Prop => forall s : _141495 -> _141475, forall t : (_141475 -> _141491) -> _141495 -> _141491, @GEQ Prop (f' (@pair (_141495 -> _141475) ((_141475 -> _141491) -> _141495 -> _141491) s t)) (forall x : _141495, (f (s x)) = (t f x)))) clauses.
Axiom thm_SUPERADMISSIBLE_T : forall {_141605 _141607 _141611 : Type'} (lt2 : _141605 -> _141605 -> Prop) (s : _141611 -> _141605) (t : (_141605 -> _141607) -> _141611 -> _141607), (@superadmissible _141605 _141607 _141611 lt2 (fun f : _141605 -> _141607 => fun x : _141611 => True) s t) = (@tailadmissible _141605 _141607 _141611 lt2 (fun f : _141605 -> _141607 => fun x : _141611 => True) s t).
