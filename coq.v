Require Import ProofWeb.

Section ZadanieOne.

Variables A B C D : Prop. 

Theorem impl_rozdz : (A -> B) -> (A -> C) -> A -> B -> C.
Proof.
intro.
intro.
intro.
intro.
apply H0.
exact H1.
Qed.

Theorem impl_komp : (A -> B) -> (B -> C) -> A -> C.
Proof.
intro.
intro.
intro.
apply H0.
apply H.
exact H1.
Qed.

Theorem impl_perm : (A -> B -> C) -> B -> A -> C. 
Proof.
intro.
intro.
intro.
apply H.
exact H1.
exact H0.
Qed.


Theorem impl_conj : A -> B -> A /\ B. 
Proof.
intro.
intro.
split.
exact H.
exact H0.
Qed.

Theorem conj_elim_l : A /\ B -> A. 
Proof.
intro.
elim H.
intro.
intro.
exact H0.
Qed.

Theorem disj_intro_l : A -> A \/ B.
Proof.
intro.
left.
exact H.
Qed.

Theorem rozl_elim : A \/ B -> (A -> C) -> (B -> C) -> C. 
Proof.
intro.
intro.
intro.
elim H.
exact H0.
exact H1.
Qed.

Theorem diamencik : (A -> B) -> (A -> C) -> (B -> C -> D) -> A -> D.
Proof.
intro.
intro.
intro.
intro.
apply H1.
apply H.
exact H2.
apply H0.
exact H2.
Qed.

Theorem slaby_peirce : ((((A -> B) -> A) -> A) -> B) -> B.
Proof.
intro.
apply H.
intro.
apply H0.
intro.
apply H.
intro.
exact H1.
Qed.

Theorem rozl_impl_rozdz : (A \/ B -> C) -> (A -> C) /\ (B -> C).
Proof.
intro.
split.
intro.
apply H.
left.
exact H0.
intro.
apply H.
right.
exact H0.
Qed.

Theorem rozl_impl_rozdz_odw : (A -> C) /\ (B -> C) -> A \/ B -> C.
Proof.
intro.
intro.
elim H0.
intro.
elim H.
intro.
intro.
apply H2.
exact H1.
elim H.
intro.
intro.
intro.
apply H2.
exact H3.
Qed.

Theorem curry : (A /\ B -> C) -> A -> B -> C.
Proof.
intro.
intro.
intro.
apply H.
split.
exact H0.
exact H1.
Qed.

Theorem uncurry : (A -> B -> C) -> A /\ B -> C.
Proof.
intro.
intro.
apply H.
elim H0.
intro.
intro.
exact H1.
elim H0.
intro.
intro.
exact H2.
Qed.​

End ZadanieOne.
