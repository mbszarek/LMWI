-- Mateusz Szarek
-- Computer Science Student
-- Faculty of Computer Science, Electronics and Telecommunication
-- AGH University of Science and Technology
-- 2016/2017 Academic Year

data A = A
data B = B
data C = C
data D = D
data Disjunction a b = Either a b

impl_rozdz :: (A -> B) -> (A -> C) -> A -> B -> C
impl_rozdz = \p -> \q -> \r -> \s -> q r

impl_kompl :: (A -> B) -> (B -> C) -> A -> C
impl_kompl = \p -> \q -> \r -> q (p r)

impl_perm :: (A -> B -> C) -> B -> A -> C
impl_perm = \p -> \q -> \r -> p r q

impl_conj :: A -> B -> (A, B)
impl_conj = \p -> \q -> (p, q)

conj_elim_l :: (A, B) -> A
conj_elim_l = \p -> (\q -> \r -> q) (fst p) (snd p)

disj_intro_l :: A -> Either A B
disj_intro_l = \p -> Left p

rozl_elim :: Either A B -> (A -> C) -> (B -> C) -> C
rozl_elim = \(Left p) -> \q -> \r -> q p
rozl_elim' = \(Right p) -> \q -> \r -> r p

diamencik :: (A -> B) -> (A -> C) -> (B -> C -> D) -> A -> D
diamencik = \p -> \q -> \r -> \t -> r (p t) (q t)

slaby_peirce :: ((((A -> B) -> A) -> A) -> B) -> B
slaby_peirce = \p -> p (\q -> q (\r -> p (\t -> r)))

rozl_impl_rozdz :: (Disjunction A B -> C) -> ((A -> C), (B -> C))
rozl_impl_rozdz = \p -> ((\q -> p (Either q B)), (\q -> p (Either A q)))

rozl_impl_rozdz_odw :: ((A -> C), (B -> C)) -> (Either A B) -> C
rozl_impl_rozdz_odw = \p -> \(Left q) -> (\r -> \t -> r q) (fst p) (snd p)
rozl_impl_rozdz_odw' = \p -> \(Right q) -> (\r -> \t -> r q) (fst p) (snd p)

curry :: ((A, B) -> C) -> A -> B -> C
curry = \p -> \q -> \r -> p (q, r)

uncurry :: (A -> B -> C) -> (A, B) -> C
uncurry = \p -> \q -> p ((\r -> \t ->r) (fst q) (snd q)) ((\r -> \t -> t) (fst q) (snd q))

main = print()

