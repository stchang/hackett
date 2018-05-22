#lang hackett
(require hackett/gadt
         "hackunit.rkt")

; more tests like gadt2.rkt

;; Pair, with Fst and Snd
(data (Term A)
 [Lit :: Integer -> (Term Integer)]
 [Boo :: Bool -> (Term Bool)]
 [Par :: (forall (A B) (Term A) (Term B) -> (Term (Tuple A B)))]
 [Fst :: (forall (A B) (Term (Tuple A B)) -> (Term A))]
 [Snd :: (forall (A B) (Term (Tuple A B)) -> (Term B))])

(defn eval : (forall (A) (-> (Term A) A))
  [[(Lit i)] i]
  [[(Boo b)] b]
  [[(Par e1 e2)] (Tuple (eval e1) (eval e2))]
  [[(Fst e)] (fst (eval e))]
  [[(Snd e)] (snd (eval e))])

(check-type (Par (Lit 0) (Boo False)) : (Term (Tuple Integer Bool)))
(check-type (Fst (Par (Lit 0) (Boo False))) : (Term Integer))
(check-type (Snd (Par (Lit 0) (Boo False))) : (Term Bool))

(check-type (eval (Par (Lit 0) (Boo False))) : (Tuple Integer Bool))
(check-type (eval (Fst (Par (Lit 0) (Boo False)))) : Integer)
(check-type (eval (Snd (Par (Lit 0) (Boo False)))) : Bool)

;; TODO: fix gadt exhaustiveness checking
(defn eval2 : (-> (Term Integer) Integer)
  [[(Lit i)] i])
;  [[(Boo b)] b]
;  [[(Par e1 e2)] (Tuple (eval e1) (eval e2))]
;  [[(Fst e)] (fst (eval e))]
;  [[(Snd e)] (snd (eval e))])
