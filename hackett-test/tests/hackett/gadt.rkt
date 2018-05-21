#lang hackett
(require hackett/gadt
         "hackunit.rkt")

;; example taken from:
;; https://downloads.haskell.org/~ghc/6.6/docs/html/users_guide/gadt.html

(def = : (-> Integer (-> Integer Bool))
  (lambda [x]
    (lambda [y]
      (&& (<= x y) (<= y x)))))

(data (Term A)
      (Lit Integer -> (Term Integer))
      (Inc (Term Integer) -> (Term Integer))
      (IsZ (Term Integer) -> (Term Bool))
      (If (Term Bool) (Term A) (Term A) -> (Term A)))
      ;(Pair (Term A) (Term B) -> (Term (Tuple A B))))

(check-type (Lit 1) : (Term Integer))
(check-type (Inc (Lit 1)) : (Term Integer))
(check-type (IsZ (Lit 1)) : (Term Bool))
(check-type (If (IsZ (Lit 0)) (Lit 1) (Lit 2)) : (Term Integer))
(check-type (If (IsZ (Lit 0)) (IsZ (Lit 1)) (IsZ (Lit 2))) : (Term Bool))

(typecheck-fail (Lit True)
 #:with-msg (tyerrmsg #:got "Bool" #:expected "Integer"))
(typecheck-fail (Inc 1)
 #:with-msg (tyerrmsg #:got "Integer" #:expected "(Term Integer)"))
(typecheck-fail (Inc (IsZ (Lit 1)))
 #:with-msg (tyerrmsg #:got "Bool" #:expected "Integer"))
(typecheck-fail (IsZ 1)
 #:with-msg (tyerrmsg #:got "Integer" #:expected "(Term Integer)"))
(typecheck-fail (If (Lit 0) (Lit 1) (Lit 2)) ; wrong test
                #:with-msg (tyerrmsg #:got "Integer" #:expected "Bool"))
(typecheck-fail (If (IsZ (Lit 0)) (Lit 1) (IsZ (Lit 1))) ; branches dont match
 #:with-msg (tyerrmsg #:got "Bool" #:expected "Integer"))

(defn eval : (forall (A) (-> (Term A) A))
  [[(Lit i)] i]
  [[(Inc t)] {(eval t) + 1}]
  [[(IsZ t)] (= 0 (eval t))]
  [[(If b t e)] (if (eval b) (eval t) (eval e))])

(check-type (eval (Lit 1)) : Integer -> 1)
(check-type (eval (Inc (Lit 1))) : Integer -> 2)
(check-type (eval (IsZ (Lit 0))) : Bool -> True)
(check-type (eval (IsZ (Lit 1))) : Bool -> False)
(check-type (eval (If (IsZ (Lit 0)) (Lit 1) (Lit 2))) : Integer)
(check-type (eval (If (IsZ (Lit 1)) (Lit 1) (Lit 2))) : Integer)
(check-type (eval (If (IsZ (Lit 0)) (IsZ (Lit 0)) (IsZ (Lit 1)))) : Bool -> True)
(check-type (eval (If (IsZ (Lit 1)) (IsZ (Lit 0)) (IsZ (Lit 1)))) : Bool -> False)

(typecheck-fail/toplvl
 (defn eval : (forall (A) (-> (Term A) A))
   [[(Lit i)] False] ; wrong type
   [[(Inc t)] {(eval t) + 1}]
   [[(IsZ t)] (= 0 (eval t))]
   [[(If b t e)] (if (eval b) (eval t) (eval e))])
 #:with-msg (tyerrmsg #:got "Bool" #:expected "Integer" #:in "False"))

(typecheck-fail/toplvl
 (defn eval : (forall (A) (-> (Term A) A))
   [[(Lit i)] i]
   [[(Inc t)] {(eval t) + 1}]
   [[(IsZ t)] 1] ; wrong type
   [[(If b t e)] (if (eval b) (eval t) (eval e))])
 #:with-msg (tyerrmsg #:got "Integer" #:expected "Bool" #:in "1"))

(typecheck-fail/toplvl
 (defn eval : (forall (A) (-> (Term A) A))
   [[(Lit i)] i]
   [[(Inc t)] {(eval t) + 1}]
   [[(IsZ t)] (= 0 (eval t))]
   [[(If b t e)] 1]) ; wrong type
 #:with-msg (tyerrmsg #:got "Integer" #:expected "A" #:in "1"))

;; exhaustive checks
(typecheck-fail/toplvl
 (defn eval : (forall (A) (-> (Term A) A))
;   [[(Lit i)] i]
   [[(Inc t)] {(eval t) + 1}]
   [[(IsZ t)] (= 0 (eval t))]
   [[(If b t e)] (if (eval b) (eval t) (eval e))])
 #:with-msg "unmatched case.*Lit")
(typecheck-fail/toplvl
 (defn eval : (forall (A) (-> (Term A) A))
   [[(Lit i)] i]
;   [[(Inc t)] {(eval t) + 1}]
   [[(IsZ t)] (= 0 (eval t))]
   [[(If b t e)] (if (eval b) (eval t) (eval e))])
 #:with-msg "unmatched case.*Inc")
(typecheck-fail/toplvl
 (defn eval : (forall (A) (-> (Term A) A))
   [[(Lit i)] i]
   [[(Inc t)] {(eval t) + 1}]
;   [[(IsZ t)] (= 0 (eval t))]
   [[(If b t e)] (if (eval b) (eval t) (eval e))])
 #:with-msg "unmatched case.*IsZ")
(typecheck-fail/toplvl
 (defn eval : (forall (A) (-> (Term A) A))
   [[(Lit i)] i]
   [[(Inc t)] {(eval t) + 1}]
   [[(IsZ t)] (= 0 (eval t))])
;   [[(If b t e)] (if (eval b) (eval t) (eval e))])
 #:with-msg "unmatched case.*If")
