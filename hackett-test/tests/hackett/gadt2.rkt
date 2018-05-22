#lang hackett
(require hackett/gadt
         "hackunit.rkt")

;; like the example in gadt.rkt,
;; but uses alternative `data` syntax that allows specifying the full type
;; - this allows adding `Pair` to the datatype def

;; example taken from:
;; https://downloads.haskell.org/~ghc/6.6/docs/html/users_guide/gadt.html

(def = : (-> Integer (-> Integer Bool))
  (lambda [x]
    (lambda [y]
      (&& (<= x y) (<= y x)))))

(data (Term A)
 [Lit  :: Integer -> (Term Integer)]
 [Inc  :: (Term Integer) -> (Term Integer)]
 [IsZ  :: (Term Integer) -> (Term Bool)]
 [If   :: (forall (A) (Term Bool) (Term A) (Term A) -> (Term A))]
 [Pair :: (forall (A B) (Term A) (Term B) -> (Term (Tuple A B)))])
#;(data (Term A)
      (Lit Integer -> (Term Integer))
      (Inc (Term Integer) -> (Term Integer))
      (IsZ (Term Integer) -> (Term Bool))
      (If (Term Bool) (Term A) (Term A) -> (Term A)))

(check-type (Lit 1) : (Term Integer))
(check-type (Inc (Lit 1)) : (Term Integer))
(check-type (IsZ (Lit 1)) : (Term Bool))
(check-type (If (IsZ (Lit 0)) (Lit 1) (Lit 2)) : (Term Integer))
(check-type (If (IsZ (Lit 0)) (IsZ (Lit 1)) (IsZ (Lit 2))) : (Term Bool))
(check-type (Pair (Lit 1) (IsZ (Lit 0))) : (Term (Tuple Integer Bool)))
(check-type (Pair (Lit 1) (Lit 0)) : (Term (Tuple Integer Integer)))

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
  [[(If b e1 e2)] (if (eval b) (eval e1) (eval e2))]
  [[(Pair e1 e2)] (Tuple (eval e1) (eval e2))])

(check-type (eval (Lit 1)) : Integer -> 1)
(check-type (eval (Inc (Lit 1))) : Integer -> 2)
(check-type (eval (IsZ (Lit 0))) : Bool -> True)
(check-type (eval (IsZ (Lit 1))) : Bool -> False)
(check-type (eval (If (IsZ (Lit 0)) (Lit 1) (Lit 2))) : Integer)
(check-type (eval (If (IsZ (Lit 1)) (Lit 1) (Lit 2))) : Integer)
(check-type (eval (If (IsZ (Lit 0)) (IsZ (Lit 0)) (IsZ (Lit 1)))) : Bool -> True)
(check-type (eval (If (IsZ (Lit 1)) (IsZ (Lit 0)) (IsZ (Lit 1)))) : Bool -> False)
(check-type (eval (Pair (IsZ (Lit 0)) (Lit 1))) : (Tuple Bool Integer))

(typecheck-fail/toplvl
 (defn eval : (forall (A) (-> (Term A) A))
   [[(Lit i)] False] ; wrong type
   [[(Inc t)] {(eval t) + 1}]
   [[(IsZ t)] (= 0 (eval t))]
   [[(If b e1 e2)] (if (eval b) (eval e1) (eval e2))]
   [[(Pair e1 e2)] (Tuple (eval e1) (eval e2))])
 #:with-msg (tyerrmsg #:got "Bool" #:expected "Integer" #:in "False"))

(typecheck-fail/toplvl
 (defn eval : (forall (A) (-> (Term A) A))
   [[(Lit i)] i]
   [[(Inc t)] {(eval t) + 1}]
   [[(IsZ t)] 1] ; wrong type
   [[(If b e1 e2)] (if (eval b) (eval e1) (eval e2))]
   [[(Pair e1 e2)] (Tuple (eval e1) (eval e2))])
 #:with-msg (tyerrmsg #:got "Integer" #:expected "Bool" #:in "1"))

(typecheck-fail/toplvl
 (defn eval : (forall (A) (-> (Term A) A))
   [[(Lit i)] i]
   [[(Inc t)] {(eval t) + 1}]
   [[(IsZ t)] (= 0 (eval t))]
   [[(If b t e)] 1] ; wrong type
   [[(Pair e1 e2)] (Tuple (eval e1) (eval e2))])
 #:with-msg (tyerrmsg #:got "Integer" #:expected "A" #:in "1"))

(typecheck-fail/toplvl
 (defn eval : (forall (A) (-> (Term A) A))
   [[(Lit i)] i]
   [[(Inc t)] {(eval t) + 1}]
   [[(IsZ t)] (= 0 (eval t))]
   [[(If b e1 e2)] (if (eval b) (eval e1) (eval e2))]
   [[(Pair e1 e2)] (Tuple (eval e2) (eval e1))]) ; wrong type: pair elements flipped
 #:with-msg (tyerrmsg #:got "B" #:expected "A"))

(typecheck-fail/toplvl
 (defn eval : (forall (A) (-> (Term A) A))
   [[(Lit i)] i]
   [[(Inc t)] {(eval t) + 1}]
   [[(IsZ t)] (= 0 (eval t))]
   [[(If b e1 e2)] (if (eval b) (eval e1) (eval e2))]
   [[(Pair e1 e2)] (Tuple (eval e1) (eval e1))]) ; wrong type: pair with same type
 #:with-msg (tyerrmsg #:got "A" #:expected "B"))

(typecheck-fail/toplvl
 (defn eval : (forall (A) (-> (Term A) A))
   [[(Lit i)] i]
   [[(Inc t)] {(eval t) + 1}]
   [[(IsZ t)] (= 0 (eval t))]
   [[(If b e1 e2)] (if (eval b) (eval e1) (eval e2))]
   [[(Pair e1 e2)] (Tuple (eval e1) 1)]) ; wrong type: pair with concrete types
 #:with-msg (tyerrmsg #:got "Integer" #:expected "B"))

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
(typecheck-fail/toplvl
 (defn eval : (forall (A) (-> (Term A) A))
   [[(Lit i)] i]
   [[(Inc t)] {(eval t) + 1}]
   [[(IsZ t)] (= 0 (eval t))]
   [[(If b t e)] (if (eval b) (eval t) (eval e))])
   ;[[(Pair e1 e2)] (Tuple (eval e1) (eval e2))]
 #:with-msg "unmatched case.*Pair")

