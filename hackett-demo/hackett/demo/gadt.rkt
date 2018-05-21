#lang hackett
(require hackett/gadt)

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

(defn eval : (forall (A) (-> (Term A) A))
  [[(Lit i)] i]
  [[(Inc t)] {(eval t) + 1}]
  [[(IsZ t)] (= 0 (eval t))]
  [[(If b t e)] (if (eval b) (eval t) (eval e))])
