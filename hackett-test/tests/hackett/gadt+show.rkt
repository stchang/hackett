#lang hackett
(require hackett/gadt)

;; Term example from gadt.rkt, used with type classes

(data (Term A)
      (Lit Integer -> (Term Integer))
      (Inc (Term Integer) -> (Term Integer))
      (IsZ (Term Integer) -> (Term Bool))
      (If (Term Bool) (Term A) (Term A) -> (Term A)))

(defn term->str : (forall (A) (-> (Term A) String))
  [[(Lit i)] (show i)]
  [[(Inc t)] (++ "Inc " (term->str t))]
  [[(IsZ t)] (++ "IsZ " (term->str t))]
  [[(If b t e)] {"If (" ++ (term->str b) ++ ") then " ++ (term->str t) ++ " else " ++ (term->str e)}])

;(main (println {"1" ++ "2" ++ "3"}))
(main (println (term->str (If (IsZ (Inc (Lit 1))) (Lit 2) (Lit 3)))))
