#lang hackett
(require hackett/gadt)

;; Term example from gadt2.rkt, used with type classes

(data (Term A)
 [Lit  :: Integer -> (Term Integer)]
 [Inc  :: (Term Integer) -> (Term Integer)]
 [IsZ  :: (Term Integer) -> (Term Bool)]
 [If   :: (forall (A) (Term Bool) (Term A) (Term A) -> (Term A))]
 [Pair :: (forall (A B) (Term A) (Term B) -> (Term (Tuple A B)))])

(defn term->str : (forall (A) (-> (Term A) String))
  [[(Lit i)] (show i)]
  [[(Inc t)] (++ "Inc " (term->str t))]
  [[(IsZ t)] (++ "IsZ " (term->str t))]
  [[(If b t e)] {"If (" ++ (term->str b) ++ ") then " ++ (term->str t) ++ " else " ++ (term->str e)}]
  [[(Pair e1 e2)] {"(" ++ (term->str e1) ++ ", " ++ (term->str e2) ++ ")"}])

;(main (println {"1" ++ "2" ++ "3"}))
(main (println (term->str (Pair (If (IsZ (Inc (Lit 1))) (Lit 2) (Lit 3))
                                (Lit 0)))))
