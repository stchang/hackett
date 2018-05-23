#lang hackett
(require hackett/gadt)

(data (Foo a) (foo Integer -> (Foo a)))

(defn unfoo : (forall [a] {(Foo a) -> String})
  [[(foo n)] (show n)])

(main (println (unfoo (foo 1))))
