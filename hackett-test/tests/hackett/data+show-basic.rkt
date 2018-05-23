#lang hackett


(data (Foo a) (foo Integer))

(defn unfoo : (forall [a] {(Foo a) -> String})
  [[(foo n)] (show n)])

(main (println (unfoo (foo 1))))
