(ns aoc.d2)

(def input [1 0 0 3 1 1 2 3 1 3 4 3 1 5 0 3 2 1 6 19 1 9 19 23 1 6 23 27 1 10 27 31 1 5 31 35 2 6 35 39 1 5 39 43 1 5 43 47 2 47 6 51 1 51 5 55 1 13 55 59 2 9 59 63 1 5 63 67 2 67 9 71 1 5 71 75 2 10 75 79 1 6 79 83 1 13 83 87 1 10 87 91 1 91 5 95 2 95 10 99 2 9 99 103 1 103 6 107 1 107 10 111 2 111 10 115 1 115 6 119 2 119 9 123 1 123 6 127 2 127 10 131 1 131 6 135 2 6 135 139 1 139 5 143 1 9 143 147 1 13 147 151 1 2 151 155 1 10 155 0 99 2 14 0 0])

(defn init-stack
  [codes noun verb]
  (assoc codes
         1 noun
         2 verb))

(def ops {1 {:arity 2
             :func +}
          2 {:arity 2
             :func *}
          99 {:arity 0
              :func nil}})

(defn pull-args
  [stack ip arity]
  (let [start (inc ip)
        end (+ start arity)]
    (->> (subvec stack start end)
         (map #(get stack %)))))

(defn compute
  [noun verb]
  (first
   (loop [stack (init-stack input noun verb)
          ip 0]
     (let [opcode (get stack ip)
           op (ops opcode)
           func (:func op)
           arity (:arity op)]
       (if (and func (pos? arity))
         (let [ret-addr (+ ip arity 1)]
           (recur (assoc stack
                         (get stack ret-addr)
                         (apply func (pull-args stack ip arity)))
                  (inc ret-addr)))
         stack)))))

(defn part1
  []
  (compute 12 2))

(defn part2
  []
  (for [noun (range 100)
        verb (range 100)
        :when (= (compute noun verb) 19690720)
        :let [res (+ (* 100 noun) verb)]]
    res))
