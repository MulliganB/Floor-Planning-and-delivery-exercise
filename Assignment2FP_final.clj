(def graph {:Mail {:oA2 1, :ts 1},
            :oA2 {:oA4 1, :Mail 1},
            :oA4 {:oD4 1, :oA2 1},
            :oD4 {:oD1 1, :oA4 1},
            :oD1 {:o131 1, :oD4 1},
            :o131 {:o129 1, :oD1 1, :r131 2},
            :r131 {:o131 2},
            :o129 {:o127 1, :d1 2, :r129 2},
            :o127 {:o125 1, :d2 1, :o129 1, :r127 2},
            :d1 {:d2 1, :o129 2},
            :r129 {:o129 2},
            :d2 {:d3 1, :d1 1, :o125 2},
            :o125 {:o123 1, :o127 1, :r125 2, :d2 2},
            :o123 {:o121 1, :r123 2, :c1 2},
            :r125 {:o125 2},
            :r123 {:o123 2},
            :d3 {:d2 1, :a1 1},
            :o121 {:o119 1, :o123 1, :r121 2},
            :r121 {:o121 2},
            :o119 {:o117 0.5, :Storage 0.5, :r119 2},
            :r119 {:o119 2},
            :Storage {:o119 0.5, :o117 0.5},
            :o117 {:o119 0.5, :Storage 0.5, :o115 1.5, :r117 2},
            :r117 {:o117 2},
            :o115 {:o117 1.5, :o113 1.5, :r115 2},
            :r115 {:o115 2},
            :o113 {:o115 1.5, :o109 1.5, :r113 2},
            :r113 {:o113 2},
            :o109 {:o107 1, :o111 1, :o113 1.5, :r109 2},
            :r109 {:o109 1},
            :o111 {:o109 1, :r111 2},
            :r111 {:o111 2},
            :o107 {:o109 1, :o105 1, :b4 2, :r107 2},
            :r107 {:o107 2},
            :o105 {:o107 1, :o103 1, :r105 2},
            :r105 {:o105 2},
            :o103 {:o105 1, :o101 1, :b3 2, :r103 2},
            :r103 {:o103 2},
            :o101 {:o103 1, :ts 1.5, :a3 2, :r101 2},
            :r101 {:o101 2},
            :ts {:o101 1.5, :Mail 1, :a2 2},
            :b3 {:o103 1, :b4 1, :b1 1},
            :b4 {:b3 1, :b2 1, :o107 2},
            :b1 {:b3 1, :b2 1, :a1 1.5, :c2 2},
            :b2 {:b1 1, :b4 1, :c3 2},
            :c2 {:c3 1, :c1 1, :b1 2},
            :c1 {:c2 1, :o123 2},
            :c3 {:c2 1, :b2 2},
            :a1 {:a3 1, :b1 1.5, :d3 2},
            :a2 {:a3 1, :ts 2},
            :a3 {:a2 1, :a1 1, :o101 2}})

(def robot {:parcelNumber 0, :cost 0, :current 0, :path 0})

;;Achieved with help from http://codereview.stackexchange.com/questions/15961/depth-first-search-algorithm-in-clojure - Answer by tnoda
(defn- dfs
  [graph goal]
  (fn search
    [path visited]
    (let [current (peek path)]
      (if (= goal current)
        [path]
          (->> current graph keys
             (remove visited)
             (mapcat #(search (conj path %) (conj visited %))))))))

(defn findpath
  [graph start goal]
  ((dfs graph goal) [start] #{start}))

(defn lSort [n]
  (sort-by < (first n)))

(defn select-vals [map keys] ((apply juxt keys) map))

;;http://stackoverflow.com/questions/29064260/flatten-nested-map-into-list-with-depth-first-search-ordering - answer by Leonid Beschastny
(defn flatten-map [m]
  (->> [nil m]
       (tree-seq sequential? second)
       (drop 1)
       (map first)))

(defn sum [coll] (reduce + coll))

(defn calculate_cost
  [graph start goal]
  (sum (take-nth 2(rest (flatten (flatten-map (select-vals graph (lSort(findpath graph start goal)))))))))

(defn moveRobotSingleRoute
  [graph robot parcelNum start goal]
    (assoc robot
      :cost  (calculate_cost graph start goal)
      :path (lSort(findpath graph start goal))
      :current (last(lSort(findpath graph start goal)))
      :parcelNumber (- parcelNum 1)
      )
)

(defn moveRobot2
  [graph robot parcelNum start goal]
  (assoc robot
        :cost (:cost + (calculate_cost graph start goal))
        :path  (flatten(cons (get robot :path)(remove #{start} (lSort(findpath graph start goal)))))
        :current (last(lSort(findpath graph start goal)))
        :parcelNumber (- (get robot :parcelNumber) 1))
)
(defn MultiRobotRoute
  [graph robot parcelNum start goal goal2]
     (moveRobot2 graph (assoc robot
        :cost  (calculate_cost graph start goal)
        :path (lSort(findpath graph start goal))
        :current (last(lSort(findpath graph start goal)))
        :parcelNumber (- parcelNum 1)) (robot :parcelNumber) goal goal2)
)

;;Runnable Questions
(prn (moveRobotSingleRoute graph robot 1 :Mail :r131)) ;;question 1
(prn (moveRobotSingleRoute graph robot 1 :Mail :r119)) ;;question 2
(prn (moveRobotSingleRoute graph robot 1 :r113 :r115)) ;;question 3
(prn (moveRobotSingleRoute graph robot 1 :r113 :r129)) ;;question 4

(prn (MultiRobotRoute graph robot 2 :Mail :r131 :Mail)) ;;partial question 5
(prn (MultiRobotRoute graph robot 2 :Mail :r131 :r111)) ;;partial question 6
