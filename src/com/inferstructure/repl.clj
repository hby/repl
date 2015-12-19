(ns com.inferstructure.repl)

(declare explore-map)
(declare explore-vector)
(declare explore-set)
(declare explore-seq)
(defn explore
  "Safely explore possibly large map/vector/set/seq structures in the repl.
   threshold - if count is under threshold, recursively explore
   peekn - if over threshold, recursivley explore peekn entries
           and summarize the rest
   mvsors - map, vector, set, or seq"
  [threshold peekn mvsors]
  {:pre [(> threshold peekn)]}
  (cond
    (map? mvsors) (explore-map threshold peekn mvsors)
    (vector? mvsors) (explore-vector threshold peekn mvsors)
    (set? mvsors) (explore-set threshold peekn mvsors)
    (seq? mvsors) (explore-seq threshold peekn mvsors)
    :else mvsors))

(declare big-map)
(defn- explore-map
  [t p m]
  (if (< (count m) t)
    (into {} (map (fn [[key val]] [key (explore t p val)]) m))
    (big-map t p m)))

(defn- big-map
  [t p m]
  (let [peeked (take p m)
        rest (drop p m)
        more (str (count rest) " more entries")]
    (assoc (into {}
                 (map (fn [[key val]] [key (explore t p val)]) peeked))
      :more more)))

(declare big-vector)
(defn- explore-vector
  [t p v]
  (if (< (count v) t)
    (into [] (map (fn [e] (explore t p e)) v))
    (big-vector t p v)))

(defn- big-vector
  [t p v]
  (let [peeked (take p v)
        rest (drop p v)
        more (str (count rest) " more entries")]
    (conj (mapv (fn [e] (explore t p e)) peeked)
          more)))

(declare big-set)
(defn- explore-set
  [t p s]
  (if (< (count s) t)
    (into #{} (map (fn [e] (explore t p e)) s))
    (big-set t p s)))

(defn- big-set
  [t p s]
  (let [peeked (take p s)
        rest (drop p s)
        more (str (count rest) " more entries")]
    (conj (into #{} (map (fn [e] (explore t p e)) peeked))
          more)))

(declare big-seq)
(defn- explore-seq
  [t p s]
  (let [front (take t s)
        more (drop t s)]
    (if (and (< (count front) t)
             (empty? more))
      (map (fn [e] (explore t p e)) s)
      (big-seq t p s))))

(defn- big-seq
  [t p s]
  (let [peeked (take p s)
        more "more ..."]
    (concat (map (fn [e] (explore t p e)) peeked)
            (list more))))
