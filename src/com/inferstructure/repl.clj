(ns com.inferstructure.repl)

(declare explore-map)
(declare explore-vector)
(declare explore-set)
(declare explore-seq)
(defn explore
  "Safely explore possibly large map/vector/set/seq structures in the repl.
   mvsors - map, vector, set, or seq
   threshold - if count is under threshold, recursively explore
   peekn - if over threshold, recursivley explore peekn entries
           and summarize the rest"
  [mvsors threshold peekn]
  {:pre [(> threshold peekn)]}
  (cond
    (map? mvsors) (explore-map mvsors threshold peekn)
    (vector? mvsors) (explore-vector mvsors threshold peekn)
    (set? mvsors) (explore-set mvsors threshold peekn)
    (seq? mvsors) (explore-seq mvsors threshold peekn)
    :else mvsors))

(declare big-map)
(defn explore-map
  [m t p]
  (if (< (count m) t)
    (into {} (map (fn [[key val]] [key (explore val t p)]) m))
    (big-map m t p)))

(defn big-map
  [m t p]
  (let [peeked (take p m)
        rest (drop p m)
        more (str (count rest) " more entries")]
    (assoc (into {}
                 (map (fn [[key val]] [key (explore val t p)]) peeked))
      :more more)))

(declare big-vector)
(defn explore-vector
  [v t p]
  (if (< (count v) t)
    (into [] (map (fn [e] (explore e t p)) v))
    (big-vector v t p)))

(defn big-vector
  [v t p]
  (let [peeked (take p v)
        rest (drop p v)
        more (str (count rest) " more entries")]
    (conj (mapv (fn [e] (explore e t p)) peeked)
          more)))

(declare big-set)
(defn explore-set
  [s t p]
  (if (< (count s) t)
    (into #{} (map (fn [e] (explore e t p)) s))
    (big-set s t p)))

(defn big-set
  [s t p]
  (let [peeked (take p s)
        rest (drop p s)
        more (str (count rest) " more entries")]
    (conj (into #{} (map (fn [e] (explore e t p)) peeked))
          more)))

(declare big-seq)
(defn explore-seq
  [s t p]
  (let [front (take t s)
        more (drop t s)]
    (if (and (< (count front) t)
             (empty? more))
      (map (fn [e] (explore e t p)) s)
      (big-seq s t p))))

(defn big-seq
  [s t p]
  (let [peeked (take p s)
        more "more ..."]
    (concat (map (fn [e] (explore e t p)) peeked)
            (list more))))
