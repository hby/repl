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

;;; In hindsight, I realize I could use multimethods.
;;; I may make that update in the future.

(declare big-map)
(defn- explore-map
  "Used by com.inferstructure.repl/explore to safely explore
   potentially large maps."
  [t p m]
  (if (< (count m) t)
    (into {} (map (fn [[key val]] [key (explore t p val)]) m))
    (big-map t p m)))

(defn- big-map
  "Return a map with p+1 entries.
   p entries, [k v], come from m and have the values replaced with a call
   to (com.inferstructure.repl/explore t p v).
   Another entry is [:more \"n more entries \"] where n is (- (count m) p)."
  [t p m]
  (let [peeked (take p m)
        rest (drop p m)
        more (str (count rest) " more entries")]
    (assoc (into {}
                 (map (fn [[key val]] [key (explore t p val)]) peeked))
      :more more)))

(declare big-vector)
(defn- explore-vector
  "Used by com.inferstructure.repl/explore to safely explore
   potentially large vectors."
  [t p v]
  (if (< (count v) t)
    (into [] (map (fn [e] (explore t p e)) v))
    (big-vector t p v)))

(defn- big-vector
  "Return a vector of p+1 elements.
   p elements come from calling (com.inferstructure.repl/explore t p e) on the first p elements (e) of v.
   Another element (the last) is \"n more entries \" where n is (- (count v) p)"
  [t p v]
  (let [peeked (take p v)
        rest (drop p v)
        more (str (count rest) " more entries")]
    (conj (mapv (fn [e] (explore t p e)) peeked)
          more)))

(declare big-set)
(defn- explore-set
  "Used by com.inferstructure.repl/explore to safely explore
   potentially large sets."
  [t p s]
  (if (< (count s) t)
    (into #{} (map (fn [e] (explore t p e)) s))
    (big-set t p s)))

(defn- big-set
  "Return a set with p+1 elements.
   p elements (e) come from s and are the result of a call
   to (com.inferstructure.repl/explore t p e).
   Another element is \"n more entries \" where n is (- (count s) p)."
  [t p s]
  (let [peeked (take p s)
        rest (drop p s)
        more (str (count rest) " more entries")]
    (conj (into #{} (map (fn [e] (explore t p e)) peeked))
          more)))

(declare big-seq)
(defn- explore-seq
  "Used by com.inferstructure.repl/explore to safely explore
   potentially large sequences."
  [t p s]
  (let [front (take t s)
        more (drop t s)]
    (if (and (< (count front) t)
             (empty? more))
      (map (fn [e] (explore t p e)) s)
      (big-seq t p s))))

(defn- big-seq
  "Return a sequence of p+1 elements.
   p elements come from calling (com.inferstructure.repl/explore t p e) on the first p elements (e) of v.
   Another element (the last) is \"more ...\""
  [t p s]
  (let [peeked (take p s)
        more "more ..."]
    (concat (map (fn [e] (explore t p e)) peeked)
            (list more))))
